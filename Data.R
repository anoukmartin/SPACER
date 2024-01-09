
# Packages
library(dplyr)
library(stringr)
library(readr)
library(openxlsx)

# Résultats des législatives ###################################################

## Téléchargement des données ----
dataURL <- "https://www.data.gouv.fr/fr/datasets/r/6d9b33e5-667d-4c3e-9a0b-5fdf5baac708"
elec <- read.xlsx(dataURL,sheet=1)

## On ne conserve que les données pour la région PACA ----

elec_paca <- elec %>% 
  filter(Code.du.département %in% c("04", "05", "06", "13", "83", "84"))

## On renomme les variables liées aux résultats par candidats ----

names(elec_paca) # Ceratins nom de variables sont nuls

# On recréer des nom de variables exmplicites
vars_candidats <- c("N°Panneau", "Sexe", "Nom", "Prénom", "Voix", "%.Voix/Ins", "%.Voix/Exp")
candidats <- c("ARTHAUD", "ROUSSEL", "MACRON", "LASSALLE", "LE_PEN", "ZEMMOUR", "MELENCHON", "HIDALGO", "JADOT", "PECRESSE", "POUTOU", "DUPONT-AIGNAN")

names_corr <- lapply(candidats, 
                     function(x){
                       paste0(x, "_", vars_candidats)
                       })
names_corr <- unlist(names_corr)

# On définit les noms de variables pour toutes la base de données
names(elec_paca)
names_new <- c(names(elec_paca)[1:19], names_corr)
names(elec_paca) <- names_new

## On crée un code unique par commune ----

elec_paca$ident_commune <- paste0(elec_paca$Code.du.département, elec_paca$Code.de.la.commune)


# Données socio-démo ###########################################################

## Import des données ----
soc_dem <- read_delim("socdem.csv", delim = ";", 
                      escape_double = FALSE, trim_ws = TRUE)

## on ajoute un "0" devant certains codes communes parce disparu à l'export ----

soc_dem <- soc_dem %>%
  mutate(Code = as.character(Code)) %>%
  mutate(Code = if_else(str_length(Code) == 4, 
                        paste0("0", Code), 
                        Code))

## On ajoute ces données socio-déom aux données éléctroales
elec_paca <- left_join(elec_paca, soc_dem, by = c("ident_commune" = "Code"))


## Coordonnées géographiques ###################################################

library(sf)
library(mapview)
library(mapsf)
library(viridis)

# charger la géométrie avec sf
comsf <- st_read(dsn = "geometry/COMMUNE.shp", 
                 stringsAsFactors = F)

# filtrer comme un data frame
comsf_paca <- comsf %>% 
  filter(INSEE_DEP %in% c("04", "05", "06", "13", "83", "84")) %>% 
  rename(ident_commune = INSEE_COM)



plot(comsf_paca$geometry)

# interroger le système de coordoonées 
st_crs(comsf_paca)

# projeter le fond de carte dans le référentiel Lambert93 (adapté pour la France)
comsf_paca <- st_transform(comsf_paca, crs = 2154)

# visualiser la géométrie
plot(comsf_paca$geometry)

# joindre les données attributaires
comsf_paca <- left_join(comsf_paca, elec_paca, by = "ident_commune")

# vérifier qu'il s'agit toujours d'un objet sf...
class(comsf_paca)










