
# Packages
library(dplyr)
library(stringr)
library(readr)
library(openxlsx)

# Résultats des législatives #################################################

## Téléchargement des données ----
dataURL <- "https://www.data.gouv.fr/fr/datasets/r/6d9b33e5-667d-4c3e-9a0b-5fdf5baac708"
elec <- read.xlsx(dataURL,sheet=1)

## On renomme les variables liées aux résultats par candidats ----
names(elec) # Certains noms de variables sont nuls

# On recréer des nom de variables explicites
vars_candidats <- c("N°Panneau", "Sexe", "Nom", "Prénom", "Voix", "%.Voix/Ins", "%.Voix/Exp")
candidats <- c("ARTHAUD", "ROUSSEL", "MACRON", "LASSALLE", "LE_PEN", "ZEMMOUR", "MELENCHON", "HIDALGO", "JADOT", "PECRESSE", "POUTOU", "DUPONT-AIGNAN")

names_corr <- lapply(candidats, 
                     function(x){
                       paste0(x, "_", vars_candidats)
                       })
names_corr <- unlist(names_corr)

# On définit les noms de variables pour toutes la base de données
names(elec)
names_new <- c(names(elec)[1:19], names_corr)
names(elec) <- names_new

## On ne conserve que les données pour la région PACA ----

elec_paca <- elec %>% 
  filter(Code.du.département %in% c("04", "05", "06", "13", "83", "84"))

## On crée un code unique par commune ----

elec_paca$ident_commune <- paste0(elec_paca$Code.du.département, elec_paca$Code.de.la.commune)


# Données socio-démo ###########################################################

## Import des données ----
soc_dem <- read_csv2("socdem.csv", trim_ws = TRUE,
                     col_types = cols(voiture = col_double()))

## On ajoute un "0" devant certains codes communes parce disparu à l'export ----

soc_dem <- soc_dem %>%
  mutate(Code = as.character(Code)) %>%
  mutate(Code = if_else(str_length(Code) == 4, 
                        paste0("0", Code), 
                        Code))

## On ajoute ces données socio-démo aux données éléctorales ----
elec_paca <- left_join(elec_paca, soc_dem, by = c("ident_commune" = "Code"))


# Données revenu ###########################################################

## Import des données ----
revenu <- read_csv2("FILO2018_DISP_COM.csv", trim_ws = TRUE)

## Filtre par département ----
revenu <- revenu %>%
  mutate(DEPT = substr(CODGEO, 1, 2))

revenu <- revenu %>%
  filter(DEPT %in% c("04", "05", "06", "13", "83", "84"))

## On ne garde que la colonne qui nous intéresse et on la renomme ----
revenu <- revenu %>%
  select(CODGEO, Q218)

names(revenu)[names(revenu)=="Q218"] <- "med_rev"

## On ajoute aux données éléctroales ----
elec_paca <- left_join(elec_paca, revenu, by = c("ident_commune" = "CODGEO"))

library(questionr)
freq(is.na(elec_paca$med_rev))
# 834 réponses ce qui semble correct


# Données taux de pauvreté ######################################################

## Import des données ----
pauvres <- read_csv2("FILO2018_DISP_Pauvres_COM.csv", trim_ws = TRUE)

## Filtre par département ----
pauvres <- pauvres %>%
  mutate(DEPT = substr(CODGEO, 1, 2))

pauvres <- pauvres %>%
  filter(DEPT %in% c("04", "05", "06", "13", "83", "84"))

## On ne garde que la colonne qui nous intéresse ----
pauvres <- pauvres %>%
  select(CODGEO, TP6018)

names(pauvres)[names(pauvres)=="TP6018"] <- "taux_pauvrete_60"

## On ajoute aux données éléctroales ----
elec_paca <- left_join(elec_paca, pauvres, by = c("ident_commune" = "CODGEO"))

freq(is.na(elec_paca$taux_pauvrete_60))
# Attention seulement 309 réponses


# Nettoyage ###################################################################

rm(dataURL)
rm(names_corr)
rm(names_new)
rm(vars_candidats)
rm(soc_dem)
rm(pauvres)
rm(revenu)


# Coordonnées géographiques ###################################################

library(sf)
library(mapview)
library(mapsf)
library(viridis)

## charger la géométrie avec sf ----
comsf <- st_read(dsn = "geometry/COMMUNE.shp", 
                 stringsAsFactors = F)

## filtrer comme un data frame ----
comsf_paca <- comsf %>% 
  filter(INSEE_DEP %in% c("04", "05", "06", "13", "83", "84")) %>% 
  rename(ident_commune = INSEE_COM)

plot(comsf_paca$geometry)

## interroger le système de coordonnées ----
st_crs(comsf_paca)

# projeter le fond de carte dans le référentiel Lambert93 (adapté pour la France)
comsf_paca <- st_transform(comsf_paca, crs = 2154)

# visualiser la géométrie
plot(comsf_paca$geometry)

## joindre les données attributaires ----
comsf_paca <- left_join(comsf_paca, elec_paca, by = "ident_commune")

# vérifier qu'il s'agit toujours d'un objet sf...
class(comsf_paca)


# Nettoyage #####################################################################
rm(comsf)


depsf <- st_read(dsn = "geometry/DEPARTEMENT.shp", 
                 stringsAsFactors = F)
depsf_paca <-  depsf %>%
  filter(INSEE_DEP %in% c("04", "05", "06", "13", "83", "84"))

