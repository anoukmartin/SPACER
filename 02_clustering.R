
# On va faire une CAH sur le vote de droite 

library(FactoMineR)
# On définit la liste des candidtas de droite 
droite_candidiats <- c("LE_PEN", "ZEMMOUR", "PECRESSE", "DUPONT-AIGNAN", "MACRON")

# On ne conserve que les données sur les voix exprimées pour ces candidats
paca_droite <- elec_paca %>%
  select(c("ident_commune", paste0(droite_candidiats, "_%.Voix/Exp")))
row.names(paca_droite) <- paca_droite$ident_commune
paca_droite$ident_commune <- NULL

# Réalisation d'une ACP et d'une CAH
acp <- PCA(paca_droite, ncp = 10, graph = FALSE)
cah <- HCPC(acp, graph = FALSE, nb.clust = 4)

plot(cah, choice = "tree") # dendrograme
plot(cah, choice = "bar") # Saut d'inertie
# 4 classes paraîssent pertinentes au vu des sauts d'inerties


# On ajoute l'apparentance aux cluster sur les autres bases de données

# On garde seulement l'appartenance des communes aux 4 groupes
tab <- cah$data.clust %>%
  select(clust) 
tab$ident_commune <- rownames(tab)

# Ajout sur les données spatiales  
comsf_paca_clust<- left_join(comsf_paca, tab)

# Une carte pour voir
library(RColorBrewer)
palette <- brewer.pal(12, "BrBG")
nuancesdedroite <- palette[c(8, 7, 4, 5)]
mf_map(comsf_paca_clust, 
       var = "clust", 
       type = "typo", 
       border = "grey60", 
       pal = nuancesdedroite,
       leg_title = "Cluster")
mf_label(comsf_paca[comsf_paca$POPULATION > 50000, ], 
         var = "NOM")

names(comsf_paca)

# Un tableau pour voir qui a voté pour qui 
library(gt)
data_clust <- elec_paca %>%
  left_join(tab) %>%
  select(c(paste0(droite_candidiats, "_Voix"), "Votants", "ident_commune", "clust"))  %>%
  group_by(clust) %>%
  summarise(Votants = sum(Votants),
            LE_PEN_Voix = sum(LE_PEN_Voix),
            ZEMMOUR_Voix = sum(ZEMMOUR_Voix), 
            `DUPONT-AIGNAN_Voix` = sum(`DUPONT-AIGNAN_Voix`), 
            PECRESSE_Voix = sum(PECRESSE_Voix), 
            MACRON_Voix = sum(MACRON_Voix)) %>%
  mutate(`LE_PEN_%.Voix/Exp` = 100*LE_PEN_Voix/Votants, 
         `ZEMMOUR_%.Voix/Exp` = 100*ZEMMOUR_Voix/Votants, 
         `DUPONT-AIGNAN_%.Voix/Exp` = 100*`DUPONT-AIGNAN_Voix`/Votants, 
         `PECRESSE_%.Voix/Exp` = 100*PECRESSE_Voix/Votants, 
         `MACRON_%.Voix/Exp` = 100*MACRON_Voix/Votants)

data_clust %>%
  select("clust", ends_with("Voix/Exp"))%>%
  gt()
       
            



