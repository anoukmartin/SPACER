
# Préparation des données ######################################################

library(FactoMineR)
# On définit la liste des candidtas de droite 
droite_candidiats <- c("LE_PEN", "ZEMMOUR", "PECRESSE", "DUPONT-AIGNAN", "MACRON")
names(elec_paca)

# Liste des variables illustratives pour l'analyse factorielle
vars_sup <- names(elec_paca)[!str_starts(names(elec_paca), paste0("(", candidats,")", collapse = "|"))]
vars_sup
vars_sup_quali <- c("ident_commune", "Libelle", "Code.du.département", 
                    "Libellé.du.département", "Code.de.la.commune", 
                    "Libellé.de.la.commune", "Etat.saisie")
vars_sup_quanti <- vars_sup[!(vars_sup %in% vars_sup_quali)]


# On ne conserve que les données qui nous intéressent
paca_droite <- elec_paca %>%
  select(c("ident_commune", "Libellé.de.la.commune", paste0(droite_candidiats, "_%.Voix/Exp"), vars_sup)) 
row.names(paca_droite) <- paca_droite$ident_commune
paca_droite$ident_commune <- NULL
names(paca_droite)

# liste des pcs
csp <- c("agriculteurs",
         "artisans_commercants",
         "cpis", 
         "professions_intermediaires", 
         "employes",
         "ouvriers", 
         "retraites", 
         "autres_sans_activite")
pop15ans <- rowSums(paca_droite[, csp])

# Mise en forme des variables 
paca_droite <- paca_droite %>%
  # variables quanti en format numérique 
  mutate(across(.cols = any_of(vars_sup_quanti), 
                .fns = as.numeric)) %>%
  # calcul de % des pcs 
  mutate(across(.cols = any_of(csp), 
                .fns = function(x){round((100*x/pop15ans), 2)}))%>%
  # variables quali en format character
  mutate(across(.cols = any_of(vars_sup_quali), 
                .fns = as.character))


# Analyse factorielle ##########################################################
# Réalisation de l'ACP 
acp <- PCA(paca_droite, ncp = 10, graph = FALSE, 
           quanti.sup = vars_sup_quanti, quali.sup = vars_sup_quali)

# Plan des variables 
library(factoextra)
fviz_pca_var(acp, 
             col.quanti.sup = "grey50", 
             repel = TRUE)

# réalisation d'une CAH
cah <- HCPC(acp, graph = FALSE, nb.clust = 4)

plot(cah, choice = "tree") # dendrograme
plot(cah, choice = "bar") # Saut d'inertie
# 4 classes paraîssent pertinentes au vu des sauts d'inerties


# représentations des clusters #################################################

# Les couleurs pour les quartes clusters
library(RColorBrewer)
palette <- brewer.pal(12, "BrBG")
nuancesdedroite <- palette[c(8, 7, 4, 5)]


# On ajoute appartenance aux cluster sur les autres bases de données
# On garde seulement l'appartenance des communes aux 4 groupes
tab <- cah$data.clust %>%
  select(clust) 
tab$ident_commune <- rownames(tab)

# Ajout sur les données spatiales  
comsf_paca <- left_join(comsf_paca, tab)

## Représentations de l'analyse factorielle ###################################
# on refait l'ACM en ajoutant les cluster en variables illustrative
paca_droite$ident_commune <- rownames(paca_droite)
paca_droite <- left_join(paca_droite, tab)
paca_droite$ident_commune <- NULL
acp <- PCA(paca_droite, ncp = 10, graph = FALSE, 
           quanti.sup = vars_sup_quanti, quali.sup = c(vars_sup_quali, "clust"))

# On regarder le nuages de points et le cercle des variables 
fviz_pca(acp, 
         axes = c(1, 2),
         habillage = "clust", 
         geom = c("point"), 
         repel = TRUE, 
         addEllipses = TRUE)


# dendrogrames
fviz_dend(cah, 
          k_colors = nuancesdedroite, 
          horiz = TRUE, 
          cex = 0.1)
          

## Cartes #####################################################################
# Labels des communes a ploter : on prend les pref et sous pref
names(comsf_paca)
labels_communes <- comsf_paca %>% 
  filter(STATUT != "Commune simple")


# Affichez le graphique
print(graphique)

library(mapsf)
mf_theme("default")
mf_map(comsf_paca, 
       var = "clust", 
       type = "typo", 
       border = "grey50", 
       pal = nuancesdedroite,
       leg_title = "Cluster")
mf_label(labels_communes, 
         var = "NOM", 
         col = "black",
         overlap = FALSE,
         lines = TRUE,
         halo=FALSE)
mf_layout(title = "50 nuances de droite en région PACA (2022)",
          credits = "Auteurices : Bolotny, Girard, Martin, 2024. \nDonnées: Ministère de l'intérieur, 2022",
          arrow = FALSE)
mf_arrow(pos = "topright")

names(comsf_paca)


## Tableau des % de votes pour les candidiats de droits par cluster ############
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


       
    