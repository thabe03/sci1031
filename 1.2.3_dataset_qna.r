rm(list = ls())
setwd("G:/Mon disque/SCI1031/TN1/1.2.3/Module1_donnees")
villes <- read.csv("villes_qc.csv")

# 1b Quelles sont les dimensions du data frame villes et comment se nomment ses attributs (c’est-à-dire le nom de ses colonnes)?
dim(villes)
names(villes)

# 1c Utiliser la fonction str() pour produire un résumé du contenu du data frame villes. Déterminer à quelle classe appartient chaque attribut.
str(villes)

# 1d Transformer l’attribut “regadm” en facteur et déterminer son nombre de niveaux.
villes$regadm <- as.factor(villes$regadm)
levels(villes$regadm)

# 2a Nous souhaitons créer un data frame comprenant seulement la ligne de villes pour laquelle l’attribut “munnon” prend la valeur “Montréal”.
Mtl <- villes[villes$munnom=="Montréal",]

# Remarquer que l’expression villes$munnom=="Montréal" est un vecteur logique qui prend la valeur vrai (TRUE) lorsque le nom de municipalité est Montréal, et la valeur faux (FALSE) dans le cas contraire (c’est-à-dire pour les 1130 autres municipalités)
class(villes$munnom=="Montréal")

# 3a Créer un histogramme de la distibution de la population des villes du Québec comprenant une dizaine de bandes. Utiliser la fonction log10() pour représenter la taille des populations. Préciser les axes de votre graphique adéquatement.
hist(log10(villes$mpopul), breaks = 10,
     main = "",
     xlab = "Nombre d'habitants",
     ylab = "Nombre de villes",
     col = "darkorange",
     xlim = c(0, 7),
     ylim = c(0, 400),
     xaxt='n'  # ceci retire le nom des ticks sur l'axe x.
)
axis(side =1 , at = 0:7,
     labels = c("1","10","100","1000",
                expression(10^4) , expression(10^5),
                expression(10^6), expression(10^7)))

# 3b
villes_Outaouais <- villes[villes$regadm=="Outaouais (07)",]
boxplot(villes_Outaouais$msuperf,
        main = "Superficie des municipalités de l'Ouatouais",
        ylab = expression(paste("Superficie (km"^"2",")")),
        col = "deepskyblue"
)