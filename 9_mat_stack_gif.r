rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(raster)
library(ncdf4)
library(lubridate) # année mois ...
library(animation)
library(tmap)

# les couches sont ordonnées dans le temps (elles ont été combinées ainsi) et nous avons 1 couche par mois sur 10 ans

# temperature minimale par mois
mint <- stack("9/mint/mint.nc")
# temperature maximale par mois 
maxt <- stack("9/maxt/maxt.nc")
# total des precipitations par mois 
pcp <- stack("9/pcp/pcp.nc")
# print(mint) # le nombre de couches, et les valeurs affichées correspondent aux valeurs minimales et maximales pour chacune des différentes couches

extent(mint)
projection(mint)
ncell(mint)
dim(mint)
nlayers(mint) # 120 objets de classe rasterLayer

# print(mint[[1]]) # 01-01-2007 type rasterLayer
# print(mint[[1:12]]) # 2007 type rasterLayer
# print(mint[[seq(9, by = 12, length.out = 10)]]) #9/12*10 01-09-2007, 01-09-2008...
# print(mint[[1:12]][1:10]) # top 10 des rows des 12 premiers mois de 2007

# stack () combiner des couches du même rasterStack
comb.rasterStack <- stack(mint[[1]], maxt[[1]], pcp[[1]])

# apply() trouver le minimum de chaque couche
val <- getValues(mint)
dim(val) # 3600 120
# print(apply(val, 2, min)) # sélectionne la deuxième colonne
# ou print(summary(mint))

# calc() trouver le minimum d'un rasterStack
# print(calc(mint, min)) # return rasterLayer

# apply() trouver le quantile de chaque couche
# print(apply(val, 2, quantile))[, 1:10] # sélectionne la deuxième colonne
# ou print(quantile(mint)[1:10, ])
# ou print(calc(mint, quantile)) # return rasterBrick

library(sf)
# projectRaster() équivalent de st_transform(t_vect, crs=30198) pour t_mat
mint_merc <- projectRaster(mint, crs = st_crs(3857)$proj4string) # mercator
parcs <- st_read("8/parcs_sherbrooke")
frontenac <- st_transform(parcs[1, ], st_crs(mint))
# crop() gains d’efficacité en faisant appel à crop() avant d’utiliser mask()
mint_crop <- crop(mint, extent(c(-71.5, -71, 45, 45.5))) # ref 8
mint_mask <- mask(mint, frontenac) # return rasterBrick ref 8
val_frontenac <- extract(mint, frontenac) # return matrice ref 8 col 1, row 2
# plot(mint[[117:120]], main = c("09/2016", "10/2016", "11/2016", "12/2016")) # plot(mint, maxnl = 25)
# print(mapview(mint[[117:120]]))

moan <- paste0(rep(1:12, 10), "/", rep(2007:2016, each = 12)) # répète 1:12*10, répète i*12
# my() formaliser
temps <- my(moan)
mint <- setZ(mint, temps, name = "temps") # ajouté aux métadatas
# print(getZ(mint)[1:10])
# print(mint[[which(year(getZ(mint)) == 2015)]]) # ref 8 which()





# 9.1.3.3 Quel mois dispose du plus faible niveau de pluviométrie dans la région du Parc national du Mont-Mégantic?

# CELLULES DU PARC NATIONAL DU MONT-MÉGANTIC
# ref 7.1.3
# Lire la géodatabase du réseau de la SÉPAQ, type polygone
# parcs <- st_layers("7/parcs.gdb") # lire dossier
# print(parc)
parcs <- st_read("7/parcs.gdb", layer = "terpnq_s") # ref 4.1.6_vect_couche
# Pour accéder à la première colonne.
parcs <- parcs[,"TRQ_NM_TER"] # ou [, 13] municipalites$HAP_NM_TOP ne conserve pas le type de donnée
# Renommer
names(parcs) <- c("nom", "Shape")
megantic <- subset(parcs, nom == "Parc national du Mont-Mégantic")
megantic_buf <- st_buffer(megantic, dist = 5000) # en m st_crs(parcs)$units
# print(mapview(megantic, col.regions = "seagreen") + mapview(megantic_buf, col.regions = "lightgreen"))
pcp_megantic <- mask(pcp, st_transform(megantic_buf, st_crs(pcp)))
plot(pcp_megantic[[1:4]])

# CALCULER LA PLUVIOMÉTRIE MOYENNE POUR TOUTES LES COUCHES
# apply()
mean_pcp <- apply(getValues(pcp_megantic), 2, mean, na.rm = TRUE) # sélectionne la deuxième colonne
# plot(temps, mean_pcp) # colonne temps

# DÉTERMINER LE MOIS DE PLUS FAIBLE PLUVIOMÉTRIE MOYENNE
# Nous sommes maintenant en mesure de chercher le minimum de ces moyennes.
min(mean_pcp) # return Object
# Mois de cette valeur
getZ(mint)[which.min(mean_pcp)]
# ou temps[which.min(mean_pcp)] # retrouve la métadatas et return temps == temps
# C’est la couche 71 qui présente la plus faible valeur moyenne des précipitations totales, cette couche correspond au mois de novembre 2012.





# 9.1.3.4 Où se situe le (ou les) point(s) le(s) plus chaud(s) dans la région du Parc national du Mont-Mégantic?
# CELLULES DU PARC NATIONAL DU MONT-MÉGANTIC
# maxt <- setZ(maxt, temps, name = "temps") # ajouté aux métadatas
# maxt_megantic <- mask(maxt, st_transform(megantic, st_crs(maxt)))
# max_maxt <- apply(getValues(maxt_megantic), 2, max, na.rm = TRUE)
# getZ(maxt)[which.min(max_maxt)] # ref 8 which()
maxt <- setZ(maxt, temps, name = "temps") # ajouté aux métadatas
maxt_megantic <- mask(maxt, st_transform(megantic, st_crs(maxt)))
max_maxt <- apply(getValues(maxt_megantic), 2, max, na.rm = TRUE)
# Nous sommes maintenant en mesure de chercher la température maximum de ce jour.
max(max_maxt)
# Mois de cette valeur
getZ(maxt)[which.max(max_maxt)] # ref 8 which()
# Les coordonnées spatiales auxquelles cette température a été enregistrée ref 8.2
xyFromCell(maxt_megantic[[which.max(max_maxt)]], which.max(maxt_megantic[[which.max(max_maxt)]])) # rasterLayer, id





# 9.1.3.5, 6 Profils de temperatures et de precipitations
profil <- function(ras, masque) { # return numeric
  masque_crs <- st_transform(masque, st_crs(ras))
  ras_extract <- extract(ras, masque_crs)[[1]] # return matrice ref 8 col 1, row 2
  return(apply(ras_extract, 2, mean, na.rm = TRUE))
}

par(mfrow = c(3, 1), mar = c(4, 4, 1, 1)) # bottom, left, top, right
plot(getZ(mint), profil(mint, megantic_buf), type = "l", ylab = "Températures minimales (°C)", xlab = "")
plot(getZ(mint), profil(maxt, megantic_buf), type = "l", ylab = "Températures maximales (°C)", xlab = "")
plot(getZ(mint), profil(pcp, megantic_buf), type = "h", ylab = "Précipitations totales (mm)", xlab = "Années") # cumulé total

plot_profil <- function(n, pr_mint, pr_maxt, pr_pcp, temps) {
  id <- seq_len(n) # plus vite, mais plus limité
  par(mar = c(4, 4.5, 1, 4.5), las = 1) 
  plot(range(temps), c(min(pr_mint), max(pr_maxt)), type = "n", 
       xlab = "Années", ylab = "Températures minimales et maximales (°C)")
  polygon(c(temps[id], rev(temps[id])), c(pr_mint[id], pr_maxt[rev(id)]), col = "#aaaaaa", border = "#6a6a6a")
  par(new = TRUE)
  plot(range(temps), range(pr_pcp), type = "n", axes = FALSE, ann = FALSE) # type="n"~vide pour un polygone pour un plot
  points(temps[id], pr_pcp[id], type = "h", col = "#c62b63", lwd = 2)
  axis(4)
  mtext("Précipitations totales (mm)", side = 4, line = 3, las = 0, col = "#c62b63")
  mtext(temps[n], side = 1, line = 2.5, at = as.Date("2007-01-01"),cex = 1.4)
}

pr_mint <- profil(mint, megantic_buf)
pr_maxt <- profil(maxt, megantic_buf)
pr_pcp <- profil(pcp, megantic_buf)

plot_profil(40, pr_mint, pr_maxt, pr_pcp, getZ(mint)) # 40 premiers mois, numeric, numeric, numeric

# saveGIF({
#   for (i in seq(3, 120, by = 3)) plot_profil(i, pr_mint, pr_maxt, pr_pcp, temps)
# }, movie.name = "9_mat_plot.gif", ani.height = 400, ani.width = 600)





# 9.2 Créer une animation de la carte des températures minimums pour les 12 premiers mois de l’année 2007. Votre animation doit avoir les caractéristiques suivantes :
# Couvrir seulement la région allant de 71.4°Ouest à 71°Ouest, en longitude, et de 45°Nord à 45.4°Nord en latitude dans le dans le système géodésique mondial WGS84.
region <- extent(c(-71.5, -71, 45, 45.4))
mint_region <- crop(mint,region)
# Comprendre une légende affichant les températures de -24 C à +24 C par bond de 2 C.
L <- seq(-24, 24, by = 2)
# Utiliser une palette de couleurs allant d’une couleur froide pour les températures négatives vers une couleur chaude pour les températures négatives.
Pal <- colorRampPalette(c("blue","grey", "red"))
Coul <- Pal(length(L))
noms <- c(L[1:12], L[14:25])
noms <- as.character(noms)

fct_carte <- function(RS,t){ # rasterStack, mois t pour temps
  tm_shape(RS[[t]])+ #couche correspondant au mois t
    tm_raster(palette=Coul,  
              title = "",     # pas de titre pour la légende
              breaks = L,
              labels=noms, 
              legend.is.portrait = TRUE,
              legend.reverse = TRUE) +  # assure que les températures négatives sont au bas de la légende
    # Avoir un titre qui comprend le mois (en français) et l’année.
    tm_layout(main.title = paste0("Températures minimales (°C) - ",month(temps[t], label = TRUE, abbr = FALSE)," ",year(temps[t])),                              main.title.size = 0.9, 
              legend.outside = TRUE, 
              legend.outside.position = "right", # Afficher la légende à droite de la carte.
              legend.text.size = 0.8, 
              legend.format = list(text.align = "right"))
}

fct_carte(mint_region,3)

anim_carte <- function(RS, T){
  for(i in 1:T){
    print(fct_carte(RS,i))
  }
}

saveGIF(anim_carte(mint_region, 12), movie.name = "9_mat_tmap", ani.height = 500, ani.width = 500)