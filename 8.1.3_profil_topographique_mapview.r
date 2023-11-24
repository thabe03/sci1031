rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(sf)
library(mapview)
library(leafsync) # multiple figures
library(units)
library(elevatr)

# ref 5

dem <- raster("8/DEM.tif")
parcs <- st_read("8/parcs_sherbrooke/parcs_sherbrooke.shp")

# Quelle est le profil topographique du sentier se rendant au plus proche de ce sommet?, type MULTILINESTRING
sentiers <- st_read("8/sentiers_sepaq/Sentier_ete_l.shp")

# Trouver l'élevation du parc d'une bd Amazon
elv_megantic <- get_elev_raster(parc_megantic, z = 13) # zoom type matrice
# print(res(elv_megantic))

# Isoler les LINESTRING se situant dans le POLYGON
sentiers_megantic <- sentiers[parc_megantic, ] # type MULTILINESTRING
# print(mapview(sentiers_megantic))

# st_nearest_feature() point d'intérêt, vecteur POLYGON LINESTRING POINT, return ID
sentier_top <- sentiers_megantic[st_nearest_feature(sf_point_max, sentiers_megantic), ] # type MULTILINESTRING
rando_sections <- subset(sentiers_megantic, Toponyme1 == sentier_top$Toponyme1)

# st_combine()
rando <- st_combine(rando_sections) # type MULTILINESTRING
# print(mapview(sentiers_megantic) + mapview(parc_megantic) + mapview(rando, color="green") + mapview(sentier_top, color="red"))

# extract()
topo_elv <- extract(elv_megantic, st_as_sf(rando), along = TRUE, cellnumbers = TRUE)
# print(dim(topo_elv))
colnames(topo_elv[[1]]) <- c("cellule_id", "elevation")
# print(head(topo_elv[[1]])) # type LIST

# xyFromCell() récupérer les coordonnées des cellule_id
df_pts <- as.data.frame(xyFromCell(elv_megantic, topo_elv[[1]][, 1])) # coordonnée en xy, [, 1] colonne 1 cellule_id return xy type data.frame
topo_pts <- st_as_sf(df_pts,
                     coords = c("x", "y"),
                     crs = st_crs(sentiers_megantic)
) # type point

# st_distance() dans un SCR d’unité de mesure métrique, type point, point à point
# dist_tout <- st_distance(topo_pts,topo_pts) # type matrice
dist_pts <- st_distance(topo_pts[-1, ], topo_pts[-nrow(topo_pts),], by_element = TRUE) # successif -1 vs 1

# cumsum()
dist_parcourue <- cumsum(dist_pts)

plot(c(0, dist_parcourue), topo_elv[[1]][, 2], # x dist_parcourue, y élevation
     main = "Profil topographique du Sentier du Mont-Mégantic", 
     xlab = "Distance depuis le sommet (en mètre)", 
     ylab = "Altitude (en mètre)", 
     type = "l", # pour utiliser une ligne
     lwd = 2 # augmente le trait de la ligne 
) 