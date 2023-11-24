rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(sf)
library(mapview)
library(leafsync) # multiple figures
library(units)
library(elevatr)

pt_max <- function(dem, subset){
  subset <- st_transform(subset, crs = 32198)
  dem_subset <- mask(dem, subset) # type matricielle
  # print(mapview(dem_subset))
  # imax <- which(getValues(dem_subset) ==  max(getValues(dem), na.rm = TRUE)) # ~maxValue(G), quantile, mean, min, median
  imax <- which.max(getValues(dem_subset))
  # ref 8.1.3.1
  # df_max <- as.data.frame(dem_subset[imax, drop = FALSE], xy = TRUE, centroid = TRUE, na.rm = TRUE) # drop = FALSE, conserver le format matriciel pour xy
  coordmax_df <- as.data.frame(xyFromCell(dem_subset,imax)) # coordonnée en xy, cellule id return xy type data.frame
  pt_max <- st_as_sf(coordmax_df, coords = c("x", "y"), crs = 32198) 
  map_point <- mapview(dem_subset) + mapview(pt_max,  col.regions = "red")
  # print(map_point)
  return(pt_max)
}

pt_buffer_dem <- function(dem, pt, dist){
  return(mask(dem, st_buffer(pt, dist = dist)))
}

# ref 5

dem <- raster("8/DEM.tif")

# extent() pas de mapview
parcs <- st_read("8/parcs_sherbrooke/parcs_sherbrooke.shp")
ext <- extent(parcs[1, ]) # coin inférieur gauche, coin supérieur droit, dans un SCR de même unité
# print(ext)
# plot(dem)
# plot(ext, add = TRUE)

# crop() gains d’efficacité en faisant appel à crop() avant d’utiliser mask()
dem <- projectRaster(dem, crs = crs(parcs)) # dans un SCR de même unité, métrique
print(mapview(dem) + mapview(parcs, zcol = "TRQ_NM_")) # noms des parcs nationaux
dem_cr <- crop(dem, parcs[1, ])
# print(mapview(dem_cr))





# qna ref 5, type POLYGON
# Parmi les quatre parcs nationaux de la région de Sherbrooke, lequel dispose du plus haut sommet?
sf_point_max <- pt_max(dem, parcs)

# parcs[st_intersects(parcs, sf_point_max, sparse = FALSE), ]$TRQ_NM_
parc_megantic <- parcs[sf_point_max, ]
print(parc_megantic$TRQ_NM_)





# 8.2.1 Identifier le point d’élévation maximale sur une carte du parc du Mont-Orford.
# Lire la géodatabase du réseau de la SÉPAQ, type polygone
# parcs <- st_layers("7/parcs.gdb") # lire dossier
# print(parc)
parcs_nationaux <- st_read("7/parcs.gdb", layer = "terpnq_s") # ref 4.1.6_vect_couche
# Pour accéder à la première colonne.
parcs_nationaux <- parcs_nationaux[,"TRQ_NM_TER"] # ou [, 13] municipalites$HAP_NM_TOP ne conserve pas le type de donnée
# Renommer
print(names(parcs_nationaux))
names(parcs_nationaux) <- c("nom", "Shape")

# Pour accéder à la première ligne.
parc_mo <- subset(parcs_nationaux, nom == "Parc national du Mont-Orford")

dem_mo <- mask(dem, parc_mo) 
pt_max(dem, parc_mo)





# 8.2.2 Reclassifier le raster dem_mo selon quatre classes correspondant aux valeurs comprises entre le zéro et le 1e quantile, le 1e et le 2e, le 2e et le 3e, le 3e et le 4e.
# Filtrer en utilisant la valeur des cellules, requantifier, quantile
# catégorie 1: classe d’élévation faible allant de 0 à 250 m,
# catégorie 2: classe d’élévation modérée allant de 251 à 500 m,
# catégorie 3: classe d’élévation forte allant de 501 à 1200 m.
qt_mo <- quantile(getValues(dem_mo), na.rm = TRUE) # ~maxValue(G), quantile, mean, min, median
q_25 <- as.numeric(qt_mo["25%"]) # as.numeric pour conserver seulement les chiffres
q_50 <- as.numeric(qt_mo["50%"])
q_75 <- as.numeric(qt_mo["75%"])
q_100 <- as.numeric(qt_mo["100%"])
nouvelles_classes <- matrix(c(0, q_25, 1, q_25, q_50, 2, q_50, q_75, 3, q_75, q_100, 4), nrow = 4, ncol = 3, byrow = TRUE) # NA pour exclure les classes 1
colnames(nouvelles_classes) <- c("Limite_min", "Limite_max", "Classement_quantile")
# print(nouvelles_classes)
nouvelles_classes <- reclassify(dem_mo, nouvelles_classes, rigth = FALSE) # right = FALSE l’inclusion de la borne inférieure mais pas de la borne supérieure [limite_lim, limite_max[
levels(nouvelles_classes) <- data.frame(ID=1:4, quantile = c("[0%, 25%[", "[25%, 50%[", "[50%, 75%[", "[75%, 100%]"))
# print(mapview(nouvelles_classes))





# 8.2.3 À partir du raster dem, créer un seul raster composé de zones tampons circulaires, de 10 km de rayon, autour des points d’élévation maximale de chaque parc national de la région de Sherbrooke.
pt_mo <- pt_max(dem, subset(parcs, TRQ_NM_== "Parc national du Mont-Orford"))
pt_meg <- pt_max(dem, subset(parcs, TRQ_NM_== "Parc national du Mont-Mégantic"))
pt_y <- pt_max(dem, subset(parcs, TRQ_NM_== "Parc national de la Yamaska"))
pt_f <- pt_max(dem, subset(parcs, TRQ_NM_== "Parc national de Frontenac"))

# mask() autres formes POLYGON POINT qu'un rectangle xy, gains d’efficacité en faisant appel à crop() avant d’utiliser mask()
pt_mo <- pt_buffer_dem(dem, pt_mo, 10e3)
pt_meg <- pt_buffer_dem(dem, pt_meg, 10e3)
pt_y <- pt_buffer_dem(dem, pt_y, 10e3)
pt_f <- pt_buffer_dem(dem, pt_f, 10e3)

dem_max <- merge(pt_mo, 
                        merge(pt_meg,
                              merge(pt_y, pt_f)))

print(mapview(dem_max))
