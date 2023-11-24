# Les données matricielles Temp_vdq.tif présentent la température de surface sur le territoire de la ville de Québec.
# Le fichier Landsat_LaTuque.tif contient des données captées par le satellite Landsat sur une section du territoire de la Haute Mauricie, près de la ville de La Tuque.
# Les données Couvert_2001.tif et Couvert_2016.tif correspondent à la couverture terrestre (p. ex. forêt, champ, eau, etc.) d’une petite région du Colorado aux années 2001 et 2016

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(raster)
library(mapview)
library(leaflet)
library(sf) # syntaxe proj4string

# Les données matricielles (raster) représentent la surface terrestre par une grille régulière
# nrows et ncols correspondent respectivement au nombre de lignes et au nombre de colonnes du raster M
# xmn et xmx correspondent respectivement aux coordonnées-x minimale et maximale du raster
# ymn et ymx aux coordonnées-y minimale et maximale
# vals est un vecteur comprenant la valeur de chaque pixel, 64 pixels
# type discrètes (integer), des nombres réelles (numeric) ou valeurs logiques (logical) 0,1, ne peuvent pas prendre des valeurs de type caractères (character) mais ils peuvent tout de même prendre des valeurs catégoriques
M <- raster(nrows=8, ncols=8, xmn = 1, xmx = 5, ymn = 1, ymx = 5, vals = 1:64)
# M <- raster(res = 0.5, xmn = 1, xmx = 5, ymn = 1, ymx = 5, vals = 1:64)
M
# dimension(ncell) correspond au nombre de cellules (ou de pixels) dans le raster
# resolution correspond à la résolution des pixels
# extent correspond à l’étendue du raster définie par ses coordonnées maximales et minimales
# crs correspond aux paramètres du système de coordonnées de référence utilisé. par défaut WGS84
class(M) # RasterLayer, RasterBrick et les RasterStack





# 5.1.2 paramètres d’un raster en utilisant les fonctions correspondantes
dim(M)
ncell(M)
nrow(M)
ncol(M)
res(M) # resolution
extent(M)
crs(M)
# le SCR selon la syntaxe PROJ4.
st_crs(M)$proj4string
# +proj : le nom de la projection
# +lat_0 : la latitude de l’origine
# +lon_0 : la longitude du méridien central
# +lat_1 : la latitude du premier parallèle standard35
# +lat_2 : la latitude du deuxième parallèle standard
# +x_0 : le faux est (false easting; dans le cas de projection transverse comme UTM)
# +y_0 : le faux nord (false northing)
# +datum : le nom du datum
# +units : les unités (mètres, pieds, etc.)
names(M) # nom de la couche de donnee

plot(M)

# print(mapview(M))

# type
# integer
z <- 1:64
# character
z_char <- z
z_char[z%%3 == 0] <- "Multiple de 3"
z_char[z%%3 != 0] <- "Autre"
# facteur
z_fact <- factor(z_char)
M_factor <- raster(nrows = 8, ncols = 8, xmn = 1, xmx = 5, ymn = 1, ymx = 5, 
                   vals = z_fact)
# print(mapview(M_factor))





# 5.1.3 accéder
zz=c(0.2,0.3, NA,NA,0.4,0.3,0.6,0.4,0.6,0.4, 1,NA,1,0.1,0.3,0.4,0.6,0.4,NA,0.2,0.1,1,0.1,0.3,0.5)
G <- raster(nrows = 5, ncols = 5, xmn = 1, xmx = 3.5, ymn = 1, ymx = 3.5, vals = zz)

# Accéder à la valeur de la cellule à la position (3,2), ligne colonne
G[3, 2]

G_df <- as.data.frame(G, xy = TRUE) # centroids = TRUE
G_df[13,] # index

# print(mapview(G))





# 5.1.4 Statistique de bases
max(getValues(G), na.rm = TRUE) # ~maxValue(G), quantile, mean, min, median
print(summary(getValues(G))) # idée de la distribution des valeurs, nombre de cellules NA, summary is an estimate based on a sample of 1e+05 cells (3.39% of all cells) réalisés sur un échantillon de 100 000 cellules choisies aléatoirement donc 3.39%
# ou
summary(G, maxsamp = ncell(G))
# ou
cellStats(G, max, na.rm = TRUE) # quantile, mean, min, median, sd, range minimum maximum, sum

hist(getValues(G), # toutes les cellules, break = 10
     main = "",
     xlab = "Valeurs",
     ylab = "Fréquence",
     col = "darkorange")

boxplot(G,
        main = "",
        ylab = "Valeurs",
        col = "darkorange")





# 5.1.6 T_vdq ville de quebec, section
T_vdq <- raster("5/Temp_vdq.tif")

# print(mapview(T_vdq, maxpixels = ncell(T_vdq), # par défaut 500 pixels max
#         map.types = "OpenStreetMap",
#         legend = TRUE,
#         layer.name = 'ICU')) # nom de la légende

T_sec <- T_vdq[351:450, 351:450, drop = FALSE] # section, drop = False, permet un nouvel objet type raster au lieu de vecteur

# print(mapview(T_sec, map.types = "OpenStreetMap", legend = TRUE, layer.name = 'ICU'))





# 5.1.7 pixels refactoriser
T_fact4_moy <- aggregate(T_sec, fact = 4)
T_fact4_max <- aggregate(T_sec, fact = 4, fun = 'max') # selon les valeurs maximum
T_fact8_moy <- aggregate(T_sec, fact = 8)
T_fact8_max <- aggregate(T_sec, fact = 8, fun = 'max') # selon les valeurs maximum


mapviewOptions(basemaps = "OpenStreetMap")
map_fact4_moy <- mapview(T_fact4_moy, legend = FALSE, homebutton = FALSE)
map_fact4_max <- mapview(T_fact4_max, legend = FALSE, homebutton = FALSE)
map_fact8_moy <- mapview(T_fact8_moy, legend = FALSE, homebutton = FALSE)
map_fact8_max <- mapview(T_fact8_max, legend = FALSE, homebutton = FALSE)

print(leafsync::latticeView(map_fact4_moy, map_fact4_max, map_fact8_moy, map_fact8_max, ncol = 2))





# 5.1.8, ref 4.1.3-5_cartes.r, transformer un raster https://spatialreference.org/
proj_mtm7 <- "+proj=tmerc +lat_0=0 +lon_0=-70.5 +k=0.9999 +x_0=304800 +y_0=0 +ellps=GRS80 +units=m +no_defs"
T_mtm7 <- projectRaster(T_sec, crs = proj_mtm7)

par(mfrow = c(1,2)) # plot parce que donnee non native, ref 4.1.3-5_cartes pour donnee vectorielles et natives
plot(T_sec, main = "Québec Lambert", legend = FALSE) # projection conique conforme de Lambert, lcc
plot(T_mtm7, main = "MTM fuseau 7")





# 5.1.9 écrire
# writeRaster(T_mtm7,"Module5/Module5_donnees/ICU_vdq_MTM7.tif")





# 5 Landsat_LaTuque.tif qui correspond aux données satellitaires captées par Landsat sur une section du territoire de la Haute Mauricie, près de la ville de La Tuque
# rasterlayer
S <- raster("5/Landsat_LaTuque.tif")
print(S)
# print(mapview(S)) # band 1 de 3

# rasterbrick, multi-bandes
S_mult <- brick("5/Landsat_LaTuque.tif")
print(S_mult) # names:, accéder par S_mult$Landsat_LaTuque_1
# print(mapview(S_mult))

nom_fichier <- "5/Landsat_LaTuque.tif"

SR <- raster(nom_fichier, band = 1) #lecture de la bande rouge
SG <- raster(nom_fichier, band = 2) #lecture de la bande verte (green)
SB <- raster(nom_fichier, band = 3) #lecture de la bande bleu

# rasterstack, combiner avec rasterlayer ayant la même étendue, la même résolution et le même SCR
raster_1 <- raster(res = res(SR), ext = extent(SR), crs = crs(SR)) # création de raster fictif pour combiner avec SR
values(raster_1) = 1:ncell(SR)
R_stack <- stack(raster_1, SR)
print(R_stack)
# print(mapview(R_stack))

mapviewOptions(raster.palette = gray.colors(256)) # palette de gris de 256 tons différents
Map_SR <- mapview(SR, homebutton = FALSE)
Map_SG <- mapview(SG, homebutton = FALSE)
Map_SB <- mapview(SB, homebutton = FALSE)
# print(leafsync::latticeView(Map_SR, Map_SG, Map_SB, ncol = 3))

viewRGB(S_mult, r = 1, g = 2, b = 3) # band
# writeRaster(S_mult, "Module5/Module5_donnees/LaTuque-copie.tif")

