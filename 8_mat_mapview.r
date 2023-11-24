rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(sf)
library(mapview)
library(leafsync) # multiple figures
library(units)
library(raster)

# ref 5

# 8.1.2
dem <- raster("8/DEM.tif")
print(summary(getValues(dem))) # min -30.5

# Combien de cellules possèdent une valeur d’élévation négative
ind <- which(getValues(dem) < 0) # which(getValues(dem) < 0 | getValues(dem) > 1000)
nombre <- length(ind)
# print(nombre)

# Accéder aux valeurs de ces cellules en utilisant les indices ind identifiés:
# print(getValues(dem)[ind]) # indice

# Filtrer/exclure ces valeurs 
dem[ind] <- NA
print(summary(getValues(dem)))

# Filtrer en utilisant les coordonnées spatiales des cellules
# extent() pas de mapview
ext <- extent(c(-72, -71.5, 45.2, 45.8)) # coin inférieur gauche, coin supérieur droit, dans un SCR de même unité
# print(ext)
plot(dem)
plot(ext, add = TRUE) 

# crop() gains d’efficacité en faisant appel à crop() avant d’utiliser mask()
dem_cr <- crop(dem, ext)
# print(mapview(dem_cr))

# mask() autres formes POLYGON POINT qu'un rectangle xy, gains d’efficacité en faisant appel à crop() avant d’utiliser mask()
# `mat` est une matrice 7x2 des coordonnées du polygone.
mat <- matrix(c( -72.5, 45.8,
                 -72, 45.5,
                 -72.5, 45.2,
                 -71.5, 45.4,
                 -71.7, 45.6,
                 -71.5, 45.7,
                 -72.5, 45.8), ncol = 2, byrow = TRUE)

# nous transformons mat en un data frame puis en un objet de class `sf`
pol <- st_as_sf(
  data.frame(
    var = 1,
    geom = st_sfc(st_polygon(x = list(mat)))
  ),
  crs = st_crs(dem) # 32198
) # type POLYGON
dem_ma <- mask(dem, pol) # inverse = TRUE pour inverse le masque
# print(mapview(dem_ma))
print(summary(getValues(dem_ma)))

# extent()
# print(extent(dem))
# print(extent(dem_cr)) # L’étendue du raster retourné par la fonction crop() sera différente de celle du raster initial
# print(extent(dem_ma)) # La fonction mask() préserve l’étendue spatiale

# extract(), au lieu de mask() pour plusieurs polygones
# dem_ex <- extract(dem, pol)
# print(length(dem_ex[[1]]))
# print(summary(dem_ex[[1]]))





# 8.1.3

# merge () avoir la même résolution et posséder le même système de coordonnées de référence (SCR)
dem_ma_inv <- mask(dem, pol, inverse = TRUE)
# print(paste0(res(dem_ma), res(dem_ma_inv)))
dem_merge <- merge(dem_ma, dem_ma_inv)
# print(mapview(dem_merge))

# valeur des pixels se superposant diffère, priorité
dem_cr_plus <- dem_cr + 100
dem_merge_ordre1 <- merge(dem_cr_plus, dem_ma)
dem_merge_ordre2 <- merge(dem_ma, dem_cr_plus)
# print(leafsync::latticeView(mapview(dem_merge_ordre1), mapview(dem_merge_ordre2), ncol = 1))

# mosaic() préciser comment la valeur des pixels redondants est calculée, avoir la même résolution et posséder le même système de coordonnées de référence (SCR)
# dem_mosaic  <- mosaic(dem_cr_plus, dem_ma, fun = min) # max, mean
# print(mapview(dem_mosaic))