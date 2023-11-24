rm(list = ls())
library(sf)
library(mapview)
library(spData)

# attribuer aux géométries un datum et une projection
# classe sfc, c’est-à-dire simple feature columns
# sfc est une liste d’objets sfg
# transformer un objet de classe sfg en un objet de classe sfc
# contenir l’information relative au système de coordonnées de référence (SRC) utilisé
# sans crs aucune carte, seulement les objets

# créer une géométrie multipoint
M <- rbind( c(3,5), c(5,5), c(4,1), c(2,3))
multi_point <- st_multipoint(M)

# créer une ligne
ligne <- st_linestring(M)

# créer des multi-lignes ou des multi-polygones
M1 <- rbind( c(3,5), c(5,5), c(4,1), c(2,3))
M2 <- rbind( c(1,2), c(2,2), c(2,1))
L  <- list(M1, M2)
multi_ligne <- st_multilinestring(L)

L1 <- list(rbind( c(3,5), c(5,5), c(4,1), c(2,3), c(3,5)))
L2 <- list(rbind( c(1,2), c(2,2), c(2,1), c(1,2)))
L  <- list(L1, L2)
multi_polygone <- st_multipolygon(L)

# être définies dans une liste
# connaître le SCR d’un object vectoriel
# EPSG 32198 correspondant au système de coordonnées conique conforme de Lambert dans le datum NAD83
L1 <- list(rbind( c(3,5), c(5,5), c(4,1), c(2,3), c(3,5)))
polygone1 <- st_polygon(L1)
L2 <- list(rbind( c(1,2), c(2,2), c(2,1), c(1,2)))
polygone2 <- st_polygon(L2)
polygone_sfc <- st_sfc(polygone1, polygone2, crs = 32198)
# st_sfc(point, crs = st_crs(polygone_sfc)) # référé au SCR d’un autre objet

# p1 <- c(3,5)
# p2 <- c(5,5)
# p3 <- c(4,1)
# p4 <- c(2,3)
# point1 <- st_point(p1)
# point2 <- st_point(p2)
# point3 <- st_point(p3)
# point4 <- st_point(p4)
# points_sfc <- st_sfc(point1,point2,point3,point4, crs = 32198)
# points_attribut <- data.frame(
#   nom = c("École A", "École B", "École C", "École D"),
#   nombre_eleves = c(403, 357, 280, 296),
#   ecole_publique = as.logical(c(1, 1, 0, 1))
# )
# points_sf <- st_sf(points_sfc, points_attribut)
# print(points_sf)
point_df <- data.frame(
  nom = c("École A", "École B", "École C", "École D"),
  nombre_eleves = c(403, 357, 280, 296),
  ecole_publique = as.logical(c(1, 1, 0, 1)),
  x = c(3, 5, 4, 2),
  y = c(5,  5, 1, 3)
)
points_sf <- st_as_sf(point_df, coords = c("x","y"), crs = 32198) # # sf contient les attributs (x,y) spatiaux des données tandis que la composante de classe data.frame (reste) contient les attributs non-spatiaux
# print(points_sf) # shapefile





# 4.1.3 shapefile
limites_terrestres <- st_read("4/terre/terre_shp.shp") # type POLYGON
pistes_cyclables <- st_read("4/pistes/pistes_cyclables_type.shp") # type LINESTRING
accidents_velo <- st_read("4/accidents/accidents2018_Mtl_velo.shp") # type point
# print(pistes_cyclables)

# st_bbox() l’étendue spatiale des données (Bounding box)
# print(st_bbox(limites_terrestres))





# 4.1.4 mapview optionnel, z = la colonne , layer.name = personnaliser le titre
# shapefile
map_limites_terrestres <- mapview(limites_terrestres, col.regions = "white", legend = NULL)
couleurs_voie <- c("black","goldenrod", "cornflowerblue", "darkcyan", "hotpink", "mediumpurple") 
map_pistes_cyclables <- mapview(pistes_cyclables, z = "TYPE_VOIE", color=couleurs_voie, layer.name = "Types de pistes cyclables", lwd = 1) # ~title, couleur des lignes, nous utilisons l’argument color, colors()[81], lwd réduit épaisseur de la ligne
map_accidents <- mapview(accidents_velo, color = "red", col.regions = "red", cex = 1, legend = NULL) # zcol pour groupby, contour et l’intérieur des points, cex indique la taille des cercle, par défaut 6
# print(map_limites_terrestres + map_pistes_cyclables + map_accidents)





# 4,1,5 st_transform() transformer Mercator Transversal à une projection Robin, lcc = projection conique conforme de Lambert, ref 5.1.8
# legend = FALSE désactive explicitement la légende, tandis que legend = NULL laisse la fonction décider si la légende doit être affichée en fonction du contexte
limites_terrestres_rob <- st_transform(limites_terrestres, crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
map_web <- mapview(limites_terrestres, col.regions = "red", legend = NULL, layer.name = "Mercator Web")
map_mercator <- mapview(limites_terrestres, col.regions = "yellow", legend = NULL, layer.name = "Mercator", native.crs=TRUE) # forcer à non Mercator web
map_robinson <- mapview(limites_terrestres_rob, legend = NULL, layer.name = "Robinson", native.crs=TRUE) # # forcer à non Mercator web
map <- leafsync::latticeView(map_web,map_mercator,map_robinson, ncol = 3) # multiple views, fonction existe à la fois dans la bibliothèque leafsync et dans la bibliothèque mapview mais qu’elle est obsolète dans cette dernière, nous devons préciser que nous voulons la fonction latticeView
# print(map)





# 4.1.6 vecteur couche
# le nom donné à chaque couche (layer_name), leur géométrie (geometry_type), le nombre d’objets vectoriels qu’elles contiennent (features), et le nombre d’attributs qu’elles décrivent (fields)
couches_mees <- st_layers("4/Donnees_Ouvertes_MEES.gdb")
# print(couches_mees) # 18 couches différentes

# CS, couvre un territoire qui leur est propre, ces données sont des multi-polygones.
ecoles_pub <- st_read("4/Donnees_Ouvertes_MEES.gdb", layer = "PPS_Public_Ecole") # ~shapefile
# print(names(ecoles_pub)) # tous les attributs/colonnes
print(head(ecoles_pub$NOM_OFFCL_ORGNS)) # nom école
print(head(ecoles_pub$NOM_MUNCP_GDUNO_IMM)) # nom municipalité
print(head(ecoles_pub$TYPE_CS)) # commision scolaire
print(head(ecoles_pub$SHAPE)) # position géographique
print(head(ecoles_pub$ORDRE_ENS)) # niveau d'enseignement
ecoles_pub_Mtl<- ecoles_pub[ecoles_pub$NOM_MUNCP_GDUNO_IMM == "Montréal",]
map_pub_mtl<-mapview(ecoles_pub_Mtl, z = "TYPE_CS", cex = 2, layer.name = 'Commissions scolaires')
# print(map_pub_mtl)

# sélection du layer
ecoles_priv <- st_read("4/Donnees_Ouvertes_MEES.gdb", layer = "PPS_Prive_Etablissement")
college<- st_read("4/Donnees_Ouvertes_MEES.gdb", layer = "ES_Collegial")
univ <- st_read("4/Donnees_Ouvertes_MEES.gdb", layer = "ES_Universitaire")

# where = Montréal
ecoles_priv_Mtl <- ecoles_priv[ecoles_priv$NOM_MUNCP == "Montréal",] 
college_Mtl <- college[college$NOM_MUNCP == "Montréal",]
univ_Mtl <- univ[univ$NOM_MUNCP == "Montréal",]

map_priv_mtl <- mapview(ecoles_priv_Mtl, color = "red", col.regions = "red", cex = 2)
map_college_mtl <- mapview(college_Mtl, color = "green", col.regions = "green", cex = 4)
map_univ_mtl <- mapview(univ_Mtl, color = "orange", col.regions = "orange", cex = 6)

# print(map_pub_mtl + map_priv_mtl + map_college_mtl + map_univ_mtl) # sélectionner/désélectionner chaque couche
# st_write() pour sauvegarder chacun des shapefiles





# 4.2 qna

# 1 Créer une géométrie simple de type polygone qui a la forme d’un carré de 10 unités et un carré de 4 unités de long (trou)
matrice_carre_grand  <- rbind(c(1,1), c(1,11), c(11,11), c(11,1), c(1,1))
matrice_carre_petit <- rbind(c(4,4),c(4,8),c(8,8),c(8,4),c(4,4))

polygone <- st_polygon(list(matrice_carre_grand, matrice_carre_petit))
polygone

# print(mapview(polygone))



# 2a Créer 3 géométries simples de type LINESTRING en utilisant les matrices de coordonnées suivantes
matrice_ligne1 <- rbind(c(1,1), c(2,2), c(3,1), c(4,2), c(5,1), c(6,2), c(7,1))
matrice_ligne2 <- rbind(c(1,5), c(3,3), c(7,5), c(3,5), c(4,4))
matrice_ligne3 <- rbind(c(1,3), c(3,7), c(7,8), c(10,10))

ligne1 <- st_linestring(matrice_ligne1)
ligne2 <- st_linestring(matrice_ligne2)
ligne3 <- st_linestring(matrice_ligne3)

# 2b Définir une couche de données vectorielles comprenant ces 3 lignes et lui attribuer la projection Web de Mercator.
lignes <- st_sfc(ligne1, ligne2, ligne3, crs = 3857)

# 2c Le premier attribut correspond à un nom de votre choix pour désigner chaque ligne, et le deuxième attribut correspond au nombre d’extrémités dans chaque ligne.
lignes_att <- data.frame(
  nom = c("Zigzag", "Tourbillon", "Tordu"),
  nombre = c(nrow(matrice_ligne1), nrow(matrice_ligne2), nrow(matrice_ligne3))
)
lignes_sf <- st_sf(lignes, lignes_att)

# 2d Visualiser cette couche de données vectorielles. Assurez-vous d’avoir une légende identifiant chaque ligne par son nom.
# print(mapview(lignes_sf, z="nom", layer.name = "Nom des lignes"))



# 3a Combien d’éléments spatiaux contient la couche nz et de quel type de géométrie sont ces éléments ?
data(nz)
nrow(nz) # 16 éléments
st_geometry_type(nz)

# 3b Trouver le nombre d’attributs de nz et leur nom
ncol(nz)
names(nz)
# levels(factor(nz$Island))


# 3c Trouver le code EPSG, le nom et l’unité de mesure de la projection utilisée
# le système de coordonnées projetées (Projected CRS)
st_crs(nz)$epsg
st_crs(nz)$Name # New Zealand Geodetic Datum
st_crs(nz)$units

# 3d Transformer la projection de nz pour la projection conique conforme de Lambert (LCC).
nz_lcc <- st_transform(nz, crs = 32198) # 4.1.5.r ligne 42 retourner en mapview pour forcer la vue lcc ou robin etc...
st_crs(nz_lcc)$proj4string # projection de Mercator transverse +proj=tmerc +proj=lcc +proj=robin North America Datum 1983 +datum=NAD83

# 3e Visualiser la couche nz en illustrant chaque région (polygone).
mapview(nz, z = "Name", layer.name = "Régions")

# 3f Importer le fichier nz_capitales.cvs. Ce fichier donne le nom et la localisation des capitales de chaque région de la Nouvelle-Zélande. Créer une carte illustrant les frontières des régions ainsi que la position de leur capitale respective.
nz_capitales <- read.table("nz_capitales.csv", header = TRUE, sep = ",")
nz_capitales_points <- st_as_sf(x = nz_capitales, 
                                coords = c("Longitude", "Latitude"),
                                crs = 4126) # 4.r ligne 69, longitude latitude => datum WGS 84 pour définir le SCR. Ce dernier est associé au code EPSG 4326
# print(mapview(nz, col.regions = "blue", legend = FALSE) + mapview(nz_capitales_points)) # attributs/colonnes Capitales en longitude (x) et latitude (y)