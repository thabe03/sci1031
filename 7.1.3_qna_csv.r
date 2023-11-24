rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(sf)
library(mapview)
library(leafsync) # multiple figures
library(units)

# Parmi les dix plus grandes villes du Québec, quelle est celle qui dispose du plus grand nombre de parcs nationaux dans un rayon de 70 km ?

# Obtenir la taille de la population de chacune des municipalités.

municipalites <- st_read("7/villes/villes.shp") # points
# print(mapview(municipalites, legend = FALSE))

# Pour accéder à la première colonne.
villes <- municipalites[,"HAP_NM_TOP"] # ou [, 13] municipalites$HAP_NM_TOP ne conserve pas le type de donnée
# Renommer
names(villes) <- c("toponyme", "geometry")

pop <- read.csv("7/villes/population.csv",encoding="UTF-8") # accent pris en compte
# munnom: Nom de la ville.
# msuperf: Superficie de la municipalité.
# mpopul: Taille de la population de la municipalité.

# Choisir les colonnes
pop <- pop[, c("munnom", "msuperf", "mpopul")]

villes_pop <- merge(x = villes, y = pop, by.x="toponyme", by.y="munnom") # le premier paramètre transforme le deuxième, Pour conserver l’entièreté des éléments initialement présents dans villes, il faudrait ajouter l’argument all.x = TRUE à la fonction merge().
names(villes_pop)[2:3] <- c("superficie", "population") # "toponyme" "msuperf"  "mpopul"   "geometry"

# Filtrer ces municipalités pour retenir les 10 municipalités ayant la taille de population la plus importante.
# sort()
sort <- function(x, y){ # array, by
  return(x[order(y, decreasing = TRUE), ])
}
top <- function(x, y){ # array, top
  if(missing(y)) {
    return(x[,])
  }
  return(x[1:y,])
}
top10_villes <- top(sort(villes_pop, villes_pop$population),10)
print(mapview(top10_villes, zcol= "population")) # couleur des points selon la population

# Lire la géodatabase du réseau de la SÉPAQ, type polygone
# parcs <- st_layers("7/parcs.gdb") # lire dossier
# print(parc)
parcs_nationaux <- st_read("7/parcs.gdb", layer = "terpnq_s") # ref 4.1.6_vect_couche
# Pour accéder à la première colonne.
parcs_nationaux <- parcs_nationaux[,"TRQ_NM_TER"] # ou [, 13] municipalites$HAP_NM_TOP ne conserve pas le type de donnée
# Renommer
names(parcs_nationaux) <- c("nom", "Shape")

# Tracer une zone tampon de 70 km de rayon autour de chacune des dix plus grandes villes.
# st_buffer() dans un SCR d’unité de mesure métrique, type point
top10_villes <- st_transform(top10_villes, crs = 32198)
# print(st_crs(top10_villes)$proj4string)
top10_villes <- st_buffer(top10_villes, dist = 70e3)
# print(mapview(top10_villes, zcol = "toponyme", legend = FALSE))

# Pour chaque zone, compter le nombre de parcs présent dans la zone tampon de 70 km.
parcs_nationaux = st_transform(parcs_nationaux, crs = st_crs(top10_villes))
# st_intersects(), ce qui se trouve à l'intérieur
# print(st_intersects(top10_villes, parcs_nationaux)) # parc inside ville return ville : parc
# on veut le nombre de parc
top10_villes$nbr_parcs <- rowSums(st_intersects(x = top10_villes, y = parcs_nationaux, sparse = FALSE)) # rowSums en 2d
# print(top10_villes)

# Déterminer la ville qui compte le plus grand nombre de parcs dans la zone tampon qui lui est associée.
resultat <- top(sort(top10_villes, top10_villes$nbr_parcs))
print(resultat)
# Sherbrooke et Saint-Jean-sur-Richelieu disposent toutes deux du plus grand nombre (4) de parcs nationaux dans un rayon de 70 km.





# 7.2.1
# 1a Construire un polygone de la forme d’un quadrilatère dont les sommets correspondent aux municipalités suivantes: Blanc-Sablon, Gaspé, Ivujivik et Chisasibi.
# multiple selection
Selection <- subset(villes, (toponyme == "Blanc-Sablon") | 
                      (toponyme == "Gaspé") | 
                      (toponyme == "Ivujivik") | 
                      (toponyme == "Chisasibi"))

# st_combine()
# st_cast() convertir la géométrie d’un objet spatial donné vers une autre géométrie
# type point
Selection <- st_combine(Selection) # plus de point toponyme, appartiennent tous à Nord-du-Québec
Selection<-st_cast(Selection, to = "POLYGON") # MULTIPOINT MULTILINESTRING LINESTRING POLYGON
# print(mapview(Selection))





# 1b Quelle est la superficie, en km, de ce polygone?
# st_area() dans un SCR d’unité de mesure métrique, type polygone
Selection <- st_transform(Selection, crs = 32198)
A_m2 <- st_area(Selection) # type units
A_km2 <- set_units(A_m2, km2)
# print(A_km2)





# 2a Combien de villes se trouvent dans une zone tampon de 20 km autour de la frontière séparant les régions administratives des Laurentides et de Lanaudière.
# corrigé
regions <- st_read("7/regions_admin/regions_admin.shp") # polygons
# print(mapview(regions))

# ne pas merge ville et région, trop lourd
regions <- st_transform(regions, crs = 32198) # dans un SCR d’unité de mesure métrique

# Isoler les polygones des deux régions
Laurentides <- subset(regions, Rgns_Ad == "Laurentides")
Lanaudiere <- subset(regions, Rgns_Ad == "Lanaudière")

# st_intersection() frontiere
Frontiere <- st_intersection(Laurentides, Lanaudiere)

# Calculer une zone tampon pour chacun des polygones
Frontiere <- st_buffer(Frontiere, dist = 20e3)  #20 km

villes <- st_transform(villes, crs = 32198) # dans un SCR d’unité de mesure métrique
Villes_Frontiere <- st_intersection(villes, Frontiere) # deux polygones qui ne sont pas du même vecteur, différent, return villes
print(nrow(Villes_Frontiere))

# # 2a Combien de villes se trouvent dans une zone tampon de 20 km à l'intérieur des régions administratives des Laurentides et de Lanaudière.
# regions <- st_read("7/regions_admin/regions_admin.shp") # polygons
# # print(mapview(regions))
# 
# regions <- st_transform(regions, crs = 32198) # dans un SCR d’unité de mesure métrique
# 
# # Isoler les polygones des deux régions
# regions <- subset(regions, (Rgns_Ad == "Laurentides") | (Rgns_Ad == "Lanaudière"))
#          
# regions <- st_buffer(regions, dist = 20e3)  #20 km
# 
# villes <- st_transform(villes, crs = 32198) # dans un SCR d’unité de mesure métrique
# villes_count <- st_intersection(villes, regions) # deux polygones qui ne sont pas du même vecteur, différent
# print(nrow(villes_count))
# 
# print(mapview(regions) + mapview(villes_count, legend = FALSE))





# # 2b Calculer la taille de la population qui habite cette zone tampon.

# v1 st_join()
# villes_pop <- st_transform(villes_pop, crs = 32198)
# Villes_Frontiere <- st_join(Villes_Frontiere, villes_pop[,"mpopul"])
# print(sum(Villes_Frontiere$mpopul))
# print(Villes_Frontiere)

# v2 merge()
# Villes_Frontiere <- merge(x = Villes_Frontiere, y = pop, by.x = "toponyme", by.y = "munnom" )
# print(sum(Villes_Frontiere$mpopul))

# v3 aggreate()
villes <- st_transform(villes, crs = 32198) # dans un SCR d’unité de mesure métrique
Villes_Frontiere <- merge(x = Villes_Frontiere, y = pop, by.x = "toponyme", by.y = "munnom" )
print(aggregate(Villes_Frontiere["mpopul"], by = Frontiere, FUN = sum))





# 3a Trouver les régions administratives traversées (st_crosses) par la ligne (st_crosses) reliant la ville de Sherbrooke à celle de Fermont.
# Ligne entre deux points
# st_combine() 
# st_cast() convertir la géométrie d’un objet spatial donné vers une autre géométrie
# type point
villes_pop <- subset(villes_pop, (toponyme == "Sherbrooke") | (toponyme == "Fermont")) # points toponyme
villes_pop <- st_combine(villes_pop) # plus de point toponyme, appartiennent tous à Nord-du-Québec
# print(villes_NQ)
# print(villes_NQ_combo)
villes_pop<-st_cast(villes_pop, to = "LINESTRING") # MULTIPOINT MULTILINESTRING LINESTRING POLYGON

# st_crosses() un polygone qui croise un polygone ne se croisent pas parce qu'ils forment un nouveau polygone, dans un SCR d’unité de mesure métrique
villes_pop <- st_transform(villes_pop, crs = 32198) # dans un SCR d’unité de mesure métrique
traverse <- regions[st_crosses(regions, villes_pop, sparse = FALSE),] # polygone, LINESTRING MULTILINESTRING
# print(traverse)
# print(mapview(traverse) + mapview(villes_pop))





# 4a Déterminer que 4 parcs de la SÉPAQ se trouvent dans un rayon de 70 km de la municipalité de Saint-Jean-sur-Richelieu. Quelle est la superficie totale couverte par ces parcs à l’intérieur de ce rayon ?
# Isoler les polygones
sjr <- subset(top10_villes, toponyme == "Saint-Jean-sur-Richelieu") # zone tampon déjà calculée

# # st_intersects() ce qui se trouve à l'intérieur
# parcs_nationaux <- parcs_nationaux[st_intersects(parcs_nationaux, sjr, sparse=FALSE),] # 1:1, 3:1, 5:1 sous forme matricielle d'éléments logiques sparse = FALSE, si 1 alors intersect oui, [,] pour retourner seulement ceux qui intersect

# st_intersection()
parcs_nationaux <- st_intersection(parcs_nationaux, sjr)

print(mapview(parcs_nationaux) + mapview(sjr, col.regions = "red", alpha.regions = 0.2))

parcs_nationaux <- st_area(parcs_nationaux)
# print(parcs_nationaux)

parcs_nationaux <- sum(parcs_nationaux)
parcs_nationaux <- set_units(parcs_nationaux, ha)
# print(parcs_nationaux)