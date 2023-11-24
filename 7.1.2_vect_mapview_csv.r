rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(sf)
library(mapview)
library(leafsync) # multiple figures
library(units)

# 7.1.2.3 Opérations sur les attributs des données vectorielles Indépendantes de la composante spatiale des données which(), subset(), merge()

municipalites <- st_read("7/villes/villes.shp") # points
# print(mapview(municipalites, legend = FALSE))

# Pour accéder à la première colonne.
villes <- municipalites[,"HAP_NM_TOP"] # ou [, 13] municipalites$HAP_NM_TOP ne conserve pas le type de donnée
# Renommer
colnames(villes) <- c("toponyme", "geometry")
print(head(villes))
# Pour accéder à la première ligne.
id_la_poc <- which(villes$toponyme == "La Pocatière") # Retourne son index/ligne
villes[id_la_poc,] # ou [128, ]

pop <- read.csv("7/villes/population.csv",encoding="UTF-8") # accent pris en compte
# munnom: Nom de la ville.
# msuperf: Superficie de la municipalité.
# mpopul: Taille de la population de la municipalité.

# Choisir les colonnes
pop <- pop[, c("munnom", "msuperf", "mpopul")]


# merge vlookup
villes_pop <- merge(x = villes, y = pop, by.x="toponyme", by.y="munnom") # le premier paramètre transforme le deuxième, Pour conserver l’entièreté des éléments initialement présents dans villes, il faudrait ajouter l’argument all.x = TRUE à la fonction merge().
print(villes)
colnames(villes_pop)[2:3] <- c("superficie", "population") # "toponyme" "msuperf"  "mpopul"   "geometry"





# 7.1.2.3 Opération spatiale sur données vectorielles Jointure spatiale
regions <- st_read("7/regions_admin/regions_admin.shp") # polygons
# print(mapview(regions))

# st_join() par les coordonnées vlookup
villes_reg = st_join(villes, regions[ ,"Rgns_Ad"]) # par les coordonnées en "geometry", le premier paramètre transforme le deuxième, même SCRS
# Pour conserver seulement les éléments de x qui intersectent un ou l’autre des éléments de y, nous devons ajouter l’argument left = FALSE

# print(mapview(villes_reg, zcol = "Rgns_Ad", legend = FALSE))

reg_villes <- st_join(regions, villes[,"toponyme"], left = FALSE)
print(reg_villes)

# Sous ensemble
villes_CN<- subset(villes_reg, Rgns_Ad=="Côte-Nord")
# print(mapview(villes_CN, legend = FALSE))

# Opérations géométriques, transforme la géométrie
# st_join() par les coordonnées vlookup
villes_reg_pop <- st_join(villes_pop, regions[ ,"Rgns_Ad"], left = FALSE) # villes_pop ref 7.1.2.2 type point, regions type polygone, le premier paramètre transforme le deuxième
# Pour conserver seulement les éléments de x qui intersectent un ou l’autre des éléments de y, nous devons ajouter l’argument left = FALSE

# Exception groupby point, doit être en left = FALSE, type point
reg_pop<-aggregate(villes_reg_pop["population"], by = list(villes_reg_pop$Rgns_Ad), FUN = sum, na.rm = TRUE) # population vient de villes_pop, le csv population pour variable pop
# list() Les points ainsi regroupés forment une géométrie multipoint (MULTIPOINT)

# Plusieurs polygones de régions pour définir la côte nord, type polygons, juste pour le mapview, sum pour 1 polygone sans mapview
reg_pop2 <-aggregate(villes_pop["population"], by = regions, FUN = sum, na.rm = TRUE)
# print(mapview(reg_pop2))
# reg_pop2, le type de by est conservé donc polygons

# 1 polygons
regions_agg <-aggregate(regions, by=list(regions$Rgns_Ad), unique)
regions <- regions_agg[-1]  # pour retirer la première colonne "Group.1"


# st_simplify() cartes à des échelles plus petites. La simplification permet de réduire l’utilisation de la mémoire, du disque, et de la bande passante. système de coordonnées métriques.
# type polygons
regions_nad <- st_transform(regions, crs = 32198) # dans un SCR d’unité de mesure métrique
# print(st_crs(la_poc_nad)$proj4string)
regions_simple_10 <- st_simplify(regions_nad, dTolerance = 10000) #10000m
regions_simple_40 <- st_simplify(regions_nad, dTolerance = 40000) #40000m

regions_nad_tmp <- mapview(regions_nad, legend=NULL)
regions_simple_10_tmp <- mapview(regions_simple_10, legend=NULL)
regions_simple_40_tmp <- mapview(regions_simple_40, legend=NULL)

map <- leafsync::latticeView(regions_nad_tmp, regions_simple_10_tmp,regions_simple_40_tmp, ncol = 3) # multiple views, fonction existe à la fois dans la bibliothèque leafsync et dans la bibliothèque mapview mais qu’elle est obsolète dans cette dernière, nous devons préciser que nous voulons la fonction latticeView
# print(map)

# Ligne entre deux points
# st_combine() 
# st_cast() convertir/transformer la géométrie d’un objet spatial (vecteur) donné vers une autre géométrie
# type point
villes_NQ <- subset(villes_reg, Rgns_Ad == "Nord-du-Québec") # points toponyme
villes_NQ_combo <- st_combine(villes_NQ) # plus de point toponyme, appartiennent tous à Nord-du-Québec
# print(villes_NQ)
# print(villes_NQ_combo)
villes_NQ_pol<-st_cast(villes_NQ_combo, to = "LINESTRING") # MULTIPOINT MULTILINESTRING LINESTRING POLYGON

# st_buffer() dans un SCR d’unité de mesure métrique, type point
la_poc <- subset(villes, toponyme == "La Pocatière") # 1 variable
la_poc_nad <- st_transform(la_poc, crs = 32198)
# print(st_crs(la_poc_nad)$proj4string)
la_poc_tampon10 <- st_buffer(la_poc_nad, dist = 10e3)
la_poc_tampon50 <- st_buffer(la_poc_nad, dist = 50e3)
# print(mapview(la_poc_tampon10, legend=NULL) +
#   mapview(la_poc_tampon50, col.regions = "lightgreen", legend=NULL)
# )

# Isoler les polygones des deux régions
Abitibi <- subset(regions_nad, Rgns_Ad == "Abitibi-Témiscamingue")
SagStJean <- subset(regions_nad, Rgns_Ad == "Saguenay - Lac-Saint-Jean")

# Calculer une zone tampon pour chacun des polygones
Abitibi_tampon20 <- st_buffer(Abitibi, dist = 20e3)  #20 km
SagStJean_tampon50 <- st_buffer(SagStJean, dist = 70e3)  #70 km

# print(mapview(Abitibi_tampon20, legend=NULL) + 
#         mapview(SagStJean_tampon50, legend=NULL) + 
#         mapview(Abitibi, legend=NULL) + 
#         mapview(SagStJean, legend=NULL)
#       )

# st_centroid() calculer le centroïde de polygones, dans un SCR d’unité de mesure métrique
centre_SagStJean <- st_centroid(SagStJean)
centre_Abitibi <- st_centroid(Abitibi)
# print((mapview(centre_Abitibi)))

# st_coordinates() pour un point XY
print(st_coordinates(la_poc))
print(st_is_longlat(la_poc)) # +proj=longlat, longitude latitude
print(st_crs(la_poc)$proj4string)

print(st_coordinates(la_poc_nad)) # sortie en mètre
print(st_is_longlat(la_poc_nad)) # crs=32198 +proj=lcc, unité de mesure métrique
print(st_crs(la_poc_nad)$proj4string)

# st_union() st_intersection() frontiere st_difference() st_sym_difference
A <- st_buffer(Abitibi, dist = 80e3)
B <- st_buffer(SagStJean, dist = 100e3)
union_AB <- st_union(A,B)
inter_AB <- st_intersection(A,B)
diff_AB <- st_difference(A,B)
diff_BA <- st_difference(B,A)
sym_diff_AB <- st_sym_difference(A,B)

mapview(A, legend=NULL) +
mapview(B, legend=NULL) +
mapview(union_AB, legend = NULL) +
mapview(inter_AB, legend = NULL) +
mapview(diff_AB, legend = NULL) +
mapview(diff_BA, legend = NULL) +
mapview(sym_diff_AB, legend = NULL)

map <- leafsync::latticeView(
  mapview(A, col.regions="blue") + mapview(B, col.regions="red"),
  mapview(union_AB),
  mapview(union_AB, legend = NULL, col.regions = "white") + mapview(diff_AB),
  mapview(union_AB, legend = NULL, col.regions = "white") + mapview(diff_BA),
  mapview(union_AB, legend = NULL, col.regions = "white") + mapview(inter_AB),
  mapview(union_AB, legend = NULL, col.regions = "white") + mapview(sym_diff_AB),
  ncol = 6
)
# print(map)

# st_intersects(), ce qui se trouve à l'intérieur et traverse
points <- data.frame(
  couleur = c("Bleu", "Rouge", "Violet", "Jaune", "Vert"),
  x = c(-685765, -224449, -480113, -395847, -902947),
  y = c(476835, 667962, 560159, 597020, 480553)
)
couleurs <- c("blue","red","purple","gold3","darkgreen")
points <- st_as_sf(points, coords = c("x","y"), crs = 32198) # # ref 4.1.2
# print(mapview(points, color = "black", col.regions = couleurs, legend = NULL) + mapview(A, legend=NULL))
# print(points[st_intersects(points, A, sparse=FALSE),]) # 1:1, 3:1, 5:1 sous forme matricielle d'éléments logiques sparse = FALSE, si 1 alors intersect oui, [,] pour retourner seulement ceux qui intersect
# ou st_intersects(A, points) 1,3,5

# st_disjoint() inverse, ce qui se trouve à l'extérieur
# print(points[st_disjoint(points, A, sparse=FALSE),]) # 2:1, 4:1 sous forme matricielle d'éléments logiques sparse = FALSE, si 1 alors disjoint oui, [,] pour retourner seulement ceux qui disjoint
# ou st_disjoint(A, points) 2,4

# st_crosses() traversé par, un polygone qui croise un polygone ne se croisent pas parce qu'ils forment un nouveau polygone, dans un SCR d’unité de mesure métrique
villes_NQ_pol <- st_transform(villes_NQ_pol, crs = 32198) # dans un SCR d’unité de mesure métrique
traverse <- A[st_crosses(A,villes_NQ_pol, sparse = FALSE),] # polygone, LINESTRING MULTILINESTRING
# print(traverse)
# print(mapview(traverser) + mapview(villes_NQ_pol))

# print(mapview(A, col.regions="blue", legend=NULL) + mapview(B, col.regions="red", legend=NULL) + mapview(Abitibi, col.regions="lightgreen", legend=NULL))
# print(st_overlaps(A,B)) # oui parce que l'intersection forme un polygone
# print(st_overlaps(Abitibi,A))

# st_touches() les frontieres se touchent, mais ne se croisent pas
Mauricie <- subset(regions_nad, Rgns_Ad == "Mauricie")
# print(st_touches(Abitibi,Mauricie))

# st_contains()
point_bleu <- subset(points, couleur=="Bleu")
# print(mapview(point_bleu, legend=NULL) + mapview(A, legend=NULL))
# print(st_contains(A, point_bleu))

# inverse
# print(st_within(point_bleu, A))

# Avancé, st_contains, st_touches, st_overlaps...
# print(st_join(regions, regions, join = st_touches)) # .x les régions, .y celles qui touchent les .x (adjacente)

# st_distance() dans un SCR d’unité de mesure métrique, type point, point à point
rimouski <- subset(villes, toponyme == "Rimouski")
rimouski_nad <-  st_transform(rimouski, crs = 32198)
distance_lapoc_rimou <- st_distance(la_poc_nad, rimouski_nad)
# print(distance_lapoc_rimou) # type units
# print(as.integer(round(distance_lapoc_rimou[1]/1000))) # en km

# point vs villes en col
villes_nad <- st_transform(villes, crs = 32198)
distance_la_poc_villes <- st_distance(villes_nad, la_poc_nad)
rownames(distance_la_poc_villes) <- villes$toponyme
colnames(distance_la_poc_villes) <- "La Pocatière"
# print(head(distance_la_poc_villes))

# villes vs villes
distance_villes_villes <- st_distance(villes_nad, villes_nad)
colnames(distance_villes_villes) <- villes$toponyme
rownames(distance_villes_villes) <- villes$toponyme
# print(head(distance_villes_villes))
# sélection de quelques colonnes ou quelques lignes par leur id
quelques_villes <- c(49, 154, 314, 549, 639)
distance_villes_villes[quelques_villes, quelques_villes]

# point vs polygone, not inside
# print(st_distance(centre_Abitibi, SagStJean))

# polygone vs polygone
# print(st_distance(Abitibi, SagStJean, by_element = TRUE))

# st_area() dans un SCR d’unité de mesure métrique, type polygone
A_m2 <- st_area(SagStJean) # type units
A_km2 <- set_units(A_m2, km2)
A_ha <- set_units(A_m2, ha) # 1 hectare (ha) mesure 100 m x 100 m
# print(A_m2)
# print(A_km2)
# print(A_ha)

# st_length() calcule la longueur/distance parcourues d’un objet spatial unidimensionnel, type MULTILINESTRING LINESTRING
L_m <- st_length(villes_NQ_pol) # type units
L_km <- set_units(L_m,km)
L_miles <- set_units(L_m, miles)
# print(L_m)
# print(L_km)
# print(L_miles)