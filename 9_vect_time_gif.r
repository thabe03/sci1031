rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(sf)
library(mapview)
library(leafsync) # multiple figures
library(units)
library(lubridate) # temps

# ref 7.1.2

# 9.1.2.2 Quel trajet présente la vitesse moyenne la plus élevée? km/h plus élevé, la personne qui a fait un trajet le plus vite peu importe le temps ou la longueur du trajet

# retirer une colonne
# tmp <- st_read("trip5000.json") 
# st_write(tmp[1:400, names(tmp) != "liste_segments_jsonb"], "trip400.geojson") 
trajets <- st_read("9/trip400/trip400.geojson") # type MULTILINESTRING LINESTRING
len <- st_length(trajets)
# plot(trajets$length, st_length(trajets), 
#      xlab = "distance - colonne 'length' (m)",
#      ylab = "distance - fonction 'st_length()' (m)", 
#      pch = 19) # point fill 
# abline(a = 0, b = 1, lty = 2) # b = 1 intersection, lty = 2 donne une ligne discontinue

class(trajets$start) # POSIXct (calendar time) est une variété de POSIXt (time) qui contient le nombre de secondes
trajets$duree_s <- difftime(trajets$stop, trajets$start, units = 'secs') # ou trajets$stop - trajets$start automatiquement, type difftime
# par() figure à deux panneaux côte à côte multiple vues
# par(mfrow = c(1, 2))
# hist(trajets$length, 
#      breaks = seq(0, 44000, 2000),
#      xlab = "Longueur (m)",
#      ylab = "Fréquence",
#      main = ""
# ) 
# hist(as.numeric(trajets$duree_s), 
#      breaks = seq(0, 20000, 1000),
#      xlab = "Durée (s)",
#      ylab = "Fréquence",
#      main = ""
# )

# as.numeric() type difftime
trajets$vitesse_km_h <- trajets$length/as.numeric(trajets$duree_s) * 3.6 # diviser par 1000 [m/km] pour convertir les mètres en kilomètres et de multiplier les secondes par 3600 [s/h]
vit_dec <- sort(trajets$vitesse_km_h, decreasing = TRUE) # valeur aberrante, sort() return list
traj_rapide <- trajets[order(trajets$vitesse_km_h, decreasing = FALSE)[2], ] # ref 7.1.3, retient la deuxième ligne seulement, order() return ind
# print(traj_rapide)
# print(mapview(traj_rapide))





# 9.1.2.3 Quelle est la période de l’année préférée des cyclistes du jeu de données?

# month() day() hour() date
# month(trajets$start, label = TRUE, abbr = FALSE)
trajets$mois <- month(trajets$start)
trajets$jour <- day(trajets$start)
trajets$heure <- hour(trajets$start)

# table() nombre d'occurence
nb_mois <- table(trajets$mois)

traj_10juillet <- trajets[trajets$mois == 7 & trajets$jour == 10, ]
barplot(
  table(traj_10juillet$heure),
  xlab = "Heure du 10 Juillet",
  ylab = "Nombre de trajets"
)





# 9.1.2.4 Animation du parcours le plus long

# Trouver le trajet le plus long.
traj_long <- trajets[which.max(trajets$length), ]

# Associer un temps à chaque segment du parcours.
traj_long_pts <- st_cast(traj_long, to = "POINT")
traj_long_pts$temps <- seq(
  from = traj_long$start, 
  to = traj_long$stop, 
  length.out = nrow(traj_long_pts)
)

# Créer une carte pour différents états d’avancement du parcours.
# ref TN2 tmap

library(tmap)

txt_temps <- function(temps) {
  paste0(hour(temps), "h ", minute(temps), "m ", floor(second(temps)), "s")
} 
txt_distance <- function(lin) {
  len <- st_length(lin)
  paste0("Distance parcourue: ", format(as.numeric(len)/1000, digits = 4), "km")
}
visual <- function(traj_pts, n = 100, basemap = NULL) {
  stopifnot(n > 1)
  tps <- traj_pts$temps[n] # le 100e dans in row
  pts <- traj_pts[1:n, drop = FALSE]
  lin <- st_cast(st_combine(pts), "LINESTRING")
  return(basemap + # avoir un fond de la library(tmap)
    tm_shape(lin, bbox = st_bbox(traj_long_pts)) + # de la grosseur de la carte et non la ligne 
    tm_lines(lwd = 2, col = "#62852eaa") +
    tm_layout(main.title = paste0(txt_temps(tps), "  -  ", txt_distance(lin)), 
              main.title.size = 0.9))
}
# print(visual(traj_long_pts, 2000))
anim <- function(traj_pts, n_img, basemap = NULL) {
  for(i in floor(seq(2, nrow(traj_pts), length.out = n_img))) { # floor() pour "arrondir", length.out~pas
    print(visual(traj_pts, i, basemap))
  }
}
# anim(traj_long_pts, n_img = 5, NULL)

# Rassembler les cartes ainsi créées et en faire un fichier GIF.

library(animation)
library(raster)
library(rosm)

carte_fond <- osm.raster(extent(traj_long_pts), crop = TRUE)
saveGIF(anim(traj_long_pts, 20, tm_shape(carte_fond) + tm_rgb()), movie.name = "9_vect.gif", ani.height = 500, ani.width = 500)