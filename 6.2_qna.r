rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(sf)
library(mapview)
library(spData) 
library(RColorBrewer)
data(World) # polygons
data(metro) # points
data(land) # stars

# 1a Quelle est la géométrie des données World, et quels sont leurs attributs?
st_geometry(World)
names(World)

# 1b Déterminer les dix premières entrées des attributs iso_a3 et name des données World.
World[1:10, c("iso_a3", "name")]

# 1c Utiliser le style col_blind pour illustrer la carte du monde. N’ajouter pas de légende.
c_d_e <- function(dataset){
  return(tm_shape(dataset) + tm_polygons(col="MAP_COLORS") + # colonne "name" ou minimiser le fait qu'un  territoire voisin a la même couleur
          tm_style("col_blind") +                             
          tm_layout(legend.show = FALSE))
}
print(c_d_e(World))

# 1d Produire une carte identique à la carte de la question c mais représentant uniquement les pays d’Afrique.
Afrique <- subset(World, continent == "Africa")
print(c_d_e(Afrique))

# 1e Produire une carte de l’Afrique sur laquelle les pays sont identifiés par leur code “iso_a3”.
print(c_d_e(Afrique) + tm_text("iso_a3", size = 0.6))

# 1f Produire une carte de l’Amérique du Sud
f_g_h <- function(){
  AS <- World[World$continent == "South America",]
  return(tm_shape(AS) + 
           tm_polygons(col = "pop_est_dens", # où la couleur de chaque pays représente sa densité de population (“pop_est_dens”). 
                       style = "quantile", # Utiliser le style quantile pour classer les valeurs de densité, 
                       palette = brewer.pal(n=5, "Reds"), # et la palette Reds de la bibliothèque RColorBrewer.
                       title = "Densité de population") # Assurez-vous que la légende porte le titre “Densité de population”.
         )
}
# print(f_g_h())

# 1g Produire une carte à panneaux multiples 
print(f_g_h() +
        tm_facets(by = "name", # Chaque panneau de la carte doit correspondre à un pays identifié par son nom.
                  nrow = 4, # La carte doit comprendre quatre rangées de panneaux.
                  scale.factor = 5 # Les pays doivent être illustrés avec un facteur d’échelle de cinq.
        ))

# 1h Utiliser les arguments de la fonction tm_layout() pour modifier l’apparence de la carte à panneaux multiples de la question g.

print(f_g_h() + 
        tm_facets(by = "name", # Chaque panneau de la carte doit correspondre à un pays identifié par son nom.
                  nrow = 4, # La carte doit comprendre quatre rangées de panneaux.
                  scale.factor = 5 # Les pays doivent être illustrés avec un facteur d’échelle de cinq.
        ) +
        tm_layout(panel.label.height = 2, # Une vignette de hauteur 2 pour chaque facette.
                  panel.label.bg.color = "white", # Une couleur de fond blanc pour chaque vignette.
                  panel.label.size = 0.9, # Un texte de taille 0.9 sur chaque vignette.
                  legend.frame = TRUE, # Une légende encadrée 
                  legend.outside = TRUE, # et positionnée à l’extérieure 
                  legend.outside.position = "left", # à gauche des panneaux.
                  legend.format = list(format = "f", digits = 0, text.separator = "-") # Une légende dont les chiffres sont arrondis à l’unité près, et séparés par le symbole “-”.
                  )
      )

# 2c En utilisant les données metro et World, créer une carte du monde sur laquelle les métropoles sont représentées par un cercle dont le diamètre est proportionnel à la taille de sa population en 2020. La carte doit comprendre les éléments suivants:
print(tm_shape(World) +
        tm_borders(col="black") + # La carte du monde est blanche et seules les frontières noires entre les pays y apparaissent.
        tm_shape(metro) +
        tm_symbols(col="red", # Chaque métropole est illustrée par un cercle rouge de bordure noire.
                   title.size = "Population", # La légende porte le nom “Population”.
                   border.col = "black",
                   size = "pop2020",
                   legend.size.show = TRUE,
                   legend.size.is.portrait = TRUE
        )
      )

# 3a C’est un objet matriciel composé de plusieurs couches d’attributs. Déterminer les couches comprises dans land.
names(land)
levels(land$cover)

# 3b Utiliser la palette de couleur Greens de la bibliothèque RColorBrewer pour produire une carte du pourcentage de couvert forestier. De plus, la carte doit comprendre les éléments suivants:
print(tm_shape(land) +
        tm_raster("trees",
                  palette = brewer.pal(n=6, "Greens"), # Six classes de pourcentage de couvert.,
                  title = "", # !!! et sans titre (par défaut).
                  legend.is.portrait = FALSE
        ) +
        tm_layout(main.title = "Pourcentage de couvert forestier", # Un titre principal “Pourcentage de courvert forestier”
                  main.title.size = 1, # de taille 1.
                  legend.outside = TRUE, # Une légende extérieure à la carte,
                  legend.outside.position = "bottom", # située sous la carte, horizonale (par défaut) !!!
                  legend.format = list(text.separator = "-") # Une légende dont les chiffres sont séparés par le symbole “-”.
        )
)
