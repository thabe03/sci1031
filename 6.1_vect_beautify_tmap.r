rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tmap)
library(sf)
library(raster)

Q <- st_read("6.1/Population/QC_RegAdm_Pop.shp") # polygon
Ro <- st_read("6.1/Routes/QC_routes.shp") # multiligne
V <- st_read("6.1/Villes/QC_coord_municipalites.shp") # point

layout <- tm_layout(# title = "Municipalités", 
                    legend.text.color = "black",
                    legend.outside = TRUE,
                    legend.outside.position = "right",
                    # legend.outside.size = 0.15,
                    # legend.title.fontfamily = "serif",
                    legend.title.fontface = 2, # "bold"
                    # legend.title.color = NA, # ou "black"
                    legend.title.size = 1,
                    legend.text.size = 0.75,
                    legend.stack = "vertical", 
                    legend.bg.color = NA, 
                    frame = FALSE,
                    legend.frame = FALSE  # cadre couleur
                    # legend
                    # hist.width = 0.6,
                    # hist.height = 0.6,
                    # position=c("right","bottom")
                    )

# 8
multiligne <- tm_shape(Q) +
  tm_polygons() +
  tm_shape(Ro) +
  tm_lines(col = "ClsRte",
           palette = "Accent",
           title.col = "Types de route")

# 9
dots <- tm_shape(Q) +
  tm_polygons() + # Equivalent de t_fill et t_borders
  tm_shape(V) +
  tm_dots(col = "Mncplts",
          palette = "Paired", # "Accent"
          size = 1) +
  layout

# 11
symbols <- tm_shape(Q) +
  tm_polygons() + # Equivalent de t_fill et t_borders
  tm_style("bw") + # tout en gris, reset tm_layout
  tm_symbols(shape = marker_icon(), # 21 par défaut, 15 carré
             size = "Pop_tot", 
             border.col = "grey", 
             col = "black", # forme
             scale = 2, # zoom les éléments
             legend.size.show = TRUE, 
             legend.size.is.portrait = TRUE, 
             sizes.legend.labels = c("500","1000","1500","2000","2500"), 
             title.size = "Population") 

# 11-12
# proportion d’enfants dans chaque région de 0 à 14 ans
Q$Pop_prop_enfant <- Q$Pop_0_14/Q$Pop_tot
bubbles <- tm_shape(Q) +
  tm_polygons() + 
  tm_bubbles(size = "Pop_tot" , 
             col = "Pop_prop_enfant", 
             # style = "quantile", 
             style = "fixed",
             breaks = c(0.1, 0.14, 0.15, 0.16, 0.18, 0.3),
             scale = 2,  
             border.col = "black", 
             border.lwd = 1,
             legend.size.show = TRUE, 
             legend.size.is.portrait = FALSE, # tout en y
             title.size ="Population (en milliers)", 
             title.col = "Proportion d'enfants (0-14 ans)", 
             sizes.legend.labels = c("500","1000","1500","2000","2500")) +
  layout

# 9
num <- tm_shape(Q) +
  tm_polygons() +
  tm_text("NUM_REG", col= "NOM_REG", # montre, legende
               palette = "Paired", # "Accent"
               size = 0.8,
               fontface="bold",
               legend.col.show = TRUE, # habituellement n'est pas affiché
               title.col = "Régions administratives",
               auto.placement=TRUE,
               just="right")

# 13
multiples_polygons <- tm_shape(Q) + 
  tm_polygons(col = "Pop_prop_enfant", 
              style = "quantile") +
  tm_facets(by = "NOM_REG", 
            nrow = 5, # y
            scale.factor = 5) + # ~title.size
  tm_layout(panel.label.height = 2, # 2 fois la grosseur du texte 
            panel.label.size = 0.8, 
            legend.show = FALSE)

names(Q)
levels(factor(Q$NOM_REG))

# Q_Outaouais <- Q[Q$NOM_REG == "Outaouais",]

# print(tm_shape(Q)+
#         # tm_fill(col = "NOM_REG", title = "Régions administratives") + # layer (col="red"|"NUM_REG", alpha, title~layer.name de mapview, legend.is.portrait)
#         # tm_borders() + # layer (col, lwd epaisseur du trait, lty type de trait)
#         tm_polygons(col = "NOM_REG", title = "Régions administratives") + # palette="Set1", border.col = "darkgrey", 
#         tm_scale_bar(breaks = c(0,250,500), # width = 2 au lieu de break
#                      text.size = 0.8,
#                      position=c("right","bottom")) +
#         tm_compass(type = "arrow",
#                    position = c("right", "top")) +
#         # tm_compass(type = "4star",
#         #            size = 2,http://127.0.0.1:15545/graphics/45fa0918-e48e-43ce-a2db-a1408e683abb.png
#         #            show.labels = 2,
#         #            position = c("right", "top"))
#         tm_graticules() + # longitude, latitude (labels.col couleur de l'ecriture, alpha, labels.cardinal bool ajout de la cardinalité)
#         tm_grid() + # (labels.size, col, lwd epaisseur du trait, n.x, n.y)
#         tm_credits("Données récupérées \nsur le site donneesquebec.ca",
#                    size = 0.6, col= "white") +
#         tm_layout(title = "Carte du Québec",
#           title.size = 0.8,
#           title.position = c("right","top"),
#           title.color = "white",
#           bg.color = "black",
#           frame = FALSE,
#           frame.lwd = 5, # épaisseur du trait de la carte
#           legend.show = FALSE,
#           # aes.color = c(fill="lightblue", borders="darkgreen"), # si aucun col dans tm_polygons
#           aes.palette = list(cat = "Accent"),
#           scale = 2, # zoom ex : titre
#           # sepia.intensity = 0.5, # couleurs chaudes seulement, avec tm_polygons
#           # saturation = 0 # en gris
#         )
#       )

# print(bubbles)
# print(tmap_arrange(multiligne, dots, symbols, num, bubbles, multiples_polygons))