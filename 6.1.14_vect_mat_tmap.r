rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tmap)
library(sf)
library(raster)
library(grid)

Q <- st_read("6.1/Population/QC_RegAdm_Pop.shp") # polygon
E <- raster("6.1.10/Elevation/QC_Elevation.tif")
pal.elevation = colorRampPalette( c("#377EB8","#984EA3"))
layout <- tm_layout(# title = "Municipalités", 
  legend.text.color = "black",
  legend.outside = TRUE, # tout en y
  legend.outside.position = "right",
  # legend.outside.size = 0.15,
  # legend.title.fontfamily = "serif",
  legend.title.fontface = 2, # "bold"
  # legend.title.color = NA, # ou "black"
  legend.title.size = 1,
  legend.text.size = 0.75,
  legend.stack = "vertical",
  legend.bg.color = "white",
  frame = TRUE, 
  legend.frame = "black")

Q_Outaouais <- subset(Q, NOM_REG == "Outaouais")
E_Outaouais <- crop(E, extent(Q_Outaouais))
E_Outaouais <- mask(E_Outaouais, Q_Outaouais) # E_Outaouais - Q_Outaouais

carte_princ <- tm_shape(E_Outaouais) + 
  tm_raster(title = "Élévation (m)",
            # n = 10, # 2000m / 10 = 200, 0 to 200
            legend.hist = FALSE,
            palette = pal.elevation(5), # mélange les 5 couleurs
            colorNA = "beige",
            # style = "fixed", 
            # breaks = c(0,100,200,300,400,500,600,700,800,900,1000,1600)
  )

carte_cadre <- tm_shape(Q) + 
  tm_borders(col = "black") + 
  tm_shape(Q_Outaouais) + 
  tm_borders(lw=2, col="red")+
  layout

print(carte_princ)
print(carte_cadre, vp = viewport(0.72, 0.42, width = 0.4, height = 0.4)) # x, y