rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tmap)
library(sf)
library(raster)

E <- raster("6.1.10/Elevation/QC_Elevation.tif")
pal.elevation = colorRampPalette( c("midnightblue","forestgreen",
                                    "darkolivegreen4","burlywood", 
                                    "chocolate4"))

res(E) # 2000m x 2000m
print(tm_shape(E) +
        tm_raster(title = "Élévation (m)",
                  # n = 10, # 2000m / 10 = 200, 0 to 200
                  legend.hist = TRUE,
                  palette = pal.elevation(10), # mélange les 5 couleurs
                  colorNA = "beige",
                  style="quantile",
                  # style = "fixed", 
                  # breaks = c(0,100,200,300,400,500,600,700,800,900,1000,1600)
                  ) +
        tm_legend(outside = TRUE,
                  hist.width = 4) +
        tm_scale_bar(position = c("left","bottom"), 
                     text.size = 0.6) +
        tm_layout(frame = FALSE, 
                  legend.position = c(0.04,0.1), # x0 collé, y0 en bas
                  legend.title.size = 0.8, 
                  legend.format=c(text.align="right"),
                  legend.bg.color = "white", 
                  legend.frame = "black")
      ) 


# tmap_arrange(Efixed,Equant)