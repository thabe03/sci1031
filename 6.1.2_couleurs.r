rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(viridis)
library(RColorBrewer)

colors <- grep("red", colours(), value = TRUE)

plot(1, 1, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", main = "Colors")

for (i in 1:length(colors)) {
  rect(0.1, 0.9 - i * 0.05, 0.3, 0.95 - i * 0.05, col = colors[i], border = "black") # rect(start,,end)
  text(0.4, 0.92 - i * 0.05, colors[i]) # text(startX,startY)
}

# COMPOSANTES RVB
col2rgb("forestgreen")

# CODES HEX
rgb(0,255,0, maxColorValue=255) # vert
rgb(0.0,1.0,0.0) # vert
rgb(0,(250:255)/255,0, names = paste("vert", 0:5, sep="_"))

# COMPOSANTES TSL/TSV
rgb2hsv(0,255,0,maxColorValue=255) # rgb2hsv(red, green, blue) 0-255
hsv(0.333333,1.0,1.0) # hsv(hue, saturation, value) 0-1
hcl(h = 120, c = 1, l = 0.5) # hcl(hue, chroma, luminance) 0-360

# Les palettes de couleurs dans R https://sci1031.github.io/sci1031/Cours_SpatialR_files/figure-html/basepal-1.svg
n <- 5 # nombre de couleurs
# print(rainbow(n))
# print(heat.colors(n))
# print(terrain.colors(n))
# print(topo.colors(n))
# print(cm.colors(n))

# LES PALETTES VIRIDIS https://sci1031.github.io/sci1031/Cours_SpatialR_files/figure-html/viridipal-1.svg
# Les palettes séquentielles, données ordonnées, la température, la densité de population, le revenu moyen par habitant
# print(viridis(n))
# print(magma(n))
# print(plasma(n))
# print(inferno(n))

# LES PALETTES COLORBREWER https://sci1031.github.io/sci1031/Cours_SpatialR_files/figure-html/brewerpal-1.svg
brewer.pal(5, "Accent")


