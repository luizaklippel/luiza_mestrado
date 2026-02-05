# VISUALIZING DISTRIBUTIONS

# Packages
library(sf)
library(terra)
library(devtools)
install_github("BrunoVilela/letsR")
library(letsR)
library(tidyverse)
library(raster)
terra::rast(system.file("ex/elev.tif", package="terra"))

# Open shapefiles

UCs <- terra::vect("Data/shp_cnuc_2024_02/cnuc_2024_02.shp")
UCs <- UCs[UCs$esfera == c("Federal", "Estadual"), ]
UCs <- UCs[UCs$categoria != "Reserva Particular do Patrimônio Natural",]
UCs <- UCs[is.na(UCs$marinho) | UCs$marinho == "", ]
BR <- terra::vect("Data/BR_UF_2024/BR_UF_2024.shp")

# Set csv file as list

path_dir <- "Results/Distributions/"

coordenadas <- list()
pastas <- dir(path_dir)
for(i in 1:length(pastas)) {
  path_i <- paste0(path_dir, pastas[i])
  arquivo <- list.files(path_i, pattern = ".csv", full.names = TRUE)
  coord.temp <- read_csv(arquivo)
  if ("scientific_name" %in% colnames(coord.temp)) {
    coord.temp <- coord.temp %>%  
      mutate(Species = ifelse(is.na(scientific_name), Species, scientific_name)) %>%
      select(-scientific_name)
  }
  coord.temp <- st_as_sf(coord.temp, 
                         coords = c("decimalLongitude", "decimalLatitude"))
  coordenadas[[i]] <- coord.temp
  names(coordenadas)[i] <- gsub(".csv","",arquivo)
}



# Plot coordinates and shapefiles

coordenadas_all <- do.call(rbind, coordenadas)
save(coordenadas_all, file = "Data/coordenadas_all.RData")
st_crs(coordenadas_all) <- crs(UCs)

g <- ggplot(st_as_sf(BR)) +
  geom_sf(color = "black", fill = "white") +
  geom_sf(data = st_as_sf(UCs), fill = "lightblue", lwd = 0) +
  geom_sf(data = coordenadas_all,
          size = .1)

g
ggsave(g, file ="Figures/Coordinates.tiff")



