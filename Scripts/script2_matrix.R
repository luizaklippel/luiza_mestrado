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
st_crs(coordenadas_all) <- crs(UCs)
g <- ggplot(st_as_sf(BR)) +
  geom_sf(color = "black", fill = "white") +
  geom_sf(data = st_as_sf(UCs), fill = "lightblue", lwd = 0) +
  geom_sf(data = coordenadas_all, size = .1)

ggsave(g, file ="Figures/Coordinates.tiff")

# Organize matrix data
sp <-as.matrix(coordenadas_all,rownames= "Species")
col_vec <- c("Species")
col_mat <- sp[,col_vec]
col_mat1 <- as.matrix(col_mat)
col_mat2 <- col_mat1[ ,1]
col_mat3 <- as.character(col_mat2)

col_c <- c("geometry")
col_cm <- sp[,col_c]
col_cord <- as.data.frame(col_cm)
col_cord1 <- t(col_cord)
col_cord2 <- as.matrix(col_cord1)


# Create matrix
mat_uni <- lets.presab.grid.points(col_cord2,
                                   col_mat3, 
                            UCs, 
                            "uc_id")

rich_plus1 <- rowSums(mat_uni$PAM[, -1, drop = FALSE]) + 1
colfunc <- colorRampPalette(c("#fff5f0", "#fb6a4a", "#67000d"))
colors <- c("white", colfunc(max(rich_plus1)))
occs <- terra::vect(col_cord2, crs = crs)


m <- plot(mat_uni$grid, border = "gray40",
     col = colors[rich_plus1]) +
plot(sf::st_geometry(coordenadas_all), add = TRUE) +
plot(occs, cex = 0.5, col = rep(1:4, each = pastas), add = T)

save(m, file = "Figures/Occurrences.tiff")