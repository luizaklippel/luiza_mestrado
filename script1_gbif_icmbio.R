# GETTING THE DATA


# Load and install packages
library(rgbif)
library(CoordinateCleaner) 
library(rnaturalearth)     
library(tidyverse)

# SPECIES LIST FROM ICMBio
eeis_icmbio <- read.csv2("Data/eeis_icmbio.csv") # FAUNA AND FLORA LIST AVAILABLE 


# Maximum number of occurrences (when testing use 10)
limit_occ = 100000

# progress bar
sps <- unique(eeis_icmbio$Species)
n <- length(eeis_icmbio$Species) # number of species

pb <- txtProgressBar(min = 0, max = n, initial = 0, style = 3) 

# Get occurrence data for each species
for (i in 1:n) {
  # progress bar
  setTxtProgressBar(pb, i)
  
  species_name <- sps[i]
  species_name2 <- gsub(" ", "_", species_name) # for saving
  ocorrencias <- occ_search(scientificName =  species_name, 
                            hasCoordinate = TRUE, 
                            country = "BR",
                            limit = limit_occ)
  if (is.null(nrow(ocorrencias$data))) { 
    close(pb)
    warning(paste0("Nenhum dado encontrado para a espécie. ",
                   "A espécie não encontrada foi:", species_name, 
                   " E o i =", i))
    next
  }

  dados_ocorrencias <- ocorrencias$data
  cleaned_coords <- clean_coordinates(
    x = dados_ocorrencias, 
    lon = "decimalLongitude", 
    lat = "decimalLatitude", 
    tests = c("equal", "seas", "zeros"),
    verbose = FALSE)
  # Get cleaned data
  coords <- cleaned_coords[cleaned_coords$.summary, c("decimalLongitude", "decimalLatitude")]
  
  # Save data
  path_sp <- paste0("Results/Distributions/", species_name2)
  if (!dir.exists(path_sp)) {
    dir.create(path_sp)
  }
  sp_coords <- bind_cols(Species = species_name, coords)
  write_csv(sp_coords, paste0(path_sp, "/", species_name2, ".csv"), progress = F)
  ### Create a basic map for check up
  world_map <- map_data("world", "Brazil")
  
  # plot map
  map_plot <- ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                 fill = "lightblue", color = "black") +
    geom_point(data = coords, aes(x = decimalLongitude, y = decimalLatitude),
               color = "red", size = 2) +
    theme_minimal() +
    labs(title = species_name,
         x = "Longitude", y = "Latitude")
  # save plot
  ggsave(paste0(path_sp,"/", species_name2, "_map.tif"), map_plot,
         width = 12, height = 12, units = "cm", bg = "white")
  
 print(i)
}
close(pb) # close the progress bar
