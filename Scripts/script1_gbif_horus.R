# GETTING THE DATA


# Load and install packages
library(rgbif)
library(CoordinateCleaner) 
library(rnaturalearth)     
library(tidyverse)

# SPECIES LIST FROM Instituto Hórus
eeis_horus <- read_csv("Data/Invasive_species(1).csv") # FAUNA AND FLORA LIST AVAILABLE 


# Maximum number of occurrences (when testing use 10)
limit_occ = 100000

# progress bar
sps <- unique(eeis_horus$scientific_name)
n <- length(eeis_horus$scientific_name) # number of species

pb <- txtProgressBar(min = 0, max = n, initial = 0, style = 3) 

# Get occurrence data for each species
for (i in 1:n) {
  # progress bar
  setTxtProgressBar(pb, i)
  
  species_name3 <- sps[i]
  species_name4 <- gsub(" ", "_", species_name3) # for saving
  ocorrencias1 <- occ_search(scientificName =  species_name3, 
                            hasCoordinate = TRUE, 
                            country = "BR",
                            limit = limit_occ)
  if (is.null(nrow(ocorrencias1$data))) { 
    close(pb)
    warning(paste0("Nenhum dado encontrado para a espécie. ",
                   "A espécie não encontrada foi:", species_name3, 
                   " E o i =", i))
    next
  }
  
  dados_ocorrencias1 <- ocorrencias1$data
  cleaned_coords1 <- clean_coordinates(
    x = dados_ocorrencias1, 
    lon = "decimalLongitude", 
    lat = "decimalLatitude", 
    tests = c("equal", "seas", "zeros"),
    verbose = FALSE)
  # Get cleaned data
  coords1 <- cleaned_coords1[cleaned_coords1$.summary, c("decimalLongitude", "decimalLatitude")]
  
  # Save data
  path_sp1 <- paste0("Results/Distributions/", species_name4)
  if (!dir.exists(path_sp1)) {
    dir.create(path_sp1)
  }
  sp_coords1 <- bind_cols(scientific_name = species_name3, coords1)
  write_csv(sp_coords1, paste0(path_sp1, "/", species_name4, ".csv"), progress = F)
  ### Create a basic map for check up
  world_map1 <- map_data("world", "Brazil")
  
  # plot map
  map_plot1 <- ggplot() +
    geom_polygon(data = world_map1, aes(x = long, y = lat, group = group),
                 fill = "lightblue", color = "black") +
    geom_point(data = coords1, aes(x = decimalLongitude, y = decimalLatitude),
               color = "red", size = 2) +
    theme_minimal() +
    labs(title = species_name3,
         x = "Longitude", y = "Latitude")
  # save plot
  ggsave(paste0(path_sp1,"/", species_name4, "_map.tif"), map_plot1,
         width = 12, height = 12, units = "cm", bg = "white")
  
  print(i)
}
close(pb) # close the progress bar

