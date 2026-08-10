### TOTAL SPECIES RICHNESS

#Packages
library(terra)
library(dplyr)
library(sf)
library(letsR)
library(BIEN)
library(iNEXT)
library(purrr)
library(tibble)

# Shapefiles

UCs <-  terra::vect("Data/shp_cnuc_2024_02/cnuc_2024_02.shp")
UCs <- UCs[UCs$esfera == c("Federal", "Estadual"), ]
UCs <- UCs[UCs$categoria != "Reserva Particular do Patrimônio Natural",]
UCs <- UCs[is.na(UCs$marinho) | UCs$marinho == "", ]
UCs <- project(UCs, "EPSG:4326")
UCs$uc_fator <- as.numeric(as.factor(UCs$nome_uc))

## IUCN ANIMALS

# Data

mama <- rbind(
  terra::vect("Data/MAMMALS/MAMMALS_PART1.shp"),
  terra::vect("Data/MAMMALS/MAMMALS_PART2.shp")
)

rept <-  rbind(
  terra::vect("Data/REPTILES/REPTILES_PART1.shp"),
  terra::vect("Data/REPTILES/REPTILES_PART2.shp")
)

amph <- rbind(
  terra::vect("Data/AMPHIBIANS/AMPHIBIANS_PART1.shp"),
  terra::vect("Data/AMPHIBIANS/AMPHIBIANS_PART2.shp")
)

#bird <- terra::vect("Data/bird/BOTW_2025.gpkg")

# Presence Absence Matrix
# Mammals
names(mama)[2] <- "sciname"

mama_presab <- lets.presab.grid(mama, UCs, "uc_fator")

mama_ucs <- mama_presab$PAM
idx <- match(mama_ucs[, 1], UCs$uc_fator)
mama_ucs[, 1] <- UCs$nome_uc[idx]
colnames(mama_ucs)[1] <- "nome_uc"

mama_ucs_df <- as.data.frame(mama_ucs)
rownames(mama_ucs_df) <- mama_ucs_df$nome_uc
mama_ucs_df <- mama_ucs_df[, -1]

save(mama_ucs_df, file = "Data/mama_ucs_df.RData")

# Reptiles
names(rept)[2]<- "sciname"

rept_presab <- lets.presab.grid(rept, UCs, "uc_fator")

rept_ucs <- rept_presab$PAM
idxrept <- match(rept_ucs[, 1], UCs$uc_fator)
rept_ucs[, 1] <- UCs$nome_uc[idxrept]
colnames(rept_ucs)[1] <- "nome_uc"

rept_ucs_df <- as.data.frame(rept_ucs)
rownames(rept_ucs_df) <- rept_ucs_df$nome_uc
rept_ucs_df <- rept_ucs_df[, -1]

save(rept_ucs_df, file = "Data/rept_ucs_df.RData")

# Amphibians
names(amph)[2] <- "sciname"

amph_presab <- lets.presab.grid(amph, UCs, "uc_fator")

amph_ucs <- amph_presab$PAM
idxamph <- match(amph_ucs[, 1], UCs$uc_fator)
amph_ucs[, 1] <- UCs$nome_uc[idxamph]
colnames(amph_ucs)[1] <- "nome_uc"

amph_ucs_df <- as.data.frame(amph_ucs)
rownames(amph_ucs_df) <- amph_ucs_df$nome_uc
amph_ucs_df <- amph_ucs_df[, -1]

save(amph_ucs_df, file = "Data/amph_ucs_df.RData")

# Birds

#names(bird)[3] <- "sciname"

#bird_presab <- lets.presab.grid(bird, UCs, "uc_fator")

#bird_ucs <- bird_presab$PAM
#idxbird <- match(bird_ucs[, 1], UCs$uc_fator)
#bird_ucs[, 1] <- UCs$nome_uc[idxbird]
#colnames(bird_ucs)[1] <- "nome_uc"

#bird_ucs_df <- as.data.frame(bird_ucs)
#rownames(bird_ucs_df) <- bird_ucs_df$nome_uc
#bird_ucs_df <- bird_ucs_df[, -1]

#save(bird_presab, file = "Data/bird_presab.RData")



bird <- lets.load(file = "Data/pam_birds_br.RData")
names(bird)[3] <- "sciname"
bird_pam <- lets.pamcrop(bird, UCs, remove.sp = TRUE)

bird_ucs <- bird_pam$Presence_and_Absence_Matrix

#Coords as points
coords <- as.data.frame(bird_ucs[, c("Longitude(x)", "Latitude(y)")])
pontos <- terra::vect(coords,
                      geom = c("Longitude(x)", "Latitude(y)"),
                      crs  = "EPSG:4326")

#Join
join <- terra::intersect(pontos, UCs[, c("uc_fator", "nome_uc")])
join_df <- as.data.frame(join)
join_df$row_idx <- as.integer(rownames(join_df))

bird_ucs_df <- as.data.frame(bird_ucs[join_df$row_idx, ])
bird_ucs_df <- bird_ucs_df[, !colnames(bird_ucs_df) %in% c("Longitude(x)", "Latitude(y)")]

#Unite duplicates
dupes <- colnames(bird_ucs_df)[duplicated(colnames(bird_ucs_df))]
for (sp in unique(dupes)) {
  cols <- which(colnames(bird_ucs_df) == sp)
  bird_ucs_df[, cols[1]] <- apply(bird_ucs_df[, cols], 1, max)
  bird_ucs_df <- bird_ucs_df[, -cols[-1]]
}

bird_ucs_df$nome_uc <- join_df$nome_uc

bird_ucs_df <- bird_ucs_df %>%
  group_by(nome_uc) %>%
  summarise(across(everything(), max)) %>%
  tibble::column_to_rownames("nome_uc")

bird_ucs_df <- bird_ucs_df[, colSums(is.na(bird_ucs_df)) == 0]
bird_ucs_df <- bird_ucs_df[rowSums(bird_ucs_df) > 0, ]

write.csv(bird_ucs_df, file = "Data/bird_ucs_df.RData")

## BIEN PLANTS

UCs_sf <- st_as_sf(UCs)
UCs_sf <- st_transform(UCs_sf, 4326)

#flora <- BIEN_ranges_sf(sf = UCs_sf,directory = "Data/flora", crop.ranges= TRUE)

# Data

all_ranges <- BIEN_ranges_list()

brazil_species <- BIEN_list_country("Brazil")

# Filter to species that also have range maps available
ranges_available <- all_ranges$species
brazil_with_ranges <- brazil_species |>
  dplyr::mutate(
    species_underscore = gsub(" ", "_", scrubbed_species_binomial)
  ) |>
  dplyr::filter(species_underscore %in% ranges_available)

# Load only those ranges
flora_sf <- BIEN_ranges_load_species(
  species = brazil_with_ranges$species_underscore
)

flora_sf <- st_make_valid(flora_sf)

# Also make sure UCs are valid
UCs_sf <- st_make_valid(UCs_sf)

# Now filter
flora <- st_filter(flora_sf, UCs_sf)

st_write(flora, "Data/flora/flora_UCs.shp")
st_write(flora, "Data/flora/flora_UCs.gpkg")

flora <- terra::vect("Data/flora/flora_UCs.shp")

names(flora)[1] <- "sciname"

flora_pam <- lets.presab(flora, xmn = -93, xmx = -29,
                         ymn = -57, ymx = 15, res = 1)

lets.save(flora_pam, file = "Data/flora_pam.RData")

flora_ucs <- flora_pam$Presence_and_Absence_Matrix

coords <- as.data.frame(flora_ucs[, c("Longitude(x)", "Latitude(y)")])
pontos <- terra::vect(coords,
                      geom = c("Longitude(x)", "Latitude(y)"),
                      crs  = "EPSG:4326")

#Join
join <- terra::intersect(pontos, UCs[, c("uc_fator", "nome_uc")])
join_df <- as.data.frame(join)
join_df$row_idx <- as.integer(rownames(join_df))

flora_ucs_df <- as.data.frame(flora_ucs[join_df$row_idx, ])
flora_ucs_df <- flora_ucs_df[, !colnames(flora_ucs_df) %in% c("Longitude(x)", "Latitude(y)")]

#Unite duplicates
dupes <- colnames(flora_ucs_df)[duplicated(colnames(flora_ucs_df))]
for (sp in unique(dupes)) {
  cols <- which(colnames(flora_ucs_df) == sp)
  flora_ucs_df[, cols[1]] <- apply(flora_ucs_df[, cols], 1, max)
  flora_ucs_df <- flora_ucs_df[, -cols[-1]]
}

flora_ucs_df$nome_uc <- join_df$nome_uc

flora_ucs_df <- flora_ucs_df %>%
  group_by(nome_uc) %>%
  summarise(across(everything(), max)) %>%
  tibble::column_to_rownames("nome_uc")

flora_ucs_df <- flora_ucs_df[, colSums(is.na(flora_ucs_df)) == 0]
flora_ucs_df <- flora_ucs_df[rowSums(flora_ucs_df) > 0, ]

write.csv(flora_ucs_df, file = "Data/flora_ucs_df.RData")

# Plot

plot_flora <- plot(flora_pam, xlab = "Longitude", ylab = "Latitude",
             main = "Protected Areas' Flora Richness")

plot_bird <- plot(bird_pam, xlab = "Longitude", ylab = "Latitude",
                   main = "Protected Areas' Bird Richness")

plot_mama <- plot(mama_presab$grid, xlab = "Longitude", ylab = "Latitude",
                   main = "Protected Areas' Mammals Richness")

plot_amph <- plot(amph_presab$PAM, xlab = "Longitude", ylab = "Latitude",
                   main = "Protected Areas' Amphibians Richness")

plot_rept <- plot(rept_pam, xlab = "Longitude", ylab = "Latitude",
                                main = "Protected Areas' Reptile Richness")
## MERGE ALL PAM

lista_dfs <- list(mama_ucs_df, amph_ucs_df, rept_ucs_df, bird_ucs_df,flora_ucs_df)

df_merged <- lista_dfs %>%
  map(~ rownames_to_column(.x, "UC")) %>%   # rownames → coluna em todos
  reduce(full_join, by = "UC") %>%           # une todos pela coluna UC
  column_to_rownames("UC")                   # devolve como rownames

df_merged[is.na(df_merged)] <- 0

df_merged[] <- lapply(df_merged, function(x) {
  if (is.character(x)) as.integer(x) else x
})

save(df_merged, file = "Data/all_spp_rich.RData")

## iNEXT

str(df_merged[, 1:5])

rich <- iNEXT(t(df_merged), q = 0, datatype = "abundance")

saveRDS(rich, file = "Data/rich.rds")
