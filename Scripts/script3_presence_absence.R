# PACKAGES
library(letsR) #install_github("macroecology/letsR")
library(sf)
library(sampbias) # install_github("azizka/sampbias")
library(rnaturalearth)

# LOAD
load("Data/coordenadas_all.RData")
coords <- coordenadas_all
rm("coordenadas_all")

# Example polygon of Brazil
br <- ne_countries(scale = "medium", country = "Brazil", returnclass = "sf")

# Spatial intersection test
st_crs(coords) <- st_crs(br)

inside <- st_intersects(coords, br, sparse = FALSE)

# Filter points that intersect with Brazil polygon
coords <- coords[inside[,1], ]

# Check unique species name
coords$Species%>%unique()%>%length()

# If want to see the plot (Takes some time)
#plot(sf::st_geometry(br))
#plot(sf::st_geometry(coords), add = TRUE, pch = 20, col = "red", alpha = .2)

# Protected areas
UCs <-  terra::vect("Data/shp_cnuc_2024_02/cnuc_2024_02.shp")
UCs <- UCs[UCs$esfera == c("Federal", "Estadual"), ]
UCs <- UCs[UCs$categoria != "Reserva Particular do Patrimônio Natural",]
UCs <- UCs[is.na(UCs$marinho) | UCs$marinho == "", ]

# Extract points
coo_mat <- st_coordinates(coords)
crs = "+proj=longlat +datum=WGS84 +no_defs"


pam_inv <- lets.presab.grid.points(coo_mat, coords$Species, 
                                   UCs, "uc_id",
                                   abundance = TRUE)


#UCs_sf <- st_as_sf(UCs)
#UCs_sf

#UCs_sf<-UCs_sf%>%st_transform(., crs = st_crs(crs))

# Check total number of occurrences
pam_inv$PAM[,-1]%>%sum()

# Check total number of species
pam_inv$PAM%>%dim()

# Total species not included in any protected area
### (Unique invasive species retrived) - (total species maintained in pam_inv$PAM)
(coords$Species%>%unique()%>%length())  -  (pam_inv$PAM[,-1]%>%dim())[2]



# Get protected areas' names 
pam_nogeo<-pam_inv$grid%>%sf::st_as_sf()%>%
  sf::st_drop_geometry()%>%select(uc_id,nome_uc)


# Get protected areas' names from the community matrix 
pam_IDs_only<-pam_inv$PAM%>%select(sample.unit)

# Merge protected areas' name from spatial object and community matrix 
pam_namesID<-left_join(pam_IDs_only, pam_nogeo, by=c("sample.unit"="uc_id"))

#Check
pam_namesID%>%glimpse()

# Substitute protected area ID for protected area names in the community matrix
pam_inv$PAM[,1]<-pam_namesID$nome_uc

pam_inv$PAM%>%glimpse()

# Check total number of occurrences. Must be equal to last check
pam_inv$PAM[,-1]%>%sum()

# Remove any potential rownames
rownames(pam_inv$PAM)<-NULL

## Plot
rich_plus1 <- rowSums(pam_inv$PAM[, -1, drop = FALSE]) + 1
colfunc <- colorRampPalette(c("#fff5f0", "#fb6a4a", "#67000d"))
colors <- c("white", colfunc(max(rich_plus1)))
occs <- terra::vect(coo_mat, crs = crs)
plot(pam_inv$grid, border = "gray40",
     col = colors[rich_plus1])
plot(sf::st_geometry(wrld_simpl), add = TRUE)

# Save
write.csv(pam_inv$PAM, file = "Data/presab.csv")

# BIAS ANALYSES
# Only 76.72% of the Federal and State PAs have registered invasive species occurrences
round(100 * (sum(rich_plus1 != 1) / length(rich_plus1)), 2)

# SAMPBIAS EVALUATION 
# There is a bias towards roads,airports, waterbodies and cities
sampbias.in <- data.frame("decimalLongitude" = coo_mat[, 1], 
                          "decimalLatitude" = coo_mat[, 2],
                          "species" = coords$Species)
sampbias.out <- calculate_bias(x = sampbias.in)
summary(sampbias.out)
plot(sampbias.out)
proj <- project_bias(sampbias.out)
map_bias(proj, type = "log_sampling_rate")

# SPECIES ACCUMULATION

accumula <- specaccum(presab)
plot(accumula)
save(accumula, file = "Data/accumula.RData")

# Plot Sites
plot_data <- data.frame("Locais" = c(0, accumula$sites),
                        "Riqueza" = c(0, accumula$richness),
                        "lower" = c(0, accumula$richness - accumula$sd),
                        "upper" = c(0, accumula$richness + accumula$sd))
g <- ggplot(plot_data, aes(x = Locais, y = Riqueza)) +
  geom_point(color = "blue", size = 2) +
  geom_line(color = "blue", lwd = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              linetype=2, alpha=0.3, fill = "orange") +
  ylab("Riqueza acumulada") +
  theme_classic() +
  theme(text = element_text(size = 16))
g

ggsave("Figures/Rarefac.png")
