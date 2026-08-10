library(sf)
library(terra)
library(iNEXT)
library(letsR)
library(rnaturalearth)
library(dplyr)

UCs <-  terra::vect("Data/shp_cnuc_2024_02/cnuc_2024_02.shp")
UCs <- UCs[UCs$esfera == c("Federal", "Estadual"), ]
UCs <- UCs[UCs$categoria != "Reserva Particular do Patrimônio Natural",]
UCs <- UCs[is.na(UCs$marinho) | UCs$marinho == "", ]

load("Data/coordenadas_all.RData")
coords <- coordenadas_all
rm("coordenadas_all")


UCs_sf <- st_as_sf(UCs)
UCs_sf

buf <- st_buffer(UCs_sf, dist = 1000)

br <- ne_countries(scale = "medium", country = "Brazil", returnclass = "sf")

# Spatial intersection test
st_crs(coords) <- st_crs(br)

inside <- st_intersects(coords, br, sparse = FALSE)

inside%>%dplyr::glimpse()

# Filter points that intersect with Brazil polygon
coords <- coords[inside[,1], ]

# Check unique species name
coords$Species%>%unique()%>%length()

coo_mat <- st_coordinates(coords)
# crs = "+proj=longlat +datum=WGS84 +no_defs"
crs = crs(coords)  # Mudei para ver se o problema era mismatch entre crs
buf <- st_transform(buf, crs = 4326)

bufvect <- vect(buf)

pam_inv <- lets.presab.grid.points(coo_mat, coords$Species, 
                                   bufvect, "uc_id",
                                   abundance = TRUE)



# Check total number of occurrences
pam_inv$PAM[,-1]%>%sum()

# Check total number of species
pam_inv$PAM%>%dim()


# Total species not included in any protected area
### (Unique invasive species retrived) - (total species maintained in pam_inv$PAM)
(coords$Species%>%unique()%>%length())  -  (pam_inv$PAM[,-1]%>%dim())[2]


##Add the UCs name to the id for iNEXT
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

# Save

write.csv(pam_inv$PAM, file = "Data/presab_buf.csv")



mydata <- read.csv(file = "Data/presab.csv")
community <- as.matrix(mydata[, -c(1,2)])
rownames(community)<-mydata$sample.unit

rem <- rowSums(community) < 1

community <- t(community[!rem, ])

# iNEXT
out <- iNEXT(community, q = 0,
             datatype = "abundance",)

save(out,file = "Data/out_buf.RData")
write.csv(out$AsyEst, file = "Data/invas_est_buf.csv")




