### DATA EXTRACTION

library(sf)
library(nngeo)
library(terra)
library(dplyr)
library(exactextractr)
library(rgee)

## CREATE A DATAFRAME

UCs <-  terra::vect("Data/shp_cnuc_2024_02/cnuc_2024_02.shp")
UCs <- UCs[UCs$esfera == c("Federal", "Estadual"), ]
UCs <- UCs[UCs$categoria != "Reserva Particular do Patrimônio Natural",]
UCs <- UCs[is.na(UCs$marinho) | UCs$marinho == "", ]

ucs2 <- read.csv2("Data/cnuc_2025_08.csv")
ucs2_filtrado <- ucs2 %>%
  select(uc_id = ID_UC, biome= Bioma.declarado, year = Ano.de.Criação) %>%
  mutate(uc_id = gsub("\\.", "", uc_id))

UCs_df <- as.data.frame(UCs)
UCs_sf <- st_as_sf(UCs)

vari <- UCs_df %>%
  select(cd_cnuc, uc_id, nome_uc) %>%
  left_join(ucs2_filtrado, by = "uc_id") %>%
  mutate(
    polyg       = UCs_sf$geometry,
    category    = UCs_df$categoria,
    mp          = UCs_df$pl_manejo,
    fu          = UCs_df$uf,
    year        = .$year,
    adm         = UCs_df$esfera,
    area        = UCs_df$ha_total,
    biome       = .$biome,  
    urb_dist    = NA,
    min_dist_pa = NA,
    humidity    = NA,
    water_bodies = NA,
    coverage    = NA,
    mean_temp   = NA,
    altitude    = NA,
    spp_rich    = NA,
    invas_spp_ich = NA
  )

## GET THE DATA

# Distance to Urban Centers (km)

urb_dist <- read.csv("Data/Management/Distancia_Urbano_por_Poligono.csv")

vari <- vari %>%
  select(-urb_dist) %>%
  left_join(
    urb_dist %>% select(cd_cnuc = cd_cnuc, urb_dist = D_2024),
    by = "cd_cnuc"
  )

# Closest Protected Area (km)

#dist_matrix <- distance(UCs, UCs)
#diag(dist_matrix) <- Inf
#vari$min_dist_pa  <- apply(dist_matrix, 1, min) / 1000  # meters → km

min_dist <- sapply(1:nrow(UCs_sf), function(i) {
  dists <- st_distance(UCs_sf[i, ], UCs_sf[-i, ])
  min(dists)
})

vari$min_dist_pa <- min_dist

# Humidity

humidity <- read.csv("Data/Environment/Umidade_Media_por_poligono.csv")

vari <- vari %>%
  select(-humidity) %>%
  left_join(
    humidity %>% select(cd_cnuc = cd_cnuc, humidity = U_2024),
    by = "cd_cnuc"
  )

# Water Bodies Area (hectare)


water_bodies <- read.csv("Data/Environment/Area_Agua_por_Poligono.csv")

vari <- vari %>%
  select(-water_bodies) %>%
  left_join(
    water_bodies %>% select(cd_cnuc = cd_cnuc, water_bodies = W_2024),
    by = "cd_cnuc"
  )

# Vegetation Coverage (%)

coverage <- read.csv("Data/Environment/Export_Percentuais_Cobertura.csv")

vari <- vari %>%
  select(-coverage) %>%
  left_join(
    coverage %>% select(cd_cnuc = cd_cnuc, coverage = F_2024),
    by = "cd_cnuc"
  )

# Mean Temperature (ºC)

mean_temp <- read.csv("Data/Environment/Temperatura_Media_por_Poligono.csv")

vari <- vari %>%
  select(-mean_temp) %>%
  left_join(
    mean_temp %>% select(cd_cnuc = cd_cnuc, mean_temp = T_2024),
    by = "cd_cnuc"
  )

# Altitude (meters)

altitude <- read.csv("Data/Environment/Altitude_Media_por_Poligono.csv")

vari <- vari %>%
  select(-altitude) %>%
  left_join(
    altitude %>% select(cd_cnuc = cd_cnuc, altitude = Alt_media),
    by = "cd_cnuc"
  )

# Invasive Species Richness

rich <- read.csv("Data/invas_est.csv")

rich <- rich %>% filter(Diversity == "Species richness")%>%
  rename(nome_uc = Assemblage)%>%
  select(-Diversity)

vari <- vari %>%
  select(-invas_spp_ich)%>%
  left_join(
    rich %>% select(nome_uc = nome_uc, invas_obs = Observed, invas_est = Estimator,
                    invas_s.e. = s.e.),
    by = "nome_uc"
  )


# All Species Richness

rich <- readRDS("Data/rich.rds")
rich <- rich$AsyEst
rich <- rich %>% filter(Diversity == "Species richness")%>%
  rename(nome_uc = Assemblage)%>%
  select(-Diversity)

vari <- vari %>%
  select(-spp_rich)%>%
  left_join(
    rich %>% select(nome_uc = nome_uc, spp_obs = Observed, spp_est = Estimator,
                    spp_s.e. = s.e.),
    by = "nome_uc"
  )

# Invasive Species Richness Buffer

rich<- read.csv("Data/invas_est_buf.csv")

rich <- rich %>% 
  filter(Diversity == "Species richness")%>%
  rename(nome_uc = Assemblage)%>%
  select(-Diversity)

vari <- vari %>%
  left_join(
    rich %>% select(nome_uc = nome_uc, buff_obs = Observed, buff_est = Estimator,
                    buff_s.e. = s.e.),
    by = "nome_uc"
  )

vari <- vari%>%
  mutate(pct_pot_spp = (invas_obs/buff_obs)*100)

vari <- vari%>%
  mutate(pct_pot_spp_est = (invas_est/ buff_est)*100)

### SAVE FINAL DATAFRAME

save(vari, file = "Data/vari.RData")




