### DATA EXTRACTION

library(sf)
library(nngeo)
library(terra)
library(dplyr)
library(exactextractr)
library(rgee)
library(googledrive)
library(readr)
library(tidyr)

## ════════════════════════════════════════════════════
## PREPARATION
## ════════════════════════════════════════════════════

## Destination folder
pasta_destino <- "Data/mapbiomas_dados"
dir.create(pasta_destino, showWarnings = FALSE)

## Create initial dataframe
UCs <- terra::vect("Data/shp_cnuc_2024_02/cnuc_2024_02.shp")
UCs <- UCs[UCs$esfera == c("Federal", "Estadual"), ]
UCs <- UCs[UCs$categoria != "Reserva Particular do Patrimônio Natural", ]
UCs <- UCs[is.na(UCs$marinho) | UCs$marinho == "", ]

n <- 451
vari <- data.frame(
  PA             = rep(NA, n),
  polyg          = rep(NA, n),
  category       = rep(NA, n),
  mp             = rep(NA, n),
  fu             = rep(NA, n),
  year           = rep(NA, n),
  adm            = rep(NA, n),
  urb_dist       = rep(NA, n),
  min_dist_pa    = rep(NA, n),
  area           = rep(NA, n),
  biome          = rep(NA, n),
  humidity       = rep(NA, n),
  water_bodies   = rep(NA, n),
  coverage       = rep(NA, n),
  mean_temp      = rep(NA, n),
  altitude       = rep(NA, n),
  spp_rich       = rep(NA, n),
  invas_spp_sich = rep(NA, n)
)

vari$nome_uc <- as.character(UCs$nome_uc)

## ════════════════════════════════════════════════════
## GOOGLE EARTH ENGINE AUTHENTICATION
## ════════════════════════════════════════════════════

reticulate::use_condaenv("base", required = TRUE)

ee <- reticulate::import("ee")
ee$Authenticate()
ee$Initialize(project = "ee-luizaklippel")

## ════════════════════════════════════════════════════
## LOAD SHAPEFILE AND GEE CONFIGURATIONS
## ════════════════════════════════════════════════════

shapefile <- ee$FeatureCollection("users/luizaklippel/MAPBIOMAS/cnuc_2025_08")$
  map(ee_utils_pyfunc(function(feature) {
    feature$set("geomType", feature$geometry()$type())
  }))$
  filter(ee$Filter$inList("geomType", list("Polygon", "MultiPolygon")))

anos <- ee$List$sequence(1985L, 2024L)

mapbiomas <- ee$Image(
  "projects/mapbiomas-public/assets/brazil/lulc/collection10/mapbiomas_brazil_collection10_coverage_v2"
)

# Initial accumulator image for iterate (replaces ee$Image(list()) which is invalid in R)
img_inicial <- ee$Image$constant(0)$rename("init")$toFloat()

## ════════════════════════════════════════════════════
## VARIABLES EXTRACTION — GEE TASKS
## ════════════════════════════════════════════════════

# 1. Distance to urban centers

imagemDistancia <- ee$Image(
  anos$iterate(
    ee_utils_pyfunc(function(ano, imgAcumulada) {
      anoInt        <- ee$Number(ano)$int()
      bandaNome     <- ee$String("D_")$cat(ee$String(anoInt))
      classificacao <- mapbiomas$select(
        ee$String("classification_")$cat(ee$String(anoInt))
      )
      urbano    <- classificacao$eq(24L)
      distancia <- urbano$fastDistanceTransform(2048L)$
        sqrt()$multiply(30)$divide(1000)$rename(bandaNome)
      ee$Image(imgAcumulada)$addBands(distancia)
    }),
    img_inicial
  )
)$select(paste0("D_", 1985:2024))

imagemDistancia$reduceRegions(
  collection = shapefile,
  reducer    = ee$Reducer$min(),
  scale      = 30L
) |>
  (\(x) ee$batch$Export$table$toDrive(
    collection  = x,
    description = "Distancia_Urbano_por_Poligono",
    fileFormat  = "CSV"
  )$start())()

cat("✔ Task 1 started: Distancia_Urbano_por_Poligono\n")

# 2. PA polygon area (hectares)

shapefile$map(ee_utils_pyfunc(function(feature) {
  areaHa <- feature$geometry()$area(1)$divide(10000)
  feature$set("Area_ha", areaHa)$select(list("Area_ha"))
})) |>
  (\(x) ee$batch$Export$table$toDrive(
    collection  = x,
    description = "Area_por_Poligono",
    fileFormat  = "CSV"
  )$start())()

cat("✔ Task 2 started: Area_por_Poligono\n")

# 3. Relative humidity

imagemUmid <- ee$Image(
  anos$iterate(
    ee_utils_pyfunc(function(ano, imgAcumulada) {
      anoInt    <- ee$Number(ano)$int()
      bandaNome <- ee$String("U_")$cat(ee$String(anoInt))
      colecao   <- ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$
        filter(ee$Filter$calendarRange(anoInt, anoInt, "year"))
      T  <- colecao$select("temperature_2m")$mean()$subtract(273.15)
      Td <- colecao$select("dewpoint_temperature_2m")$mean()$subtract(273.15)
      rh <- (Td$multiply(17.625)$divide(Td$add(243.04))$exp())$
        divide(T$multiply(17.625)$divide(T$add(243.04))$exp())$
        multiply(100)$rename(bandaNome)
      ee$Image(imgAcumulada)$addBands(rh)
    }),
    img_inicial
  )
)$select(paste0("U_", 1985:2024))

imagemUmid$reduceRegions(
  collection = shapefile,
  reducer    = ee$Reducer$mean(),
  scale      = 11132L
) |>
  (\(x) ee$batch$Export$table$toDrive(
    collection  = x,
    description = "Umidade_Media_por_Poligono",
    fileFormat  = "CSV"
  )$start())()

cat("✔ Task 3 started: Umidade_Media_por_Poligono\n")

# 4. Water bodies area (hectares)

imagemAgua <- ee$Image(
  anos$iterate(
    ee_utils_pyfunc(function(ano, imgAcumulada) {
      anoInt        <- ee$Number(ano)$int()
      bandaNome     <- ee$String("W_")$cat(ee$String(anoInt))
      classificacao <- mapbiomas$select(
        ee$String("classification_")$cat(ee$String(anoInt))
      )
      agua <- classificacao$eq(33L)$
        Or(classificacao$eq(31L))$
        Or(classificacao$eq(34L))
      areaAgua <- agua$multiply(ee$Image$pixelArea())$
        divide(10000)$rename(bandaNome)
      ee$Image(imgAcumulada)$addBands(areaAgua)
    }),
    img_inicial
  )
)$select(paste0("W_", 1985:2024))

imagemAgua$reduceRegions(
  collection = shapefile,
  reducer    = ee$Reducer$sum(),
  scale      = 30L
) |>
  (\(x) ee$batch$Export$table$toDrive(
    collection  = x,
    description = "Area_Agua_por_Poligono",
    fileFormat  = "CSV"
  )$start())()

cat("✔ Task 4 started: Area_Agua_por_Poligono\n")

# 5. Mean temperature (°C)

imagemTemp <- ee$Image(
  anos$iterate(
    ee_utils_pyfunc(function(ano, imgAcumulada) {
      anoInt    <- ee$Number(ano)$int()
      bandaNome <- ee$String("T_")$cat(ee$String(anoInt))
      tempAnual <- ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$
        filter(ee$Filter$calendarRange(anoInt, anoInt, "year"))$
        select("temperature_2m")$mean()$subtract(273.15)$rename(bandaNome)
      ee$Image(imgAcumulada)$addBands(tempAnual)
    }),
    img_inicial
  )
)$select(paste0("T_", 1985:2024))

imagemTemp$reduceRegions(
  collection = shapefile,
  reducer    = ee$Reducer$mean(),
  scale      = 11132L
) |>
  (\(x) ee$batch$Export$table$toDrive(
    collection  = x,
    description = "Temperatura_Media_por_Poligono",
    fileFormat  = "CSV"
  )$start())()

cat("✔ Task 5 started: Temperatura_Media_por_Poligono\n")

# 6. Altitude (meters)

srtm <- ee$Image("USGS/SRTMGL1_003")$select("elevation")

srtm$reduceRegions(
  collection = shapefile,
  reducer    = ee$Reducer$mean(),
  scale      = 30L
)$map(ee_utils_pyfunc(function(f) {
  f$set("Alt_media", f$get("mean"))$select(list("Alt_media"))
})) |>
  (\(x) ee$batch$Export$table$toDrive(
    collection  = x,
    description = "Altitude_Media_por_Poligono",
    fileFormat  = "CSV"
  )$start())()

cat("✔ Task 6 started: Altitude_Media_por_Poligono\n")
cat("\n⏳ Wait for tasks to complete at: https://code.earthengine.google.com/tasks\n")

## ════════════════════════════════════════════════════
## VARIABLES EXTRACTION — LOCAL (no GEE required)
## ════════════════════════════════════════════════════

# 7. Distance to closest PA (km)

dist_matrix <- distance(UCs, UCs)
diag(dist_matrix) <- Inf
vari$min_dist_pa  <- apply(dist_matrix, 1, min) / 1000  # meters → km

# 8. Vegetation coverage (MapBiomas via exactextractr)

UCs_sf <- st_as_sf(UCs)
ucs  <- st_transform(UCs_sf, crs = 4326)

mapbiomas_cob <- ee$Image(
  "projects/mapbiomas-public/assets/brazil/lulc/collection10/mapbiomas_brazil_collection10_coverage_v2"
)

mb_2024 <- mapbiomas_cob$select("classification_2024")

# Folder in Google Drive to store individual PA rasters
drive_folder <- "mapbiomas_ucs_tiles"

# Export one GeoTIFF per PA
n_ucs <- nrow(ucs)

for (i in seq_len(n_ucs)) {
  
  uc_id_val   <- as.character(ucs$uc_id[i])
  description <- paste0("mb2024_uc_", uc_id_val)
  
  # Check if task already exported (avoids re-running completed ones)
  arquivo_local <- file.path(pasta_destino, "tiles", paste0(description, ".tif"))
  if (file.exists(arquivo_local)) {
    cat("⏭ Skipping (already exists):", description, "\n")
    next
  }
  
  # Get PA geometry
  geom_uc <- ee$Geometry$Rectangle(
    coords = as.numeric(sf::st_bbox(ucs[i, ])),
    proj   = "EPSG:4326",
    geodesic = FALSE
  )
  
  # Clip raster to PA
  mb_clip <- mb_2024$clip(geom_uc)
  
  # Export to Drive
  task <- ee_image_to_drive(
    image       = mb_clip,
    description = description,
    folder      = drive_folder,
    fileFormat  = "GeoTIFF",
    scale       = 30,
    region      = geom_uc,
    maxPixels   = 1e10
  )
  task$start()
  
  cat(sprintf("✔ Task started [%d/%d]: %s\n", i, n_ucs, description))
  
  # Small pause to avoid overwhelming GEE task queue (max 3000 tasks)
  if (i %% 50 == 0) {
    cat("⏳ Pausing 30s to avoid task queue overflow...\n")
    Sys.sleep(30)
  }
}

#### AFTER ALL TASKS ARE COMPLETED ###

# Create local folder for tiles
dir.create(file.path(pasta_destino, "tiles"), showWarnings = FALSE, recursive = TRUE)

# Download all tiles from Drive
tiles_drive <- drive_ls(path = drive_folder)

for (i in seq_len(nrow(tiles_drive))) {
  nome     <- tiles_drive$name[i]
  destino  <- file.path(pasta_destino, "tiles", nome)
  
  if (file.exists(destino)) {
    cat("⏭ Already downloaded:", nome, "\n")
    next
  }
  
  drive_download(
    file      = tiles_drive[i, ],
    path      = destino,
    overwrite = TRUE
  )
  cat(sprintf("✔ Downloaded [%d/%d]: %s\n", i, nrow(tiles_drive), nome))
}

# Load all tiles and extract coverage per PA
tile_files <- list.files(
  file.path(pasta_destino, "tiles"),
  pattern = "^mb2024_uc_.*\\.tif$",
  full.names = TRUE
)

cat("\n── Extracting coverage fractions ──\n")

legenda <- data.frame(
  codigo = c(1, 3, 4, 5, 6, 9, 11, 12, 13, 15, 18, 19,
             20, 21, 22, 23, 24, 25, 26, 29, 30, 31, 32, 33, 36),
  classe = c("Floresta", "Formação Florestal", "Formação Savânica",
             "Mangue", "Floresta Alagável", "Silvicultura",
             "Campo Alagado e Área Pantanosa", "Formação Campestre",
             "Afloramento Rochoso", "Pastagem", "Agricultura",
             "Lavoura Temporária", "Cana", "Mosaico de Usos",
             "Área não Vegetada", "Praia e Duna", "Área Urbanizada",
             "Área Degradada", "Açude", "Afloramento Rochoso",
             "Mineração", "Aquicultura", "Apicum", "Rio/Lago/Oceano",
             "Lavoura Perene")
)

classes_vegetacao <- c(
  "Floresta", "Formação Florestal", "Formação Savânica",
  "Mangue", "Floresta Alagável",
  "Campo Alagado e Área Pantanosa", "Formação Campestre",
  "Afloramento Rochoso", "Praia e Duna",
  "Área Degradada", "Apicum"
)

# Extract coverage for each PA from its corresponding tile
resultados_lista <- vector("list", length(tile_files))

head(basename(tile_files))
gsub("^mb2024_uc_|_\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2}_\\d{2}\\.tif$", "", 
     basename(head(tile_files)))

ucs_original <- ucs

for (i in seq_along(tile_files)) {
  
  # Extract uc_id from filename
  basename_clean <- basename(tile_files[i])
  uc_id_val <- gsub("^mb2024_uc_|_\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2}_\\d{2}\\.tif$",
                    "", basename_clean)
  
  # Always match against the original clean copy
  idx <- which(as.character(ucs_original$uc_id) == uc_id_val)
  
  if (length(idx) == 0) {
    cat("⚠ PA not found for tile:", basename_clean, "| extracted id:", uc_id_val, "\n")
    next
  }
  
  mb_tile <- tryCatch(rast(tile_files[i]), error = function(e) NULL)
  if (is.null(mb_tile)) {
    cat("⚠ Could not load tile:", basename_clean, "\n")
    next
  }
  
  # Reproject only a temporary copy, never overwrite ucs_original
  uc_reproj <- st_transform(ucs_original[idx, ], crs(mb_tile))
  frac      <- exact_extract(x = mb_tile, y = uc_reproj, fun = "frac")
  
  resultados_lista[[i]] <- bind_cols(
    st_drop_geometry(ucs_original[idx, ]),
    frac
  )
  
  cat(sprintf("✔ Extracted [%d/%d]: UC %s\n", i, length(tile_files), uc_id_val))
}

# Combine all results
ucs_resultado <- bind_rows(resultados_lista)

# Reshape and join legend
resultado_longo <- ucs_resultado %>%
  pivot_longer(
    cols         = starts_with("frac_"),
    names_to     = "codigo",
    names_prefix = "frac_",
    values_to    = "fracao"
  ) %>%
  mutate(codigo = as.integer(codigo)) %>%
  left_join(legenda, by = "codigo") %>%
  filter(fracao > 0)

# Save full coverage table
write.csv(
  resultado_longo,
  file.path(pasta_destino, "cobertura_por_uc_2024.csv"),
  row.names = FALSE
)

# Native vegetation rate per PA
taxa_vegetacao <- resultado_longo %>%
  mutate(is_vegetacao = classe %in% classes_vegetacao) %>%
  group_by(uc_id) %>%
  summarise(
    taxa_vegetacao_nativa     = sum(fracao[is_vegetacao], na.rm = TRUE),
    taxa_nao_vegetacao        = 1 - sum(fracao[is_vegetacao], na.rm = TRUE),
    taxa_vegetacao_nativa_pct = round(taxa_vegetacao_nativa * 100, 2)
  )

write.csv(
  taxa_vegetacao,
  file.path(pasta_destino, "taxa_vegetacao_por_uc.csv"),
  row.names = FALSE
)



## ════════════════════════════════════════════════════
## DOWNLOAD CSVs FROM GOOGLE DRIVE
## (run only after all GEE tasks are COMPLETED)
## ════════════════════════════════════════════════════

drive_auth(email = "luizaklippel@ufba.br")

baixar_csv_local <- function(nome_arquivo) {
  arquivo <- drive_find(pattern = nome_arquivo, type = "csv")
  if (nrow(arquivo) == 0) {
    cat("⚠ File not found yet:", nome_arquivo, "\n")
    return(NULL)
  }
  caminho_local <- file.path(pasta_destino, paste0(nome_arquivo, ".csv"))
  drive_download(arquivo[1, ], path = caminho_local, overwrite = TRUE)
  df <- read_csv(caminho_local, show_col_types = FALSE)
  cat("✔", nome_arquivo, "saved to:", caminho_local, "\n")
  return(df)
}

df_dist_urbano <- baixar_csv_local("Distancia_Urbano_por_Poligono")
df_area        <- baixar_csv_local("Area_por_Poligono")
df_umidade     <- baixar_csv_local("Umidade_Media_por_Poligono")
df_agua        <- baixar_csv_local("Area_Agua_por_Poligono")
df_temperatura <- baixar_csv_local("Temperatura_Media_por_Poligono")
df_altitude    <- baixar_csv_local("Altitude_Media_por_Poligono")

## ════════════════════════════════════════════════════
## FIX UC_ID AND ADD EXTRACTED DATA TO ORIGINAL DATAFRAME
## ════════════════════════════════════════════════════

# Check nome_uc in both objects
cat("df_temperatura$nome_uc:", head(df_temperatura$nome_uc), "\n")
cat("UCs$nome_uc:           ", head(UCs$nome_uc), "\n")

# Add nome_uc as join key to vari
vari$nome_uc <- as.character(UCs$nome_uc)

# Updated join function using nome_uc
join_por_nome <- function(df_vari, df_externo, col_valor, col_destino) {
  if (is.null(df_externo)) {
    cat("⚠ Missing data for:", col_destino, "\n")
    return(df_vari)
  }
  if (!col_valor %in% names(df_externo)) {
    cat("⚠ Column", col_valor, "not found for:", col_destino, "\n")
    return(df_vari)
  }
  df_externo$nome_uc <- as.character(df_externo$nome_uc)
  df_vari[[col_destino]] <- df_externo[[col_valor]][
    match(df_vari$nome_uc, df_externo$nome_uc)
  ]
  return(df_vari)
}

# Distance to urban centers (mean across all years)
if (!is.null(df_dist_urbano)) {
  cols_d <- grep("^D_", names(df_dist_urbano), value = TRUE)
  df_dist_urbano$urb_dist_media <- rowMeans(df_dist_urbano[, cols_d], na.rm = TRUE)
  vari <- join_por_nome(vari, df_dist_urbano, "urb_dist_media", "urb_dist")
}

# Polygon area (hectares)
vari <- join_por_nome(vari, df_area, "Area_ha", "area")

# Relative humidity (mean across all years)
if (!is.null(df_umidade)) {
  cols_u <- grep("^U_", names(df_umidade), value = TRUE)
  df_umidade$humidity_media <- rowMeans(df_umidade[, cols_u], na.rm = TRUE)
  vari <- join_por_nome(vari, df_umidade, "humidity_media", "humidity")
}

# Water bodies area (mean across all years, hectares)
if (!is.null(df_agua)) {
  cols_w <- grep("^W_", names(df_agua), value = TRUE)
  df_agua$water_media <- rowMeans(df_agua[, cols_w], na.rm = TRUE)
  vari <- join_por_nome(vari, df_agua, "water_media", "water_bodies")
}

# Mean temperature (mean across all years, °C)
if (!is.null(df_temperatura)) {
  cols_t <- grep("^T_", names(df_temperatura), value = TRUE)
  df_temperatura$temp_media <- rowMeans(df_temperatura[, cols_t], na.rm = TRUE)
  vari <- join_por_nome(vari, df_temperatura, "temp_media", "mean_temp")
}

# Altitude (meters)
vari <- join_por_nome(vari, df_altitude, "Alt_media", "altitude")

# Distance to closest PA (km)
vari$min_dist_pa <- apply(dist_matrix, 1, min) / 1000

# Native vegetation coverage (%)
if (exists("taxa_vegetacao") && nrow(taxa_vegetacao) > 0) {
  taxa_vegetacao$nome_uc <- as.character(taxa_vegetacao$nome_uc)
  vari <- join_por_nome(vari, taxa_vegetacao, "taxa_vegetacao_nativa_pct", "coverage")
}

# Quick fill check
cat("\n── Variables fill summary ──\n")
cat("urb_dist:     ", sum(!is.na(vari$urb_dist)),     "/", nrow(vari), "\n")
cat("min_dist_pa:  ", sum(!is.na(vari$min_dist_pa)),  "/", nrow(vari), "\n")
cat("area:         ", sum(!is.na(vari$area)),          "/", nrow(vari), "\n")
cat("humidity:     ", sum(!is.na(vari$humidity)),      "/", nrow(vari), "\n")
cat("water_bodies: ", sum(!is.na(vari$water_bodies)),  "/", nrow(vari), "\n")
cat("mean_temp:    ", sum(!is.na(vari$mean_temp)),     "/", nrow(vari), "\n")
cat("altitude:     ", sum(!is.na(vari$altitude)),      "/", nrow(vari), "\n")
cat("coverage:     ", sum(!is.na(vari$coverage)),      "/", nrow(vari), "\n")

# Check which UCs did not match
unmatched <- vari$nome_uc[!vari$nome_uc %in% df_temperatura$nome_uc]
cat("\n── Unmatched UCs (", length(unmatched), ") ──\n")
print(unmatched)

# Save final dataframe
write.csv(
  vari,
  file.path(pasta_destino, "vari_completo.csv"),
  row.names = FALSE
)

cat("\n✔ Final dataframe saved to:", file.path(pasta_destino, "vari_completo.csv"), "\n")
cat("  Dimensions:", nrow(vari), "rows x", ncol(vari), "columns\n")

