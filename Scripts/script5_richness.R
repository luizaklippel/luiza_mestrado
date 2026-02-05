# SPECIES RICHNESS AND COMPLETENESS
# Rarefaction and Extrapolation

library(iNEXT)
library(dplyr)
library(tidyr)
library(ggpubr)
library(ggplot2)
library(ggridges)
library(viridis)
library(sf)
library(cowplot)

# DATA 

sample_sizes <- c(30, 50, 100, 500, 1000, 2500, 5000)
site_totals <- rowSums(presab_final)
area_names <- rownames(presab_final)  

results_table <- data.frame(
  UC = area_names,
  Pontos_Totais = rowSums(presab_final),
  Riqueza_Observada = rowSums(presab_final > 0),
  Riqueza_Assimptotica = NA,
  Completude = NA,
  stringsAsFactors = FALSE
)

# Add columns for each sample size
for(size in sample_sizes) {
  results_table[[paste0("Riqueza_", size, "_pontos")]] <- NA
}


# PROCESS EACH CONSERVATION UNIT

for(i in 1:nrow(presab_final)) {
  cat("Processing:", area_names[i], "\n")
  
  single_area_data <- presab_final[i, , drop = FALSE]
  total_points <- sum(single_area_data)
  
  # Skip sites with insufficient data
  if(total_points < 10 || sum(single_area_data > 0) < 2) {
    next
  }
  
  # Valid sample sizes for rarefaction and extrapolation
  rarefaction_sizes <- sample_sizes[sample_sizes < total_points]
  extrapolation_sizes <- sample_sizes[sample_sizes > total_points & sample_sizes <= total_points * 2]
  all_valid_sizes <- c(rarefaction_sizes, extrapolation_sizes)
  
  if(length(all_valid_sizes) > 0) {
    
    # iNEXT
    out_single <- iNEXT(t(single_area_data), q = 0, 
                        datatype = "abundance",
                        size = all_valid_sizes,
                        endpoint = max(total_points * 2, max(sample_sizes)))
    
    # Estimates
    estimates <- out_single$iNextEst$size_based
    
    # Estimated richness for each sample size
    for(size in sample_sizes) {
      col_name <- paste0("Riqueza_", size, "_pontos")
      
      if(size < total_points) {
        
        # RAREFACTION
        size_row <- estimates[estimates$m == size & estimates$Method == "Rarefaction", ]
        if(nrow(size_row) > 0) {
          results_table[i, col_name] <- round(size_row$qD[1], 1)
        }
      } else if(size == total_points) {
        
        # OBSERVED
        results_table[i, col_name] <- results_table$Riqueza_Observada[i]
      } else {
        
        # EXTRAPOLATION
        size_row <- estimates[estimates$m == size & estimates$Method == "Extrapolation", ]
        if(nrow(size_row) > 0) {
          results_table[i, col_name] <- round(size_row$qD[1], 1)
        } else {
          
          # If no extrapolation available, asymptotic estimate
          asym_estimates <- out_single$AsyEst
          if(nrow(asym_estimates) > 0) {
            results_table[i, col_name] <- round(asym_estimates$Estimator[1], 1)
          }
        }
      }
    }
    
    # ASYMPTOTIC ESTIMATES
    asym_estimates <- out_single$AsyEst
    
    if(nrow(asym_estimates) > 0) {
      
      # Asymptotic richness (estimated total richness)
      results_table$Riqueza_Assimptotica[i] <- round(asym_estimates$Estimator[1], 1)
      
      # Completeness Observed / Asymptotic 100
      results_table$Completude[i] <- round(
        (results_table$Riqueza_Observada[i] / asym_estimates$Estimator[1]) * 100, 1
      )
    }
  }
}

# iNEXT FOR ALL UCs

t_mat_uni2_clean <- t(mat_uni2_clean)

# Combine all UCs 
all_ucs_combined <- colSums(t_mat_uni2_clean)  

# Create named list
combined_list <- list("all_ucs" = all_ucs_combined)

t_mat_uni2_clean <- t(mat_uni2_clean)

t_mat_clean <- t_mat_uni2_clean[rowSums(t_mat_uni2_clean) > 0, ]
t_mat_clean <- t_mat_clean[, colSums(t_mat_clean) > 0]

t_df_clean <- data.frame(t_mat_clean)

test2 <- t_df_clean[,1:3]

inext.test2 <- iNEXT(test2, q=0, datatype="abundance", endpoint=500)

ggiNEXT(inext.test2, type=1)

todasjuntas <- apply(t_mat_clean, 1, sum)

out_juntas <- iNEXT(todasjuntas, 
                    q = 0,
                    datatype = "abundance",
                    endpoint = 5000)

ggiNEXT(out_juntas, type=1)

# iNEXT
out_all <- iNEXT(as.data.frame(t_mat_clean), 
                 q = 0,
                 datatype = "abundance",
                 endpoint = 5000)

# Plot
g_all <- ggiNEXT(out_all,
                 type = 1, 
                 color.var = "Assemblage") +
  theme_bw() +
  labs(title = "Rarefaction/Extrapolation Curve - All UCs Combined",
       x = "Number of individuals",
       y = "Species diversity") +
  theme(legend.position = "right")

g_all
ggsave("Results/iNEXT_all_ucs.png", g_all, width = 8, height = 6, dpi = 300 )


# COMPLETENESS CALCULATION

# Function for completeness specific sample sizes
calc_completeness_at_size <- function(riqueza_at_size, riqueza_assimptotica) {
  if(is.na(riqueza_at_size) || is.na(riqueza_assimptotica)) {
    return(NA)
  }
  return(round((riqueza_at_size / riqueza_assimptotica) * 100, 1))
}


# Calculate completeness at each sample size
results_table$Completude_30 <- mapply(calc_completeness_at_size, 
                                      results_table$Riqueza_30_pontos,
                                      results_table$Riqueza_Assimptotica)

results_table$Completude_50 <- mapply(calc_completeness_at_size, 
                                      results_table$Riqueza_50_pontos,
                                      results_table$Riqueza_Assimptotica)

results_table$Completude_100 <- mapply(calc_completeness_at_size, 
                                       results_table$Riqueza_100_pontos,
                                       results_table$Riqueza_Assimptotica)

results_table$Completude_500 <- mapply(calc_completeness_at_size, 
                                       results_table$Riqueza_500_pontos,
                                       results_table$Riqueza_Assimptotica)

results_table$Completude_1000 <- mapply(calc_completeness_at_size, 
                                        results_table$Riqueza_1000_pontos,
                                        results_table$Riqueza_Assimptotica)

results_table$Completude_2500 <- mapply(calc_completeness_at_size, 
                                        results_table$Riqueza_2500_pontos,
                                        results_table$Riqueza_Assimptotica)

results_table$Completude_5000 <- mapply(calc_completeness_at_size, 
                                        results_table$Riqueza_5000_pontos,
                                        results_table$Riqueza_Assimptotica)

riqueza_UCs <- results_table[order(-results_table$Pontos_Totais), ]

write.csv(riqueza_UCs, "Results/riqueza_UCs.csv", row.names = FALSE)



# VISUALIZATION

# Shapefiles 

UCs <- terra::vect("Data/shp_cnuc_2024_02/cnuc_2024_02.shp")
UCs <- UCs[UCs$esfera == c("Federal", "Estadual"), ]
UCs <- UCs[UCs$categoria != "Reserva Particular do Patrimônio Natural",]
UCs <- UCs[is.na(UCs$marinho) | UCs$marinho == "", ]
BR <- terra::vect("Data/BR_UF_2024/BR_UF_2024.shp")

# Plot 1: Observed vs Asymptotic (all sites)

paired_data_asymptotic <- riqueza_UCs %>%
  filter(!is.na(Riqueza_Assimptotica)) %>%
  dplyr::select(UC, Riqueza_Observada, Riqueza_Assimptotica, Pontos_Totais) %>%
  pivot_longer(cols = c(Riqueza_Observada, Riqueza_Assimptotica),
               names_to = "Type", 
               values_to = "Richness") %>%
  mutate(Type = case_when(
    Type == "Riqueza_Observada" ~ "Observed",
    Type == "Riqueza_Assimptotica" ~ "Asymptotic"
  ))

gp_asymptotic <- ggpaired(paired_data_asymptotic,
                          x = "Type",
                          y = "Richness", 
                          id = "UC",
                          color = "Type",
                          palette = c("#00AFBB", "#E7B800"),
                          line.color = "gray60",
                          line.size = 0.3,
                          point.size = 1.5,
                          title = "Observed vs Asymptotic Richness",
                          xlab = "Richness Type",
                          ylab = "Species"
) +
  scale_y_log10() +
  theme_bw() +
  labs(color = NULL)

print(gp_asymptotic)

ggsave("Results/observed_vs_asymptotic.png", gp_asymptotic, width = 8, height = 6, dpi = 300)


# Plot 2: 500 points

paired_data500_extrapolation <- riqueza_UCs %>%
  filter(Pontos_Totais < 500, !is.na(Riqueza_500_pontos)) %>%
  dplyr::select(UC, Riqueza_Observada, Riqueza_500_pontos, Pontos_Totais) %>%
  pivot_longer(cols = c(Riqueza_Observada, Riqueza_500_pontos),
               names_to = "Type", 
               values_to = "Richness") %>%
  mutate(Type = case_when(
    Type == "Riqueza_Observada" ~ "Observed",
    Type == "Riqueza_500_pontos" ~ "Extrapolated to 500"
  ))


gp500_extrap <- ggpaired(paired_data500_extrapolation,
                         x = "Type",
                         y = "Richness", 
                         id = "UC",
                         color = "Type",
                         palette = c("#00AFBB", "#FC4E07"),
                         line.color = "gray60",
                         line.size = 0.3,
                         point.size = 1.5,
                         title = "Observed Richness vs Extrapolation to 500 Points",
                         xlab = "Sample Status",
                         ylab = "Species"
) +
  scale_y_log10() +
  theme_bw() +
  labs(color = NULL)

gp500_extrap
ggsave("Results/extrapolation_500pts.png", gp500_extrap, width = 8, height = 6, dpi = 300)

# Plot 3: 100 points

paired_data100_extrapolation <- riqueza_UCs %>%
  filter(Pontos_Totais < 100, !is.na(Riqueza_100_pontos)) %>%
  dplyr::select(UC, Riqueza_Observada, Riqueza_100_pontos, Pontos_Totais) %>%
  pivot_longer(cols = c(Riqueza_Observada, Riqueza_100_pontos),
               names_to = "Type", 
               values_to = "Richness") %>%
  mutate(Type = case_when(
    Type == "Riqueza_Observada" ~ "Observed",
    Type == "Riqueza_100_pontos" ~ "Extrapolated to 100"
  ))


gp100_extrap <- ggpaired(paired_data100_extrapolation,
                         x = "Type",
                         y = "Richness", 
                         id = "UC",
                         color = "Type",
                         palette = c("#00AFBB", "#FC4E07"),
                         line.color = "gray60",
                         line.size = 0.3,
                         point.size = 1.5,
                         title = "Observed Richness vs Extrapolation to 100 Points ",
                         xlab = "Sample Status",
                         ylab = "Species"
) +
  scale_y_log10() +
  theme_bw() +
  labs(color = NULL)

print(gp100_extrap)
ggsave("Results/extrapolation_100pts.png", gp100_extrap, width = 8, height = 6, dpi = 300)

# Plot 4: 5000 points

paired_data5000_extrapolation <- riqueza_UCs %>%
  filter(Pontos_Totais < 5000, !is.na(Riqueza_5000_pontos)) %>%
  dplyr::select(UC, Riqueza_Observada, Riqueza_5000_pontos, Pontos_Totais) %>%
  pivot_longer(cols = c(Riqueza_Observada, Riqueza_5000_pontos),
               names_to = "Type", 
               values_to = "Richness") %>%
  mutate(Type = case_when(
    Type == "Riqueza_Observada" ~ "Observed",
    Type == "Riqueza_5000_pontos" ~ "Extrapolated to 5000"
  ))


gp5000_extrap <- ggpaired(paired_data5000_extrapolation,
                          x = "Type",
                          y = "Richness", 
                          id = "UC",
                          color = "Type",
                          palette = c("#00AFBB", "#FC4E07"),
                          line.color = "gray60",
                          line.size = 0.3,
                          point.size = 1.5,
                          title = "Observed Richness vs Extrapolation to 5000 Points",
                          xlab = "Sample Status",
                          ylab = "Species"
) +
  scale_y_log10() +
  theme_bw() +
  labs(color = NULL)

print(gp5000_extrap)
ggsave("Results/extrapolation_5000pts.png", gp5000_extrap, width = 8, height = 6, dpi = 300)  

# Plot 5: 1000 points

paired_data1000_extrapolation <- riqueza_UCs %>%
  filter(Pontos_Totais < 1000, !is.na(Riqueza_1000_pontos)) %>%
  dplyr::select(UC, Riqueza_Observada, Riqueza_1000_pontos, Pontos_Totais) %>%
  pivot_longer(cols = c(Riqueza_Observada, Riqueza_1000_pontos),
               names_to = "Type", 
               values_to = "Richness") %>%
  mutate(Type = case_when(
    Type == "Riqueza_Observada" ~ "Observed",
    Type == "Riqueza_1000_pontos" ~ "Extrapolated to 1000"
  ))


gp1000_extrap <- ggpaired(paired_data1000_extrapolation,
                          x = "Type",
                          y = "Richness", 
                          id = "UC",
                          color = "Type",
                          palette = c("#00AFBB", "#FC4E07"),
                          line.color = "gray60",
                          line.size = 0.3,
                          point.size = 1.5,
                          title = "Observed Richness vs Extrapolation to 1000 Points ",
                          xlab = "Sample Status",
                          ylab = "Species"
) +
  scale_y_log10() +
  theme_bw() +
  labs(color = NULL)

print(gp1000_extrap)
ggsave("Results/extrapolation_1000pts.png", gp1000_extrap, width = 8, height = 6, dpi = 300)

# Plot 6: 30 points 

paired_data30 <- riqueza_UCs %>%
  filter(Pontos_Totais < 30, !is.na(Riqueza_30_pontos)) %>%
  dplyr::select(UC, Riqueza_Observada, Riqueza_30_pontos, Pontos_Totais) %>%
  pivot_longer(cols = c(Riqueza_Observada, Riqueza_30_pontos),
               names_to = "Type", 
               values_to = "Richness") %>%
  mutate(Type = case_when(
    Type == "Riqueza_Observada" ~ "Observed",
    Type == "Riqueza_30_pontos" ~ "iNEXT 30"
  ))


gp30 <- ggpaired(paired_data30,
                        x = "Type",
                        y = "Richness", 
                        id = "UC",
                        color = "Type",
                        palette = c("#00AFBB", "#FC4E07"),
                        line.color = "gray60",
                        line.size = 0.3,
                        point.size = 1.5,
                        title = "Observed Richness vs iNEXT to 30 Points ",
                        xlab = "Sample Status",
                        ylab = "Species"
) +
  scale_y_log10() +
  theme_bw() +
  labs(color = NULL)

print(gp30)
ggsave("Results/iNEXT30pts.png", gp30, width = 8, height = 6, dpi = 300)  

# Plot 7: 50 points 

paired_data50 <- riqueza_UCs %>%
  filter(Pontos_Totais < 50, !is.na(Riqueza_50_pontos)) %>%
  dplyr::select(UC, Riqueza_Observada, Riqueza_50_pontos, Pontos_Totais) %>%
  pivot_longer(cols = c(Riqueza_Observada, Riqueza_50_pontos),
               names_to = "Type", 
               values_to = "Richness") %>%
  mutate(Type = case_when(
    Type == "Riqueza_Observada" ~ "Observed",
    Type == "Riqueza_50_pontos" ~ "iNEXT to 50"
  ))


gp50 <- ggpaired(paired_data50,
                        x = "Type",
                        y = "Richness", 
                        id = "UC",
                        color = "Type",
                        palette = c("#00AFBB", "#FC4E07"),
                        line.color = "gray60",
                        line.size = 0.3,
                        point.size = 1.5,
                        title = "Observed Richness vs iNEXTto 50 Points ",
                        xlab = "Sample Status",
                        ylab = "Species"
) +
  scale_y_log10() +
  theme_bw() +
  labs(color = NULL)

print(gp50)
ggsave("Results/iNEXT_50pts.png", gp50, width = 8, height = 6, dpi = 300)  


# MAPPING RICHNESS

terra::rast(system.file("ex/elev.tif", package="terra"))

# Shapefiles

UCs_sf <- st_as_sf(UCs)
UCs_data <- cbind(riqueza_UCs, UCs_sf)
BR_sf <- st_as_sf(BR)
UCs_data <- st_as_sf(UCs_data)

# Organized Data

UCs_caat <- filter(UCs_data, caatinga != "NA")
UCs_ama <- filter(UCs_data, amazonia != "NA")
UCs_ma <- filter(UCs_data, matlantica != "NA")
UCs_cer <- filter(UCs_data, cerrado != "NA")
UCs_pampa <- filter(UCs_data, pampa != "NA")
UCs_pant <- filter(UCs_data, pantanal != "NA")


ama <- UCs_ama[,c("UC", "Riqueza_Observada")]  
caat <- UCs_caat[,c("UC", "Riqueza_Observada")]  
cer <- UCs_cer[,c("UC", "Riqueza_Observada")]  
ma <- UCs_ma[,c("UC", "Riqueza_Observada")]  
pampa <- UCs_pampa[,c("UC", "Riqueza_Observada")]  
pant <- UCs_pant[,c("UC", "Riqueza_Observada")]  



combined_biomes <- bind_rows(
  ama %>% mutate(Biome = "Amazonia"),
  caat %>% mutate(Biome = "Caatinga"),
  cer %>% mutate(Biome = "Cerrado"),
  ma %>% mutate(Biome = "Mata Atlantica"),
  pampa %>% mutate(Biome = "Pampa"),
  pant %>% mutate(Biome = "Pantanal")
) %>%
  rename(Richness = Riqueza_Observada)


save(combined_biomes, file = "Data/combined_biomes.RData")

biome_totals <- combined_biomes %>%
  group_by(Biome) %>%
  summarise(Richness = sum(Richness, na.rm = TRUE),
            .groups = 'drop')

# Plot

p <- ggplot(biome_totals, aes(x = Biome,
                              y = Richness,
                              fill = Biome)) +
  geom_col(alpha = 0.8, color = "black") +
  scale_fill_viridis_d(name = "Biome",
                       option = "plasma") +
  labs(title = "Total Species Richness in UCs by Biome",
       x = "Biome", 
       y = "Species Richness") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")

p

# Plot

p2 <- ggplot(combined_biomes, 
             aes(x = Richness + 0.1, 
                 y = Biome, 
                 fill = Richness)) +
  geom_density_ridges_gradient(aes(fill = after_stat(x)),
                               rel_min_height = 0.01
  ) +
  scale_x_log10(breaks = c(1, 10, 100),
                labels = c("1", "10", "100")
  ) +
  scale_fill_viridis( 
    option = "plasma",
    trans = "log10",
    breaks = c(1, 10, 100),
    labels = c("1", "10", "100")) +
  labs(
    x = "Species Richness", 
    y = "Biome",
  ) +
  theme_ridges() +
  theme(
    legend.position="right",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )
p2

gr3 <- ggplot() +
  geom_sf(data = BR_sf, 
          color = "black", 
          fill = "white") +
  geom_sf(data = UCs_data, 
          color = 0, 
          fill = "lightblue", 
          size = 0.1) +
  geom_sf(data = UCs_data, 
          aes(fill = Riqueza_Observada), 
          color = 0,
          size = 0.1) +
  scale_fill_viridis_c(name = "Species\nRichness", 
                       option = "plasma",
                       trans = "log10",
                       breaks = c(1, 10, 100, 1000),
                       labels = c("1", "10", "100", "1000"),
                       na.value = "blue") +
  theme_void() 

gr3

ggsave("Results/Invasive_Species_Richness.tiff",gr3)



p2 <- p2 + 
  theme(
    plot.margin = margin(5, 5, 5, 5),        
    axis.title.x = element_text(size = 8, hjust = 0.5),    
    axis.title.y = element_text(size = 8, hjust = 0.2, vjust = 0),
    axis.text = element_text(size = 7),       
    legend.title = element_text(size = 8),    
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.4, "cm"),
    legend.position = "none"
  )
p2

combined_plot <- ggdraw() +
  draw_plot(gr3) +
  draw_plot(p2, 
            x = 0.60, y = 0,              
            width = 0.4, height = 0.35) +
  draw_plot_label(c("(a)", "(b)"), 
                  c(0.15, 0.60), c(0.95, 0.30),  
                  size = 12) +
  draw_label("Invasive Species Richness in Brazil by Conservations Units and Biome",
             x = 0.5, y = 0.99,           
             hjust = 0.5, vjust = 1,
             size = 12, fontface = "bold")
combined_plot
ggsave("Figures/combined_plot.png", combined_plot, 
       width = 8, height = 6, dpi = 300)
