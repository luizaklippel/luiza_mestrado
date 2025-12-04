# PACKAGES
library(tidyverse)
library(iNEXT)
library(vegan)
library(ggplot2)
terra::rast(system.file("ex/elev.tif", package="terra"))
library(dplyr)
library(tidyr)
library(ggpubr)
library(ggridges)
library(viridis)
library(sf)
library(cowplot)


# LOAD

mydata <- read_csv("Data/presab.csv")

mydata
community <- as.matrix(mydata[, -c(1,2)])
rownames(community)<-mydata$sample.unit

rem <- rowSums(community) < 10

community <- t(community[!rem, ])

# iNEXT
out <- iNEXT(community, q = 0,
             datatype = "abundance",
             )

ucs <- unique(out$iNextEst$size_based[, 1])
n <- length(ucs)
b <- numeric(n)
for (i in 1:n) {
  out_i <- out$iNextEst$coverage_based %>% 
    filter(Assemblage == ucs[i])
  mydata_i <- out_i %>% 
    filter(Method  == "Observed") %>% 
    bind_rows(out_i[nrow(out_i), ]) %>% 
    select(m, qD) 
  b[i] <- lm(qD ~ m, data = mydata_i)$coefficients[2]
}
b <- ifelse(is.na(b), 0, b) # SAMPLE INCOMPLETUDE ESTIMATE

save(out, file = "Data/out.RData")


# iNEXT FOR EACH AREA

# Prepare Data

site_totals <- rowSums(presab)
presab_final <- presab[site_totals > 0, ]

individual_plots <- list()

area_names <- rownames(presab_final)
area_names[grepl("[^A-Za-z0-9_-]", area_names)]

# Loop iNEXT 
for(i in 1:length(area_names)) {
  single_area_data <- presab_final[i, , drop = FALSE]
  out_single <- iNEXT(t(single_area_data), q = 0, 
                      datatype = "abundance")
  save(out_single, file = "Data/out_single.RData")
  # Plot
  individual_plots[[i]] <- ggiNEXT(out_single, type = 1) +
    theme_bw() +
    labs(title = UCs$nome_uc[i]) +
    xlab("Número de indivíduos") + 
    ylab("Riqueza de espécies") +
    theme(legend.position = "none")
  
  # Save
  clean_name <- gsub("[^A-Za-z0-9_-]", "_", UCs$nome_uc[i])
  filename <- paste0("Figures/", clean_name, "_rarefaction.png")
  ggsave(filename, individual_plots[[i]], 
         width = 8, height = 6, dpi = 300)
  print(paste("Created and saved:", filename))
}

# Name the plots
names(individual_plots) <- area_names
individual_plots[i]


# MAPPING RICHNESS

terra::rast(system.file("ex/elev.tif", package="terra"))

rich <- out$AsyEst
all_rich <- rich[rich$Diversity == "Species richness",]

results_table <- data.frame(
  UC = area_names,
  Pontos_Totais = rowSums(presab_final),
  Riqueza_Observada = rowSums(presab_final > 0),
  stringsAsFactors = FALSE
)

# Shapefiles

UCs_sf <- st_as_sf(UCs)
UCs_data <- cbind(results_table, UCs_sf)
BR_sf <- st_as_sf(br)
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

p <- ggplot(biome_totals, aes(x = biome,
                              y = estimated_richness,
                              fill = biome)) +
  geom_col(alpha = 0.8, color = "black") +
  scale_fill_viridis_d(name = "biome",
                       option = "plasma") +
  labs(title = "Total Invasive Species Richness in PAs by Biome",
       x = "Biome", 
       y = "Species Richness") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")

p

# Plot

p2 <- ggplot(combined_biomes, 
             aes(x = estimated_richness + 0.1, 
                 y = biome, 
                 fill = estimated_richness)) +
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
  geom_sf(data = all_rich1, 
          color = 0, 
          fill = "lightblue", 
          size = 0.1) +
  geom_sf(data = all_rich1, 
          aes(fill = Estimator), 
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
            width = 0.4, height = 0.30) +
  draw_plot_label(c("(a)", "(b)"), 
                  c(0.15, 0.60), c(0.95, 0.28),  
                  size = 12) +
  draw_label("Invasive Species Richness in Brazil by Conservations Units and Biome",
             x = 0.5, y = 0.99,           
             hjust = 0.5, vjust = 1,
             size = 12, fontface = "bold")
combined_plot
ggsave("Figures/combined_plot.png", combined_plot, 
       width = 8, height = 6, dpi = 300)
