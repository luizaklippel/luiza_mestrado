mydata <- read_csv("Data/presab.csv")
mydata<-mydata%>%as.data.frame()

mydata%>%class()
rownames(mydata)<-mydata$...1
community <- mydata[, -c(1,2)]

mydata
rem <- rowSums(community) < 10

community2 <- t(community[!rem, ])


sum(community2)

community3<-community2[,c(1:10)]

community2

# iNEXT
out2 <- iNEXT(community3, q = 0,
             datatype = "abundance",
)


out2

## install iNEXT package from CRAN
install.packages("remotes")
## install the latest version from github
install.packages('devtools')
library(remotes)
install_github('AnneChao/iNEXT')
## import packages
library(iNEXT)
library(ggplot2)

data(spider)
str(spider)
str(pam_inv)

# ESTIMATOR
UCs <-  terra::vect("Data/shp_cnuc_2024_02/cnuc_2024_02.shp")
UCs <- UCs[UCs$esfera == c("Federal", "Estadual"), ]
UCs <- UCs[UCs$categoria != "Reserva Particular do Patrimônio Natural",]
UCs <- UCs[is.na(UCs$marinho) | UCs$marinho == "", ]
BR <- terra::vect("Data/BR_UF_2024/BR_UF_2024.shp")

UCs_sf <- st_as_sf(UCs)
BR_sf <- st_as_sf(BR)

rich <- out$AsyEst
all_rich <- rich[rich$Diversity == "Species richness",]

all_rich1 <- cbind(all_rich, UCs_sf)
all_rich1 <- st_as_sf(all_rich1)


UCs_caat <- filter(all_rich1, caatinga != "NA")
UCs_ama <- filter(all_rich1, amazonia != "NA")
UCs_ma <- filter(all_rich1, matlantica != "NA")
UCs_cer <- filter(all_rich1, cerrado != "NA")
UCs_pampa <- filter(all_rich1, pampa != "NA")
UCs_pant <- filter(all_rich1, pantanal != "NA")

ama <- UCs_ama[,c("nome_uc", "Estimator","Observed")]  
caat <- UCs_caat[,c("nome_uc", "Estimator","Observed")]  
cer <- UCs_cer[,c("nome_uc", "Estimator","Observed")] 
ma <- UCs_ma[,c("nome_uc", "Estimator","Observed")]  
pampa <- UCs_pampa[,c("nome_uc", "Estimator","Observed")]  
pant <- UCs_pant[,c("nome_uc", "Estimator","Observed")] 

combined_biomes <- bind_rows(
  ama %>% mutate(Biome = "Amazonia"),
  caat %>% mutate(Biome = "Caatinga"),
  cer %>% mutate(Biome = "Cerrado"),
  ma %>% mutate(Biome = "Mata Atlantica"),
  pampa %>% mutate(Biome = "Pampa"),
  pant %>% mutate(Biome = "Pantanal")
) %>%
  rename(estimated_richness = Estimator)%>%
  rename(biome = Biome)%>%
  rename(pa_name = nome_uc)%>%
  rename(observed_richness = Observed)

biome_totals <- combined_biomes %>%
  group_by(biome) %>%
  summarise(estimated_richness = sum(estimated_richness, na.rm = TRUE),
              .groups = 'drop')

biome_totals1 <- combined_biomes %>%
  group_by(biome) %>%
  summarise(observed_richness = sum(observed_richness, na.rm = TRUE),
            .groups = 'drop')

biome_totals <- biome_totals %>%
  left_join(biome_totals1 %>% 
              st_drop_geometry()
            %>% select(biome, observed_richness), 
            by = "biome")

save(biome_totals, file = "Data/biome_total.RData")

# ESTIMATOR

est1<- ggplot(biome_totals, aes(x = biome,
                              y = estimated_richness,
                              fill = biome)) +
  geom_col(alpha = 0.8, color = "black") +
  scale_fill_viridis_d(name = "Biome",
                       option = "plasma") +
  labs(title = "Total Invasive Species Richness in PAs by Biome",
       x = "Biome", 
       y = "Species Richness") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")
est1


# Plot

est2 <- ggplot(combined_biomes, 
             aes(x = estimated_richness + 0.1, 
                 y = biome, 
                 fill = Species Richness)) +
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
est2

est3 <- ggplot() +
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

est3

ggsave("Results/Est_Invasive_Species_Richness.tiff",est3)



est2 <- est2 + 
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
est2

est_combined <- ggdraw() +
  draw_plot(est3) +
  draw_plot(est2, 
            x = 0.60, y = 0,              
            width = 0.4, height = 0.30) +
  draw_plot_label(c("(a)", "(b)"), 
                  c(0.15, 0.60), c(0.95, 0.28),  
                  size = 12) +
  draw_label("Invasive Species Richness in Brazil by Conservations Units and Biome",
             x = 0.5, y = 0.99,           
             hjust = 0.5, vjust = 1,
             size = 12, fontface = "bold")
est_combined
ggsave("Figures/est_combined.png", est_combined, 
       width = 8, height = 6, dpi = 300)

# OBSERVED

obs1 <- ggplot(biome_totals, aes(x = biome,
                              y = observed_richness,
                              fill = biome)) +
  geom_col(alpha = 0.8, color = "black") +
  scale_fill_viridis_d(name = "Biome",
                       option = "plasma") +
  labs(title = "Total Invasive Species Richness in PAs by Biome",
       x = "Biome", 
       y = "Species Richness") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")

obs1

# Plot

obs2 <- ggplot(combined_biomes, 
             aes(x = observed_richness + 0.1, 
                 y = biome, 
                 fill = Species Richness)) +
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
obs2

obs3 <- ggplot() +
  geom_sf(data = BR_sf, 
          color = "black", 
          fill = "white") +
  geom_sf(data = all_rich1, 
          color = 0, 
          fill = "lightblue", 
          size = 0.1) +
  geom_sf(data = all_rich1, 
          aes(fill = Observed), 
          color = 0,
          size = 0.1) +
  scale_fill_viridis_c(name = "Species\nRichness", 
                       option = "plasma",
                       trans = "log10",
                       breaks = c(1, 10, 100, 1000),
                       labels = c("1", "10", "100", "1000"),
                       na.value = "blue") +
  theme_void() 

obs3

ggsave("Results/Obs_Invasive_Species_Richness.tiff",obs3)



obs2 <- obs2 + 
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
obs2

obs_combined<- ggdraw() +
  draw_plot(obs3) +
  draw_plot(obs2, 
            x = 0.60, y = 0,              
            width = 0.4, height = 0.30) +
  draw_plot_label(c("(a)", "(b)"), 
                  c(0.15, 0.60), c(0.95, 0.28),  
                  size = 12) +
  draw_label("Invasive Species Richness in Brazil by Conservations Units and Biome",
             x = 0.5, y = 0.99,           
             hjust = 0.5, vjust = 1,
             size = 12, fontface = "bold")
obs_combined
ggsave("Figures/obs_combined.png", obs_combined, 
       width = 8, height = 6, dpi = 300)




