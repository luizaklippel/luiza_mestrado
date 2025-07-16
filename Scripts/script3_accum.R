library(vegan)
library(ggplot2)
terra::rast(system.file("ex/elev.tif", package="terra"))

#Species Accumulation

mat_uni1 <- mat_uni$PAM 
str(mat_uni1)
class(mat_uni1)
head(mat_uni1)
mat_uni1_clean <- mat_uni1[, sapply(mat_uni1, is.numeric)]
accumula <- specaccum(mat_uni1_clean)
plot(accumula)
save(accumula, file = "Data/accumula.RData")
save(mat_uni1_clean,file = "Data/mat_uni_clean.RData")

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

# Curva de Rarefação 
library(forcats)
library(iNEXT)
library(vegan)
library(tidyverse)


site_totals <- rowSums(mat_uni1_clean)
mat_uni1_final <- mat_uni1_clean[site_totals > 0, ]

individual_plots <- list()

area_names <- rownames(mat_uni1_final)

# Loop iNEXT for each area
for(i in 1:length(area_names)) {
  single_area_data <- mat_uni1_final[i, , drop = FALSE]
  out_single <- iNEXT(t(single_area_data), q = 0, 
                      datatype = "abundance")
  
  # Plot
  individual_plots[[i]] <- ggiNEXT(out_single, type = 1) +
    theme_bw() +
    labs(title = area_names[i]) +
    xlab("Número de indivíduos") + 
    ylab("Riqueza de espécies") +
    theme(legend.position = "none")
  
  # Save
  filename <- paste0("Figures/", area_names[i], "_rarefaction.png")
  ggsave(filename, individual_plots[[i]], width = 8, height = 6, dpi = 300)
  
  print(paste("Created and saved:", filename))
}

# Name the plots
names(individual_plots) <- area_names
individual_plots[1]

save(out_single, file = "Data/out_single.RData")

