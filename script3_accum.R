library(vegan)
library(ggplot2)
terra::rast(system.file("ex/elev.tif", package="terra"))
library(dplyr)



# RAREFACTION CURVE
library(forcats)
library(iNEXT)
library(vegan)
library(tidyverse)


site_totals <- rowSums(presab)
presab_final <- presab[site_totals > 0, ]

individual_plots <- list()

area_names <- rownames(presab_final)
area_names[grepl("[^A-Za-z0-9_-]", area_names)]

# Loop iNEXT for each area
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


