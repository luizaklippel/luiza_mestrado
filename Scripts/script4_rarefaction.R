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

rem <- rowSums(community) < 1

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




