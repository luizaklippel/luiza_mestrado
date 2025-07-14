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


