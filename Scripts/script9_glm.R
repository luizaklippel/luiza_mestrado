#MODELING PCA AND FAMD AS PREDICTORS
library(visdat)
library(tidyverse)
library(lattice)
library(RVAideMemoire)
library(DHARMa)
library(performance)
library(MuMIn)
library(piecewiseSEM)
library(MASS)
library(ggExtra)
library(Rmisc)
library(emmeans) 
library(sjPlot)
library(bbmle)
library(glmmTMB)
library(ordinal)
library(car)
library(ecolottery)
library(naniar)
library(vcd)
library(generalhoslem)
library(broom.mixed)
library(dplyr)
library(ggplot2)
options(na.action = "na.fail") # Necessário para o dredge

# Model once with invas_obs when spp_obs was used in the PCA and another time with invas_est when spp_est was used in the PCA

df_glm$area <- as.numeric(df_glm$area)
df_glm$biome <- as.factor(df_glm$biome)
df_glm <- df_glm %>%
  mutate(gap = invas_est - invas_obs)
df_glm <- df_glm %>%
  mutate(buffer = buff_obs - invas_obs)
df_glm <- df_glm %>%
  dplyr::filter(complete.cases(dplyr::select(.,biome, gap,buffer, invas_obs, invas_est, Dim.1, Dim.2, Dim.3, PC1, PC2, PC3, PC4)))
model_nb <- MASS::glm.nb(buffer ~ Dim.1 * Dim.2 + PC1 * PC2 * PC3 * PC4, data = df_glm)

df_glm %>% dplyr::filter(buffer < 0) %>% dplyr::select(biome, buff_est, invas_est)
#####
####TEST FOR EACH BIOME SEPARETLY
df_amazonia<-filter(df_glm, biome=="Amazônia")
model_ama <- MASS::glm.nb(
  invas_est ~ Dim.1 * Dim.2 + PC1 + PC2 + PC3 + PC4,
  data = df_amazonia)

df_mata<-filter(df_glm, biome=="Mata Atlântica")
model_mata <- MASS::glm.nb(
  invas_est ~ Dim.1 * Dim.2 + PC1 + PC2 + PC3 + PC4,
  data = df_mata)

df_caat<-filter(df_glm, biome=="Caatinga")
model_caat <- MASS::glm.nb(
  invas_est ~ Dim.1 * Dim.2 + PC1 + PC2 + PC3 + PC4,
  data = df_caat)

df_cer<-filter(df_glm, biome=="Cerrado")
model_cer <- MASS::glm.nb(
  invas_est ~ Dim.1 * Dim.2 + PC1 + PC2 + PC3 + PC4,
  data = df_cer)

df_pant<-filter(df_glm, biome=="Pantanal")
model_pant <- MASS::glm.nb(
  invas_est ~ Dim.1 * Dim.2 + PC1 + PC2 + PC3 + PC4,
  data = df_pant)

df_pampa<-filter(df_glm, biome=="Pampa")
model_pampa <- MASS::glm.nb(
  invas_est ~ Dim.1 * Dim.2 + PC1 + PC2 + PC3 + PC4,
  data = df_pampa)
#####

par(mfrow = c(2, 2))
plot(model_nb) 
summary(model_nb)

## Diagnose avançada
simulationOutput <- simulateResiduals(fittedModel = model_nb, plot = TRUE)

# DISPERSION PARAMETER
## maior que 1.5 tem overdispersion
par(mfrow = c(1, 1))

(chat <- deviance(model_nb) / df.residual(model_nb)) 


## Coeficiente de determinação
rsquared(model_nb)


summary(model_nb)

#DREDGE

dredge_results <- dredge(model_nb, rank = "AIC")
head(dredge_results)
#Akaike weights
w <- Weights(dredge_results)
w
#Models in the 95% "confidence set"
length(cumsum(w)[cumsum(w)< 0.95]) + 1
#Refit best linear model
bestmodel <- get.models(dredge_results, 1)[[1]]

sw(dredge_results)
summary(dredge_results)
subset(dredge_results, delta <= 2, recalc.weights=FALSE)


summary(model.avg(dredge_results, delta <= 2))


plot(dredge_results, type="s")
plot(dredge_results)


# Extrair a importância das variáveis
var_imp <- sw(model.avg(dredge_results))

# Converter para data frame para facilitar o plot
df_imp <- data.frame(
  Variable = names(var_imp),
  Importance = as.numeric(var_imp)
)

# Ordenar por importância
df_imp <- df_imp[order(df_imp$Importance, decreasing = TRUE), ]



ggplot(df_imp, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
  coord_flip() + # Facilita a leitura dos nomes das variáveis/interações
  labs(
    x = "Predictors",
    y = "Relative Variable Importance (Sum of AICc weights)",
    title = "Variable Importance across Model Set"
  ) +
  theme_minimal() +
  ylim(0, 1) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red")  


# Plot GLM results

coefs_obs<- tidy(model_nb, conf.int = TRUE) %>%
  dplyr::filter(term != "(Intercept)") %>%         
  dplyr::mutate(
    term = dplyr::recode(term,
                  "Dim.1"        = "Dim1 (administrative attention axis)",
                  "Dim.2"        = "Dim2 (protection reinforcement axis)",
                  "PC1"          = "PC1 (climate axis)",
                  "PC2"          = "PC2 (habitat structure axis)",
                  "PC3"          = "PC3 (humidity axis)",
                  "PC4"          = "PC4 (anthropic pressure axis)",
                  "Dim.1:Dim.2"  = "Dim1 × Dim2"
    ),
    signif = conf.low > 0 | conf.high < 0     # TRUE = IC não cruza zero
  )

ggplot(coefs_obs, aes(x = estimate, y = reorder(term, estimate), color = signif)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), width = 0.15, linewidth = 0.7) +
  geom_point(size = 3) +
  scale_color_manual(  values = c("TRUE" = "#1b6ca8", "FALSE" = "grey60"),
                       labels = c("TRUE" = "Significative",
                                  "FALSE" = "Non-Significative"),
                       name = NULL) +
  labs(
    x = "Estimate (log-scale)",
    y = NULL,
    title = "GLM negative binomial model coefficients",
    subtitle = "invas_obs ~ Dim.1 * Dim.2 + PC1 * PC2 * PC3 *PC4"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

 ggsave("Figures/invas_obs_model_nb.png", width = 7, height = 5, dpi = 600)

 