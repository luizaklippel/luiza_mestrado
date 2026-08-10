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
options(na.action = "na.fail") # Necessário para o dredge

df_glm <- df_glm %>%
  dplyr::filter(complete.cases(dplyr::select(., invas_est, gov_PCoA1, gov_PCoA2, PC1, PC2, PC3, area)))
model_nb <- MASS::glm.nb(invas_est ~ gov_PCoA1 * gov_PCoA2 + PC1 * PC2 * PC3 + offset(log(area)), data = df_glm)

par(mfrow = c(2, 2))
plot(model_nb) 
summary(model_nb)

## Diagnose avançada
simulationOutput <- simulateResiduals(fittedModel = model_nb, plot = TRUE)

# DISPERSION PARAMETER
par(mfrow = c(1, 1))

(chat <- deviance(model_nb) / df.residual(model_nb)) 
#[1] 1.196551

## Coeficiente de determinação
rsquared(model_nb)
#  Response            family link     method R.squared
#1   invas_est Negative Binomial  log nagelkerke  0.230008

summary(model_nb)

#DREDGE

dredge_results <- dredge(model_nb)
sw(dredge_results)
summary(dredge_results)
subset(dredge_results, delta <= 2, recalc.weights=FALSE)

get.models(dredge_results, subset = 1)[[1]]%>%summary()

summary(model.avg(dredge_results))


plot(dredge_results, type="s")
plot(dredge_results)


summary(dredge_results@objects[[1]])

model.avg(dredge_results)%>%summary()


# Extrair a importância das variáveis
var_imp <- sw(model.avg(dredge_results))

# Converter para data frame para facilitar o plot
df_imp <- data.frame(
  Variable = names(var_imp),
  Importance = as.numeric(var_imp)
)

# Ordenar por importância
df_imp <- df_imp[order(df_imp$Importance, decreasing = TRUE), ]


library(ggplot2)

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
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") # Threshold 
