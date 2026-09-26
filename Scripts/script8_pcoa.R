# CREATE THE AXES FOR MODEL PREDICTORS
library(readxl)
library(vegan)
library(dplyr)
library(cluster)
library(ape)
library(ggplot2)
library(ggrepel)
library(corrplot)
library(reshape2)
library(ade4)
library(plotly)
library(FactoMineR)
library(sf)
library(factoextra)
library(ggcorrplot)
library(factoextra)
library(patchwork)
library(readr)
library(tibble)
library(reshape2)
rm(list=ls())

#Data

load("Data/vari.RData")

UCs <-  terra::vect("Data/shp_cnuc_2024_02/cnuc_2024_02.shp")
UCs <- UCs[UCs$esfera == c("Federal", "Estadual"), ]
UCs <- UCs[UCs$categoria != "Reserva Particular do Patrimônio Natural",]
UCs <- UCs[is.na(UCs$marinho) | UCs$marinho == "", ]
UCs_df <- as.data.frame(UCs)
ucs_filt <- UCs_df %>%
  dplyr::select( uc_id,grupo,co_gestor, cat_iucn)

vari <- vari %>%
  left_join(ucs_filt %>% st_drop_geometry(), by = "uc_id")

cnuc <- read_delim("Data/cnuc_2026_07.csv", 
                   delim = ";", escape_double = FALSE,
                   locale = locale(encoding = "ISO-8859-1"), 
                   trim_ws = TRUE)
vari <- vari %>%
  dplyr::select(-mp) %>%
  left_join(
    cnuc %>% dplyr::select( cd_cnuc =`Código UC` , mp = "Plano de Manejo"),
    by = "cd_cnuc"
  )%>%
  dplyr::select(-co_gestor) %>%
  left_join(
    cnuc %>% dplyr::select( cd_cnuc =`Código UC` , co_gestor = "Conselho Gestor"),
    by = "cd_cnuc"
  )%>%
  dplyr::select(-biome) %>%
  left_join(
    cnuc %>% dplyr::select( cd_cnuc =`Código UC` , biome = "Bioma declarado"),
    by = "cd_cnuc"
  )

## GOVERNANCE

gove <- vari%>%
# ------------------ INICIO DAS ALTEARAÇÕES
column_to_rownames("nome_uc")%>%                       #add rownames to the dataframe
 dplyr::select(invas_est,mp,adm,min_dist_pa,year,grupo,co_gestor)%>%
  dplyr::select(-ends_with("s.e."))%>%
  dplyr::mutate(across(where(is.character), as.factor))%>%
  dplyr::mutate(across(where(is.numeric), as.numeric))%>%
  dplyr::mutate(across(where(is.numeric), scale))%>%     # Scalonei as variáveis numéricas 
  dplyr::mutate(across(where(is.numeric), as.numeric))


#TODO: VEJA A POSSIBILIDADE DE FAZER scale(log10(distancia de outras UCs))) para evitar medidas muito discrepantes.


gove <- gove%>%filter(complete.cases(.)) # Removi todo os NAs para fazer a análise de PCoA e FAMD.

gove_trait <- gove[,2:7]


# FAMD

res_famd <- FAMD(gove_trait, graph = FALSE)

eig_val <- get_eigenvalue(res_famd)
head(eig_val)

fviz_screeplot(res_famd, addlabels = TRUE, ylim = c(0, 50),
               barfill = "#3a86d4", barcolor = "#3a86d4")
fviz_eig(res_famd, addlabels = TRUE)

coords_famd <- res_famd$ind$coord[, 1:3]
var <- get_famd_var(res_famd)

# Contribution (%) of each variable to the first three dimensions
round(var$contrib[, 1:3], 3)

fviz_famd_var(res_famd, repel = TRUE,
              ggtheme = theme_minimal())

par(mfrow = c(1, 3))
# Contribution to dimension 1
p1 <- fviz_contrib(res_famd, "var", axes = 1,
             fill = "#3a86d4", color = "#3a86d4")
# Contribution to dimension 2
p2 <- fviz_contrib(res_famd, "var", axes = 2,
             fill = "#3a86d4", color = "#3a86d4")
# Contribution to dimension 3
p3 <- fviz_contrib(res_famd, "var", axes = 3,
             fill = "#3a86d4", color = "#3a86d4")

p1 + p2+ p3 + plot_layout(ncol = 3)

# Pull out only numeric variables
quanti_var <- get_famd_var(res_famd, "quanti.var")

# Coordinates on the correlation circle
round(quanti_var$coord[, 1:3], 3)

fviz_famd_var(res_famd, "quanti.var", repel = TRUE,
              col.var = "black")

fviz_famd_var(res_famd, "quanti.var", col.var = "contrib",
              gradient.cols = c("#dbe9f6", "#3a86d4", "#08306b"),
              repel = TRUE)

# Pull out only qualitative variables
quali_var <- get_famd_var(res_famd, "quali.var")

# Coordinates of the categories
round(quali_var$coord[, 1:3], 3)

fviz_famd_var(res_famd, "quali.var", col.var = "contrib",
              gradient.cols = c("#dbe9f6", "#3a86d4", "#08306b"))

## Graph of individuals
fviz_famd_ind(res_famd, col.ind = "cos2",
              gradient.cols = c("#dbe9f6", "#3a86d4", "#08306b"),
              repel = TRUE)

# Colour individuals by group
fviz_famd_ind(res_famd,
              habillage = "adm", select.ind = list(name = c(
                "FLORESTA NACIONAL DE CARAJÁS",
                "PARQUE NACIONAL DE ILHA GRANDE",
                "ÁREA DE PROTEÇÃO AMBIENTAL CACHOEIRA DAS ANDORINHAS",
                "ÁREA DE PROTEÇÃO AMBIENTAL DE GERICINÓ/MENDANHA",
                "ÁREA DE PROTEÇÃO AMBIENTAL DA SERRA DE SAPIATIBA",
                "PARQUE NACIONAL DA TIJUCA","RESERVA BIOLÓGICA DE SOORETAMA")),          # colour by groups
              palette = "jco",              # colourblind-safe journal palette
              addEllipses = TRUE, ellipse.type = "confidence",
              repel = TRUE)

fviz_ellipses(res_famd, c("grupo", "adm"), repel = TRUE)

res_famd%>%plotellipses()

plot(res_famd,choix="var")

par(mfrow = c(1, 3))
p12 <- plot(res_famd, choix = "var", axes = c(1,2))
p13 <-plot(res_famd, choix = "var", axes = c(1,3))
p23 <- plot(res_famd, choix = "var", axes = c(2,3))

p12 + p13 + p23 + plot_layout(ncol = 3)

par(mfrow = c(1, 3))
res_famd %>% plotellipses(axes = c(1,2))
res_famd %>% plotellipses(axes = c(1,3))
res_famd %>% plotellipses(axes = c(2,3))

png("axes_12.png", width = 12, height = 3.5, units = "in", res = 600)
res_famd %>% plotellipses(axes = c(1,2))
dev.off()

png("axes_13.png", width = 12, height = 3.5, units = "in", res = 600)
res_famd %>% plotellipses(axes = c(1,3))
dev.off()

png("axes_23.png", width = 12, height = 3.5, units = "in", res = 600)
res_famd %>% plotellipses(axes = c(2,3))
dev.off()

library(magick)
img1 <- image_read("axes_12.png")
img2 <- image_read("axes_13.png")
img3 <- image_read("axes_23.png")

final <- image_append(c(img1, img2, img3), stack = TRUE)
image_write(final, "Figures/famd_axes.png", density = 600)



## ENVIRONMENT
# Do the PCA once with spp_est and another time with spp_obs
dados <- vari %>%column_to_rownames("nome_uc")%>%
  mutate(across(where(is.character), as.factor))%>%                       
  dplyr::select(biome, spp_est, altitude, mean_temp, humidity, coverage, water_bodies, urb_dist, area)%>%
  dplyr::select(-ends_with("s.e."))%>%
  dplyr::mutate(across(where(is.character), as.factor))%>%
  dplyr::mutate(across(where(is.numeric), as.numeric))%>%
  dplyr::mutate(across(where(is.numeric), scale))%>%     # Scalonei as variáveis numéricas 
  dplyr::mutate(across(where(is.numeric), as.numeric))%>%
  dplyr::mutate(biome = as.factor(biome))
dados$area <- as.numeric(dados$area)

df_env <- dados%>%filter(complete.cases(.))
## Verify NAs
sum(is.na(df_env))
#> [1] 165

## Remove NA
env <- na.omit(df_env)

## Keep only continuous variables for PCA
env_trait <-df_env[, 2:9]

## Compare com este código a variância das variáveis
env_trait %>% 
  dplyr::summarise(across(where(is.numeric), 
                          ~var(.x, na.rm = TRUE)))

## Agora, veja o mesmo cálculo se fizer a padronização (scale.unit da função PCA)
env_pad <- decostand(x = env_trait, method = "standardize")
env_pad %>% 
  dplyr::summarise(across(where(is.numeric), 
                          ~var(.x, na.rm = TRUE)))
## PCA
pca_env <- PCA(X = env_trait, scale.unit = TRUE, graph = FALSE)

## Autovalores: porcentagem de explicação para usar no gráfico
pca_env$eig 



fviz_eig(pca_env,addlabels = TRUE)
pca_env$eig[, 1]  # eigenvalues — reter onde > 1
## Visualização da porcentagem de explicação de cada eixo
# nota: é necessário ficar atento ao valor máximo do eixo 1 da análise para determinar o valor do ylim (neste caso, colocamos que o eixo varia de 0 a 70).
fviz_screeplot(pca_env, addlabels = TRUE, ylim = c(0, 70), main = "", 
               xlab = "Dimensões",
               ylab = "Porcentagem de variância explicada") 
## Outros valores importantes
var_env <- get_pca_var(pca_env)

## Escores (posição) das variáveis em cada eixo
var_env$coord 

## Contribuição (%) das variáveis para cada eixo
var_env$contrib 


## Loadings - correlação das variáveis com os eixos
var_env$cor 


## Qualidade da representação da variável. Esse valor é obtido multiplicado var_env$coord por var_env$coord
var_env$cos2

## Escores (posição) das localidades ("site scores") em cada eixo 
ind_env <- get_pca_ind(pca_env)

## Variáveis mais importantes para o Eixo 1
dimdesc(pca_env)$Dim.1 



## Variáveis mais importantes para o Eixo 2
dimdesc(pca_env)$Dim.2 


## Variáveis mais importantes para o Eixo 3
dimdesc(pca_env)$Dim.3 

## Variáveis mais importantes para o Eixo 4
dimdesc(pca_env)$Dim.4
dimdesc(pca_env, axes = 4, proba = 0.05)

#Plot
ids_usados <- rownames(pca_env$ind$coord)
biome_alinhado <- df_env[ids_usados, "biome"]

make_biplot <- function(axes) {
  fviz_pca_biplot(pca_env,
                  axes = axes,
                  habillage = biome_alinhado,
                  addEllipses = FALSE,
                  label = "var",
                  col.var = "black",
                  repel = TRUE,
                  pointsize = 1.5,
                  arrowsize = 0.6,
                  labelsize = 3,
                  geom = "point",
                  pointshape = 19) +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_color_discrete(name = "Biome") +
    ggtitle(NULL)   
}

p12 <- make_biplot(c(1,2)); p13 <- make_biplot(c(1,3)); p23 <- make_biplot(c(2,3))
p14 <- make_biplot(c(1,4)); p24 <- make_biplot(c(2,4)); p34 <- make_biplot(c(3,4))

final_plot <- (p12 | p13 | p23) / (p14 | p24 | p34) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "right")

final_plot + plot_annotation(
  title = "PCA - Environmental variables",
  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
)
##CORRELAÇÃO VARIÁVEIS
df_env <- dados %>%
  dplyr::select(biome, area, spp_obs, altitude, mean_temp, humidity, coverage, water_bodies,urb_dist) %>%
  dplyr::mutate(biome = as.factor(biome))
env_trait <-df_env[, 2:8]

cor_env <- cor(env_trait, use = "pairwise.complete.obs")

ggcorrplot(cor_env, lab = TRUE, lab_size = 3, hc.order = TRUE,
           type = "lower",
           outline.color = "white")



var_cor_df <- melt(pca_env$var$cor)
colnames(var_cor_df) <- c("variable", "axis", "correlation")

ggplot(var_cor_df, aes(axis, variable, fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(correlation, 2)), size = 3) +
  scale_fill_gradient2(
    low = "orange4",      
    mid = "white",        
    high = "orange",     
    midpoint = 0,
    limits = c(-1, 1),
    name = "correlation"
  ) +
  theme_minimal()

## DATAFRAME FOR GLM
coords <- as.data.frame(pca_env$ind$coord[, 1:4])
colnames(coords) <- c("PC1", "PC2", "PC3", "PC4")
coords_famd <- as.data.frame(res_famd$ind$coord[, 1:3])
coord_env <- coords
coord_env <- coord_env %>% rownames_to_column("nome_uc")
coords_famd <- coords_famd %>% rownames_to_column("nome_uc")

df_glm <- left_join(vari, coords_famd, by = "nome_uc")
df_glm <- left_join(df_glm, coord_env, by = "nome_uc")
