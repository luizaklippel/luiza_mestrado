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

## GOVERNANCE

gove <- vari%>%
 dplyr::select(invas_est,mp,adm,min_dist_pa,year,grupo,co_gestor)%>%
  dplyr::select(-ends_with("s.e."))%>%
  dplyr::mutate(across(where(is.character), as.factor))

gove <- gove[!is.na(gove$invas_est),]

gove_trait <- gove[,2:6]

# Gower distance
gower_gov <- daisy(gove_trait, metric = "gower")

#PCoA

pcoa_gov <- pcoa(gower_gov)

## Porcentagem de explicação do Eixo 1
100 * (pcoa_gov$values[, 1]/pcoa_gov$trace)[1]
#> [1] 49.79715

## Porcentagem de explicação dos Eixo 2
100 * (pcoa_gov$values[, 1]/pcoa_gov$trace)[2]
#> [1] 36.52721

## Porcentagem de explicação acumulada dos dois primeiros eixos 
sum(100 * (pcoa_gov$values[, 1]/pcoa_gov$trace)[1:2])
#> [1] 86.32436

## Selecionar os dois primeiros eixos
eixos <- pcoa_gov$vectors[, 1:2]

## Juntar com algum dado categórico de interesse para fazer a figura
# Dataframe para plotar
pcoa_gov.dat <- data.frame( eixos, mp = gove$mp, adm = gove$adm, grupo = gove$grupo)

ggplot(pcoa_gov.dat, aes(x = Axis.1, y = Axis.2, fill = grupo, 
                     color = grupo, shape = mp)) +
  geom_point(size = 4, alpha = 0.7) + 
  labs(x = paste0("PCoA 1 (", round(pcoa_gov$values$Relative_eig[1]*100, 2), "%)"),
       y = paste0("PCoA 2 (", round(pcoa_gov$values$Relative_eig[2]*100, 2), "%)")) + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2)
colnames(pcoa_gov.dat) <- c("Axis.1", "Axis.2")


# Ajustar variáveis (quanti + quali) aos eixos do PCoA
ef_gov <- envfit(pcoa_gov$vectors, gove, permutations = 999, na.rm = TRUE)
ef_gov  # ver R² e p-valor de cada variável



## Check missing values 
vars_usadas <- c("min_dist_pa", "urb_dist", "year", "invas_obs", "invas_est")
gove$linha_removida <- !complete.cases(gove[, vars_usadas])

table(gove$linha_removida)
table(gove$mp, gove$linha_removida)
table(gove$co_gestor, gove$linha_removida)
table(gove$adm, gove$linha_removida)

gove %>%
  dplyr::group_by(category) %>%
  dplyr::summarise(dplyr::across(dplyr::all_of(vars_usadas), ~sum(is.na(.))))
#> A tibble: 10 × 6
#>category                            min_dist_pa urb_dist  year invas_obs invas_est
#><fct>                                     <int>    <int> <int>     <int>     <int>
#>1 Área de Proteção Ambiental                    0        3    16         0         0
#>2 Área de Relevante Interesse Ecológ…           0        1     4         0         0
#>3 Estação Ecológica                             0        1     7         0         0
#>4 Floresta                                      0        3     7         0         0
#>5 Monumento Natural                             0        1     1         0         0
#>6 Parque                                        0        8    19         0         0
#>7 Refúgio de Vida Silvestre                     0        0     5         0         0
#>8 Reserva Biológica                             0        1     3         0         0
#>9 Reserva de Desenvolvimento Sustent…           0        1     1         0         0
#>10 Reserva Extrativista                          0        0     3         0         0

# Chi-square
chisq.test(table(gove$co_gestor, gove$linha_removida))


## ENVIRONMENT

dados <- vari%>%
  dplyr::select(-c(cd_cnuc,uc_id,polyg))%>%
  dplyr::select(-ends_with("s.e."))
dados$area <- as.numeric(dados$area)
dados <- dados %>%
  mutate(across(where(is.character), as.factor))

dados <- dados[!is.na(dados$invas_obs), ]

# Do the PCA once with spp_est and another time with spp_obs
df_env <- dados %>%
  dplyr::select(biome, spp_est, altitude, mean_temp, humidity, coverage, water_bodies, urb_dist) %>%
  dplyr::mutate(biome = as.factor(biome))


## Verify NAs
sum(is.na(df_env))
#> [1] 165

## Remove NA
env <- na.omit(df_env)

## Keep only continuous variables for PCA
env_trait <-df_env[, 2:8]

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



# Three dimensions plot

coords <- as.data.frame(pca_env$ind$coord[, 1:3])
colnames(coords) <- c("PC1", "PC2", "PC3")

plot_ly(coords, x = ~PC1 , y = ~PC2, z = ~PC3,
        type = "scatter3d", mode = "markers",
        color = df_env$biome) %>%
  layout(
    scene = list(
      xaxis = list(title = "PC1 (32.73%)"),
      yaxis = list(title = "PC2 (19.64%)"),
      zaxis = list(title = "PC3 (18.61%)")
    ) )


##CORRELAÇÃO VARIÁVEIS
df_env <- dados %>%
  dplyr::select(biome, area, spp_est, altitude, mean_temp, humidity, coverage, water_bodies,urb_dist) %>%
  dplyr::mutate(biome = as.factor(biome))
env_trait <-df_env[, 2:8]

cor_env <- cor(env_trait, use = "pairwise.complete.obs")

ggcorrplot(cor_env, lab = TRUE, lab_size = 3, hc.order = TRUE,
           type = "lower",
           outline.color = "white")



## DATAFRAME FOR GLM
coord_gov <- as.data.frame(pcoa_gov$vectors[, 1:2])
colnames(coord_gov) <- paste0("gov_PCoA", 1:2)
coord_gov <- na.omit(coord_gov)

coord_env <- coords

df_glm <- dados %>%
  bind_cols(coord_gov, coord_env)


