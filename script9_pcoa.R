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


#Data

load("Data/vari.RData")

env <- vari%>%
  dplyr::select(-c(nome_uc,category,mp,fu,adm,area,cd_cnuc,uc_id,polyg, min_dist_pa,urb_dist, year))%>%
  dplyr::select(-ends_with("s.e."))%>%
 dplyr:: mutate(across(where(is.character), as.factor))

env <- env[!is.na(env$invas_obs),]

gove <- vari%>%
 dplyr::select(category,mp,adm,min_dist_pa,urb_dist,year, invas_obs,invas_est)%>%
  dplyr::select(-ends_with("s.e."))%>%
  dplyr::mutate(across(where(is.character), as.factor))
gove$area <- as.numeric(gove$area)

gove <- gove[!is.na(gove$invas_obs),]

dados <- vari%>%
  dplyr::select(-c(cd_cnuc,uc_id,polyg))%>%
  dplyr::select(-ends_with("s.e."))
dados$area <- as.numeric(dados$area)
dados <- dados %>%
  mutate(across(where(is.character), as.factor))

dados <- dados[!is.na(dados$invas_obs), ]

## Governance

gove_trait <- gove[,2:8]
# Gower distance
gower_gov <- daisy(gove_trait, metric = "gower")

#PCoA

pcoa_gov <- pcoa(gower_gov)

## Porcentagem de explicação do Eixo 1
100 * (pcoa_gov$values[, 1]/pcoa_gov$trace)[1]
#> [1] 59.33143

## Porcentagem de explicação dos Eixo 2
100 * (pcoa_gov$values[, 1]/pcoa_gov$trace)[2]
#> [[1] 35.52761

## Porcentagem de explicação acumulada dos dois primeiros eixos 
sum(100 * (pcoa_gov$values[, 1]/pcoa_gov$trace)[1:2])
#> [1] 94.85904

## Selecionar os dois primeiros eixos
eixos <- pcoa_gov$vectors[, 1:2]

## Juntar com algum dado categórico de interesse para fazer a figura
# Dataframe para plotar
pcoa_gov.dat <- data.frame(categoria = gove$category, eixos, mp = gove$mp, adm = gove$adm)

ggplot(pcoa_gov.dat, aes(x = Axis.1, y = Axis.2, fill = mp, 
                     color = mp, shape = adm)) +
  geom_point(size = 4, alpha = 0.7) + 
  labs(x = paste0("PCoA 1 (", round(pcoa_gov$values$Relative_eig[1]*100, 2), "%)"),
       y = paste0("PCoA 2 (", round(pcoa_gov$values$Relative_eig[2]*100, 2), "%)")) + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2)
colnames(pcoa_gov.dat) <- c("Axis.1", "Axis.2")

ggplot(pcoa_gov.dat, aes(x = Axis.1, y = Axis.2)) +
  geom_point(size = 3, alpha = 0.7, shape = 21, fill = "darkorange", color = "black") +
  labs(x = paste0("PCoA 1 (", round(pcoa_gov$values$Relative_eig[1]*100, 2), "%)"),
       y = paste0("PCoA 2 (", round(pcoa_gov$values$Relative_eig[2]*100, 2), "%)")) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_bw()

# Ajustar variáveis (quanti + quali) aos eixos do PCoA
ef_gov <- envfit(pcoa_gov$vectors, gove, permutations = 999, na.rm = TRUE)
ef_gov  # ver R² e p-valor de cada variável

# Extrair vetores das variáveis quantitativas (setas)
vec_gov <- as.data.frame(scores(ef_gov, "vectors")) 
vec_gov$var <- rownames(vec_gov)
vec_gov$pval <- ef_gov$vectors$pvals
r2_gov <- ef_gov$vectors$r
vec_gov$Axis.1 <- vec_gov$Axis.1 * sqrt(r2_gov)   # escalar pelo R² (padrão em biplots ecológicos)
vec_gov$Axis.2 <- vec_gov$Axis.2 * sqrt(r2_gov)

# Extrair centróides das variáveis qualitativas
fac_gov <- as.data.frame(scores(ef_gov, "factors"))
fac_gov$var <- rownames(fac_gov)
fac_gov$pval <- ef_gov$factors$pvals[ef_gov$factors$var.id]

# Escala para as setas ficarem visíveis no gráfico (ajuste o multiplicador se necessário)
mult_gov <- 1

ggplot() +
  geom_point(data = pcoa_gov.dat, aes(x = Axis.1, y = Axis.2),
             size = 3, alpha = 0.6, shape = 21, fill = "darkorange", color = "black") +
  geom_segment(data = vec_gov, aes(x = 0, y = 0, xend = Axis.1 * mult_gov, yend = Axis.2 * mult_gov),
               arrow = arrow(length = unit(0.25, "cm")), color = "black", linewidth = 0.6) +
  geom_text_repel(data = vec_gov, aes(x = Axis.1 * mult_gov, y = Axis.2 * mult_gov, label = var),
                  color = "black", fontface = "bold", size = 3.5) +
  geom_point(data = fac_gov, aes(x = Axis.1, y = Axis.2), shape = 17, size = 2, color = "blue") +
  geom_text_repel(data = fac_gov, aes(x = Axis.1, y = Axis.2, label = var), color = "blue", size = 3) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = paste0("PCoA 1 (", round(pcoa_gov$values$Relative_eig[1]*100, 2), "%)"),
       y = paste0("PCoA 2 (", round(pcoa_gov$values$Relative_eig[2]*100, 2), "%)")) +
  theme_bw()

#Correlation

# Mesmo bloco de governança (só as quanti)
scores_gov <- as.data.frame(pcoa_gov$vectors[, 1:5])
colnames(scores_gov) <- paste0("PCo", 1:5)

quanti_gov <- dados %>% select(year, area, min_dist_pa, urb_dist)

cor_gov <- cor(quanti_gov, scores_gov, use = "pairwise.complete.obs")
round(cor_gov, 3)

# Transformar em formato longo
cor_gov_long <- melt(cor_gov, varnames = c("Variavel", "PCo"), value.name = "Correlation")

# Garantir a ordem correta das variáveis e componentes nos eixos
cor_gov_long$PCo <- factor(cor_gov_long$PCo, levels = rev(paste0("PCo", 1:5)))  # 5 no topo? ajuste conforme necessidade
cor_gov_long$Variavel <- factor(cor_gov_long$Variavel, levels = colnames(quanti_gov))

ggplot(cor_gov_long, aes(x = Variavel, y = PCo, size = abs(Correlation), color = Correlation)) +
  geom_point() +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", 
                        midpoint = 0, limits = c(-1, 1), name = "Correlation") +
  scale_size_continuous(range = c(2, 12), guide = "none") +
  labs(x = NULL, y = "Principal Coordinates") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )

## Environment

df_env <- dados %>%
  dplyr::select(biome, spp_est, altitude, mean_temp, humidity, coverage, water_bodies) %>%
  dplyr::mutate(biome = as.factor(biome))


## Verificar se existem NAs nos dados
sum(is.na(df_env))
#> [1] 165

## Remover dados ausentes (NA), quando houver
env <- na.omit(df_env)

## Manter somentes dados contínuos que pretende aplicar a PCA
env_trait <-df_env[, 2:7]

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
#>       eigenvalue percentage of variance cumulative percentage of variance
#>comp 1  1.9639264               32.73211                          32.73211
#>comp 2  1.1785094               19.64182                          52.37393
#>comp 3  1.1170462               18.61744                          70.99137
#>comp 4  0.9147069               15.24511                          86.23648
#>comp 5  0.7180264               11.96711                          98.20359
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
#>                     Dim.1       Dim.2        Dim.3       Dim.4       Dim.5
#>spp_est       0.43063604 -0.34890969  0.227756267  0.67598770 -0.42892650
#>altitude     -0.88274141 -0.26298630 -0.007296309  0.28770452  0.15280464
#>mean_temp     0.87764348  0.01323923 -0.407396828 -0.12575551  0.01385403
#>humidity      0.06190612  0.60776820  0.764569303 -0.09606668 -0.14014340
#>coverage      0.07456067  0.68341334 -0.283358705  0.58669615  0.31889159
#>water_bodies  0.46861161 -0.38858506  0.484034642  0.07561740  0.62383912

## Contribuição (%) das variáveis para cada eixo
var_env$contrib 
#>                   Dim.1       Dim.2        Dim.3      Dim.4       Dim.5
#>spp_est       9.4426857 10.32982638  4.643757383 49.9569187 25.62272614
#>altitude     39.6772715  5.86858245  0.004765794  9.0492257  3.25186633
#>mean_temp    39.2203132  0.01487279 14.858129431  1.7289090  0.02673078
#>humidity      0.1951381 31.34316913 52.331425407  1.0089359  2.73529964
#>coverage      0.2830704 39.63089355  7.187899025 37.6308934 14.16268897
#>water_bodies 11.1815212 12.81265570 20.974022961  0.6251173 54.20068814


## Loadings - correlação das variáveis com os eixos
var_env$cor 
#>                    Dim.1       Dim.2        Dim.3       Dim.4       Dim.5
#>spp_est       0.43063604 -0.34890969  0.227756267  0.67598770 -0.42892650
#>altitude     -0.88274141 -0.26298630 -0.007296309  0.28770452  0.15280464
#>mean_temp     0.87764348  0.01323923 -0.407396828 -0.12575551  0.01385403
#>humidity      0.06190612  0.60776820  0.764569303 -0.09606668 -0.14014340
#>coverage      0.07456067  0.68341334 -0.283358705  0.58669615  0.31889159
#>water_bodies  0.46861161 -0.38858506  0.484034642  0.07561740  0.62383912

## Qualidade da representação da variável. Esse valor é obtido multiplicado var_env$coord por var_env$coord
var_env$cos2
#>                    Dim.1        Dim.2        Dim.3       Dim.4        Dim.5
#>spp_est      0.185447395 0.1217379711 5.187292e-02 0.456959374 0.1839779390
#>altitude     0.779232404 0.0691617937 5.323612e-05 0.082773890 0.0233492588
#>mean_temp    0.770258081 0.0001752772 1.659722e-01 0.015814449 0.0001919341
#>humidity     0.003832368 0.3693821828 5.845662e-01 0.009228806 0.0196401736
#>coverage     0.005559294 0.4670537911 8.029216e-02 0.344212372 0.1016918463
#>water_bodies 0.219596845 0.1509983470 2.342895e-01 0.005717991 0.3891752518

## Escores (posição) das localidades ("site scores") em cada eixo 
ind_env <- get_pca_ind(pca_env)

## Variáveis mais importantes para o Eixo 1
dimdesc(pca_env)$Dim.1 
#>              correlation      p.value   n
#>              mean_temp      0.8776435 1.876088e-82 254
#>              water_bodies   0.4686116 2.859652e-15 254
#>              spp_est        0.4306360 6.843797e-13 254
#>              altitude      -0.8827414 1.231243e-84 254


## Variáveis mais importantes para o Eixo 2
dimdesc(pca_env)$Dim.2 
#>              correlation      p.value   n
#>              coverage       0.6834133 2.668709e-36 254
#>              humidity       0.6077682 4.838880e-27 254
#>              altitude      -0.2629863 2.179995e-05 254
#>              spp_est       -0.3489097 1.104499e-08 254
#>              water_bodies  -0.3885851 1.395284e-10 254

## Variáveis mais importantes para o Eixo 3
dimdesc(pca_env)$Dim.3 

#>        correlation      p.value   n
#> humidity       0.7645693 5.590018e-50 254
#>water_bodies   0.4840346 2.527259e-16 254
#>spp_est        0.2277563 2.520636e-04 254
#>coverage      -0.2833587 4.472924e-06 254
#>mean_temp     -0.4073968 1.416878e-11 254


fviz_pca_biplot(X = pca_env, 
                geom.ind = "point", 
                fill.ind = env$biome, 
                col.ind = "black",
                alpha.ind = 0.7,
                pointshape = 21, 
                pointsize = 4,
                
                col.var = "black",
                invisible = "quali",
                title = NULL) +
  labs(x = "PC1 (32.73%)", y = "PC2 (19.64%)") + 
  xlim(c(-4, 5)) +
  ylim(c(-3, 3)) 

library(plotly)

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
cor_env <- cor(env_trait, use = "pairwise.complete.obs")

ggcorrplot(cor_env, lab = TRUE, lab_size = 3, hc.order = TRUE,
           type = "lower",
           outline.color = "white")


# Correlation 

# Coordenadas dos indivíduos no PCoA (todas as dimensões, ou só as que quiser)
scores_amb <- as.data.frame(pcoa_env$vectors[, 1:5])
colnames(scores_amb) <- paste0("PCo", 1:5)

# Variáveis quantitativas do bloco ambiental
quanti_amb <- dados %>%
  select(spp_est, altitude, mean_temp, humidity, coverage, water_bodies)

cor_amb <- cor(quanti_amb, scores_amb, use = "pairwise.complete.obs")

# Transformar em formato longo
cor_env_long <- melt(cor_amb, varnames = c("Variavel", "PCo"), value.name = "Correlation")

# Garantir a ordem correta das variáveis e componentes nos eixos
cor_env_long$PCo <- factor(cor_env_long$PCo, levels = rev(paste0("PCo", 1:5)))  # 5 no topo? ajuste conforme necessidade
cor_env_long$Variavel <- factor(cor_env_long$Variavel, levels = colnames(quanti_amb))

ggplot(cor_env_long, aes(x = Variavel, y = PCo, size = abs(Correlation), color = Correlation)) +
  geom_point() +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", 
                        midpoint = 0, limits = c(-1, 1), name = "Correlation") +
  scale_size_continuous(range = c(2, 12), guide = "none") +
  labs(x = NULL, y = "Principal Coordinates") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )

# Combine Plots
cor_env_long$Bloco <- "Environmental"
cor_gov_long$Bloco <- "Governance"
cor_all <- bind_rows(cor_env_long, cor_gov_long)

# Ordenar PCs (PC1 embaixo, PC5 no topo, como no gráfico de referência)
cor_all$PCo <- factor(cor_all$PCo, levels = rev(paste0("PCo", 1:5)))

# Ordenar variáveis dentro de cada bloco (mantém a ordem de seleção)
cor_all$Variavel <- factor(cor_all$Variavel, 
                           levels = c(colnames(quanti_gov), colnames(quanti_amb)))

ggplot(cor_all, aes(x = Variavel, y = PCo, size = abs(Correlation), color = Correlation)) +
  geom_point() +
  scale_color_gradient2(low = "darkorange", mid = "white", high = "green4", 
                        midpoint = 0, limits = c(-1, 1), name = "Correlation") +
  scale_size_continuous(range = c(2, 12), guide = "none") +
  facet_wrap(~ Bloco, scales = "free_x") +
  labs(x = NULL, y = "Principal Coordinates") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    strip.text = element_text(face = "bold", size = 13)
  )

## DATAFRAME FOR GLM
coord_gov <- as.data.frame(pcoa_gov$vectors[, 1:2])
colnames(coord_gov) <- paste0("gov_PCoA", 1:2)
coord_gov <- na.omit(coord_gov)

#coord_env <- as.data.frame(pcoa_env$vectors[, 1:2])
#colnames(coord_env) <- paste0("env_PCoA", 1:2)

coord_env <- coords

df_glm <- dados %>%
  bind_cols(coord_gov, coord_env)


