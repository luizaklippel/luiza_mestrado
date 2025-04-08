# GETTING THE DATA

# install.packages("rgbif")
# library(rgbif)

# SPECIES LIST FROM ICMBio

# library(readxl)
eeis_icmbio <- read.csv2("Data/eeis_icmbio.csv") # FAUNA AND FLORA LIST AVAILABLE 

eeis_icmbio$Species

# EXTRACTING INVASIVE EXOTIC SPECIES FROM GBIF

dados <- occ_search(scientificName = eeis_icmbio$Species)
save(dados, file = "Data/Occs.RData")

git config --global user.email "luiza.mvk@gmail.com"
git config --global user.name "luizaklippel"
