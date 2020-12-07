# Importing libraries
library(tidyverse)
library(readr)

# Loading data
bcn <- read.csv("C:/Users/Gonzalo/Desktop/EDSD/05 - Thesis/02 - Data/Datos_inmi_BCN.csv", sep = ";", header = T, encoding = "UTF-8")

glimpse(bcn)


# Quick tables

# 1. Less than 100 movements per Barrio
bcn %>% 
  group_by(Barrio) %>% 
  summarize(N = sum(Casos)) %>% 
  filter(N < 100) %>% 
  pull(Barrio)
# 8 barrios a ser reagrupados:
# [1] 12. la Marina del Prat Vermell - AEI Zona Franca (2)
# [2] 22. Vallvidrera, el Tibidabo i les Planes           
# [3] 42. la Clota                                        
# [4] 47. Can Peguera                                     
# [5] 49. Canyelles                                       
# [6] 54. Torre Baró                                      
# [7] 56. Vallbona                                        
# [8] 58. Baró de Viver 


# Top 10 Barrios for Women
bcn %>%   
  group_by(Barrio, Sex) %>% 
  summarize(N = sum(Casos))  %>%
  group_by(Barrio) %>% 
  mutate(dist_barrio = N / sum(N)) %>%
  select(-N) %>% 
  pivot_wider(names_from = Sex, values_from = dist_barrio) %>% 
  arrange(desc(Dona))


# Top 10 Barrios for Men
bcn %>%   
  group_by(Barrio, Sex) %>% 
  summarize(N = sum(Casos))  %>%
  group_by(Barrio) %>% 
  mutate(dist_barrio = N / sum(N)) %>%
  select(-N) %>% 
  pivot_wider(names_from = Sex, values_from = dist_barrio) %>% 
  arrange(desc(Home))



# Top 10 Barrios for Argentines
bcn %>%   
  group_by(Barrio, Birth_Country) %>% 
  summarize(N = sum(Casos))  %>%
  group_by(Barrio) %>% 
  mutate(dist_barrio = N / sum(N)) %>%
  select(-N) %>% 
  pivot_wider(names_from = Birth_Country, values_from = dist_barrio) %>%
  select(Barrio, Argentina) %>% 
  arrange(desc(Argentina))


bcn %>% 
  filter(Birth_Country == "Argentina", Casos > 10) %>% 
  group_by(Barrio) %>% 
  mutate(dist_barrio = Casos / sum(Casos)) %>%
  arrange(desc(dist_barrio))
  

###############################################
# Foreigners in BCN (2018) = social capital
foreign <- read_csv("C:/Users/Gonzalo/Desktop/EDSD/05 - Thesis/02 - Data/2018_domicilis_nacionalitat_espanyola_estrangera.csv")


glimpse(foreign)

foreign %>% 
  mutate(nationality = case_when(Nacionalitat %in% c("1 espanyol", "2 espanyols", "3 espanyols", "4 espanyols o més") ~ "National",
                                 TRUE ~ "Foreigner")) %>%
  group_by(Nom_Districte, Codi_Barri, Nom_Barri, nationality) %>% 
  summarize(N_nationality = sum(Nombre)) %>% 
  pivot_wider(names_from = nationality, values_from = N_nationality) %>% 
  mutate(dist_foreign = Foreigner / (Foreigner + National)) %>% 
  ungroup() %>% 
  select(Codi_Barri, dist_foreign) %>% 
  mutate(Codi_Barri = as.factor(str_pad(Codi_Barri, width=2, side="left", pad="0"))) %>% 
  rename(BARRI = Codi_Barri) -> extranjeros_map


###############################################
# Lugares de Culto BCN = social capital?

cult <- read_csv("C:/Users/Gonzalo/Desktop/EDSD/05 - Thesis/02 - Data/LLOCS_CULTE.csv")

cult %>% 
  select(NOM_DISTRICTE, NOM_BARRI, NOM_CAPA) %>% 
  group_by(NOM_DISTRICTE, NOM_BARRI, NOM_CAPA) %>% 
  summarize(N = n()) %>% 
  pivot_wider(names_from = NOM_CAPA, values_from = N) %>%
  arrange(NOM_DISTRICTE, NOM_BARRI)

# HAVE TO GROUP INTO CATEGORIES THE PLACES TO BE MORE MANAGEABLE



##########################################
# $$$ = economic capital

money <- read_csv("C:/Users/Gonzalo/Desktop/EDSD/05 - Thesis/02 - Data/2017_rendatributariamitjanaperpersona.csv")

money %>% 
  select(Nom_Districte, Nom_Barri, Import_Euros) %>%
  group_by(Nom_Districte, Nom_Barri) %>% 
  summarize(income = mean(Import_Euros)) %>% 
  arrange(income)
  

#########################################
# Bars per Barrio = cultural capital

bars <- read_csv("C:/Users/Gonzalo/Desktop/EDSD/05 - Thesis/02 - Data/2019_1s_data_set_opendata_terrasses.csv")


bars %>% 
  select(NOM_DISTRICTE, CODI_BARRI, NOM_BARRI, TAULES) %>%
  group_by(NOM_DISTRICTE, CODI_BARRI, NOM_BARRI) %>% 
  summarize(mesas = sum(TAULES)) %>% 
  ungroup() %>% 
  select(CODI_BARRI, mesas) %>% 
  mutate(CODI_BARRI = as.factor(str_pad(CODI_BARRI, width=2, side="left", pad="0"))) %>% 
  rename(BARRI = CODI_BARRI) -> bares_map



####################################
# Median Age of Buildings = control

age_build <- read_csv("C:/Users/Gonzalo/Desktop/EDSD/05 - Thesis/02 - Data/2018_loc_hab_edat_mitjana.csv")


age_build %>% 
  select(Nom_districte, Codi_barri, Nom_barri, Edat_mitjana) %>%
  group_by(Nom_districte, Codi_barri, Nom_barri) %>% 
  summarize(age_building = mean(Edat_mitjana)) %>% 
  ungroup() %>% 
  select(Codi_barri, age_building) %>% 
  mutate(Codi_barri = as.factor(str_pad(Codi_barri, width=2, side="left", pad="0"))) %>% 
  rename(BARRI = Codi_barri) -> flat_age_map


####################################
# Size (renting?) flats = control

size_flat <- read_csv("C:/Users/Gonzalo/Desktop/EDSD/05 - Thesis/02 - Data/2018_loc_hab_sup.csv")


size_flat %>% 
  select(Nom_districte, Nom_barri, Desc_sup, Nombre) %>%
  mutate(size = case_when(Desc_sup %in% c("Fins a 30 m2", "31- 60 m2") ~"01 S (Up to 60m2)",
                          Desc_sup %in% c("61- 90 m2", "91- 120 m2") ~"02 M (60 to 120m2)",
                          Desc_sup %in% c("121- 150 m2", "151- 210 m2") ~"03 L (120 to 210m2)",
                          Desc_sup %in% c("211- 250 m2", "Més de 250 m2") ~"04 XL (més 210m2)",
                          TRUE ~ "CHECK"
                          )) %>% 
  select(Nom_districte, Nom_barri, size, Nombre) %>%
  group_by(Nom_districte, Nom_barri, size) %>% 
  summarize(N = sum(Nombre)) %>% 
  pivot_wider(names_from = size, values_from = N) 


  
#######################################
# Voters (left vs right) from BCN web

# Tutorial:
# http://blog-r.es/data-extraction/web-scraping-of-tables-in-r/

library(rvest)
library(magrittr)

url_vote <- "https://www.bcn.cat/estadistica/catala/dades/barris/telec/loc/a2015.htm"

pagina <- read_html(url_vote, as.data.frame=T, stringsAsFactors = TRUE)

pagina %>% 
  #Here, we indicate that this is the table we want to extract.
  html_nodes("table") %>% 
  #Here we put of which table of the HTML is about, in our example it is the third table of the web.
  .[[1]] %>% 
  html_table(fill=T) %>% 
  data.frame() -> temp_table

# Selecting names from row 7 and data.frame from row 12 (73 neighborhoods)
names_voters <- as.list(temp_table[7,])
value_voters <- temp_table[12:(73+12-1),]
names(value_voters) <- names_voters




# Source :
# https://www.bcn.cat/estadistica/catala/dades/barris/telec/loc/a2015.htm




######################################
# Cultural stuff in BCN with Geo

# It's and RDF file, so need tutorial to read from web:
# https://cran.r-project.org/web/packages/rdflib/vignettes/rdf_intro.html


# Source:
# http://www.bcn.cat/tercerlloc/cultura.rdf




##############################
# MAPPING
library(ggplot2)
library(sf)
library(colorspace)

# Reading the json with sf::st_read and saving the map information for Argentina
# web <- "https://opendata-ajuntament.barcelona.cat/data/dataset/808daafa-d9ce-48c0-925a-fa5afdb1ed41/resource/cd800462-f326-429f-a67a-c69b7fc4c50a/download"

bcn_map <- st_read("C:/Users/Gonzalo/Desktop/EDSD/05 - Thesis/02 - Data/Maps/0301100100_UNITATS_ADM_POLIGONS.json")

bcn_map2 <- bcn_map %>% filter(SCONJ_DESC == "Barri")

bcn_map3 <- bcn_map2 %>% 
              left_join(extranjeros_map, by = "BARRI") %>% 
              left_join(bares_map, by = "BARRI") %>% 
              left_join(flat_age_map, by = "BARRI") %>% 
              mutate(dist_foreign = if_else(is.na(dist_foreign), 0, dist_foreign),
                     mesas = if_else(is.na(mesas), 0, mesas),
                     age_building = if_else(is.na(age_building), 0, age_building))


bcn_map3 %>% 
  filter(!is.na(dist_foreign)) %>% 
ggplot() +
  geom_sf(aes(fill = dist_foreign))

bcn_map3 %>% 
  #filter(!is.na(dist_foreign)) %>% 
  ggplot() +
  geom_sf(aes(fill = mesas))

bcn_map3 %>% 
  #filter(!is.na(dist_foreign)) %>% 
  ggplot() +
  geom_sf(aes(fill = age_building))





######################################
# FLICKR data
# Academic Research: High Skilled Migration in Barcelona

# Key:
# fa328684f9add14cf5cf073b1c6ff992

# Secret:
# 6a46788cb1238ad5
