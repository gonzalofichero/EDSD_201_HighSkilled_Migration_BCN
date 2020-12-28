# Importing libraries
library(tidyverse)
library(readr)

# Loading data
bcn <- read.csv("02 - Data/Datos_inmi_BCN.csv", sep = ";", header = T, encoding = "UTF-8")

glimpse(bcn)

# Convert Barri code to factor
bcn %>% 
  mutate(BARRI = as.factor(str_pad(BARRI, width=2, side="left", pad="0"))) -> bcn

####################################################################
# Population of Barcelona by Barri

pop_bcn <- read_csv("02 - Data/2018_padro_ocupacio_mitjana.csv")

pop_bcn <- pop_bcn %>% 
  select(Codi_Barri, Poblacio, Domicilis) %>%
  mutate(Codi_Barri = as.factor(str_pad(Codi_Barri, width=2, side="left", pad="0"))) %>% 
  rename(BARRI = Codi_Barri)


#####################################################
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
# [6] 54. Torre Bar?                                      
# [7] 56. Vallbona                                        
# [8] 58. Bar? de Viver 


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



# 2. Histogram of Nationalities to find the correct group by
bcn %>% 
  filter(Origin == "Ext") %>% 
  group_by(Birth_Country) %>% 
  summarize(total = sum(Casos, na.rm = T)) %>% 
  arrange(desc(total)) -> nationalities

# Grouping by into Europeans vs Latinos:
bcn %>% 
  mutate(nation = case_when(as.character(Birth_Country) %in% c("Resta Unió Europea", "Itàlia", "França", "Regne Unir", "Alemanya") ~ "European",
                            as.character(Birth_Country) %in% c("Argentina", "Veneçuela", "Colòmbia", "Brasil", "Mèxic", "Xile", "Perú", "Equador", "República Dominicana", "Hondures", "Uruguai", "Bolívia", "Paraguai", "Cuba") ~ "Latino",
                            TRUE ~ as.character(Birth_Country))) -> bcn

# Check:
bcn %>% 
  filter(nation == "European" | nation == "Latino") %>% 
  group_by(BARRI, NOM, nation) %>% 
  summarize(total = sum(Casos, na.rm = T)) %>% 
  pivot_wider(names_from = nation, values_from = total) %>% 
  mutate(prevalence = European - Latino) -> prevalence_by_barri


# Top Barris per origin
prevalence_by_barri %>%
  arrange(desc(Latino))


# Europeans:
  # 1. Raval
  # 2. Grácia
  # 3. Born
  # 4. Gótic
  # 5. Dreta de l'Eixample

# Latinos:
  # 1. Nova Esquerra de l'Eixample
  # 2. Sagrada Família
  # 3. Antiga Esquerra de l'Eixample
  # 4. Grácia
  # 5. Dreta de l'Eixample 


bcn_map %>% 
  filter(SCONJ_DESC == "Barri") %>% 
  left_join(prevalence_by_barri, by = "BARRI") %>%
  ggplot() +
  geom_sf(aes(fill = European))


# Barris that will need to be put together
bcn %>% 
    filter(nation == "European") %>% 
    group_by(NOM) %>% 
    summarize(N = sum(Casos)) %>% 
    filter(N < 15) %>% 
    pull(NOM) # 10 barris that need new grouping for Europeans

bcn %>% 
  filter(nation == "Latino") %>% 
  group_by(NOM) %>% 
  summarize(N = sum(Casos)) %>% 
  filter(N < 15) %>% 
  pull(NOM) # 5 barris that need new grouping for Europeans



  

###############################################
# Foreigners in BCN (2018) = social capital
foreign <- read_csv("C:/Users/Gonzalo/Desktop/EDSD/05 - Thesis/02 - Data/2018_domicilis_nacionalitat_espanyola_estrangera.csv")


glimpse(foreign)

foreign %>% 
  mutate(nationality = case_when(Nacionalitat %in% c("1 espanyol", "2 espanyols", "3 espanyols", "4 espanyols o m?s") ~ "National",
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

money <- read_csv("02 - Data/2017_rendatributariamitjanaperpersona.csv")

money %>% 
  group_by(Codi_Barri, Seccio_Censal) %>% 
  summarize(income = mean(Import_Euros)) %>%
  ungroup() %>% 
  mutate(Codi_Barri = as.factor(str_pad(Codi_Barri, width=2, side="left", pad="0"))) %>%
  rename(BARRI = Codi_Barri) -> money_v2


bcn_map %>% 
  filter(SCONJ_DESC == "Barri") %>% 
  left_join(money_v2, by = "BARRI") %>%
  ggplot() +
  geom_sf(aes(fill = income))


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
                          Desc_sup %in% c("211- 250 m2", "M?s de 250 m2") ~"04 XL (m?s 210m2)",
                          TRUE ~ "CHECK"
                          )) %>% 
  select(Nom_districte, Nom_barri, size, Nombre) %>%
  group_by(Nom_districte, Nom_barri, size) %>% 
  summarize(N = sum(Nombre)) %>% 
  pivot_wider(names_from = size, values_from = N) 


#####################################################
# Dynamic stock of net migration by nationality

inm_nac <- read_csv("02 - Data/Time Series Ethnic change/2018_immigrants_nacionalitats.csv")
emi_nac <- read_csv("02 - Data/Time Series Ethnic change/2018_emigrants_nacionalitats.csv")


inm_nac %>% 
    select(Nom_districte, Codi_barri, Nacionalitats, Nombre) %>%
    pivot_wider(names_from = Nacionalitats, values_from = Nombre) %>% 
    rename( spanish_inm = Espanyola,
            foreing_inm = Estrangera) -> inm_nac

emi_nac %>% 
    select(Nom_districte, Codi_barri, Nacionalitats, Nombre) %>%
    pivot_wider(names_from = Nacionalitats, values_from = Nombre) %>% 
    rename( spanish_emi = Espanyola,
            foreing_emi = Estrangera) -> emi_nac
  

total_movement <- left_join(inm_nac, emi_nac, by=c("Nom_districte","Codi_barri")) %>% 
                    mutate(spanish_net_move = spanish_inm - spanish_emi,
                           foreign_net_move = foreing_inm - foreing_emi) %>%
                    mutate(Codi_barri = as.factor(str_pad(Codi_barri, width=2, side="left", pad="0"))) %>% 
                    rename(BARRI = Codi_barri)
  
# Net migration for spaniards
bcn_map %>% 
  filter(SCONJ_DESC == "Barri") %>% 
  left_join(total_movement, by = "BARRI") %>%
  ggplot() +
  geom_sf(aes(fill = spanish_net_move))

# Net migration for foreigners
bcn_map %>% 
  filter(SCONJ_DESC == "Barri") %>% 
  left_join(total_movement, by = "BARRI") %>%
  ggplot() +
  geom_sf(aes(fill = foreign_net_move))


  
#######################################
# Voters (left vs right) from BCN web

# Tutorial:
# http://blog-r.es/data-extraction/web-scraping-of-tables-in-r/

library(rvest)
library(magrittr)

url_vote <- "https://www.bcn.cat/estadistica/catala/dades/barris/telec/loc/a2015.htm"

pagina <- read_html(url_vote, as.data.frame=T, stringsAsFactors = TRUE, encoding = "utf8")

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


# Counting votes to political left
# LEFT = BComú-E + ERC-AM + PSC-CP + CUP-PA
value_voters[,4:16] %>% 
  mutate(left_vote = as.numeric(gsub(".", "", `BCom<U+663C><U+3E61>-E`)) +
                     as.numeric(gsub(".", "", `ERC-AM`)) +
                     as.numeric(gsub(".", "", `PSC-CP`)) +
                     as.numeric(gsub(".", "", `CUP-PA`)),
         total_votes = as.numeric(gsub(".", "", `Votants`))
  )


votes <- read_csv("02 - Data/2015_Eleccions_Locals.csv")

votes %>% 
  mutate(left_vote = `BComu-E` + `ERC-AM` + `PSC-CP` + `CUP-PA`,
         indep_vote = CiU + `ERC-AM` + `CUP-PA`,
         perc_left = left_vote / Votants,
         perc_indep = indep_vote / Votants) -> votes


# More votes to catalan parties
bcn_map %>% 
  filter(SCONJ_DESC == "Barri") %>% 
  left_join(votes, by = "BARRI") %>%
  ggplot() +
  geom_sf(aes(fill = perc_indep))


# More votes to leftist parties
bcn_map %>% 
  filter(SCONJ_DESC == "Barri") %>% 
  left_join(votes, by = "BARRI") %>%
  ggplot() +
  geom_sf(aes(fill = perc_left))



# Source :
# https://www.bcn.cat/estadistica/catala/dades/barris/telec/loc/a2015.htm


######################################
# Nivel educativo por Barri

educ_barri <- read_csv("02 - Data/2018_padro_nivell_academic.csv")

educ_barri_2 <- educ_barri %>%
                mutate(Codi_Barri = as.factor(str_pad(Codi_Barri, width=2, side="left", pad="0"))) %>% 
                group_by(Codi_Barri, Nivell_academic) %>%
                summarize(total_educ = sum(Nombre)) %>%
                filter(Nivell_academic == "Estudis universitaris / CFGS grau superior") %>% 
                rename(BARRI = Codi_Barri)

educ_barri_pop <- educ_barri_2 %>% 
                  left_join(pop_bcn, by="BARRI") %>%
                  mutate(perc_educ_superior = total_educ / Poblacio)



# % Pop with University degree by Barri
bcn_map %>% 
  filter(SCONJ_DESC == "Barri") %>% 
  left_join(educ_barri_pop, by = "BARRI") %>%
  ggplot() +
  geom_sf(aes(fill = perc_educ_superior))


######################################
# Population Structure per Barri

demo_struc_barri <- read_csv("02 - Data/2018_ine_edat_any_a_any_per_sexe.csv")

# This is the Sex-Age structure per barri, wihtout kids (since they cannot have University degrees)
demo_struc_barri_2 <- demo_struc_barri %>%
                        mutate(Codi_Barri = as.factor(str_pad(Codi_Barri, width=2, side="left", pad="0")),
                               edat_group = case_when(Edat < 15 ~ "01 - 0 a 14 anys",
                                                      Edat >= 15 & Edat < 25 ~ "02 - 15 a 24 anys",
                                                      Edat >= 25 & Edat < 40 ~ "03 - 25 a 39 anys",
                                                      Edat >= 40 & Edat < 65 ~ "04 - 40 a 64 anys",
                                                      Edat >= 65 ~ "05 - 65 i mes"
                                                      )) %>% 
                        filter(edat_group != "01 - 0 a 14 anys") %>% 
                        group_by(Codi_Barri, Sexe, edat_group) %>%
                        summarize(total_edat = sum(Nombre)) %>%
                        ungroup() %>%  
                        rename(BARRI = Codi_Barri)

# Creating intermediate tables to calculate % by Age Group for each Sex and Barri, and % by Sex for each Barri
demo_struc_barri_tot_sexe <- demo_struc_barri_2 %>% 
                          group_by(BARRI, Sexe) %>% 
                          summarize(total_group_sexe = sum(total_edat))

demo_struc_barri_tot_barri <- demo_struc_barri_2 %>% 
                          group_by(BARRI) %>% 
                          summarize(total_group_barri = sum(total_edat))


demo_struc_barri_3 <- demo_struc_barri_2 %>% 
  left_join(demo_struc_barri_tot_sexe, by=c("BARRI","Sexe")) %>%
  mutate(perc_edat = total_edat / total_group_sexe) %>%
  # Expected % University degree by Sex and Age group, based on Encuesta Sociodemográfica de Barcelona
  mutate(standard = case_when(Sexe == "Dona" & edat_group == "02 - 15 a 24 anys" ~ 0.118625456,
                              Sexe == "Dona" & edat_group == "03 - 25 a 39 anys" ~ 0.568616795,
                              Sexe == "Dona" & edat_group == "04 - 40 a 64 anys" ~ 0.383737353,
                              Sexe == "Dona" & edat_group == "05 - 65 i mes" ~ 0.141615998,
                              
                              Sexe == "Home" & edat_group == "02 - 15 a 24 anys" ~ 0.0689734025,
                              Sexe == "Home" & edat_group == "03 - 25 a 39 anys" ~ 0.434937446,
                              Sexe == "Home" & edat_group == "04 - 40 a 64 anys" ~ 0.338336146,
                              Sexe == "Home" & edat_group == "05 - 65 i mes" ~ 0.241529018
                              )) %>% 
  left_join(demo_struc_barri_tot_barri, by=c("BARRI")) %>%
  # Adding % of Sex by Barri and calculating weights
  mutate(perc_sexe = total_group_sexe / total_group_barri,
         uni_per_structure = perc_edat * perc_sexe * standard) %>%
  group_by(BARRI) %>% 
  # When summing up, got the expected % University degree by Barri
  summarize(perc_university = sum(uni_per_structure))
  

# Plotting
bcn_map %>% 
  filter(SCONJ_DESC == "Barri") %>% 
  left_join(demo_struc_barri_3, by = "BARRI") %>%
  ggplot() +
  geom_sf(aes(fill = perc_university))








######################################
# Unitary Households by Barrio 2018

uni_house <- read_csv("02 - Data/2018_Hog_Unipersonal.csv")


uni_house_pop <- uni_house %>% 
                  left_join(pop_bcn, by="BARRI") %>%
                  mutate(perc_domi_uni = Total / Domicilis)
                  


# % Unipersonal Households by Barri
bcn_map %>% 
  filter(SCONJ_DESC == "Barri") %>% 
  left_join(uni_house_pop, by = "BARRI") %>%
  ggplot() +
  geom_sf(aes(fill = perc_domi_uni))



# Source:
# https://www.bcn.cat/estadistica/castella/dades/inf/llars/a2018/t102.htm


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

bcn_map <- st_read("02 - Data/Maps/0301100100_UNITATS_ADM_POLIGONS.json")

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
