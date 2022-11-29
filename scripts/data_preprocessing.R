#Este script toma los datos iniciales, crea los bloques, lee el match que se hizo a mano y crea las variables
#explicativas del modelo.

library(stringr)
library(openxlsx)
library(tidyverse)
library(stringi)
library(haven)
library(stringdist)
library(doParallel)
library(tidyverse)
library(foreach)
library(openxlsx)
library(caret)


registerDoParallel(cores=4)

# READING DATA ------------------------------------------------------------



census_1859 <- read.xlsx("./datasets/clean_census_1859_v2.xlsx") %>% mutate(id_1859=as.numeric(id_1859))
census_1869 <- read.xlsx("./datasets/clean_census_1869.xlsx") %>% 
  rowid_to_column("id_1869") %>% mutate(id_1869=as.numeric(id_1869))



source("blocking_function.R")
source("features_function.R")

# BLOCK CREATION ----------------------------------------------------------

possible_matches <- data_blocking_initials(census_1859,census_1869,1859,1869)

possible_matches <- possible_matches %>%   
  mutate(id_1859=as.numeric(id_1859),
         id_1869=as.numeric(id_1869)) 


# HAND MATCH --------------------------------------------------------------


#Añadir variables adicionales necesarias para creación de variables

hand_match <- census_1859 %>% select(id_1859,individual_name,gender,relationship,male_relative,
                                     female_relative) %>% 
  
  mutate(relationship=ifelse(relationship=="child","offspring",relationship)) %>% 
  
  mutate_at(vars(id_1859),~as.numeric(.)) %>% 
  
  left_join(possible_matches,.,by="id_1859") %>%
  
  dplyr::rename(individual_a=individual_name,gender_a=gender,
                relationship_a=relationship,father_a=male_relative,
                mother_a=female_relative) %>% 
  
  left_join(.,census_1869[,c("id_1869","individual_name","gender","relationship","male_relative",
                             "female_relative")],by="id_1869") %>%
  
  dplyr::rename(individual_b=individual_name,gender_b=gender,
                relationship_b=relationship,father_b=male_relative,
                mother_b=female_relative) %>% 
  
  mutate_at(vars(gender_b),~str_to_lower(.)) %>% 
  
  mutate_if(is.character,~replace_na(.,"")) %>% 
  mutate(block=as.numeric(block),id_1859=as.numeric(id_1859)) %>% 
  arrange(block,id_1859)


#Leer match hecho a mano

hand_match_w_links <- read.xlsx("./datasets/hand_match_w_links.xlsx")



# Explanatory variables ---------------------------------------------------

explanatory_variables <- match_variables_creation(dataset_a = census_1859,
                                                  dataset_b= census_1869,
                                                  possible_matches=possible_matches,
                                                  year_census_a = 1859,
                                                  year_census_b = 1869)
#Join matches
curated_matches_2 <- hand_match_w_links %>% 
  mutate(id_1859=as.numeric(id_1859),
         id_1869=as.numeric(id_1869),
         match=factor(match)
         
         ) %>% 
  select(id_1859,id_1869,match,comentario) %>% 
  right_join(.,explanatory_variables,
             by=c("id_1859","id_1869")) %>% 
  filter(!is.na(match))

levels(curated_matches_2$match) <- c("false","true")



