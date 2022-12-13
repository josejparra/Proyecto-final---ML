#Este script contiene la función que crea las variables explicativas del modelo

library(stringdist)
library(doParallel)
library(tidyverse)
library(foreach)
library(openxlsx)
library(caret)
library(reticulate)


registerDoParallel(cores=4)

# FUNCTION TO CREATE EXPLANATORY VARIABLES ----------------------------------

#Llamar script con funcion de distancia fonetica; no existe en R para español.
reticulate::source_python("./scripts/phonetic_algorithm.py")

match_variables_creation <- function(dataset_a,dataset_b,possible_matches,year_census_a,year_census_b){
  
  
  
  id_a <- paste0("id_",year_census_a)
  id_b <- paste0("id_",year_census_b)
  
  
  
  explanatory_variables <- dataset_a %>% 
    
    select(id_a,name_2,surname_2,relationship,
           mr_name_1,mr_surname_1,fr_name_1,fr_surname_1,gender) %>%
    
    left_join(possible_matches,.,by=id_a) %>%
    dplyr::rename(m_name_a=name_2,sl_name_a=surname_2,
                  relationship_a=relationship,gender_a=gender,f_name_father_a=mr_name_1,
                  l_name_father_a=mr_surname_1,f_name_mother_a=fr_name_1,l_name_mother_a=fr_surname_1) %>% 
    
    mutate_at(vars(id_a),~as.numeric(.)) %>% 
    
    left_join(.,dataset_b[,c(id_b,"name_2","surname_2","relationship",
                             "mr_name_1","mr_surname_1","fr_name_1","fr_surname_1","gender")],by=id_b) %>%
    
    dplyr::rename(m_name_b=name_2,sl_name_b=surname_2,
                  relationship_b=relationship,gender_b=gender,f_name_father_b=mr_name_1,
                  l_name_father_b=mr_surname_1,f_name_mother_b=fr_name_1,
                  l_name_mother_b=fr_surname_1) %>% 
    
    mutate_at(vars(gender_b),~str_to_lower(.)) %>% 
    
    mutate_at(vars(contains(c("name","relationship","gender"))),~replace_na(.,"")) 
  
  
  #Calculate variables with parallel cycles
  
  hits <- unlist(foreach(i=1:nrow(explanatory_variables)) %dopar% {
    sum(possible_matches[[id_a]]%in%possible_matches[[id_a]][i])})
  
  exact_mult <- unlist(foreach(i=1:nrow(explanatory_variables)) %dopar% {
    ifelse(sum(explanatory_variables$f_name_b[which(explanatory_variables[[id_a]]%in%explanatory_variables[[id_a]][i])]%in%explanatory_variables$f_name_a[i]) > 1 & sum(explanatory_variables$l_name_b[which(explanatory_variables[[id_a]]%in%explanatory_variables[[id_a]][i])]%in%explanatory_variables$l_name_a[i]) > 1,1,0)})
  
  
  
  #continuation of tidy use  
  
  
  
  
  explanatory_variables_2 <- explanatory_variables %>%
    
    bind_cols(.,tibble("hits"=hits)) %>% 
    bind_cols(.,tibble("exact_mult"=exact_mult)) %>% 
    
    
    #First names and lastnames
    
    
    mutate(
      exact=ifelse(f_name_a==f_name_b&l_name_a==l_name_b,1,0),
      exact_all=ifelse(f_name_a==f_name_b&l_name_a==l_name_b&Age_Dist==0,1,0)) %>% 
    
    
    mutate(
      f_start=ifelse(str_extract(f_name_a,"^\\w")==str_extract(f_name_b,"^\\w"),1,0),
      f_end=ifelse(str_extract(f_name_a,"\\w$")==str_extract(f_name_b,"\\w$"),1,0),
      l_start=ifelse(str_extract(l_name_a,"^\\w")==str_extract(l_name_b,"^\\w"),1,0),
      l_end=ifelse(str_extract(l_name_a,"\\w$")==str_extract(l_name_b,"\\w$"),1,0))%>% 
    
    mutate(f_name_dif=abs(str_count(f_name_a,"\\w")-str_count(f_name_b,"\\w")),
           l_name_dif=abs(str_count(l_name_a,"\\w")-str_count(l_name_b,"\\w"))) %>% 
    
    mutate(
      f_name_a_length=str_count(f_name_a,"\\w"),
      f_name_b_length=str_count(f_name_b,"\\w"),
      
      f_name_subset=ifelse(
        f_name_a_length-f_name_b_length>0,
        as.numeric(str_detect(f_name_a,paste("^",f_name_b,sep=""))),
        as.numeric(str_detect(f_name_b,paste("^",f_name_a,sep=""))))) %>% 
    
    mutate(
      middle_name_not_empty=ifelse(m_name_a==""|m_name_b=="",0,1),
      second_lastname_not_empty=ifelse(sl_name_a==""|sl_name_b=="",0,1)) %>% 
    
    #checking if initial is equal only if both nonempty and one of the strings has is an abbreviation
    mutate(m_start=ifelse(middle_name_not_empty==0,0,
                          ifelse((str_extract(m_name_a,"^\\w")==str_extract(m_name_b,"^\\w")),
                                 2,1)),
           
           sl_start=ifelse(second_lastname_not_empty==0,0,
                           ifelse((str_extract(sl_name_a,"^\\w")==str_extract(sl_name_b,"^\\w")),
                                  2,1))
    ) %>% 
    #string_distance distance only if both are non empty (there is no difference if values when empty are 1 or 0)
    mutate(strdist_mn_2=ifelse(middle_name_not_empty==0,0,
                               ifelse(round(stringdist(m_name_a,m_name_b,method="jw",p=0.1),2)==0,
                                      0.01,round(stringdist(m_name_a,m_name_b,method="jw",p=0.1),2))),
           strdist_sln_2=ifelse(second_lastname_not_empty==0,0,
                                ifelse(round(stringdist(sl_name_a,sl_name_b,method="jw",p=0.1),2)==0,0.01,
                                       round(stringdist(sl_name_a,sl_name_b,method="jw",p=0.1),2)))) %>% 
    
    
    mutate(
      
      relationship_offspring=ifelse(
        relationship_a=="child"&relationship_b=="child",1,0),
      
      father_fn_not_empty=ifelse(relationship_offspring==1&!(f_name_father_a==""|f_name_father_b==""),1,0),
      
      father_ln_not_empty=ifelse(relationship_offspring==1&!(l_name_father_a==""|l_name_father_b==""),1,0)) %>% 
    
    #string_distance distance  
    mutate(strdist_father_fn=ifelse(father_fn_not_empty==0,0,
                                    ifelse(round(stringdist(f_name_father_a,f_name_father_b,method="jw",p=0.1),2)==0,0.01,
                                           round(stringdist(f_name_father_a,f_name_father_b,method="jw",p=0.1),2))),
           
           strdist_father_ln=ifelse(father_ln_not_empty==0,0,
                                    ifelse(round(stringdist(l_name_father_a,l_name_father_b,method="jw",p=0.1),2)==0,0.01,
                                           round(stringdist(f_name_father_a,f_name_father_b,method="jw",p=0.1),2)))) %>% 
    
    mutate(
      
      mother_fn_not_empty=ifelse(relationship_offspring==1&!(f_name_mother_a==""|f_name_mother_b==""),1,0),
      mother_ln_not_empty=ifelse(relationship_offspring==1&!(l_name_mother_a==""|l_name_mother_b==""),1,0)) %>% 
    
    #string_distance distance  
    mutate(strdist_mother_fn=ifelse(mother_fn_not_empty==0,0,
                                    ifelse(round(stringdist(f_name_mother_a,f_name_mother_b,method="jw",p=0.1),2)==0,0.01,
                                           round(stringdist(f_name_mother_a,f_name_mother_b,method="jw",p=0.1),2))),
           
           strdist_mother_ln=ifelse(mother_ln_not_empty==0,0,
                                    ifelse(round(stringdist(l_name_mother_a,l_name_mother_b,method="jw",p=0.1),2)==0,0.01,
                                           round(stringdist(f_name_mother_a,f_name_mother_b,method="jw",p=0.1),2)))) %>% 
    
    
    
    mutate(
      gender_exist=ifelse(gender_a!=""&gender_b!="",1,0),
      gender_exact=ifelse(gender_a!=""&gender_b!=""&gender_a==gender_b,1,0)) %>% 
    
    mutate(
      hits_2 =hits^2,
      middle_name_not_empty_strdist_mn_2=middle_name_not_empty*strdist_mn_2,
      second_lastname_not_empty_strdist_sln_2=second_lastname_not_empty*strdist_sln_2,
      father_ln_not_empty_father_fn_not_empty=father_ln_not_empty*father_fn_not_empty,
      mother_fn_not_empty_mother_ln_not_empty=mother_fn_not_empty*mother_ln_not_empty,
      relationship_offspring_father_fn_not_empty_strdist_father_fn=relationship_offspring*father_fn_not_empty*strdist_father_fn,
      relationship_offspring_father_ln_not_empty_strdist_father_ln=relationship_offspring*father_ln_not_empty*strdist_father_ln,
      relationship_offspring_mother_fn_not_empty_strdist_mother_fn=relationship_offspring*mother_fn_not_empty*strdist_mother_fn,
      relationship_offspring_mother_ln_not_empty_strdist_mother_ln=relationship_offspring*mother_ln_not_empty*strdist_mother_ln)
  
  
  
  unique_f_n_exact_prev <- unlist(foreach(i=1:nrow(explanatory_variables_2)) %dopar% {
    
    ifelse(sum(explanatory_variables_2$f_name_b[which(explanatory_variables_2[[id_a]]%in%explanatory_variables_2[[id_a]][i])]%in%explanatory_variables_2$f_name_a[i]) == 1 & sum(explanatory_variables_2$l_name_b[which(explanatory_variables_2[[id_a]]%in%explanatory_variables_2[[id_a]][i])]%in%explanatory_variables_2$l_name_a[i]) == 1,1,0)})
  
  
  unique_gender_prev <- unlist(foreach(i=1:nrow(explanatory_variables_2)) %dopar% {
    
    ifelse(sum(explanatory_variables_2$gender_b[which(explanatory_variables_2[[id_a]]%in%explanatory_variables_2[[id_a]][i])]%in%explanatory_variables_2$gender_a[i]) == 1,1,0)})
  
  
  
  explanatory_variables_3 <-  explanatory_variables_2 %>% 
    bind_cols(.,tibble("unique_f_n_exact_prev"=unique_f_n_exact_prev)) %>% 
    bind_cols(.,tibble("unique_gender_prev"=unique_gender_prev)) %>% 
    
    mutate(
      
      unique_f_n_exact=ifelse(exact==1&unique_f_n_exact_prev==1,1,0),
      unique_gender=ifelse(unique_gender_prev==1&gender_exact==1,1,0)) %>% 
    
    mutate(
      strdist_FN=as.numeric(strdist_FN),
      strdist_LN=as.numeric(strdist_LN))
  
  
  
  phonetic_encoding<- phonetic_encoder(explanatory_variables_3,
                                       list("f_name_a","f_name_b","l_name_a","l_name_b")) %>% 
    mutate(
      fsoundex=ifelse(f_name_a==f_name_b,1,0),
      lsoundex=ifelse(l_name_a==l_name_b,1,0))
  
  
  
  explanatory_variables_4 <- explanatory_variables_3 %>% 
    bind_cols(phonetic_encoding[,c("fsoundex","lsoundex")])
  
  
  return(explanatory_variables_4)
  
  
}



