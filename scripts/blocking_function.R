#Este script contiene la función que permite crear los bloques de posibles match

library(stringdist)
library(doParallel)
library(tidyverse)
library(foreach)
library(openxlsx)
library(caret)


registerDoParallel(cores=4)

registerDoSEQ()

#Advertencias:
#1. Las columnas tienen que estar limpia y corresponder a su tipo natural: edad es numérico, nombre es string, etc.
#2. No puede haber NA en las variables básicas que se utilizan para calcular las otras variables como los nombres,
#la edad, el género, etc.





# FUNCTION TO BLOCK AND CREATE POSSIBLE MATCHES ---------------------------

data_blocking_initials <- function(data_a,data_b,year_census_a,year_census_b){
  
  
  data_a_2<- data_a %>% 
    
    filter(age!=""&name_1!=""&surname_1!="") %>% 
    
    mutate(
      f_name=name_1 %>% str_squish(),
      l_name=surname_1 %>% str_squish(),
      age=as.numeric(age),
      initial_frst=str_extract(name_1,"^\\w{1}"),
      initial_last=str_extract(surname_1,"^\\w{1}"))  %>%
    
    rowid_to_column("id") %>% 
    
    select(contains("id"),f_name,l_name,age,initial_frst,initial_last)
  
  
  data_b_2<- data_b %>% 
    
    filter(age!=""&name_1!=""&surname_1!="") %>% 
    
    mutate(
      f_name=name_1 %>% str_squish(),
      l_name=surname_1 %>% str_squish(),
      age=as.numeric(age),
      initial_frst=str_extract(name_1,"^\\w{1}"),
      initial_last=str_extract(surname_1,"^\\w{1}"))  %>%
    
    rowid_to_column("id") %>% 
    
    select(contains("id"),f_name,l_name,age,initial_frst,initial_last)
  
  
  #BLOCK CREATION  
  
  timediff <- year_census_b-year_census_a
  
  data_ab <- data_a_2 %>% 
    mutate(Data=0) %>% 
    #add b
    bind_rows(.,data_b_2) %>% 
    mutate(Data=ifelse(is.na(Data),1,Data)) %>% 
    #update age to b
    mutate(age_match=ifelse(Data==0,age+timediff,age)) %>% 
    #group and assign ids based on blocking variables
    group_by(initial_frst,initial_last) %>% 
    dplyr::mutate(Data_Block=cur_group_id()) %>% 
    ungroup() %>% 
    #check that every block has data from a and b
    group_by(Data_Block,Data) %>% 
    mutate(Block_Size_ID=ifelse(duplicated(Data),0,1)) %>% 
    ungroup() %>% 
    group_by(Data_Block) %>% 
    mutate(Block_Size_Both=sum(Block_Size_ID)) %>% 
    ungroup() %>% 
    filter(Block_Size_Both==2) %>% 
    #update id variable
    group_by(Data_Block) %>% 
    dplyr::mutate(Block=cur_group_id()) %>% 
    ungroup() %>% 
    #select relevant variables
    select(Data, contains("id"), f_name, l_name, age_match, Block)
  
  
  #STRING DISTANCE  
  
  n_blocks<-max(data_ab$Block)
  All_Data<-data_ab
  
  data_final <- tibble()
  for (j in 1:n_blocks) {
    
    A<-All_Data[which(All_Data$Data==0 & All_Data$Block==j),]
    B<-All_Data[which(All_Data$Data==1 & All_Data$Block==j),]
    
    #Count the number of observations in the source dataset
    n<-nrow(A)
    
    #Initialize empty lists
    strdist_FN<-list()
    strdist_LN<-list()
    agedist<-list()
    n_obs<-list()
    
    #I now loop over each of the observations in the source dataset A. 
    # the line that starts with "index" identifies, for each observation in A, which observations in B belong to the same block
    # the lines that start with strdist_FN and strdist_LN compute the string distance of each observation in A with respect to each observation in B 
    #that belongs to its same block (this step is aimed at saving computational time)
    
    #Drop those that do not have a match within a five years window
    #PARA CADA OBSERVACIÓN EN A TOMA LA DIFERENCIA DE TODAS LAS B Y DEFINE UNA LONGITUD DE CUANTAS CUMPLEN EL CRITERIO
    #PARA CADA A Y ESTO LO ALMACENA EN LA LISTA n_obs
    n_obs<-foreach(i=1:n) %dopar% length(which(abs(B$age_match-A[i,"age_match"])<=3))
    
    #SELECCIONA SOLO AQUELLAS A QUE CONTIENEN MATCH EN B
    A<-A[which(unlist(n_obs)>0),]
    n_obs<-unlist(n_obs)[which(unlist(n_obs)>0)]
    
    #n CUENTA CUANTOS DE LOS NOMBRES EN A QUE TIENEN MATCH EN B RESPECTO AL CRITERIO DE EDAD
    n<-length(n_obs)
    
    #index_B identifies the position or the observations in dataset B that I compare to each observation in dataset A
    index_B<-foreach(i=1:n) %dopar% which(abs(B$age_match-A[i,"age_match"])<=3)
    
    #Create distances in age, first and last names
    agedist<-foreach(i=1:n) %dopar% abs(c(A[i,"age_match"]-B[which(abs(B$age_match-A[i,"age_match"])<=3),"age_match"]))
    strdist_LN<-foreach(i=1:n, .export='stringdist') %dopar% c(stringdist(A[i,"l_name"],B[which(abs(B$age_match-A[i,"age_match"])<=3),"l_name"],method="jw",p=0.1))
    strdist_FN<-foreach(i=1:n, .export='stringdist') %dopar% c(stringdist(A[i,"f_name"],B[which(abs(B$age_match-A[i,"age_match"])<=3),"f_name"],method="jw",p=0.1))
    
    strdist_FN<-unlist(strdist_FN)
    strdist_LN<-unlist(strdist_LN)
    agedist<-unlist(agedist)
    
    #Rounding: Apply some rounding to JW distances (this makes it more comparable to the Stata code)
    strdist_FN<-round(strdist_FN, 2)
    strdist_LN<-round(strdist_LN, 2)
    
    index_A<-c()
    
    #ES POSIBLE QUE NO HAYA NINGÚN MATCH EN B
    
    if (length(unlist(index_B))>0){
      
      for (i in 1:n){
        index_A<-append(index_A,rep(i,n_obs[i]))
      }
      
      data<-cbind(A[index_A,"id"], B[unlist(index_B),"id"], agedist, strdist_FN, strdist_LN)
      colnames(data)<-c("id_sample_A","id_sample_B","Age_Dist","strdist_FN","strdist_LN")
      
      data_2 <- left_join(data,unique(A[index_A,c("id","f_name","l_name")]),by=c("id_sample_A"="id"))
      colnames(data_2)<-c("id_sample_A","id_sample_B","Age_Dist","strdist_FN","strdist_LN","f_name_a","l_name_a")
      
      data_3 <- left_join(data_2,unique(B[unlist(index_B),c("id","f_name","l_name")]),by=c("id_sample_B"="id"))
      colnames(data_3)<-c("id_sample_A","id_sample_B","Age_Dist","strdist_FN","strdist_LN","f_name_a","l_name_a","f_name_b","l_name_b")
      
      
      data_4 <- data_3[data_3$strdist_FN<=0.2&data_3$strdist_LN<=0.2,]
      
      if(nrow(data_4)>0){
        data_4["block"] <- j
      }    
      
      
    }else{
      
      data_4 <- tibble()
    }
    
    #AÑADIR NUMERO DEL BLOQUE EN CASO DE QUE EL DATAFRAME NO ESTÉ VACÍO  
    
    
    data_final <- bind_rows(data_final,data_4)
    
    print(j)
  }
  
  
  data_final_2 <- data_final %>% 
    left_join(.,data_a_2 %>% select(starts_with("id")),by=c("id_sample_A"="id"),copy=T) %>% 
    left_join(.,data_b_2 %>% select(starts_with("id")),by=c("id_sample_B"="id"),copy=T)
  
  return(data_final_2)
  
  
  
  
}





