#Este script toma las variables creadas en data_preprocessing.R. Define las funciones de pérdida, 
#hace la partición de los datos estratificada y entrena los modelos usando diferentes métodos 
#para lidiar con problema de desbalance


library(stringr)
library(doParallel)
library(foreach)
library(caret)
library(reticulate)
library(rlang)
library(tidyverse)


source("data_preprocessing.R")

# LOSS FUNCTION -----------------------------------------------------------

#VERSION NUEVA

train_weighed_tpr_ppv <- function (data, 
                                   lev = NULL, 
                                   model = NULL,
                                   weight,
                                   id_a,
                                   id_b,
                                   data_w_ids,#train en este caso
                                   par_1,#threshold
                                   par_2#distance to second best
                                   ) {
  
  
  
  data_w_ids <- data_w_ids[data$rowIndex,]
  
  
  
  weight=weight
  verdict <- vector()
  df_results_2 <- data %>% bind_cols(!!sym(id_a):=data_w_ids[[id_a]],!!sym(id_b):=data_w_ids[[id_b]])
  
  for (i in 1:nrow(data)){
    
    
    subset_a <- df_results_2[which(df_results_2[[id_a]]%in%df_results_2[[id_a]][i]),] %>% arrange(desc(true))
    subset_b <- df_results_2[which(df_results_2[[id_b]]%in%df_results_2[[id_b]][i]),] %>% arrange(desc(true))
    
    max_a <- subset_a[1,]
    max_a_prev <- subset_a[2,]
    max_b <- subset_b[1,]
    max_b_prev <- subset_b[2,]
    
    if(max_a$rowIndex==data$rowIndex[i] & max_a[[id_b]]==max_b[[id_b]] & max_a[[id_a]]==max_b[[id_a]]){
      
      if(!is.na(max_a_prev$true)&max_a$true>par_1&(max_a$true/max_a_prev$true)>par_2){
        
        if(!is.na(max_b_prev$true)&(max_b$true/max_b_prev$true)>par_2){
          
          verdict[i] <- "true"
          
        }else if(is.na(max_b_prev$true)){
          
          verdict[i] <- "true"
          
        } else{
          
          verdict[i] <- "false"
          
        }
        
      }else if(is.na(max_a_prev$true)&max_a$true>par_1){
        if(!is.na(max_b_prev$true)&(max_b$true/max_b_prev$true)>par_2){
          verdict[i] <- "true"
          
        }else if(is.na(max_b_prev$true)){
          verdict[i] <- "true"
        } else{
          verdict[i] <- "false"
        }
      }else{
        verdict[i] <- "false"
      }
    }else{
      verdict[i] <- "false"
    }
    
    
    
  }
  
  
  
  #1's are true positives and 0's are false positives
  tp <- sum(verdict[data$obs%in%"true"]%in%"true")
  fp <- sum(verdict[data$obs%in%"false"]%in%"true")
  
  #1's are false negatives and 0's are true negatives
  fn <- sum(verdict[data$obs%in%"true"]%in%"false")
  tn <- sum(verdict[data$obs%in%"false"]%in%"false")
  
  #tpr
  
  if((tp+fn)!=0){
    tpr <- tp/(tp+fn)
  }else{
    tpr <- 0
    
  }
  
  if((tp+fp)!=0){
    ppv <- tp/(tp+fp)
    
  }else{
    ppv <- 0
  }
  
  weighed_tpr_ppv=weight*ppv+(1-weight)*tpr
  names(weighed_tpr_ppv) <- c("weighed_tpr_ppv")
  
  return(weighed_tpr_ppv)
  
} 


#LOSS CON TODAS LAS MÉTRICAS

test_weighed_tpr_ppv <- function (data, 
                                  lev = NULL, 
                                  model = NULL,
                                  weight,
                                  id_a,
                                  id_b,
                                  data_w_ids,#test en este caso
                                  par_1,#threshold
                                  par_2#distance to second best
                                  
                                  
) {
  
  weight=weight
  verdict <- vector()
  df_results_2 <- data %>% bind_cols(!!sym(id_a):=data_w_ids[[id_a]],!!sym(id_b):=data_w_ids[[id_b]])
  
  for (i in 1:nrow(data)){
    
    subset_a <- df_results_2[which(df_results_2[[id_a]]%in%df_results_2[[id_a]][i]),] %>% arrange(desc(true))
    subset_b <- df_results_2[which(df_results_2[[id_b]]%in%df_results_2[[id_b]][i]),] %>% arrange(desc(true))
    
    max_a <- subset_a[1,]
    max_a_prev <- subset_a[2,]
    max_b <- subset_b[1,]
    max_b_prev <- subset_b[2,]
    
    if(max_a$rowIndex==data$rowIndex[i] & max_a[[id_b]]==max_b[[id_b]] & max_a[[id_a]]==max_b[[id_a]]){
      
      if(!is.na(max_a_prev$true)&max_a$true>par_1&(max_a$true/max_a_prev$true)>par_2){
        
        if(!is.na(max_b_prev$true)&(max_b$true/max_b_prev$true)>par_2){
          
          verdict[i] <- "true"
          
        }else if(is.na(max_b_prev$true)){
          
          verdict[i] <- "true"
          
        } else{
          
          verdict[i] <- "false"
          
        }
        
      }else if(is.na(max_a_prev$true)&max_a$true>par_1){
        if(!is.na(max_b_prev$true)&(max_b$true/max_b_prev$true)>par_2){
          verdict[i] <- "true"
          
        }else if(is.na(max_b_prev$true)){
          verdict[i] <- "true"
        } else{
          verdict[i] <- "false"
        }
      }else{
        verdict[i] <- "false"
      }
    }else{
      verdict[i] <- "false"
    }
    
    
    
  }
  
  
  
  #1's are true positives and 0's are false positives
  tp <- sum(verdict[data$obs%in%"true"]%in%"true")
  fp <- sum(verdict[data$obs%in%"false"]%in%"true")
  
  #1's are false negatives and 0's are true negatives
  fn <- sum(verdict[data$obs%in%"true"]%in%"false")
  tn <- sum(verdict[data$obs%in%"false"]%in%"false")
  
  #tpr
  
  if((tp+fn)!=0){
    tpr <- tp/(tp+fn)
  }else{
    tpr <- 0
    
  }
  
  if((tp+fp)!=0){
    ppv <- tp/(tp+fp)
    
  }else{
    ppv <- 0
  }
  
  
  weighed_tpr_ppv=weight*ppv+(1-weight)*tpr
  
  names(weighed_tpr_ppv) <- c("weighed_tpr_ppv")
  names(ppv) <- c("ppv")
  names(tpr) <- c("tpr")
  names(weight) <- c("weight")
  
  names(par_1) <- "prob_threshold"
  names(par_2) <- "prob_ratio"
  
  return(list(tpr,ppv,weighed_tpr_ppv,weight,par_1,par_2))
  
} 

# DATA SPLIT -------------------------------------------------------------



prop.table(table(curated_matches_2$match))

set.seed(1)
split1 <- createDataPartition(curated_matches_2$match , p = 0.8)[[1]]

#separar en entrenamiento y testeo
train=curated_matches_2[split1,]
test=curated_matches_2[-split1,]

#Verificar que respetan las proporciones
prop.table(table(train$match))
prop.table(table(test$match))


#TODAS LAS VARIABLES
formula <- match~strdist_FN+
  strdist_LN+
  hits+
  hits_2+
  exact+
  exact_all+
  gender_exact+
  lsoundex+
  fsoundex+
  exact_mult+
  f_name_subset+
  middle_name_not_empty_strdist_mn_2+
  second_lastname_not_empty_strdist_sln_2+
  m_start+
  relationship_offspring+
  father_ln_not_empty_father_fn_not_empty+
  mother_fn_not_empty_mother_ln_not_empty+
  relationship_offspring_father_fn_not_empty_strdist_father_fn+
  relationship_offspring_father_ln_not_empty_strdist_father_ln+
  relationship_offspring_mother_fn_not_empty_strdist_mother_fn+
  relationship_offspring_mother_ln_not_empty_strdist_mother_ln+
  unique_gender+
  unique_f_n_exact



# MODEL SELECTION ---------------------------------------------------------


# PROBIT -------------------------------------------------------
#NUEVA VERSION





results_table_probit <- tibble()


for (h in c("optimal_cutoff","rose","smote","up","down")){
  
  #weight
  for (i in seq(0,1,0.5)){
    
    #probability ratio
    for (k in seq(1, 2, by = 0.5)){
      
      
      if(h=="optimal_cutoff"){
        
        
        #probability threshold
        for (j in seq(0.3, 0.9, by = 0.3)){
          
          
          

          
          folds=10
          cvIndex <- createFolds(factor(train$match), folds, returnTrain = T)
          
          #TRAIN CONTROL
          train_control <- trainControl(method="cv", 
                                        index=cvIndex,
                                        number=10, 
                                        savePredictions = TRUE,
                                        classProbs=TRUE,
                                        summaryFunction = function(...) train_weighed_tpr_ppv(...,
                                                                                              weight=i,
                                                                                              id_a="id_sample_A",
                                                                                              id_b="id_sample_B",
                                                                                              data_w_ids=train,
                                                                                              par_1=j,
                                                                                              par_2=k))
          
          #TRAIN MODEL
          set.seed(1)
          trained_model <- train(formula,
                                 data = train, 
                                 method = "glm",
                                 family = binomial(link="probit"),
                                 trControl =train_control,
                                 metric="weighed_tpr_ppv",
                                 preProc = c("center", "scale"),
                                 maximize=TRUE)
          
          
          #PREDICTION
          
          prediction <-predict(trained_model,test,type="prob") %>% 
            rowid_to_column("rowIndex") %>% 
            bind_cols("obs"=test$match)
          
          
          
          metrics <- test_weighed_tpr_ppv(prediction,
                                          weight=i,
                                          id_a="id_sample_A",
                                          id_b="id_sample_B",
                                          data_w_ids=test,
                                          par_1=j,
                                          par_2=k) 
          
          
          results_table_probit <- results_table_probit %>% 
            bind_rows(tibble(
              "strategy"=h,
              "weight"=i,
              "probability_threshold"=metrics[[5]],
              "probability_ratio"=metrics[[6]],
              "weighted_ppv_tpr"=metrics[[3]],
              "tpr"=metrics[[1]],
              "ppv"=metrics[[2]]
              
              
              
            ))
          
          
          
          
          print(j)
          
        }
        
        
        
      }else{
        
        folds=10
        cvIndex <- createFolds(factor(train$match), folds, returnTrain = T)
        
        
        #TRAIN CONTROL
        
        train_control <- trainControl(method="cv", 
                                      number=10, 
                                      index=cvIndex,
                                      savePredictions = TRUE,
                                      classProbs=TRUE,
                                      sampling=h,
                                      summaryFunction = function(...) train_weighed_tpr_ppv(...,
                                                                                            weight=i,
                                                                                            id_a="id_sample_A",
                                                                                            id_b="id_sample_B",
                                                                                            data_w_ids=train,
                                                                                            par_1=0.5,
                                                                                            par_2=k))
        
        #TRAIN MODEL
        set.seed(1)
        trained_model <- train(formula,
                               data = train, 
                               method = "glm",
                               family = binomial(link="probit"),
                               trControl =train_control,
                               metric="weighed_tpr_ppv",
                               preProc = c("center", "scale"),
                               maximize=TRUE
                               
        )
        
        
        prediction <-predict(trained_model,test,type="prob") %>% 
          rowid_to_column("rowIndex") %>% 
          bind_cols("obs"=test$match)
        
        
        
        
        metrics <- test_weighed_tpr_ppv(prediction,
                                        weight=i,
                                        id_a="id_sample_A",
                                        id_b="id_sample_B",
                                        data_w_ids=test,
                                        par_1=0.5,
                                        par_2=k)
        
        
        
        results_table_probit <- results_table_probit %>% 
          bind_rows(tibble(
            "strategy"=h,
            "weight"=i,
            "probability_threshold"=metrics[[5]],
            "probability_ratio"=metrics[[6]],
            "weighted_ppv_tpr"=metrics[[3]],
            "tpr"=metrics[[1]],
            "ppv"=metrics[[2]]
            
            
            
          ))
        
        
      }
      
      
      print(k)
      
    }
    
    
    print(i)
  }  
  
  print(h)
  
}




results_table_probit
# LOGIT -------------------------------------------------------------------



results_table_logit <- tibble()



for (h in c("optimal_cutoff","rose","smote","up","down")){
  
  #weight
  for (i in seq(0,1,0.5)){
    
    #probability ratio
    for (k in seq(1, 2, by = 0.5)){
      
      
      if(h=="optimal_cutoff"){
        
        
        #probability threshold
        for (j in seq(0.3, 0.9, by = 0.3)){
          
          
          folds=10
          cvIndex <- createFolds(factor(train$match), folds, returnTrain = T)
          
          #TRAIN CONTROL
          train_control <- trainControl(method="cv", 
                                        index=cvIndex,
                                        number=10, 
                                        savePredictions = TRUE,
                                        classProbs=TRUE,
                                        summaryFunction = function(...) train_weighed_tpr_ppv(...,weight=i,
                                                                                              id_a="id_sample_A",
                                                                                              id_b="id_sample_B",
                                                                                              data_w_ids=train,
                                                                                              par_1=j,
                                                                                              par_2=k))
          
          #TRAIN MODEL
          set.seed(1)
          trained_model <- train(formula,
                                 data = train, 
                                 method = "glm",
                                 family = binomial(link="logit"),
                                 trControl =train_control,
                                 metric="weighed_tpr_ppv",
                                 preProc = c("center", "scale"),
                                 maximize=TRUE)
          
          
          #PREDICTION
          
          prediction <-predict(trained_model,test,type="prob") %>% 
            rowid_to_column("rowIndex") %>% 
            bind_cols("obs"=test$match)
          
          
          
          metrics <- test_weighed_tpr_ppv(prediction,
                                          weight=i,
                                          id_a="id_sample_A",
                                          id_b="id_sample_B",
                                          data_w_ids=test,
                                          par_1=j,
                                          par_2=k) 
          
          
          results_table_logit <- results_table_logit %>% 
            bind_rows(tibble(
              "strategy"=h,
              "weight"=i,
              "probability_threshold"=metrics[[5]],
              "probability_ratio"=metrics[[6]],
              "weighted_ppv_tpr"=metrics[[3]],
              "tpr"=metrics[[1]],
              "ppv"=metrics[[2]]
              
              
              
            ))
          
          
          
          
          print(j)
          
        }
        
        
        
      }else{
        
        folds=10
        cvIndex <- createFolds(factor(train$match), folds, returnTrain = T)
        
        
        #TRAIN CONTROL
        
        train_control <- trainControl(method="cv", 
                                      number=10, 
                                      index=cvIndex,
                                      savePredictions = TRUE,
                                      classProbs=TRUE,
                                      sampling=h,
                                      summaryFunction = function(...) train_weighed_tpr_ppv(...,weight=i,
                                                                                            id_a="id_sample_A",
                                                                                            id_b="id_sample_B",
                                                                                            data_w_ids=train,
                                                                                            par_1=0.5,
                                                                                            par_2=k))
        
        #TRAIN MODEL
        set.seed(1)
        trained_model <- train(formula,
                               data = train, 
                               method = "glm",
                               family = binomial(link="logit"),
                               trControl =train_control,
                               metric="weighed_tpr_ppv",
                               preProc = c("center", "scale"),
                               maximize=TRUE
                               
        )
        
        
        prediction <-predict(trained_model,test,type="prob") %>% 
          rowid_to_column("rowIndex") %>% 
          bind_cols("obs"=test$match)
        
        
        
        
        metrics <- test_weighed_tpr_ppv(prediction,
                                        weight=i,
                                        id_a="id_sample_A",
                                        id_b="id_sample_B",
                                        data_w_ids=test,
                                        par_1=0.5,
                                        par_2=k)
        
        
        
        results_table_logit <- results_table_logit %>% 
          bind_rows(tibble(
            "strategy"=h,
            "weight"=i,
            "probability_threshold"=metrics[[5]],
            "probability_ratio"=metrics[[6]],
            "weighted_ppv_tpr"=metrics[[3]],
            "tpr"=metrics[[1]],
            "ppv"=metrics[[2]]
            
            
            
          ))
        
        
      }
      
      
      print(k)
      
    }
    
    
    print(i)
  }  
  
  print(h)
  
}


# XGBOOST -----------------------------------------------------------------

#VERSION NUEVA


results_table_xgboost <- tibble()



for (h in c("optimal_cutoff","rose","smote","up","down")){
  
  #weight
  for (i in seq(0,1,0.5)){
    
    #probability ratio
    for (k in seq(1, 2, by = 0.5)){
      
      
      if(h=="optimal_cutoff"){
        
        
        #probability threshold
        for (j in seq(0.3, 0.9, by = 0.3)){
          
          
          folds=10
          cvIndex <- createFolds(factor(train$match), folds, returnTrain = T)
          
          #TRAIN CONTROL
          train_control <- trainControl(method="cv", 
                                        number=10, 
                                        savePredictions = TRUE,
                                        classProbs=TRUE,
                                        summaryFunction = function(...) train_weighed_tpr_ppv(...,weight=i,
                                                                                              id_a="id_sample_A",
                                                                                              id_b="id_sample_B",
                                                                                              data_w_ids=train,
                                                                                              par_1=j,
                                                                                              par_2=k))
          
          
          grid_xgboost <- expand.grid(nrounds = c(250,500),
                                      max_depth = c(4,6,8),
                                      eta = c(0.01,0.3,0.5),
                                      gamma = c(0,1),
                                      min_child_weight = c(10, 25,50),
                                      colsample_bytree = c(0.7),
                                      subsample = c(0.6))
          
          
          
          #TRAIN MODEL
          set.seed(1)
          trained_model <- train(
            formula,
            data = train, 
            method = "xgbTree",
            trControl = train_control,
            metric = "weighed_tpr_ppv",
            tuneGrid = grid_xgboost,
            preProc = c("center", "scale"),
            maximize=TRUE,
            verbosity=0,
            allowParallel=T
            
          )
          
          
          #PREDICTION
          
          prediction <-predict(trained_model,test,type="prob") %>% 
            rowid_to_column("rowIndex") %>% 
            bind_cols("obs"=test$match)
          
          
          metrics <- test_weighed_tpr_ppv(prediction,
                                          weight=i,
                                          id_a="id_sample_A",
                                          id_b="id_sample_B",
                                          data_w_ids=test,
                                          par_1=j,
                                          par_2=k) 
          
          
          results_table_xgboost <- results_table_xgboost %>% 
            bind_rows(tibble(
              "strategy"=h,
              "weight"=i,
              "probability_threshold"=metrics[[5]],
              "probability_ratio"=metrics[[6]],
              "weighted_ppv_tpr"=metrics[[3]],
              "tpr"=metrics[[1]],
              "ppv"=metrics[[2]]
              
              
              
            ))
          
          
          
          
          print(j)
          
        }
        
        
        
      }else{
        
        folds=10
        cvIndex <- createFolds(factor(train$match), folds, returnTrain = T)
        
        
        #TRAIN CONTROL
        
        train_control <- trainControl(method="cv", 
                                      number=10, 
                                      index=cvIndex,
                                      savePredictions = TRUE,
                                      classProbs=TRUE,
                                      sampling=h,
                                      summaryFunction = function(...) train_weighed_tpr_ppv(...,weight=i,
                                                                                            id_a="id_sample_A",
                                                                                            id_b="id_sample_B",
                                                                                            data_w_ids=train,
                                                                                            par_1=j,
                                                                                            par_2=k))
        
        
        grid_xgboost <- expand.grid(nrounds = c(250,500),
                                    max_depth = c(4,6,8),
                                    eta = c(0.01,0.3,0.5),
                                    gamma = c(0,1),
                                    min_child_weight = c(10, 25,50),
                                    colsample_bytree = c(0.7),
                                    subsample = c(0.6))
        
        
        
        #TRAIN MODEL
        set.seed(1)
        trained_model <- train(
          formula,
          data = train, 
          method = "xgbTree",
          trControl = train_control,
          metric = "weighed_tpr_ppv",
          tuneGrid = grid_xgboost,
          preProc = c("center", "scale"),
          maximize=TRUE,
          verbosity=0,
          allowParallel=T
          
        )
        
        
        
        
        prediction <-predict(trained_model,test,type="prob") %>% 
          rowid_to_column("rowIndex") %>% 
          bind_cols("obs"=test$match)
        
        
        
        
        metrics <- test_weighed_tpr_ppv(prediction,
                                        weight=i,
                                        id_a="id_sample_A",
                                        id_b="id_sample_B",
                                        data_w_ids=test,
                                        par_1=0.5,
                                        par_2=k)
        
        
        
        results_table_xgboost <- results_table_xgboost %>% 
          bind_rows(tibble(
            "strategy"=h,
            "weight"=i,
            "probability_threshold"=metrics[[5]],
            "probability_ratio"=metrics[[6]],
            "weighted_ppv_tpr"=metrics[[3]],
            "tpr"=metrics[[1]],
            "ppv"=metrics[[2]]
            
            
            
          ))
        
        
      }
      
      
      print(k)
      
    }
    
    
    print(i)
  }  
  
  print(h)
  
}






results_table_xgboost



# Best model --------------------------------------------------------------

results <- results_table_xgboost %>% 
  mutate("algorithm"="XGBoost") %>% 
  bind_rows(
    results_table_logit%>% 
      mutate("algorithm"="Logit")
    
  ) %>% 
  
  bind_rows(
    results_table_probit%>% 
      mutate("algorithm"="Probit")
    
  ) %>% 
  arrange(desc(ppv)) %>% 
  filter(weighted_ppv_tpr>0.85)
  
results[1:8,] %>% 
  kable(.,escape = F,format="latex", booktabs=T) %>% 
  kable_styling()

