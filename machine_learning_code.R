#Set Random Seed
set.seed(2)
c1 <- Sys.time()

#############################################Load Packages################################3

library('ggplot2') # visualization

library('ggthemes') # visualization

library('scales') # visualization

library('dplyr') # data manipulation

library('Boruta') # Var Importance Check

library('mice') # imputation

library('randomForest') # classification algorithm

library('caret') # ml algorithm

library('cluster') # clustering

library('caretEnsemble') # Ensemble modeling

library('pROC') # AUC - ROC

library('doParallel') # parallel processing

library('DMwR') # smore

library('ROSE')

#set_working directory

setwd('C:/Users/p2s/Documents/ML')

#################################################Reading data#####################################

cs <- read.csv("train_ml_2.csv")

############################################Feature Engineering################################

cs$WC <- ((cs$IHP + cs$DSO - cs$DPO) * cs$netSales) / 365

cs$CIC <- (cs$tradeReceivables - cs$tradePayable)

###########################Search for NAs#####################################################

if (sum(is.na(cs)) != 0) {
  na_count <- sapply(cs, function(y)
    sum(length(which(is.na(
      y
    )))))
  nac <- as.data.frame(na_count)
  nac
} else{
  cat("BYE!")
}

# NA substitution


#NA handling
for (i in 1:ncol(cs)) {
  cs[is.na(cs[, i]), i] <- mean(cs[, i], na.rm = TRUE)
}


#######################################Train and test######################################

index <- sample(nrow(cs), floor(0.86 * nrow(cs)))

cs$Verdict <- as.factor(cs$Verdict)

train_cs <- cs[index, -c(1, 2)]

test_cs <- cs[-index, -c(1, 2)]


#############################################Sampling####################################

train_cs <-
  ovun.sample(
    Verdict ~ .,
    data = train_cs,
    method = "both",
    p = 0.5,
    N = 500,
    seed = 2
  )$data

###########################Build the model###################################################################



#############################################CV and train control###########################################

myControl <- trainControl(
  method = "repeatedcv",
  
  number = 10,
  
  repeats = 1,
  
  verboseIter = TRUE,
  
  returnResamp = "all"
  
)

######################################train the model###################################################

model_list <- c( "gbm", "rf", "glm", "nn", "c5")

method_list <- c("gbm", "rf", "glm", "nnet", "C5.0")

model_output_list <- vector("list", length(method_list))



  for (i in 1:length(model_list)){
    
    model_output_list[[i]] <- train(Verdict ~ ., data = train_cs,
                                  trControl = myControl, 
                                  method = method_list[i], preProcess = c("zv", "nzv",  "scale", "center", "YeoJohnson"))
      
  }


################################Cutoff Determination###############################

cutoff_determination <- function(x) {
  cutoff <- 0.1
  
  no_it  <- 100
  
  cutoff <- as.numeric(cutoff)
  
  no_it <- as.numeric(no_it)
  
  in_cutoff <- cutoff
  
  min_it_size <- 1 / no_it
  
  no_iter <- (1 - cutoff) * (1 / min_it_size)
  
  class_pred_test <- vector("numeric", length = nrow(test_cs))
  
  index_test <- 1:no_iter
  
  cutoff_vector <- vector("numeric", length = no_iter)
  
  cutoff_vector[1] <- in_cutoff
  
  
  for (i in 1:(no_iter - 1)){
    cutoff_vector[i + 1] <- cutoff_vector[i] + min_it_size
    
  }
  
  min_cutoff <- cutoff
  
  
  
  while (cutoff < 1) {
    for (j in 1:(no_iter)) {
      for (i in 1:nrow(test_cs)) {
        if (model_pred_list[[x]]$`1`[i] > cutoff) {
          class_pred_test[i] = 1
          
        } else{
          class_pred_test[i] = 0
          
        }
        
      }
      
      conf[[x]][[j]] <-
        confusionMatrix(class_pred_test, test_cs$Verdict)
      
      accuracy[[x]][j] <-
        (conf[[x]][[j]]$table[1, 1] + conf[[x]][[j]]$table[2, 2]) /
        (conf[[x]][[j]]$table[1, 1] + conf[[x]][[j]]$table[1, 2] +
           conf[[x]][[j]]$table[2, 1] + conf[[x]][[j]]$table[2, 2])
      
      bad_acc[[x]][j]  <-
        (conf[[x]][[j]]$table[2, 2]) / (conf[[x]][[j]]$table[1, 2] + conf[[x]][[j]]$table[2, 2])
      product[[x]][j] <- accuracy[[x]][j] * bad_acc[[x]][j]
      cutoff = cutoff + min_it_size
      
    }
    
    accuracy_df[[x]] <-
      data.frame(
        cbind(
          cutoff_vector,
          "accuracy" = accuracy[[x]],
          "bad_acc" = bad_acc[[x]],
          "product" = product[[x]],
          index_test
        )
      )
    
    return(accuracy_df[[x]])
    
  }
  
}


###########################################Variable Importance###########################################


var_imp <- function(x){

    imp <- varImp(x, useModel = TRUE)
  
}

imp_list <- lapply(model_output_list, var_imp)

names(imp_list) <- c("gbm", "rf", "glm", "nn", "c5")

n_cutoff = 0

m_cutoff = 0

conf <- vector("list", length(imp_list))

accuracy <- vector("list", length(imp_list))

bad_acc <- vector("list", length(imp_list))

product <- vector("list", length(imp_list))

accuracy_df <- vector("list", length(imp_list))

n_fin_df <-  vector("list", length(imp_list))

result_cutoff_vector_total <- vector("numeric", length(imp_list))

result_cutoff_vector_bad <- vector("numeric", length(imp_list))

result_cutoff_vector_product <- vector("numeric", length(imp_list))

model_names <- c( "gbm", "rf", "glm", "nn", "c5")

model_output_list_1 <- vector("list", length(model_names))

model_pred_list <- vector("list", length(model_names))

names(result_cutoff_vector_total) <-  model_names

names(result_cutoff_vector_bad) <- model_names

names(result_cutoff_vector_product) <- model_names

imp_list_names <- vector("list", length(imp_list))

while (n_cutoff < 100 & m_cutoff <= 0.4) {
  for (i in 1:length(imp_list)) {
    var_names <-
      rownames(subset(
        imp_list[[i]]$importance,
        imp_list[[i]]$importance$Overall >= n_cutoff
      ))
    
    
    imp_list_names[[i]] <- var_names[!is.na(var_names)]
  }

  for(i in 1:length(model_list)){
    model_output_list_1[[i]] <- train(as.formula(paste("Verdict ~", paste(imp_list_names[[i]], collapse="+"))), data = train_cs,
                                    trControl = myControl, 
                                    method = method_list[i], preProcess = c("zv", "nzv",  "scale", "center", "YeoJohnson"))
    
  }
  
  for(i in 1:length(model_list)){
    
    model_pred_list[[i]] <- predict.train(model_output_list[[i]], newdata=test_cs, type='prob')
    
  }
  
  
  n_cutoff = n_cutoff + 5
  
  result_cutoff <- vector("list", length(model_list))
  
  for (i in 1:length(result_cutoff)) {
    result_cutoff[[i]] <- cutoff_determination(i)
    
  }
  
  m = 0
  n = 0
  
  for (j in 1:length(result_cutoff)) {
    while (nrow(n_fin_df[[j]]) == 0 || is.null(nrow(n_fin_df[[j]]))) {
      n_fin_df[[j]] <-
        subset(
          result_cutoff[[j]],
          result_cutoff[[j]]$product >= 0.64 - n &
            result_cutoff[[j]]$bad_acc >= 0.8 - n
        )
      
      n <- n + 0.01
      
    }
    
    
    for (j in 1:length(result_cutoff_vector_total)) {
      result_cutoff_vector_total[j] <- max(n_fin_df[[j]][, 2])
      
    }
    
    for (j in 1:length(result_cutoff_vector_bad)) {
      result_cutoff_vector_bad[j] <- max(n_fin_df[[j]][, 3])
      
    }
    
    for (j in 1:length(result_cutoff_vector_bad)) {
      result_cutoff_vector_product[j] <- max(n_fin_df[[j]][, 4])
      
    }
    
  }
  if (max(result_cutoff_vector_product) > 0.64 - m_cutoff &
      max(result_cutoff_vector_bad) > 0.8 - m_cutoff &
      max(result_cutoff_vector_total) > 0.8 - m_cutoff) {
    break
    
  }
  
  m_cutoff =  m_cutoff + 0.01
  
}

c2 <- Sys.time()

c2 - c1

if ( max(result_cutoff_vector_product) >= 0.42 &
     max(result_cutoff_vector_bad) >= 0.7 &
     max(result_cutoff_vector_total) >= 0.7) {
  result_cutoff_vector_total_subset <-
    result_cutoff_vector_total[result_cutoff_vector_total > 0.6]
  
  result_cutoff_vector_bad_subset <-
    result_cutoff_vector_bad[result_cutoff_vector_bad > 0.7]
  
  result_cutoff_vector_product_subset <-
    result_cutoff_vector_product[result_cutoff_vector_product > 0.42]
  
  name_final_models <- Reduce(intersect,
                              list(
                                names(result_cutoff_vector_product_subset),
                                names(result_cutoff_vector_bad_subset),
                                names(result_cutoff_vector_total_subset)
                              ))
  
}



model_names_re_train <- which(model_names %in% name_final_models)


#############################################Find Individual Cutoffs and accuracies########################

# Subsetting based on product

n_fin_df_subs <- vector("list", length(model_names_re_train))

cutoffs <- vector("numeric", length(model_names_re_train))

accuracies <- vector("numeric", length(model_names_re_train))



names(cutoffs) <- model_names[model_names_re_train]

names(accuracies) <- model_names[model_names_re_train]

j=0

for (i in model_names_re_train) {
  
  j=j+1
  
  n_fin_df_subs[[i]] <-
    subset(n_fin_df[[i]], n_fin_df[[i]]$product > 0.42 &
             n_fin_df[[i]]$accuracy > 0.6, n_fin_df[[i]]$bad_acc > 0.7)
  
  n_fin_df_subs[[i]] <-
    n_fin_df_subs[[i]][order(n_fin_df_subs[[i]]$product), ]
  
  cutoffs[[j]] <- tail(n_fin_df_subs[[i]]$cutoff_vector, 1)
  
  accuracies[[j]] <-
    max(subset(n_fin_df_subs[[i]], n_fin_df_subs[[i]]$cutoff_vector == cutoffs[[j]])$product)
  
}


#ENSEMBLE MODELLING

n <- length(model_names_re_train)

if (n > 1) {
  sum <- vector("numeric", nrow(test_cs))
  
  
  
  for(j in 1:nrow(test_cs)){
    for (i in model_names_re_train) {
     
       sum[j] <- sum[j] + model_pred_list[[i]]$`1`[j]
      
    }
  }
  
  average_final_ensemble <- sum/n

  #ACCURACY OF ENSEMBLE MODELS
  
  model_list_ensemble <-
    list(a = average_final_ensemble)
  
  result_cutoff_ensemble <-
    vector("list", length(model_list_ensemble))
  
  for (i in 1:length(result_cutoff_ensemble)) {
    result_cutoff_ensemble[[i]] <- cutoff_determination_2(i)
    
  }
  
  
  
  n_fin_df_ensemble <-  vector("list", length(model_list_ensemble))
  
  result_cutoff_vector_total_ensemble <-
    vector("numeric", length(model_list_ensemble))
  
  result_cutoff_vector_bad_ensemble <-
    vector("numeric", length(model_list_ensemble))
  
  result_cutoff_vector_product_ensemble <-
    vector("numeric", length(model_list_ensemble))
  
  for (j in 1:length(result_cutoff_ensemble)) {
    while (nrow(n_fin_df_ensemble[[j]]) == 0 ||
           is.null(nrow(n_fin_df_ensemble[[j]]))) {
      n_fin_df_ensemble[[j]] <-
        subset(
          result_cutoff_ensemble[[j]],
          result_cutoff_ensemble[[j]]$accuracy >= 0.8 - n &
            result_cutoff_ensemble[[j]]$bad_acc >= 0.8 - n
        )
      
      n <- n + 0.01
      
    }
    
    for (j in 1:length(result_cutoff_vector_total_ensemble)) {
      result_cutoff_vector_total_ensemble[j] <-
        max(n_fin_df_ensemble[[j]][, 2])
      
    }
    
    for (j in 1:length(result_cutoff_vector_bad_ensemble)) {
      result_cutoff_vector_bad_ensemble[j] <-
        max(n_fin_df_ensemble[[j]][, 3])
      
    }
    
    for (j in 1:length(result_cutoff_vector_bad_ensemble)) {
      result_cutoff_vector_product_ensemble[j] <-
        max(n_fin_df_ensemble[[j]][, 4])
      
    }
    
  }
  
  
  #CHECK IF THE ENSEMBLE IS BETTER THAN THE INDIVIDUAL MODELS
  
  n_fin_df_subs_ensemble <-
    vector("list", length(model_list_ensemble))
  
  cutoffs_ensemble <- vector("numeric", length(model_list_ensemble))
  
  accuracies_ensemble <-
    vector("numeric", length(model_list_ensemble))
  
  names(cutoffs_ensemble) <- c("average")
  
  names(accuracies_ensemble) <- c("average")
  
  for (i in 1:length(model_list_ensemble)) {
    n_fin_df_subs_ensemble[[i]] <-
      subset(n_fin_df_ensemble[[i]], n_fin_df_ensemble[[i]]$product > 0.42)
    
    n_fin_df_subs_ensemble[[i]] <-
      n_fin_df_subs_ensemble[[i]][order(n_fin_df_subs_ensemble[[i]]$product), ]
    
    cutoffs_ensemble[[i]] <-
      tail(n_fin_df_subs_ensemble[[i]]$cutoff_vector, 1)
    
    accuracies_ensemble[[i]] <-
      max(
        subset(
          n_fin_df_subs_ensemble[[i]],
          n_fin_df_subs_ensemble[[i]]$cutoff_vector == cutoffs_ensemble[[i]]
        )$product
      )
    
  }
  
}


if(max(accuracies_ensemble) >= max(accuracies)){
  
  flag_for_ensemble_check <- "Y"
  
}else{
  
  flag_for_ensemble_check <- "N"
  
}

######################################RE-TRAINING AND PREDICTING##########################################################



if (flag_for_ensemble_check == "N") {

  n <- which.max(accuracies)
  
  final_model_output <- train(as.formula(paste("Verdict ~", paste(imp_list_names[[n]], collapse="+"))), data = cs,
                                    trControl = myControl, 
                                    method = method_list[n], preProcess = c("zv", "nzv",  "scale", "center", "YeoJohnson"))
  
  pred_final_model_output <- predict.train(final_model_output, newdata = test_new_request, type = 'prob')
  
}else{
  
  final_model_output <- vector("list", length(model_names_re_train))
  pred_final__model_output <- vector("list", length(model_names_re_train))
  
  for(i in 1:model_names_re_train){
    
    
    final_model_output[[i]] <- train(as.formula(paste("Verdict ~", paste(imp_list_names[[i]], collapse="+"))), data = cs,
                                     trControl = myControl, 
                                     method = method_list[i], preProcess = c("zv", "nzv",  "scale", "center", "YeoJohnson"))
    
    pred_final_model_output[[i]] <- predict.train(final_model_output[[i]], newdata = test_new_request, type = 'prob')
    
  }
  
  
}


