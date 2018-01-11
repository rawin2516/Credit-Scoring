#Set Random Seed
set.seed(2)


#############################################################################################################
#                                                                                                           #
#                                        LOAD REQUIRED PACKAGES                                             #
#                                                                                                           #
#############################################################################################################

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
#############################################################################################################
#                                                                                                           #
#                                         READING DATA                                                      #
#                                                                                                           #
#############################################################################################################

cs <- read.csv("train_ml_2.csv") #reading from a csv file


#######################################Train and test######################################


  index <- sample(nrow(cs), floor(0.85 * nrow(cs)))
  
  cs$Verdict <- as.factor(cs$Verdict)
  
  train_cs <- cs[index, -c(1, 2)]
  
  test_cs <- cs[-index, -c(1, 2)]


###########################Search for NAs#####################################################  

cs.miss.model <- preProcess(train_cs, method = c("center", "scale", "YeoJohnson", "knnImpute")) # Pre processing the data with centering, scaling, transforming each variab and imputing missing values
  
train_cs_t <- predict(cs.miss.model, train_cs) #Applying the preprocessing model on the training set   
  
test_cs_t <- predict(cs.miss.model, test_cs)  #Applying the preprocessing model on the test set 

#############################################Sampling####################################

##########check if sampling is required###################


classification_balance <- as.data.frame(table(train_cs_t$Verdict)) #check the number of 1s and 0s in the dataset

num_0 <- classification_balance$Freq[classification_balance$Var1 == 0] 
num_1 <- classification_balance$Freq[classification_balance$Var1 == 1]

classification_balance_percentages <- c(num_0/(num_0 + num_1) , num_1/(num_0 + num_1)) #percentage of each classification bin in the dataset

#Resampling based on the criteria if one bin outweighs the other by 10%

if(abs(classification_balance_percentages[1] - classification_balance_percentages[2]) >= 0.1){

train_cs_t <-
  ovun.sample(
    Verdict ~ .,
    data = train_cs_t,
    method = "both",
    p = 0.5,
    N = 500,
    seed = 2
  )$data

}

#############################################################################################################
#                                                                                                           #
#                                         BUILD THE MODEL                                                   #
#                                                                                                           #
#############################################################################################################

#############################################CV and train control###########################################



myControl <- trainControl(
  method = "repeatedcv", #Repeated Cross-Validation
  
  number = 10, # 10 folds
  
  repeats = 1, # No repeats
  
  verboseIter = TRUE,
  
  returnResamp = "all"
  
)

######################################train the model###################################################



  model_list <- c( "gbm", "rf", "glm", "nn", "c5") #Variable names of models
  
  method_list <- c("gbm", "rf", "glm", "nnet", "C5.0") # Actual model names
  
  model_output_list <- vector("list", length(method_list))
  
  ####Loop to to train each model#######
  
    for (i in 1:length(model_list)){
      
      model_output_list[[i]] <- train(as.factor(Verdict) ~ ., data = train_cs_t,
                                    trControl = myControl, 
                                    method = method_list[i])
        
    }


################################Cutoff Determination###############################

cutoff_determination <- function(x) {

  cutoff <- 0.1 #initialising a value for  cutoff
  
  no_it  <- 100 
  
  cutoff <- as.numeric(cutoff)
  
  no_it <- as.numeric(no_it)
  
  in_cutoff <- cutoff
  
  min_it_size <- 1 / no_it #step size
  
  no_iter <- (1 - cutoff) * (1 / min_it_size) #number of steps
  
  class_pred_test <- vector("numeric", length = nrow(test_cs)) #initializing a vector for storing predicted values
  
  index_test <- 1:no_iter
  
  cutoff_vector <- vector("numeric", length = no_iter)
  
  cutoff_vector[1] <- in_cutoff
  
  
  for (i in 1:(no_iter - 1)){
    cutoff_vector[i + 1] <- cutoff_vector[i] + min_it_size
    
  }
  
  min_cutoff <- cutoff
  
  x=1
  
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
        confusionMatrix(class_pred_test, test_cs$Verdict) #confusion matrix at each iteration
      
      accuracy[[x]][j] <-
        (conf[[x]][[j]]$table[1, 1] + conf[[x]][[j]]$table[2, 2]) /
        (conf[[x]][[j]]$table[1, 1] + conf[[x]][[j]]$table[1, 2] +
           conf[[x]][[j]]$table[2, 1] + conf[[x]][[j]]$table[2, 2]) #overall accuracy
      
      bad_acc[[x]][j]  <-
        (conf[[x]][[j]]$table[2, 2]) / (conf[[x]][[j]]$table[1, 2] + conf[[x]][[j]]$table[2, 2]) #Accuracy of predicting defauls
     
      area_roc[[x]][j] <- as.numeric(roc(test_cs$Verdict, class_pred_test)$auc) # AUC ROC values
      
      cutoff = cutoff + min_it_size
      
    }
    
    accuracy_df[[x]] <-
      data.frame(
        cbind(
          cutoff_vector,
          "accuracy" = accuracy[[x]],
          "bad_acc" = bad_acc[[x]],
          "area_roc" = area_roc[[x]],
          index_test
        )
      ) #creating a data-frame with the three metrics as our columns
    
    return(accuracy_df[[x]])
    
  }
  
}


###########################################Variable Importance###########################################


  #varImp calucaltes the influence of each variable on prediction
  
  var_imp <- function(x){
  
      imp <- varImp(x, useModel = TRUE)
    
  } 
  
  imp_list <- lapply(model_output_list, var_imp) #applying the var_imp function on each model
  
  names(imp_list) <- c("gbm", "rf", "glm", "nn", "c5") # initialising the names for the list of variable importances of each model
  
  n_cutoff = 0 #n_cutoff is used to recursively incerase the cutoff of the variable importance score (after starting with 0 i.e. all variables are included)
  
  m_cutoff = 0 #m_cutoff is used to recursively increase the curoff of the probability of default which gives the best results
  
  conf <- vector("list", length(imp_list)) #confusion matrix list for each iteration
  
  accuracy <- vector("list", length(imp_list)) # accuracy of the confusion matrices
  
  bad_acc <- vector("list", length(imp_list)) # accuracy of predicting delinquencies 
  
  area_roc <- vector("list", length(imp_list)) # AUC - ROC values of each model at each iteration
  
  accuracy_df <- vector("list", length(imp_list))
  
  n_fin_df <-  vector("list", length(imp_list))
  
  result_cutoff_vector_total <- vector("numeric", length(imp_list))
  
  result_cutoff_vector_bad <- vector("numeric", length(imp_list))
  
  result_cutoff_vector_area_roc <- vector("numeric", length(imp_list))
  
  model_names <- c( "gbm", "rf", "glm", "nn", "c5")
  
  model_output_list_1 <- vector("list", length(model_names))
  
  model_pred_list <- vector("list", length(model_names))
  
  names(result_cutoff_vector_total) <-  model_names
  
  names(result_cutoff_vector_bad) <- model_names
  
  names(result_cutoff_vector_area_roc) <- model_names
  
  imp_list_names <- vector("list", length(imp_list))



#######################Choosing the best model##########################


# Loop over both the variable importance score cutoff and the probabiltiy of default curoff
  
while (n_cutoff < 100 & m_cutoff <= 0.4) {
  for (i in 1:length(imp_list)) {
    var_names <-
      rownames(subset(
        imp_list[[i]]$importance,
        imp_list[[i]]$importance$Overall >= n_cutoff #subsetting the variables based on their importance score i.e. n_cutoff
      ))
    
    
    imp_list_names[[i]] <- var_names[!is.na(var_names)]
  }

  for(i in 1:length(model_list)){
    model_output_list_1[[i]] <- train(as.formula(paste("as.factor(Verdict) ~", paste(imp_list_names[[i]], collapse="+"))), data = train_cs_t,
                                    trControl = myControl, 
                                    method = method_list[i]) # training each model with different variables at each step based on the value of n_cutoff
    
  }
  
  for(i in 1:length(model_list)){
    
    model_pred_list[[i]] <- predict(model_output_list[[i]], newdata=test_cs, type='prob') #predicting each model on the test set
    
  }
  
  
  n_cutoff = n_cutoff + 5 #incrementing n_cutoff so that in the next step the variables are subsetted on the basis of a score greater than the previous score by 5
  
  result_cutoff <- vector("list", length(model_list))
  
  # checking for the optimum probability of default cut off
  
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
          result_cutoff[[j]]$area_roc >= 0.64 - n &
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
      result_cutoff_vector_area_roc[j] <- max(n_fin_df[[j]][, 4])
      
    }
    
  }
  
  if (max(result_cutoff_vector_area_roc) > 0.64 - m_cutoff &
      max(result_cutoff_vector_bad) > 0.8 - m_cutoff &
      max(result_cutoff_vector_total) > 0.8 - m_cutoff) {
    break
    
  }
  
  m_cutoff =  m_cutoff + 0.01
  
}

# Subsetting the models based on the performance metrics
  
if ( max(result_cutoff_vector_area_roc) >= 0.6 &
     max(result_cutoff_vector_bad) >= 0.7 &
     max(result_cutoff_vector_total) >= 0.65) {
  result_cutoff_vector_total_subset <-
    result_cutoff_vector_total[result_cutoff_vector_total > 0.6]
  
  result_cutoff_vector_bad_subset <-
    result_cutoff_vector_bad[result_cutoff_vector_bad > 0.7]
  
  result_cutoff_vector_area_roc_subset <-
    result_cutoff_vector_area_roc[result_cutoff_vector_area_roc > 0.42]
  
  name_final_models <- Reduce(intersect,
                              list(
                                names(result_cutoff_vector_area_roc_subset),
                                names(result_cutoff_vector_bad_subset),
                                names(result_cutoff_vector_total_subset)
                              ))
  
}

model_names_re_train <- which(model_names %in% name_final_models)



#############################################Find Individual model Cutoffs of the chosen ones########################


  
  n_fin_df_subs <- vector("list", length(model_names_re_train))
  
  cutoffs <- vector("numeric", length(model_names_re_train))
  
  accuracies <- vector("numeric", length(model_names_re_train))
  
  
  
  names(cutoffs) <- model_names[model_names_re_train]
  
  names(accuracies) <- model_names[model_names_re_train]
  
  j=0
  
  for (i in model_names_re_train) {
    
    j=j+1
    
    n_fin_df_subs[[i]] <-
      subset(n_fin_df[[i]], n_fin_df[[i]]$area_roc > 0.42 &
               n_fin_df[[i]]$accuracy > 0.6, n_fin_df[[i]]$bad_acc > 0.7)
    
    n_fin_df_subs[[i]] <-
      n_fin_df_subs[[i]][order(n_fin_df_subs[[i]]$area_roc), ]
    
    cutoffs[[j]] <- tail(n_fin_df_subs[[i]]$cutoff_vector, 1)
    
    accuracies[[j]] <-
      max(subset(n_fin_df_subs[[i]], n_fin_df_subs[[i]]$cutoff_vector == cutoffs[[j]])$area_roc)
    
  }


###################ENSEMBLE MODELLING############################333333
  
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
  
  result_cutoff_vector_area_roc_ensemble <-
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
      result_cutoff_vector_area_roc_ensemble[j] <-
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
      subset(n_fin_df_ensemble[[i]], n_fin_df_ensemble[[i]]$area_roc > 0.42)
    
    n_fin_df_subs_ensemble[[i]] <-
      n_fin_df_subs_ensemble[[i]][order(n_fin_df_subs_ensemble[[i]]$area_roc), ]
    
    cutoffs_ensemble[[i]] <-
      tail(n_fin_df_subs_ensemble[[i]]$cutoff_vector, 1)
    
    accuracies_ensemble[[i]] <-
      max(
        subset(
          n_fin_df_subs_ensemble[[i]],
          n_fin_df_subs_ensemble[[i]]$cutoff_vector == cutoffs_ensemble[[i]]
        )$area_roc
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
  
  pred_final_model_output <- predict(final_model_output, newdata = test_new_request, type = 'prob')
  
}else{
  
  final_model_output <- vector("list", length(model_names_re_train))
  pred_final__model_output <- vector("list", length(model_names_re_train))
  
  for(i in 1:model_names_re_train){
    
    
    final_model_output[[i]] <- train(as.formula(paste("Verdict ~", paste(imp_list_names[[i]], collapse="+"))), data = cs,
                                     trControl = myControl, 
                                     method = method_list[i])
    
    pred_final_model_output[[i]] <- predict(final_model_output[[i]], newdata = test_new_request, type = 'prob')
    
  }
  
  
}






