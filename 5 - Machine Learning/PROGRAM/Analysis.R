## start
# Load:
options(scipen=999)
rm(list=ls())
Start = Sys.time()
# 0a - Install and load required packages
pkgs <- c("tidyverse","openxlsx","stringr","lubridate","magrittr", "haven","rstudioapi","readxl","ggrepel","plyr", "dplyr", "mosaic", "class", "glmnet", "MASS", "splitstackshape", "rms", "caret", "nnet", "fastDummies", "clusterSim", "randomForest", "xgboost", "e1071")
# Install: - should not need to re-install if already installed
# install.packages(pkgs)
# Load:
lapply(pkgs, require, character.only = TRUE)
###############################################
###             Read-ins and Paths          ###
###############################################
Input <- 'C:/Users/Talla/Documents/Projects/FIFA Match Win Prediction/4 - Combine Data Sources/OUTPUT/'
Output <- 'C:/Users/Talla/Documents/Projects/FIFA Match Win Prediction/5 - Machine Learning/OUTPUT/'

###############################################
###             Run Analysis                ###
###############################################
Intial_df <- readRDS(file = paste0(Input,"Final.rds"))

if(FALSE) {
Intial_df <- Intial_df %>% 
  filter(league_name_h == "English Premier League")
}


Intial_df$Result = factor(Intial_df$Result,
                          order = TRUE,
                   levels = c('Lose', 'Draw', 'Win'))
Intial_df$Match_Combo = make.names(Intial_df$Match_Combo)


#Intial_df <- Intial_df %>%
#  mutate(Time_Season_FE = paste0(league_name_a, Game_Date))
#Intial_df$Time_Season_FE = factor(Intial_df$Time_Season_FE,
#                           levels = unique(Intial_df$Time_Season_FE),
#                           exclude = NULL)mnklo

Keep <- Intial_df %>%
  count(Match_Combo, sort = TRUE) %>%
  filter(n > 3) %>%
  dplyr::select(Match_Combo)


Intial_df <- inner_join(Intial_df, Keep, by = c("Match_Combo"))


Intial_df$Match_Combo = factor(Intial_df$Match_Combo,
                           levels = unique(Intial_df$Match_Combo),
                           exclude = NULL)

Intial_df <- Intial_df %>%
  fastDummies::dummy_cols(select_columns = "Match_Combo") %>%
  dplyr::select(-c(Match_Combo))

PredictorVariables <- Intial_df %>% 
  dplyr::select(c(age_Att:Def_Work_Rate_Ind_Mid, starts_with('Match_Combo')))%>%
  names( )


Formula <- formula(paste("Class ~ ", 
                         paste(PredictorVariables, collapse=" + ")))




set.seed(123)
train_separator = (Intial_df$Game_Date == '20-21')
Training_df <- Intial_df[!train_separator,]
Testing_df <- Intial_df[train_separator,]

Actual_Test_Results <- Testing_df[['Result']]
Total_observations <- length(Actual_Test_Results)


Balanced_Training <- upSample(x = Training_df[,10:length(Training_df)], y = Training_df$Result)
Train_Results <- Balanced_Training[['Class']]



saveRDS(Balanced_Training, file = paste0(Output,"Balanced Training.rds"))
saveRDS(Testing_df, file = paste0(Output,"Testing.rds"))


#KNN
KNN_Train <- Balanced_Training %>%
  dplyr::select(c(overall_Att:defending_Mid, Class)) 

KNN_Test <- Testing_df %>%
  dplyr::select(overall_Att:defending_Mid)

PredictorVariables_knn <- Balanced_Training %>% 
  dplyr::select(c(overall_Att:defending_Mid))%>%
  names( )
Formula_KNN <- formula(paste("Class ~ ", 
                         paste(PredictorVariables_knn, collapse=" + ")))



set.seed(123)
ctrl <- trainControl(method="repeatedcv",repeats = 3)
knnFit <- train(Formula_KNN, data = KNN_Train, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
knnFit

Best_K <- as.integer(knnFit$bestTune)

KNN_Train <- Balanced_Training %>%
  dplyr::select(overall_Att:defending_Mid)



knn_results = knn(KNN_Train, KNN_Test, Train_Results ,k=Best_K)
table(Actual_Test_Results,knn_results)
KNN_accuracy <- sum(diag(table(Actual_Test_Results,knn_results)))/Total_observations






#Ordered & Multinomial Logit 
Ordered_Logit <- orm(Formula, data = Balanced_Training, maxit= 150)
Multi_nLogit <- multinom(Formula, data = Balanced_Training, maxit = 250, MaxNWts= 40000)

Olog_probs_m <- predict(Ordered_Logit ,Testing_df , type="fitted.ind")
Mlog_probs <- predict(Multi_nLogit ,Testing_df , type="class")

Olog_probs =rep ("Lose" ,dim(Testing_df)[1])
Olog_probs[Olog_probs_m[,1] < Olog_probs_m[,2] & Olog_probs_m[,2] > Olog_probs_m[,3]]="Draw"
Olog_probs[Olog_probs_m[,3] > Olog_probs_m[,2] & Olog_probs_m[,3] > Olog_probs_m[,1]]="Win"




# LDA
LDA <- lda(Formula, data = Balanced_Training)
LDA_probs_m <- predict(LDA , Testing_df)
LDA_probs <- LDA_probs_m$class






Temp1 <- table(Actual_Test_Results,Olog_probs)
Ologit_accuracy <- sum(Temp1['Lose', 'Lose'] + Temp1['Win', 'Win'] + Temp1['Draw', 'Draw'])/Total_observations


table(Actual_Test_Results,Mlog_probs)
Mlogit_accuracy <- sum(diag(table(Actual_Test_Results,Mlog_probs)))/Total_observations


table(Actual_Test_Results,LDA_probs)
LDA_accuracy <- sum(diag(table(Actual_Test_Results,LDA_probs)))/Total_observations





#Lasso
X_Shrinkage <- Balanced_Training %>%
  dplyr::select(-c(Class)) %>%
  data.matrix()

Test_Shrinkage <- Testing_df %>%
  data.matrix()
Test_Shrinkage <- Test_Shrinkage[,10:ncol(Test_Shrinkage)]

Lasso <- cv.glmnet(x=X_Shrinkage, y=Train_Results, alpha=1, family="multinomial")
Lasso_probs <- predict(Lasso, newx = Test_Shrinkage, s = "lambda.min", type = "class")
Temp2 <- table(Actual_Test_Results,Lasso_probs)
Lasso_accuracy <- sum(Temp2['Lose', 'Lose'] + Temp2['Win', 'Win'] + Temp2['Draw', 'Draw'])/Total_observations





#Elastic Net

E_Net <- cv.glmnet(x=X_Shrinkage, y=Train_Results, alpha=0.5, family="multinomial")
E_Net_probs <- predict(E_Net, newx = Test_Shrinkage, s = "lambda.min", type = "class")
Temp3 <- table(Actual_Test_Results,E_Net_probs)
ElasticNet_accuracy <- sum(Temp3['Lose', 'Lose'] + Temp3['Win', 'Win'] + Temp3['Draw', 'Draw'])/Total_observations




#Random Forest
set.seed(123)
rf = randomForest(Formula,data=Balanced_Training , importance =TRUE, ntree = (ncol(Balanced_Training)-1)*10)
rf_class <- predict(rf ,Testing_df , type="class")
table(Actual_Test_Results,rf_class)
RF_accuracy <- sum(diag(table(Actual_Test_Results,rf_class)))/Total_observations






#Boosted Trees
Int_Train_Result = as.integer(Train_Results)-1
num_class = length(levels(Train_Results))


hyper_grid <- expand.grid(
  eta = c(.01, .1, .3),
  max_depth = c( 3, 5, 7),
  min_child_weight = c( 3, 5, 7),
  subsample = c(.65, .8, 1), 
  colsample_bytree = c(.8, 1),
  optimal_trees = 0,               
  min_RMSE = 0                     
)


#hyper_grid <- expand.grid(
#  eta = c( .1, .3),
#  max_depth = c(1, 7),
#  min_child_weight = c(1, 7),
#  subsample = c(.65,  1), 
#  colsample_bytree = c(.8,  1),
#  optimal_trees = 0,               
#  min_RMSE = 0                     
#)


for(i in 1:nrow(hyper_grid)) {
  
  # create parameter list
  params <- list(
    eta = hyper_grid$eta[i],
    max_depth = hyper_grid$max_depth[i],
    min_child_weight = hyper_grid$min_child_weight[i],
    subsample = hyper_grid$subsample[i],
    colsample_bytree = hyper_grid$colsample_bytree[i] )

  
  set.seed(123)
  xgb.tune <- xgb.cv(
    params = params,
    data = X_Shrinkage,
    label = Int_Train_Result,
    nrounds = 2000,
    nfold = 5,
    objective = "multi:softprob",
    eval_metric = "mlogloss",
    num_class = num_class,  # for regression models
    verbose = 0,               # silent,
    early_stopping_rounds = 10, # stop if no improvement for 10 consecutive trees
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_mlogloss_mean)
  hyper_grid$min_RMSE[i] <- min(xgb.tune$evaluation_log$test_mlogloss_mean)
}

hyper_grid <- hyper_grid %>%
  dplyr::arrange(min_RMSE)
  

set.seed(123)
xgb_train = xgb.DMatrix(data=X_Shrinkage,label=Int_Train_Result)


params <- list(
  eta = hyper_grid$eta[1],
  max_depth = hyper_grid$max_depth[1],
  min_child_weight = hyper_grid$min_child_weight[1],
  subsample = hyper_grid$subsample[1],
  colsample_bytree = hyper_grid$colsample_bytree[1] )


xgb_fit=xgboost(
  params=params,
  data = xgb_train,
  nrounds=hyper_grid$optimal_trees[1],
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = num_class, 
  early_stopping_rounds=10,
  verbose=1
)

Testing_m_matrix <- as.matrix(Testing_df[,10:ncol(Testing_df)])
xgb_pred = predict(xgb_fit, Testing_m_matrix,reshape=T)
colnames(xgb_pred) = levels(Train_Results)


Boosted_probs =rep ("Lose" ,dim(xgb_pred)[1])
Boosted_probs[xgb_pred[,'Lose'] < xgb_pred[,'Draw'] & xgb_pred[,'Draw'] > xgb_pred[,'Win']]="Draw"
Boosted_probs[xgb_pred[,'Win'] > xgb_pred[,'Draw'] & xgb_pred[,'Win'] > xgb_pred[,'Lose']]="Win"

Temp4 <- table(Actual_Test_Results,Boosted_probs)
BoostedF_accuracy <- sum(Temp4['Lose', 'Lose'] + Temp4['Win', 'Win'] + Temp4['Draw', 'Draw'])/Total_observations







#Support Vector Machine One vs. One
set.seed(123)
tune_out=tune(svm , Formula, data=Balanced_Training, kernel ="radial",
              ranges =list(cost=c(10, 250, 500,750,1000, 1500),
                           gamma=c(0.25,1,3,5, 10) ))

b_gamma = 3
b_cost = 10


SVM_predi <- Balanced_Training %>%
  dplyr::select(-c(starts_with('Match_Combo'), Class)) %>%
  names()
Formula_SVM<- formula(paste("Class ~ ", 
                                   paste(SVM_predi, collapse=" + ")))

set.seed(123)
svm_fit = svm(Formula_SVM, data=Balanced_Training, kernel ="radial",cost =b_cost, gamma =b_gamma, fitted = TRUE)
svm_pred <- predict(svm_fit, Testing_m_matrix)
SVM_accuracy <- sum(diag(table(Actual_Test_Results,svm_pred)))/Total_observations
table(Actual_Test_Results,svm_pred)


K <- Balanced_Training %>%
  dplyr::select(Class) %>%
  dplyr::mutate(N = 1) %>%
  dplyr::group_by(Class) %>%
  dplyr::summarise(sum1 = sum(N))
  



#Principal Component
Continuous_Variables <- Balanced_Training %>%
  dplyr::select(-c(starts_with('Match_Combo'), Class))

Fixed_Effects_train <- Balanced_Training %>%
  dplyr::select(c(starts_with('Match_Combo')))

Results_train <- Balanced_Training %>%
  dplyr::select(c(Class))

corr <- cor(Continuous_Variables, method="pearson")
corrplot::corrplot(corr, method= "color", order = "hclust", tl.pos = 'n')


PCA <- prcomp(Continuous_Variables, center=TRUE, scale.=TRUE)
PCA_train_df <- as.data.frame(PCA$x)
summary(PCA)


Percent_Variation_Expl <- PCA$sdev^2/sum(PCA$sdev^2)
Percent_Variation_Expl

Sum_Var = 0
for (k in 1:length(Percent_Variation_Expl)){
  Sum_Var <- sum(Percent_Variation_Expl[k] + Sum_Var)
  if (Sum_Var < 0.99) {
    Number_PCs <-  k
  }
}

pve =100* PCA$sdev ^2/ sum(PCA$sdev ^2)
par(mfrow =c(1,2))
plot(pve , type ="o", ylab="PVE ", xlab=" Principal Component ",
       col =" blue")
plot(cumsum (pve ), type="o", ylab =" Cumulative PVE", xlab="
Principal Component ", col =" brown3 ")


PCA_train_df <-  PCA_train_df %>%
  dplyr::select(c(PC1:paste0('PC',toString(Number_PCs))))


PCA_FE_training <- cbind(PCA_train_df, Fixed_Effects_train, Results_train)


#PCA on testing set
Continuous_Variables_test <- Testing_df %>%
  dplyr::select(-c(starts_with('Match_Combo'), Result))
Continuous_Variables_test <- Continuous_Variables_test[,9:length(Continuous_Variables_test)]


Fixed_Effects_test  <- Testing_df %>%
  dplyr::select(c(starts_with('Match_Combo')))


PCA_Test_df = as.data.frame(predict(PCA, Continuous_Variables_test))
PCA_Test_df <- PCA_Test_df %>%
  dplyr::select(c(PC1:paste0('PC',toString(Number_PCs))))

PCA_FE_test <- cbind(PCA_Test_df, Fixed_Effects_test)





# PCA KNN
KNN_Train_PCA <- PCA_FE_training %>%
  dplyr::select(c(PC1:PC30, Class))

KNN_Test_PCA <- PCA_FE_test %>%
  dplyr::select(c(PC1:PC30))


PredictorVariables_knn_pca <- KNN_Test_PCA %>% 
  names( )
Formula_KNN_pca <- formula(paste("Class ~ ", 
                             paste(PredictorVariables_knn_pca, collapse=" + ")))

set.seed(123)
ctrl <- trainControl(method="repeatedcv",repeats = 3)
knnFit <- train(Formula_KNN_pca, data = KNN_Train_PCA, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
knnFit

Best_K_pca <- as.integer(knnFit$bestTune)

KNN_Train_PCA <- KNN_Train_PCA %>%
  dplyr::select(c(PC1:PC30))


pca_knn_results = knn(KNN_Train_PCA, KNN_Test_PCA, Train_Results ,k=Best_K_pca)
table(Actual_Test_Results,pca_knn_results)
PCA_knn_accuracy <- sum(diag(table(Actual_Test_Results,pca_knn_results)))/Total_observations





PredictorVariables_logit_pca <- PCA_FE_training %>% 
  dplyr::select(-c(Class)) %>%
  names()
Formula_logit_pca <- formula(paste("Class ~ ", 
                                   paste(PredictorVariables_logit_pca, collapse=" + ")))

#PCA Ordered & Multinomial Logit 
PCA_Ordered_Logit <- orm(Formula_logit_pca, data = PCA_FE_training, maxit= 150)
PCA_Multi_nLogit <- multinom(Formula_logit_pca, data = PCA_FE_training, maxit = 250, MaxNWts= 40000)

PCA_Olog_probs_m <- predict(PCA_Ordered_Logit ,PCA_FE_test , type="fitted.ind")
PCA_Mlog_probs <- predict(PCA_Multi_nLogit ,PCA_FE_test , type="class")

PCA_Olog_probs =rep ("Lose" ,dim(PCA_FE_test)[1])
PCA_Olog_probs[PCA_Olog_probs_m[,1] < PCA_Olog_probs_m[,2] & PCA_Olog_probs_m[,2] > PCA_Olog_probs_m[,3]]="Draw"
PCA_Olog_probs[PCA_Olog_probs_m[,3] > PCA_Olog_probs_m[,2] & PCA_Olog_probs_m[,3] > PCA_Olog_probs_m[,1]]="Win"



Temp1 <- table(Actual_Test_Results,PCA_Olog_probs)
PCA_Ologit_accuracy <- sum(Temp1['Lose', 'Lose'] + Temp1['Win', 'Win'] + Temp1['Draw', 'Draw'])/Total_observations


table(Actual_Test_Results,PCA_Mlog_probs)
PCA_Mlogit_accuracy <- sum(diag(table(Actual_Test_Results,PCA_Mlog_probs)))/Total_observations


#PCA  LDA
PCA_LDA <- lda(Formula_logit_pca, data = PCA_FE_training)
PCA_LDA_probs_m <- predict(PCA_LDA , PCA_FE_test)
PCA_LDA_probs <- PCA_LDA_probs_m$class

table(Actual_Test_Results,PCA_LDA_probs)
PCA_LDA_accuracy <- sum(diag(table(Actual_Test_Results,PCA_LDA_probs)))/Total_observations





Train_Results <- PCA_FE_training[['Class']]

#PCA Lasso
X_Shrinkage <- PCA_FE_training %>%
  dplyr::select(-c(Class)) %>%
  data.matrix()

Test_Shrinkage <- PCA_FE_test %>%
  data.matrix()
#Test_Shrinkage <- Test_Shrinkage[,10:length(Test_Shrinkage)]

PCA_Lasso <- cv.glmnet(x=X_Shrinkage, y=Train_Results, alpha=1, family="multinomial")
PCA_Lasso_probs <- predict(PCA_Lasso, newx = Test_Shrinkage, s = "lambda.min", type = "class")


Temp2 <- table(Actual_Test_Results,PCA_Lasso_probs)
PCA_Lasso_accuracy <- sum(Temp2['Lose', 'Lose'] + Temp2['Win', 'Win'] + Temp2['Draw', 'Draw'])/Total_observations





#PCA Elastic Net

PCA_E_Net <- cv.glmnet(x=X_Shrinkage, y=Train_Results, alpha=0.5, family="multinomial")
PCA_E_Net_probs <- predict(PCA_E_Net, newx = Test_Shrinkage, s = "lambda.min", type = "class")
Temp3 <- table(Actual_Test_Results,PCA_E_Net_probs)
PCA_ElasticNet_accuracy <- sum(Temp3['Lose', 'Lose'] + Temp3['Win', 'Win'] + Temp3['Draw', 'Draw'])/Total_observations





#PCA Random Forest
set.seed(123)
PCA_rf = randomForest(Formula_logit_pca,data=PCA_FE_training , importance =TRUE, ntree = (ncol(PCA_FE_training)-1)*10)
PCA_rf_class <- predict(PCA_rf ,PCA_FE_test , type="class")
table(Actual_Test_Results,PCA_rf_class)
PCA_RF_accuracy <- sum(diag(table(Actual_Test_Results,PCA_rf_class)))/Total_observations


#PCA SVM
b_gamma = 3
b_cost = 10

set.seed(123)
svm_fit = svm(Formula_KNN_pca, data=PCA_FE_training, kernel ="radial", cost =b_cost, gamma =b_gamma)
svm_pred <- predict(svm_fit, PCA_FE_test)
table(Actual_Test_Results,svm_pred)

if(FALSE) {
  


#PCA Boosted Trees
Int_Train_Result = as.integer(Train_Results)-1
num_class = length(levels(Train_Results))


hyper_grid <- expand.grid(
  eta = c(.01, .05, .1, .3),
  max_depth = c(1, 3, 5, 7),
  min_child_weight = c(1, 3, 5, 7),
  subsample = c(.65, .8, 1), 
  colsample_bytree = c(.8, .9, 1),
  optimal_trees = 0,               
  min_RMSE = 0                     
)


#hyper_grid <- expand.grid(
#  eta = c( .1, .3),
#  max_depth = c(1, 7),
#  min_child_weight = c(1, 7),
#  subsample = c(.65,  1), 
#  colsample_bytree = c(.8,  1),
#  optimal_trees = 0,               
#  min_RMSE = 0                     
#)


for(i in 1:nrow(hyper_grid)) {
  
  # create parameter list
  params <- list(
    eta = hyper_grid$eta[i],
    max_depth = hyper_grid$max_depth[i],
    min_child_weight = hyper_grid$min_child_weight[i],
    subsample = hyper_grid$subsample[i],
    colsample_bytree = hyper_grid$colsample_bytree[i] )
  
  
  set.seed(123)
  xgb.tune <- xgb.cv(
    params = params,
    data = X_Shrinkage,
    label = Int_Train_Result,
    nrounds = 2000,
    nfold = 5,
    objective = "multi:softprob",
    eval_metric = "mlogloss",
    num_class = num_class,  # for regression models
    verbose = 0,               # silent,
    early_stopping_rounds = 10, # stop if no improvement for 10 consecutive trees
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_mlogloss_mean)
  hyper_grid$min_RMSE[i] <- min(xgb.tune$evaluation_log$test_mlogloss_mean)
}

hyper_grid <- hyper_grid %>%
  dplyr::arrange(min_RMSE)



xgb_train = xgb.DMatrix(data=X_Shrinkage,label=Int_Train_Result)


params <- list(
  eta = hyper_grid$eta[1],
  max_depth = hyper_grid$max_depth[1],
  min_child_weight = hyper_grid$min_child_weight[1],
  subsample = hyper_grid$subsample[1],
  colsample_bytree = hyper_grid$colsample_bytree[1] )

set.seed(123)
pca_xgb_fit=xgboost(
  params=params,
  data = xgb_train,
  nrounds=hyper_grid$optimal_trees[1],
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = num_class, 
  early_stopping_rounds=10,
  verbose=1
)

Testing_m_matrix <- as.matrix(Testing_df[,10:ncol(Testing_df)])


xgb_pca_pred = predict(pca_xgb_fit, Testing_m_matrix,reshape=T)
colnames(xgb_pca_pred) = levels(Train_Results)


Boosted_probs_pca =rep ("Lose" ,dim(xgb_pca_pred)[1])
Boosted_probs_pca[xgb_pca_pred[,'Lose'] < xgb_pca_pred[,'Draw'] & xgb_pca_pred[,'Draw'] > xgb_pca_pred[,'Win']]="Draw"
Boosted_probs_pca[xgb_pca_pred[,'Win'] > xgb_pca_pred[,'Draw'] & xgb_pca_pred[,'Win'] > xgb_pca_pred[,'Lose']]="Win"

Temp5 <- table(Actual_Test_Results,Boosted_probs_pca)
PCA_BoostedF_accuracy <- sum(Temp5['Lose', 'Lose'] + Temp5['Win', 'Win'] + Temp5['Draw', 'Draw'])/Total_observations

}



