## start
# Load:
options(scipen=999)
rm(list=ls())
Start = Sys.time()
# 0a - Install and load required packages
pkgs <- c("tidyverse","openxlsx","stringr","lubridate","magrittr", "haven","rstudioapi","readxl","ggrepel","plyr", "dplyr", "mosaic", "class", "glmnet", "MASS", "splitstackshape", "rms", "caret", "nnet", "fastDummies", "clusterSim")
# Install: - should not need to re-install if already installed
# install.packages(pkgs)
# Load:
lapply(pkgs, require, character.only = TRUE)
###############################################
###             Read-ins and Paths          ###
###############################################
Input <- 'C:/Users/Talla/Documents/Projects/FIFA Match Win Prediction/4 - Combine Data Sources/OUTPUT/'
Output <- Intial_df$Match_Combo = gsub("\\.", "_", Intial_df$Match_Combo)
'C:/Users/Talla/Documents/Projects/FIFA Match Win Prediction/4 - Combine Data Sources/OUTPUT/'
###############################################
###             Run Analysis                ###
###############################################
Intial_df <- readRDS(file = paste0(Input,"Final.rds"))
Intial_df <- Intial_df %>% 
  filter(league_name_h == "English Premier League")

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


#KNN
KNN_Train <- Balanced_Training %>%
  dplyr::select(c(overall_Att:defending_Mid, Class))

KNN_Test <- Testing_df %>%
  dplyr::select(overall_Att:defending_Mid)

set.seed(123)
ctrl <- trainControl(method="repeatedcv",repeats = 3)
knnFit <- train(c ~ ., data = KNN_Train, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
knnFit

KNN_Train <- Balanced_Training %>%
  dplyr::select(overall_Att:defending_Mid)


knn_results = knn(KNN_Train, KNN_Test, Train_Results ,k=5)
table(Actual_Test_Results,knn_results)
sum(diag(table(Actual_Test_Results,knn_results)))/Total_observations


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
sum(Temp1['Lose', 'Lose'] + Temp1['Win', 'Win'] + Temp1['Draw', 'Draw'])/Total_observations


table(Actual_Test_Results,Mlog_probs)
sum(diag(table(Actual_Test_Results,Mlog_probs)))/Total_observations


table(Actual_Test_Results,LDA_probs)
sum(diag(table(Actual_Test_Results,LDA_probs)))/Total_observations

#Lasso
X_Shrinkage <- Balanced_Training %>%
  dplyr::select(-c(Class)) %>%
  data.matrix()

Test_Shrinkage <- Testing_df %>%
  data.matrix()
Test_Shrinkage <- Test_Shrinkage[,10:539]

Lasso <- cv.glmnet(x=X_Shrinkage, y=Train_Results, alpha=1, family="multinomial")
Lasso_probs <- predict(Lasso, newx = Test_Shrinkage, s = "lambda.min", type = "class")
Temp2 <- table(Actual_Test_Results,Lasso_probs)
sum(Temp2['Lose', 'Lose'] + Temp2['Win', 'Win'] + Temp2['Draw', 'Draw'])/Total_observations

#Elastic Net

E_Net <- cv.glmnet(x=X_Shrinkage, y=Train_Results, alpha=0.5, family="multinomial")
E_Net_probs <- predict(E_Net, newx = Test_Shrinkage, s = "lambda.min", type = "class")
Temp3 <- table(Actual_Test_Results,E_Net_probs)
sum(Temp3['Lose', 'Lose'] + Temp3['Win', 'Win'] + Temp3['Draw', 'Draw'])/Total_observations



#Principal Component
Continuous_Variables <- Balanced_Training %>%
  dplyr::select(-c(starts_with('Match_Combo'), Class))

Fixed_Effects <- Balanced_Training %>%
  dplyr::select(c(starts_with('Match_Combo')))

corr <- cor(Continuous_Variables, method="pearson")
corrplot::corrplot(corr, method= "color", order = "hclust", tl.pos = 'n')


PCA_Variables_Normalized <- data.Normalization(Continuous_Variables, type="n1", normalization="column")
PCA <- prcomp(PCA_Variables_Normalized, center=TRUE, scale.=TRUE)
PCA_df <- as.data.frame(PCA$x)
summary(PCA)


Percent_Variation_Expl <- PCA$sdev^2/sum(PCA$sdev^2)
Percent_Variation_Expl
sum(Percent_Variation_Expl[0:34])


pve =100* PCA$sdev ^2/ sum(PCA$sdev ^2)
par(mfrow =c(1,2))
plot(pve , type ="o", ylab="PVE ", xlab=" Principal Component ",
       col =" blue")
plot(cumsum (pve ), type="o", ylab =" Cumulative PVE", xlab="
Principal Component ", col =" brown3 ")

