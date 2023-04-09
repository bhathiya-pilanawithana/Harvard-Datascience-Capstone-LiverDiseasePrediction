#Author: Bhathiya Maneendra Pilanawithana
#Date: 07/04/2023

#-----[Load packages and install if required]
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

if (!require(caret)) install.packages("caret")
library(caret)

if (!require(kernlab)) install.packages("kernlab") #for svmRadial Model
library(kernlab)
#-end-[Load packages and install if required]

#-----[Load the .csv file in 'Raw-Dataset' folder to a Dataframe]
dat <- read.csv("./Raw-Dataset/indian_liver_patient.csv")
#-end-[Load the .csv file in 'Raw-Dataset' folder to a Dataframe]

#-----[Rename "Dataset" column and change notation of disease existence]
dat <- dat %>% mutate(Disease = as.factor(ifelse(Dataset==1,1,0)), Gender=as.factor(Gender)) %>% select(-Dataset)
#-end-[Rename "Dataset" column and change notation of disease existence]

#-----[Check for missing values]
sum(is.na(dat %>% select(-Gender)))
colSums(is.na(dat %>% select(-Gender)))
#-end-[Check for missing values]

#-----[Omit rows with missing values]
dat <- na.omit(dat)
#-end-[Omit rows with missing values]

#-----[Calculate susceptibility of genders for liver disease]
mean((dat %>% filter(Gender=="Male"))$Disease==1)
mean((dat %>% filter(Gender=="Female"))$Disease==1)
#-end-[Calculate susceptibility of genders for liver disease]

#-----[Check outliers and visible patterns of numerical variables]
#The following codes plots individual numerical variable against its index and color coded with Disease
dat %>% ggplot(aes(x = seq(1,nrow(dat),1), y = Total_Bilirubin)) +
  geom_point(aes(col=as.factor(Disease)), alpha = 0.5, size = 0.7) +
  guides(color = guide_legend(title = "Disease")) +
  xlab("Index")

dat %>% ggplot(aes(x = seq(1,nrow(dat),1), y = Direct_Bilirubin)) +
  geom_point(aes(col=as.factor(Disease)), alpha = 0.5, size = 0.7) +
  guides(color = guide_legend(title = "Disease")) +
  xlab("Index")

dat %>% ggplot(aes(x = seq(1,nrow(dat),1), y = Alkaline_Phosphotase)) +
  geom_point(aes(col=as.factor(Disease)), alpha = 0.5, size = 0.7) +
  guides(color = guide_legend(title = "Disease")) +
  xlab("Index")

dat %>% ggplot(aes(x = seq(1,nrow(dat),1), y = Alamine_Aminotransferase)) +
  geom_point(aes(col=as.factor(Disease)), alpha = 0.5, size = 0.7) +
  guides(color = guide_legend(title = "Disease")) +
  xlab("Index")

dat %>% ggplot(aes(x = seq(1,nrow(dat),1), y = Aspartate_Aminotransferase)) +
  geom_point(aes(col=as.factor(Disease)), alpha = 0.5, size = 0.7) +
  guides(color = guide_legend(title = "Disease")) +
  xlab("Index")

dat %>% ggplot(aes(x = seq(1,nrow(dat),1), y = Total_Protiens)) +
  geom_point(aes(col=as.factor(Disease)), alpha = 0.5) +
  guides(color = guide_legend(title = "Disease")) +
  xlab("Index")

dat %>% ggplot(aes(x = seq(1,nrow(dat),1), y = Albumin)) +
  geom_point(aes(col=as.factor(Disease)), alpha = 0.5) +
  guides(color = guide_legend(title = "Disease")) +
  xlab("Index")

dat %>% ggplot(aes(x = seq(1,nrow(dat),1), y = Albumin_and_Globulin_Ratio)) +
  geom_point(aes(col=as.factor(Disease)), alpha = 0.5) +
  guides(color = guide_legend(title = "Disease")) +
  xlab("Index")
#-end-[Check outliers and visible patterns  of numerical variables]

#----[Apply Log Transformations]
#Reason for applying log transformation is explained in the rmd and pdf of project report
dat_log <- dat %>% 
  mutate(Total_Bilirubin_log = log2(Total_Bilirubin),
         Direct_Bilirubin_log = log2(Direct_Bilirubin),
         Alkaline_Phosphotase_log = log10(Alkaline_Phosphotase),
         Alamine_Aminotransferase_log = log10(Alamine_Aminotransferase),
         Aspartate_Aminotransferase_log = log10(Aspartate_Aminotransferase)) %>%
  select(-Total_Bilirubin, -Direct_Bilirubin, -Alkaline_Phosphotase, 
         -Alamine_Aminotransferase, -Aspartate_Aminotransferase)
#-end-[Apply Log Transformations]

#-----[Correlation Matrix]
dat_log_for_corr <- dat_log %>% select(-Age,-Gender,-Disease)
colnames(dat_log_for_corr) <-             #Change column names to abbrivations
  c("Tot Pr","Alb", "Alb Glb", "Tot Bil", "Dir Bil", "ALP", "ALA", "ASA")
cr <- round(cor(dat_log_for_corr),4)
cr
  # Highly Correlated Variables are
  #  Total_Bilirubin - Direct_Bilirubin = 0.9657
  #  Alamine Aminotransferase - Aspartate Aminotransferase = 0.8416
  #  Total Protiens - Albumin = 0.7831
  #  Albumin - Albumin and Globulin Ratio = 0.6896
#-end-[Correlation Matrix]

#-----[Create test and training sets]
set.seed(1)
test_index <- createDataPartition(dat_log$Disease, times=1, p=0.4, list=FALSE)
test_set <- dat_log[test_index,]
train_set <- dat_log[-test_index,]
#-end-[Create test and training sets]

#-----[Preprocessing and variable transformation using PCA]
preProc <- preProcess(train_set, method = c("pca"), pcaComp = 8)
train_set_transformed <- predict(preProc, train_set)
test_set_transformed <- predict(preProc, test_set)
#-end-[Preprocessing and variable transformation using PCA]

#-----[Cross-validation parameters for model selection]
control <- trainControl(method = "cv", number = 8, p = .8)
#-end-[Cross-validation parameters for model selection]

#-----[kNN parameter optimization using cross-validation]
set.seed(2)
k <- seq(2,100,2)
fit_knn <- train(Disease ~ ., 
                 data = train_set_transformed, 
                 method = "knn", 
                 tuneGrid = data.frame(k),
                 trControl = control)
knn_validation_acc <- max(fit_knn$results$Accuracy) #Holds kNN Validation Accuracy
#-end-[kNN parameter optimization using cross-validation]

#-----[Decision-Tree parameter optimization using cross-validation]
set.seed(3)
cp <- seq(0, 0.2, 0.004)
fit_rpart <- train(Disease ~ ., 
                     data=train_set_transformed, 
                     method = "rpart",
                     tuneGrid = data.frame(cp = cp),
                     trControl = control)
rpart_validation_acc <- max(fit_rpart$results$Accuracy) #Holds Decision-Tree Validation Accuracy
#-end-[Decision-Tree parameter optimization using cross-validation]

#-----[Random-Forest parameter optimization using cross-validation]

#Caret package cross-validation for Random-Forest does not automatically include
#ntree and nodesize. Therefore, ntree and nodesize parameters are fine-tuned manually
#using the following code

set.seed(4)
ntree <- seq(100, 130, 5) #Vector for ntree
nodesize <- c(1,2,3) #Vector for nodesize
exgrd <- expand.grid(ntree,nodesize) #Create a vector with all combinations of ntree and nodesize
ind_vec <- seq(1,nrow(exgrd)) #Holds an index vector to sweep through exgrd vector using following sapply
acc <- sapply(ind_vec, function(n){
  train(Disease ~., 
        data = train_set_transformed,
        method = "rf", 
        tuneGrid = data.frame(mtry = 1), 
        ntree = exgrd$Var1[n],
        nodesize = exgrd$Var2[n],
        trControl = control)$results$Accuracy
})
opt_mtry <- 1
opt_ntree <- exgrd$Var1[which.max(acc)] #Hold optimal ntree value that maximizes the validation accuracy
opt_nodesize <- exgrd$Var2[which.max(acc)] #Hold optimal nodesize value that maximizes the validation accuracy

#The following code build a Random-Forest predictor using optimal ntree and nodesize values found
set.seed(4)
fit_rf <- train(Disease ~., 
                data = train_set_transformed,
                method = "rf", 
                tuneGrid = data.frame(mtry = opt_mtry), 
                ntree = opt_ntree,
                nodesize = opt_nodesize,
                trControl = control)
rf_validation_acc <- max(fit_rf$results$Accuracy) #Holds Random-Forest Validation Accuracy
#-end-[Random-Forest parameter optimization using cross-validation]

#-----[SVM-Radial optimization using cross-validation]
set.seed(5)
C <- seq(0.1, 2, 20)
sigma <- seq(0.1, 2, 20)
fit_svmRadial <- train(Disease ~.,
                  data = train_set_transformed,
                  method = "svmRadial", 
                  tuneGrid = data.frame(C = C, sigma = sigma),
                  trControl = control)
svmRadial_validation_acc <- max(fit_svmRadial$results$Accuracy) #Holds Svm-Radial Validation Accuracy
#-end-[SVM-Radial optimization using cross-validation]

#-----[Print validation accuracies for model selection]
knn_validation_acc
rpart_validation_acc 
rf_validation_acc #Random-Forest has the highest validation accuracy. It is used for final results.
svmRadial_validation_acc
#-end-[Print validation accuracies for model selection]

#-----[Obtain and print performance metrices for Test-set]
Disease_hat_rf_test <- predict(fit_rf, test_set_transformed)
cm_rf <- confusionMatrix(Disease_hat_rf_test, test_set_transformed$Disease, positive = "1")
cm_rf[["overall"]][["Accuracy"]]
cm_rf[["byClass"]][["Sensitivity"]]
cm_rf[["byClass"]][["Specificity"]]
cm_rf[["byClass"]][["F1"]]
cm_rf[["byClass"]][["Pos Pred Value"]]
cm_rf[["byClass"]][["Neg Pred Value"]]
#-end-[Obtain and print performance metrices for Test-set]