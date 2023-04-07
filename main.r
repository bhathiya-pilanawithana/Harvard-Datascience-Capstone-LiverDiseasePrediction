#Author: Bhathiya Maneendra Pilanawithana
#Date: 07/04/2023

#-----[Load packages and install if required]
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

if (!require(caret)) install.packages("caret")
library(caret)
#-end-[Load packages and install if required]

#-----[Load the .csv file in 'Raw-Dataset' folder to a Dataframe]
dat <- read.csv("./Raw-Dataset/indian_liver_patient.csv")
#-end-[Load the .csv file in 'Raw-Dataset' folder to a Dataframe]

#-----[Rename to "Dataset" and change notation of disease existence]
dat <- dat %>% mutate(Disease = ifelse(Dataset==1,1,0), Gender=as.factor(Gender)) %>% select(-Dataset)
#-end-[Rename to "Dataset" and change notation of disease existence]

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
colnames(dat_log_for_corr) <- 
  c("Tot Pr","Alb", "Alb Glb", "Tot Bil", "Dir Bil", "ALP", "ALA", "ASA")
cr <- round(cor(dat_log_for_corr),4)
cr
  # Highly Correlated Variables are
  #  Total_Bilirubin - Direct_Bilirubin = 0.9657
  #  Alamine Aminotransferase - Aspartate Aminotransferase = 0.8416
  #  Total Protiens - Albumin = 0.7831
  #  Albumin - Albumin and Globulin Ratio = 0.6896
#-end-[Correlation Matrix]

#-----[Plot highly correlated variables]
dat_log %>% ggplot(aes(x = Total_Bilirubin_log, y = Direct_Bilirubin_log)) +
  geom_point(aes(col = as.factor(Disease)), alpha = 0.3) +
  guides(color = guide_legend(title = "Disease"))

dat_log %>% ggplot(aes(x = Alamine_Aminotransferase_log, y = Aspartate_Aminotransferase_log)) +
  geom_point(aes(col = as.factor(Disease)), alpha = 0.3) +
  guides(color = guide_legend(title = "Disease"))

dat_log %>% ggplot(aes(x = Total_Protiens, y = Albumin)) +
  geom_point(aes(col = as.factor(Disease)), alpha = 0.3) +
  guides(color = guide_legend(title = "Disease"))

dat_log %>% ggplot(aes(x = Albumin, y = Albumin_and_Globulin_Ratio)) +
  geom_point(aes(col = as.factor(Disease)), alpha = 0.3) +
  guides(color = guide_legend(title = "Disease"))
#-end-[Plot highly correlated variables]'

#-----[Create test and training sets]
set.seed(1)
test_index <- createDataPartition(dat_log$Disease, times=1, p=0.4, list=FALSE)
test_set <- dat_log[test_index,]
train_set <- dat_log[-test_index,]
#-end-[Create test and training sets]