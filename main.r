#Author: Bhathiya Maneendra Pilanawithana

#-----[Load packages and install if required]
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

#-end-[Load packages and install if required]

#-----[Load the .csv file in 'Raw-Dataset' folder to a Dataframe]
dat <- read.csv("./Raw-Dataset/indian_liver_patient.csv")
#-end-[Load the .csv file in 'Raw-Dataset' folder to a Dataframe]
