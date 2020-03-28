rm(list=ls())
# We load the 'InformationValue' library
library(InformationValue)
# We load the 'pROC' library
library(pROC)
# First of all, we read the file that contains the data
train_data=read.table('./UNED/MASTER-INGENIERIA-CIENCIA-DATOS/MED/tp2/data/d_d_3.txt',header=T)
# and we print it to take a look at the data
head(train_data)
table(train_data)
# We train our logistic regression model, binomial as a family indicates R to run a logistic regression
model.lr=glm(train_data$rta ~ train_data$exp, data = train_data, family = "binomial")
# and we print the summary of our model
summary(model.lr)