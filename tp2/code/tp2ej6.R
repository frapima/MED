rm(list=ls())
# We load the 'pROC' library
library(pROC)
# First of all, we read the file that contains the data
train_data=read.table('./UNED/MASTER-INGENIERIA-CIENCIA-DATOS/MED/tp2/data/d_ddd.txt',header=T)

# We train our logistic regression model, binomial as a family indicates R to run a logistic regression
# and we train the model with different predictors one by one. In this case, we are going to train the model
# three times: enf<-fumar, enf<-cafe and enf<-trat
model.lr.smoke=glm(train_data$enf ~ train_data$fumar, data = train_data, family = "binomial")
model.lr.coffee=glm(train_data$enf ~ train_data$cafe, data = train_data, family = "binomial")
model.lr.treatment=glm(train_data$enf ~ train_data$trat, data = train_data, family = "binomial")
# and we print the summary of our models
summary(model.lr.smoke)
summary(model.lr.coffee)
summary(model.lr.treatment)

# Now, let's train the model with multiple predictors (enf<-fumar,cafe,trat)
model.lr.multiple=glm(train_data$enf ~ train_data$fumar + train_data$cafe + train_data$trat, data  = train_data, family = "binomial")
# and we print the summary of the model
summary(model.lr.multiple)

# The next step is to apply to the multivariate logisitic regression model, un automatic selection process
model.lr.multiple.step = step(model.lr.multiple)

# We train the model with fumar*cafe looking for some interaction between variables
model.lr.smokeIcoffee=glm(train_data$enf ~ train_data$fumar + train_data$cafe + train_data$fumar*train_data$cafe, data  = train_data, family = "binomial")
summary(model.lr.smokeIcoffee)

# We train the model with fumar*treatment looking for some interaction between variables
model.lr.smokeItreatment=glm(train_data$enf ~ train_data$fumar + train_data$trat + train_data$fumar*train_data$trat, data  = train_data, family = "binomial")
summary(model.lr.smokeItreatment)

# Final model roc and AUC
model.lr.multiple.smokeTreatment=glm(train_data$enf ~ train_data$fumar + train_data$trat, data  = train_data, family = "binomial")
summary(model.lr.multiple.smokeTreatment)
# Now, let's predict our model.
model.lr.multiple.smokeTreatmentProb=predict(model.lr.multiple.smokeTreatment, train_data, type="response")
# and we take a look at them
model.lr.multiple.smokeTreatmentProb
# plot roc
roc_obj=roc(train_data$enf, model.lr.multiple.smokeTreatmentProb)
auc(roc_obj)
# Area under the curve: 0.8162

roc_df <- data.frame(
  fvp=rev(roc_obj$sensitivities),
  ffp=rev(1 - roc_obj$specificities))
plot(0:10/10, 0:10/10, type='n', xlab="ffp", ylab="fvp") 
abline(h=0:10/10, col="lightblue")
abline(v=0:10/10, col="lightblue")
abline(coef = c(0,1), col="lightblue")
with(roc_df, {
  lines(ffp, fvp, type='l', lwd=1, col="blue")
  lines(ffp, fvp, type='b', lwd=1, col="blue")
})

# Now, let's compute what would be the optimal cut-off to reduce the missclassiffication error
model.optCutOff=optimalCutoff(train_data, model.lr.multiple.smokeTreatmentProb) 
model.optCutOff


