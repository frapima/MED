rm(list=ls())
# We load the 'InformationValue' library
library(InformationValue)
# We load the 'pROC' library
library(pROC)
# First of all, we read the file that contains the data
train_data=read.table('./UNED/MASTER-INGENIERIA-CIENCIA-DATOS/MED/tp2/data/le.txt',header=T)
# and we print it to take a look at the data
head(train_data)
table(train_data)

# We train our logistic regression model, binomial as a family indicates R to run a logistic regression
model.lr=glm(train_data$rta ~ train_data$exp, data = train_data, family = "binomial")
# and we print the coefficients of our model
coef(model.lr)
#(Intercept) train_data$exp 
#-1.92703240     0.02040076 

# Now, let's predict our model.
model.prob=predict(model.lr, train_data, type="response")
# and we take a look at them
model.prob

# Now, let's transform all probabilities > 0.5 into 1, and all probabilities <0.5 into 0
model.pred=rep(0, 53)
model.pred[model.prob > 0.5] = 1
# and we take a look at the confussion matrix
table(model.pred, rta)
#             rta
# model.pred  0  1
#          0  29 17
#          1  4  3
# In the main diagonal represent the correct predictions.
# The off-diagonal represents the incorrect predictions.

# Let's compute the fraction of correct classifications
mean(model.pred == rta)
# 0.6037736
# And the missclassification error
misClassError(train_data, model.prob, threshold = 50)
# 36.5

# Now, let's compute what would be the optimal cut-off to reduce the missclassiffication error
model.optCutOff=optimalCutoff(train_data, model.prob) 
model.optCutOff
# 0.3585171

# So let's see the results with this new threshold
model.pred2=rep(0, 53)
model.pred2[model.prob > model.optCutOff] = 1
# and we take a look at the confussion matrix
table(model.pred2, rta)
#             rta
# model.pred  0  1
#          0  23 4
#          1  10  16
# The main diagonal represents the correct predictions.
# The off-diagonal represents the incorrect predictions.

# Let's compute the fraction of correct classifications
mean(model.pred2 == rta)
# 0.7358491
# And the missclassification error
misClassError(train_data, model.prob, threshold = model.optCutOff)
# 33.5

#plotROC(rta, predicted)
roc_obj=roc(train_data$rta, model.prob)
auc(roc_obj)
# Area under the curve: 0.725

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

