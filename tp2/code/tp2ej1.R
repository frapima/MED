rm(list=ls())
# We read the data
datos=read.table("./UNED/MASTER-INGENIERIA-CIENCIA-DATOS/MED/tp2/data/le.txt", header=T) 
attach(datos)
# We load the library pROC
library(pROC)

# We create the ROC object
roc_obj <- roc(datos$rta, datos$exp)
# We print the area under curve
auc(roc_obj)
# and we plot the roc curve
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