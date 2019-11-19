rm(list=ls())
datos=read.table('/Users/javierpiquerasmartinez/UNED/MASTER-INGENIERIA-CIENCIA-DATOS/MED/unit1/data/c_d_1.txt',header=T) attach(datos)
ind1=which(exp==1)
ind2=which(exp==2)
n1=length(rta[ind1]); n1
n2=length(rta[ind2]); n2
tapply(rta,exp,mean)
tapply(rta,exp,sd)

shapiro.test(rta[ind1]) 
shapiro.test(rta[ind2])

library(lawstat) 
levene.test(rta,exp,location="mean")

t.test(rta[ind1],rta[ind2],var.equal=TRUE)