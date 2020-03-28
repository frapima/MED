# Example D <- D

# We read the data and create the table
rm(list=ls())
datos=read.table('/Users/javierpiquerasmartinez/UNED/MASTER-INGENIERIA-CIENCIA-DATOS/MED/unit1/data/d_d_1.txt',header=T) 
attach(datos)
tabla=table(-rta,exp); tabla

# We create the variables to store the important features we need to compute the CI
ind1=which(exp==1);
ind2=which(exp==2);
ind11=which(rta==1 & exp==1);
ind12=which(rta==1 & exp==2);
a1=length(rta[ind11]); a1
a2=length(rta[ind12]); a2
n1=length(rta[ind1]); n1
n2=length(rta[ind2]); n2
alfa=0.05
pi1=a1/n1 
pi2=a2/n2

#Standart error computation
ee=sqrt(pi1*(1-pi1)*(1/n1)+pi2*(1-pi2)*(1/n2)); ee

#CI computation
ic1=pi1-pi2-qnorm(1-alfa/2)*ee; ic1 
ic2=pi1-pi2+qnorm(1-alfa/2)*ee; ic2

pit=(a1+a2)/(n1+n2); pit

ee0=sqrt(pit*(1-pit)*(1/n1+1/n2)) z=(pi1-pi2)/ee0; z
z*z
p_valor= 2*(1-pnorm(abs(z))); p_valor

a=c(a1,a2); a 
n=c(n1,n2); n 

#We do the test
prop.test(a,n,correct=F)