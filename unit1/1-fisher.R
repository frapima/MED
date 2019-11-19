rm(list=ls())
datos=read.table('/Users/javierpiquerasmartinez/UNED/MASTER-INGENIERIA-CIENCIA-DATOS/MED/unit1/data/d_d_2.txt',header=T) attach(datos)
tabla=table(rta,exp)
fisher.test(x=tabla,alternative="two.sided")