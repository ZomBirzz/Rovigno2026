rm(list=ls(all=TRUE))
library(GAD)
dataset <- read.csv(file.choose(),header=T,sep=",", stringsAsFactors = T)
attach(dataset)
summary(dataset)

boxplot(S~Anno+Sito+Anno*Sito) #why dafuq non ci sono i quadrati?

Anno <- as.fixed(Anno)
Sito <- as.random(Sito)

linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model) # C = NA ???????
gad(linear.model) # ???????????

estimates(linear.model)


detach(dataset)