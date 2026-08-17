rm(list=ls(all=TRUE))
library(GAD)
dataset <- read.csv(file.choose(),header=T,sep=",", stringsAsFactors = T)
attach(dataset)
summary(dataset)

pielou = H1 / log(Sricoprimento) #log naturale
cat(pielou, file = "pielou.csv", sep = ",\n")

detach(dataset)
