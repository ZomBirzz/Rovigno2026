rm(list=ls(all=TRUE))
library(GAD)
dataset <- read.csv(file.choose(),header=T,sep=",", stringsAsFactors = T)
attach(dataset)
summary(dataset)

for (variable in vector) {
  
}
row = 1
pop = dataset[row,1:19]
n = pop+1
N = sum(pop)
p = n/N
H = -sum(p*log(p))
H

detach(dataset)