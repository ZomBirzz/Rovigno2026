rm(list=ls(all=TRUE))
library(GAD)
dataset <- read.csv(file.choose(),header=T,sep=",", stringsAsFactors = T)
attach(dataset)
summary(dataset)

length(dataset)

# ciclo per H delle specie in copertura
for (i in 1:450) {  
  pop = as.numeric(dataset[i,6:25])
  n = pop+1
  N = sum(pop)
  p = n/N
  H = -sum(p*log(p))
  print(H)
}

# ciclo per H delle specie in n individui
for (i in 1:450) {           
  pop = dataset[i,26:47]
  n = pop+1
  N = sum(pop)
  p = n/N
  H = -sum(p*log(p))
  print(H)
}

detach(dataset)