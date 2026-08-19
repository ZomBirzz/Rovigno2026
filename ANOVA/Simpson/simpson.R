rm(list=ls(all=TRUE))
library(GAD)
dataset <- read.csv(file.choose(),header=T,sep=",", stringsAsFactors = T)
attach(dataset)
summary(dataset)

for (i in 1:450) {  
  # i=1
  pop = as.numeric(dataset[i,6:25])
  n = pop+1
  N = sum(n)
  d = sum(n*(n-1))
  q = N*(N-1)
  D = d/q
  # p = n/N
  # D = -log(sum(p^2)) #base e
  # print(H)
  cat(D, file = "D1.csv", sep = ",\n", append = TRUE)

}

detach(dataset)