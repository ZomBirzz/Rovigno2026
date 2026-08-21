rm(list=ls(all=TRUE))
library(GAD)
library(ggplot2)
dataset <- read.csv(file.choose(),header=T,sep=",", stringsAsFactors = T)
attach(dataset)
summary(dataset)

d = 1 - D
cor.test(H1, d)

ggplot(dataset, aes(x = H1, y = d)) + 
      geom_point(aes(color = factor(ANNO), shape = factor(SITE))) +
      stat_smooth(method=lm)