rm(list=ls(all=TRUE))
library(GAD)
dataset <- read.csv(file.choose(),header=T,sep=",", stringsAsFactors = T)
attach(dataset)
summary(dataset)

boxplot(H1~ANNO+SITE)

Anno <- as.fixed(ANNO)
Sito <- as.random(SITE)
linear.model=lm(H1~Anno+Sito+Anno*Sito)

H = log10(H1)
linear.model=lm(H~Anno+Sito+Anno*Sito)
C.test(linear.model) 
gad(linear.model) # molto significativa l'interazione e il sito

snk.test(linear.model, term = "Anno:Sito", among = "Sito", within = "Anno", anova.tab = gad(linear.model))
snk.test(linear.model, term = "Anno:Sito", among = "Anno", within = "Sito", anova.tab = gad(linear.model))

detach(dataset)
