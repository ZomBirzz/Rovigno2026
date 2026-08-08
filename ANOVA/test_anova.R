rm(list=ls(all=TRUE))
library(GAD)
dataset <- read.csv(file.choose(),header=T,sep=",", stringsAsFactors = T)
attach(dataset)
summary(dataset)

boxplot(S~ANNO+SITE)

Anno <- as.fixed(ANNO)
Sito <- as.random(SITE)

# P = sqrt(S)
# P = log(S, base = 2)
# linear.model=lm(P~Anno+Sito+Anno*Sito)

linear.model=lm(S~Anno+Sito+Anno*Sito)


C.test(linear.model) # qumentiamo il pvalue a 0.1
gad(linear.model) # effetto significativo dell'interazione annoXsito, singoli fattori non significativi

estimates(linear.model)
?snk.test()
snk.test(linear.model, term = "Anno:Sito", among = "Sito", within = "Anno", anova.tab = gad(linear.model))
snk.test(linear.model, term = "Anno:Sito", among = "Anno", within = "Sito", anova.tab = gad(linear.model))


# detach(dataset)