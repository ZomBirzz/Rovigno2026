rm(list=ls(all=TRUE))
dataset <- read.csv(file.choose(),header=T,sep=",", stringsAsFactors = T)
attach(dataset)
summary(dataset)
install.packages("ggplot2")
library(ggplot2)



ggplot(dataset, aes(x = anno, y = S, fill = sito)) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d() +
  labs(x = "anno", y = "media S", fill = "sito") +
  theme_minimal()


ggplot(dataset, aes(x = anno, y = H, fill = sito)) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d() +
  labs(x = "anno", y = "media H", fill = "sito") +
  theme_minimal()


ggplot(dataset, aes(x = anno, y = D, fill = sito)) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d() +
  labs(x = "anno", y = "media D", fill = "sito") +
  theme_minimal()