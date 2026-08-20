rm(list=ls(all=TRUE))
library(ggplot2)
dataset <- read.csv(file.choose(),header=T,sep=",", stringsAsFactors = T)
attach(dataset)
summary(dataset)



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