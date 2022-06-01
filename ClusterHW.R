setwd("~/Downloads")
library(readr)
library(ggplot2)
library(ape)
TomatoFirst <- read_csv("TomatoFirst.csv")
head(TomatoFirst)

tomato <- subset((TomatoFirst), !Tomato %in% c("Round", "Tomato", "Price", "Source", 
                                    "Sweet", "Color", "Texture", "Overall"))
hc=hclust(dist(TomatoFirst))
plot(hc)
plot(hc, labels = TomatoFirst$Tomato)
plot(hc, labels = TomatoFirst$Source)

TomatoFirst <- as.data.frame(TomatoFirst)
rownames(TomatoFirst) <- TomatoFirst$Tomato
TomatoFirst$Tomato <- NULL

hc=hclust(dist(TomatoFirst), method = "ward.D2")
plot(hc)
plot(hc, labels = TomatoFirst$Source)
rect.hclust(hc, 3)

#EXTRA CREDIT

plot(as.phylo(hc), type = "fan", show.tip.label = TRUE, show.node.label = TRUE)
colors = c("blue", "green", "purple")
clus = cutree(hc, 3)
plot(as.phylo(hc), type = "fan", tip.color = colors[clus],
     label.offset = 1, cex = 0.7)

