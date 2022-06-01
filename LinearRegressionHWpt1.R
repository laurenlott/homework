library(readr)
library(ggplot2)
Advertising <- read_csv("Downloads/Advertising.csv")
names(Advertising)


plot(Sales, TV, col="blue")
lm.fit <- lm(TV ~ Sales)
abline(lm.fit)
abline(lm.fit, lwd=3, col="red")

regression=lm(TV~Sales)
summary(regression)
lm(formula = TV ~ Sales)


