library(readr)
library(ggplot2)
library(dplyr)
library(ROCR)
HeartDisease <- read_csv("Downloads/HeartDisease.csv")
names(HeartDisease)

HeartDisease_subset <- HeartDisease %>%
  select(systolicBP, `tobacco(kg)Intake`, LDL, fatTissues, stressLevel, BMI, alcoholConsumption, age,
         coronaryHeartDisease)
str(HeartDisease_subset)

HeartDisease_subset <- HeartDisease_subset %>%
  mutate(coronaryHeartDisease=gsub("Present", "1", coronaryHeartDisease),
         coronaryHeartDisease=factor(gsub("Absent", "0", coronaryHeartDisease)))

set.seed(5623)
split <- sample(2, nrow(HeartDisease_subset), replace=T, prob = c(0.7, 0.30))
table(split)
training <- subset(HeartDisease_subset, split=="1")
testing <- subset(HeartDisease_subset,split=="2") 

training[, -9] <- scale(training[, -9])
testing[, -9] <- scale(testing[, -9])

model1 <- glm(coronaryHeartDisease~., training, family="binomial") #binomail is yes or no
summary(model1)

model2 <- glm(coronaryHeartDisease~. -alcoholConsumption -fatTissues, training, family="binomial")
summary(model2)


ModelTrain <- predict(model2, training, type="response")
head(ModelTrain)

table(ActualValue=training$coronaryHeartDisease, predictedValue=ModelTrain>0.5)

Accuracy <- (175+50)/(175+26+59+50)
Accuracy


y_pred2 <- predict(model2, training, type="response")
ROCRPred <- prediction(y_pred2, training$coronaryHeartDisease)
ROCRPref <- performance(ROCRPred, 'tpr', "fpr")
plot(ROCRPref, colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))

#Extra Credit

p <- predict(model2, testing)
table(ActualValue=testing$coronaryHeartDisease, predictedValue=p>0.5)

Accuracy_Test <- (95+12)/(95+6+39+12)
Accuracy_Test

