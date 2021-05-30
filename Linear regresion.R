# title: World Happiness report 2021 - Linear Regression - R
# version: 2021-05-27T1430 ES
rm(list=ls())
# business question - Kurie laimingumo indekso veiksniai labiausiai susije su laisve priimti spendimus gyvenime?
# get data - is kaggle (https://www.kaggle.com/ajaypalsinghlo/world-happiness-report-2021)
# load data +
happiness <- read.csv("C:/Users/oswal/OneDrive/Stalinis kompiuteris/Duomenu analitika/R/Atsiskaitymui/world-happiness-report-2021.csv")
# understand data
str(happiness)
summary(happiness)
head(happiness)
plot(happiness)
# prepare data
happiness$ï..Country.name <- 0
happiness$Regional.indicator <-0
cor(happiness[,3:18])

library(corrplot)
corrplot::corrplot(cor(happiness[,3:18]))
View(happiness)

set.seed(2668)
library(caTools)
split <- sample.split(happiness, SplitRatio = 0.7 )
split
train <- subset(happiness, split = "TRUE" )
test <- subset(happiness, split = "FALSE")
train
test
# fit model
Model <- lm(Freedom.to.make.life.choices ~., data = train )
Model <- lm(formula = Model, data = train )

# evaluate model
str(Model)
summary(Model)

# choose final
predict(Model)
pred <- predict(Model, test)
pred
happiness$Freedom.to.make.life.choices

y_delta <- 
  data.frame(
    y_predicted = predict(Model)
    , y_true = happiness$Freedom.to.make.life.choices
  )

y_delta$delta <-
  (y_delta$y_predicted - y_delta$y_true)^2


# Finding accuracy
sqrt(sum(y_delta$delta))
rmse <- sqrt(mean(pred-happiness$Freedom.to.make.life.choices)^2)
rmse




