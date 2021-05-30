# title: World Happiness report 2021 - Logistic Regression - R
# version: 2021-05-29T930 ES
happiness <- read.csv("C:/Users/oswal/OneDrive/Stalinis kompiuteris/Duomenu analitika/R/Atsiskaitymui/world-happiness-report-2021.csv")
setwd("C:/Users/oswal/OneDrive/Stalinis kompiuteris/Duomenu analitika/R/Atsiskaitymui/world-happiness-report-2021.csv")
rm(list=ls())
table(happiness$Freedom.to.make.life.choices)
summary(happiness$Freedom.to.make.life.choices)
str(happiness)


0 -> happiness$y
summary(happiness)
1 -> happiness$y[happiness$Freedom.to.make.life.choices %in% c(0.970, 0.960, 0.959, 0.955, 0.949, 0.946, 0.940, 0.935, 0.934, 0.932, 0.929, 0.927, 0.925, 0.919, 0.917, 0.915, 0.914, 0.913, 0.910, 0.909, 0.908, 0.907, 0.906, 0.904) ] 

  
table(happiness$y)
table(happiness$y, happiness$Freedom.to.make.life.choices %in% c(0.970, 0.960, 0.959, 0.955, 0.949, 0.946, 0.940, 0.935, 0.934, 0.932, 0.929, 0.927, 0.925, 0.919, 0.917, 0.915, 0.914, 0.913, 0.910, 0.909, 0.908, 0.907, 0.906, 0.904))

happiness$Regional.indicator <- NULL
happiness$ï..Country.name <- NULL
happiness$Freedom.to.make.life.choices <- NULL
which(names(happiness)=='Freedom.to.make.life.choices')
X <- which(names(happiness)!='Freedom.to.make.life.choices')
happinessX <- happiness[,X]
happinessX <- data.frame(scale(happinessX))
happinessX <- scale(happinessX)
happinessX <- scale(happinessX)
summary(happinessX)
class(happinessX)
summary(happinessX)


happiness2 <- data.frame(cbind(happinessX, happiness$y))
names(happinessX)

happiness3 <- cbind(happiness[,X], happiness$y)

library('caTools')
set.seed(2668)

split = sample.split(happiness$y, SplitRatio = 0.7)
training_set = subset(happiness, split == TRUE)
test_set = subset(happiness, split == FALSE)

mean(test_set$y)
data.frame()
# m1: logit
my_formula <- y ~ .
m1_logit <- glm(formula = my_formula, family = binomial(link='logit'), data = training_set)
# m2: KNN
m2_knn <-
  class::knn(
    train = training_set
    , test = test_set
    , cl = training_set$y
    , k = 5 
    , l = 0
    , prob = FALSE
    , use.all = TRUE
  )
