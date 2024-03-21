data <- read.csv("/Users/ryan/Documents/GitHub/Statistics/Applied Statistics Modeling/CW1/DefaultData.csv")

default = data[, 2]
default = factor(default)

student <- data[, 3]
student <- factor(student)

balance <- data[, 4]
income <- da
ta[, 5]

out1 = glm(default ~ student + balance + log(income), family=binomial(link="logit"))

summary(out1)

vcov(out1)


