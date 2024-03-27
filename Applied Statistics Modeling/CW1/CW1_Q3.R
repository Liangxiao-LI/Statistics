data <- read.csv("/Users/ryan/Documents/GitHub/Statistics/Applied Statistics Modeling/CW1/DefaultData.csv")
# Source the script containing the function
source("/Users/ryan/Documents/GitHub/Statistics/Applied Statistics Modeling/CW1/CW1_Q3_functions.R")

library(ggplot2)


default = data[, 2]
default = factor(default)

default_num<- as.numeric(default) - 1

student <- data[, 3]
student <- factor(data$student, levels = c("No", "Yes"))

balance <- data[, 4]
income <- data[, 5]





#Qa
correlation1 <- cor(default_num, balance)
correlation2 <- cor(default_num, income)
correlation1
correlation2

ggplot(data, aes(x = default, y = balance)) +
  geom_boxplot() +
  labs(title = "Box plot of Balance by Default Status",
       x = "Default Status",
       y = "Balance") +
  theme_minimal()

ggplot(data, aes(x = default, y = income)) +
  geom_boxplot() +
  labs(title = "Box plot of income by Default Status",
       x = "Default Status",
       y = "income") +
  theme_minimal()


#Qc
out1 = glm(default ~ student + balance + income, family=binomial(link="logit"))
#out1_num = glm(default_num ~ student + balance + income, family=binomial(link="logit"))

# Assuming 'out1' is your glm model
confint_out1 <- confint(out1, "balance", level = 0.95)
confint_out2 <- confint(out1, "income", level = 0.95)
print(confint_out1)
print(confint_out2)

summary(out1)

vcov(out1)

#summary(out1_num)

#Qd

mbp1 <- max_balance_probability(data,out1)
mbp1

mp1 <- max_probability(data,out1)
mp1

# mbp1_num <- max_balance_probability(data,out1_num)
# mbp1_num
# 
# mp1_num <- max_probability(data,out1_num)
# mp1_num

#Qe
out1 = glm(default ~ student + balance + income, family=binomial(link="logit"))
out2 <- glm(default ~ student + balance + income, family=binomial(link="probit"))
out3 <- glm(default ~ student + balance + income, family=binomial(link="cloglog"))

mbp2 <- max_balance_probability(data,out2)
mbp2

mp2 <- max_probability(data,out2)
mp2

mbp3 <- max_balance_probability(data,out3)
mbp3

mp3 <- max_probability(data,out3)
mp3

# Create a matrix with 4 rows and 2 columns
results_table <- matrix(c(mbp1[2], mbp2[2], mbp3[2], mbp1[1],mbp2[1],mbp3[1],mp1[2], mp2[2], mp3[2],mp1[1], mp2[1], mp3[1]), 
                        nrow = 3, ncol = 4, byrow = FALSE)

# Name the rows and columns
rownames(results_table) <- c("logit", "probit", "cloglog")
colnames(results_table) <- c("mbp", "mbp_index","mp","mp_index")

# Print the table
print(results_table)

