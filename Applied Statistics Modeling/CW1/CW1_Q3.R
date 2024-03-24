data <- read.csv("/Users/ryan/Documents/GitHub/Statistics/Applied Statistics Modeling/CW1/DefaultData.csv")
# Source the script containing the function
source("/Users/ryan/Documents/GitHub/Statistics/Applied Statistics Modeling/CW1/CW1_Q3_functions.R")


default = data[, 2]
default = factor(default)

#default_num<- as.numeric(default) - 1

student <- data[, 3]
student <- factor(data$student, levels = c("No", "Yes"))

balance <- data[, 4]
income <- data[, 5]



#Qa
correlation1 <- cor(default_num, balance)
correlation2 <- cor(default_num, income)
correlation1
correlation2

#Qc
out1 = glm(default ~ student + balance + income, family=binomial(link="logit"))
#out1_num = glm(default_num ~ student + balance + income, family=binomial(link="logit"))

summary(out1)

vcov(out1)

summary(out1_num)

#Qd

mbp1 <- max_balance_probability(data,out1)
mbp1

mp1 <- max_probability(data,out1)
mp1

mbp1_num <- max_balance_probability(data,out1_num)
mbp1_num

mp1_num <- max_probability(data,out1_num)
mp1_num

#Qe
out1 = glm(default ~ student + balance + income, family=binomial(link="logit"))
out2 <- glm(default ~ student + balance + income, family=binomial(link="probit"))
out3 <- glm(default ~ student + balance + income, family=binomial(link="cloglog"))
out4 <- glm(default ~ student + balance + income, family=binomial(link="cauchit"))

mbp2 <- max_balance_probability(data,out2)
mbp2

mp2 <- max_probability(data,out2)
mp2

mbp3 <- max_balance_probability(data,out3)
mbp3

mp3 <- max_probability(data,out3)
mp3

mbp4 <- max_balance_probability(data,out4)
mbp4

mp4 <- max_probability(data,out4)
mp4


# Create a matrix with 4 rows and 2 columns
results_table <- matrix(c(mbp1, mbp2, mbp3, mbp4, mp1, mp2, mp3, mp4), 
                        nrow = 4, ncol = 2, byrow = FALSE)

# Name the rows and columns
rownames(results_table) <- c("logit", "probit", "cloglog", "cauchit")
colnames(results_table) <- c("mbp", "mp")

# Print the table
print(results_table)
