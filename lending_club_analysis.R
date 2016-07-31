library(plyr)
library(dplyr)
library(DescTools)
library(data.table)
options(scipen = 999)
setwd("~/Desktop/lending-club-loan-data/")
loanbook <- fread("loan.csv")
loanbook <- as.data.frame(loanbook)
View(head(loanbook))
colnames(loanbook)
# To find missing values
apply(is.na(loanbook), MARGIN = 2, FUN = sum)
A <- apply(is.na(loanbook), MARGIN = 2, FUN = sum)
B <- which(A== 0,arr.ind = TRUE)
B <- B[B!= 17]

# Find the change in the loan amount over time
loanbook$issue_d <- as.Date(apply(as.data.frame(loanbook$issue_d),MARGIN =  1, FUN = paste0, "-01"),format="%b-%Y-%d") 

loan_amt_ts <- loanbook %>% group_by(issue_d) %>% summarise(Amount = sum(loan_amnt))
View(loan_amt_ts)
plot(loan_amt_ts$issue_d,loan_amt_ts$Amount,type = "l")

# Find the loan amount by status
loan_amt_status <- loanbook %>% group_by(loan_status) %>% summarise(Amount = sum(loan_amnt))
# Using the DescTools package to create descriptive statistics
Desc(loanbook$loan_status, plotit = T,col=PalHelsana())

# Loan Book by issue date and grade
loanbook_by_grade <- loanbook %>% 
    group_by(issue_d, grade) %>% 
    summarise(Amount = sum(loan_amnt))

loanbook_amnt_grade <- ggplot(loanbook_by_grade, aes(x = issue_d, y = Amount))
loanbook_amnt_grade + geom_area(aes(fill=grade)) + xlab("Issue date")

# Creating an indicator for Default
unique(loanbook$loan_status)
loanbook$default <- loanbook$loan_status == "Default"
sum(loanbook$default) # Checking that there are 1219 marked as in Default
# This gives a Default rate of 10 bps

# Look at default rates over the grades
default_by_grade <- loanbook %>% 
    group_by(grade) %>% 
    summarise(Issue = length(unique(member_id)),Default = sum(default))

default_by_grade$default_rate_in_bps <- 10000*default_by_grade$Default/default_by_grade$Issue

x <- default_by_grade$Default
names(x) <- default_by_grade$grade
barplot(x, las = 1, space = 0)

# Look at default rates over time
default_by_time <- loanbook %>% 
    group_by(issue_d) %>% 
    summarise(Issue = length(unique(member_id)),Default = sum(default))

default_by_time$default_rate_in_bps <- 10000*default_by_time$Default/default_by_time$Issue
default_by_time
loanbook_default_by_time <- ggplot(default_by_time, aes(x = issue_d, y = default_rate_in_bps))
loanbook_default_by_time + geom_line() + xlab("Issue date")

# The chart above shows the defaults increasing steadily and then suddenly dropping
# This is probably because the newest loans are too new to go into default yet.

# Other than default there are loan statuses that will be a problem
# Including default here as well
problem_ind <- c("Charged Off", "Default",
                 "Does not meet the credit policy. Status:Charged Off",
                    "In Grace Period", 
                    "Default Receiver", 
                    "Late (16-30 days)",
                    "Late (31-120 days)")

loanbook$is_bad <- ifelse(loanbook$loan_status %in% problem_ind, 1, 0)
sum(loanbook$is_bad)

nrow(loanbook) -sum(loanbook$is_bad)

# This gives us 67429 problem loans and 819950 good loans
# which is 8.2% ones and 91.8% zeros

# We can try building a logistic regression on this to see which are the important variables
logit.model <- glm(is_bad ~ loan_amnt 
                            + int_rate 
                            + as.factor(grade) , 
                   data = loanbook, family = "binomial")

summary(logit.model)

# This is interesting because it tells us that while loan amount is slightly correlated with prob of default
# home ownership is not. interest rate and grade of the loan would be related and thus
# are important variables
# Increasing interest rates are correlated with increased probability of default

library(caret)
library(randomForest)

# creating a data partition 70% of the data for training and 30% for testing

inTrain <- createDataPartition(y = loanbook$is_bad,times = 1,p = 0.7, list = FALSE)
training <- loanbook[inTrain,]
testing <- loanbook[-inTrain,]

# keeping only the numeric variables with no missing values
T <- cbind(training[,B],training$is_bad)
T <- T[,sapply(T,is.numeric)]
colnames(T)[ncol(T)] <- "is_bad"
T$is_bad <- as.factor(T$is_bad)

# Testing the model on a Random Forest with 5 trees
rf_model_5<-randomForest(is_bad ~ .,data= T,ntree = 5)
print(rf_model_5)
# Class error 0: 1.4% , 1 : 23% - which is not good for us
# This means that we are classifying many customers who are good as bad
# We are missing out on these customers as we might reject them
# On the testing set this model actually does better with an error rate of about 10%

pred <- predict(rf_model_5, testing, type="response")
table(pred,testing$is_bad)
#            Actual
#  pred      0      1
#   0    245652   2119
#   1     323     18119
confusionMatrix(pred,testing$is_bad)

# Testing the model on a Random Forest with 50 trees
rf_model_50 <- randomForest(is_bad ~ .,data= T,ntree = 50)
print(rf_model_50)
# Class error 0: 1.4% , 1 : 23% - which is not good for us
# This means that we are classifying many customers who are good as bad
# We are missing out on these customers as we might reject them
# On the testing set this model actually does better with an error rate of about 8%
# a 20% improvement over the 5 tree model

pred <- predict(rf_model_50, testing, type="response")
table(pred,testing$is_bad)
#  pred      0      1
#   0    245872   1606
#   1     103     18632
confusionMatrix(pred,testing$is_bad)

# Testing the model on a Random Forest with 100 trees
rf_model_100 <- randomForest(is_bad ~ .,data= T,ntree = 100)
print(rf_model_100)
# Class error 0: 1.4% , 1 : 23% - which is not good for us
# This means that we are classifying many customers who are good as bad
# We are missing out on these customers as we might reject them
# On the testing set this model actually does better with an error rate of about 7.5%
# a 25% improvement over the 5 tree model

pred <- predict(rf_model_100, testing, type="response")
table(pred,testing$is_bad)
#  pred      0      1
#   0    245872   1565
#   1     98     18673
confusionMatrix(pred,testing$is_bad)

# With no feature engineering we can get good results with Random Forests

# Potential Next Steps:
# 1. Missing value treatment
# 2. Use of categorical variables
# 3. Other techniques like Naive Bayes, SVM
# 4. Ensembling
