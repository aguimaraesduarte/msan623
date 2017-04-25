

setwd("~/Downloads/")

# Get data
train <- read.csv("OnlineNewsPopularityTraining.csv", stringsAsFactors = F)

# Remove variables
train$shares <- NULL
train$url <- NULL
train$timedelta <- NULL

# Verify that the data is clean (ie no NAs)
# sapply(train, function(x) sum(is.na(x)))

# Look at the data (distribution of popular)
hist(train$popular) # or table(train$popular)
1-sum(train$popular)/nrow(train)
# The set is very unbalanced! If we predict only 0, we will have an accuracy of ~80%!!!

# Fit glm using all variables
glm.fit <- glm(popular ~ ., data = train, family = binomial)
summary(glm.fit)

# Predict probabilities
glm.probs <- predict(glm.fit, type = "response")
head(glm.probs)

# Tranform into {0, 1} predictions
glm.pred.train <- ifelse(glm.probs > 0.5, 1, 0)
table(glm.pred.train, train$popular)

# Get misclassification rate
misclass <- sum(glm.pred.train != train$popular)/length(train$popular)
print(paste('Accuracy', 1-misclass))
# The accuracy seems pretty good, but since the dataset is unbalanced, it's actually pretty bad...

# ROC curve
library(ROCR)
p <- predict(glm.fit, type = "response")
pr <- prediction(p, train$popular)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
# The ROC curve confirms that our predictions are not great!

#####
# Let's create a 50-50 split sample
pos_idx <- which(train$popular == 1)
undersample <- append(pos_idx, sample(which(train$popular == 0), length(pos_idx), F))
train_under <- train[undersample,]

# Logistic regression
# Look at the data (distribution of popular)
hist(train_under$popular) # or table(train$popular)
1-sum(train_under$popular)/nrow(train_under)
# The set is correctly balanced

# Fit glm using all variables
glm.fit <- glm(popular ~ ., data = train_under, family = binomial)
summary(glm.fit)

# Predict probabilities
glm.probs <- predict(glm.fit, type = "response")
head(glm.probs)

# Tranform into {0, 1} predictions
glm.pred.train <- ifelse(glm.probs > 0.5, 1, 0)
table(glm.pred.train, train_under$popular)

# Get misclassification rate
misclass <- sum(glm.pred.train != train_under$popular)/length(train_under$popular)
print(paste('Accuracy', 1-misclass))
# The accuracy seems pretty good! 64% > 50%

# A Beta(2, 2) has expected mean of 0.5, which is what we want since the data set is now balanced
alpha <- 2
gamma <- 2
n <- nrow(train_under)
prior <- rbeta(n, 2, 2)
hist(prior)
posterior <- rbeta(n, alpha+n/2, gamma+n/2)
hist(posterior)
logits <- log(posterior/(1-posterior))
hist(logits)
train_bayes <- train_under
train_bayes$popular <- logits
glm.fit.bayes <- glm(popular~., data=train_bayes)
summary(glm.fit.bayes)
glm.probs.bayes <- predict(glm.fit.bayes, type = "response")
head(glm.probs.bayes)
# Tranform into {0, 1} predictions
glm.pred.train.bayes <- ifelse(glm.probs.bayes > 0.5, 1, 0)
table(glm.pred.train.bayes, train_under$popular)
# Get misclassification rate
misclass.bayes <- sum(glm.pred.train.bayes != train_under$popular)/length(train_under$popular)
print(paste('Accuracy', 1-misclass.bayes))

# Test
test <- read.csv("OnlineNewsPopularityTest.csv", stringsAsFactors = F)

# Remove variables
test$shares <- NULL
test$url <- NULL
test$timedelta <- NULL

# Standard
glm.probs.test <- predict(glm.fit, test[,1:ncol(test)-1], type="response")
# Tranform into {0, 1} predictions
glm.pred.test <- ifelse(glm.probs.test > 0.5, 1, 0)
table(glm.pred.test, test$popular)
# Get misclassification rate
misclass.test <- sum(glm.pred.test != test$popular)/length(test$popular)
print(paste('Accuracy', 1-misclass.test))

# Bayes
glm.probs.test.bayes <- predict(glm.fit.bayes, test[,1:ncol(test)-1], type="response")
# Tranform into {0, 1} predictions
glm.pred.test.bayes <- ifelse(glm.probs.test.bayes > 0.5, 1, 0)
table(glm.pred.test.bayes, test$popular)
# Get misclassification rate
misclass.test.bayes <- sum(glm.pred.test.bayes != test$popular)/length(test$popular)
print(paste('Accuracy', 1-misclass.test.bayes))
