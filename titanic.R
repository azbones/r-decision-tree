library(rattle)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(titanic)
library(caret)

## Get the data from a csv file

titanic <- read.csv("C:/Users/colli/Downloads/titanic.csv")

## 75% sample for training data
sample_size <- floor(0.75 * nrow(titanic))

## Use seed to make models reproduceable
set.seed(123)

## Determine number of rows to sample
train_split <- sample(seq_len(nrow(titanic)), size = sample_size)

## Split the data into 75% training and 25% testing
train <- titanic[train_split, ]
test <- titanic[-train_split, ]


## Build a decision tree model
my_tree <- rpart(Survived ~ Pclass + Sex + Age + Siblings.Spouses.Aboard + Parents.Children.Aboard + Fare, data = train, method = "class")

## Plot the model results
fancyRpartPlot(my_tree)

## Create the probabilities for each test data point
predict_probs <- as.data.frame(predict(my_tree, newdata = test, type = "p"))

## Create the predicted test values and ground truth values
predicted <- as.integer(predict_probs$`1` > .5)
actual <- test$Survived

## Build confusion matrix
confusionMatrix(as.factor(predicted), as.factor(actual), positive = "1")

## https://www.dataschool.io/simple-guide-to-confusion-matrix-terminology/

