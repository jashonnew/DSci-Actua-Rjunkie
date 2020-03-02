

pacman::p_load(tidyverse, e1071)

let_dat <- read_csv("../.Rdata/letters.csv")
vow_data <- read_csv("../.Rdata/vowel.csv")


#Letters

# Partition the data into training and test sets
# by getting a random 30% of the rows as the testRows
allRows <- 1:nrow(let_dat)
testRows <- sample(allRows, trunc(length(allRows) * 0.3))

# The test set contains all the test rows
letTest <- let_dat[testRows,]

# The training set contains all the other rows
letTrain <- let_dat[-testRows,]

# Train an SVM model
# Tell it the attribute to predict vs the attributes to use in the prediction,
#  the training data to use, and the kernal to use, along with its hyperparameters.
#  Please note that "Species~." contains a tilde character, rather than a minus
costcof <- c(1,5,10,20,50)
gamm <- c(.001,.002,.005,.01,.02,.05,.1)

for (i in costcof){
  for (j in gamm){
    model <- svm(letter~., data = letTrain, kernel = "radial", gamma = 0.001, cost = 10, type = "C")
    predi <- predict(model, letTest[,-1])
    agreem <- predi == letTest$letter
    accur <- prop.table(table(agreem))
    print(accur)
    print(i)
    print(j)
  }
}
model <- svm(letter~., data = letTrain, kernel = "radial", gamma = 0.001, cost = 10, type = "C")

# Use the model to make a prediction on the test set
# Notice, we are not including the last column here (our target)
prediction <- predict(model, letTest[,-1])

# Produce a confusion matrix
confusionMatrix <- table(pred = prediction, true = letTest$letter)

# Calculate the accuracy, by checking the cases that the targets agreed
agreement <- prediction == letTest$letter
accuracy <- prop.table(table(agreement))

# Print our results to the screen
print(confusionMatrix)
print(accuracy)



#Vowels
allRows <- 1:nrow(vow_data)
testRows <- sample(allRows, trunc(length(allRows) * 0.3))


# The test set contains all the test rows
vowTest <- vow_data[testRows,]

# The training set contains all the other rows
vowTrain <- vow_data[-testRows,]

costcof <- c(1,5,10,20,50)
gamm <- c(.001,.002,.005,.01,.02,.05,.1)

for (i in costcof){
  for (j in gamm){
    model <- svm(Class~., data = vowTrain, kernel = "radial", gamma = j, cost = i,type = "C")
    predi <- predict(model, vowTest[,-13])
    agreem <- predi == vowTest$Class
    accur <- prop.table(table(agreem))
    print(accur)
    print(i)
    print(j)
  }
}
model <- svm(Class~., data = vowTrain, kernel = "radial", gamma = 0.01, cost = 50,type = "C")

# Use the model to make a prediction on the test set
# Notice, we are not including the last column here (our target)
prediction <- predict(model, vowTest[,-13])

# Produce a confusion matrix
confusionMatrix <- table(pred = prediction, true = vowTest$Class)

# Calculate the accuracy, by checking the cases that the targets agreed
agreement <- prediction == vowTest$Class
accuracy <- prop.table(table(agreement))

# Print our results to the screen
print(confusionMatrix)
print(accuracy)
