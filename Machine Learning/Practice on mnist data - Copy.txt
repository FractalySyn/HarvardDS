rm(list = ls())
libs = c("tidyverse", "ggplot2", "imager", "broom", "caret", "dslabs", 
         "matrixStats", "purrr", "rpart", "randomForest")
lapply(libs, library, character.only = TRUE)
options(digits = 3)
im = function(url) plot(imager::load.image(url), axes = F)
seed = function(x) set.seed(x, sample.kind = "Rounding")

mnist = read_mnist()
# Each of these components includes a matrix with features in the columns and vector of classes
dim(mnist$tra$images)
table(mnist$train$labels)

# Because we want this example to run on a small laptop and in less than one hour, we will
# consider a subset of the dataset. We will sample 10,000 random rows from the training set
# and 1,000 random rows from the test set:
seed(1990)
index = sample(nrow(mnist$train$images), 1000)
x = mnist$train$images[index, ]
y = mnist$train$labels[index]
index = sample(nrow(mnist$test$images), 100)
x_test = mnist$test$images[index, ]
y_test = mnist$test$labels[index]





# Preprocessing -----------------------------------------------------------

"In machine learning, we often transform predictors before running the machine algorithm.
We also remove predictors that are clearly not useful. We call these steps preprocessing."

# Examples of preprocessing include standardizing the predictors, taking the log transform of
# some predictors, removing predictors that are highly correlated with others, and removing
# predictors with very few non-unique values or close to zero variation.

"We can run the nearZero function from the caret package to see that several features do
not vary much from observation to observation. We can see that there is a large number of
features with 0 variability:"
sds = colSds(x)
qplot(sds, bins = 256)

# This is expected because there are parts of the image that rarely contain writing 

"We can see the columns recommended for removal:"
nzv = nearZeroVar(x) # computed on columns (features)
image(matrix(1:784 %in% nzv, 28, 28))

# So we end up keeping this number of columns:
col_index = setdiff(1:ncol(x), nzv)
length(col_index)

"Now we are ready to fit some models. Before we start, we need to add column names to the
feature matrices as these are required by caret:"
colnames(x) = 1:ncol(mnist$train$images)
colnames(x_test) = colnames(x)




# k-nearest neighbor and random forest ------------------------------------

"Let’s start with kNN. The first step is to optimize for k. Keep in mind that when we run
the algorithm, we will have to compute a distance between each observation in the test set
and each observation in the training set. There are a lot of computations. We will therefore
use k-fold cross validation to improve speed."

# If we run the following code, the computing time on a standard laptop will be several
# minutes.
control = trainControl(method = "cv", number = 10, p = 0.9)
train_knn = train(x[, col_index], y, method = "knn",
                  tuneGrid = data.frame(k = c(3, 5, 7)),
                  trControl = control)

"In general, it is a good idea to try a test run with a subset of the data to get an idea of
timing before we start running code that might take hours to complete. We can do this as
follows:"
ind = sample(nrow(x), 1000)
control = trainControl(method = "cv", number = 2, p = 0.9)
train_knn = train(x[ind, col_index], y[ind], method = "knn",
                  tuneGrid = data.frame(k = c(3, 5, 7)),
                  trControl = control)
# We can then increase n and b and try to establish a pattern of how they affect computing
# time to get an idea of how long the fitting process will take for larger values of n and b. You
# want to know if a function is going to take hours, or even days, before you run it

"Once we optimize our algorithm, we can fit it to the entire dataset"
fit_knn = knn3(x[, col_index], factor(y), k = 3)
"The accuracy is almost 0.95!"
y_hat_knn = predict(fit_knn, x_test[, col_index], type="class")
confusionMatrix(y_hat_knn, factor(y_test))$overall["Accuracy"]
# "We now achieve an accuracy of about 0.95. From the specificity and sensitivity, we also see
# that 8s are the hardest to detect and the most commonly incorrectly predicted digit is 7."
confusionMatrix(y_hat_knn, factor(y_test))$byClass[, 1:2]



"With random forest, computation time is a challenge. For each forest, we need to build
hundreds of trees. We also have several parameters we can tune.

Because with random forest the fitting is the slowest part of the procedure rather than the
predicting (as with kNN), we will use only five-fold cross validation. We will also reduce the
number of trees that are fit since we are not yet building our final model."

# Finally, to compute on a smaller dataset, we will take a random sample of the observations
# when constructing each tree. We can change this number with the nSamp argument.
control = trainControl(method = "cv", number = 5)
grid = data.frame(mtry = c(1, 5, 10, 25, 50, 100))
train_rf = train(x[, col_index], y, method = "rf", 
                 tuneGrid = grid, trControl = control,
                 nSamp = 5000)
ggplot(train_rf)
train_rf$bestTune

"Now that we have optimized our algorithm, we are ready to fit our final model:"
fit_rf <- randomForest(x[, col_index], factor(y),
                       minNode = 10)
# To check that we ran enough trees we can use the plot function:
plot(fit_rf)

"We see that we achieve high accuracy:"
y_hat_rf <- predict(fit_rf, x_test[ ,col_index])
confusionMatrix(y_hat_rf, y_test)$overall["Accuracy"]





# Variable importance -----------------------------------------------------

"The following function computes the importance of each feature:"
imp = importance(fit_rf)
imp

# We can see which features are being used most by plotting an image:
mat = rep(0, ncol(x))
mat[col_index] = imp
image(matrix(mat, 28, 28))





# Visual assessments ------------------------------------------------------

# An important part of data analysis is visualizing results to determine why we are failing.
# How we do this depends on the application. Below we show the images of digits for which
# we made an incorrect prediction
im("https://rafalab.github.io/dsbook/book_files/figure-html/rf-images,-1.png")

"By examining errors like this we often find specific weaknesses to algorithms or parameter
choices and can try to correct them."







# Ensembles ---------------------------------------------------------------

"In machine learning, one can usually greatly improve the final results by combining the
results of different algorithms.
Here is a simple example where we compute new class probabilities by taking the average
of random forest and kNN. We can see that the accuracy improves to 0.96:"
p_rf = predict(fit_rf, x_test[, col_index], type = "prob")
p_rf %>% head() # returns the probabilities to be of each class
p_knn = predict(fit_knn, x_test[, col_index])
p_knn %>% head()
p = (p_rf + p_knn) / 2
head(p)

y_hat = (apply(p, 1, which.max) -1) %>% factor() # 1 for rows # select the higher prob of each row
y_hat

confusionMatrix(y_hat, factor(y_test))$overall["Accuracy"]









# Assessment --------------------------------------------------------------

"For these exercises we are going to build several machine learning models for the
mnist_27 dataset and then build an ensemble. Each of the exercises in this comprehension 
check builds on the last."
data("mnist_27")

# "Use the training set to build a model with several of the models available from the 
# caret package. We will test out 10 of the most common machine learning models in this exercise:"
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", 
            "multinom", "qda", "rf", "adaboost")

"Q1 Apply all of these models using train() with all the default parameters. You may 
need to install some packages. Keep in mind that you will probably get some warnings. 
Also, it will probably take a while to train all of the models - be patient!"
seed(1)
fits = lapply(models, function(model){
   print(model)
   train(y~., method = model, data = mnist_27$train)
})


"Q2 Now that you have all the trained models in a list, use sapply() or map() to 
create a matrix of predictions for the test set. You should end up with a matrix 
with length(mnist_27$test$y) rows and length(models) columns."
predictions = sapply(fits, function(fit){
   predict(fit, mnist_27$test)
}) 
colnames(predictions) = models
predictions %>% head()
dim(predictions)

"Q3 Now compute accuracy for each model on the test set."
acc = apply(predictions, 2, function(x){
   confusionMatrix(factor(x), factor(mnist_27$test$y))$overall["Accuracy"]
})
mean(acc)

"Q4 Next, build an ensemble prediction by majority vote and compute the accuracy of 
the ensemble. Vote 7 if more than 50% of the models are predicting a 7, and 2 otherwise."
y_ensemble = apply(predictions, 1, function(x){
   ifelse(mean(as.numeric(x) == 2) > 0.5, 2, 7) 
})
y_ensemble
ensemble_acc = confusionMatrix(factor(y_ensemble), factor(mnist_27$test$y))$overall["Accuracy"]
ensemble_acc

"Q5 How many of the individual methods do better than the ensemble?"
sum(acc > ensemble_acc)
acc[which(acc > ensemble_acc)]

"Q6 It is tempting to remove the methods that do not perform well and re-do the ensemble. 
The problem with this approach is that we are using the test data to make a decision. 
However, we could use the minimum accuracy estimates obtained from cross validation with
the training data for each model. Obtain these estimates and save them in an object. 
Report the mean of these training set accuracy estimates."
train_acc = sapply(fits, function(fit){
   fit$results$Accuracy %>% max()
}) 
mean(train_acc)

"Q7 Now let's only consider the methods with an estimated accuracy of greater than or 
equal to 0.8 when constructing the ensemble. Vote 7 if 50% or more of the models are 
predicting a 7, and 2 otherwise."
y_ensemble = apply(predictions[, which(train_acc > 0.8)], 1, function(x){
   ifelse(mean(as.numeric(x) == 2) > 0.5, 2, 7) 
})
y_ensemble
ensemble_acc = confusionMatrix(factor(y_ensemble), factor(mnist_27$test$y))$overall["Accuracy"]
ensemble_acc










 