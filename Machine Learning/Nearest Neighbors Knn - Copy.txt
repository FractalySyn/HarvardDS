rm(list = ls())
libs = c("tidyverse", "ggplot2", "imager", "broom", "caret", "dslabs", 
         "matrixStats", "purrr")
lapply(libs, library, character.only = TRUE)
options(digits = 3)





# Knn ---------------------------------------------------------------------

data("mnist_27")
mnist_27$test%>% ggplot(aes(x_1, x_2, color = y)) + geom_point()


"With k-nearest neighbors (kNN) we estimate p(x1, x2) in a similar
way to bin smoothing. However, as we will see, kNN is easier to adapt to multiple dimensions.
First we define the distance between all observations based on the features. Then, for any
point (x1, x2) for which we want an estimate of p(x1, x2), we look for the k nearest points
to (x1, x2) and then take an average of the 0s and 1s associated with these points. We refer
to the set of points used to compute the average as the neighborhood. Due to the connection
we described earlier between conditional expectations and conditional probabilities, this
gives us a ˆp(x1, x2), just like the bin smoother gave us an estimate of a trend. As with
bin smoothers, we can control the flexibility of our estimate, in this case through the k
parameter: larger ks result in smoother estimates, while smaller ks result in more flexible
and more wiggly estimates."

# To implement the algorithm, we can use the knn3 function from the caret package. Looking
# at the help file for this package, we see that we can call it in one of two ways. We will use
# the first in which we specify a formula and a data frame. The data frame contains all the
# data to be used. The formula has the form outcome ~ predictor_1 + predictor_2 +
# predictor_3 and so on. Therefore, we would type y ~ x_1 + x_2. If we are going to use
# all the predictors, we can use the . like this y ~ .. The final call looks like this:
knn_fit = knn3(y ~ ., data = mnist_27$train, k = 5)

"In this case, since our dataset is balanced and we care just as much about sensitivity as we
do about specificity, we will use accuracy to quantify performance.
The predict function for knn produces a probability for each class. We keep the probability
of being a 7 as the estimate ˆp(x1, x2)"
y_hat_knn = predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]

# Before we used linear regression and got 0.75 accuracy which is lower

"In this case, since our dataset is balanced and we care just as much about sensitivity as we
do about specificity, we will use accuracy to quantify performance.
The predict function for knn produces a probability for each class. We keep the probability
of being a 7 as the estimate ˆp(x1, x2)"

"https://rafalab.github.io/dsbook/book_files/figure-html/knn-fit-1.png" %>%
   load.image() %>% plot(axes = F)

# We see that kNN better adapts to the non-linear shape of p(x1, x2). However, our estimate
# has some islands of blue in the red area, which intuitively does not make much sense. This is
# due to what we call over-training. We describe over-training in detail below. Over-training
# is the reason that we have higher accuracy in the train set compared to the test set:
y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(y_hat_knn, mnist_27$train$y)$overall["Accuracy"]
#> Accuracy
#> 0.882
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]
#> Accuracy
#> 0.815





# Over-training -----------------------------------------------------------

"Over-training is at its worst when we set k = 1. With k = 1, the estimate for each (x1, x2)
in the training set is obtained with just the y corresponding to that point. In this case, if the
(x1, x2) are unique, we will obtain perfect accuracy in the training set because each point
is used to predict itself. Remember that if the predictors are not unique and have different
outcomes for at least one set of predictors, then it is impossible to predict perfectly"
knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(y_hat_knn_1, mnist_27$train$y)$overall[["Accuracy"]]
#> [1] 0.995

# However, the test set accuracy is actually worse than logistic regression:
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn_1, mnist_27$test$y)$overall["Accuracy"]
#> Accuracy
#> 0.735

"https://rafalab.github.io/dsbook/book_files/figure-html/knn-1-overfit-1.png" %>%
   load.image() %>% plot(axes = F)

# The black curves denote the decision rule boundaries.
# The estimate ˆp(x1, x2) follows the training data too closely (left). You can see that in the
# training set, boundaries have been drawn to perfectly surround a single red point in a sea
# of blue. Because most points (x1, x2) are unique, the prediction is either 1 or 0 and the
# prediction for that point is the associated label. However, once we introduce the training
# set (right), we see that many of these small islands now have the opposite color and we end
# up making several incorrect predictions






# Over-smoothing ----------------------------------------------------------

"Although not as badly as with the previous examples, we saw that with k = 5 we also
over-trained. Hence, we should consider a larger k. Let’s try, as an example, a much larger
number: k = 401."

knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn_401, mnist_27$test$y)$overall["Accuracy"]
#> Accuracy
#> 0.79
# This turns out to be similar to regression:
"https://rafalab.github.io/dsbook/book_files/figure-html/mnist-27-glm-est-1.png" %>%
   load.image() %>% plot(axes = F)

"This size of k is so large that it does not permit enough flexibility. We call this oversmoothing."






# Picking the k in kNN ----------------------------------------------------

"So how do we pick k? In principle we want to pick the k that maximizes accuracy, or minimizes
the expected MSE as defined in 27.4.8. The goal of cross validation is to estimate these
quantities for any given algorithm and set of tuning parameters such as k. To understand
why we need a special method to do this let’s repeat what we did above but for different
values of k:"

ks = seq(3, 251, 2)
accuracy = map_df(ks, function(k){
   fit = knn3(y ~ ., data = mnist_27$train, k = k)
   
   y_hat = predict(fit, mnist_27$train, type = "class")
   cm_train = confusionMatrix(y_hat, mnist_27$train$y)
   train_error = cm_train$overall[["Accuracy"]]
   
   y_hat = predict(fit, mnist_27$test, type = "class")
   cm_test = confusionMatrix(y_hat, mnist_27$test$y)
   test_error = cm_test$overall[["Accuracy"]]
   
   tibble(k = k, train = train_error, test = test_error)
})

# Note that we estimate accuracy by using both the training set and the test set. We can now
# plot the accuracy estimates for each value of k:
accuracy %>% ggplot(aes(k)) +
   geom_line(aes(y = train), color = "red") +
   geom_line(aes(y = test), color = "blue")

"First, note that the estimate obtained on the training set is generally higher than the one
obtained with the test set, with the difference larger for smaller values of k. This is due to
over-training. Also note that the accuracy versus k plot is quite jagged. We do not expect
this because small changes in k should not affect the algorithm’s performance too much.
The jaggedness is explained by the fact that the accuracy is computed on a sample and
therefore is a random variable. This demonstrates why we prefer to minimize the expected
loss rather than the loss we observe with one dataset."

# If we were to use these estimates to pick the k that maximizes accuracy, we would use the
# estimates built on the test data:
ks[which.max(accuracy$test)]
#> [1] 41
max(accuracy$test)
#> [1] 0.86


"Another reason we need a better estimate of accuracy is that if we use the test set to pick
this k, we should not expect the accompanying accuracy estimate to extrapolate to the real
world. This is because even here we broke a golden rule of machine learning: we selected the
k using the test set. Cross validation also provides an estimate that takes this into account."





# Assessment --------------------------------------------------------------

"Previously, we used logistic regression to predict sex based on height. Now we are 
going to use knn to do the same. Set the seed to 1, then use the caret package to 
partition the dslabs heights data into a training and test set of equal size. Use 
the sapply() function to perform knn with k values of seq(1, 101, 3) and calculate 
F1 scores with the F_meas() function using the default value of the relevant argument."

data("heights")
set.seed(1, sample.kind = "Rounding")
ind = createDataPartition(heights$sex, list = F)
train_set = heights[-ind, ]; test_set = heights[ind, ]

ks = seq(1, 101, 3)

F1score = function(k)
{
   train = data.frame(y = train_set$sex, 
                      x = train_set$height)
   fit = knn3(y~x, k = k, data = train)
   
   test = data.frame(y = test_set$sex,
                     x = test_set$height)
   y_hat = predict(fit, test, type = "class")
   
   F_meas(y_hat, test$y)
}

F_1 = sapply(ks, F1score); F_1

"Q1"
max(F_1)
ks[which.max(F_1)]



"Q2 Next we will use the same gene expression example used in the Distance exercises"
data("tissue_gene_expression")
set.seed(1, sample.kind = "Rounding")

ind = createDataPartition(tissue_gene_expression$y, list = F)
xtest = tissue_gene_expression$x[ind, ]; xtrain = tissue_gene_expression$x[-ind, ]
ytest = tissue_gene_expression$y[ind] %>% factor()
ytrain = tissue_gene_expression$y[-ind] %>% factor()

k = seq(1, 11, 2)
acc = function(k){
   fit = knn3(xtrain, ytrain, k = k)
   y_hat = predict(fit, xtest, type = "class")
   confusionMatrix(y_hat, ytest)$overall[["Accuracy"]]
}

sapply(k, acc)













