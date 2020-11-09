rm(list = ls())
libs = c("tidyverse", "ggplot2", "imager", "broom", "caret", "dslabs", 
         "matrixStats", "purrr", "rpart", "randomForest")
lapply(libs, library, character.only = TRUE)
options(digits = 3)
im = function(url) plot(imager::load.image(url), axes = F)




"The caret package currently includes 237 different methods which are summarized in 
the caret package manual (https://topepo.github.io/caret/available-models.html). 
Keep in mind that caret does not include the needed packages and, to implement a 
package through caret, you still need to install the library."


data("mnist_27")




# Train function ----------------------------------------------------------

"The caret train function lets us train different algorithms using similar syntax. So, for
example, we can type:"
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

# To make predictions, we can use the output of this function directly
y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

"This permits us to quickly compare the algorithms. For example, we can compare the accuracy
like this:"
confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]





# Cross validation --------------------------------------------------------

"When an algorithm includes a tuning parameter, train automatically uses cross validation
to decide among a few default values. To find out what parameter or parameters are
optimized, you can read the manual or study the output of:"
getModelInfo("knn")$knn$parameters
modelLookup("knn")

# If we run it with default values:
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)
# you can quickly see the results of the cross validation using the ggplot function. The argument
# highlight highlights the max:
ggplot(train_knn, highlight = TRUE)



"By default, the cross validation is performed by taking 25 bootstrap samples comprised of
25% of the observations. For the kNN method, the default is to try k = 5, 7, 9. We change
this using the tuneGrid parameter. The grid of values must be supplied by a data frame
with the parameter names as specified in the modelLookup output."
train_knn = train(y~., method = "knn", data = mnist_27$train,
                  tuneGrid = data.frame(k = seq(9, 71, 2)))
ggplot(train_knn, highlight = T)
# Note that when running this code, we are fitting 30 versions of kNN to 25 bootstrapped
# samples. Since we are fitting 30×25 = 750 kNN models, running this code will take several
# seconds.

"To access the parameter that maximized the accuracy, you can use this:"
train_knn$bestTune
"and the best performing model like this:"
train_knn$finalModel

# The function predict will use this best performing model. Here is the accuracy of the best
# model when applied to the test set, which we have not used at all yet because the cross
# validation was done on the training set:
confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall["Accuracy"]



"If we want to change how we perform cross validation, we can use the trainControl function.
We can make the code above go a bit faster by using, for example, 10-fold cross validation.
This means we have 10 samples using 10% of the observations each. We accomplish this
using the following code:"
control = trainControl(method = "cv", number = 10, p = 0.9) # 10-fold CV on a validation set of 10%
train_knn_cv = train(y~., method = "knn", data = mnist_27$train,
                     tuneGrid = data.frame(k = seq(9, 71, 2)),
                     trControl = control) 
ggplot(train_knn_cv, highlight = T)
# We notice that the accuracy estimates are more variable, which is expected since we changed
# the number of samples used to estimate accuracy





# Example: fitting with loess ---------------------------------------------

"The best fitting kNN model approximates the true conditional probability"
im("https://rafalab.github.io/dsbook/book_files/figure-html/mnist27-optimal-knn-fit-1.png")

# However, we do see that the boundary is somewhat wiggly. This is because kNN, like the
# basic bin smoother, does not use a kernel. To improve this we could try loess. By reading
# through the available models part of the manual we see that we can use the gamLoess
# method. In the manual we also see that we need to install the gam package if we have not
# done so already. Finally a lookup shows that we can tune two parameters
modelLookup("gamLoess")

"We will stick to a degree of 1. But to try out different values for the span, we still have to
include a column in the table with the name degree so we can do this"
grid = expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1) # dataframe of all parameters values combinations
train_loess = train(y~., method = "gamLoess", data = mnist_27$train,
                    tuneGrid = grid)
ggplot(train_loess, highlight = TRUE)

# We can see that the method performs similar to kNN:
confusionMatrix(data = predict(train_loess, mnist_27$test),
                reference = mnist_27$test$y)$overall["Accuracy"]
# and produces a smoother estimate of the conditional probability:
im("https://rafalab.github.io/dsbook/book_files/figure-html/gam-smooth-1.png")












# Assessment --------------------------------------------------------------

"Q1 Load the rpart package and then use the caret::train() function with method = 
'rpart' to fit a classification tree to the tissue_gene_expression dataset. Try
out cp values of seq(0, 0.1, 0.01). Plot the accuracies to report the results of 
the best model. Set the seed to 1991.
Which value of cp gives the highest accuracy?"
data("tissue_gene_expression")
dat = data.frame(x = tissue_gene_expression$x, y = tissue_gene_expression$y)
set.seed(1991, sample.kind = "Rounding")
train_rpart = train(y ~ ., data = dat, method = "rpart",
                    tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)))
ggplot(train_rpart, highlight = T)
train_rpart$bestTune
train_rpart$results


"Q2 Note that there are only 6 placentas in the dataset. By default, rpart requires 
20 observations before splitting a node. That means that it is difficult to have a
node in which placentas are the majority. Rerun the analysis you did in Q1 with 
caret::train(), but this time with method = rpart and allow it to split any node
by using the argument control = rpart.control(minsplit = 0). Look at the confusion 
matrix again to determine whether the accuracy increases. Again, set the seed to 1991."
set.seed(1991, sample.kind = "Rounding")
train_rpart = train(y ~ ., data = dat, method = "rpart",
                    tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)),
                    control = rpart.control(minsplit = 0))
ggplot(train_rpart, highlight = T)
train_rpart$results

"Q3 Plot the tree from the best fitting model of the analysis you ran in Q2"
fit = rpart(y ~ ., data = dat, cp = 0, minsplit = 0)
plot(fit); text(fit)


"Q4 We can see that with just seven genes, we are able to predict the tissue type. 
Now let's see if we can predict the tissue type with even fewer genes using a Random
Forest. Use the train() function and the rf method to train a Random Forest model 
and save it to an object called fit. Try out values of mtry ranging from 
seq(50, 200, 25) (you can also explore other values on your own). What mtry value 
maximizes accuracy? To permit small nodesize to grow as we did with the classification 
trees, use the following argument: nodesize = 1"
set.seed(1991, sample.kind = "Rounding")
fit = train(y~., data = dat, method = "rf", 
            tuneGrid = data.frame(mtry = seq(50, 200, 25)),
            nodesize = 1)
fit$results$mtry[which.max(fit$results$Accuracy)]


"Q5 Use the function varImp() on the output of train() and save it to an object called imp"
imp = varImp(fit)
imp

"Q6 The rpart() model we ran above in Q2 produced a tree that used just seven predictors. 
Extracting the predictor names is not straightforward, but can be done. We can extract the
names like this:"
tree_terms = as.character(unique(train_rpart$finalModel$frame$var[!(train_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms
"Calculate the variable importance in the Random Forest call from Q4 for these seven 
predictors and examine where they rank."
imp$importance[tree_terms,] %>% data.frame(tree_terms)
imp











