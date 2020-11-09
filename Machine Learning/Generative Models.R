rm(list = ls())
libs = c("tidyverse", "ggplot2", "imager", "broom", "caret", "dslabs", 
         "matrixStats", "purrr")
lapply(libs, library, character.only = TRUE)
options(digits = 3)



"We have described several approaches to estimating p(x). In all these approaches, we estimate
the conditional probability directly and do not consider the distribution of the predictors.
In machine learning, these are referred to as discriminative approaches."

"However, Bayes’ theorem tells us that knowing the distribution of the predictors X may be
useful. Methods that model the joint distribution of Y and X are referred to as generative
models (we model how the entire data, X and Y , are generated). We start by describing
the most general generative model, Naive Bayes, and then proceed to describe two specific
cases, quadratic discriminant analysis (QDA) and linear discriminant analysis (LDA)."




# NAive Bayes -------------------------------------------------------------

"Recall that Bayes rule tells us that we can rewrite p(x) like this:
      p(x) = P(Y=1 | X=x) = f[X|Y=1]*P(Y=1) / (f[X|Y=0]*P(Y=0) + f[X|Y=1]*P(Y=1)) "
# with f representing distribution functions of the predictor X for the two 
# classes Y=1 and Y=0.The formula implies that if we can estimate these conditional
# distributions of the predictors, we can develop a powerful decision rule. However, this is a
# big if. As we go forward, we will encounter examples in which X has many dimensions and
# we do not have much information about the distribution. In these cases, Naive Bayes will be
# practically impossible to implement.

"The Naive Bayes model will is the above model. Given the a priori probability (here the prevalence 
to be a Female in the population, the model will estimate the conditional probabilities of being
a female knowing the height thanks to the estimated distributions of the height in the population.
The formula can then be used to make predictions"


"Let’s start with a very simple and uninteresting, yet illustrative, case: the example related
to predicting sex from height."
data("heights")
y = heights$height
set.seed(1995, sample.kind = "Rounding")
ind = createDataPartition(y, list = F)
train = heights[-ind, ]; test = heights[ind, ]

# In this case, the Naive Bayes approach is particularly appropriate because we know that the
# normal distribution is a good approximation for the conditional distributions of height given
# sex for both classes Y = 1 (female) and Y = 0 (male). This implies that we can approximate
# the conditional distributions
params = train %>% group_by(sex) %>%
   summarise(avg = mean(height), sd = sd(height))
params

"The prevalence, which we will denote with π = Pr(Y = 1), can be estimated from the data
with:"
pi = train %>% summarise(pi = mean(sex == "Female")) %>% pull(pi)
pi

"Now we can use our estimates of average and standard deviation to get an actual rule:"
x = test$height
f0 = dnorm(x, params$avg[2], params$sd[2])
f1 = dnorm(x, params$avg[1], params$sd[1])

p_hat_bayes = f1*pi / (f1*pi + f0*(1-pi))
p_hat_bayes %>% plot(x, y=.) # probability to be a Female given the size
"Our Naive Bayes estimate ˆp(x) looks a lot like our logistic regression estimate"
# In fact, we can show that the Naive Bayes approach is similar to the logistic regression
# prediction mathematically.







# Controlling prevalence --------------------------------------------------

"As we discussed earlier, our sample has a much lower prevalence, 0.21, than the general
population. So if we use the rule ˆp(x) > 0.5 to predict females, our accuracy will be affected
due to the low sensitivity
Again, this is because the algorithm gives more weight to specificity to account for the low
prevalence

This is due mainly to the fact that ˆπ is substantially less than 0.5, so we tend to predict
Male more often. It makes sense for a machine learning algorithm to do this in our sample
because we do have a higher percentage of males. But if we were to extrapolate this to a
general population, our overall accuracy would be affected by the low sensitivity."

y_hat = ifelse(p_hat_bayes > 0.5, "Female", "Male")
sensitivity(factor(y_hat), reference = factor(test$sex))
specificity(factor(y_hat), reference = factor(test$sex))

# The Naive Bayes approach gives us a direct way to correct this since we can simply force ˆπ
# to be whatever value we want it to be. So to balance specificity and sensitivity, instead of
# changing the cutoff in the decision rule, we could simply change ˆπ to 0.5 like this:
p_hat_bayes_unbiased <- f1 * 0.5 / (f1 * 0.5 + f0 * (1 - 0.5))
y_hat_bayes_unbiased <- ifelse(p_hat_bayes_unbiased> 0.5, "Female", "Male")
sensitivity(factor(y_hat_bayes_unbiased), factor(test$sex))
specificity(factor(y_hat_bayes_unbiased), factor(test$sex))

"The new rule also gives us a very intuitive cutoff between 66-67, which is about the middle
of the female and male average heights:"
qplot(x, p_hat_bayes_unbiased, geom ="line") +
   geom_hline(yintercept = 0.5, lty = 2) +
   geom_vline(xintercept = 67, lty = 2)





# QDA - Quadratic discriminant analysis -----------------------------------

"Quadratic Discriminant Analysis (QDA) is a version of Naive Bayes in which we assume that
the distributions pX|Y =1(x) and pX|Y =0(x) are multivariate normal. The simple example
we described in the previous section is actually QDA. Let’s now look at a slightly more
complicated case: the 2 or 7 example."

data("mnist_27")

# In this case, we have two predictors so we assume each one is bivariate normal. This implies
# that we need to estimate two averages, two standard deviations, and a correlation for each
# case Y = 1 and Y = 0. Once we have these, we can approximate the distributions fX1,X2|Y =1
# and fX1,X2|Y =0. We can easily estimate parameters from the data:

params = mnist_27$train %>%
   group_by(y) %>%
   summarise(avg_1 = mean(x_1), avg_2 = mean(x_2),
             sd_1= sd(x_1), sd_2 = sd(x_2),
             r = cor(x_1, x_2))
params

"Here we provide a visual way of showing the approach. We plot the data and use contour
plots to give an idea of what the two estimated normal densities look like (we show the
curve representing a region that includes 95% of the points):"
mnist_27$train %>%
   mutate(y = factor(y)) %>%
   ggplot(aes(x_1, x_2, fill = y, color = y)) +
   geom_point() +
   stat_ellipse(type = "norm", lwd = 1.5)


"We can use the train function from the caret package to fit the model and obtain predictors:"
train_qda = train(y ~ ., method = "qda", data = mnist_27$train)
train_qda

y_hat = predict(train_qda, mnist_27$test)
confusionMatrix(y_hat, mnist_27$test$y)$overall[["Accuracy"]]

# The estimated conditional probability looks relatively good, although it does not fit as well
# as the kernel smoothers
"https://rafalab.github.io/dsbook/book_files/figure-html/qda-estimate-1.png" %>%
   load.image() %>% plot(axes = F)


"One reason QDA does not work as well as the kernel methods is perhaps because the
assumption of normality does not quite hold. Although for the 2s it seems reasonable, for
the 7s it does seem to be off. Notice the slight curvature in the points for the 7s:"
mnist_27$train %>% mutate(y = factor(y)) %>%
   ggplot(aes(x_1, x_2, fill = y, color=y)) +
   geom_point() +
   stat_ellipse(type="norm") +
   facet_wrap(~y)

# QDA can work well here, but it becomes harder to use as the number of predictors increases.
# Here we have 2 predictors and had to compute 4 means, 4 SDs, and 2 correlations. How
# many parameters would we have if instead of 2 predictors, we had 10? The main problem
# comes from estimating correlations for 10 predictors. With 10, we have 45 correlations for
# each class.




# LDA - Linear discriminant analysis --------------------------------------

"A relatively simple solution to the problem of having too many parameters is to assume that
the correlation structure is the same for all classes, which reduces the number of parameters
we need to estimate."

# In this case, we would compute just one pair of standard deviations and one correlation,
# and the distributions looks like this:

"https://rafalab.github.io/dsbook/book_files/figure-html/lda-explained-1.png" %>%
   load.image() %>% plot(axes = F)
# Now the size of the ellipses as well as the angle are the same. This is because they have the
# same standard deviations and correlations.

train_lda = train(y ~ ., method = "lda", data = mnist_27$train)
y_hat = predict(train_lda, mnist_27$test)
confusionMatrix(y_hat, mnist_27$test$y)$overall["Accuracy"]


"When we force this assumption, we can show mathematically that the boundary is a line,
just as with logistic regression. For this reason, we call the method linear discriminant
analysis (LDA). Similarly, for QDA, we can show that the boundary must be a quadratic
function."

# In the case of LDA, the lack of flexibility does not permit us to capture the non-linearity in
# the true conditional probability function.

"https://rafalab.github.io/dsbook/book_files/figure-html/lda-estimate-1.png" %>%
   load.image() %>% plot(axes = F)





# Case study: more than three classes -------------------------------------

"Generate an example with 3 categories from the digits data"
if(!exists("mnist")) mnist <- read_mnist()
index_123 = sample(which(mnist$train$labels %in% c(1, 2, 3)), 2000)
x = mnist$train$images[index_123, ]
y = mnist$train$labels[index_123]
"Partition the data in training/testing sets"
index_train = createDataPartition(y, p = 0.8, list = F)
"Generate the quadrants that will become the new predictors"
row_column = expand.grid(row = 1:28, col = 1:28)
upper_left_ind = which(row_column$col <= 14 & row_column$row <= 14)
lower_right_ind = which(row_column$col > 14 & row_column$row > 14)
"Binarize following the rule : above 200 is ink"
x = x > 200; x
"Compute the proportion of pixels in the quadrant i.e. new predictors"
x = cbind(rowSums(x[, upper_left_ind]) / rowSums(x),
          rowSums(x[, lower_right_ind]) / rowSums(x))
head(x)
"Create sets"
train = data.frame(y = factor(y[index_train]),
                   x_1 = x[index_train, 1], 
                   x_2 = x[index_train, 2])
test = data.frame(y = factor(y[-index_train]),
                       x_1 = x[-index_train,1], 
                       x_2 = x[-index_train,2])

# Plot the training dataset
train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

"QDA model"
train_qda = train(y~., method = "qda", data = train)
y_hat_qda = predict(train_qda, test)
confusionMatrix(y_hat_qda, test$y)$overall["Accuracy"]

# Note that for sensitivity and specificity, we have a pair of values for each class. To define
# these terms, we need a binary outcome. We therefore have three columns: one for each class
# as the positives and the other two as the negatives.
# To visualize what parts of the region are called 1, 2, and 7 we now need three colors

"https://rafalab.github.io/dsbook/book_files/figure-html/three-classes-plot-1.png" %>%
   load.image() %>% plot(axes = F)

# The accuracy for LDA, 0.629, is much worse because the model is more rigid. This is what
# the decision rule looks like:

"https://rafalab.github.io/dsbook/book_files/figure-html/lda-too-rigid-1.png" %>%
   load.image() %>% plot(axes = F)

# For Knn after optimisation of k is better because it is not quadratic
# or linear i.e. more adaptative
train_knn <- train(y ~ ., method = "knn", data = train,
                   tuneGrid = data.frame(k = seq(15, 51, 2)))
y_hat_knn = predict(train_knn, test)
confusionMatrix(y_hat_knn, test$y)$overall["Accuracy"]
# The decision rule looks like
"https://rafalab.github.io/dsbook/book_files/figure-html/three-classes-knn-better-1.png" %>%
   load.image() %>% plot(axes = F)


"Note that one of the limitations of generative models here is due to the lack of fit of the
normal assumption, in particular for class 1."
train %>% mutate(y = factor(y)) %>%
   ggplot(aes(x_1, x_2, fill = y, color=y)) +
   geom_point(show.legend = FALSE) +
   stat_ellipse(type="norm")
"Generative models can be very powerful, but only when we are able to successfully approximate
the joint distribution of predictors conditioned on each class."








# Assessment --------------------------------------------------------------

"Create a dataset of samples from just cerebellum and hippocampus, two parts of the brain, 
and a predictor matrix with 10 randomly selected columns using the following code:"
data("tissue_gene_expression")
set.seed(1993, sample.kind = "Rounding")
ind = which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y = droplevels(tissue_gene_expression$y[ind])
x = tissue_gene_expression$x[ind, ]
x = x[, sample(ncol(x), 10)]

"Use the train() function to estimate the accuracy of LDA. For this question, use 
the version of x and y created with the code above: do not split them or 
tissue_gene_expression into training and test sets. Report the accuracy from the 
train() results (do not make predictions)."
data = cbind(y, x) %>% as.data.frame() %>%
   mutate(y = factor(y))
train_lda = train(y ~ ., method = "lda", data = data)
train_lda

"In this case, LDA fits two 10-dimensional normal distributions. Look at the fitted model
by looking at the finalModel component of the result of train(). Notice there is a component
called means that includes the estimated means of both distributions. Plot the mean vectors
against each other and determine which predictors (genes) appear to be driving the algorithm."
matrix(train_lda$finalModel$means, 10, 2, byrow = T) %>% 
   as.data.frame() %>%
   rename(means1 = V1, means2 = V2) %>% 
   mutate(genes = train_lda$finalModel$means %>% colnames()) %>%
   ggplot(aes(means1, means2, color = genes, label = genes)) +
   geom_point(size = 3) +
   ggrepel::geom_text_repel()

"Same for QDA"
train_qda = train(y ~ ., method = "qda", data = data)
train_qda
matrix(train_qda$finalModel$means, 10, 2, byrow = T) %>% 
   as.data.frame() %>%
   rename(means1 = V1, means2 = V2) %>% 
   mutate(genes = train_lda$finalModel$means %>% colnames()) %>%
   ggplot(aes(means1, means2, color = genes, label = genes)) +
   geom_point(size = 3) +
   ggrepel::geom_text_repel()

"One thing we saw in the previous plots is that the values of the predictors correlate in both
groups: some predictors are low in both groups and others high in both groups. The mean value 
of each predictor found in colMeans(x) is not informative or useful for prediction and often 
for purposes of interpretation, it is useful to center or scale each column. This can be achieved
with the preProcess argument in train(). Re-run LDA with preProcess = "'center'". Note that accuracy
does not change, but it is now easier to identify the predictors that differ more between groups
than based on the plot made in Q2."
train_lda = train(y ~ ., method = "lda", data = data, preProcess = "center")
train_lda
matrix(train_lda$finalModel$means, 10, 2, byrow = T) %>% 
   as.data.frame() %>%
   rename(means1 = V1, means2 = V2) %>% 
   mutate(genes = train_lda$finalModel$means %>% colnames()) %>%
   ggplot(aes(means1, means2, color = genes, label = genes)) +
   geom_point(size = 3) +
   ggrepel::geom_text_repel()
# answers are extremes

"Now we are going to increase the complexity of the challenge slightly. Repeat the LDA 
analysis from Q5 but using all tissue types. Use the following code to create your dataset:"
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

data = cbind(y, x) %>% as.data.frame() %>%
   mutate(y = factor(y))
train_lda = train(y ~ ., method = "lda", data = data, preProcess = "center")
train_lda











