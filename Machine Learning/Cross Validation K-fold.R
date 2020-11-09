rm(list = ls())
libs = c("tidyverse", "ggplot2", "imager", "broom", "caret", "dslabs", 
         "matrixStats", "purrr")
lapply(libs, library, character.only = TRUE)
options(digits = 3)







# Mathematical description of cross validation ----------------------------

"In Section 27.4.8, we described that a common goal of machine learning is to find an algorithm
that produces predictors ˆ Y for an outcome Y that minimizes the MSE:
      MSE = E[(1/N) * sum((^Yi - Yi)^2)]

When all we have at our disposal is one dataset, we can estimate the MSE with the observed
MSE like this:
      ^MSE = (1/N) * sum((^yi - yi)^2) 

These two are often referred to as the true error and apparent error, respectively"

# There are two important characteristics of the apparent error we should always keep in
# mind:
#    1. Because our data is random, the apparent error is a random variable
#    2. If we train an algorithm on the same dataset that we use to compute the apparent
#    error, we might be overtraining. In general, when we do this, the apparent error
#    will be an underestimate of the true error


"Cross validation is a technique that permits us to alleviate both these problems. To understand
cross validation, it helps to think of the true error, a theoretical quantity, as the
average of many apparent errors obtained by applying the algorithm to B new random samples
of the data, none of them used to train the algorithm. As shown in a previous chapter,
we think of the true error as:
   (1/B) * sum(MSE)
   
with B a large number that can be thought of as practically infinite. As already mentioned,
this is a theoretical quantity because we only have available one set of outcomes: y1, . . . , yn.
Cross validation is based on the idea of imitating the theoretical setup above as best we can
with the data we have. To do this, we have to generate a series of different random samples.
There are several approaches we can use, but the general idea for all of them is to randomly
generate smaller datasets that are not used for training, and instead used to estimate the
true error."





# K-fold cross validation -------------------------------------------------

"K-fold refers to the number of times we divide the dataset. We will use each of these k pieces
to test our machine learning model after training the model on the other pieces put together."

# We usually try to select a small piece of the dataset so that we have as much data as
# possible to train. However, we also want the test set to be large so that we obtain a stable
# estimate of the loss without fitting an impractical number of models. Typical choices are to
# use 10%-20% of the data for testing.

"https://rafalab.github.io/dsbook/ml/img/cv-3.png" %>%
   load.image() %>% plot(axes = F)

"Now this presents a new problem because for most machine learning algorithms we need to
select parameters, for example the number of neighbors k in k-nearest neighbors. Here, we
will refer to the set of parameters as λ. We need to optimize algorithm parameters without
using our test set and we know that if we optimize and evaluate on the same dataset, we
will overtrain. This is where cross validation is most useful."

# For each set of algorithm parameters being considered, we want an estimate of the MSE
# and then we will choose the parameters with the smallest MSE. Cross validation provides
# this estimate.

"   MSE(λ) = (1/B) * sum( (1/N) * sum(^yi(λ) - yi)^2 ) "

"Note that a usual practice is not to use the test set. It means that we have to split the
dataset on the training set. The random sample used to calculate the MSE is called validation set"

"https://rafalab.github.io/dsbook/ml/img/cv-5.png" %>%
   load.image() %>% plot(axes = F)

"IF the validation set is a random sample of training set and is composed of M = N/k observations
 the the MSE for this sample b is : ^MSE(λ)b = (1/M) * SE "
# SE = squared errors
"Thus the final estimate of the MSE for this choice of parameters λ is simply the mean of the k
 estimated MSE : ^MSE(λ) = (1/B) * sum(^MSE(λ)b) "
# where B = k



# The job here would be to replicate this computation on different values of λ and operate an
# optimisation that minimizes the loss 
"Another practice is to compute the loss for different machine learning methods in order to select
the most appropriate for the data"



# We have described how to use cross validation to optimize parameters. However, we now
# have to take into account the fact that the optimization occurred on the training data and
# therefore we need an estimate of our final algorithm based on data that was not used to
# optimize the choice. Here is where we use the test set we separated early on:
"https://rafalab.github.io/dsbook/ml/img/cv-6.png" %>%
   load.image() %>% plot(axes = F)
# We can do cross validation again:
"https://rafalab.github.io/dsbook/ml/img/cv-7.png" %>%
   load.image() %>% plot(axes = F)
# and obtain a final estimate of our expected loss. However, note that this means that our
# entire compute time gets multiplied by K. You will soon learn that performing this task
# takes time because we are performing many complex computations. As a result, we are
# always looking for ways to reduce this time. For the final evaluation, we often just use the
# one test set.


"Once we are satisfied with this model and want to make it available to others, we could refit
the model on the entire dataset, without changing the optimized parameters."

"https://rafalab.github.io/dsbook/ml/img/cv-8.png" %>%
   load.image() %>% plot(axes = F)


# Now how do we pick the cross validation K? Large values of K are preferable because the
# training data better imitates the original dataset. However, larger values of K will have much
# slower computation time: for example, 100-fold cross validation will be 10 times slower than
# 10-fold cross validation. For this reason, the choices of K = 5 and K = 10 are popular.

"One way we can improve the variance of our final estimate is to take more samples. To do
this, we would no longer require the training set to be partitioned into non-overlapping sets.
Instead, we would just pick K sets of some size at random.

One popular version of this technique, at each fold, picks observations at random with
replacement (which means the same observation can appear twice). This approach has some
advantages (not discussed here) and is generally referred to as the bootstrap. In fact, this is
the default approach in the caret package. We describe how to implement cross validation
with the caret package in the next chapter."





# Assessment -------------------------------------------------------------

set.seed(1996, sample.kind = "Rounding")
n = 1000; p = 10000
x = matrix(rnorm(n*p), n, p)
colnames(x) = paste("x", 1:ncol(x), sep = "_")
y = rbinom(n, 1, 0.5) %>% factor() # categorical variable

x_subset = x[, sample(1:p, 100)] # randomly selects 100 predictors

"Q1 Because x and y are completely independent, you should not be able to predict y 
using x with accuracy greater than 0.5. Confirm this by running cross-validation using
logistic regression to fit the model. Because we have so many predictors, we selected 
a random sample x_subset. Use the subset when training the model" 
fit = train(x_subset, y, method = "glm")
fit$results
#> acc 0.51



# Now, instead of using a random selection of predictors, we are going to search for those
# that are most predictive of the outcome. We can do this by comparing the values for the
# y=1  group to those in the  y=0  group, for each predictor, using a t-test. You can do 
# perform this step like this:
install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)
"Q2 Which of the following lines of code correctly creates a vector of the p-values called pvals?"
tt$p.value

"Q3 Create an index ind with the column numbers of the predictors that were statistically
significantly associated with y. Use a p-value cutoff of 0.01 to define statistically 
significantly."
ind = which(tt$p.value <= 0.01)
length(ind)

"Q4 Now re-run the cross-validation after redefinining x_subset to be the subset of x defined
by the columns showing statistically significant association with y."
x_subset = x[, ind]
train(x_subset, y, method = "glm")$results

"Q5 Re-run the cross-validation again, but this time using kNN. Try out the following grid
k = seq(101, 301, 25) of tuning parameters. Make a plot of the resulting accuracies."
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

"Q6 In the previous exercises, we see that despite the fact that x and y are completely independent,
we were able to predict y with accuracy higher than 70%. We must be doing something wrong then"
Because we used the entire dataset to select the columns in the model, the accuracy is too high. 
The selection step needs to be included as part of the cross-validation algorithm, and then the
cross-validation itself is performed after the column selection step.
"This means that the predictors selection led to overfitting since it's performed on the entire dataset"
As a follow-up exercise, try to re-do the cross-validation, this time including the selection step
in the cross-validation algorithm. The accuracy should now be close to 50%.

"Q7 Use the train() function with kNN to select the best k for predicting tissue from gene expression 
on the tissue_gene_expression dataset from dslabs. Try k = seq(1,7,2) for tuning parameters. For this
question, do not split the data into test and train sets (understand this can lead to overfitting,
but ignore this for now)."
data("tissue_gene_expression")
train(tissue_gene_expression$x, tissue_gene_expression$y, method = "knn", 
      tuneGrid = data.frame(k = seq(1, 7, 2)))






# Bootstrap ---------------------------------------------------------------

"Suppose the income distribution of your population is as follows"
set.seed(1995, sample.kind = "Rounding")
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))

"The population median is:"
m = median(income); m

# Suppose we don’t have access to the entire population, but want to estimate the median m.
# We take a sample of 100 and estimate the population median m with the sample median M
set.seed(1995, sample.kind = "Rounding")
N = 100
X = sample(income, N)
median(X)


"Because we are simulating the data, we can use a Monte Carlo simulation to learn the
distribution of M."
library(gridExtra)
M = replicate(10000, sample(income, N) %>% median())
p1 = qplot(M, bins = 30, color = I("black"))
p2 = qplot(sample = scale(M), xlab = "theoretical", ylab = "sample")
grid.arrange(p1, p2, ncol = 2)


# If we know this distribution, we can construct a confidence interval. The problem here is
# that, as we have already described, in practice we do not have access to the distribution.
# In the past, we have used the Central Limit Theorem, but the CLT we studied applies to
# averages and here we are interested in the median. We can see that the 95% confidence
# interval based on CLT
median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1)
# is quite different from the confidence interval we would generate if we know the actual
# distribution of M:
quantile(M, c(0.025, 0.975))



"The bootstrap permits us to approximate a Monte Carlo simulation without access to the
entire distribution. The general idea is relatively simple. We act as if the observed sample
is the population. We then sample (with replacement) datasets, of the same sample size as
the original dataset. Then we compute the summary statistic, in this case the median, on
these bootstrap samples."
M_star = replicate(10000, sample(X, N, replace = T) %>% median())
# Note a confidence interval constructed with the bootstrap is much closer to one constructed
# with the theoretical distribution:
quantile(M_star, c(0.025, 0.975))
"The CLT is not appropriate for medians and thus we estimate it using a bootstrap"






# Assessment : Bootstrap --------------------------------------------------

"The createResample() function can be used to create bootstrap samples. For example, 
we can create the indexes for 10 bootstrap samples for the mnist_27 dataset like this:"
data("mnist_27")
set.seed(1995, sample.kind = "Rounding")
indexes = createResample(mnist_27$train$y, 10)

"Q1 How many times do 3, 4, and 7 appear in the first resampled index?"
sum(indexes[[1]] == 4)

"Q2 We see that some numbers appear more than once and others appear no times. This
has to be this way for each dataset to be independent. Repeat the exercise for all 
the resampled indexes.
What is the total number of times that 3 appears in all of the resampled indexes?"
ind = unlist(indexes)
sum(ind == 3)


# Generate a random dataset
y = rnorm(100, 0, 1)
# Estimate the 75th quantile, qnorm(0.75), with the sample quantile: quantile(y, 0.75)
quantile(y, 0.75)

"Q3 Now, set the seed to 1 and perform a Monte Carlo simulation with 10,000 repetitions,
generating the random dataset and estimating the 75th quantile each time. What is the 
expected value and standard error of the 75th quantile?"
set.seed(1, sample.kind = "Rounding")
q75 = replicate(10000, rnorm(100) %>% quantile(0.75))
mean(q75); sd(q75)


"Q4 In practice, we can't run a Monte Carlo simulation. Use the sample:"
set.seed(1, sample.kind = "Rounding")
y = rnorm(100, 0, 1)
"Set the seed to 1 again after generating y and use 10 bootstrap samples to estimate 
the expected value and standard error of the 75th quantile."
set.seed(1, sample.kind = "Rounding")
ind = createResample(y, 10)
x = sapply(ind, function(ind){
   quantile(y[ind], 0.75)
})
mean(x); sd(x)

"Q5 Repeat the exercise from Q4 but with 10,000 bootstrap samples instead of 10. Set the seed to 1"
set.seed(1, sample.kind = "Rounding")
ind = createResample(y, 10000)
x = sapply(ind, function(ind){
   quantile(y[ind], 0.75)
})
mean(x); sd(x)

"Q6"
The bootstrap is particularly useful in situations when we do not have access to the distribution 
or it is unknown.







