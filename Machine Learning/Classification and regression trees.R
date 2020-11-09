rm(list = ls())
libs = c("tidyverse", "ggplot2", "imager", "broom", "caret", "dslabs", 
         "matrixStats", "purrr", "rpart", "randomForest")
lapply(libs, library, character.only = TRUE)
options(digits = 3)





# The curse of dimensionality ---------------------------------------------

"We described how methods such as LDA and QDA are not meant to be used with many
predictors p because the number of parameters that we need to estimate becomes too large.
For example, with the digits example p = 784, we would have over 600,000 parameters with
LDA, and we would multiply that by the number of classes for QDA. Kernel methods such
as kNN or local regression do not have model parameters to estimate. However, they also
face a challenge when multiple predictors are used due to what is referred to as the curse of
dimensionality. The dimension here refers to the fact that when we have p predictors, the
distance between two observations is computed in p-dimensional space"

# A useful way of understanding the curse of dimensionality is by considering how large we have
# to make a span/neighborhood/window to include a given percentage of the data. Remember
# that with larger neighborhoods, our methods lose flexibility.

# For example, suppose we have one continuous predictor with equally spaced points in the
# [0,1] interval and we want to create windows that include 1/10th of data. Then it’s easy to
# see that our windows have to be of size 0.1:
"https://rafalab.github.io/dsbook/book_files/figure-html/curse-of-dim-1.png" %>%
   load.image() %>% plot(axes = F)

# Now, for two predictors, if we decide to keep the neighborhood just as small, 10% for each
# dimension, we include only 1 point. If we want to include 10% of the data, then we need to
# increase the size of each side of the square to √0.10 ≈ 0.316:
"https://rafalab.github.io/dsbook/book_files/figure-html/curse-of-dim-2-1.png" %>%
   load.image() %>% plot(axes = F)

"In general, to include 10% of the data in a case with p dimensions, we need an interval with
each side of size 10^(1/p) of the total. This proportion gets close to 1 quickly, and if the
proportion is 1 it means we include all the data and are no longer smoothing."
p = 1:100
qplot(p, 0.1^(1/p), ylim = c(0,1))

"Here we look at a set of elegant and versatile methods that adapt to higher dimensions and
also allow these regions to take more complex shapes while still producing models that are
interpretable. These are very popular, well-known and studied methods. We will concentrate
on regression and decision trees and their extension to random forests"






# CART motivation ---------------------------------------------------------

"To motivate this section, we will use a new dataset that includes the breakdown of the
composition of olive oil into 8 fatty acids:"
data("olive")
names(olive)
table(olive$region)
# For illustrative purposes, we will try to predict the region using the fatty acid composition
# values as predictors.

"Try Knn"
olive = olive %>% select(-area)
fit = train(region ~ ., method = "knn", data = olive,
            tuneGrid = data.frame(k = seq(1, 15, 2)))
ggplot(fit)

"We see that using just one neighbor, we can predict relatively well. However, a bit of data
exploration reveals that we should be able to do even better. For example, if we look at the
distribution of each predictor stratified by region we see that eicosenoic is only present in
Southern Italy and that linoleic separates Northern Italy from Sardinia."
olive %>% gather(fatty_acid, percentage, -region) %>%
   ggplot(aes(region, percentage, fill = region)) +
   geom_boxplot() +
   facet_wrap(~fatty_acid, scales = "free", ncol = 4) +
   theme(axis.text.x = element_blank(), legend.position = "bottom")

# This implies that we should be able to build an algorithm that predicts perfectly! We can
# see this clearly by plotting the values for eicosenoic and linoleic.
olive %>%
   ggplot(aes(eicosenoic, linoleic, color = region)) +
   geom_point() +
   geom_vline(xintercept = 0.065, lty = 2) +
   geom_segment(x = -0.2, y = 10.54, xend = 0.065, yend = 10.54,
                color = "black", lty = 2)



"We define the following decision rule. If eicosenoic is larger than 0.065, predict
Southern Italy. If not, then if linoleic is larger than 10.535, predict Sardinia, and if lower,
predict Northern Italy. We can draw this decision tree like this:"

"https://rafalab.github.io/dsbook/book_files/figure-html/olive-tree-1.png" %>%
   load.image() %>% plot(axes = F)

# A tree is basically a flow chart of yes or no questions. The general idea of the methods we
# are describing is to define an algorithm that uses data to create these trees with predictions
# at the ends, referred to as nodes. Regression and decision trees operate by predicting an
# outcome variable Y by partitioning the predictors.






# Regression trees --------------------------------------------------------

"When the outcome is continuous, we call the method a regression tree. To introduce regression
trees, we will use the 2008 poll data used in previous sections to describe the basic idea
of how we build these algorithms. As with other machine learning algorithms, we will try
to estimate the conditional expectation f(x) = E(Y |X = x) with Y the poll margin and x
the day."

data("polls_2008")
qplot(day, margin, data = polls_2008)

# The general idea here is to build a decision tree and, at the end of each node, obtain a
# predictor ˆy. A mathematical way to describe this is to say that we are partitioning the
# predictor space into J non-overlapping regions, R1,R2, . . . ,RJ , and then for any predictor
# x that falls within region Rj , estimate f(x) with the average of the training observations yi
# for which the associated predictor xi is also in Rj .

"But how do we decide on the partition R1,R2, . . . ,RJ and how do we choose J? Here is
where the algorithm gets a bit complicated."

# Regression trees create partitions recursively. We start the algorithm with one partition, the
# entire predictor space. In our simple first example, this space is the interval [-155, 1]. But
# after the first step we will have two partitions. After the second step we will split one of
# these partitions into two and will have three partitions, then four, then five, and so on.

"Once we select a partition x to split in order to create the new partitions, we find a predictor
j and value s that define two new partitions, which we will call R1(j, s) and R2(j, s), that
split our observations in the current partition by asking if xj is bigger than s:
    R1(j, s) = {x | xj < s} and R2(j, s) = {x | xj ≥ s}  "

# In our current example we only have one predictor, so we will always choose j = 1, but in
# general this will not be the case. Now, after we define the new partitions R1 and R2, and
# we decide to stop the partitioning, we compute predictors by taking the average of all the
# observations y for which the associated x is in R1 and R2. We refer to these two as ˆyR1 and
# ˆyR2 respectively.

"But how do we pick j and s? Basically we find the pair that minimizes the residual sum of
square (RSS):
   min{j,s} sum(yi - ^yR1)^2 + sum(yi - ^yR2)^2   "

# This is then applied recursively to the new regions R1 and R2.




"Let’s take a look at what this algorithm does on the 2008 presidential election poll data.
We will use the rpart function in the rpart package."
fit = rpart(margin ~ ., data = polls_2008)

# Here, there is only one predictor. Thus we do not have to decide which predictor j to split
# by, we simply have to decide what value s we use to split. We can visually see where the
# splits were made:
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

"The first split is made on day 39.5. One of those regions is then split at day 86.5. The two
resulting new partitions are split on days 49.5 and 117.5, respectively, and so on. We end
up with 8 partitions. The final estimate ˆ f(x) looks like this:"
polls_2008 %>%
   mutate(y_hat = predict(fit)) %>%
   ggplot() +
   geom_point(aes(day, margin)) +
   geom_step(aes(day, y_hat), col = "red")



# Note that the algorithm stopped partitioning at 8. Now we explain how this decision is
# made.

"First we need to define the term complexity parameter (cp). Every time we split and define
two new partitions, our training set RSS decreases. This is because with more partitions,
our model has more flexibility to adapt to the training data. In fact, if you split until every
point is its own partition, then RSS goes all the way down to 0 since the average of one
value is that same value. To avoid this, the algorithm sets a minimum for how much the
RSS must improve for another partition to be added. This parameter is referred to as the
complexity parameter (cp). The RSS must improve by a factor of cp for the new partition to
be added. Large values of cp will therefore force the algorithm to stop earlier which results
in fewer nodes."

# However, cp is not the only parameter used to decide if we should partition a current partition
# or not. Another common parameter is the minimum number of observations required
# in a partition before partitioning it further. The argument used in the rpart function is
# minsplit and the default is 20. The rpart implementation of regression trees also permits
# users to determine a minimum number of observations in each node. The argument is
# minbucket and defaults to round(minsplit/3).

"As expected, if we set cp = 0 and minsplit = 2, then our prediction is as flexible as possible
and our predictor is our original data:"
fit = rpart(margin ~ ., data = polls_2008,
            control = rpart.control(cp = 0, minsplit = 2))
polls_2008 %>%
   mutate(y_hat = predict(fit)) %>%
   ggplot() +
   geom_point(aes(day, margin)) +
   geom_step(aes(day, y_hat), col="red")


# "Intuitively we know that this is not a good approach as it will generally result in overtraining.
# These cp, minsplit, and minbucket, three parameters can be used to control the
# variability of the final predictors. The larger these values are the more data is averaged to
# compute a predictor and thus reduce variability. The drawback is that it restricts flexibility."


"So how do we pick these parameters? We can use cross validation, described in Chapter 29,
just like with any tuning parameter. Here is an example of using cross validation to choose
cp."
train_rpart = train(margin ~ ., method = "rpart", 
              tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),
              data = polls_2008)
ggplot(train_rpart)

# To see the resulting tree, we access the finalModel and plot it:
train_rpart$finalModel %>% plot(margin = 0.1)
train_rpart$finalModel %>% text(cex = 0.75)

polls_2008 %>%
   mutate(y_hat = predict(train_rpart)) %>%
   ggplot() +
   geom_point(aes(day, margin)) +
   geom_step(aes(day, y_hat), col="red")



"Note that if we already have a tree and want to apply a higher cp value, we can use the
prune function. We call this pruning a tree because we are snipping off partitions that do
not meet a cp criterion. We previously created a tree that used a cp = 0 and saved it to
fit. We can prune it like this:"
pruned_fit = prune(fit, cp = 0.01)
polls_2008 %>%
   mutate(y_hat = predict(pruned_fit)) %>%
   ggplot() +
   geom_point(aes(day, margin)) +
   geom_step(aes(day, y_hat), col="red")







# Classification / Decision trees --------------------------------------------

"Classification trees, or decision trees, are used in prediction problems where the outcome is
categorical. We use the same partitioning principle with some differences to account for the
fact that we are now working with a categorical outcome."

# The first difference is that we form predictions by calculating which class is the most common
# among the training set observations within the partition, rather than taking the average in
# each partition (as we can’t take the average of categories).

"The second is that we can no longer use RSS to choose the partition. While we could use
the naive approach of looking for partitions that minimize training error, better performing
approaches use more sophisticated metrics. Two of the more popular ones are the Gini Index
and Entropy."

# In a perfect scenario, the outcomes in each of our partitions are all of the same category
# since this will permit perfect accuracy. The Gini Index is going to be 0 in this scenario, and
# become larger the more we deviate from this scenario. To define the Gini Index, we define
# ˆpj,k as the proportion of observations in partition j that are of class k. The Gini Index is
# defined as
"     Gini(j) = sum(^pj,k*(1-^pj,k))   "
# If you study the formula carefully you will see that it is in fact 0 in the perfect scenario
# described above.

"Entropy is a very similar quantity, defined as
         entropy(j) = - sum(^pj,k*log(^pj,k)) with 0 * log(0) defined as 0      "


train_rpart = train(y ~ ., method = "rpart", 
                    tuneGrid = data.frame(cp = seq(0, 0.1, len = 25)),
                    data = mnist_27$train)
plot(train_rpart)

"The accuracy achieved by this approach is better than what we got with regression, but is
not as good as what we achieved with kernel methods:"
y_hat = predict(train_rpart, mnist_27$test)
confusionMatrix(y_hat, mnist_27$test$y)$overall["Accuracy"]

# The plot of the estimated conditional probability shows us the limitations of classification
# trees:
"https://rafalab.github.io/dsbook/book_files/figure-html/rf-cond-prob-1.png" %>%
   load.image() %>% plot(axes = F) 

# "Note that with decision trees, it is difficult to make the boundaries smooth since each
# partition creates a discontinuity."

"Classification trees have certain advantages that make them very useful. They are highly
interpretable, even more so than linear models. They are easy to visualize (if small enough).
Finally, they can model human decision processes and don’t require use of dummy predictors
for categorical variables. On the other hand, the approach via recursive partitioning can
easily over-train and is therefore a bit harder to train than, for example, linear regression
or kNN. Furthermore, in terms of accuracy, it is rarely the best performing method since
it is not very flexible and is highly unstable to changes in training data. Random forests,
explained next, improve on several of these shortcomings."





# Random forests ----------------------------------------------------------

"Random forests are a very popular machine learning approach that addresses the shortcomings
of decision trees using a clever idea. The goal is to improve prediction performance
and reduce instability by averaging multiple decision trees (a forest of trees constructed with
randomness). It has two features that help accomplish this."

# The first step is bootstrap aggregation or bagging. The general idea is to generate many
# predictors, each using regression or classification trees, and then forming a final prediction
# based on the average prediction of all these trees. To assure that the individual trees are
# not the same, we use the bootstrap to induce randomness. These two features combined
# explain the name: the bootstrap makes the individual trees randomly different, and the
# combination of trees is the forest. The specific steps are as follows.

"
1. Build B decision trees using the training set. We refer to the fitted models as
T1, T2, . . . , TB. We later explain how we ensure they are different.

2. For every observation in the test set, form a prediction ˆyj using tree Tj .

3. For continuous outcomes, form a final prediction with the average ˆy. For
categorical data classification, predict ˆy with majority vote (most frequent class)."

# So how do we get different decision trees from a single training set? For this, we use randomness
# in two ways which we explain in the steps below. Let N be the number of observations
# in the training set. To create Tj , j = 1, . . . ,B from the training set we do the following:

"
1. Create a bootstrap training set by sampling N observations from the training set with
replacement. This is the first way to induce randomness.

2. A large number of features is typical in machine learning challenges. Often, many features
can be informative but including them all in the model may result in overfitting. The second
way random forests induce randomness is by randomly selecting features to be included in
the building of each tree. A different random subset is selected for each tree. This reduces
correlation between trees in the forest, thereby improving prediction accuracy."

# To illustrate how the first steps can result in smoother estimates we will demonstrate by
# fitting a random forest to the 2008 polls data. We will use the randomForest function in
# the randomForest package:
fit = randomForest(margin ~ ., data = polls_2008)

"Note that if we apply the function plot to the resulting object, stored in fit, we see how
the error rate of our algorithm changes as we add trees."
plot(fit)
# We can see that in this case, the accuracy improves as we add more trees until about 30
# trees where accuracy stabilizes.

"The resulting estimate for this random forest can be seen like this:"
polls_2008 %>%
   mutate(y_hat = predict(fit, newdata = polls_2008)) %>%
   ggplot() +
   geom_point(aes(day, margin)) +
   geom_line(aes(day, y_hat), col="red")

# Notice that the random forest estimate is much smoother than what we achieved with the
# regression tree in the previous section. This is possible because the average of many step
# functions can be smooth. We can see this by visually examining how the estimate changes as
# we add more trees. In the following figure you see each of the bootstrap samples for several
# values of b and for each one we see the tree that is fitted in grey, the previous trees that were
# fitted in lighter grey, and the result of averaging all the trees estimated up to that point

"https://github.com/FractalySyn/harvardXdatascience/raw/master/2020-08-18_11h00_09.png" %>%
   load.image() %>% plot(axes = F)

# Here is the random forest fit for our digits example based on two predictors:
train_rf = randomForest(y ~ ., data=mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test),
                mnist_27$test$y)$overall["Accuracy"]

# "Here is what the conditional probabilities look like:"
"https://rafalab.github.io/dsbook/book_files/figure-html/cond-prob-rf-1.png" %>%
   load.image() %>% plot(axes = F)

"Visualizing the estimate shows that, although we obtain high accuracy, it appears that
there is room for improvement by making the estimate smoother. This could be achieved
by changing the parameter that controls the minimum number of data points in the nodes
of the tree. The larger this minimum, the smoother the final estimate will be. We can train
the parameters of the random forest."

nodesize = seq(1, 51, 10)
acc = sapply(nodesize, function(ns){
   train(y ~ ., method = "rf", data = mnist_27$train,
         tuneGrid = data.frame(mtry = 2),
         nodesize = ns)$results$Accuracy
})
qplot(nodesize, acc)

# We can now fit the random forest with the optimized minimun node size to the entire
# training data and evaluate performance on the test data.
train_rf = randomForest(y ~ ., data=mnist_27$train,
                        nodesize = nodesize[which.max(acc)])
confusionMatrix(predict(train_rf, mnist_27$test),
                mnist_27$test$y)$overall["Accuracy"]

# The selected model improves accuracy and provides a smoother estimate
"https://rafalab.github.io/dsbook/book_files/figure-html/cond-prob-final-rf-1.png" %>%
   load.image() %>% plot(axes = F)


"Random forest performs better in all the examples we have considered. However, a disadvantage
of random forests is that we lose interpretability. An approach that helps with
interpretability is to examine variable importance. To define variable importance we count
how often a predictor is used in the individual trees. You can learn more about variable
importance in an advanced machine learning book5. The caret package includes the function
varImp that extracts variable importance from any model in which the calculation is
implemented. We give an example on how we use variable importance in the next section"








# Assessment --------------------------------------------------------------

"Create a simple dataset where the outcome grows 0.75 units on average for every increase 
in a predictor, using this code:"
n = 1000; sigma = 0.25
set.seed(1, sample.kind = "Rounding")
dat = data.frame(x = rnorm(n, 0, 1),
                 y = 0.75 * x + rnorm(n, 0, sigma))

"Q1 Which code correctly uses rpart() to fit a regression tree and saves the result to fit?"
fit = rpart(y ~ ., data = dat)

"Q2 Which of the following plots has the same tree shape obtained in Q1?"
plot(fit); text(fit)

"Q3 Below is most of the code to make a scatter plot of y versus x along with the predicted 
values based on the fit."
dat %>% mutate(y_hat = predict(fit)) %>%
   ggplot() +
   geom_point(aes(x, y)) +
   geom_step(aes(x, y_hat), color = "red", lwd = 2)

"Q4 Now run Random Forests instead of a regression tree using randomForest() from the 
randomForest package, and remake the scatterplot with the prediction line. Part of the 
code is provided for you below."
fit = randomForest(y ~ ., data = dat)
dat %>% mutate(y_hat = predict(fit)) %>%
   ggplot() +
   geom_point(aes(x, y), alpha = 0.5) +
   geom_step(aes(x, y_hat), color = "red")

"Q5 Use the plot() function to see if the Random Forest from Q4 has converged or if we need more trees."
plot(fit)

"Q6 It seems that the default values for the Random Forest result in an estimate that is too 
flexible (unsmooth). Re-run the Random Forest but this time with a node size of 50 and a maximum
of 25 nodes. Remake the plot"
fit = randomForest(y ~ ., data = dat,
                   nodesize = 50, maxnodes = 25)
dat %>% mutate(y_hat = predict(fit)) %>%
   ggplot() +
   geom_point(aes(x, y), alpha = 0.5) +
   geom_step(aes(x, y_hat), color = "red")












 