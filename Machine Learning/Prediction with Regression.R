rm(list = ls())
x = c("tidyverse", "ggplot2", "imager", "broom", "caret", "dslabs")
lapply(x, library, character.only = TRUE)
options(digits = 3)





# Case study: is it a 2 or a 7? -------------------------------------------

"Let’s go back to the digits example in which we had 784 predictors. For illustrative purposes,
we will start by simplifying this problem to one with two predictors and two classes.
Specifically, we define the challenge as building an algorithm that can determine if a digit is
a 2 or 7 from the predictors. We are not quite ready to build algorithms with 784 predictors,
so we will extract two simple predictors from the 784: the proportion of dark pixels that are
in the upper left quadrant (X1) and the lower right quadrant (X2)."

# We then select a random sample of 1,000 digits, 500 in the training set and 500 in the test
# set. We provide this dataset in the dslabs package:
data("mnist_27")
# We can explore the data by plotting the two predictors and using colors to denote the labels:
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

# We can immediately see some patterns. For example, if X1 (the upper left panel) is very
# large, then the digit is probably a 7. Also, for smaller values of X1, the 2s appear to be in
# the mid range values of X2.

# These are the images of the digits with the largest and smallest values for X1: And here are
# the original images corresponding to the largest and smallest value of X2:
"https://rafalab.github.io/dsbook/book_files/figure-html/two-or-seven-images-large-x1-1.png" %>%
   load.image() %>% plot(axes = F)
# We can start getting a sense for why these predictors are useful, but also why the problem
# will be somewhat challenging.

head(mnist_27$train)

fit <- mnist_27$train %>%
   mutate(y = ifelse(y==7, 1, 0)) %>%
   lm(y ~ x_1 + x_2, data = .)

"We can now build a decision rule based on the estimate of ˆp(x1, x2):"

tidy(fit) # upperleft quadrant increase the proba to have a 7, vice versa

p_hat = predict(fit, newdata = mnist_27$test) # gives the probability for each element to be a 7
y_hat = factor(ifelse(p_hat > 0.5, 7, 2)) #♣ if proba > 50% it's a 7

confusionMatrix(y_hat, mnist_27$test$y)$overall[["Accuracy"]] # compare y to y_hat
"We get an accuracy well above 50%. Not bad for our first try. But can we do better?"




"Because we constructed the mnist_27 example and we had at our disposal 60,000 digits in
just the MNIST dataset, we used this to build the true conditional distribution p(x1, x2).

Keep in mind that this is something we don’t have access to in practice, but we include it
in this example because it permits the comparison of ˆp(x1, x2) to the true p(x1, x2). This
comparison teaches us the limitations of different algorithms. Let’s do that here. We have
stored the true p(x1, x2) in the mnist_27 object and can plot the image using the ggplot2
function geom_raster(). 

We choose better colors and use the stat_contour function to
draw a curve that separates pairs (x1, x2) for which p(x1, x2) > 0.5 and pairs for which
p(x1, x2) < 0.5:"

mnist_27$true_p %>% ggplot(aes(x_1, x_2, z = p, fill = p)) +
   geom_raster() +
   scale_fill_gradientn(colors = c("#F8766D", "white", "#00BFC4")) +
   stat_contour(breaks = c(0.5), color = "black")

# Above you see a plot of the true p(x, y). To start understanding the limitations of linear
# regression here, first note that with linear regression ˆp(x, y) has to be a plane, and as a
# result the boundary defined by the decision rule is given by: ˆp(x, y) = 0.5, which implies the
# boundary can’t be anything other than a straight line:
"β0 + β1x1 + β2x2 = 0.5 =⇒ x2 = (0.5 − β0) / β2 − β1 / β2x1"



"Note that for this boundary, x2 is a linear function of x1. This implies that our linear
regression approach has no chance of capturing the non-linear nature of the true p(x1, x2).
Below is a visual representation of ˆp(x1, x2). We used the squish function from the scales
package to constrain estimates to be between 0 and 1. We can see where the mistakes were
made by also showing the data and the boundary. They mainly come from low values x1
that have either high or low value of x2. Regression can’t catch this."

"https://rafalab.github.io/dsbook/book_files/figure-html/regression-p-hat-1.png" %>%
   load.image() %>% plot(axes = F)






# Assessment Linear Regression --------------------------------------------

set.seed(1, sample.kind = "Rounding") 
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
   data.frame() %>% setNames(c("x", "y"))

"We will build 100 linear models using the data above and calculate the mean and standard 
deviation of the combined models."
set.seed(1, sample.kind = "Rounding") 
RMSE = replicate(100, {
   ind = createDataPartition(dat$y, list = F, p = 0.5)
   test = dat[ind, ]; train = dat[-ind, ]
   
   fit = lm(y ~ x, data = train)
   forecast = predict(fit, newdata = test)
   # c = fit$coefficients
   # forecast = c[1] + c[2] * test$x
   
   mean((forecast - test$y)^2) %>% sqrt()
   
})
mean(RMSE)
sd(RMSE)


"Now we will repeat the exercise above but using larger datasets."
perf_lm = function(n) 
{
   Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
   dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
      data.frame() %>% setNames(c("x", "y"))
   
   RMSE = replicate(100, {
      ind = createDataPartition(dat$y, list = F, p = 0.5)
      test = dat[ind, ]; train = dat[-ind, ]
      
      fit = lm(y ~ x, data = train)
      forecast = predict(fit, newdata = test)
      # c = fit$coefficients
      # forecast = c[1] + c[2] * test$x
      
      mean((forecast - test$y)^2) %>% sqrt()
   })
   
   data.frame(mean = mean(RMSE), sd = sd(RMSE))
}

set.seed(1, sample.kind = "Rounding")
n <- c(100, 500, 1000, 5000, 10000)
sapply(n, perf_lm)

"Q3 What happens to the RMSE as the size of the dataset becomes larger?"
On average, the RMSE does not change much as n gets larger, but the variability 
of the RMSE decreases.

"Q4 Now repeat the exercise from Q1, this time making the correlation between x and y 
larger, as in the following code:"
set.seed(1, sample.kind = "Rounding")
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
   data.frame() %>% setNames(c("x", "y"))
set.seed(1, sample.kind = "Rounding") 
RMSE = replicate(100, {
   ind = createDataPartition(dat$y, list = F, p = 0.5)
   test = dat[ind, ]; train = dat[-ind, ]
   
   fit = lm(y ~ x, data = train)
   forecast = predict(fit, newdata = test)
   # c = fit$coefficients
   # forecast = c[1] + c[2] * test$x
   
   mean((forecast - test$y)^2) %>% sqrt()
   
})
mean(RMSE)
sd(RMSE)

"Q5 Which of the following best explains why the RMSE in question 4 is so much lower 
than the RMSE in question 1?"
When we increase the correlation between x and y, x has more predictive power and thus 
provides a better estimate of y.

"Q6-7 Create a data set using the following code."
set.seed(1, sample.kind = "Rounding")
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
   data.frame() %>% setNames(c("y", "x_1", "x_2"))
# Note that y is correlated with both x_1 and x_2 but the two predictors are independent
# of each other, as seen by 
cor(dat)
"Set the seed to 1, then use the caret package to partition into test and training sets 
with p = 0.5. Compare the RMSE when using just x_1, just x_2 and both x_1 and x_2. Train
a single linear model for each"
set.seed(1, sample.kind = "Rounding")
ind = createDataPartition(dat$y, list = F, p = 0.5)
test = dat[ind, ]; train = dat[-ind, ]
# X1
fit = lm(y ~ x_1, data = train)
forecast = predict(fit, newdata = test)
mean((forecast - test$y)^2) %>% sqrt()
# X2
fit = lm(y ~ x_2, data = train)
forecast = predict(fit, newdata = test)
mean((forecast - test$y)^2) %>% sqrt()
# X1 + X2
fit = lm(y ~ x_1 + x_2, data = train)
forecast = predict(fit, newdata = test)
mean((forecast - test$y)^2) %>% sqrt()

"Q8 Repeat the exercise from Q6 but now create an example in which x_1 and x_2 are
highly correlated."
set.seed(1, set.seed(1, sample.kind = "Rounding"))
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
   data.frame() %>% setNames(c("y", "x_1", "x_2"))
set.seed(1, sample.kind = "Rounding")
ind = createDataPartition(dat$y, list = F, p = 0.5)
test = dat[ind, ]; train = dat[-ind, ]
# X1
fit = lm(y ~ x_1, data = train)
forecast = predict(fit, newdata = test)
mean((forecast - test$y)^2) %>% sqrt()
# X2
fit = lm(y ~ x_2, data = train)
forecast = predict(fit, newdata = test)
mean((forecast - test$y)^2) %>% sqrt()
# X1 + X2
fit = lm(y ~ x_1 + x_2, data = train)
forecast = predict(fit, newdata = test)
mean((forecast - test$y)^2) %>% sqrt()
"Compare the results from Q6 and Q8. What can you conclude?"
Adding extra predictors can improve RMSE substantially, but not when the added 
predictors are highly correlated with other predictors.
"!!!!! since predictors are correlated, one of them may be predicting the other which can be 
solved using IV regression, or the predictors are pretty much the same"







# Assessment Logistic Regression ------------------------------------------

"Define a dataset using the following code:"
set.seed(2, sample.kind="Rounding")
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
   
   y <- rbinom(n, 1, p) # binary variable
   f_0 <- rnorm(n, mu_0, sigma_0) # first type of individual
   f_1 <- rnorm(n, mu_1, sigma_1) # second type
   x <- ifelse(y == 1, f_1, f_0) # y is a categorical variable (e.g. gender)
   
   test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
   
   list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
        test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

# Note that we have defined a variable x that is predictive of a binary outcome y:
dat$train %>% ggplot(aes(x, color = y)) + geom_density()

"Set the seed to 1, then use the make_data() function defined above to generate 25
different datasets with mu_1 <- seq(0, 3, len=25). Perform logistic regression on 
each of the 25 different datasets (predict 1 if p>0.5) and plot accuracy (res in the
figures) vs mu_1 (delta in the figures).”"
set.seed(1, sample.kind="Rounding")
mu_1 = seq(0, 3, len = 25)
res = c()
for(i in 1:25)
{
   data = make_data(mu_1 = mu_1[i])
   fit = data[[1]] %>%
      glm(y~x, family = binomial(link = "logit"), data = .)
   y_hat = predict(fit, newdata = data[[2]])
   y_hat = ifelse(y_hat > 0.5, 1, 0)
   res[i] = confusionMatrix(factor(y_hat, levels = c(0,1)), data[[2]]$y)$overall[["Accuracy"]]
}
plot(mu_1, res)




































