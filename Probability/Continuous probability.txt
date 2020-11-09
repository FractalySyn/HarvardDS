rm(list = ls())
options(digits = 4)
library(imager); library(gtools); library(tidyverse); library(ggplot2)
library(dslabs)



# CDF ---------------------------------------------------------------------


## Just as when using distributions to summarize numeric data, it is much more practical to define a function that operates on intervals 
## rather than single values. The standard way of doing this is using the cumulative distribution function (CDF).
"CDF = répartition"

## As an example, we earlier defined the height distribution for adult male students. Here, we define the vector x to contain these heights:
data("heights")
x = heights %>% filter(sex == "Male") %>% pull(height)

f = function(a) mean(x <= a) # which, for any value a, gives the proportion of values in the list x that are smaller or equal than a.
##  if I pick one of the male students at random, what is the chance that he is at least 70.5 inches?
1 - f(70.5)



# Theoretical continuous distributions ------------------------------------

## We say that a random quantity is normally distributed with average m and standard deviation s if its probability distribution is 
## defined by:
"F(a) = pnorm(a, m, s)"

## This is useful because if we are willing to use the normal approximation for, say, height, we don't need the entire dataset to answer 
## questions such as: what is the probability that a randomly selected student is taller then 70 inches? We just need the average height 
## and standard deviation:
1 - pnorm(70.5, mean(x), sd(x))


"Theoretical distributions as approximations"
## Data is always, technically speaking, discrete. For example, we could consider our height data categorical with each specific height 
## a unique category. The probability distribution is defined by the proportion of students reporting each height. Here is a plot of that
## probability distribution:
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/plot-of-height-frequencies-1.png"))
## While most students rounded up their heights to the nearest inch, others reported values with more precision. One student reported his
## height to be 69.6850393700787, which is 177 centimeters. The probability assigned to this height is 0.001 or 1 in 812. The probability 
## for 70 inches is much higher at 0.106, but does it really make sense to think of the probability of being exactly 70 inches as being
## different than 69.6850393700787? Clearly it is much more useful for data analytic purposes to treat this outcome as a continuous numeric
## variable, keeping in mind that very few people, or perhaps none, are exactly 70 inches, and that the reason we get more values at 70 is
## because people round to the nearest inch.

## In cases like height, in which the data is rounded, the normal approximation is particularly useful if we deal with intervals that include
## exactly one round number. For example, the normal distribution is useful for approximating the proportion of students reporting values in
## intervals like the following three:
mean(x <= 68.5) - mean(x <= 67.5)
#> [1] 0.115
mean(x <= 69.5) - mean(x <= 68.5)
#> [1] 0.119
mean(x <= 70.5) - mean(x <= 69.5)
#> [1] 0.122
"Note how close we get with the normal approximation:"; m = mean(x); s = sd(x)
pnorm(68.5, m, s) - pnorm(67.5, m, s) 
#> [1] 0.103
pnorm(69.5, m, s) - pnorm(68.5, m, s) 
#> [1] 0.11
pnorm(70.5, m, s) - pnorm(69.5, m, s) 
#> [1] 0.108
"However, the approximation is not as useful for other intervals. otice how the approximation breaks down when we try to estimate:"
mean(x <= 70.9) - mean(x<=70.1)
#> [1] 0.0222
pnorm(70.9, m, s) - pnorm(70.1, m, s)
#> [1] 0.0836
"In general, we call this situation discretization. Although the true height distribution is continuous, the reported heights tend to be
 more common at discrete values, in this case, due to rounding. As long as we are aware of how to deal with this reality, the normal 
 approximation can still be a very useful tool."



# Monte Carlo simulations for continuous variables ------------------------

## how we could generate data that looks like our reported heights:
n = length(x); m = mean(x); s = sd(x)
simulated_heights = rnorm(n, m, s)
hist(simulated_heights, border = "white", col = "black")

"Rnorm() is one of the most useful functions in R as it will permit us to generate data that mimics natural events and answers questions
 related to what could happen by chance by running Monte Carlo simulations."

## If, for example, we pick 800 males at random, what is the distribution of the tallest person? How rare is a seven footer in a group of 
## 800 males? The following Monte Carlo simulation helps us answer that question:
tallest = replicate(10000, {simulated_heights = rnorm(800, m, s)
                            max(simulated_heights)})
mean(tallest >= 7*12) # 1.66 %
hist(tallest, border = "white", col = "black")



# Continuous distributions ------------------------------------------------

"We introduced the normal distribution in Section 8.8 and used it as an example above. The normal distribution is not the only useful
 theoretical distribution. Other continuous distributions that we may encounter are the student-t, Chi-square, exponential, gamma, beta,
 and beta-binomial. R provides functions to compute the density, the quantiles, the cumulative distribution functions and to generate 
 Monte Carlo simulations. R uses a convention that lets us remember the names, namely using the letters d, q, p, and r in front of a 
 shorthand for the distribution."

## Plot the normal density
x <- seq(-5, 5, length.out = 100)
qplot(x, f, geom = "line", data = data.frame(x, f = dnorm(x)))
## dnorm returns the density for quantiles x applying the gaussian formula



# DataCamp Assessment -----------------------------------------------------

"The distribution of IQ scores is approximately normally distributed. The average is 100 and the standard deviation is 15. Suppose you
 want to know the distribution of the person with the highest IQ in your school district, where 10,000 people are born each year.
 Generate 10,000 IQ scores 1,000 times using a Monte Carlo simulation. Make a histogram of the highest IQ scores."
# The variable `B` specifies the number of times we want the simulation to run.
B <- 1000
# Use the `set.seed` function to make sure your answer matches the expected result after random number generation.
set.seed(1)
# Create an object called `highestIQ` that contains the highest IQ score from each random distribution of 10,000 people.
highestIQ = replicate(B, max(rnorm(10000, 100, 15)))
# Make a histogram of the highest IQ scores.
hist(highestIQ)




# edX Assessment ---------------------------------------------------------

"The ACT is a standardized college admissions test used in the United States. The four multi-part questions in this assessment all
 involve simulating some ACT test scores and answering probability questions about them.
 For the three year period 2016-2018, ACT standardized test scores were approximately normally distributed with a mean of 20.9 and 
 standard deviation of 5.7. (Real ACT scores are integers between 1 and 36, but we will ignore this detail and use continuous values 
 instead.)"
## First we'll simulate an ACT test score dataset and answer some questions about it. Set the seed to 16, then use rnorm() to generate 
## a normal distribution of 10000 tests with a mean of 20.9 and standard deviation of 5.7. Save these values as act_scores. You'll be 
## using this dataset throughout these four multi-part questions.
set.seed(16, sample.kind = "Rounding")
act_scores = rnorm(10000, 20.9, 5.7)
# What is the mean of act_scores? What is the standard deviation of act_scores?
mean(act_scores); sd(act_scores)
# A perfect score is 36 or greater. In act_scores, how many perfect scores are there out of 10,000 simulated tests?
length(act_scores[which(act_scores >= 36)])
sum(act_scores >= 36)
# In act_scores, what is the probability of an ACT score greater than 30?
length(act_scores[which(act_scores > 30)]) / length(act_scores)
mean(act_scores > 30)
# In act_scores, what is the probability of an ACT score less than or equal to 10?
length(act_scores[which(act_scores <= 10)]) / length(act_scores)
mean(act_scores <= 10)
# Set x equal to the sequence of integers 1 to 36. Use dnorm to determine the value of the probability density function
# over x given a mean of 20.9 and standard deviation of 5.7; save the result as f_x. Plot x against f_x.
x = 1:36
qplot(x, f_x, data = data.frame(x = x, f_x = dnorm(x, 20.9, 5.7)), geom = "line")
# Convert act_scores to Z-scores.
z_scores = scale(act_scores)
# What is the probability of a Z-score greater than 2 (2 standard deviations above the mean)?
mean(z_scores > 2)
# What ACT score value corresponds to 2 standard deviations above the mean (Z = 2)?
mean(act_scores) + 2*sd(act_scores)
# A Z-score of 2 corresponds roughly to the 97.5th percentile. Use qnorm() to determine the 97.5th percentile of normally distributed 
# data with the mean and standard deviation observed in act_scores.
qnorm(0.975, 20.9, 5.7) # ~ mean + 2sd
# Write a function that takes a value and produces the probability of an ACT score less than or equal to that value (the CDF). Apply this
# function to the range 1 to 36.
cdf = function(a) mean(act_scores <= a)
a = 1:36
data.frame(a, sapply(a, cdf))
# Use qnorm() to determine the expected 95th percentile
qnorm(0.95, 20.9, 5.7)
# Make a vector containing the quantiles for p <- seq(0.01, 0.99, 0.01), the 1st through 99th percentiles of the act_scores data. 
p = seq(0.01, 0.99, 0.01)
sample_quantiles = quantile(act_scores, p)
data.frame(p, sample_quantiles)
# In what percentile is a score of 26?
"Your answer should be an integer (i.e. 60), not a percent or fraction. Note that a score between the 98th and 99th percentile should 
 be considered the 98th percentile, answer is 82 = between 82 and 83"
data.frame(p, sample_quantiles)[82:83,]
# Make a corresponding set of theoretical quantiles using qnorm() over the interval p <- seq(0.01, 0.99, 0.01) with mean 20.9 and standard 
# deviation 5.7. Save these as theoretical_quantiles. Make a QQ-plot graphing sample_quantiles on the y-axis versus theoretical_quantiles 
# on the x-axis.
theoretical_quantiles = qnorm(p, 20.9, 5.7)
qplot(theoretical_quantiles, sample_quantiles)





