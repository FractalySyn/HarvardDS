rm(list = ls())
options(digits = 3)
library(imager); library(gtools); library(tidyverse); library(ggplot2); library(dslabs)

## We want to construct an estimate of p using only the information we observe. An estimate should be thought of as a summary of the observed data that we 
## think is informative about the parameter of interest. It seems intuitive to think that the proportion of blue beads in the sample 0.48 must be at least 
## related to the actual proportion p. But do we simply predict p to be 0.48? First, remember that the sample proportion is a random variable. If we run 
## the command take_poll(25) four times, we get a different answer each time, since the sample proportion is a random variable.
take_poll(25)
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/four-simulated-polls-1.png"))
## Note that in the four random samples shown above, the sample proportions range from 0.44 to 0.60. By describing the distribution of this random variable, 
## we will be able to gain insights into how good this estimate is and how we can make it better.


## The ideas presented here on how we estimate parameters, and provide insights into how good these estimates are, extrapolate to many data science tasks.
## For example, we may want to determine the difference in health improvement between patients receiving treatment and a control group. We may ask, what 
## are the health effects of smoking on a population? What are the differences in racial groups of fatal shootings by police? What is the rate of change 
## in life expectancy in the US during the last 10 years? All these questions can be framed as a task of estimating a parameter from a sample.


"Before we continue, let’s make an important clarification related to the practical problem of forecasting the election. If a poll is conducted four months 
 before the election, it is estimating the p for that moment and not for election day. The p for election night might be different since people’s opinions
 fluctuate through time. The polls provided the night before the election tend to be the most accurate since opinions don’t change that much in a day. However,
 forecasters try to build tools that model how opinions vary across time and try to predict the election night results taking into consideration the fact that
 opinions fluctuate. We will describe some approaches for doing this in a later section."



# Estimators of polls --------------------------------------------------------------

"E(X_hat) = p"
"SE(X_hat) = √n.√(p(1-p))"

## This result reveals the power of polls. The expected value of the sample proportion X_bar is the parameter of interest p and we can make the standard 
## error as small as we want by increasing N. The law of large numbers tells us that with a large enough poll, our estimate converges to p.

## If we take a large enough poll to make our standard error about 1%, we will be quite certain about who will win. But how large does the poll have to be
## for the standard error to be this small?

## One problem is that we do not know p, so we can’t compute the standard error. However, for illustrative purposes, let’s assume that p = 0.51 and make a
## plot of the standard error versus the sample size N:
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/standard-error-versus-sample-size-1.png"))
## From the plot we see that we would need a poll of over 10,000 people to get the standard error that low. We rarely see polls of this size due in part 
## to costs. From the Real Clear Politics table, we learn that the sample sizes in opinion polls range from 500-3,500 people. For a sample size of 1,000 
## and p = 0.51, the standard error is:
p = 0.51
sqrt(p*(1-p))/sqrt(1000)





# Central Limit Theorem in practice ---------------------------------------

## Suppose we want to know what is the probability that we are within 1% from p. We can use the normal distribution to answer this.
## One problem we have is that since we don’t know p, we don't know SE(X_hat). But it turns out that the CLT still works if we estimate the standard error
## by using X_hat in place of p. We say that we plug-in the estimate.
"SE[X_hat] = √(X_hat(1-X_hat)/N)"
avg = 0.48; se = 0.01
## Confidence interval of +/- 1%
min_1 = avg - qnorm(0.01)*se
max_1 = avg + qnorm(0.01)*se
## Probability to be inside [0.47, 0.49] = 0.01 from avg
pnorm(0.01/se) - pnorm(-0.01/se)
plot(load.image("https://github.com/FractalySyn/harvardXdatascience/raw/master/2020-04-22_16h07_37.png"))

## Pollsters often include a Marging of Error (MoE) that is 2 or 1.96 standard errors for a bilateral 95% confidence level
"MoE = 2*sd/sqrt(n) = 2*se"
qnorm(0.975)



# Monte Carlo simulation for the CLT --------------------------------------

## Let p be an arbitrary value and let's see what should be the sample size N to converge to it
p = 0.3; n = 10^seq(1, 6, length = 100)
avg = c(); se = c()
for(j in 1:100){
  x = sample(c(0,1), n[j], replace = T, prob = c(1-p, p))
  avg[j] = mean(x)
  se[j] = sqrt(p*(1-p)/n[j])
}
plot(avg ~ log10(n), type = "l")
plot(se ~ log10(n), type = "l")
## It becomes really interesting ffrom 10000 observations

## For realistic values of p, say from 0.35 to 0.65, if we run a very large poll with 100,000 people, theory tells us that we would predict the election 
## perfectly since the largest possible margin of error is around 0.3%. Why don't we run a very large poll ?
## One reason is that running such a poll is very expensive. Another possibly more important reason is that theory has its limitations. Polling is much 
## more complicated than picking beads from an urn. Some people might lie to pollsters and others might not have phones. But perhaps the most important 
## way an actual poll differs from an urn model is that we actually don’t know for sure who is in our population and who is not. How do we know who is 
## going to vote? Are we reaching all possible voters? Hence, even if our margin of error is very small, it might not be exactly right that our expected 
## value is p. We call this bias. Historically, we observe that polls are indeed biased, although not by that much. The typical bias appears to be about 
## 1-2%. This makes election forecasting a bit more interesting and we will talk about how to model this in a later chapter.




# Assignement : Parameters and estimates ----------------------------------

"Suppose you poll a population in which a proportion p of voters are Democrats and 1−p are Republicans. Your sample size is N=25. Consider the random 
 variable S, which is the total number of Democrats in your sample."
# What is the expected value of this random variable S?
E[S] = 25.p
# What is the standard error of S?
SE[S] = sqrt(25).sqrt(p(1-p))
"Consider the random variable S/N, which is equivalent to the sample average that we have been denoting as X_bar."
# What is the expected value of X_bar?
X_bar = S/N
E[X_bar] = (N.p)/N = p
# What is the standard error of the sample average, X_bar?
SE[X_bar] = sqrt(p(1-p))/sqrt(N)

"Write a line of code that calculates the standard error se of a sample average when you poll 25 people in the population. Generate a sequence of 100 
 proportions of Democrats p that vary from 0 (no Democrats) to 1 (all Democrats)."
# `N` represents the number of people polled
N <- 25
# Create a variable `p` that contains 100 proportions ranging from 0 to 1 using the `seq` function
p = seq(0, 1, length = 100)
# Create a variable `se` that contains the standard error of each sample average
se = sqrt(p*(1-p)/N)
# Plot `p` on the x-axis and `se` on the y-axis
plot(p, se)

"Using the same code as in the previous exercise, create a for-loop that generates three plots of p versus se when the sample sizes equal N=25, N=100, 
 and N=1000."
# The vector `p` contains 100 proportions of Democrats ranging from 0 to 1 using the `seq` function
p <- seq(0, 1, length = 100)
# The vector `sample_sizes` contains the three sample sizes
sample_sizes <- c(25, 100, 1000)
# Write a for-loop that calculates the standard error `se` for every value of `p` for each of the three samples sizes `N` in the vector `sample_sizes`. 
# Plot the three graphs, using the `ylim` argument to standardize the y-axis across all three plots.
for(i in 1:3)
{
  se = sqrt(p*(1-p)/sample_sizes[i])
  plot(p, se, ylim = c(0, 0.1))
}

"Our estimate for the difference in proportions of Democrats and Republicans is d=X¯−(1−X¯)"
## Which derivation correctly uses the rules we learned about sums of random variables and scaled random variables to derive the expected value of d?
E[X¯−(1−X¯)]= E[2X¯−1] = 2E[X¯]−1 = 2p−1 = p−(1−p)
## Which derivation correctly uses the rules we learned about sums of random variables and scaled random variables to derive the standard error of d?
SE[X¯−(1−X¯)]= SE[2X¯−1] = 2SE[X¯] = 2sqrt(p(1−p)/N) ## 2.se it's logical

"Say the actual proportion of Democratic voters is p=0.45. In this case, the Republican party is winning by a relatively large margin of d=−0.1, or a
 10% margin of victory. What is the standard error of the spread 2X¯−1 in this case?"
# `N` represents the number of people polled
N <- 25
# `p` represents the proportion of Democratic voters
p <- 0.45
# Calculate the standard error of the spread. Print this value to the console.
2*sqrt(p*(1-p)/N)




# Assignment : CLT in practice --------------------------------------------

"Write function called take_sample that takes the proportion of Democrats p and the sample size N as arguments and returns the sample average of Democrats 
 (1) and Republicans (0)."
## Calculate the sample average if the proportion of Democrats equals 0.45 and the sample size is 100.
# Write a function called `take_sample` that takes `p` and `N` as arguements and returns the average value of a randomly sampled population.
take_sample = function(p, N)
{
  votes = sample(c(1,0), N, replace = T, prob = c(p, 1-p))
  mean(votes)
}
set.seed(1)
p <- 0.45
N <- 100
# Call the `take_sample` function to determine the sample average
take_sample(p, N)
# Create an objected called `errors` that replicates subtracting the result of the `take_sample` function from `p` for `B` replications
B = 10000
errors = p - replicate(B, take_sample(p, N))
# Calculate the mean of the errors. Print this value to the console.
mean(errors)

## The errors object has already been loaded for you. Use the hist function to plot a histogram of the values contained in the vector errors. Which statement
## best describes the distribution of the errors?
hist(errors)
"The errors are symmetrically distributed around 0."

"The error p−X¯ is a random variable. In practice, the error is not observed because we do not know the actual proportion of Democratic voters, p. However, we
 can describe the size of the error by constructing a simulation."
## What is the average size of the error if we define the size by taking the absolute value ∣p−X¯∣ ?
# Calculate the mean of the absolute value of each simulated error. Print this value to the console.
mean(abs(errors))

"As we have discussed, the standard error is the square root of the average squared distance (X¯−p)2. The standard deviation is defined as the square root of 
 the distance squared."
# Calculate the standard deviation of the spread.
sqrt(mean(errors^2))
# The theory we just learned tells us what this standard deviation is going to be because it is the standard error of X¯.
# Calculate the standard error
sqrt(p*(1-p)/N)

"In practice, we don't know p, so we construct an estimate of the theoretical prediction based by plugging in X¯ for p. Calculate the standard error of the estimate:"
# Define `X` as a random sample of `N` voters with a probability of picking a Democrat ('1') equal to `p`
X = sample(c(1,0), N, replace = T, prob = c(p, 1-p))
# Define `X_bar` as the average sampled proportion
X_bar = mean(X)
# Calculate the standard error of the estimate. Print the result to the console.
sqrt(X_bar*(1-X_bar)/N)


"The theoretical result gives us an idea of how large a sample size is required to obtain the precision we need. Earlier we learned that the largest standard errors 
 occur for p=0.5."
N <- seq(100, 5000, len = 100)
p <- 0.5
se <- sqrt(p*(1-p)/N)
plot(se~N)
abline(h = 0.01)
# Create a plot of the largest standard error for N ranging from 100 to 5,000. Based on this plot, how large does the sample size have to be to have a standard error 
# of about 1%?
2500
# For N=100, the central limit theorem tells us that the distribution of X^ is...
"approximately normal with expected value p and standard error sqrt(p(1−p)/N)"
# The errors X¯−p are:
"approximately normal with expected value 0 and standard error sqrt(p(1−p)/N)"

"Make a qq-plot of the errors you generated previously to see if they follow a normal distribution."
qqnorm(errors); qqline(errors)

"If p=0.45 and N=100, use the central limit theorem to estimate the probability that X¯>0.5."
# Calculate the probability that the estimated proportion of Democrats in the population is greater than 0.5. Print this value to the console.
p = 0.45; N = 100
1 - pnorm(0.5, p, sqrt(p*(1-p)/N))

"Assume you are in a practical situation and you don't know p. Take a sample of size N=100 and obtain a sample average of X¯=0.51."
X_hat <- 0.51
# Define `se_hat` as the standard error of the sample average
se_hat = sqrt(X_hat*(1-X_hat)/N)
# Calculate the probability that the error is 0.01 or larger
1 - (pnorm(0.01, 0, se_hat) - pnorm(-0.01, 0, se_hat))
















































