rm(list = ls())
options(digits = 3)
library(imager); library(gtools); library(tidyverse); library(ggplot2); library(dslabs)


"In this chapter we will demonstrate how poll aggregators, such as FiveThirtyEight, collected and combined data reported by 
 different experts to produce improved predictions. We will introduce ideas behind the statistical models, also known as 
 probability models, that were used by poll aggregators to improve election forecasts beyond the power of individual polls."




# Poll aggregators --------------------------------------------------------

## A few weeks before the 2012 election Nate Silver was giving Obama a 90% chance of winning. How was Mr. Silver so confident?
## We will use a Monte Carlo simulation to illustrate the insight Mr. Silver had and others missed. To do this, we generate 
## results for 12 polls taken the week before the election. We mimic sample sizes from actual polls and construct and report 
## 95% confidence intervals for each of the 12 polls. We save the results from this simulation in a data frame and add a poll 
## ID column.

## polls data
d = 0.039
Ns = c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p = (d+1)/2

## Monte Carlo
"map_df() applies a function to a vector, alternative to sapply(), returns a tibble"
polls = map_df(Ns, function(N) {
  x = sample(c(0,1), size = N, replace = T, prob = c(1-p, p))
  x_hat = mean(x)
  se_hat = sqrt(x_hat*(1-x_hat)/N)
  
  list(estimate = 2*x_hat-1, 
       low = 2*(x_hat-qnorm(0.975)*se_hat),
       high = 2*(x_hat+qnorm(0.975)*se_hat),
       sample_size = N)
})
"seq_along() enumerates 1...n a vector"
polls = polls %>% mutate(poll = seq_along(Ns))
polls
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/simulated-polls-1.png"), axes = F)


## Not surprisingly, all 12 polls report confidence intervals that include the election night result (dashed line). However, all 12 
## polls also include 0 (solid black line) as well. Therefore, if asked individually for a prediction, the pollsters would have to 
## say: it’s a toss-up. Below we describe a key insight they are missing.

"Poll aggregators, such as Nate Silver, realized that by combining the results of different polls you could greatly improve precision.
 By doing this, we are effectively conducting a poll with a huge sample size. We can therefore report a smaller 95% confidence interval
  and a more precise prediction."

## Although as aggregators we do not have access to the raw poll data, we can use mathematics
sum(polls$sample_size) #> 11269
## We can estimate the difference with a weighted average
"Usual average wouldn't be accurate here, therefore we weight each estimate by the sampl size"
d_hat = sum(polls$estimate*polls$sample_size) / sum(polls$sample_size)
d_hat

## Once we have an estimate of d, we can construct an estimate for the proportion voting for Obama, which we can then use to estimate
## the standard error. Once we do this, we see that our margin of error is 0.018.
p_hat = (d_hat+1)/2
se_hat = sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))
MoE = 2*qnorm(0.975)*se_hat; MoE #> 0.018
ic95 = c(d_hat-MoE, d_hat+MoE); ic95
## We get this estimation
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/confidence-coverage-2008-election-1.png"), axes = F)



"Of course, this was just a simulation to illustrate the idea. The actual data science exercise of forecasting elections is much more 
 complicated and it involves modeling. Below we explain how pollsters fit multilevel models to the data and use this to forecast 
 election results. In the 2008 and 2012 US presidential elections, Nate Silver used this approach to make an almost perfect prediction 
 and silence the pundits."




# Poll data ---------------------------------------------------------------

## We use public polling data organized by FiveThirtyEight for the 2016 presidential election
data(polls_us_election_2016)
## The table includes results for national polls, as well as state polls, taken during the year prior to the election. For this first 
## example, we will filter the data to include national polls conducted during the week before the election. We also remove polls that
## FiveThirtyEight has determined not to be reliable and graded with a “B” or less. We include polls that have not been graded.
polls = polls_us_election_2016 %>% filter(state == "U.S." & enddate >= "2016-10-31" &
                                          (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) # !!! parentheses arroung grades !
polls = mutate(polls, spread = rawpoll_clinton/100 - rawpoll_trump/100)

## We have 49 estimates of the spread. We will assume that there are only two parties
attach(polls)
d_hat = sum(spread*samplesize) / sum(samplesize); d_hat
p_hat = (d_hat+1)/2; p_hat
MoE = 2*qnorm(0.975)*sqrt(p_hat*(1-p_hat)/sum(samplesize)); MoE

"So we report a spread of 1.43% with a margin of error of 0.66%. On election night, we discover that the actual percentage was 2.1%, 
 which is outside a 95% confidence interval. What happened?"
## A histogram of the reported spreads shows a problem:
polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color="black", binwidth = .01)
"The data does not appear to be normally distributed and the standard error appears to be larger than 0.007"




# Pollster bias -----------------------------------------------------------

## Notice that various pollsters are involved and some are taking several polls a week:
polls %>% group_by(pollster) %>% summarize(n())
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/pollster-bias-1.png"), axes = F)
## This plot reveals an unexpected result. First, consider that the standard error predicted by theory for each poll is between 0.018 
## and 0.033, which agrees with the within poll variation we see. However, there appears to be differences across the polls. Note, 
## for example, how the USC Dornsife/LA Times pollster is predicting a 4% win for Trump, while Ipsos is predicting a win larger than
## 5% for Clinton. The theory we learned says nothing about different pollsters producing polls with different expected values. 
## All the polls should have the same expected value. 

"FiveThirtyEight refers to these differences as “house effects”. We also call them pollster bias"

"In the following section, rather than use the urn model theory, we are instead going to develop a data-driven model"



# Data-driven models ------------------------------------------------------

## For each pollster, let’s collect their last reported result before the election:
one_poll_per_pollster = polls %>%
  group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()
one_poll_per_pollster
qplot(spread, data = one_poll_per_pollster, binwidth = 0.01)

"The new model can also be thought of as an urn model, although the connection is not as direct. Rather than 0s (Republicans) and 1s
 (Democrats), our urn now contains poll results from all possible pollsters i.e. spreads. We assume that the expected value of our 
 urn is the actual spread d=2p-1"

"Because instead of 0s and 1s, our urn contains continuous numbers between -1 and 1, the standard deviation of the urn is no longer 
 sqrt(p(1-p)). Rather than voter sampling variability, the standard error now includes the pollster-to-pollster variability. Our new 
 urn also includes the sampling variability from the polling. Regardless, this standard deviation is now an unknown parameter. In 
 statistics textbooks, the Greek symbol σ is used to represent this parameter."

"Our task is to estimate d. Because we model the observed values X1...Xn as a random sample from the urn, the CLT might still work 
 in this situation because it is an average of independent random variables. For a large enough sample size N, the probability
 distribution of the sample average X_bar is approximately normal with expected value μ and standard error σ/√(N). If we are willing
 to consider N=15 large enough, we can use this to construct confidence intervals."

## We can use sd() to calculate the standard deviation estimate S
"Unlike for the population standard deviation definition, we now divide by N−1. This makes s a better estimate of σ. There is a 
 mathematical explanation for this, which is explained in most statistics textbooks, but we don’t cover it here."
sd(one_poll_per_pollster$spread)

## We are now ready to form a new confidence interval based on our new data-driven model:
results = one_poll_per_pollster %>%
  summarise(avg = mean(spread), se = sd(spread) / sqrt(length(spread))) %>%
  mutate(start = avg - qnorm(0.975) * se, end = avg + qnorm(0.975) * se) 
round(results*100, 2)
#> 2.9% +/- 1.22
"!!!!! SE = σ/√(N) where sigma is the adjusted standard deviation S = √(∑(Xi-X)/(n-1))"
"The SE is the variability of the Random Variable (here the mean of the spread) on the population, assuming we repeat the calculation
 of the mean many times !!"
"SE is the variability of the mean, not of the data used to calculate the mean, the estimate"
"SE is used for interval calculations, not SD"
"Note that we do not work with proportions anymore, so the specific case doesn't apply"

## Our confidence interval is wider now since it incorporates the pollster variability. It does include the election night result of 
## 2.1%. Also, note that it was small enough not to include 0, which means we were confident Clinton would win the popular vote.

## Are we now ready to declare a probability of Clinton winning the popular vote? Not yet. In our model d is a fixed parameter so we
## can’t talk about probabilities. To provide probabilities, we will need to learn about Bayesian statistics.




# DataCamp Assessment -----------------------------------------------------

"Let's revisit the heights dataset. For now, consider x to be the heights of all males in the data set. 
 Mathematically speaking, x is our population. Using the urn analogy, we have an urn with the values of 
 x in it.
 What are the population average and standard deviation of our population?"
# Load the 'dslabs' package and data contained in 'heights'
library(dslabs)
data(heights)
# Make a vector of heights from all males in the population
x <- heights %>% filter(sex == "Male") %>%
  .$height
# Calculate the population average. Print this value to the console.
mean(x)
# Calculate the population standard deviation. Print this value to the console.
sd(x) ## remember that we consider x to be the population

"Call the population average computed above μ and the standard deviation σ. Now take a sample of size 50, 
 with replacement, and construct an estimate for μ and σ."
set.seed(1)
N <- 50
# Define `X` as a random sample from our population `x`
X = sample(x, 50, replace = T)
# Calculate the sample average. Print this value to the console.
mean(X)
# Calculate the sample standard deviation. Print this value to the console.
sd(X)

"What does the central limit theory tell us about the sample average and how it is related to μ, the
 population average?"
It is a random variable with expected value μ and standard error √(σ/N)

"Construct a 95% confidence interval for μ."
# Define `se` as the standard error of the estimate. Print this value to the console.
se = sd(X)/sqrt(N); se
# Construct a 95% confidence interval for the population average based on our sample. Save the lower 
# and then the upper confidence interval to a variable called `ci`.
ci = c(mean(X)-qnorm(0.975)*se, mean(X)+qnorm(0.975)*se)

"Now run a Monte Carlo simulation in which you compute 10,000 confidence intervals as you have just done. 
 What proportion of these intervals include μ?"
mu <- mean(x)
B <- 10000
# Define an object `res` that contains a logical vector for simulated intervals that contain mu
res = replicate(B, {X = sample(x, N, replace = T)
                    avg = mean(X)
                    se = sd(X)/sqrt(N)
                    low = avg - qnorm(0.975)*se
                    high = avg + qnorm(0.975)*se
                    between(mu, low, high)})
# Calculate the proportion of results in `res` that include mu. Print this value to the console.
mean(res) #> ~95%

" Lets consider two pollsters that conducted daily polls and look at national polls for the month before the
 election. Is there a poll bias? Make a plot of the spreads for each poll."
data("polls_us_election_2016")
# These lines of code filter for the polls we want and calculate the spreads
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
# Make a boxplot with points of the spread for each pollster
polls %>% ggplot(aes(pollster, spread)) +
  geom_boxplot() +
  geom_point()

"Under the urn model, both pollsters should have the same expected value: the election day difference, d.
 We will model the observed data Yij in the following way: Yij = d + bi + εij with i=1,2 indexing the two 
 pollsters, bi the bias for pollster i, and εij poll to poll chance variability. We assume the ε are 
 independent from each other, have expected value 0 and standard deviation σi regardless of j.
 
 Which of the following statements best reflects what we need to know to determine if our data fit the urn 
 model?"
Is b1≠b2?
"On the right side of this model, only εij is a random variable. The other two values are constants.
 What is the expected value of Y1j?"
E[Y1] = d+b1
"Suppose we define E[Y1] as the average of poll results from the first poll and σ1 as the standard deviation 
 of the first poll.
 What is the expected value and standard error of Y1?"
E[Y1] = d+b1
SE[Y1] = σ1/√(N1)
"Using what we learned by answering the previous questions, what is the expected value of Y2−Y1?"
E[Y2-Y1] = d+b2 - d+b1 = b2-b1
"Using what we learned by answering the previous questions, what is the standard error of Y2−Y1?"
## Develop variance V[Y2-Y1] = V[Y2] + V[Y1] = (σ2/√(N2))^2 + (σ1/√(N1))^2 = ...
## SE[Y2-Y1] = √(σ2^2/N2 + σ1^2/N1)

"Compute the estimates of σ1 and σ2"
# Create an object called `sigma` that contains a column for `pollster` and a column for `s`, the standard
## deviation of the spread
sigma = polls %>% group_by(pollster) %>%
  summarise(s = sd(spread)) # standard deviation
sigma

"What does the central limit theorem tell us about the distribution of the differences between the pollster 
 averages, Y¯2−Y¯1?"
If we assume N2 and N1 are large enough, Y2 and Y1, and their difference, are approximately normal.


"Construct a 95% confidence interval for the difference b2 and b1. Does this interval contain zero?"
# Create an object called `res` that summarizes the average, standard deviation, and number of polls for the two pollsters.
res = polls %>% group_by(pollster) %>%
  summarise(m = mean(spread), s = sd(spread), n = n())
# Store the difference between the larger average and the smaller in a variable called `estimate`. Print this value to the console.
estimate = max(res$m) - min(res$m); estimate
# Store the standard error of the estimates as a variable called `se_hat`. Print this value to the console.
se_hat = sqrt(res$s[1]^2/res$n[1] + res$s[2]^2/res$n[2]); se_hat
# Calculate the 95% confidence interval of the spreads. Save the lower and then the upper confidence interval to a variable called `ci`.
ci = c(estimate-qnorm(0.975)*se_hat, estimate+qnorm(0.975)*se_hat)

"The confidence interval tells us there is relatively strong pollster effect resulting in a difference of about
 5%. Random variability does not seem to explain it.
 Compute a p-value to relay the fact that chance does not explain the observed pollster effect"
# Calculate the p-value
t = estimate / se_hat
pvalue = 2*(1-pnorm(t)); pvalue ## singnificant difference means there is a bias <=> b2-b1>0

"Compute the average and standard deviation for each pollster and examine the variability across the averages and
 how it compares to the variability within the pollsters, summarized by the standard deviation."
# Execute the following lines of code to filter the polling data and calculate the spread
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-15" &
           state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ungroup()
# Create an object called `var` that contains columns for the pollster, mean spread, and standard deviation. Print 
# the contents of this object to the console.
var = polls %>% group_by(pollster) %>%
  summarise(avg = mean(spread), s = sd(spread))
var








