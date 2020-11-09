rm(list = ls())
options(digits = 3)
library(imager); library(gtools); library(tidyverse); library(ggplot2); library(dslabs)


"What does it mean when an election forecaster tells us that a given candidate has a 90% chance of winning? In the context of 
 the urn model, this would be equivalent to stating that the probability p > 0.5 is 90%. However, as we discussed earlier, in
 the urn model p is a fixed parameter and it does not make sense to talk about probability. With Bayesian statistics, we model 
 p as random variable and thus a statement such as “90% chance of winning” is consistent with the approach.

 Forecasters also use models to describe variability at different levels. For example, sampling variability, pollster to 
 pollster variability, day to day variability, and election to election variability. One of the most successful approaches 
 used for this are hierarchical models, which can be explained in the context of Bayesian statistics."


# Bayes Theorem -----------------------------------------------------------

## We start by describing Bayes theorem. We do this using a hypothetical cystic fibrosis test as an example. Suppose a test for 
## cystic fibrosis has an accuracy of 99%. We will use the following notation: (mucoviscidose)
"The probability to get positive on the test given that the person has the disease is :
      P(+ | D=1) = 0.99 
 The probability to get negative on the test given that the person hasn't the disease is :
      P(- | D=0) = 0.99 "
## Suppose we get a person positive. What is the probability that they have the disease P(D=1 | +) ?
## The cystic fibrosis rate is P(D=1) = 0.00025. The Bayes theorem tells us that
"P(A|B) = (P(B|A) * P(A)) / P(B)"
## So we get
"P(D=1 | +) = P(+ | D=1) * P(D = 1) / P(+) 
 where P(A) = P(A & B) + P(A & B_bar) 
 -> P(+) = P(+ & D=1) + P(+ & D=0)
 -> P(+) = P(+ | D=1).P(D=1) + P(+ | D=0).P(D=0)
 -> P(+) = 0.99*0.00025 + (1-0.99)*(1-0.00025) = 0.010245
 <=> P(D=1 | +) = 0.99 * 0.00025 / 0.010245 = 0.02415813

So the probability to have the disease if the test returns positive is 2.4%"
## This says that despite the test having 0.99 accuracy, the probability of having the disease given a positive test is only
## 0.02. This may appear counter-intuitive to some, but the reason this is the case is because we have to factor in the very
## rare probability that a person, chosen at random, has the disease.



# Bayes theorem simulation ------------------------------------------------

## The following simulation is meant to help you visualize Bayes theorem. We start by randomly selecting 100,000 people from a 
## population in which the disease in question has a 1 in 4,000 prevalence.
prev = 0.00025
N = 1000000
outcome = sample(c("Disease", "Healthy"), N, replace = T, prob = c(prev, 1-prev))
dis = sum(outcome == "Disease"); heal = sum(outcome == "Healthy")

## Now each person gets the test, which is correct 99% of the time:
accuracy = 0.99
test = vector("character", N)
test[outcome == "Disease"] = sample(c("+","-"), dis, replace = T, prob = c(accuracy, 1-accuracy)) ## gives the test to sick people
test[outcome == "Healthy"] = sample(c("-","+"), heal, replace = T, prob = c(accuracy, 1-accuracy)) ## gives the test to healthy people
table(outcome, test)
"We get a lot more of false positive than true positive. Approximately 98% of positive for healthy people"
"We can try for different values of accuracy and prevalence"
#> 99.9% turns into 80% the probability of false positive
#> 99.9% and prev = 1 in 1000 = 0.001 turns into 50% 

## José Iglesias is a professional baseball player. In April 2013, when he was starting his career, he was performing rather well:
Month	AtBats	H	 AVG
April	 20	    9	 .450
## The batting average (AVG) statistic is one way of measuring success. Roughly speaking, it tells us the success rate when 
## batting. An AVG of .450 means José has been successful 45% of the times he has batted (At Bats) which is rather high, 
## historically speaking. Keep in mind that no one has finished a season with an AVG of .400 or more since Ted Williams did 
## it in 1941! To illustrate the way hierarchical models are powerful, we will try to predict José’s batting average at the
## end of the season. Note that in a typical season, players have about 500 at bats.

"With the techniques we have learned up to now, referred to as frequentist techniques, the best we can do is provide a confidence 
 interval. We can think of outcomes from hitting as a binomial with a success rate of p. So if the success rate is indeed .450,
 the standard error of just 20 at bats is:"
sd = sqrt(0.45*0.55/20); sd
ci = c(0.45-1.96*sd, 0.45+1.96*sd); ci
## This prediction has two problems. First, it is very large, so not very useful. Second, it is centered at .450, which implies 
# that our best guess is that this new player will break Ted Williams’ record.

## If you follow baseball, this last statement will seem wrong and this is because you are implicitly using a hierarchical model 
## that factors in information from years of following baseball. Here we show how we can quantify this intuition. First, let’s 
## explore the distribution of batting averages for all players with more than 500 at bats during the previous three seasons:
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/batting-averages-histogram-1.png"), axes = F)
## The average player had an AVG of .275 and the standard deviation of the population of players was 0.027. So we can see already 
## that .450 would be quite an anomaly since it is over six standard deviations away from the mean.



# Hierarchical models -----------------------------------------------------

"First level of variability"
## We use a model to represent two levels of variability in our data. First, each player is assigned a natural ability to hit.
## We will use the symbol p to represent this ability. You can think of p as the batting average you would converge to if this
## particular player batted over and over again.
## Based on the plots we showed earlier, we assume that p has a normal distribution. With expected value .270 and standard error
## 0.027.
"Second level of variability"
## Now the second level of variability has to do with luck when batting. Regardless of how good the player is, sometimes you 
## have bad luck and sometimes you have good luck. At each at bat, this player has a probability of success p. If we add up
## these successes and failures, then the CLT tells us that the observed average, call it Y, has a normal distribution with 
## expected value p and standard error √(p(1-p)/N) with N the number of at bats.
"Statistical textbooks will write the model like this:
        p ~ N(μ, τ)
        Y|p ~ N(p, σ) 
We refer to the model as hierarchical because we need to know p, the first level, in order to model Y, the second level"
##  In our example the first level describes randomness in assigning talent to a player and the second describes randomness 
## in this particular player’s performance once we have fixed the talent parameter. 
"In a Bayesian framework, the first level is called a prior distribution and the second the sampling distribution."


## The data analysis we have conducted here suggests that we set μ = 0.270, τ = 0.027 and σ = √(p(1-p)/N) 
## p ~ N(0.275, 0.027)
## Y|P ~ N(p, 0.111)
"We now are ready to compute a posterior distribution to summarize our prediction of p. The continuous version of Bayes’ rule
 can be used here to derive the posterior probability function, which is the distribution of p assuming we observe Y=y.
      E[p | Y=y] = Bμ + (1-B)μ
      where B = σ^2 / (σ^2 + τ^2)"
"This is the weighted average of the population average μ and the observed data y (weight depends on variance).  This weighted 
 average is sometimes referred to as shrinking because it shrinks estimates towards a prior mean."

## In the case of José Iglesias, we have:
"E[p | Y=0.450] = B*0.275 + (1-B)*(0.450) with B = 0.111^2/(0.111^2 + 0.027^2) = 0.944
                = 0.285"
## We do not show the derivation here, but the standard error can be shown to be:
"SE[p | y] = √(1 / ((1/σ^2) + (1/τ^2))) = √(1 / (1/0.111^2 + 1/0.027^2)) = 0.0262"


"So we started with a frequentist 95% confidence interval that ignored data from other players and summarized just José’s data:
 0.450 ± 0.220. Then we used a Bayesian approach that incorporated data from other players and other years to obtain a posterior
 probability. This is actually referred to as an empirical Bayes approach because we used data to construct the prior. From the 
 posterior, we can report what is called a 95% credible interval by reporting a region, centered at the mean, with a 95% chance 
 of occurring. In our case, this turns out to be: "
c(0.285-1.96*0.0262, 0.285+1.96*0.0262)
"The Bayesian credible interval suggests that if another team is impressed by the .450 observation, we should consider trading 
 José as we are predicting he will be just slightly above average."
## Interestingly, the Red Sox traded José to the Detroit Tigers in July. José Iglesias batting average for the next five months 
## is 0.293





# DataCamp Assessment -----------------------------------------------------

"In 1999 in England Sally Clark was found guilty of the murder of two of her sons. Both infants were found dead in the morning, 
 one in 1996 and another in 1998, and she claimed the cause of death was sudden infant death syndrome (SIDS). No evidence of 
 physical harm was found on the two infants so the main piece of evidence against her was the testimony of Professor Sir Roy 
 Meadow, who testified that the chances of two infants dying of SIDS was 1 in 73 million. He arrived at this figure by finding 
 that the rate of SIDS was 1 in 8,500 and then calculating that the chance of two SIDS cases was 8,500 × 8,500 ≈ 73 million.

 Based on what we've learned throughout this course, which statement best describes a potential flaw in Sir Meadow's reasoning?"
Sir Meadow assumed the second death was independent of the first son being affected, thereby ignoring possible genetic causes.

"Let's assume that there is in fact a genetic component to SIDS and the the probability of
 Pr(second case of SIDS∣first case of SIDS)=1/100, is much higher than 1 in 8,500."
"What is the probability of both of Sally Clark's sons dying of SIDS?"
# Define `Pr_1` as the probability of the first son dying of SIDS
Pr_1 <- 1/8500
# Define `Pr_2` as the probability of the second son dying of SIDS
Pr_2 <- 1/100
# Calculate the probability of both sons dying of SIDS. Print this
# value to the console.
"P(A&B) = P(B|A).P(A) -> P(1&2) = Pr_2.Pr_1"
Pr_2*Pr_1


"Many press reports stated that the expert claimed the probability of Sally Clark being innocent as 1 in 73 million. Perhaps the 
 jury and judge also interpreted the testimony this way. This probability can be written like this:"
"Pr(mother is a murderer∣two children found dead with no evidence of harm)
 Bayes' rule tells us this probability is equal to:"
Pr(two children found dead with no evidence of harm ∣ mother is a murderer) * Pr(mother is a murderer) / Pr(two children found dead with no evidence of harm)

"Assume that the probability of a murderer finding a way to kill her two children without leaving evidence of physical harm is:
 Pr(two children found dead with no evidence of harm ∣ mother is a murderer)=0.50
 Assume that the murder rate among mothers is 1 in 1,000,000.

According to Bayes' rule, what is the probability of: 
Pr(mother is a murderer∣two children found dead with no evidence of harm)"
# Define `Pr_B` as the probability of both sons dying of SIDS
Pr_B = Pr_1*Pr_2
# Define Pr_A as the rate of mothers that are murderers
Pr_A <- 1/1000000
# Define Pr_BA as the probability that two children die without evidence of harm, given that their mother is a murderer
Pr_BA <- 0.50
# Define Pr_AB as the probability that a mother is a murderer, given that her two children died with no evidence of physical harm. Print this value to the console
Pr_AB = Pr_BA*Pr_A/Pr_B; Pr_AB

"After Sally Clark was found guilty, the Royal Statistical Society issued a statement saying that there was 'no statistical basis' for the expert's claim. They 
 expressed concern at the 'misuse of statistics in the courts'. Eventually, Sally Clark was acquitted in June 2003.

In addition to misusing the multiplicative rule as we saw earlier, what else did Sir Meadow miss?"
He did not take into account how rare it is for a mother to murder her children.



"Florida is one of the most closely watched states in the U.S. election because it has many electoral votes and the election is generally close. "
"The CLT tells us that the average of these spreads is approximately normal. Calculate a spread average and provide an estimate of the standard error."
# Create an object `polls` that contains the spread of predictions for each candidate in Florida during the last polling days
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
results = polls %>% summarise(avg = mean(spread), se = sd(spread)/sqrt(length(spread)))
results

"Assume a Bayesian model sets the prior distribution for Florida's election night spread d to be normal with expected value μ and standard deviation τ."
"What are the interpretations of μ and τ?"
μ and τ summarize what we would predict for Florida before seeing any polls.
"Based on past elections, we would set mu close to 0, because both Republicans and Democrats have won, and tau at about 0.02, because these elections 
 tend to be close."


"The CLT tells us that our estimate of the spread d^ has a normal distribution with expected value d and standard deviation σ, which we calculated.
Use the formulas for the posterior distribution to calculate the expected value of the posterior distribution if we set μ=0 and τ=0.01."
# Define `mu` and `tau`
mu <- 0
tau <- 0.01
# Define a variable called `sigma` that contains the standard error in the object `results`
sigma = results$se
# Define a variable called `Y` that contains the average in the object `results`
Y = results$avg
# Define a variable `B` using `sigma` and `tau`. Print this value to the console.
B = sigma^2/(sigma^2+tau^2)
# Calculate the expected value of the posterior distribution
exp_p_y = B*mu + (1-B)*Y; exp_p_y
# Compute the standard error of the posterior distribution. Print this value to the console.
se_p_y = sqrt(1 / (1/sigma^2 + 1/tau^2)); se_p_y
# Construct the 95% credible interval. Save the lower and then the upper confidence interval to a variable called `ci`.
ci = c(B*mu+(1-B)*Y - qnorm(0.975)*se, B*mu+(1-B)*Y + qnorm(0.975)*se)

"According to this analysis, what was the probability that Trump wins Florida?"
# Using the `pnorm` function, calculate the probability that the actual spread was less than 0 (in Trump's favor). Print this value to the console.
pnorm(0, exp_p_y, se_p_y)
"We had set the prior variance τ to 0.01, reflecting that these races are often close.
Change the prior variance to include values ranging from 0.005 to 0.05 and observe how the probability of Trump winning Florida changes by making a plot"
# Define a variable `taus` as different values of tau
taus <- seq(0.005, 0.05, len = 100)
# Create a function called `p_calc` that generates `B` and calculates the probability of the spread being less than 0
p_calc = function(mu, Y, sigma, tau)
{
  B = sigma^2/(sigma^2+tau^2)
  exp = B*mu + (1-B)*Y
  se = sqrt(1 / (1/sigma^2 + 1/tau^2))
  pnorm(0, exp, se)
}
# Create a vector called `ps` by applying the function `p_calc` across values in `taus`
ps = sapply(taus, p_calc, mu=mu, Y=Y, sigma=sigma)
# Plot `taus` on the x-axis and `ps` on the y-axis
plot(ps~taus)























