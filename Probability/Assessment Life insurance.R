rm(list = ls())
options(digits = 4)
library(imager); library(gtools); library(tidyverse); library(ggplot2); library(dslabs)

data(death_prob)
head(death_prob)

# An insurance company offers a one-year term life insurance policy that pays $150,000 in the event of death within
# one year. The premium (annual cost) for this policy for a 50 year old female is $1,150. Suppose that in the event 
# of a claim, the company forfeits the premium and loses a total of $150,000, and if there is no claim the company 
# gains the premium amount of $1,150. The company plans to sell 1,000 policies to this demographic.

"The death_prob data frame from the dslabs package contains information about the estimated
 probability of death within 1 year (prob) for different ages and sexes."

"Q1a Use death_prob to determine the death probability of a 50 year old female, p"
p = death_prob$prob[death_prob$sex == "Female" & death_prob$age == 50]; p

"Q1b The loss in the event of the policy holder's death is -$150,000 and the gain if 
 the policy holder remains alive is the premium $1,150.
What is the expected value of the company's net profit on one policy for a 50 year old female?"
loss = -150000; premium = 1150
exp = p*loss + (1-p)*premium; exp

"Q1c Calculate the standard error of the profit on one policy for a 50 year old female."
se = abs(loss - premium) * sqrt(p*(1-p)); se

"Q1d What is the expected value of the company's profit over all 1,000 policies for 50 year old females?"
exp1000 = 1000*exp; exp1000

"Q1e What is the standard error of the sum of the expected value over all 1,000 policies for 50 year old females?"
se1000 = sqrt(1000) * se; se1000

"Q1f Use the Central Limit Theorem to calculate the probability that the insurance company loses money on this set of 1,000 policies."
pnorm(0, exp1000, se1000)


# 50 year old males have a different probability of death than 50 year old females. We will calculate
# a profitable premium for 50 year old males in the following four-part question.

"Q2a Use death_prob to determine the probability of death within one year for a 50 year old male."
p2 = death_prob$prob[death_prob$sex == "Male" & death_prob$age == 50]; p2

"Q2b Suppose the company wants its expected profits from 1,000 50 year old males with $150,000life insurance 
policies to be $700,000. Use the formula for expected value of the sum of draws with the following values and solve for the premium"
# 1000 * (x(1-p) - 150000p) = 700000
#• ... x = (700000 + 150000000p) / 1000(1-p)
premium2 = (700000 + 150000000*p2) / (1000*(1-p2)); premium2

"Q2c Using the new 50 year old male premium rate, calculate the standard error of the sum of 1,000 premiums."
se2 = abs(premium2 - -150000) * sqrt(1000*p2*(1-p2)); se2

"Q2d What is the probability of losing money on a series of 1,000 policies to 50 year old males?"
pnorm(0, 700000, se2)


# In this 6-part question, we'll look at a scenario in which a lethal pandemic disease increases the probability 
# of death within 1 year for a 50 year old to .015. Unable to predict the outbreak, the company has sold 1,000
# $150,000 life insurance policies for $1,150.

"Q3a What is the expected value of the company's profits over 1,000 policies?"
new_p = 0.015
new_exp = 0.985*1150 - 0.015*150000; new_exp*1000

"Q3b What is the standard error of the expected value of the company's profits over 1,000 policies?"
new_se = (150000+1150) * sqrt(new_p*(1-new_p)); new_se*sqrt(1000)

"Q3c What is the probability of the company losing money?"
pnorm(0, new_exp*1000, new_se*sqrt(1000))

"Q3d Suppose the company can afford to sustain one-time losses of $1 million, but larger losses will force it to go out of business.
 What is the probability of losing more than $1 million?"
pnorm(-1000000, new_exp*1000, new_se*sqrt(1000))

"Q3e Investigate death probabilities p <- seq(.01, .03, .001).
What is the lowest death probability for which the chance of losing money exceeds 90%?"
probs = seq(0.01, 0.03, 0.001)
losing = c()
for(i in 1:length(probs)) 
{
   avg = 1150*(1-probs[i]) - 150000*probs[i]
   se = 151500 * sqrt(1000*probs[i]*(1-probs[i]))
   losing[i] = pnorm(0, avg*1000, se)
}
data.frame(probs, losing)

"Q3f Investigate death probabilities p <- seq(.01, .03, .0025).
What is the lowest death probability for which the chance of losing over $1 million exceeds 90%?"
probs = seq(0.01, 0.03, 0.0025)
losing = c()
for(i in 1:length(probs)) 
{
   avg = 1150*(1-probs[i]) - 150000*probs[i]
   se = 151500 * sqrt(1000*probs[i]*(1-probs[i]))
   losing[i] = pnorm(-1000000, avg*1000, se)
}
data.frame(probs, losing)

"Define a sampling model for simulating the total profit over 1,000 loans with probability
of claim p_loss = .015, loss of -$150,000 on a claim, and profit of $1,150 when there is no
claim. Set the seed to 25, then run the model once."
set.seed(25, sample.kind = "Rounding")
profits = sample(c(1150, -150000), 1000, replace = T, prob = c(0.985, 0.015))
"What is the reported profit (or loss) in millions (that is, divided by  1000000)?"
sum(profits)/10^6

"Set the seed to 27, then run a Monte Carlo simulation of your sampling model with 10,000
replicates to simulate the range of profits/losses over 1,000 loans."
set.seed(27, sample.kind = "Rounding")
profits_VA = replicate(10000, sum(sample(c(1150, -150000), 1000, replace = T, prob = c(0.985, 0.015))))
"What is the observed probability of losing $1 million or more?"
mean(profits_VA < -1000000)



# Suppose that there is a massive demand for life insurance due to the pandemic, and the company 
# wants to find a premium cost for which the probability of losing money is under 5%, assuming the
# death rate stays stable at  p=0.015 .

"Q5a Calculate the premium required for a 5% chance of losing money given  n=1000  loans, probability 
of death  p=0.015 , and loss per claim  l=−150000 . Save this premium as x for use in further questions."
l = -150000; n = 1000; p = 0.015; z = qnorm(0.05)
x = -l * (n*p - z*sqrt(n*p*(1-p))) / (n*(1-p) + z*sqrt(n*p*(1-p)))
x

"Q5b What is the expected profit per policy at this rate?"
expx = p*l + (1-p)*x; expx
"Q5c What is the expected profit over 1,000 policies?"
expx*1000

"Q5c Run a Monte Carlo simulation with B=10000to determine the probability of losing 
money on 1,000 policies given the new premium x, loss on a claim of $150,000, and 
probability of claim  p=.015 . Set the seed to 28 before running your simulation."
set.seed(28, sample.kind = "Rounding")
profits_VAx = replicate(10000, sum(sample(c(x, -150000), 1000, replace = T, prob = c(1-p, p))))
"What is the probability of losing money here?"
mean(profits_VAx<0)


# The company cannot predict whether the pandemic death rate will stay stable. Set the 
# seed to 29, then write a Monte Carlo simulation that for each of  B=10000  iterations:
# 
# randomly changes  p  by adding a value between -0.01 and 0.01 with sample(seq(-0.01, 0.01, length = 100), 1)
# uses the new random  p  to generate a sample of  n=1,000  policies with premium x and loss per claim  l=−150000 
# returns the profit over  n  policies (sum of random variable)
# 
# The outcome should be a vector of  B  total profits. Use the results of the Monte Carlo simulation to answer the following three questions.

set.seed(29, sample.kind = "Rounding")
random_profits = replicate(10000, {
   p = 0.015 + sample(seq(-0.01, 0.01, length = 100), 1)
   sum(sample(c(x, -150000), 1000, replace = T, prob = c(1-p, p)))
})

"Q6a What is the expected value over 1,000 policies?"
mean(random_profits)
"Q6b What is the probability of losing money?"
mean(random_profits<0)
"Q6c What is the probability of losing more than $1 million?"
mean(random_profits< -1000000)








