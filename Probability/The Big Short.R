rm(list = ls())
options(digits = 4)
library(imager); library(gtools); library(tidyverse); library(ggplot2)



# Interest rates explained with chance model ------------------------------

## More complex versions of the sampling models we have discussed are also used by banks to decide interest rates. Suppose you run a small 
## bank that has a history of identifying potential homeowners that can be trusted to make payments. In fact, historically, in a given year,
## only 2% of your customers default, meaning that they donât pay back the money that you lent them. However, you are aware that if you 
## simply loan money to everybody without interest, you will end up losing money due to this 2%. Although you know 2% of your clients will 
## probably default, you donât know which ones. Yet by charging everybody just a bit extra in interest, you can make up the losses incurred 
## due to that 2% and also cover your operating costs. You can also make a profit, but if you set the interest rates too high, your clients
## will go to another bank. We use all these facts and some probability theory to decide what interest rate you should charge.

## Suppose your bank will give out 1,000 loans for $180,000 this year. Also, after adding up all costs, suppose your bank loses $200,000 per 
## foreclosure. For simplicity, we assume this includes all operational costs. A sampling model for this scenario can be coded like this:
n_loans = 1000; loss = -200000; prob_default = 0.02; loan = 180000
defaults = sample(c(0,1), n_loans, prob = c(1-prob_default, prob_default), replace = T)
sum(defaults * loss) ## all losses over 1000 loans

## Note that the total loss defined by the final sum is a random variable. Every time you run the above code, you get a different answer. We 
## can easily construct a Monte Carlo simulation to get an idea of the distribution of this random variable.
losses = replicate(10000, {defaults = sample(c(0,1), n_loans, prob = c(1-prob_default, prob_default), replace = T)
                            sum(defaults * loss)})

## We donât really need a Monte Carlo simulation though. Using what we have learned, the CLT tells us that because our losses are a sum of 
## independent draws, its distribution is approximately normal with expected value and standard errors given by:
avg = n_loans * (prob_default*loss - 0); avg
se = sqrt(n_loans) * abs(loss)*sqrt(prob_default*(1-prob_default)); se

## We can now set an interest rate to guarantee that, on average, we break even. So we introduce x that will represent the interest paid by
## borrowers -> x verifies loss.p + x(1-p) = 0 <=> x = -(loss.p)/(1-p)
x = -(loss * prob_default) / (1 - prob_default); x
i = x/loan; i

## However, we still have a problem. Although this interest rate guarantees that on average we break even, there is a 50% chance that we 
## lose money. If our bank loses money, we have to close it down. We therefore need to pick an interest rate that makes it unlikely for
## this to happen. At the same time, if the interest rate is too high, our clients will go to another bank so we must be willing to take
## some risks. So letâs say that we want our chances of losing money to be 1 in 100.
"We want P(gains<0) = 0.01"
I = seq(i, 0.05, 0.0001)
X = loan*I
avg_sum = n_loans * (prob_default*loss + (1-prob_default)*X)
se_sum = sqrt(n_loans) * abs(X - loss)*sqrt(prob_default * (1-prob_default))
"Approaching"
prob_lose = pnorm(0, avg_sum, se_sum)
data.frame(I, X, prob_lose) %>% round(5) #> ~0.3468 -> x = 6242
"Mathematically"
# Now we are going to use a mathematical âtrickâ that is very common in statistics. We add and subtract the same quantities to both sides
## of the event S < 0 so that the probability does not change and we end up with a standard normal random variable on the left, which will
## then permit us to write down an equation with only x as an unknown. This âtrickâ is as follows:
"P(S<0) = 0.01 <=> P((S-E[S]) / SE[S] < -E[S] / SE[S]) = 0.01 
               <=> P(Z < -E[S] / SE[S]) = 0.01
               <=> -E[S] / SE[S] = z as P(Z < z) = 0.01
               <=> -E[S] / SE[S] = z(0.01)
               <=> -n.(loss.p + x(1-p) / (sqrt(n).|x-loss|.sqrt(p(1-p)))) = z
resolving leads to  x = -l.[(np-z.sqrt(np(1-p))) / (n(l-p) + z.sqrt(np(1-p))] "
z = qnorm(0.01); n = n_loans; l = loss; p = prob_default
x_1percent = -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p))); x_1percent
x_1percent/loan
#> 6249 - 3.472% ang the expected return is
n*(l*p + x_1percent*(1-p))
## This is still a very competitive interest rate. By choosing this interest rate, we now have an expected profit per loan of:
(l*p + x_1percent*(1-p))*n #> 2,124,198
"Monte Carlo verification"
profit = replicate(10000, {draws = sample(c(x_1percent, l), n, prob = c(1-p, p), replace = T)
                           sum(draws)})
mean(profit) #> ~2,130,000
mean(profit<0) #> lose in 1.35% of cases




# The big short -----------------------------------------------------------

## One of your employees points out that since the bank is making 2,124 dollars per loan, the bank should give out more loans! Why just 
## n ? You explain that finding those n clients was hard. You need a group that is predictable and that keeps the chances of defaults 
## low. He then points out that even if the probability of default is higher, as long as our expected value is positive, you can minimize
## your chances of losses by increasing n and relying on the law of large numbers.

## Even if the default rate is higher, we can minimize our chances of losing money by simply increasing n since :
"P(S<0) = P(Z < -E[S]/SE[S]) with E[S] = n.Î¼ and SE[S] = â(n)Ï
<=> z(0.01) = -(n.Î¼) / (â(n)Ï) = -(â(n).Î¼) / Ï"
"So we are guaranteed to have a probability of less than 0.01 if we let :
                n â¥ z^2.Ï^2/Î¼^2 <=> n â¥ (z.Ï/Î¼)^2"
## The implication is that, as long as Î¼ is positive, we can find an n that minimizes the probability of a loss. This is a form of the 
## law of large numbers: when n is large, our average earnings per loan converges to the expected earning Î¼.

## With x fixed to a 5% interest and the probability of default being 4%, now we can ask what n do we need for the probability to 
## be 0.01? In our example, if we give out:
z = qnorm(0.01); x = 0.05*180000; p = 0.04
n = ceiling((z^2 * (x-l)^2*p*(1-p)) / (l*p + x*(1-p))^2); n #> this config works better if we make 22163 loans or more
"ceiling() returns the nearest integer higher than the result e.g. ceiling(2.1) = 3"
## Now we expect to win:
n*(l*p + x*(1-p)) # 14,184,320 against


"If events are dependent and thus defaults probs change, the standard deviation increases a lot and with it the proba to lose money"
## Your colleagueâs scheme was mainly based on this mathematical formula: SE[(X1 + ... + Xn)/n] = Ï/ân
## By making n large, we minimize the standard error of our per-loan profit. However, for this rule to hold, the Xs must be independent 
## draws: one person defaulting must be independent of others defaulting. Note that in the case of averaging the same event over and over,
## an extreme example of events that are not independent, we get a standard error that is ân times bigger:
## SE[(X1 + ... + Xn)/n] = SE[nX/n] = Ï > Ï/ân

## To construct a more realistic simulation than the original one your colleague ran, letâs assume there is a global event that affects
## everybody with high-risk mortgages and changes their probability. We will assume that with 50-50 chance, all the probabilities go up
## or down slightly to somewhere between 0.03 and 0.05. But it happens to everybody at once, not just one person. These draws are no 
## longer independent.
p = 0.04; x = 0.05*180000
profit = replicate(10000, {new_p = 0.04 + sample(seq(-0.01, 0.01, length = 1000), 1)
                           draws = sample(c(x, loss), n, prob = c(1-new_p, new_p), replace = T)
                           sum(draws)})
## Note that our expected profit is still large. However, the probability of the bank having negative earnings shoots up
mean(profit); mean(profit<0) #> 35% to lose
## Even scarier is that the probability of losing more than 10 million dollars is:
mean(profit < -10^7) #> 24%

## To understand how this happens look at the distribution:
data.frame(profits_millions = profit / 10^6) %>%
  ggplot(aes(profits_millions)) +
  geom_histogram(color = "black", binwidth = 5)

## The theory completely breaks down and the random variable has much more variability than expected. The financial meltdown of 2007 was
## due, among other things, to financial âexpertsâ assuming independence when there was none.









