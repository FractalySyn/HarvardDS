rm(list = ls())
options(digits = 3)
library(imager); library(gtools); library(tidyverse); library(ggplot2); library(dslabs)

"In June 2016, the United Kingdom (UK) held a referendum to determine whether the country would 
 "'Remain'" in the European Union (EU) or "'Leave'" the EU. This referendum is commonly known as
 Brexit. Although the media and others interpreted poll results as forecasting "'Remain'" (p>0.5),
 the actual proportion that voted "'Remain'" was only 48.1%  (p=0.481)  and the UK thus voted to 
 leave the EU. Pollsters in the UK were criticized for overestimating support for "'Remain'""

data(brexit_polls)
p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread -0.038

"Q1 The final proportion of voters choosing "'Remain'" was  p=0.481 . Consider a poll with a 
 sample of  N=1500  voters."
# What is the expected total number of voters in the sample choosing "Remain"? 
n = 1500; voting_remain = p*n; voting_remain
# What is the standard error of the total number of voters in the sample choosing "Remain"?
se_remain = sqrt(p*(1-p)) * sqrt(n); se_remain
# What is the expected value of  X^ , the proportion of "Remain" voters?
X_hat = p
# What is the standard error of  X^ , the proportion of "Remain" voters?
se_hat = se_remain/n; se_hat
# What is the expected value of  d , the spread between the proportion of "Remain" voters and "Leave" voters?
d_hat = d; d
# What is the standard error of  d , the spread between the proportion of "Remain" voters and "Leave" voters?
se_d_hat = se_hat*2; se_d_hat

"Q2 Calculate x_hat for each poll, the estimate of the proportion of voters choosing "'Remain'" on 
 the referendum day ( p=0.481 ), given the observed spread and the relationship  d^=2X^−1"
head(brexit_polls)
brexit = brexit_polls %>%
        mutate(X_hat = (spread+1)/2)
# What is the average of the observed spreads (spread)?
mean(brexit$spread)
# What is the standard deviation of the observed spreads?
sd(brexit$spread)
# What is the average of x_hat, the estimates of the parameter  p ?
mean(brexit$X_hat)
# What is the standard deviation of x_hat?
sd(brexit$X_hat)

"Q3 Consider the first poll in brexit_polls, a YouGov poll run on the same day as the Brexit referendum:"
poll = brexit[1,]; poll
# What is the the 95% confidence interval?
ci = poll$X_hat + c(-1,1) * sqrt(poll$X_hat*(1-poll$X_hat)/poll$samplesize) * qnorm(0.975)
ci


"Q4 Create the data frame june_polls containing only Brexit polls ending in June 2016"
#> (enddate of "2016-06-01" and later)
june_polls = brexit %>%
        filter(enddate >= "2016-06-01") %>%
        mutate(se_x_hat = sqrt(X_hat*(1-X_hat)/samplesize),
               se_spread = 2*se_x_hat,
               lower = spread - qt(0.975, samplesize-1)*se_spread,
               upper = spread + qt(0.975, samplesize-1)*se_spread,
               hit = d >= lower & d <= upper)
june_polls
# How many polls are in june_polls?
len = dim(june_polls); len
# What proportion of polls have a confidence interval that covers the value 0?
june_polls %>% mutate(hit = upper >= 0 & 0 >= lower) %>% 
        pull(hit) %>% sum() / len
# What proportion of polls predict "Remain" (confidence interval entirely above 0)?
june_polls %>% filter(0 <= lower) %>% dim() / len
# What proportion of polls have a confidence interval covering the true value of  d ?
sum(june_polls$hit) / len

"Q5 Group and summarize the june_polls object by pollster to find the proportion of hits
 for each pollster and the number of polls per pollster. Use arrange() to sort by hit rate."
june_polls %>% group_by(pollster) %>%
        summarise(n = n(), hit = sum(hit)/n) %>%
        arrange(desc(hit))
# The results are consistent with a large general bias that affects all pollsters.
"!!! I strongly disagree with the expected answer !!! poll results are heterogeneous, there's no general bias"

"Q6 Make a boxplot of the spread in june_polls by poll type."
june_polls %>%
        ggplot(aes(poll_type, spread)) +
        geom_boxplot() +
        geom_point()
# Telephone polls tend to show support "Remain" (spread > 0).
# Telephone polls tend to show higher support for "Remain" than online polls (higher spread).
# Online polls have a larger interquartile range (IQR) for the spread than telephone polls, indicating that they are more variable.
# Poll type introduces a bias that affects poll results.

"Q7 Calculate the confidence intervals of the spread combined across all polls in june_polls, grouping by poll type."
combined_by_type <- june_polls %>%
        group_by(poll_type) %>%
        summarize(N = sum(samplesize),
                  spread = sum(spread*samplesize)/N,
                  p_hat = (spread + 1)/2,
                  se = 2*sqrt(p_hat*(1-p_hat)/N),
                  lower = spread - qnorm(0.975)*se,
                  upper = spread + qnorm(0.975)*se)
combined_by_type


"Define brexit_hit, with the following code, which computes the confidence intervals for all 
 Brexit polls in 2016 and then calculates whether the confidence interval covers the actual
 value of the spread  d=−0.038 :"
brexit_hit <- brexit_polls %>%
        mutate(p_hat = (spread + 1)/2,
               se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
               spread_lower = spread - qnorm(.975)*se_spread,
               spread_upper = spread + qnorm(.975)*se_spread,
               hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
        select(poll_type, hit)
brexit_hit

"Q9 Use brexit_hit to make a two-by-two table of poll type and hit status. Then use the
 chisq.test() function to perform a chi-squared test to determine whether the difference in hit rate is significant."
contingency = table(brexit_hit) 
contingency %>%
        chisq.test(-poll_type) %>%
        .$p.value
# Online polls are more likely to cover the correct value of the spread and this difference is statistically significant.

"Q10 Use the two-by-two table constructed in the previous exercise to calculate the odds ratio between the hit rate 
 of online and telephone polls to determine the magnitude of the difference in performance between the poll types."
# ad / bc
odds_ratio = (contingency[1,1] * contingency[2,2]) / (contingency[1,2] * contingency[2,1])
odds_ratio
# odds
table = data.frame(no = c(37,32), yes = c(48,10)); table; table %>% attach()
odds_online = (yes[1]/sum(table[1,])) / (no[1]/sum(table[1,])); odds_online
odds_tel = (yes[2]/sum(table[2,])) / (no[2]/sum(table[2,])); odds_tel
1/odds_ratio

"Q11 Use brexit_polls to make a plot of the spread (spread) over time (enddate) colored 
 by poll type (poll_type). Use geom_smooth() with method = "'loess'" to plot smooth curves
 with a span of 0.4. Include the individual data points colored by poll type. Add a 
 horizontal line indicating the final value of  d=−.038 ."
brexit_polls %>%
        ggplot(aes(enddate, spread, color = poll_type)) +
        geom_point() +
        geom_smooth(method = "loess", span = 0.4) +
        geom_hline(yintercept = -0.038)

"Q12 Use the following code to create the object brexit_long, which has a column vote 
 containing the three possible votes on a Brexit poll ("'remain'", "'leave'", "'undecided'") 
 and a column proportion containing the raw proportion choosing that vote option on the given poll:"
brexit_long <- brexit_polls %>%
        gather(vote, proportion, "remain":"undecided") %>%
        mutate(vote = factor(vote))
"Make a graph of proportion over time colored by vote. Add a smooth trendline with geom_smooth()
and method = "'loess'" with a span of 0.3."
brexit_long %>%
        ggplot(aes(enddate, proportion, color = vote)) +
        geom_point() +
        geom_smooth(method = "loess", span = 0.3)








