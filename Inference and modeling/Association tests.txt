rm(list = ls())
options(digits = 3)
library(imager); library(gtools); library(tidyverse); library(ggplot2); library(dslabs)


"The statistical tests we have studied up to now leave out a substantial portion of data types. Specifically, we have not discussed inference 
 for binary, categorical, and ordinal data."
## To give a very specific example, consider the following case study

## A 2014 PNAS paper analyzed success rates from funding agencies in the Netherlands and concluded that their:
## 'results reveal gender bias favoring male applicants over female applicants in the prioritization of their “quality of researcher” (but not
## “quality of proposal”) evaluations and success rates, as well as in the language use in instructional and evaluation materials.'
data("research_funding_rates")
research_funding_rates %>% select(discipline, applications_total, 
                                  success_rates_total) %>% head(15)

## We can compute the totals that were successful and the totals that were not as follows:
totals = research_funding_rates %>%
  select(-discipline) %>% # all columns excepting discipline
  summarise_all(sum) %>% # returns the sum of all columns
  summarise(percent_men = awards_men / applications_men,
            percent_women = awards_women / applications_women)
totals # men have a 17.7 - 14.9 = 2.8% higher success rate
"But could this be due just to random variability? Here we learn how to perform inference for this type of data."



# Lady Tasting Tea --------------------------------------------------------

## R.A. Fisher was one of the first to formalize hypothesis testing. The “Lady Tasting Tea” is one of the most famous examples.

## The story is as follows: an acquaintance of Fisher’s claimed that she could tell if milk was added before or after tea was poured. Fisher
## was skeptical. He designed an experiment to test this claim. He gave her four pairs of cups of tea: one with milk poured first, the other
## after. The order was randomized. The null hypothesis here is that she is guessing. Fisher derived the distribution for the number of 
## correct picks on the assumption that the choices were random and independent.

## As an example, suppose she picked 3 out of 4 correctly. Do we believe she has a special ability? The basic question we ask is: if the 
## tester is actually guessing, what are the chances that she gets 3 or more correct? Just as we have done before, we can compute a 
## probability under the null hypothesis that she is guessing 4 of each. Under this null hypothesis, we can think of this particular 
## example as picking 4 balls out of an urn with 4 blue (correct answer) and 4 red (incorrect answer) balls. Remember, she knows that 
## there are four before tea and four after.


## Under the null hypothesis that she is simply guessing, each ball has the same chance of being picked. We can then use combinations to figure 
## out each probability. The probability of picking 3 is
combinations(4, 3)[,1] %>% length() * combinations(4, 1)[,1] %>% length() / combinations(8, 4)[,1] %>% length() #> 23%
## explication fr : elle a un choix unique a faire car choisir l'un des deux thés attribue le second caractere a l'autre, donc l'ensemble des
## possibilités revient a prendre 4 decisions parmi 8 choix. Réussir 3 fois sur 4 reviens a choisir 3 bonnes reponses sur 4 et 1 mauvaise
## reponse sur 4 -> C(3,4)*C(1,4) / C(4/8)

## The probability of picking all 4 correct is C(4,4)*C(0,4) / C(4,8)
combinations(4, 4)[,1] %>% length() * 1 / combinations(8, 4)[,1] %>% length() #> 1.4%
## Thus, the chance of observing a 3 or something more extreme, under the null hypothesis, is ~ 24%
"This is the p-value. The procedure that produced this p-value is called Fisher’s exact test and it uses the hypergeometric distribution."





# Two-by-two tables -------------------------------------------------------

## The data from the experiment is usually summarized by a table like this:
tab = matrix(c(3,1,1,3), 2, 2)
rownames(tab) = c("Poured before", "Poured after")
colnames(tab) = c("Guessed before", "Guessed after")
tab

"These are referred to as a two-by-two table. For each of the four combinations one can get with a pair of binary variables, they show the
 observed counts for each occurrence." # tableau de contingence

"The function fisher.test performs the inference calculations above:"
fisher.test(tab, alternative = "greater")$p.value





# Chi-square test ---------------------------------------------------------

## Notice that, in a way, our funding rates example is similar to the Lady Tasting Tea. However, in the Lady Tasting Tea example, the 
## number of blue and red beads is experimentally fixed and the number of answers given for each category is also fixed. This is because 
## Fisher made sure there were four cups with milk poured before tea and four cups with milk poured after and the lady knew this, so the 
## answers would also have to include four befores and four afters. If this is the case, the sum of the rows and the sum of the columns 
## are fixed. This defines constraints on the possible ways we can fill the two by two table and also permits us to use the hypergeometric 
## distribution. In general, this is not the case. Nonetheless, there is another approach, the Chi-squared test, which is described below.

## Imagine we have 290, 1,345, 177, 1,011 applicants, some are men and some are women and some get funded, whereas others don’t. We saw that
## the success rates for men and woman were:
totals 
#>   percent_men percent_women
#> 1       0.177         0.149

"Would we see this again if we randomly assign funding at the overall rate:"
rate = research_funding_rates %>%
  select(-discipline) %>% # all columns excepting discipline
  summarise_all(sum) %>% # returns the sum of all columns
  summarise(percent_total = awards_total / applications_total) %>%
  pull(percent_total)
rate
"The Chi-square test answers this question. The first step is to create the two-by-two data table:"
totals <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(sum) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women) 
two_by_two = data.frame(awarded = c("no", "yes"),
                        men = c(totals$no_men, totals$yes_men),
                        women = c(totals$no_women, totals$yes_women))
two_by_two ## empirical observations

"The general idea of the Chi-square test is to compare this two-by-two table to what you expect to see, which would be:"
data.frame(awarded = c("no", "yes"),
           men = (totals$no_men + totals$yes_men) * c(1-rate, rate),
           women = (totals$no_women + totals$yes_women) * c(1-rate, rate))
## theoretical observations under the null hypothesis of no gender discrimination

"Under the null hypothesis these observations are random variables. The Chi-square test tells us how likely it is to see a
 deviation this large or larger."
chisq_test = two_by_two %>% select(-awarded) %>% chisq.test()
chisq_test$p.value ## the pvalue is 5% => the difference is significant







# The odds ratio ----------------------------------------------------------

"An informative summary statistic associated with two-by-two tables is the odds ratio. Define the two variables as X=1 if you are a 
 male and 0 otherwise, and Y=1 if tou are funded and 0 otherwise. The odds of getting funded if you are a man is defined: 
                  P(Y=1 | X=1) / P(Y=0 | X=1) -> how many times more is it likely to be funded than not for a man ?
 And the odds of being funded if you are a woman is:
                  P(Y=1 | X=1) / P(Y=0 | X=1) "
odds_men <- with(two_by_two, (men[2]/sum(men)) / (men[1]/sum(men)))
odds_men; 1/odds_men #> it's 4.6 times more probable to be refused as a man
odds_women = with(two_by_two, (women[2]/sum(women)) / (women[1]/sum(women))) # with attaches the dataframe
odds_women; 1/odds_women #> it's 5.7 times more likely to be refused as a woman

"The odds ratio is the ratio for these two odds: how many times larger are the odds for men than for women?"
odds_men / odds_women #> 1.23

## We often see the two by two tables written like this:
#>              Men	Women
#> Awarded	     a	  b
#> Not Awarded	 c	  d
"Note that we can remove the totals (sums) denominators so that we get :
        odds ratio = ((a/(a+c)) / (c/(a+c))) / ((b/(b+d)) / (d/(b+d))) 
                   = (a/c) / (b/d)
                   = ad / bc"





# Confidence intervals for the odds ratio ---------------------------------

## Computing confidence intervals for the odds ratio is not mathematically straightforward. Unlike other statistics,for which we can 
## derive useful approximations of their distributions, the odds ratio is not only a ratio, but a ratio of ratios. Therefore, there 
## is no simple way of using, for example, the CLT.

"However, statistical theory tells us that when all four entries of the two-by-two table are large enough, then the log of the odds 
 ratio is approximately normal with standard error √(1/a + 1/b + 1/c + 1/d)

 This implies that a 95% confidence interval for the log odds ratio can be formed by:
                            log(ad/bc) +/- 1.96*√(1/a + 1/b + 1/c + 1/d)
 By exponentiating these two numbers we can construct a confidence interval of the odds ratio."
log_or = log(odds_men / odds_women)
log_se = two_by_two %>% 
  select(-awarded) %>%
  summarise(log_se = sqrt(1/men[1] + 1/men[2] + 1/women[1] + 1/women[2])) %>%
  pull(log_se)
ci = log_or + c(-1, 1) * qnorm(0.975) * log_se
ci
## The odds ration confidence interval is thus
exp(ci)
## Note that 1 isn't included in the 95% confidence interval which means that the p-value should be slightly smaller than 5%
t = (log_or - log(1))/log_se; t # t = 2 for h0 : odds ratio = 1
2*(1 - pnorm(2)) # pvalue = 4.55%

## This is a slightly different p-value than that with the Chi-square test. This is because we are using a different asymptotic 
## approximation to the null distribution. To learn more about inference and asymptotic theory for odds ratio, consult the 
## Generalized Linear Models book by McCullagh and Nelder.


"Note that the log odds ratio is not defined if any of the cells of the two-by-two table is 0. This is because if a, b, c or d is 0, 
 the log(ad / bc) is either the log of 0 or has a 0 in the denominator. For this situation, it is common practice to avoid 0s by 
 adding 0.5 to each cell. This is referred to as the Haldane–Anscombe correction and has been shown, both in practice and theory, to
 work well"




# Large samples, small p-values --------------------------------------------------

"As mentioned earlier, reporting only p-values is not an appropriate way to report the results of data analysis. In scientific journals, 
 for example, some studies seem to overemphasize p-values. Some of these studies have large sample sizes and report impressively small 
 p-values. Yet when one looks closely at the results, we realize odds ratios are quite modest: barely bigger than 1. In this case the
 difference may not be practically significant or scientifically significant."

"Note that the relationship between odds ratio and p-value is not one-to-one. It depends on the sample size. So a very small p-value 
 does not necessarily mean a very large odds ratio. Notice what happens to the p-value if we multiply our two-by-two table by 10, which
 does not change the odds ratio:"

two_by_two %>% select(-awarded) %>%
  mutate(men = men*10, women = women*10) %>%
  chisq.test() %>% .$p.value

## Own notes
"The p-value still has its importance in that it tells us if the odds ratio is different from 1, but it is necessaery to report as well
 the value of the odds ratio in order to see how large it is. Thus a small p-value doesn't measure the degree of discrimination, it 
 only tells us that there is discrimination"





# DataCamp Assessment -----------------------------------------------------

"In a previous exercise, we determined whether or not each poll predicted the correct winner
 for their state in the 2016 U.S. presidential election. Each poll was also assigned a grade
 by the poll aggregator. Now we're going to determine if polls rated A- made better predictions 
 than polls rated C-."
# Data generated in the Case study
data("polls_us_election_2016")
errors = polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  mutate(X_hat = (spread+1)/2,
         se = 2*sqrt(X_hat*(1-X_hat)/samplesize),
         lower = spread - se*qnorm(0.975),
         upper = spread + se*qnorm(0.975)) %>%
  select(state, startdate, enddate, pollster, grade, 
         spread, lower, upper) %>%
  mutate(state = as.character(state)) %>% 
  left_join(results_us_election_2016 %>% 
              mutate(actual_spread = clinton/100 - trump/100) %>% 
              select(state, actual_spread), 
            by = "state") %>%
  mutate(error = spread - actual_spread,
         hit = sign(spread) == sign(actual_spread))
# Generate an object called 'totals' that contains the numbers of good and bad predictions for polls rated A- and C-
totals = errors %>%
  filter(grade %in% c("A-", "C-")) %>%
  group_by(grade, hit) %>%
  summarise(hits = n()) %>%
  spread(grade, hits) 
totals
# Print the proportion of hits for grade A- polls to the console
106/132 #> A- polls made 80.3%
# Print the proportion of hits for grade C- polls to the console
311/361 #> C- polls made 86.1%

"We found that the A- polls predicted the correct winner about 80% of the time in their 
 states and C- polls predicted the correct winner about 86% of the time.
Use a chi-squared test to determine if these proportions are different."
# Perform a chi-squared test on the hit data. Save the results as an object called 'chisq_test'.
chisq_test = totals %>% 
  select(-hit) %>% 
  chisq.test()
chisq_test$p.value #> 15%

"It doesn't look like the grade A- polls performed significantly differently than the grade
 C- polls in their states. Calculate the odds ratio to determine the magnitude of the 
 difference in performance between these two grades of polls."
# Generate a variable called `odds_C` that contains the odds of getting the prediction right for grade C- polls
odds_C = totals[[2,2]] / totals[[1,2]] # bug from datacamp, must use double brackets
# Generate a variable called `odds_A` that contains the odds of getting the prediction right for grade A- polls
odds_A = totals[[2,3]] / totals[[1,3]]
# Calculate the odds ratio to determine how many times larger the odds ratio is for grade A- polls than grade C- polls
odds_A / odds_C


"Imagine we expanded our analysis to include all election polls and we repeat our analysis. 
 In this hypothetical scenario, we get that the p-value for the difference in prediction 
 success if 0.0015 and the odds ratio describing the effect size of the performance of 
 grade A- over grade B- polls is 1.07.
Based on what we learned in the last section, which statement reflects the best interpretation of this result?"
The p-value is below 0.05, but the odds ratio is very close to 1. There is not a scientifically significant 
difference in performance.












