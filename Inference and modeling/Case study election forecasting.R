rm(list = ls())
options(digits = 3)
library(imager); library(gtools); library(tidyverse); library(ggplot2); library(dslabs)




# Data --------------------------------------------------------------------

## In a previous section, we generated these data tables:
polls <- polls_us_election_2016 %>% 
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% group_by(pollster) %>% 
  filter(enddate == max(enddate)) %>%
  ungroup()

results <- one_poll_per_pollster %>% 
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>% 
  mutate(start = avg - 1.96*se, end = avg + 1.96*se) 



# Bayesian approach -------------------------------------------------------

## Pollsters tend to make probabilistic statements about the results of the election. For example, “The chance that Obama wins 
## the electoral college is 91%” is a probabilistic statement about a parameter which in previous sections we have denoted with  
## d. We showed that for the 2016 election, FiveThirtyEight gave Clinton an 81.4% chance of winning the popular vote. To do this,
## they used the Bayesian approach we described.

"Statistical textbooks will write the model like this:
  d ~ N(μ, τ) describes our best guess without seeing any poll data -> a priori
  X|d ~ N(d, σ) describes randomness due to sampling and the  pollster effect -> biases variability

For our best guess, we note that before any poll data is available, we can use data sources other than polling data. A popular 
approach is to use what pollsters call fundamentals, which are based on properties about the current economy that historically 
appear to have an effect in favor or against the incumbent party. 

We won’t use these here. Instead, we will use μ=0 which is interpreted as a model that simply does not provide any information 
on who will win. we will use recent historical data that shows the winner of the popular vote has an average spread of about 
3.5%. Therefore, we set τ=0.035"

## Now we can use the formulas for the posterior distribution for the parameter d
mu = 0; tau = 0.035
sigma = results$se; Y = results$avg
B = sigma^2 / (sigma^2 + tau^2)

posterior_mean = B*mu + (1-B)*Y; posterior_mean
posterior_se = 1 / (1/sigma^2 + 1/tau^2) %>% sqrt(); posterior_se

ci = posterior_mean + c(-1.96, 1.96)*posterior_se; ci
prob_ab0 = 1 - pnorm(0, posterior_mean, posterior_se); prob_ab0
"This says we are 100% sure Clinton will win the popular vote, which seems too overconfident. Also, it is not in agreement with 
 FiveThirtyEight’s 81.4%. What explains this difference?"



# The general bias --------------------------------------------------------

## After elections are over, one can look at the difference between pollster predictions and actual result. An important 
## observation that our model does not take into account is that it is common to see a general bias that affects many pollsters
## in the same way making the observed data correlated. There is no good explanation for this, but we do observe it in historical
## data: in one election, the average of polls favors Democrats by 2%, then in the following election they favor Republicans by 
## 1%, then in the next election there is no bias, then in the following one Republicans are favored by 3%, and so on. In 2016, 
## the polls were biased in favor of the Democrats by 1-2%.

"Although we know this bias term affects our polls, we have no way of knowing what this bias is until election night. So we can’t 
 correct our polls accordingly. What we can do is include a term in our model that accounts for this variability."




# Mathematical representations of models ----------------------------------

## Suppose we are collecting data from one pollster and we assume there is no general bias. The pollster collects several polls 
## with a sample size of N, so we observe several measurements of the spread X1...Xj. These RN have expected value d and standard
## error 2√(p(1-p)/N)
"Let’s start by using the following model to describe the observed variability:
                Xj = d + εj 
-> εj is a random variable that explains the poll-to-poll variability introduced by sampling error 
-> εj is zero mean and has a SE of 2√(p(1-p)/N) -> it explains the total variability since d is fixed"

## If d is 2.1% and N = 2000, we can simulate j=6 data points :
j = 6; n = 2000; d = 0.021
p = (d+1)/2
se = 2*sqrt(p*(1-p)/n)
X = d + rnorm(j, 0, se); X



"Now suppose we have J=6 sata points from I=5 different pollsters. To represent this we now need two indexes, one for pollster 
and one for the polls each pollster takes. We use Xij (i-th pollster, j-th poll from that pollster).
              Xij = d + εij "
## To simulate data, we now have to loop through the pollsters:
i = 5
X = sapply(1:i, function(i){
  d + rnorm(j, 0, se)}) # returns ixj polls
X #> 6x5 matrix

## The simulated data does not really seem to capture the features of the actual data:
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/simulated-data-without-bias-1.png"), axes = F)



"The model above does not account for pollster-to-pollster variability. To fix this, we add a new term for the pollster effect. 
 We will use hi to represent the house effect :
                Xij = d + hi + εij "
## To simulate data from a specific pollster, we now need to draw an hi and the add the ε. We do it here while assuming
## the variability of house effect is σh = 0.025
h = rnorm(i, 0, 0.025) # returns i house effects
X = sapply(1:i, function(i){
  d + h[i] + rnorm(j, 0, se)}) # add the house effect
X
"Adding the house effect returns more realistic values"
## Note that hi is common to all the observed spreads from a specific pollster. Different pollsters have a different hi, 
## which explains why we can see the groups of points shift up and down from pollster to pollster.
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/simulated-pollster-data-1.png"), axes = F)



"Now, in the model above, we assume the average house effect is 0. We think that for every pollster biased in favor of our 
 party, there is another one in favor of the other and assume the standard deviation is σh."
## But historically we see that every election has a general bias affecting all polls. We can observe this with the 2016 data, 
## but if we collect historical data, we see that the average of polls misses by more than models like the one above predict. 
## To see this, we would take the average of polls for each election year and compare it to the actual value. If we did this, 
## we would see a difference with a standard deviation of between 2-3%. To incorporate this into the model, we can add another
## term to account for this variability:
              "Xij = d + b + hi + εij "
"Here b is a random variable that accounts for the election-to-election variability. This random variable changes from election
 to election, but for any given election, it is the same for all pollsters and polls within on election. This is why it does not
 have indexes. This implies that all the random variables Xij for an election year are correlated since they all have b in common"
## One way to interpret b is as the difference between the average of all polls from all pollsters and the actual result of the 
## election. Because we don’t know the actual result until after the election, we can’t estimate b until after the election. 
## However, we can estimate b from previous elections and study the distribution of these values. Based on this approach we 
## assume that, across election years, b has expected value 0 and the standard error is about σb = 0.0025.
"An implication of adding this term to the model is that the standard deviation for Xij  is actually higher than what we earlier 
 called σ, which combines the pollster variability and the sample in variability, and was estimated with : "
sd(one_poll_per_pollster$spread)
"This estimate does not include the variability introduced by b. 
 Note that because X_bar = d + b + ∑Xi/N, the standard deviation of X_bar is √(σ^2/N + σb^2) "
"Since the same b is in every measurement, the average does not reduce the variability introduced by the b term. This is an
 important point: it does not matter how many polls you take, this bias does not get reduced."



# Important application ---------------------------------------------------

"Estimation with bias variability"
"If we redo the Bayesian calculation taking this variability into account, we get a result much closer to FiveThirtyEight’s:
"mu = 0; tau = 0.035; bias_sd = 0.025
sigma = sqrt(results$se^2 + bias_sd^2)
Y = results$avg
B = sigma^2 / (sigma^2 + tau^2)

posterior_mean = B*mu + (1-B)*Y; posterior_mean
posterior_se = sqrt(1 / (1/sigma^2 + 1/tau^2)); posterior_se

ci = posterior_mean + c(-1.96, 1.96)*posterior_se; ci
prob_ab0 = 1 - pnorm(0, posterior_mean, posterior_se); prob_ab0
## The credible interval is wider so the probability for Clinton to win is lower -> from 100% to 82%






# Predicting the electoral college ----------------------------------------

## Up to now we have focused on the popular vote. But in the United States, elections are not decided by the popular vote but
## rather by what is known as the electoral college. Each state gets a number of electoral votes that depends, in a somewhat 
## complex way, on the population size of the state. Here are the top 5 states ranked by electoral votes in 2016.
results_us_election_2016 %>% top_n(5, electoral_votes)
#>          state electoral_votes clinton trump others
#> 1   California              55    61.7  31.6    6.7
#> 2        Texas              38    43.2  52.2    4.5
#> 3      Florida              29    47.8  49.0    3.2
#> 4     New York              29    59.0  36.5    4.5
#> 5     Illinois              20    55.8  38.8    5.4
#> 6 Pennsylvania              20    47.9  48.6    3.6

## With some minor exceptions we don’t discuss, the electoral votes are won all or nothing. For example, if you win California 
## by just 1 vote, you still get all 55 of its electoral votes. This means that by winning a few big states by a large margin, 
## but losing many small states by small margins, you can win the popular vote and yet lose the electoral college. This happened 
## in 1876, 1888, 2000, and 2016. The idea behind this is to avoid a few large states having the power to dominate the presidential 
## election. Nonetheless, many people in the US consider the electoral college unfair and would like to see it abolished.

"We are now ready to predict the electoral college result for 2016. We start by aggregating results from a poll taken during the 
 last week before the election. We use the str_detect, a function we introduce later in Section 24.1, to remove polls that are 
 not for entire states."
results = polls_us_election_2016 %>%
  filter(state != "U.S." &
           !str_detect(state, "CD") & ## remove polls led on states specific CD
           enddate >="2016-10-31" & 
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))
results %>% arrange(abs(avg))


"We now introduce the command left_join that will let us easily add the number of electoral votes for each state from the dataset 
 results_us_election_2016. We will describe this function in detail in the Wrangling chapter. Here, we simply say that the function
 combines the two datasets so that the information from the second argument is added to the information in the first:"
results = left_join(results, results_us_election_2016, by = "state") # add all columns excepting state

## Notice that some states have no polls because the winner is pretty much known. No polls were conducted in DC, Rhode Island,
## Alaska, and Wyoming because Democrats are sure to win in the first two and Republicans in the last two.
results_us_election_2016 %>% filter(!state %in% results$state) %>% pull(state)

## Some states have NA sd
"Because we can’t estimate the standard deviation for states with just one poll, we will estimate it as the median of the standard 
 deviations estimated for states with more than one poll:"
results = mutate(results, sd = ifelse(is.na(sd), median(results$sd, na.rm = T), sd))


"To make probabilistic arguments, we will use a Monte Carlo simulation. For each state, we apply the Bayesian approach to 
 generate an election day d. We could construct the priors for each state based on recent history. However, to keep it simple,
 we assign a prior to each state that assumes we know nothing about what will happen. Since from election year to election year
 the results from a specific state don’t change that much, we will assign a standard deviation of 2% or τ=0.02. For now, we will
 assume, incorrectly, that the poll results from each state are independent. The code for the Bayesian calculation under these 
 assumptions looks like this:"
mu = 0; tau = 0.02
clinton_EV = replicate(10000, {
                             results %>% mutate(sigma = sd/sqrt(n),
                                                B = sigma^2 / (sigma^2 + tau^2),
                                                posterior_mean = B*mu + (1-B)*avg,
                                                posterior_se = sqrt(1 / (1/sigma^2 + 1/tau^2)),
                                                result = rnorm(length(posterior_mean), posterior_mean, posterior_se), ## random results based on prior assumptions
                                                clinton = ifelse(result > 0, electoral_votes, 0)) %>%
                              summarise(clinton = sum(clinton)) %>%
                              pull(clinton) + 7 ## account for Rhode Island and D.C.
})
mean(clinton_EV > 269)
## This model gives Clinton over 99% chance of winning. A similar prediction was made by the Princeton Election Consortium. We now
## know it was quite off. What happened?


"My own model -> Instead of generating random results we could weight them by probabilities
 -> e.g. Clinton in one state has a 55% chance to win x 26EV = 14.3EV"
mu = 0; tau = 0.02
clinton_EV = results %>% mutate(sigma = sd/sqrt(n),
                               B = sigma^2 / (sigma^2 + tau^2),
                               posterior_mean = B*mu + (1-B)*avg,
                               posterior_se = sqrt(1 / (1/sigma^2 + 1/tau^2)),
                               pr = 1 - pnorm(0, posterior_mean, posterior_se),
                               clinton = pr * electoral_votes) %>%
            summarise(clinton = sum(clinton)) %>%
            pull(clinton) + 7 
clinton_EV #> 307 expected votes


"The model above ignores the general bias and assumes the results from different states are independent. After the election, we 
 realized that the general bias in 2016 was not that big: it was between 1 and 2%. But because the election was close in several
 big states and these states had a large number of polls, pollsters that ignored the general bias greatly underestimated the
 standard error.

Using the notation we introduce, they assumed the standard error was √(σ^2/N) which with large N is quite smaller than the more 
accurate estimate √(σ^2/N + σb^2). FiveThirtyEight, which models the general bias in a rather sophisticated way, reported a closer
result. We can simulate the results now with a bias term. For the state level, the general bias can be larger so we set it at 0.03"
tau = 0.02; bias_sd = 0.03
clinton_EV_2 = replicate(1000, {
                                results %>% mutate(sigma = sqrt(sd^2/n + bias_sd^2),
                                                   B = sigma^2 / (sigma^2 + tau^2),
                                                   posterior_mean = B*mu + (1-B)*avg,
                                                   posterior_se = sqrt(1 / (1/sigma^2 + 1/tau^2)),
                                                   result = rnorm(length(posterior_mean), posterior_mean, posterior_se), 
                                                   clinton = ifelse(result > 0, electoral_votes, 0)) %>%
                                summarise(clinton = sum(clinton)) %>%
                                pull(clinton) + 7
})
mean(clinton_EV_2 > 269) #> 84% chance to win

"This gives us a much more sensible estimate. Looking at the outcomes of the simulation, we see how the bias term adds variability 
 to the final results."
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/comparison-forecast-with-and-without-bias-1.png"), axes=F)

"FiveThirtyEight includes many other features we do not include here. One is that they model variability with distributions that 
 have high probabilities for extreme events compared to the normal. One way we could do this is by changing the distribution used 
 in the simulation from a normal distribution to a t-distribution. FiveThirtyEight predicted a probability of 71%."





# Forecasting -------------------------------------------------------------

"Forecasters like to make predictions well before the election. The predictions are adapted as new polls come out. However, an 
 important question forecasters must ask is: how informative are polls taken several weeks before the election about the actual 
 election? Here we study the variability of poll results across time."

## To make sure the variability we observe is not due to pollster effects, let’s study data from one pollster:
one_pollster <- polls_us_election_2016 %>% 
  filter(pollster == "Ipsos" & state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
## Since there is no pollster effect, then perhaps the theoretical standard error matches the data-derived standard deviation. 
## We compute both here (we compare to the highest possible theoretical se)
se = one_pollster %>% 
  summarise(empirical = sd(spread), theoretical = 2 * sqrt(mean(spread) * (1-mean(spread)) / min(samplesize)))
se
## But the empirical standard deviation is higher than the highest possible theoretical estimate. Furthermore, the spread data 
## does not look normal as the theory would predict:
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/time-trend-variability-1.png"), axes=F)

"The models we have described include pollster-to-pollster variability and sampling error. But this plot is for one pollster and 
 the variability we see is certainly not explained by sampling error. Where is the extra variability coming from? The following 
 plots make a strong case that it comes from time fluctuations not accounted for by the theory that assumes p is fixed"
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/time-trend-estimate-1.png"), axes=F)
## This plot only shows Ipsos polls !
## Some of the peaks and valleys we see coincide with events such as the party conventions, which tend to give the candidate a
## boost. 

"We can see the peaks and valleys are consistent across several pollsters"
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/time-trend-estimate-several-pollsters-1.png"), axes=F)

"This implies that, if we are going to forecast, our model must include a term to account for the time effect. We need to write a 
 model including a bias term for time:
                                              Yijt = d + b + hi + bt + εijt "
## The standard deviation of bt would depend on t since the closer we get to election day, the closer to 0 this bias term should be.

"Pollsters also try to estimate trends from these data and incorporate these into their predictions. We can model the time trend with 
 a function f(t) :
                    Yijt = d + b + hi + bt + f(t) + εijt  "

## We usually see the estimated f(t) not for the difference, but for the actual percentages for each candidate like this:
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/trend-estimate-for-all-pollsters-1.png"), axes=F)

"Once a model like the one above is selected, we can use historical and present data to estimate all the necessary parameters to make 
 predictions. There is a variety of methods for estimating trends f(t) which we discuss in the Machine Learning module"




# T-distribution ----------------------------------------------------------

## Estimating the standard deviation introduce further variability in intervals that are too small. For large sample sizes this extra
## variability is negligible thus we use the CLT and the normal distribution. But for sizes smaller than 30 we would prefer usind the
## student distribution.

"By substituting σ by s we introduce some variability. The theory tells us that Z = (X-mu)/(s/√n) follows a t-distribution with
 n-1 degrees of freedom. The degrees of freedom is a parameter that controls the variability via fatter tails"
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/t-distribution-examples-1.png"), axes=F)

## If we are willing to assume the pollster effect data is normally distributed, perhaps a better confidence interval for d is :
z = qt(0.975, nrow(one_poll_per_pollster)-1); z #> 2.14 > 1.96
one_poll_per_pollster %>%
  summarize(avg = mean(spread), moe = z*sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg-moe, end = avg+moe)






# DataCamp Assessment -----------------------------------------------------

"For each poll in the polling data set, use the CLT to create a 95% confidence interval for 
 the spread. Create a new table called cis that contains columns for the lower and upper 
 limits of the confidence intervals."
# Create a table called `polls` that filters by  state, date, and reports the spread
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
# # Create an object called `cis` that has the columns indicated in the instructions
cis = polls %>% mutate(X_hat = (spread+1)/2,
                       se = 2*sqrt(X_hat*(1-X_hat)/samplesize),
                       lower = spread - se*qnorm(0.975),
                       upper = spread + se*qnorm(0.975)) %>%
                select(state, startdate, enddate, pollster, grade, 
                       spread, lower, upper)


"You can add the final result to the cis table you just created using the left_join function 
 as shown in the sample code.
Now determine how often the 95% confidence interval includes the actual result."
# Add the actual results to the `cis` data set
add = results_us_election_2016 %>% 
  mutate(actual_spread = clinton/100 - trump/100) %>% 
  select(state, actual_spread)
ci_data <- cis %>% 
  mutate(state = as.character(state)) %>% 
  left_join(add, by = "state")
# # Create an object called `p_hits` that summarizes the proportion of confidence intervals 
# that contain the actual value. Print this object to the console.
p_hits = ci_data %>%
  mutate(hit = (actual_spread >= lower & actual_spread <= upper)) %>%
  summarize(mean(hit))
p_hits

"Now find the proportion of hits for each pollster.  Show the number of polls conducted by 
 each pollster and the FiveThirtyEight grade of each pollster."
# Create an object called `p_hits` that summarizes the proportion of hits for each pollster 
# that has at least 5 polls.
p_hits = ci_data %>%
  mutate(hit = (actual_spread >= lower & actual_spread <= upper)) %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>%
  summarise(proportion_hits = mean(hit),
         n = n(),
         grade = first(grade)) %>%
  arrange(proportion_hits)
p_hits


"Repeat the previous exercise, but instead of pollster, stratify by state. Here we can't 
 show grades."
p_hits = ci_data %>%
  mutate(hit = (actual_spread >= lower & actual_spread <= upper)) %>%
  group_by(state) %>%
  filter(n() >= 5) %>%
  summarise(proportion_hits = mean(hit),
            n = n()) %>%
  arrange(proportion_hits)
p_hits
# Make a barplot of the proportion of hits for each state
p_hits %>% 
  mutate(state = reorder(state, proportion_hits)) %>%
  ggplot(aes(state, proportion_hits)) +
    geom_bar(stat = "identity") +
    coord_flip()

"Even if a forecaster's confidence interval is incorrect, the overall predictions will do
 better if they correctly called the right winner.
Add two columns to the cis table by computing, for each poll, the difference between the 
predicted spread and the actual spread, and define a column hit that is true if the signs 
are the same."
# Create an object called `errors` that calculates the difference between the predicted and 
# actual spread and indicates if the correct winner was predicted
cis = ci_data # here only, they made a mistake
errors = cis %>%
  mutate(error = spread - actual_spread,
         hit = sign(spread) == sign(actual_spread))
errors
# Examine the last 6 rows of `errors`
tail(errors, 6) ## opposite of head

# Create an object called `p_hits` that summarizes the proportion of hits for each state 
# that has 5 or more polls
p_hits = errors %>%
  group_by(state) %>%
  filter(n() >= 5) %>%
  summarise(proportion_hits = mean(hit),
            n = n())
p_hits
# # Make a barplot of the proportion of hits for each state
p_hits %>% 
  mutate(state = reorder(state, proportion_hits)) %>%
  ggplot(aes(state, proportion_hits)) +
  geom_bar(stat = "identity") +
  coord_flip()

# We see that many had well predicted the winner. Only a few states polls' were incorrect
# more than 25% of the time. Wisconsin got every single poll wrong. In Pennsylvania and 
# Michigan, more than 90% of the polls had the signs wrong.
"Make a histogram of the errors. What is the median of these errors?"
hist(errors$error)
median(errors$error)

# We see that, at the state level, the median error was slightly in favor of Clinton. 
# The distribution is not centered at 0, but at 0.037. This value represents the general 
# bias we described in an earlier section.
"Create a boxplot to examine if the bias was general to all states or if it affected some 
 states differently. Filter the data to include only pollsters with grades B+ or higher."
errors %>%
  mutate(state = reorder(state, error)) %>%
  filter(grade %in% c("A+","A","A-","B+")) %>%
  ggplot(aes(state, error)) +
    geom_boxplot() +
    geom_point()

# Create a boxplot showing the errors by state for states with at least 5 polls with grades
# B+ or higher
errors %>%
  filter(grade %in% c("A+","A","A-","B+")) %>%
  group_by(state) %>%
  filter(n() >= 5) %>%
  ungroup() %>%
  mutate(state = reorder(state, error)) %>%
  ggplot(aes(state, error)) +
    geom_boxplot() + geom_point()



# Conclusion --------------------------------------------------------------

"You see that the West (Washington, New Mexico, California) underestimated Hillary's performance, while the Midwest 
 (Michigan, Pennsylvania, Wisconsin, Ohio, Missouri) overestimated it. In our simulation in we did not model this 
 behavior since we added general bias, rather than a regional bias. Some pollsters are now modeling correlation 
 between similar states and estimating this correlation from historical data. To learn more about this, you can 
 learn about random effects and mixed models."



# DataCamp Assessmant -----------------------------------------------------

"We know that, with a normal distribution, only 5% of values are more than 2 standard deviations
 away from the mean."
# Calculate the probability of seeing t-distributed random variables being more than 2 in absolute 
# value when 'df = 3'.
1-pt(2, 3) + pt(-2, 3)

"Now use sapply to compute the same probability for degrees of freedom from 3 to 50.
 Make a plot and notice when this probability converges to the normal distribution's 5%."
# Generate a vector 'df' that contains a sequence of numbers from 3 to 50
df = 3:50
# Make a function called 'pt_func' that calculates the probability that a value is more than |2| 
pt_func = function(df)
{
  1-pt(2, df) + pt(-2, df)
}
# Generate a vector 'probs' that uses the `pt_func` function to calculate the probabilities
probs = sapply(df, pt_func); probs
# Plot 'df' on the x-axis and 'probs' on the y-axis
plot(df, probs)

"In a previous section, we repeatedly took random samples of 50 heights from a distribution of 
 heights. We noticed that about 95% of the samples had confidence intervals spanning the true
 population mean.
Re-do this Monte Carlo simulation, but now instead of N=50, use N=15. Notice what happens to 
the proportion of hits."
data(heights)
x <- heights %>% filter(sex == "Male") %>% .$height
mu <- mean(x); N <- 15; B <- 10000
set.seed(1)
# Generate a logical vector 'res' that contains the results of the simulations
res = replicate(B, {
                    X = sample(x, N, replace = T)
                    interval = c(mean(X) - qnorm(0.975)*sd(X)/sqrt(N), 
                                 mean(X) + qnorm(0.975)*sd(X)/sqrt(N))
                    between(mu, interval[1], interval[2])
                    })
# Calculate the proportion of times the simulation produced values within the 95% confidence interval. 
# Print this value to the console.
mean(res)

"N=15 is not that big. We know that heights are normally distributed, so the t-distribution should 
 apply. Repeat the previous Monte Carlo simulation using the t-distribution instead of using the
 normal distribution to construct the confidence intervals."
res = replicate(B, {
  X = sample(x, N, replace = T)
  interval = c(mean(X) - qt(0.975, N-1)*sd(X)/sqrt(N), 
               mean(X) + qt(0.975, N-1)*sd(X)/sqrt(N))
  between(mu, interval[1], interval[2])
})
# Calculate the proportion of times the simulation produced values within the 95% confidence interval. 
# Print this value to the console.
mean(res)

"!!!! for small sample sizes it is much better to use the student distribution"
"Why did the t-distribution confidence intervals work so much better?"
The t-distribution takes the variability into account and generates larger confidence intervals.









