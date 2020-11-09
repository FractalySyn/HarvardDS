rm(list = ls())
x = c("tidyverse", "ggplot2", "imager")
lapply(x, library, character.only = TRUE)
options(digits = 3)


"The example is from genetics. Francis Galton1 studied the variation and heredity of human
traits. Among many other traits, Galton collected and studied height data from families
to try to understand heredity. While doing this, he developed the concepts of correlation
and regression, as well as a connection to pairs of data that follow a normal distribution.
Of course, at the time this data was collected our knowledge of genetics was quite limited
compared to what we know today. A very specific question Galton tried to answer was:
how well can we predict a child’s height based on the parents’ height? The technique he
developed to answer this question, regression, can also be applied to our baseball question.
Regression can be applied in many other circumstances as well."

# Historical note: Galton made important contributions to statistics and genetics, but he was
# also one of the first proponents of eugenics, a scientifically flawed philosophical movement
# favored by many biologists of Galton’s time but with horrific historical consequences. You
# can read more about it here: https://pged.org/history-eugenics-and-genetics/.



# Case study: is height hereditary? ---------------------------------------

library(HistData)
data("GaltonFamilies")
head(GaltonFamilies)

"To imitate Galton’s analysis, we will create a dataset with the heights of fathers 
and a randomly selected son of each family:"
set.seed(1983)
galton_heights = GaltonFamilies %>%
   filter(gender == "male") %>%
   group_by(family) %>%
   sample_n(1) %>%
   ungroup() %>%
   select(father, childHeight) %>%
   rename(son = childHeight)
galton_heights

# An usual summary fails to describe an important characteristic of the data: the trend
# that the taller the father, the taller the son.
galton_heights %>% ggplot(aes(father, son)) +
   geom_point(alpha = 0.5)

"We will learn that the correlation coefficient is an informative summary of how two variables
move together and then see how this can be used to predict one variable using the other."



# The correlation coefficient ---------------------------------------------

"rhô = p = (covX*covY) / (seX*seY)"
rho = mean(scale(x) * scale(y)) = cor(x, y)

galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)

# "To see what data looks like for different values of ρ, here are six examples of pairs with
# correlations ranging from -0.9 to 0.99:
"https://rafalab.github.io/dsbook/book_files/figure-html/what-correlation-looks-like-1.png" %>%
   load.image() %>% plot(axes = F)

# As with the average and standard deviation, the sample correlation
# is the most commonly used estimate of the population correlation. This implies that the
# correlation we compute and use as a summary is a random variable.
# By way of illustration, let’s assume that the 179 pairs of fathers and sons is our entire
# population. A less fortunate geneticist can only afford measurements from a random sample
# of 25 pairs. The sample correlation can be computed with:
R = sample_n(galton_heights, 25, replace = T) %>%
   summarise(r = cor(father, son)) %>% pull(r)
R

"R is a random variable. We can run a Monte Carlo simulation to see its distribution"
R = replicate(1000, sample_n(galton_heights, 25, replace = T) %>%
                 summarise(r = cor(father, son)) %>% pull(r))
qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))

# We see that the expected value of R is the population correlation:
mean(R)
#> [1] 0.431
# and that it has a relatively high standard error relative to the range of values R can take:
sd(R)
#> [1] 0.161
"So, when interpreting correlations, remember that correlations derived from samples are
estimates containing uncertainty."

"Also, note that because the sample correlation is an average of independent draws, the
central limit actually applies. Therefore, for large enough N, the distribution of R is approximately
normal with expected value ρ.
The standard deviation, which is somewhat complex to derive, is sqrt((1-r^2) / (N-2)) "


# In our example, N = 25 does not seem to be large enough to make the approximation a
# good one:
N = 25
ggplot(aes(sample=R), data = data.frame(R)) +
   stat_qq() +
   geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))

"Correlation is not always a good summary of the relationship between two variables. The
following four artificial datasets, referred to as Anscombe’s quartet, famously illustrate this
point. All these pairs have a correlation of 0.82:"

"https://rafalab.github.io/dsbook/book_files/figure-html/ascombe-quartet-1.png" %>%
   load.image() %>% plot(axes = F)




# Conditional expectations ------------------------------------------------

"It turns out that if we were able to collect data from a very large number of fathers that are
72 inches, the distribution of their sons’ heights would be normally distributed. This implies
that the average of the distribution computed on this subset would be our best prediction."

# In general, we call this approach conditioning. The general idea is that we stratify a population
# into groups and compute summaries in each group

"The yi in the subpopulation have a distribution, referred to as the conditional distribution,
and this distribution has an expected value referred to as the conditional expectation.
      E(Y|X=x) - with x representing the fixed value that defines that subset
      SD(Y|X=x) "

# Because the conditional expectation E(Y | X = x) is the best predictor for the random variable
# Y for an individual in the strata defined by X = x, many data science challenges reduce
# to estimating this quantity. The conditional standard deviation quantifies the precision of
# the prediction.

conditional_avg = galton_heights %>%
   filter(round(father) == 72) %>%
   summarize(avg = mean(son)) %>%
   pull(avg)
conditional_avg

"If we want to make a prediction of any height, not just 72, we could apply the same approach
to each strata. Stratification followed by boxplots lets us see the distribution of each group:"
galton_heights %>% mutate(father_strata = factor(round(father))) %>%
   ggplot(aes(father_strata, son)) +
   geom_boxplot() +
   geom_point()

# Not surprisingly, the centers of the groups are increasing with height. Furthermore, these
# centers appear to follow a linear relationship

"The fact that these conditional averages follow a line is not a coincidence. In the next section,
we explain that the line these averages follow is what we call the regression line, which
improves the precision of our estimates. However, it is not always appropriate to estimate
conditional expectations with the regression line so we also describe Galton’s theoretical
justification for using the regression line."




# The regression line -----------------------------------------------------

# If we are predicting a random variable Y knowing the value of another X = x using a
# regression line, then we predict that for every standard deviation, σX, that x increases
# above the average μX, Y increase ρ standard deviations σY above the average μY with ρ
# the correlation between X and Y . The formula for the regression is therefore:

"(Y - μY)/σY = ρ(x - μX)/σX - in terms of standard units
<=> Y = μY + ρ.σY(x − μX)/σX "

"This results in the following formula :
   Y = b + mX - where m = ρ.σY/σX and b = μY - m.μX "

mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
galton_heights %>%
   ggplot(aes(father, son)) +
   geom_point(alpha = 0.5) +
   geom_abline(slope = r * s_y/s_x, intercept = mu_y - r * s_y/s_x * mu_x)

"We can make the same plots with standard units"
galton_galton_heights %>%
   ggplot(aes(scale(father), scale(son))) +
   geom_point(alpha = 0.5) +
   geom_abline(intercept = 0, slope = r)




# Let’s compare the two approaches to prediction that we have presented:
# 1. Round fathers’ heights to closest inch, stratify, and then take the average.
# 2. Compute the regression line and use it to predict.
"We use a Monte Carlo simulation sampling N = 50 families:"
N = 50
set.seed(1983)

conditional_avg = replicate(1000, {
   dat = sample_n(galton_heights, N)
   dat %>% filter(round(father) == 72) %>%
      summarise(avg = mean(son)) %>%
      .$avg
})

regression_prediction = replicate(1000, {
   dat = sample_n(galton_heights, N)
   mu_x <- mean(dat$father)
   mu_y <- mean(dat$son)
   s_x <- sd(dat$father)
   s_y <- sd(dat$son)
   r = cor(dat$father, dat$son)
   mu_y + r*(72-mu_x)/s_x*s_y
})

"Although the expected value of these two random variables is about the same:"
mean(conditional_avg, na.rm = TRUE)
#> [1] 70.5
mean(regression_prediction)
#> [1] 70.5
"The standard error for the regression prediction is substantially smaller:"
sd(conditional_avg, na.rm = TRUE)
#> [1] 0.964
sd(regression_prediction)
#> [1] 0.452

"The regression line is therefore much more stable than the conditional mean. There is an
intuitive reason for this. The conditional average is computed on a relatively small subset:
the fathers that are about 72 inches tall. The regression always uses all the data."




# Bivariate normal distribution -------------------------------------------


"The main way we motivate the use of correlation involves what is called the bivariate normal
distribution."
# When a pair of random variables is approximated by the bivariate normal distribution,
# scatterplots look like ovals. As we saw in Section 17.2, they can be thin (high correlation)
# or circle-shaped (no correlation).

"A more technical way to define the bivariate normal distribution is the following: if X is
a normally distributed random variable, Y is also a normally distributed random variable,
and the conditional distribution of Y for any X = x is approximately normal, then the pair
is approximately bivariate normal"

# If we think the height data is well approximated by the bivariate normal distribution, then
# we should see the normal approximation hold for each strata. Here we stratify the son heights
# by the standardized father heights and see that the assumption appears to hold:
galton_heights %>%
   mutate(z_father = round((father - mean(father)) / sd(father))) %>%
   filter(z_father %in% -2:2) %>%
   ggplot() +
   stat_qq(aes(sample = son)) +
   facet_wrap( ~ z_father)

"In summary, if our data is approximately bivariate, then the conditional expectation, the
best prediction of Y given we know the value of X, is given by the regression line."



# Variance explained ------------------------------------------------------

# The bivariate normal theory also tells us that the standard deviation of the conditional
# distribution described above is:
"SD(Y|X=x) = σY.sqrt(1 − ρ^2) <-> R^2 : determination coefficient"

"So if the data is bivariate normal, the variance is reduced by 1 − ρ^2, so we say that X
explains ρ^2 (the correlation squared) of the variance."





# Warning: there are two regression lines ---------------------------------

"What if we want to predict the father’s height based on the son’s? It is important to know
that this is not determined by computing the inverse function"
# We need to compute E(X | Y = y). Since the data is approximately bivariate normal, the
# theory described above tells us that this conditional expectation will follow a line with slope
# and intercept
m_2 <- r * s_x / s_y
b_2 <- mu_x - m_2 * mu_y

# So we get E(X | Y = y) = 40.9 + 0.41y.
"Here is a plot showing the two regression lines, with blue for the predicting son heights with
father heights and red for predicting father heights with son heights:"
m_1 <- r * s_y / s_x
b_1 <- mu_y - m_1*mu_x
galton_heights %>%
   ggplot(aes(father, son)) +
   geom_point(alpha = 0.5) +
   geom_abline(intercept = b_1, slope = m_1, col = "blue") +
   geom_abline(intercept = -b_2/m_2, slope = 1/m_2, col = "red")




# Assessment: Baseball as a Motivating Example ----------------------------

library(Lahman)
"Q1 What is the application of statistics and data science to baseball called?"
Sabermetrics

"Q2 Which of the following outcomes is not included in the batting average?"
Base on balls # it's not a hit

"Q3 Why do we consider team statistics as well as individual player statistics?"
The success of any individual player also depends on the strength of their team.

"Q4 You want to know whether teams with more at-bats per game have more runs per game."
Teams %>% filter(yearID %in% 1961:2001 ) %>%
   mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
   ggplot(aes(AB_per_game, R_per_game)) + 
   geom_point(alpha = 0.5)

"Q5 What does the variable “SOA” stand for in the Teams table?"
?Teams

"Q6 Filter the Teams data frame to include years from 1961 to 2001. Make a 
 scatterplot of runs per game versus at bats (AB) per game"
Teams %>% filter(yearID %in% 1961:2001 ) %>%
   mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
   ggplot(aes(AB_per_game, R_per_game)) + 
   geom_point(alpha = 0.5)

"Q7 Use the filtered Teams data frame from Question 6. Make a scatterplot of
 win rate (number of wins per game) versus number of fielding errors (E) per game."
Teams %>% filter(yearID %in% 1961:2001 ) %>%
   mutate(win_rate = W/G, fielding_errors_per_game = E/G) %>%
   ggplot(aes(fielding_errors_per_game, win_rate)) + 
   geom_point(alpha = 0.5)

"Q8 Use the filtered Teams data frame from Question 6. Make a scatterplot of 
 triples (X3B) per game versus doubles (X2B) per game."
Teams %>% filter(yearID %in% 1961:2001 ) %>%
   mutate(triples = X3B/G, doubles = X2B/G) %>%
   ggplot(aes(doubles, triples)) + 
   geom_point(alpha = 0.5)





# Assessment : Correlation ------------------------------------------------

"Q1 While studying heredity, Francis Galton developed what important statistical concept?"
Correlation

"Q2 The correlation coefficient is a summary of what?"
The trend between two variables

"Below is a scatter plot showing the relationship between two variables, x and y"
# the plot is a decreasing linear relation
"Q3 What's the correlation"
-0.9

"Q4 Instead of running a Monte Carlo simulation with a sample size of 25 from the 
179 father-son pairs described in the videos, we now run our simulation with a 
sample size of 50.
Would you expect the mean of our sample correlation to increase, decrease, or
stay approximately the same?"
Stay approximately the same

"Q5 Would you expect the standard deviation of our sample correlation to 
increase, decrease, or stay approximately the same?"
Decrease

"Q6 If X and Y are completely independent, what do you expect the value
of the correlation coefficient to be?"
0

"Q7 Load the Lahman library. Filter the Teams data frame to include years from 1961 to 2001.
What is the correlation coefficient between number of runs per game and number of at bats per game?"
library(Lahman)
Teams %>% 
   filter(yearID %in% 1961:2001) %>%
   summarise(RPG = R / G, ABPG = AB / G) %>%
   cor()

"Q8 What is the correlation coefficient between win rate (number of wins per game) 
and number of errors per game?"
Teams %>% 
   filter(yearID %in% 1961:2001) %>%
   summarise(WPG = W / G, EPG = E / G) %>%
   cor()

"Q9 What is the correlation coefficient between doubles (X2B) per game and 
triples (X3B) per game?"
Teams %>% 
   filter(yearID %in% 1961:2001) %>%
   summarise(X2 = X2B/G, X3 = X3B/G) %>%
   cor()





# Assessment : Stratification and Variance Explained ----------------------

"Q1 Consider Sons ~ Fathers
The slope of the regression line in this figure is equal to what, in words?"
Slope = (correlation of son and father heights) * (SD of sons’ heights / SD of fathers’ heights)

"Q2 Why does the regression line simplify to a line with intercept zero and slope
ρ  when we standardize our x and y variables?"
When we standardize variables, both x and y will have a mean of zero and a standard 
deviation of one. When you substitute this into the formula for the regression line, 
the terms cancel out until we have the following equation:  yi=ρxi .

"Q3 What is a limitation of calculating conditional means?"
Each stratum we condition on (e.g., a specific father’s height) may not have many data points.
Because there are limited data points for each stratum, our average values have large standard errors.
Conditional means are less stable than a regression line.

"Q4 A regression line is the best prediction of Y given we know the value of X when:"
X and Y follow a bivariate normal distribution.

"Q5 Which one of the following scatterplots depicts an x and y distribution that 
is NOT well-approximated by the bivariate normal distribution?"
U chart
# I disagree because this relation can be transformed and be regressed
# However there's a chart that will certainly return a week R2

"Q6 We previously calculated that the correlation coefficient  ρ  between fathers’ and sons’
heights is 0.5.
Given this, what percent of the variation in sons’ heights is explained by fathers’ heights?"
0.25 # R2

"Q7 Suppose the correlation between father and son’s height is 0.5, the standard deviation of
fathers’ heights is 2 inches, and the standard deviation of sons’ heights is 3 inches.
Given a one inch increase in a father’s height, what is the predicted change in the son’s height?"
0.5*3/2



# In the second part of this assessment, you'll analyze a set of mother and daughter
# heights, also from GaltonFamilies.
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
   filter(gender == "female") %>%     
   group_by(family) %>%     
   sample_n(1) %>%     
   ungroup() %>%     
   select(mother, childHeight) %>%     
   rename(daughter = childHeight)
attach(female_heights)
head(female_heights)

"Q8 Calculate the mean and standard deviation of mothers' heights, the mean and 
standard deviation of daughters' heights, and the correlaton coefficient between 
mother and daughter heights."
mean(mother); sd(mother)
mean(daughter); sd(daughter)
cor(mother, daughter)

"Q9 Calculate the slope and intercept of the regression line predicting daughters'
heights given mothers' heights. Given an increase in mother's height by 1 inch, how 
many inches is the daughter's height expected to change?"
summary(lm(daughter~mother))

"What percent of the variability in daughter heights is explained by the mother's height?"
cor(mother, daughter)^2

"A mother has a height of 60 inches.
What is the conditional expected value of her daughter's height given the mother's height?"
42.517+60*0.339

























