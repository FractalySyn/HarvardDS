"We will be working with two types of variables: categorical and numeric. Each can be divided into two other groups: 
  categorical can be ordinal or not, whereas numerical variables can be discrete or continuous."

## When each entry in a vector comes from one of a small number of groups, we refer to the data as categorical data.
## Two simple examples are sex (male or female) and regions (Northeast, South, North Central, West). Some categorical
## data can be ordered even if they are not numbers per se, such as spiciness (mild, medium, hot). In statistics textbooks,
## ordered categorical data are referred to as ordinal data.

## Keep in mind that discrete numeric data can be considered ordinal. Although this is technically true, we usually reserve
## the term ordinal data for variables belonging to a small number of different groups, with each group having many
## members. In contrast, when we have many groups with few cases in each group, we typically refer to them as discrete
## numerical variables.

rm(list=ls())
library(tidyverse); library(dslabs); library(ggplot2); library(ggthemes); library(gridExtra)
## Cheat sheet of ggplot2 - useful ressource
"https://rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf"



"It turns out that, in some cases, the average and the standard deviation are pretty much all we need to understand 
 the data. We will learn data visualization techniques that will help us determine when this two number summary is 
 appropriate or not."



# Density bandwidth -------------------------------------------------------

## The smooth density is computed by reducing the size of histogram ranges until we obtain a small curve
## The density line connects all the points of the tightened histogram
## We apply frequencies instead of observations on the y-axis because the density has to represent the population
a = rnorm(1000, 0, 3)
hist(a, freq = T, breaks = seq(-10, 10, 0.1))
plot(density(a, bw = 0.1))
"test with different bandwidths"



# Quantile-quantile plots -- QQ-plot -------------------------------------------------

"A systematic way to assess how well the normal distribution fits the data is to check if the observed and predicted 
 proportions match"

data("heights"); attach(heights)
quantiles_probas = seq(0.05, 0.95, by = 0.05)
## data quantiles
sample_q = quantile(height, quantiles_probas)
## theoretical normal quantiles
theo_q = qnorm(quantiles_probas, mean(height), sd(height))
## plot comparison
qplot(theo_q, sample_q) 
## -> it seems to fit well to the normal distribution
qplot(theo_q, sample_q) + geom_abline() ## abline y=x

## With ggplot - returns as many quantiles as data points
ggplot(heights, aes(sample = scale(height))) + ## scale is a matrix returning data and mean/sd -> no need to specify dparams in geom_qq()
  geom_qq() + ## use the sample information to center and scale the distribution -> returns centered quantiles 
  geom_abline()



# Stratification ----------------------------------------------------------

## In data analysis we often divide observations into groups based on the values of one or more variables 
## associated with those observations. For example in the next section we divide the height values into 
## groups based on a sex variable: females and males. 
"We call this procedure stratification and refer to the resulting groups as strata."
## Stratification is common in data visualization because we are often interested in how the distribution of variables differs 
## across different subgroups.



# ggplot2 practice --------------------------------------------------------

## Barplot with proportion
data("murders")
count(murders, region)
## count returns n that we can use to calculate proportions
data_region = count(murders, region) %>% mutate(proportion = n/sum(n)); data_region
ggplot(data_region, aes(region, proportion, fill = region)) + 
  geom_bar(stat = "identity") ## to avoid x/y (2 variables) error

## Histograms
data("heights")
filter(heights, sex == "Female") %>%
  ggplot(aes(height)) +
  geom_histogram(binwidth = 1, fill = "midnightblue", col = "white") +
  xlab("Male heights (in)") + ggtitle("Histogram") +
  theme_economist_white()

## Density plots
## adjust multiplies by its value the bandwidth i.e. the smoothness
p1 = filter(heights, sex == "Female") %>%
       ggplot(aes(height)) +
       geom_density(fill = "deepskyblue2", col = "black", adjust = 1)
p2 = filter(heights, sex == "Female") %>%
  ggplot(aes(height)) +
  geom_density(fill = "deepskyblue4", col = "black", adjust = 5)
grid.arrange(p1, p2, ncol = 2)
## qplot
qplot(heights$height, geom = "density", color = I("red"), fill = I("gray")) 
"I() means keep it as it is i.e no conversion -> here aes() is called in qplot and convert it to a factor"

## Boxplots
male = heights$height[heights$sex == "Male"] ## don't need to use which
female = heights$height[heights$sex == "Female"]
boxplot(male, female)
## with ggplot
ggplot(heights, aes(x = sex, y = height)) + geom_boxplot()
# with qplot
heights %>% qplot(sex, height, data = ., geom = "boxplot") 

## Images
tibble = expand.grid(x = 1:12, y = 1:10) %>% mutate(z = 1:120) # this is the tidy version of a matrix, matrix(1:120, 12, 10). 
ggplot(tibble, aes(x, y, fill = z)) +
  geom_raster() +
  scale_fill_gradientn(colors =  terrain.colors(10))

