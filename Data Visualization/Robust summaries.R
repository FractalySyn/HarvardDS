rm(list=ls())
library(tidyverse); library(dslabs); library(ggplot2); library(ggthemes)
library(gridExtra); library(imager); library(ggrepel); library(RColorBrewer)



# Outliers ----------------------------------------------------------------


## We previously described how boxplots show outliers, but we did not provide a precise definition. Here we discuss outliers, approaches 
## that can help detect them, and summaries that take into account their presence.

## Outliers are very common in data science. Data recording can be complex and it is common to observe data points generated in error. 
## For example, an old monitoring device may read out nonsensical measurements before completely failing. Human error is also a source 
## of outliers, in particular when data entry is done manually. An individual, for instance, may mistakenly enter their height in cm
## instead of inches or put the decimal in the wrong place.

## How do we distinguish an outlier from measurements that were too big or too small simply due to expected variability? This is not 
## always an easy question to answer, but we try to provide some guidance. Let's begin with a simple case.
data("outlier_example")
str(outlier_example)

## Our colleague uses the fact that heights are usually well approximated by a normal distribution and summarizes the data with average 
## and standard deviation:
mean(outlier_example)
#> [1] 6.1
sd(outlier_example)
#> [1] 7.8
## and writes a report on the interesting fact that this group of males is much taller than usual. The average height is over six feet
## tall! Using your data science skills, however, you notice something else that is unexpected: the standard deviation is over 7 feet. 
## Adding and subtracting two standard deviations, you note that 95% of this population will have heights between -9.489, 21.697 feet, 
## which does not make sense. A quick plot reveals the problem:
boxplot(outlier_example)
## There appears to be at least one value that is nonsensical, since we know that a height of 180 feet is impossible. The boxplot detects 
## this point as an outlier.



"Median"
## When we have an outlier like this, the average can become very large. The median, defined as the value for which half the values are 
## smaller and the other half are bigger, is robust to such outliers. No matter how large we make the largest point, the median remains 
## the same.
median(outlier_example)
#> [1] 5.74 (1m75)



"The inter quartile range (IQR)"
## The box in boxplots is defined by the first and third quartile. These are meant to provide an idea of the variability in the data: 
## 50% of the data is within this range. The difference between the 3rd and 1st quartile (or 75th and 25th percentiles) is referred to
## as the inter quartile range (IQR). As is the case with the median, this quantity will be robust to outliers as large values do not
## affect it.
"We can do some math to see that for normally distributed data, the IQR / 1.349 approximates the standard deviation of the data had an 
 outlier not been present."
IQR(outlier_example) / 1.349
#> [1] 0.245 (7.458 cm)



# Tukey's definition of an outlier ----------------------------------------


## In R, points falling outside the whiskers of the boxplot are referred to as outliers. This definition of outlier was introduced by 
## Tukey. The top whisker ends at the 75th percentile plus 1.5 x IQR. Similarly the bottom whisker ends at the 25th percentile minus
## 1.5 x IQR. If we define the first and third quartiles as Q1 and Q3, respectively, then an outlier is anything outside the range:
"[Q1 - 1.5 x (Q3 - Q1); Q3 + 1.5 x (Q3 - Q1)]"
"1.5 is the default value of 'coef' in geom_boxplot() and 'range' in boxplot()"

## When the data is normally distributed, the standard units of these values are:
q3 <- qnorm(0.75)
q1 <- qnorm(0.25)
iqr <- q3 - q1
r <- c(q1 - 1.5*iqr, q3 + 1.5*iqr)
r
#> [1] -2.7  2.7
"Using the pnorm function, we see that 99.3% of the data falls in this interval"
## Keep in mind that this is not such an extreme event: if we have 1000 data points that are normally distributed, we expect to see 
## about 7 outside of this range. But these would not be outliers since we expect to see them under the typical variation.

## If we want an outlier to be rarer, we can increase the 1.5 to a larger number. Tukey also used 3 and called these far out outliers. 
## With a normal distribution, 100% of the data falls in this interval. This translates into about 2 in a million chance of being 
## outside the range

## The 180 inches measurement is well beyond the range of the height data:
max_height <- quantile(outlier_example, 0.75) + 3*IQR(outlier_example)
max_height
#>  75% 
#> 6.91
## If we take this value out, we can see that the data is in fact normally distributed as expected:
x <- outlier_example[outlier_example < max_height]
qqnorm(x)
qqline(x)




# Median absolute deviation -----------------------------------------------

## Another way to robustly estimate the standard deviation in the presence of outliers is to use the median absolute deviation (MAD). 
## To compute the MAD, we first compute the median, and then for each value we compute the distance between that value and the median. 
## The MAD is defined as the median of these distances. For technical reasons not discussed here, this quantity needs to be multiplied
## by 1.4826 to assure it approximates the actual standard deviation. The mad function already incorporates this correction. For the 
## height data, we get a MAD of:
mad(outlier_example)
#> [1] 0.237





# Exercises ---------------------------------------------------------------

library(HistData)
data("Galton")
x = Galton$child

# without outliers
mean(x); median(x)
sd(x); mad(x)

# with outliers
x_outlier = x; x_outlier[1] = x[1]*10
sd(x_outlier) # 2.51 -> 18.2
median(x_outlier) # same median
mad(x_outlier) # same mad



# Case study --------------------------------------------------------------

## The heights we have been looking at are not the original heights reported by students. The original reported heights are also included 
## in the dslabs package and can be loaded like this
data("reported_heights")
## heights were reported in different units (feet, inches, cm)
reported_heights = reported_heights %>%
  mutate(original_heights = height, height = as.numeric(height))
reported_heights
## Some students self-reported their heights using feet and inches rather than just inches. Others used centimeters and others were just 
## trolling. For now we will remove these entries:
reported_heights <- filter(reported_heights, !is.na(height))
## If we compute the average and standard deviation, we notice that we obtain strange results. The average and standard deviation are
## different from the median and MAD:
reported_heights %>% 
  group_by(sex) %>%
  summarize(average = mean(height), sd = sd(height),
            median = median(height), MAD = mad(height))
## This suggests that we have outliers, which is confirmed by creating a boxplot:
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/height-outlier-ggplot-1.png"))

## We can see some rather extreme values. To see what these values are, we can quickly look at the largest values using the arrange 
## function:
reported_heights %>% arrange(desc(height)) %>% top_n(10, height)
## The first seven entries look like strange errors. However, the next few look like they were entered as centimeters instead of inches.
## Since 184 cm is equivalent to six feet tall, we suspect that 184 was actually meant to be 72 inches.

"We can review all the nonsensical answers by looking at the data considered to be far out by Tukey"
attach(reported_heights)
whisker = 3 * IQR(height)
max = quantile(height, 0.75) + whisker
min = quantile(height, 0.25) - whisker
reported_heights %>% filter(!between(height, min, max)) %>%
  select(original_heights) %>%
  head(10)

## Examining these heights carefully, we see two common mistakes: entries in centimeters, which turn out to be too large, and entries of 
## the form x.y with x and y representing feet and inches, respectively, which turn out to be too small. Some of the even smaller values,
## such as 1.6, could be entries in meters.

In the Data Wrangling part of this book we will learn techniques for correcting these values and converting them into inches. Here we were able to detect this problem using careful data exploration to uncover issues with the data: the first step in the great majority of data science projects.
