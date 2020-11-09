rm(list=ls())
library(tidyverse); library(dslabs); library(ggplot2); library(ggthemes); library(gridExtra)


# Case study : new insights on poverty ------------------------------------

## Hans Rosling was the co-founder of the Gapminder Foundation, an organization dedicated to educating the
## public by using data to dispel common myths about the so-called developing world. The organization uses data 
## to show how actual trends in health and economics contradict the narratives that emanate from sensationalist 
## media coverage of catastrophes, tragedies, and other unfortunate events.
browseURL("https://www.gapminder.org/")

## In this section, we use data to attempt to answer the following two questions: 
## Is it a fair characterization of today's world to say it is divided into western rich nations and the 
## developing world in Africa, Asia, and Latin America?
## Has income inequality across countries worsened during the last 40 years?

data("gapminder")
gapminder = as_tibble(gapminder)
study = filter(gapminder, year == 2015) %>% 
  mutate(inf_mort = infant_mortality/10) %>%
  select(country, inf_mort)
qplot(study$inf_mort, geom = "density", color = I("red"), fill = I("gray")) 



# Scatterplots ------------------------------------------------------------

## This plot shows that life expectancy is negatively correlated with the # of children per woman
## We can highlight the world regions
filter(gapminder, year == 1962) %>%
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point(size = 3) 
## In 1962, "the West versus developing world" view was grounded in some reality. Is this still the case 50 years later?



# Faceting ----------------------------------------------------------------

## We could easily plot the 2012 data in the same way we did for 1962. To make comparisons, however, side by side 
## plots are preferable. In ggplot2, we can achieve this by faceting variables: we stratify the data by some 
## variable and make the same plot for each strata.

## To achieve faceting, we add a layer with the function facet_grid, which automatically separates the plots. 
## This function lets you facet by up to two variables using columns to represent one variable and rows to 
## represent the other. The function expects the row and column variables to be separated by a ~
filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point(size = 3) +
  facet_grid(continent ~ year) # y = continents, x = years
## We can also compare only one variable (here the years)
filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point(size = 3) +
  facet_grid(. ~ year)
## This plot clearly shows that the majority of countries have moved from the developing world cluster to the western 
## world one. In 2012, the western versus developing world view no longer makes sense. This is particularly clear when 
## comparing Europe to Asia, the latter of which includes several countries that have made great improvements.


## To explore how this transformation happened through the years, we can make the plot for several years. For example,
## we can add 1970, 1980, 1990, and 2000. If we do this, we will not want all the plots on the same row, the default 
## behavior of facet_grid, since they will become too thin to show the data. Instead, we will want to use multiple rows
## and columns. The function facet_wrap permits us to do this by automatically wrapping the series of plots so that each 
## display has viewable dimensions:
gapminder %>%
  filter(year %in% c(1960, 1970, 1980, 1990, 2000, 2010) & continent %in% c("Europe", "Asia")) %>%
  ggplot(aes(fertility, life_expectancy, color = continent)) + 
  geom_point(size = 3) +
  facet_wrap(. ~ year, ncol = 3, nrow = 2)
## This plot clearly shows how most Asian countries have improved at a much faster rate than European ones.
"Using facet_ the scales are fixed by default for better comparison"
gapminder %>%
  filter(year %in% c(1960, 2010)) %>%
  ggplot(aes(fertility, life_expectancy, color = continent)) + 
  geom_point(size = 3) +
  facet_wrap(. ~ year, ncol = 3, nrow = 2, scales = "free")
## In the plot above, we have to pay special attention to the range to notice that the plot on the right has a larger 
## life expectancy.



# Time series -------------------------------------------------------------

## USA fertility rates from 1960
gapminder %>% 
  filter(country == "United States") %>% 
  ggplot(aes(year, fertility)) +
  geom_point()

## When the points are regularly and densely spaced, as they are here, we create curves by joining the points with lines, 
## to convey that these data are from a single series, here a country.
gapminder %>% 
  filter(country == "United States") %>% 
  ggplot(aes(year, fertility)) +
  geom_line()

## This is particularly helpful when we look at two countries. If we subset the data to include two countries, one
## from Europe and one from Asia, then adapt the code above:
ct = c("France", "Philippines")
gapminder %>% 
  filter(country %in% ct & !is.na(fertility)) %>% 
  ggplot(aes(year, fertility, color = country)) + ## using color autmatically groups
  geom_line()



#  Data transformations ---------------------------------------------------

##  Here we learn how transformations can sometimes help provide more informative summaries and plots.

## Using current US dollars as a unit, a person surviving on an income of less than $2 a day is defined to be living 
## in absolute poverty. The GDP values are adjusted for inflation and represent current US dollars, so these values 
## are meant to be comparable across the years. Of course, these are country averages and within each country there 
## is much variability. All the graphs and insights described below relate to country averages and not to individuals.
gapminder = mutate(gapminder, dollars_per_day = gdp/population/365)

filter(gapminder, year == 1970 & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, col = "black")
## In this plot, we see that for the majority of countries, averages are below $10 a day. However, the majority of the 
## x-axis is dedicated to the 35 countries with averages above $10. So the plot is not very informative about countries
## with values below $10 a day.
"-> log-transformation"
filter(gapminder, year == 1970 & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, col = "black")

"Which base ?"
## In general, we do not recommend using the natural log for data exploration and visualization. This is because while 
## 2^x or 10^x are easy to compute in our heads, the same is not true for e^x 

## A log base 10 is too high here. Furthermore it amplifies the binwidth (10 times). 

"Transform the values or the scale ?"
## The advantage of showing logged scales is that the original values are displayed in the plot, which are easier to 
## interpret. For example, we would see "32 dollars a day" instead of "5 log base 2 dollars a day".
filter(gapminder, year == 1970 & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, col = "black") +
  scale_x_continuous(trans = "log2")

## The histogram above suggests that the 1970 country income distribution has two modes: one at about 2 dollars per day 
## and another at about 32 dollars per day. This bimodality is consistent with a dichotomous world made up of countries
## with average incomes less than $8 a day and countries above that.



# Comparing multiple distributions with boxplots and ridge plots ----------

## A histogram showed us that the 1970 income distribution values show a dichotomy. However, the histogram does not show
## us if the two groups of countries are west versus the developing world.
## Let's start by quickly examining the data by region. 
"We reorder the regions by the median value and use a log scale"
filter(gapminder, year == 1970 & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot(aes(dollars_per_day, region)) +
  geom_point() +
  scale_x_continuous(trans = "log2")

## We can already see that there is indeed a "west versus the rest" dichotomy: we see two clear groups, with the 
## rich group composed of North America, Northern and Western Europe, New Zealand and Australia. We define groups 
## based on this observation:
gapminder = mutate(gapminder, group = case_when(
  region %in% c("Western Europe", "Northern Europe","Southern Europe", 
                "Northern America", 
                "Australia and New Zealand") ~ "West",
  region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
  region %in% c("Caribbean", "Central America", 
                "South America") ~ "Latin America",
  continent == "Africa" & region != "Northern Africa" ~ "Sub-Saharan",
  TRUE ~ "Others"))


## We turn this group variable into a factor to control the order of the levels:
gapminder = mutate(gapminder, group = factor(group, levels = c("Others", "Latin America",
                                                               "East Asia", "Sub-Saharan",
                                                               "West")))

## Boxplots
## Do not forget to reverse x and y axis
bxplot = filter(gapminder, year == 1970 & !is.na(gdp)) %>%
          ggplot(aes(group, dollars_per_day)) +
          geom_boxplot() +
          scale_y_continuous(trans = "log2") +
          xlab("") + ## remove "group"
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) # add groups name vertically 
bxplot

## Boxplots have the limitation that by summarizing the data into five numbers, we might miss important characteristics
## of the data. One way to avoid this is by showing the data.
bxplot + geom_point(alpha = 0.3)

## Although not the case here, when the number of data points is so large that there is over-plotting, showing the data
## can be counterproductive. Boxplots help with this by providing a five-number summary, but this has limitations too. 
## For example, boxplots will not permit us to discover bimodal distributions.


"Ridge plots"
## In cases in which we are concerned that the boxplot summary is too simplistic, we can show stacked smooth densities 
## or histograms. We refer to these as ridge plots. Because we are used to visualizing densities with values in the 
## x-axis, we stack them vertically. Also, because more space is needed in this approach, it is convenient to overlay 
## them. The package ggridges provides a convenient function for doing this.
library(ggridges)
## Do not forget to reverse x and y axis
ridges = filter(gapminder, year == 1970 & !is.na(gdp)) %>%
            ggplot(aes(dollars_per_day, group, fill = group)) +
            scale_x_continuous(trans = "log2") +
            ylab("") +
            theme(legend.position = "none") # remove the legend
ridges + geom_density_ridges()

## A useful geom_density_ridges parameter is scale, which lets you determine the amount of overlap, with scale = 1 meaning 
## no overlap and larger values resulting in more overlap.
ridges + geom_density_ridges(scale = 1)
ridges + geom_density_ridges(scale = 0.7)
ridges + geom_density_ridges(scale = 2.5)

## If the number of data points is small enough, we can add them to the ridge plot using the following code
ridges + geom_density_ridges(jittered_points = TRUE)

## To show data points, but without using jitter we can use the following code to add what is referred to as a 
"rug representation" ## of the data.
ridges + geom_density_ridges(jittered_points = TRUE,
                             position = position_points_jitter(height = 0),
                             point_shape = "|", point_size = 3)



# 1970 vs 2010 income distributions ---------------------------------------

filter(gapminder, year %in% c(1970, 2010) & !is.na(gdp)) %>%
  mutate(world_part = ifelse(group == "West", "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ world_part)


## Before we interpret the findings of this plot, we notice that there are more countries represented in the 2010 
## histograms than in 1970: the total counts are larger. One reason for this is that several countries were founded
## after 1970. For example, the Soviet Union divided into several countries during the 1990s. Another reason is that
## data was available for more countries in 2010.

"We remake the plots using only countries with data available for both years"
## intersect()
list1 = filter(gapminder, year == 1970 & !is.na(gdp))$country
list2 = filter(gapminder, year == 2010 & !is.na(gdp))$country
country_list = intersect(list1, list2) ## returns only countries that are in both lists

filter(gapminder, year %in% c(1970, 2010) & country %in% country_list) %>%
  mutate(world_part = ifelse(group == "West", "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ world_part)
## We now see that the rich countries have become a bit richer, but percentage-wise, the poor countries appear to have
## improved more. In particular, we see that the proportion of developing countries earning more than $16 a day increased
## substantially.


"To see which specific regions improved the most, we can remake the boxplots we made above, but now adding the year 2010 
 and then using facet to compare the two years."
filter(gapminder, year %in% c(1970, 2010) & country %in% country_list) %>%
  ggplot(aes(group, dollars_per_day, fill = group)) +
  geom_boxplot() +
  theme(legend.position = "none") +
  scale_y_continuous(trans = "log2") +
  xlab("") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(. ~ year)

"Because we want to compare each region before and after, it would be convenient to have the 1970 boxplot next to the 
 2010 boxplot for each region."
## So instead of faceting, we keep the data from each year together and ask to color (or fill) them depending on the year. 
## Note that groups are automatically separated by year and each pair of boxplots drawn next to each other. Because year is
## a number, we turn it into a factor since ggplot2 automatically assigns a color to each category of a factor. 
filter(gapminder, year %in% c(1970, 2010) & country %in% country_list) %>%
  mutate(year = factor(year)) %>% ## convert year to a factor
  ggplot(aes(group, dollars_per_day, fill = year)) + ## fill by year - automatically groups
  geom_boxplot() +
  scale_y_continuous(trans = "log2") +
  xlab("") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


## The previous data exploration suggested that the income gap between rich and poor countries has narrowed considerably 
## during the last 40 years. We used a series of histograms and boxplots to see this. We suggest a succinct way to convey
## this message with just one plot.
filter(gapminder, year %in% c(1970, 2010) & country %in% country_list) %>%
  ggplot(aes(dollars_per_day)) +
  geom_density(fill = "grey") +
  scale_x_continuous(trans = "log2") +
  facet_grid(. ~ year) +
  theme(legend.position = "none")
## In the 1970 plot, we see two clear modes: poor and rich countries. In 2010, it appears that some of the poor countries
## have shifted towards the right, closing the gap.

## The next message we need to convey is that the reason for this change in distribution is that several poor countries 
## became richer, rather than some rich countries becoming poorer. To do this, we can assign a color to the groups we
## identified during data exploration.  
a = filter(gapminder, year == 1970 & country %in% country_list) %>%
       mutate(world_part = ifelse(group == "West", "West", "Developing"))
length(a[a$world_part == "West",]$country); length(a[a$world_part == "Developing",]$country)
"However, we first need to learn how to make these smooth densities in a way that preserves information on the number of 
 countries in each group. Indeed there are 4 times more Developing countries than West countries"
filter(gapminder, year %in% c(1970, 2010) & country %in% country_list) %>%
  mutate(world_part = ifelse(group == "West", "West", "Developing")) %>%
  ggplot(aes(dollars_per_day, fill = world_part, lwd = I(0.5))) +
  scale_x_continuous(trans = "log2", limit = c(0.1, 300)) + # need to change the x-axis range - 0.1 bc. log(0) desn't exist
  geom_density(alpha = 0.3) +
  facet_grid(year ~ .)
## This makes it appear as if there are the same number of countries in each group. To change this, we will need to learn to 
## access computed variables with geom_density function.

"Accessing computed variables" ## here we prefer to plot the count on the y-axis instead of the frequency
## ## we change the y-axis doing this in aes() -> y = ..count.. OR y = after_stat(count)
densities = filter(gapminder, year %in% c(1970, 2010) & country %in% country_list) %>%
              mutate(world_part = ifelse(group == "West", "West", "Developing")) %>%
              ggplot(aes(x = dollars_per_day, y = ..count.., fill = world_part, lwd = I(0.5))) + 
              scale_x_continuous(trans = "log2", limit = c(0.1, 300))
densities + geom_density(alpha = 0.3) +
          facet_grid(year ~ .)
## If we want the densities to be smoother, we use the bw argument so that the same bandwidth is used in each density. 
densities + geom_density(alpha = 0.3, bw = 0.7) +
          facet_grid(year ~ .)


"To visualize if any of the groups defined above are driving this we can quickly make a ridge plot:"
filter(gapminder, year %in% c(1970, 2010) & country %in% country_list) %>%
  ggplot(aes(dollars_per_day, group, fill = group)) +
  scale_x_continuous(trans = "log2") +
  theme(legend.position = "none") +
  geom_density_ridges(scale = 1.5) +
  facet_grid(. ~ year) +
  ylab("")

"Another way to achieve this is by stacking the densities on top of each other:"
filter(gapminder, year %in% c(1970, 2010) & country %in% country_list) %>%
  ggplot(aes(dollars_per_day, fill = group, lwd = I(0.8))) +
  scale_x_continuous(trans = "log2", limit = c(0.1, 300)) +
  geom_density(alpha = 0.3, bw = 0.7, position = "stack") + ## stacking instead of overlapping
  facet_grid(year ~ .)
  
## Notice that we order the levels of the group so that the West's density is plotted first, then Sub-Saharan Africa. 
## Having the two extremes plotted first allows us to see the remaining bimodality better.
"-> stacking only puts the density on the others, we compare the shape, not the size !!"

"Weighted densities"
## As a final point, we note that these distributions weigh every country the same. So if most of the population is 
## improving, but living in a very large country, such as China, we might not appreciate this. We can actually weight 
## the smooth densities using the weight mapping argument. The plot then looks like this:
filter(gapminder, year %in% c(1970, 2010) & country %in% country_list) %>%
  ggplot(aes(dollars_per_day, fill = group, lwd = I(0.8))) +
  scale_x_continuous(trans = "log2", limit = c(0.1, 300)) +
  geom_density(alpha = 0.3, bw = 0.7, position = "stack", aes(weight = population)) + ## weighting by pop size
  facet_grid(year ~ .)
## This particular figure shows very clearly how the income distribution gap is closing with most of the poor remaining 
## in Sub-Saharan Africa.









