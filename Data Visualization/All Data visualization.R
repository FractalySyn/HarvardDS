rm(list=ls())

## Data visualization is the strongest tool of what we call exploratory data analysis (EDA)
## We are reminded of the saying "a picture is worth a thousand words". Data visualization provides a powerful 
## way to communicate a data-driven finding. In some cases, the visualization is so convincing that no follow-up 
## analysis is required.

## As an example, consider that measurement devices sometimes fail and that most data analysis procedures are not 
## designed to detect these. Yet these data analysis procedures will still give you an answer. The fact that it can
## be difficult or impossible to notice an error just from the reported results makes data visualization particularly 
## important.

library(dplyr)
library(ggplot2)

"A reason ggplot2 is easy for beginners is that its default behavior is carefully chosen to satisfy the great 
 majority of cases and is visually pleasing. As a result, it is possible to create informative and elegant graphs 
 with relatively simple and readable code.

 One limitation is that ggplot2 is designed to work exclusively with data tables in tidy format (where rows are 
 observations and columns are variables). However, a substantial percentage of datasets that beginners work with 
 are in, or can be converted into, this format. An advantage of this approach is that, assuming that our data is 
 tidy, ggplot2 simplifies plotting code and the learning of grammar for a variety of plots."

## Cheat sheet of ggplot2 - useful ressource
"https://rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf"


# Components of a graph ---------------------------------------------------


"Data: The US murders data table is being summarized. We refer to this as the data component."

"Geometry: The plot above is a scatterplot. This is referred to as the geometry component. Other possible 
 geometries are barplot, histogram, smooth densities, qqplot, and boxplot. We will learn more about these 
 in the Data Visualization part of the book."

"Aesthetic mapping: The plot uses several visual cues to represent the information provided by the dataset. 
 Each point represents a different observation, and we map data about these observations to visual cues like 
 x- and y-scale. Color is another visual cue that we map to region. We refer to this as the aesthetic mapping 
 component. How we define the mapping depends on what geometry we are using."

## The points are labeled with the state abbreviations.
## The range of the x-axis and y-axis appears to be defined by the range of the data. They are both on log-scales.
## There are labels, a title, a legend, and we use the style of The Economist magazine.




# ggplot2 objects, geometries, mappings ---------------------------------------------------------

library(dslabs)
data(murders)

## The first step in creating a ggplot2 graph is to define a ggplot object.
ggplot(data = murders)
## It renders a plot, in this case a blank slate since no geometry has been defined. The only style choice 
## we see is a grey background.

"In ggplot2 we create graphs by adding layers. Layers can define geometries, compute summary statistics, 
 define what scales to use, or even change styles. To add layers, we use the symbol +"


## Usually, the first added layer defines the geometry. We want to make a scatterplot. Taking a quick look at
## the cheat sheet, we see that the function used to create plots with this geometry is geom_point.
## Geometry function names follow the pattern: geom_X where X is the name of the geometry. Some examples include 
## geom_point, geom_bar, and geom_histogram.

## For geom_point() to run properly we need to provide data and a mapping. Data is already provided through the ggplot
## To find out what mappings are expected, we read the Aesthetics section of the help file geom_point help file:
?geom_point
"x, y, alpha.... are the arguments that can be customized via geom_point"
"x and y are required"


## The aes function connects data with what we see on the graph by defining aesthetic mappings and will be one of the functions 
## you use most often when plotting.
ggplot(data = murders) +
  geom_point(aes(x = population/10^6, y = total))
## We can assign part of a ggplot setup and use it later
p = ggplot(murders)
p + geom_point(aes(x = population, y = total))




# Other layers ------------------------------------------------------------

## The geom_label() and geom_text() functions permit us to add text to the plot with and without a rectangle 
## behind the text, respectively.
ggplot(data = murders) +
  geom_point(aes(x = population, y = total)) +
  geom_text(aes(population, total, label = abb)) ## we specify x and y cause labels are affected to points

## change the point size
ggplot(data = murders) +
  geom_point(aes(x = population, y = total), size = 3) +
  geom_text(aes(population, total, label = abb))
"size is not a mapping: whereas mappings use data from specific observations and need to be inside aes(), operations 
 we want to affect all the points the same way do not need to be included inside aes."

## Moving labels 
ggplot(data = murders) +
  geom_point(aes(x = population, y = total), size = 3) +
  geom_text(aes(population, total, label = abb), nudge_x = 1.5) # all labels are slightly moved to the right



# Global aesthetic mappings -----------------------------------------------

## In the previous line of code, we define the mapping aes(population/10^6, total) twice, once in each geometry. 
## We can avoid this by using a global aesthetic mapping. We can do this when we define the blank slate ggplot 
## object. The function ggplot() contains an argument that permits us to define aesthetic mappings:
ggplot(data = murders, aes(x = population, y = total, label = abb)) +
  geom_point(size = 3) +
  geom_text(nudge_x = 1.5)
"Note that the geom_point function does not need a label argument and therefore ignores that aesthetic.
 If necessary, we can override the global mapping by defining a new mapping within each layer. These local definitions 
 override the global."
ggplot(data = murders, aes(x = population, y = total, label = abb)) +
  geom_point(size = 3) +
  geom_text(aes(x = 20, y = 800, label = "Hi"), size = 10)
  

# Scales ------------------------------------------------------------------

## Change scaling
ggplot(data = murders, aes(x = population, y = total, label = abb)) +
  geom_point(size = 3) +
  geom_text(nudge_x = 0.05) + # need to reduce the nudge
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")
## This particular transformation is so common that ggplot2 provides the specialized functions
## to rewrite the code like this:
ggplot(data = murders, aes(x = population, y = total, label = abb)) +
  geom_point(size = 3) +
  geom_text(nudge_x = 0.05) +
  scale_x_log10() +
  scale_y_log10() 

## Title and axis labels
murders_plot = ggplot(data = murders, aes(x = population, y = total, label = abb)) +
                geom_text(nudge_x = 0.05) +
                scale_x_log10() +
                scale_y_log10() +
                xlab("Populations in millions (log scale)") + 
                ylab("Total number of murders (log scale)") +
                ggtitle("US Gun Murders in 2010")
murders_plot + geom_point(size = 3) # we removed the geom_point for simplifying the next sections



# More customization ------------------------------------------------------

## Categories as colors
murders_plot + geom_point(size = 3, col = "blue")

## Colors depending on the regions
"A nice default behavior of ggplot2 is that if we assign a categorical variable to color, it automatically 
 assigns a different color to each category and also adds a legend."
murders_plot + geom_point(aes(col = region), size = 3)
## Here we see yet another useful default behavior: ggplot2 automatically adds a legend that maps color to region. 
"To avoid adding this legend we set the geom_point argument show.legend = FALSE."
murders_plot + 
  geom_point(aes(col = region), size = 3) +
  scale_color_discrete(name = "USA Region")

## Plot the average line
## The average murder rate is the type of y = r.x (y and x already plotted here)
## In the log-scale this turns into log(y) = log(r) + log(x)
## -> log(r) is the intercept and the slope of log(x) is 1
avgrate = summarise(murders, avg_rate = sum(total)/sum(population))[1] %>% as.numeric()
murders_plot + 
  geom_point(aes(col = region), size = 3) +
  scale_color_discrete(name = "USA Region") +
  geom_abline(intercept = log10(avgrate), slope = 1, lty = 3, lwd = 1.5, color = "blue")



# Add-on packages - ggthemes and ggrepel ---------------------------------------------------------


## The style of a ggplot2 graph can be changed using the theme functions. Several themes are included as part 
## of the ggplot2 package. In fact, for most of the plots in this book, we use a function in the dslabs package
## that automatically sets a default theme
dslabs::ds_theme_set()
## Many other themes are added by the package ggthemes. Among those are the theme_economist theme that we used. 
## After installing the package, you can change the style by adding a layer like this:
library(ggthemes)
murders_plot = murders_plot + 
  geom_point(aes(col = region), size = 3) +
  scale_color_discrete(name = "USA Region") +
  geom_abline(intercept = log10(avgrate), slope = 1, lty = 3, lwd = 1.5, color = "blue")
murders_plot + theme_economist()
murders_plot + theme_fivethirtyeight()

## The final difference has to do with the position of the labels. In our plot, some of the labels fall on top 
## of each other. The add-on package ggrepel includes a geometry that adds labels while ensuring that they don't
## fall on top of each other. 
library(ggrepel)
"We simply change geom_text with geom_text_repel. Look at the next section"




# All together ------------------------------------------------------------
rm(list=ls())
library(ggplot2); library(ggthemes); library(ggrepel); library(dslabs)
data("murders")

# Population in millions
rate = summarise(murders, avg_rate = sum(total)/sum(population)*10^6)[1] %>% as.numeric()

ggplot(murders, aes(population/10^6, total, label = abb)) +
  geom_point(aes(col = region), size = 3) +
  scale_color_discrete(name = "USA regions") +
  geom_abline(intercept = log10(rate), slope = 1, lty = 3, lwd = 1.5, color = "blue") +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") + 
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  theme_economist()



# Quick plots with qplot() -------------------------------------------------

## ggplot() requires a data frame and allows us to go deep into customization by reading all informations of the
## data frame. If we only want a simple plot with two vectors we can use base functions like plot(), hist(), etc.
## and qplot()
qplot(murders$population, murders$total) + theme_economist()



# Plot multiple charts ----------------------------------------------------

library(gridExtra)
chart1 = qplot(murders$population)
chart2 = qplot(murders$population, murders$total) + theme_economist()
grid.arrange(chart1, chart2, ncol = 2)
















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











## Here, we aim to provide some general principles we can use as a guide for effective data visualization. Much of this section 
## is based on a talk by Karl Broman titled "Creating Effective Figures and Tables" and includes some of the figures which 
## were made with code that Karl makes available on his GitHub repository, as well as class notes from Peter Aldhous' Introduction 
## to Data Visualization course. Following Karl's approach, we show some examples of plot styles we should avoid, explain how to 
## improve them, and use these as motivation for a list of principles. We compare and contrast plots that follow these principles to 
## those that don't.
browseURL("https://github.com/kbroman/Talk_Graphs") # Karl Broman
browseURL("http://paldhous.github.io/ucb/2016/dataviz/index.html") # Peter Aldhous

rm(list=ls())
library(tidyverse); library(dslabs); library(ggplot2); library(ggthemes)
library(gridExtra); library(imager); library(ggrepel); library(RColorBrewer)




# Encoding data using visual cues -----------------------------------------

## let's suppose we want to report the results from two hypothetical polls regarding browser preference taken in 2000 and then 
## 2015. For each year, we are simply comparing five quantities - the five percentages. A widely used graphical representation
## of percentages, popularized by Microsoft Excel, is the pie chart:
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/piechart-1.png"))

## Both the angle and area of each pie slice are proportional to the quantity the slice represents.
## This turns out to be a sub-optimal choice since, as demonstrated by perception studies, humans are not good at precisely 
## quantifying angles and are even worse when area is the only available visual cue. The donut chart is an example of a plot 
## that uses only area:
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/donutchart-1.png"))

## It is not easy to tell from the plot. In fact, the pie R function help file states that:
"Pie charts are a very bad way of displaying information. The eye is good at judging linear measures and bad at judging relative 
 areas. A bar chart or dot chart is a preferable way of displaying this type of data."
## In this case, simply showing the numbers is not only clearer, but would also save on printing costs if printing a paper copy:

## Browser	2000	2015
## Opera	    3	   2
## Safari	   21	   22
## Firefox	 23	   21
## Chrome	   26	   29
## IE	       28    27

## The preferred way to plot these quantities is to use length and position as visual cues, since humans are much better at 
## judging linear measures. The barplot uses this approach by using bars of length proportional to the quantities of interest.
## By adding horizontal lines at strategically chosen values, in this case at every multiple of 10, we ease the visual burden
## of quantifying through the position of the top of the bars.
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/two-barplots-1.png"))

"Notice how much easier it is to see the differences in the barplot. In fact, we can now determine the actual percentages by 
 following a horizontal line to the x-axis."
## If for some reason you need to make a pie chart, label each pie slice with its respective percentage so viewers do not have 
## to infer them from the angles or area:
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/excel-barplot-1.png"))



# Know when to include 0 --------------------------------------------------

## When using barplots, it is misinformative not to start the bars at 0. This is because, by using a barplot, we are implying the 
## length is proportional to the quantities being displayed. By avoiding 0, relatively small differences can be made to look much
## bigger than they actually are. This approach is often used by politicians or media organizations trying to exaggerate a 
## difference. Below is an illustrative example used by Peter Aldhous in this lecture:
plot(load.image("https://rafalab.github.io/dsbook/dataviz/img/class2_8.jpg"))
## Also explore this course 
browseURL("http://paldhous.github.io/ucb/2016/dataviz/week2.html")

## From the plot above, it appears that apprehensions have almost tripled when, in fact, they have only increased by about 16%. 
## Starting the graph at 0 illustrates this clearly:
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/barplot-from-zero-1-1.png"))
## Other examples
plot(load.image("https://rafalab.github.io/dsbook/dataviz/img/Bush-cuts.png"))
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/barplot-from-zero-2-1.png"))
plot(load.image("https://rafalab.github.io/dsbook/dataviz/img/venezuela-election.png"))
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/barplot-from-zero-3-1.png"))


"When using position rather than length, it is then not necessary to include 0."
## This is particularly the case when we want to compare differences between groups relative to the within-group variability. 
## Here is an illustrative example showing country average life expectancy stratified across continents in 2012:
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/points-plot-not-from-zero-1.png"))
## Note that in the plot on the left, which includes 0, the space between 0 and 43 adds no information and makes it harder to 
## compare the between and within group variability.




# Do not distort quantities -----------------------------------------------

plot(load.image("https://rafalab.github.io/dsbook/dataviz/img/state-of-the-union.png"))
## Judging by the area of the circles, the US appears to have an economy over five times larger than China's and over 30 times 
## larger than France's. However, if we look at the actual numbers, we see that this is not the case. The actual ratios are 2.6
## and 5.8 times bigger than China and France, respectively. The reason for this distortion is that the radius, rather than the 
## area, was made to be proportional to the quantity, which implies that the proportion between the areas is squared: 2.6 turns 
## into 6.5 and 5.8 turns into 34.1. Here is a comparison of the circles we get if we make the value proportional to the radius 
## and to the area: radius =  'rayon'
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/area-not-radius-1.png"))
## Not surprisingly, ggplot2 defaults to using area rather than radius.


  
# Order categories by a meaningful value ----------------------------------

## When one of the axes is used to show categories, as is done in barplots, the default ggplot2 behavior is to order the categories 
## alphabetically when they are defined by character strings. If they are defined by factors, they are ordered by the factor levels.
## We rarely want to use alphabetical order. Instead, we should order by a meaningful quantity. In all the cases above, the barplots 
## were ordered by the values being displayed. The exception was the graph showing barplots comparing browsers. In this case, we kept
## the order the same across the barplots to ease the comparison.  

## We previously learned how to use the reorder function, which helps us achieve this goal. To appreciate how the right order can help 
## convey a message, suppose we want to create a plot to compare the murder rate across states. We are particularly interested in the 
## most dangerous and safest states. Note the difference when we order alphabetically (the default) versus when we order by the actual 
## rate:
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/do-not-order-alphabetically-1.png"))
## We can make the second plot like this:
data(murders)
mutate(murders, murder_rate = total / population * 100000) %>%
mutate(state = reorder(state, murder_rate)) %>% ## reorder (sort) by murder rates
  ggplot(aes(murder_rate, state, fill = state)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 7)) + # change axes coord font size
  ylab("") +
  theme(legend.position = "none")




# Show the data -----------------------------------------------------------

## A commonly seen plot used for comparisons between groups, popularized by software such as Microsoft Excel, is the dynamite plot, 
## which shows the average and standard errors
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/show-data-1-1.png"))
## The average of each group is represented by the top of each bar and the antennae extend out from the average to the average plus 
## two standard errors. If all ET receives is this plot, he will have little information on what to expect if he meets a group of 
## human males and females. The bars go to 0: does this mean there are tiny humans measuring less than one foot? Are all males taller
## than the tallest females? Is there a range of heights? ET can't answer these questions since we have provided almost no information
## on the height distribution.

## This brings us to our first principle: show the data. This simple ggplot2 code already generates a more informative plot than the 
## barplot by simply showing all the data points:
data("heights")
heights %>% ggplot(aes(sex, height)) + geom_point()
## For example, this plot gives us an idea of the range of the data. However, this plot has limitations as well, since we can't really 
## see all the 238 and 812 points plotted for females and males, respectively, and many points are plotted on top of each other. As we
## have previously described, visualizing the distribution is much more informative. 

"But before doing this, we point out two ways we can improve a plot showing all the points."
## The first is to add jitter, which adds a small random shift to each point : we minimize the number of points that fall on top of 
## each other and, therefore, get a better visual sense of how the data is distributed. A second improvement comes from using alpha 
## blending: making the points somewhat transparent. The more points fall on top of each other, the darker the plot, which also helps
## us get a sense of how the points are distributed.
heights %>% ggplot(aes(sex, height)) + geom_jitter(width = 0.1, alpha = 0.2)
## Now we start getting a sense that, on average, males are taller than females. 




# Ease comparisons --------------------------------------------------------

"Use common axes"
## Since there are so many points, it is more effective to show distributions rather than individual points. We therefore show 
## histograms for each group:
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/common-axes-histograms-wrong-1.png"))
## However, from this plot it is not immediately obvious that males are, on average, taller than females. We have to look carefully 
## to notice that the x-axis has a higher range of values in the male histogram. An important principle here is to keep the axes the 
## same when comparing data across two plots. Below we see how the comparison becomes easier:
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/common-axes-histograms-right-1.png"))


"Align plots vertically to see horizontal changes and horizontally to see vertical changes"
## In these histograms, the visual cue related to decreases or increases in height are shifts to the left or right, respectively:
## horizontal changes. Aligning the plots vertically helps us see this change when the axes are fixed:
heights %>% ggplot(aes(height, ..density.., fill = sex)) + ## ..density.. replace count by frequency
  geom_histogram(binwidth = 1, col = "black") +
  facet_grid(sex ~ .)
## This plot makes it much easier to notice that men are, on average, taller.

## If , we want the more compact summary provided by boxplots, we then align them horizontally since, by default, boxplots move up 
## and down with changes in height. Following our show the data principle, we then overlay all the data points:
heights %>% ggplot(aes(sex, height)) +
  geom_boxplot(aes(fill = sex), coef = 3) + # coef increase whiskers size
  geom_jitter(width = 0.1, alpha = 0.1) +
  theme(legend.position = "none")

## Now contrast and compare these three plots, based on exactly the same data:
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/show-the-data-comparison-1.png"))
## Notice how much more we learn from the two plots on the right. Barplots are useful for showing one number, but not very useful 
## when we want to describe distributions.


"Consider transformations"
## The combination of an incorrectly chosen barplot and a failure to use a log transformation when one is merited can be particularly 
## distorting. As an example, consider this barplot showing the average population sizes for each continent in 2015:
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/no-transformations-wrong-use-of-barplot-1.png"))
## From this plot, one would conclude that countries in Asia are much more populous than in other continents. Following the show the
## data principle, we quickly notice that this is due to two very large countries, which we assume are India and China:
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/no-transformation-1.png"))
## Using a log transformation here provides a much more informative plot. We compare the original barplot to a boxplot using the log 
## scale transformation for the y-axis:
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/correct-transformation-1.png"))
## With the new plot, we realize that countries in Africa actually have a larger median population size than those in Asia.
## Other transformations you should consider are the logistic transformation (logit), useful to better see fold changes in odds, and 
## the square root transformation (sqrt), useful for count data.


"Visual cues to be compared should be adjacent"
## For each continent, let's compare income in 1970 versus 2010. When comparing income data across regions between 1970 and 2010, we 
## made a figure similar to the one below, but this time we investigate continents rather than regions.
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/boxplots-not-adjacent-1.png"))
## The default in ggplot2 is to order labels alphabetically so the labels with 1970 come before the labels with 2010, making the 
## comparisons challenging because a continent's distribution in 1970 is visually far from its distribution in 2010. It is much 
## easier to make the comparison between 1970 and 2010 for each continent when the boxplots for that continent are next to each other
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/boxplot-adjacent-comps-1.png"))


"Use color"
## The comparison becomes even easier to make if we use color to denote the two things we want to compare:
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/boxplot-adjacent-comps-with-color-1.png"))




# Think of the color blind ------------------------------------------------

## About 10% of the population is color blind. Unfortunately, the default colors used in ggplot2 are not optimal for this group. 
## However, ggplot2 does make it easy to change the color palette used in the plots. An example of how we can use a color blind 
## friendly palette is described here: 
browseURL("http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette")
browseURL("http://bconnelly.net/2013/10/creating-colorblind-friendly-figures/.")
## Many graphics packages allow you to easily make use of the ColorBrewer palettes. In ggplot2, this is done with the scale_color_brewer 
## command.
data("diamonds")
ggplot(diamonds, aes(x=carat, y=price, color=cut)) +
  geom_point() +
  scale_color_brewer(palette="Dark2")




# Plots for two variables -------------------------------------------------

## In general, you should use scatterplots to visualize the relationship between two variables. In every single instance in which 
## we have examined the relationship between two variables, including total murders versus population size, life expectancy versus
## fertility rates, and infant mortality versus income, we have used scatterplots. This is the plot we generally recommend. However,
## there are some exceptions and we describe two alternative plots here: 
"the slope chart and the Bland-Altman plot"


"Slope charts"
## One exception where another type of plot may be more informative is when you are comparing variables of the same type, but at 
## different time points and for a relatively small number of comparisons. For example, comparing life expectancy between 2010 and 
## 2015. In this case, we might recommend a slope chart.
data("gapminder")
west = c("Western Europe","Northern Europe","Southern Europe",
          "Northern America","Australia and New Zealand")
gapminder %>% filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7) %>%
  mutate(location = ifelse(year == 2010, 1, 2), 
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"), location+0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
  xlab("") + ylab("Life Expectancy")
## An advantage of the slope chart is that it permits us to quickly get an idea of changes based on the slope of the lines. Although
## we are using angle as the visual cue, we also have position to determine the exact values. Comparing the improvements is a bit 
## harder with a scatterplot:
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/scatter-plot-instead-of-slope-1.png"))
## In the scatterplot, we have followed the principle use common axes since we are comparing these before and after. However, if we 
## have many points, slope charts stop being useful as it becomes hard to see all the lines.


"Bland-Altman plot, Tukey mean-difference plot or MA-plot"
##Since we are primarily interested in the difference, it makes sense to dedicate one of our axes to it. The Bland-Altman plot, shows 
## the difference versus the average:
gapminder %>% filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7) %>%
  mutate(year = paste0("life_expectancy_", year)) %>%
  select(country, year, life_expectancy) %>%
  spread(year, life_expectancy) %>% # separate 'year' levels in two columns instead of a single row
  mutate(average = (life_expectancy_2015 + life_expectancy_2010) / 2,
         difference = life_expectancy_2015 - life_expectancy_2010) %>%
  ggplot(aes(average, difference, label = country)) +
  geom_point() + 
  geom_text_repel() +
  xlab("Average of 2010 and 2015") + ylab("Difference between 2015 and 2010")
## Here, by simply looking at the y-axis, we quickly see which countries have shown the most improvement. We also get an idea of the 
## overall value from the x-axis.




# Encoding a third variable -----------------------------------------------

## An earlier scatterplot showed the relationship between infant survival and average income. Below is a version of this plot that 
## encodes three variables: OPEC membership, region, and population.
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/encoding-third-variable-1.png"))

## We encode categorical variables with color and shape. These shapes can be controlled with shape argument. Below are the shapes 
## available for use in R. For the last five, the color goes inside.
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/available-shapes-1.png"))

## For continuous variables, we can use color, intensity, or size. When selecting colors to quantify a numeric variable, we choose
## between two options: sequential and diverging. 
"Sequential colors are suited for data that goes from high to low." # High values are clearly distinguished from low values. Here 
## are some examples offered by the package RColorBrewer:
library(RColorBrewer)
display.brewer.all(type="seq")
"Diverging colors are used to represent values that diverge from a center." # We put equal emphasis on both ends of the data range:
## higher than the center and lower than the center. An example of when we would use a divergent pattern would be if we were to show 
## height in standard deviations away from the average. Here are some examples of divergent patterns:
display.brewer.all(type="div")



#  Avoid pseudo-three-dimensional plots -----------------------------------

## The figure below, taken from the scientific literature, shows three variables: dose, drug type and survival. Although your 
## screen/book page is flat and two-dimensional, the plot tries to imitate three dimensions and assigned a dimension to each variable.
plot(load.image("https://rafalab.github.io/dsbook/dataviz/img/fig8b.png"))

"Humans are not good at seeing in three dimensions" ## (which explains why it is hard to parallel park) and our limitation is even 
## worse with regard to pseudo-three-dimensions. To see this, try to determine the values of the survival variable in the plot above.
## Can you tell when the purple ribbon intersects the red one? This is an example in which we can easily use color to represent the 
## categorical variable instead of using a pseudo-3D:
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/colors-for-different-lines-1.png"))
## Notice how much easier it is to determine the survival values.

## Pseudo-3D is sometimes used completely gratuitously: plots are made to look 3D even when the 3rd dimension does not represent a 
## quantity. This only adds confusion and makes it harder to relay your message. Here are two examples:
plot(load.image("https://rafalab.github.io/dsbook/dataviz/img/fig2d.png"))




#  Avoid too many significant digits --------------------------------------

## By default, statistical software like R returns many significant digits. The default behavior in R is to show 7 significant digits. 
## That many digits often adds no information and the added visual clutter can make it hard for the viewer to understand the message.

"Useful ways to change the number of significant digits or to round numbers are signif and round."
"You can define the number of significant digits globally by setting options like this: options(digits = 3)."
state	      year	Measles	Pert Polio
California	1940	37.9	18.3	0.8
California	1950	13.9	4.7	  2.0
California	1960	14.1	NA	  0.3
California	1970	1.0	  NA	  NA
California	1980	0.4	  0.1	  NA
"Another principle related to displaying tables is to place values being compared on columns rather than rows. 
Note that our table above is easier to read than this one:"
state	      disease	    1940	1950	1960	1970	1980
California	Measles	    37.9	13.9	14.1	 1  	0.4
California	Pertussis	  18.3	4.7	  NA	  NA	  0.1
California	Polio	      0.8	  2.0	  0.3	  NA  	NA

plot(load.image(""))



# Know your audience ------------------------------------------------------

"Graphs can be used for 1) our own exploratory data analysis, 2) to convey a message to experts, or 3) to help tell a story to 
 a general audience. Make sure that the intended audience understands each element of the plot."

## As a simple example, consider that for your own exploration it may be more useful to log-transform data and then plot it. 
## However, for a general audience that is unfamiliar with converting logged values back to the original measurements, using
## a log-scale for the axis instead of log-transformed values will be much easier to digest.



# Case study: vaccines and infectious diseases ----------------------------

## Vaccines have helped save millions of lives. In the 19th century, before herd immunization was achieved through vaccination
## programs, deaths from infectious diseases, such as smallpox and polio, were common. However, today vaccination programs have
## become somewhat controversial despite all the scientific evidence for their importance.

## The controversy started with a paper published in 1988 and led by Andrew Wakefield claiming there was a link between the 
## administration of the measles, mumps, and rubella (MMR) vaccine and the appearance of autism and bowel disease. Despite much 
## scientific evidence contradicting this finding, sensationalist media reports and fear-mongering from conspiracy theorists led 
## parts of the public into believing that vaccines were harmful. As a result, many parents ceased to vaccinate their children. 
## This dangerous practice can be potentially disastrous given that the Centers for Disease Control (CDC) estimates that 
## vaccinations will prevent more than 21 million hospitalizations and 732,000 deaths among children born in the last 20 years 
## Yet misconceptions persist, in part due to self-proclaimed activists who continue to disseminate misinformation about vaccines.

## The data used for these plots were collected, organized, and distributed by the Tycho Project
data("us_contagious_diseases")

## We create a temporary object dat that stores only the measles data, includes a per 100,000 rate, orders states by average value
## of disease and removes Alaska and Hawaii since they only became states in the late 1950s. Note that there is a weeks_reporting 
## column that tells us for how many weeks of the year data was reported. We have to adjust for that value when computing the rate.
dat = us_contagious_diseases %>% filter(!(state %in% c("Hawaii", "Alaska")) & disease == "Measles") %>%
  mutate(rate = count / population * 100 * 52 / weeks_reporting) %>%
  mutate(state = reorder(state, rate))

## We can now easily plot disease rates per year. Here are the measles data from California:
## We add a vertical line at 1963 since this is when the vaccine was introduced 
filter(dat, state == "California" & !is.na(rate)) %>%
  ggplot(aes(year, rate)) +
  geom_line() + 
  ylab("Cases in %")  + 
  geom_vline(xintercept = 1963, col = "blue")


"Tile plot"
##  We have three variables to show: year, state, and rate. In the WSJ figure, they use the x-axis for year, the y-axis for state,
## and color hue to represent rates. However, the color scale they use, which goes from yellow to blue to green to orange to red,
## can be improved.
"In our example, we want to use a sequential palette since there is no meaningful center, just low and high rates."
"We use the geometry geom_tile() to tile the region with colors representing disease rates. We use a square root transformation to 
 avoid having the really high counts dominate the plot. Notice that missing values are shown in grey. Note that once a disease was 
 pretty much eradicated, some states stopped reporting cases all together. This is why we see so much grey after 1980."
dat %>% ggplot(aes(year, state, fill = rate)) +
  geom_tile(color = "grey50") + # first color attributed to all tiles - neutral
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") + # change nuances, test to see changes
  geom_vline(xintercept = 1963, col = "blue") +
  theme_minimal() + 
  ggtitle("Measles") + ylab("") + xlab("") +
  theme(legend.position = "bottom", text = element_text(size = 8), 
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) 
  

"This plot makes a very striking argument for the contribution of vaccines. However, one limitation of this plot is that it uses 
 color to represent quantity, which we earlier explained makes it harder to know exactly how high values are going. Position and 
 lengths are better cues. If we are willing to lose state information, we can make a version of the plot that shows the values with 
 position. We can also show the average for the US, which we compute like this:"
avg = us_contagious_diseases %>% filter(disease == "Measles") %>%
  group_by(year) %>%
  summarise(us_rate = sum(count, na.rm = TRUE) / sum(population, na.rm = TRUE) *100)
dat %>% filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50", show.legend = F, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1) +
  scale_y_continuous(trans = "sqrt", breaks = c(0.05, 0.25, 1.25 ,3)) +
  ggtitle("Cases in % by state") + xlab("") + ylab("") +
  geom_text(data = data.frame(x = 1955, y = 0.50), mapping = aes(x, y, label = "US average"), color = "black") +
  geom_vline(xintercept = 1963, col = "blue")


plot(load.image(""))
plot(load.image(""))
plot(load.image(""))







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
















## The Titanic was a British ocean liner that struck an iceberg and sunk on its maiden voyage in 1912 from the United Kingdom to New York. More than
## 1,500 of the estimated 2,224 passengers and crew died in the accident, making this one of the largest maritime disasters ever outside of war. The 
## ship carried a wide range of passengers of all ages and both genders, from luxury travelers in first-class to immigrants in the lower classes.
## However, not all passengers were equally likely to survive the accident. We use real data about a selection of 891 passengers to learn who was 
## on the Titanic and which passengers were more likely to survive.

rm(list = ls())
library(titanic); library(tidyverse); library(ggplot2); library(ggridges)
options(digits = 3)

titanic = titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))


ggplot(titanic, aes(Age, ..count.., fill = Sex)) +
  geom_density() +
  facet_grid(Sex ~ .)
ggplot(titanic, aes(Age, fill = Sex)) +
  geom_density() +
  facet_grid(Sex ~ .)
ggplot(titanic, aes(Age, Sex, fill = Sex)) +
  geom_density_ridges(jittered_points = TRUE, scale = 1, alpha = 0.5)
hist(titanic$Age, breaks = 30)
plot(density(titanic$Age, na.rm = T, bw = 1))

summary(titanic$Sex)
summary(titanic[which(titanic$Age == 40),]$Sex)
summary(titanic[which(titanic$Age > 18 & titanic$Age < 35),]$Sex)
summary(titanic[which(titanic$Age <17),]$Sex)

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))
ggplot(titanic, aes(sample = Age)) +
  geom_qq(dparams = params) +
  geom_abline()

ggplot(titanic) +
  geom_bar(aes(Survived, fill = Sex), position = position_dodge())

filter(titanic, Survived == 1) %>%
  group_by(Sex) %>% summary()


ggplot(titanic, aes(Age, ..count.., fill = Survived)) +
  geom_density(alpha = 0.2)

filter(titanic, Fare > 0) %>%
  ggplot(aes(Survived, Fare)) +
  scale_y_continuous(trans = "log2") +
  geom_boxplot() +
  geom_jitter(alpha = 0.05)
filter(titanic, Fare > 0) %>%
  ggplot(aes(Survived, Fare)) +
  geom_boxplot() +
  geom_point(alpha = 0.05)

filter(titanic, Fare < 13 & Fare > 6) %>% summary()

## fill barplot
ggplot(titanic, aes(Pclass, fill = Survived)) +
  geom_bar()
ggplot(titanic, aes(Pclass, fill = Survived)) +
  geom_bar(position = position_fill())
ggplot(titanic, aes(Survived, fill = Pclass)) +
  geom_bar(position = position_fill())

ggplot(titanic, aes(Age, ..count.., fill = Survived)) +
  geom_density() +
  facet_grid(Sex ~ Pclass)


















