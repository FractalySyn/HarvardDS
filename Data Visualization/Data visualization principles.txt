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

