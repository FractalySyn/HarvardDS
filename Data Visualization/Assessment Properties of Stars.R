rm(list=ls())
library(tidyverse); library(dslabs); library(ggplot2); library(ggthemes)
library(gridExtra); library(imager); library(ggrepel); library(RColorBrewer)

data(stars)
options(digits = 3)

# Astronomy is one of the oldest data-driven sciences. In the late 1800s, the director of
# the Harvard College Observatory hired women to analyze astronomical data, which at the 
# time was done using photographic glass plates. These women became known as the “Harvard Computers”.
# They computed the position and luminosity of various astronomical objects such as stars and galaxies.
# (If you are interested, you can learn more about the Harvard Computers). Today, astronomy is even 
# more of a data-driven science, with an inordinate amount of data being produced by modern instruments 
# every day.
# 
# In the following exercises we will analyze some actual astronomical data to inspect properties of 
# stars, their absolute magnitude (which relates to a star's luminosity, or brightness), temperature
# and type (spectral class).

"Q1 Load the stars data frame from dslabs. This contains the name, absolute magnitude, temperature 
in degrees Kelvin, and spectral class of selected stars. Absolute magnitude (shortened in these
problems to simply ""magnitude"") is a function of star luminosity, where negative values of 
magnitude have higher luminosity."
# What is the mean magnitude? The standard deviation ?
mean(stars$magnitude)
sd(stars$magnitude)

"Q2 Make a density plot of the magnitude.
How many peaks are there in the data?"
stars %>% ggplot(aes(magnitude)) +
   geom_density()

"Q3 Examine the distribution of star temperature.
Which of these statements best characterizes the temperature distribution?"
summary(stars$temp)
stars %>% ggplot(aes(temp)) +
   geom_histogram(binwidth = 500)

"Q4 Make a scatter plot of the data with temperature on the x-axis and magnitude on 
the y-axis and examine the relationship between the variables. Recall that lower
magnitude means a more luminous (brighter) star."
stars %>% ggplot(aes(temp, magnitude)) +
   geom_point()

"Q5 For various reasons, scientists do not always follow straight conventions when 
making plots, and astronomers usually transform values of star luminosity and temperature 
before plotting. Flip the y-axis so that lower values of magnitude are at the top of 
the axis (recall that more luminous stars have lower magnitude) using scale_y_reverse().
Take the log base 10 of temperature and then also flip the x-axis."
stars %>% ggplot(aes(log10(temp), magnitude)) +
   geom_point() +
   scale_x_reverse() +
   scale_y_reverse() 
# Remember low magnitude is brighter

"Q6 The trends you see allow scientists to learn about the evolution and lifetime of
stars. The primary group of stars to which most stars belong we will call the main 
sequence stars (discussed in question 4). Most stars belong to this main sequence, 
however some of the more rare stars are classified as “old” and “evolved” stars. 
These stars tend to be hotter stars, but also have low luminosity, and are known as white dwarfs.
How many white dwarfs are there in our sample?"
Look at the previous chart

"Q7 Consider stars which are not part of the Main Group but are not old/evolved (white dwarf)
stars. These stars must also be unique in certain ways and are known as giants. Use the plot 
from Question 5 to estimate the average temperature of a giant.
Which of these temperatures is closest to the average temperature of a giant?"
5000k
log10(5000)

"Q8The least lumninous star in the sample with a surface temperature over 5000K is _________."
stars %>% ggplot(aes(log10(temp), magnitude, label = star)) +
   geom_point() +
   scale_x_reverse() +
   scale_y_reverse() +
   geom_vline(xintercept = 3.7) +
   geom_text()
# VanMaanen's Star
"The two stars with lowest temperature and highest luminosity are known as supergiants.
The two supergiants in this dataset are ____________."
Antares, Betelgeuse
" The Sun is a ______________."
main sequence star

"Q9 Remove the text labels and color the points by star type. This classification
describes the properties of the star's spectrum, the amount of light produced at various wavelengths."
stars %>% ggplot(aes(log10(temp), magnitude, color = type)) +
   geom_point(size=4) +
   scale_x_reverse() +
   scale_y_reverse()
# M O No







