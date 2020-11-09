rm(list=ls())
library(tidyverse); library(dslabs); library(ggplot2); library(ggthemes)
library(gridExtra); library(imager); library(ggrepel); library(RColorBrewer)


data(temp_carbon)

"Q1 Load the temp_carbon dataset from dslabs, which contains annual global temperature
anomalies (difference from 20th century mean temperature in degrees Celsius), temperature
anomalies over the land and ocean, and global carbon emissions (in metric tons). Note 
that the date ranges differ for temperature and carbon emissions."
# Which of these code blocks return the latest year for which carbon emissions are reported?
tail(temp_carbon %>% arrange(year)) 
"2014"
temp_carbon %>%
   filter(!is.na(carbon_emissions)) %>%
   pull(year) %>%
   max()
temp_carbon %>%
   filter(!is.na(carbon_emissions)) %>%
   .$year %>%
   max()
temp_carbon %>%
   filter(!is.na(carbon_emissions)) %>%
   select(year) %>%
   max()

"Q2 Inspect the difference in carbon emissions in temp_carbon from the first available 
year to the last available year."
head(temp_carbon)
min = temp_carbon$year[which(!is.na(temp_carbon$carbon_emissions))] %>% min()
max = temp_carbon$year[which(!is.na(temp_carbon$carbon_emissions))] %>% max()
temp_carbon$carbon_emissions[temp_carbon$year == max] / temp_carbon$carbon_emissions[temp_carbon$year == min]

"Q3 Inspect the difference in temperature in temp_carbon from the first available year to the last available year.
What is the first year for which global temperature anomaly (temp_anomaly) data are available?"
min = temp_carbon$year[which(!is.na(temp_carbon$temp_anomaly))] %>% min()
max = temp_carbon$year[which(!is.na(temp_carbon$temp_anomaly))] %>% max()
temp_carbon$temp_anomaly[temp_carbon$year == max] - temp_carbon$temp_anomaly[temp_carbon$year == min]

"Q4 Create a time series line plot of the temperature anomaly. Only include years where 
temperatures are reported. Save this plot to the object p"
p = temp_carbon %>%
   filter(!is.na(temp_anomaly)) %>%
   ggplot(aes(year, temp_anomaly)) +
   geom_line()
"Which command adds a blue horizontal line indicating the 20th century mean temperature?"
p + geom_hline(aes(yintercept = 0), col = "blue")

"Q5 Continue working with p, the plot created in the previous question.
Change the y-axis label to be 'Temperature anomaly (degrees C)'. Add a title, 
'Temperature anomaly relative to 20th century mean, 1880-2018'. Also add a text
layer to the plot: the x-coordinate should be 2000, the y-coordinate should be
0.05, the text should be '20th century mean', and the text color should be blue.
Which of the following code blocks is correct?"
p + ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
   ylab("Temperature anomaly (degrees C)") +
   geom_text(aes(2000, 0.05, label = "20th century mean"), color = "blue")

"Q6 When was the earliest year with a temperature above the 20th century mean?"
1939
"When was the last year with an average temperature below the 20th century mean?"
1977
"In what year did the temperature anomaly exceed 0.5 degrees Celsius for the first time?"


"Q7 Add layers to the previous plot to include line graphs of the temperature anomaly 
in the ocean (ocean_anomaly) and on land (land_anomaly). Assign different colors to the
lines. Compare the global temperature anomaly to the land temperature anomaly and ocean
temperature anomaly."
p + ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
   ylab("Temperature anomaly (degrees C)") +
   geom_text(aes(2000, 0.05, label = "20th century mean"), color = "blue") +
   geom_line(aes(y=ocean_anomaly), color = "orange") +
   geom_line(aes(y=land_anomaly), color = "green")
"Which region has the largest 2018 temperature anomaly relative to the 20th century mean?"
Land
"Which region has the largest change in temperature since 1880?"
Land
"Which region has a temperature anomaly pattern that more closely matches the global pattern?"
Ocean

# A major determinant of Earth's temperature is the greenhouse effect. Many gases trap heat and 
# reflect it towards the surface, preventing heat from escaping the atmosphere. The greenhouse 
# effect is vital in keeping Earth at a warm enough temperature to sustain liquid water and life;
# however, changes in greenhouse gas levels can alter the temperature balance of the planet.
# 
# The greenhouse_gases data frame from dslabs contains concentrations of the three most significant
# greenhouse gases: carbon dioxide ( CO2 , abbreviated in the data as co2), methane ( CH4 , ch4 in 
# the data), and nitrous oxide ( N2O , n2o in the data). Measurements are provided every 20 years
# for the past 2000 years.
data("greenhouse_gases")

"Q8 Complete the code outline below to make a line plot of concentration on the y-axis 
by year on the x-axis. Facet by gas, aligning the plots vertically so as to ease 
comparisons along the year axis. Add a vertical line with an x-intercept at the year 
1850, noting the unofficial start of the industrial revolution and widespread fossil
fuel consumption. Note that the units for ch4 and n2o are ppb while the units for co2 are ppm."
greenhouse_gases %>%
   ggplot(aes(year, concentration, color = gas)) +
   geom_line() +
   facet_grid(gas~., scales = "free") +
   geom_vline(xintercept = 1850) +
   ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
   ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

"Q9 Which gas was stable at approximately 275 ppm/ppb until around 1850?"
co2
"Which gas more than doubled in concentration since 1850?"
ch4
"Which gas decreased in concentration since 1850?"
none
"Which gas had the smallest magnitude change since 1850?"
n20
"Which gas increased exponentially in concentration after 1850?"
all


# While many aspects of climate are independent of human influence, and co2 levels 
# can change without human intervention, climate models cannot reconstruct current 
# conditions without incorporating the effect of manmade carbon emissions. These 
# emissions consist of greenhouse gases and are mainly the result of burning fossil
# fuels such as oil, coal and natural gas.
data("temp_carbon")

"Q10 Make a time series line plot of carbon emissions (carbon_emissions) from the
temp_carbon dataset. The y-axis is metric tons of carbon emitted per year.
Which of the following are true about the trend of carbon emissions?"
temp_carbon %>%
   ggplot(aes(year, carbon_emissions)) +
   geom_line()
Carbon emissions were essentially zero before 1850 and have increased exponentially since then.
Carbon emissions in 2014 were about 4 times as large as 1960 emissions.
Carbon emissions have doubled since the late 1970s.
Carbon emissions change with the same trend as atmospheric greenhouse gas levels (co2, ch4, n2o)



# We saw how greenhouse gases have changed over the course of human history, but how has
# CO2  (co2 in the data) varied over a longer time scale? The historic_co2 data frame in
# dslabs contains direct measurements of atmospheric co2 from Mauna Loa since 1959 as well
# as indirect measurements of atmospheric co2 from ice cores dating back 800,000 years.
data("historic_co2")
head(historic_co2)

"Q11 Make a line plot of co2 concentration over time (year), coloring by the measurement
source (source). Save this plot as co2_time for later use.
Which of the following are true about co2_time, the time series of co2 over the last 800,000 years?"
co2_time = historic_co2 %>%
   ggplot(aes(year, co2, color = source)) +
   geom_line()
co2_time
Modern co2 levels are higher than at any point in the last 800,000 years.
There are natural cycles of co2 increase and decrease lasting 50,000-100,000 years per cycle.
In most cases, it appears to take longer for co2 levels to decrease than to increase.

# "Q12 One way to differentiate natural co2 oscillations from today's manmade co2 spike is by 
# examining the rate of change of co2. The planet is affected not only by the absolute 
# concentration of co2 but also by its rate of change. When the rate of change is slow, 
# living and nonliving systems have time to adapt to new temperature and gas levels, but 
# when the rate of change is fast, abrupt differences can overwhelm natural systems. How 
# does the pace of natural co2 change differ from the current rate of change?"

"Q12 Use the co2_time plot saved above. Change the limits as directed to investigate 
the rate of change in co2 over various periods with spikes in co2 concentration.
Change the x-axis limits to -800,000 and -775,000. About how many years did it take for 
co2 to rise from 200 ppmv to its peak near 275 ppmv?"
co2_time + xlim(c(-800000, -775000)) #♥ 10000
"Change the x-axis limits to -375,000 and -330,000. About how many years did it take
for co2 to rise from the minimum of 180 ppm to its peak of 300 ppmv?"
co2_time + xlim(c(-375000, -330000)) # 25000
"Change the x-axis limits to -140,000 and -120,000. About how many years did it take 
for co2 to rise from 200 ppmv to its peak near 280 ppmv?"
co2_time + xlim(c(-140000, -120000)) # 9000
"Change the x-axis limits to -3000 and 2018 to investigate modern changes in co2. 
About how many years did it take for co2 to rise from its stable level around 275
ppmv to the current level of over 400 ppmv?"
co2_time + xlim(c(1500, 2018)) # 250




