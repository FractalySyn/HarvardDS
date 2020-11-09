rm(list = ls())
options(digits = 4)
library(imager); library(gtools); library(tidyverse); library(ggplot2); library(dslabs)


"Very rarely in a data science project is data easily available as part of a package. We did quite a bit of work “behind the scenes” 
 to get the original raw data into the tidy tables you worked with. Much more typical is for the data to be in a file, a database, or
 extracted from a document, including web pages, tweets, or PDFs. In these cases, the first step is to import the data into R and, 
 when using the tidyverse, tidy the data. This initial step in the data analysis process usually involves several, often complicated, 
 steps to convert data from its raw form to the tidy form that greatly facilitates the rest of the analysis. We refer to this process 
 as data wrangling.

 Here we cover several common steps of the data wrangling process including tidying data, string processing, html parsing, working with
 dates and times, and text mining. Rarely are all these wrangling steps necessary in a single analysis, but data scientists will likely 
 face them all at some point. Some of the examples we use to demonstrate data wrangling techniques are based on the work we did to 
 convert raw data into the tidy datasets provided by the dslabs package and used in the book as examples."

path = system.file("extdata", package = "dslabs")
filename = file.path(path, "fertility-two-countries-example.csv")
wide_data = read_csv(filename)



# gather() - convert wide data into tidy data ------------------------------------------------------------------

head(wide_data) #> this representation of data is called wide data

# Here we want to reshape the wide_data dataset so that each row represents a fertility observation, which implies we need three columns 
# to store the year, country, and the observed value. In its current form, data from different years are in different columns with the 
# year values stored in the column names. Through the second and third argument we will tell gather the column names we want to assign 
# to the columns containing the current column names and observations, respectively.

# In this case a good choice for these two arguments would be year and fertility. Note that nowhere in the data does it tell us this
# is fertility data. Instead, we deciphered this from the file name. Through the fourth argument we specify the columns containing
# observed values; these are the columns that will be gathered. The default is to gather all columns so, in most cases, we have to 
# specify the columns. In our example we want columns 1960, 1961 up to 2015.

new_tidy_data = gather(wide_data, year, fertility, '1960':'2015')
head(new_tidy_data)

"Each year resulted in two rows since we have two countries and this column was not gathered. A somewhat quicker way to write this code 
 is to specify which column will not be gathered, rather than all the columns that will be gathered:"
new_tidy_data = gather(wide_data, year, fertility, -country)
head(new_tidy_data)


# The new_tidy_data object looks like the original tidy_data we defined this way with just one minor difference.
data("gapminder")
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany") & !is.na(fertility)) %>%
  select(country, year, fertility)
class(tidy_data$year) #> integer
class(new_tidy_data$year) #> character


"The gather function assumes that column names are characters. So we need a bit more wrangling before we are ready to make a plot. We 
 need to convert the year column to be numbers. The gather function includes the convert argument for this purpose:"
new_tidy_data = gather(wide_data, year, fertility, -country, convert = T)
# note that it's equivalent to using mutate(.., year = as.numeric(year))

new_tidy_data %>% 
  ggplot(aes(year, fertility, color = country)) + 
    geom_point()





# spread() - convert to wide data (opposite manipulation) -----------------

# We often use this as an intermediate step in tidying up data. The spread function is basically the inverse of gather. The first 
# argument is for the data. The second argument tells spread which variable will be used as the column names. The third argument 
# specifies which variable to use to fill out the cells

new_wide_data = new_tidy_data %>% spread(year, fertility)
head(new_wide_data)


"The following diagram can help remind you how these two functions work:"
plot(load.image("https://rafalab.github.io/dsbook/wrangling/img/gather-spread.png"), axes=F)

"wide/spread data"
# country 1960  1961  ... ... ...
#   fr      1     1    .   .   .
#   us      2     2    .   .   .
"tidy/gathered data"
# country  year   value
#    fr    1960     1
#    us    1960     2
#    fr    1961     1
#    us    1961     2
#     .     ..      .
#     .     ..      .
#     .     ..      .





# separate() - more complex raw data --------------------------------------

"The data wrangling shown above was simple compared to what is usually required. In our example spreadsheet files, we include an 
 illustration that is slightly more complicated. It contains two variables: life expectancy and fertility. However, the way it is 
 stored is not tidy and, as we will explain, not optimal."

path = system.file("extdata", package = "dslabs")
filename = file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_data = read_csv(filename)
head(raw_data)

# First, note that the data is in wide format. Second, notice that this table includes values for two variables, fertility and life 
# expectancy, with the column name encoding which column represents which variable. Encoding information in the column names is not 
# recommended but, unfortunately, it is quite common. We will put our wrangling skills to work to extract this information and store 
# it in a tidy fashion.

"We can start the data wrangling with the gather function, but we should no longer use the column name year for the new column since 
 it also contains the variable type. We will call it key, the default, for now:"
data = raw_data %>% gather(key, value, -country)
head(data)

# The result is not exactly what we refer to as tidy since each observation is associated with two, not one, rows. We want to have the 
# values from the two variables, fertility and life expectancy, in two separate columns. 

"The first challenge to achieve this is to separate the key column into the year and the variable type. Notice that the entries in this 
 column separate the year from the variable name with an underscore"

"Apart from the data, the separate function takes three arguments: the name of the column to be separated, the names to be used for the 
 new columns, and the character that separates the variables."
separate(data, key, c("year", "variable_name", "_"))

# The function does separate the values, but we run into a new problem. We receive the warning Too many values at 112 locations: and that
# the life_expectancy variable is truncated to life. This is because the _ is used twice : to separate life and expectancy, and year and 
# variable name

# We could add a third column to catch this and let the separate function know which column to fill in with missing values, NA, when 
# there is no third value. Here we tell it to fill the column on the right (second var name)
var_names <- c("year", "first_variable_name", "second_variable_name")
data %>% separate(key, var_names, "_", fill = "right")

"However, if we read the separate help file, we find that a better approach is to merge the last two variables when there is an extra 
 separation:"
data %>% separate(key, c("year", "variable_name"), extra = "merge")



# This achieves the separation we wanted. However, we are not done yet. We need to create a column for each variable. As we learned, the
# spread function can do this:
"Putting it all together"
data = raw_data %>% 
  gather(key, value, -country) %>%
  separate(key, c("year", "variable_name"), extra = "merge") %>%
  spread(variable_name, value)
head(data, 10)






# unite() - inverse of separate ---------------------------------------------------

"Suppose that we did not know about extra and used this command to separate:"
data = raw_data %>% 
  gather(key, value, -country) %>% 
  separate(key, var_names, fill = "right")
data

"We can achieve the same final result by uniting the second and third columns, then spreading the columns and renaming fertility_NA
 to fertility:"
data %>%
  unite(variable_name, first_variable_name, second_variable_name) %>% # merge columns
  spread(variable_name, value) %>% # spread to tidy
  rename(fertility = fertility_NA) # rename the column title




# Assessment -----------------------------------------------

"Q1 A collaborator sends you a file containing data for three years of average race finish times."
# age_group,2015,2016,2017
# 20,3:46,3:22,3:50
# 30,3:50,3:43,4:43
# 40,4:39,3:49,4:51
# 50,4:48,4:59,5:01
"Are these data considered “tidy” in R? Why or why not?"
No. These data are not considered “tidy” because the variable “year” is stored in the header.

"Q2 Below are four versions of the same dataset. Which one is in a tidy format?"
state      abb region  population total
Alabama     AL	South	  4779736	  135
Alaska      AK   West 	710231	  19
Arizona     AZ   West	  6392017   232
Arkansas    AR  South	  2915918	  93
California  CA   West   37253956  1257
Colorado    CO   West	  5029196	  65

"Q3 Your file called “times.csv” has age groups and average race finish times for three years of marathons."
# age_group,2015,2016,2017
# 20,3:46,3:22,3:50
# 30,3:50,3:43,4:43
# 40,4:39,3:49,4:51
# 50,4:48,4:59,5:01
"You read in the data file using the following command."
# d <- read_csv("times.csv")
"Which commands will help you “tidy” the data?"
tidy_data = d %>%
  gather(year, time, `2015`:`2017`)

"Q4 You have a dataset on U.S. contagious diseases, but it is in the following wide format:"
# state year population HepatitisA Mumps Polio Rubella
# Alabama 1990    4040587      86	   19    76    1
# Alabama 1991    4066003      39	   14    65    0
# Alabama 1992    4097169      35	   12    24    0
# Alabama 1993    4133242      40	   22    67    0
# Alabama 1994    4173361      72	   12    39    0
# Alabama 1995    4216645      75     2    38    0
"You want to transform this into a tidy dataset, with each row representing an observation of 
the incidence of each specific disease"
# state   year  population  disease  count
# Alabama 1990	4040587 HepatitisA	86
# Alabama 1991	4066003 HepatitisA	39
# Alabama 1992	4097169 HepatitisA	35
# Alabama 1993	4133242 HepatitisA	40
# Alabama 1994	4173361 HepatitisA	72
# Alabama 1995	4216645 HepatitisA	75
"Which of the following commands would achieve this transformation to tidy the data?"
dat_tidy <- dat_wide %>%
  gather(key = disease, value = count, HepatitisA:Rubella)

"Q5 You have successfully formatted marathon finish times into a tidy object called tidy_data"
# age_group year   time
# 20        2015   03:46
# 30        2015   03:50
# 40        2015   04:39
# 50        2015   04:48
# 20        2016   03:22
"Select the code that converts these data back to the wide format, where each year has a separate column."
tidy_data %>% spread(year, time)

# state   abb region    	var   people
# Alabama  AL  South population 4779736
# Alabama  AL  South  	total 	  135
# Alaska   AK   West population  710231
# Alaska   AK   West  	total  	   19
# Arizona  AZ   West population 6392017
# Arizona  AZ   West  	total 	  232
"Q6 You would like to transform it into a dataset where population and total are each their own column"
# state      abb region population total
# Alabama     AL  South	4779736   135
# Alaska      AK   West 	 710231    19
# Arizona     AZ   West	6392017   232
# Arkansas    AR  South	2915918    93
# California  CA   West  37253956  1257
# Colorado    CO   West	5029196	   65
dat_tidy <- dat %>% spread(key = var, value = people)

"Q7 A collaborator sends you a file containing data for two years of average race finish times, times.csv:"
# age_group,2015_time,2015_participants,2016_time,2016_participants
# 20,3:46,54,3:22,62
# 30,3:50,60,3:43,58
# 40,4:39,29,3:49,33
# 50,4:48,10,4:59,14
"You read in the data file:"
# d <- read_csv("times.csv")
"Which of the answers below best makes the data tidy?"
tidy_data <- d %>%
  gather(key = “key”, value = “value”, -age_group) %>%
  separate(col = key, into = c(“year”, “variable_name”), sep = “_”) %>% 
  spread(key = variable_name, value = value)
# This code gathers the column names 2015_time, 2015_participants, 2016_time, and 2016_participants into one 
# column called “key”, with the values for each stored in the column “value.” The key column is then separated 
# into two columns, “year” and “variable_name”. The two entries for “variable_name”, time and participants, 
# are then spread into their own columns.

"Q8 You are in the process of tidying some data on heights, hand length, and wingspan for basketball players in 
the draft. Currently, you have the following:"
# key               value
# allen_height      75
# allen_hand_length 8.25
# allen_wingspan	  79.25
# bamba_height      83.25
# bamba_hand_length 9.75
# bamba_wingspan    94
"Select all of the correct commands below that would turn this data into a “tidy” format with columns 
height, hand_length and wingspan."
tidy_data <- stats %>%
  separate(col = key, into = c("player", "variable_name"), sep = "_", extra = "merge") %>% 
  spread(key = variable_name, value = value)
# This is an efficient way to separate the key column into two new columns, “player” and “variable_name”, 
# while keeping the full variable names using the extra command.

library(dslabs); library(tidyverse)
"Q9 Examine the built-in dataset co2. This dataset comes with base R, not dslabs - just type co2 to access 
the dataset. Is co2 tidy? Why or why not?"
co2
co2 is not tidy: to be tidy we would have to wrangle it to have three columns (year, month and value), and then 
each co2 observation would have a row.

"Q10 Run the following command to define the co2_wide object:"
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
co2_wide
"Use the gather() function to make this dataset tidy. Call the column with the CO2 measurements co2 and call the 
month column month. Name the resulting object co2_tidy."
co2_tidy <- gather(co2_wide,month,co2,-year); co2_tidy

"Q11 Use co2_tidy to plot CO2 versus month with a different curve for each year:"
co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()
"What can be concluded from this plot?"
CO2 concentrations are highest around May and the yearly average increased from 1959 to 1997

"Q12 Load the admissions dataset from dslabs, which contains college admission information for men and women 
across six majors, and remove the applicants percentage column:"
data(admissions)
dat <- admissions %>% select(-applicants); dat
"Your goal is to get the data in the shape that has one row for each major, like this:"
# major  men   women
# A      62    82		
# B      63    68		
# C      37    34		
# D      33    35		
# E      28    24		
# F       6     7	
dat_tidy <- spread(dat, gender, admitted); dat_tidy

"Q13 Now use the admissions dataset to create the object tmp, which has columns major, gender, key and value:"
tmp <- gather(admissions, key, value, admitted:applicants); head(tmp)
"Combine the key and gender and create a new column called column_name to get a variable with the following 
values: admitted_men, admitted_women, applicants_men and applicants_women. Save the new data as tmp2."
tmp2 = tmp %>% unite(column_name, c(key, gender)); head(tmp2, 8)

"Q14 Which function can reshape tmp2 to a table with six rows and five columns named major, admitted_men, 
 admitted_women, applicants_men and applicants_women?"
spread()
