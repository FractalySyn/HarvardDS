rm(list = ls())
options(digits = 3)
library(imager); library(gtools); library(tidyverse); library(ggplot2); library(dslabs)
library(stringr); library(lubridate); library(pdftools)


# On September 20, 2017, Hurricane María made landfall on Puerto Rico. It was the worst 
# natural disaster on record in Puerto Rico and the deadliest Atlantic hurricane since 
# 2004. However, Puerto Rico's official death statistics only tallied 64 deaths caused 
# *directly by the hurricane (due to structural collapse, debris, floods and drownings),
# an undercount that slowed disaster recovery funding. The majority of the deaths resulted
# from infrastructure damage that made it difficult to access resources like clean food, 
# water, power, healthcare and communications in the months after the disaster, and 
# although these deaths were due to effects of the hurricane, they were not initially counted.

"In order to correct the misconception that few lives were lost in Hurricane María, 
statisticians analyzed how death rates in Puerto Rico changed after the hurricane 
and estimated the excess number of deaths likely caused by the storm. This analysis
suggested that the actual number of deaths in Puerto Rico was 2,975 (95% CI: 2,658-3,290) 
over the 4 months following the hurricane, much higher than the original count."

# We will use your new data wrangling skills to extract actual daily mortality data from 
# Puerto Rico and investigate whether the Hurricane María had an immediate effect on daily
# mortality compared to unaffected days in September 2015-2017.

"Open the pdf file"
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system("cmd.exe", input = paste("start", fn))

"Q1 Which of the following best describes this file?"
It is a report combining graphs and tables. Extracting the data seems possible.

# We are going to create a tidy dataset with each row representing one observation.
# The variables in this dataset will be year, month, day and deaths.
txt = pdf_text(fn); summary(txt)

"Q2 <describe txt"
A character string of length 12. Each entry represents the text in each page.
The mortality data is in there somewhere.

"Extract the ninth page of the PDF file from the object txt, then use the str_split()
 function from the stringr package so that you have each line in a different entry. The 
 new line character is \n. Call this string vector x."
x = txt[9] %>%
   str_split(pattern = "\n")
head(x)

"Q3 Describe x"
# I can see the table! But there is a bunch of other stuff we need to get rid of.
class(x); str(x)

"Q4 Define s to be the first entry of the x object"
s = x[[1]]
class(s)
length(s)

head(s)
# When inspecting the string we obtained above, we see a common problem: white space
# before and after the other characters. Trimming is a common first step in string 
# processing. These extra spaces will eventually make splitting the strings hard so
# we start by removing them.
"We learned about the command str_trim() that removes spaces at the start or end of 
 the strings. Use this function to trim s and assign the result to s again."
s = s %>% str_trim()
"Q5 After trimming, what single character is the last character of element 1 of s?"
s[1] # s


# We want to extract the numbers from the strings stored in s. However, there are a 
# lot of non-numeric characters that will get in the way. We can remove these, but 
# before doing this we want to preserve the string with the column header, which 
# includes the month abbreviation.
"Use the str_which() function to find the row with the header. Save this result to 
header_index. Hint: find the first string that matches the pattern "'2015'" using 
the str_which() function."
header_index = s %>%
   str_which(pattern = "2015")
"Q6 What is the value of header_index?"
header_index[1]

"We want to extract two objects from the header row: month will store the month
and header will store the column names."
# Save the content of the header row into an object called header, then use 
# str_split() to help define the two objects we need.
header = s[header_index[1]] %>% 
   str_split(pattern = "\\s+") %>%
   unlist()
"Q7 month and header values"
month = header[1]; month
header = header[-1]; header[3]

"Q8 Notice that towards the end of the page defined by s you see a "'Total'" row 
followed by rows with other summary statistics. Create an object called tail_index 
with the index of the "'Total'" entry."
tail_index = s %>%
   str_which(pattern = "Total")
tail_index

# Because our PDF page includes graphs with numbers, some of our rows have just one 
# number (from the y-axis of the plot). Use the str_count() function to create an 
# object n with the count of numbers in each row.
"Q9 How many rows have a single number in them?"
n = s %>%
   str_count(pattern = "\\d+")
sum(n == 1)


"We are now ready to remove entries from rows that we know we don't need. The entry 
header_index and everything before it should be removed. Entries for which n is 1 
should also be removed, and the entry tail_index and everything that comes after it
should be removed as well."
s = s[-c(1:header_index[1], which(n==1), tail_index:length(s))]
s

"Q10 How many entries remain in s?"
length(s)

# Now we are ready to remove all text that is not a digit or space. Do this using 
# regular expressions (regex) and the str_remove_all() function.
"In regex, using the ^ inside the square brackets [] means not, like the ! means not
in !=. To define the regex pattern to catch all non-numbers, you can type [^\\d]. 
But remember you also want to keep spaces."
s = s %>%
   str_remove_all("[^\\d\\s]")
s

# Use the str_split_fixed function to convert s into a data matrix with just the
# day and death count data:
s <- str_split_fixed(s, "\\s+", n = 6)[,1:5] # returns 6 elements max per row
s # spaces removed, 5 columns

"Now you are almost ready to finish. Add column names to the matrix: the first 
column should be day and the next columns should be the header. Convert all values 
to numeric. Also, add a column with the month. Call the resulting object tab."
mortality =  s %>%
   sapply(as.numeric) %>% 
   matrix(30, 5) %>%
   as_tibble() %>%
   setNames(c("day", header)) %>%
   mutate(month = rep(month, 30)) 
mortality

"Q12 What was the mean number of deaths per day in September 2015 and 2016?"
mean(mortality$'2015')
mean(mortality$'2016')
"Hurricane María hit Puerto Rico on September 20, 2017. What was the mean number
of deaths per day from September 1-19, 2017, before the hurricane hit?"
mean(mortality$'2017'[1:19])
"What was the mean number of deaths per day from September 20-30, 2017, after 
the hurricane hit?"
mean(mortality$'2017'[20:30])


# Finish it up by changing tab to a tidy format, starting from this code outline:
tab <- mortality %>% gather(year, deaths, -day) %>%
   mutate(deaths = as.numeric(deaths))
tab
"Q13" 
gather()

"Q14 Make a plot of deaths versus day with color to denote year. Exclude 2018 
since we have no data. Add a vertical line at day 20, the day that Hurricane 
María hit in 2017."
tab[1:120,] %>% filter(year != "2018") %>%
   ggplot(aes(day, deaths, color = year)) +
   geom_line() +
   geom_vline(xintercept = 20)
which(tab$deaths > 100)













