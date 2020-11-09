rm(list = ls())
options(digits = 4)
library(imager); library(gtools); library(tidyverse); library(ggplot2); library(dslabs); library(stringr)


# The date data type ------------------------------------------------------

"Although we can represent a date with a string, for example November 2, 2017, once we pick a reference day, referred to as the epoch,
 they can be converted to numbers by calculating the number of days since the epoch. Computer languages usually use January 1, 1970, 
 as the epoch. So, for example, January 2, 2017 is day 1, December 31, 1969 is day -1, and November 2, 2017, is day 17,204."

# November 2, 2017, you know what this means immediately. If I tell you it’s day 17,204, you will be quite confused. Similar problems
# arise with times and even more complications can appear due to time zones.
"For this reason, R defines a data type just for dates and times"

data("polls_us_election_2016")
class(polls_us_election_2016$startdate)

# Converting to numeric returns the # of days since the epoch
as.numeric(polls_us_election_2016$startdate)
as.Date("1970-01-01") %>% as.numeric #> 0

"Plotting functions, such as those in ggplot, are aware of the date format. This means that, for example, a scatterplot can use 
 the numeric representation to decide on the position of the point, but include the string in the labels:"
polls_us_election_2016 %>% filter(pollster == "Ipsos" & state =="U.S.") %>%
  ggplot(aes(startdate, rawpoll_trump)) +
  geom_line()




# The lubridate package ---------------------------------------------------

library(lubridate)

# We will take a random sample of dates to show some of the useful things one can do:
set.seed(2002)
dates = sample(polls_us_election_2016$startdate, 10) %>% sort()
dates

"The functions year, month and day extract those values:"
tibble(date = dates,
       month = month(dates),
       day = day(dates),
       year = year(dates))
"We can also extract the month labels:"
month(dates, label = T)


"Another useful set of functions are the parsers that convert strings into dates.
 The function ymd assumes the dates are in the format YYYY-MM-DD and tries to parse as well as possible."
# See below what it can recognize
x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
ymd(x)

"Different orders"
y = "09/01/02" #'> could be different dates
ymd(y)
mdy(y)
ydm(y)
myd(y)
dmy(y)
dym(y)

"The lubridate package is also useful for dealing with times. In R base, you can get the current time typing Sys.time(). 
 The lubridate package provides a slightly more advanced function, now, that permits you to define the time zone:"
Sys.time()
now()
now("GMT")
# You can see all the available time zones with OlsonNames() function.
OlsonNames()

"We can also extract hours, minutes, and seconds:"
now() %>% hour()
now() %>% minute()
now() %>% second()

# The package also includes a function to parse strings into times as well as parsers for time objects that include dates:
x <- c("12:34:56")
hms(x)
x <- "Nov/2/2012 12:34:56"
mdy_hms(x)

"The make_date function can be used to quickly create a date object. It takes three arguments: year, month, day, hour, minute,
 seconds, and time zone defaulting to the epoch values on UTC time. So create an date object representing, for example, June 19, 
 2020 we write:"
make_date(2020, 6, 19)
# To make a vector of January 1 for the 80s we write:
make_date(1980:1989)
#>  [1] "1980-01-01" "1981-01-01" "1982-01-01" "1983-01-01" "1984-01-01"
#>  [6] "1985-01-01" "1986-01-01" "1987-01-01" "1988-01-01" "1989-01-01"

"Another very useful function is the round_date. It can be used to round dates to nearest year, quarter, month, week, day, hour,
 minutes, or seconds. So if we want to group all the polls by week of the year we can do the following:"
polls_us_election_2016 %>%
  mutate(week = round_date(startdate, "week")) %>%
  group_by(week) %>%
  summarise(margin = mean(rawpoll_clinton - rawpoll_trump)) %>%
  qplot(week, margin, data = .)







# Assessment --------------------------------------------------------------

library(lubridate)

"Q! ISO 8601"
YYYY-MM-DD

"Q2 Which of the following commands could convert this string into the correct date format?"
# dates <- c("09-01-02", "01-12-07", "02-03-04")
It is impossible to know which format is correct without additional information. 
Numbers range between 1 and 12

"Load the brexit_polls data frame from dslabs:"
data("brexit_polls")
"How many polls had a start date (startdate) in April (month number 4)?"
brexit_polls$startdate[brexit_polls$startdate %>% lubridate::month() == 4] %>% length()
"Use the round_date() function on the enddate column with the argument unit='week'
How many polls ended the week of 2016-06-12?"
dates = brexit_polls$enddate %>% round_date("week")
which(dates ==  "2016-06-12") %>% length()

"Q4 Use the weekdays() function from lubridate to determine the weekday on which each poll ended (enddate)"
days =brexit_polls$enddate %>% 
  weekdays() %>%
  as.factor() %>%
  as_tibble() 
summary(days)

"Q5 movielen. This data frame contains a set of about 100,000 movie reviews. The timestamp 
 column contains the review date as the number of seconds since 1970-01-01 (epoch time).s"
data("movielens")
"Convert the timestamp column to dates using the lubridate as_datetime() function."
times = movielens$timestamp %>% as_datetime()
"Which year had the most movie reviews?"
times %>% year() %>% as.factor() %>% summary() %>% sort()
"Which hour of the day had the most movie reviews?"
times %>% hour() %>% as.factor() %>% summary() %>% sort()

library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)

gutenberg_metadata # metadata of 52000 books

"Q6 Use str_detect() to find the ID of the novel Pride and Prejudice.
How many different ID numbers are returned?"
noNA = gutenberg_metadata %>%
  filter(!is.na(title))
noNA$gutenberg_id[noNA$title %>% str_detect("Pride and Prejudice")]

"Q7 Notice that there are several versions of the book. The gutenberg_works() function filters this 
 table to remove replicates and include only English language works. Use this function to find the 
 ID for Pride and Prejudice."
gutenberg_works(title == "Pride and Prejudice")

"Use the gutenberg_download() function to download the text for Pride and Prejudice. Use the tidytext 
 package to create a tidy table with all the words in the text. Save this object as words."
pnp = gutenberg_download(gutenberg_id = 1342)
words = pnp %>%
  unnest_tokens(word, text)
length(words$word)

"Q9 Remove stop words from the words object. Recall that stop words are defined in the stop_words 
 data frame from the tidytext package."
stop_words
words %>% 
  filter(!(word %in% stop_words$word)) 

"Q10 After removing stop words, detect and then filter out any token that contains a digit from words."
words = words %>% 
  filter(!(word %in% stop_words$word))
words = words[!str_detect(words$word, "\\d{1}"),] 
words

"Q11 How many words appear more than 100 times in the book?"
as.factor(words$word) %>% summary() %>% as_tibble() %>% filter(between(value, 100, 1000))
"What is the most common word in the book?"
as.factor(words$word) %>% summary() %>% sort()
"How many times does that most common word appear?"
597

library(textdata)
afinn <- get_sentiments("afinn")
afinn

"Use this afinn lexicon to assign sentiment values to words. Keep only words that are present in both words 
 and the afinn lexicon. Save this data frame as afinn_sentiments."
words = as_tibble(words)
afinn_sentiments = left_join(words, afinn, by = "word")
afinn_sentiments = afinn_sentiments[!is.na(afinn_sentiments$value),]; afinn_sentiments
"What proportion of words in afinn_sentiments have a positive value?"
mean(afinn_sentiments$value > 0) 
"How many elements of afinn_sentiments have a value of 4?"
sum(afinn_sentiments$value[indexes] == 4)




