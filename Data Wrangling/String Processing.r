rm(list = ls())
options(digits = 4)
library(imager); library(gtools); library(tidyverse); library(ggplot2); library(dslabs); library(stringr)

"One of the most common data wrangling challenges involves extracting numeric data contained in character strings and converting them into 
the numeric representations required to make plots, compute summaries, or fit models in R. Also common is processing unorganized text into
meaningful variable names or categorical variables. Many of the string processing challenges a data scientist faces are unique and often 
unexpected. "
# It is therefore ambitious to write a comprehensive section on this topic. Here we use a series of case studies that help us demonstrate 
# how string processing is a necessary step for many data wrangling challenges. Specifically, we describe the process of converting the 
# not yet shown original raw data from which we extracted the murders, heights, and research_funding_rates example into the data frames 
# we have studied in this book.

"By going over these case studies, we will cover some of the most common tasks in string processing including extracting numbers from
strings, removing unwanted characters from text, finding and replacing characters, extracting specific parts of strings, converting
free form text to more uniform formats, and splitting strings into multiple values."





# The stringr package -----------------------------------------------------

# Base R includes functions to perform all these tasks. However, they don’t follow a unifying convention, which makes them a bit hard to
# memorize and use. The stringr package basically repackages this functionality, but uses a more consistent approach of naming functions
# and ordering their arguments. For example, in stringr, all the string processing functions start with str_. This means that if you type
# str_ and hit tab, R will auto-complete and show all the available functions. As a result, we don’t necessarily have to memorize all the
# function names. 
"Another advantage is that in the functions in this package the string being processed is always the first argument, which means we can 
 more easily use the pipe. Therefore, we will start by describing how to use the functions in the stringr package."
library(stringr)

# In general, string processing tasks can be divided into detecting, locating, extracting, or replacing patterns in strings. We will 
# see several examples. The table below includes the functions available to you in the stringr package. We split them by task. We also
# include the R-base equivalent when available.
"All these functions take a character vector as first argument. Also, for each function, operations are vectorized: the operation gets
 applied to each string in the vector."
"https://github.com/FractalySyn/harvardXdatascience/raw/master/string-processing/one.png" %>% load.image() %>% plot(axes = F)
"https://github.com/FractalySyn/harvardXdatascience/raw/master/string-processing/two.png" %>% load.image() %>% plot(axes = F)
"https://github.com/FractalySyn/harvardXdatascience/raw/master/string-processing/three.png" %>% load.image() %>% plot(axes = F)





# Case study 1: US murders data -------------------------------------------

library(rvest)
url <- paste0("https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167")
murders_raw = url %>%
  read_html() %>%
  html_node("table") %>%
  html_table() %>%
  setNames(c("state", "population", "total", "murder_rate"))
head(murders_raw)
head(murders)

murders_raw$population # is a characters vector
"The usual coercion doesn't work here because of the commas ,"
as.numeric(murders_raw$population)

"The string processing we want to do here is remove the pattern ',' from the strings in murders_raw$population and then coerce to numbers.
 We can use the str_detect function to see that two of the three columns have commas in the entries:"
detect_commas = function(vec) 
{
  any(str_detect(vec, ","))
}
murders_raw %>% summarise_all(detect_commas)

"We can then use the str_replace_all function to remove them:"
test1 = str_replace_all(murders_raw$population, pattern = ",", replacement = "") %>% as.numeric()
test1

"We can then use mutate_all to apply this operation to each column, since it won’t affect the columns without commas"
# It turns out that this operation is so common that readr includes the function parse_number specifically meant to remove 
# non-numeric characters before coercing:
test2 = parse_number(murders_raw$population)
test2; identical(test1, test2)

# So we can obtain our desired table using:
murders_new = murders_raw %>% mutate_at(2:3, parse_number) # mutate the parse_number processing to both concerned columns
                                                           # and converts to numeric
head(murders_new)
class(murders_new$population)




# Assessment --------------------------------------------------------------

"Q1 Which of the following is NOT an application of string parsing?"
Formatting numbers and characters so they can easily be displayed in deliverables 
like papers and presentations.

"Q2 Which of the following commands would not give you an error in R?"
cat(" LeBron James is 6’8\" ")

"Q3 Which of the following are advantages of the stringr package over string processing functions 
 in base R? Select all that apply."
Functions in stringr all start with “str_”, which makes them easy to look up using autocomplete.
Stringr functions work better with pipes.
The order of arguments is more consistent in stringr functions than in base R.

"Q4 You have a data frame of monthly sales and profits in R:"
# Month     Sales     Profit 
# <chr>     <chr>     <chr>  
# January   $128,568  $16,234
# February  $109,523  $12,876
# March     $115,468  $17,920
# April     $122,274  $15,825
# May       $117,921  $15,437
"Which of the following commands could convert the sales and profits columns to numeric? 
 Select all that apply."
dat %>% mutate_at(2:3, parse_number)
dat %>% mutate_at(2:3, funs(str_replace_all(., c("\\$|,"), ""))) %>% 
  mutate_at(2:3, as.numeric)
# we cannot use mutate_all(parse_number) -> parse number already uses mutate_all to remove commas and
# other non-numeric characters, but parse_number also apply as.numeric so chr vectors will return errors




# Case study 2: self-reported heights -------------------------------------

data("reported_heights") # the raw data

# These heights were obtained using a web form in which students were asked to enter their heights. They could enter anything, but the
# instructions asked for height in inches, a number. We compiled 1,095 submissions, but unfortunately the column vector with the reported
# heights had several non-numeric entries and as a result became a character vector:
class(reported_heights$height)

"If we try to parse it into numbers, we get a warning:"
x = as.numeric(reported_heights$height); head(x)
# Although most values appear to be height in inches as requested we end up with many NAs
mean(is.na(x))*100 #> >7%

"We can see some of the entries that are not successfully converted by using filter to keep only the entries resulting in NAs:"
reported_heights %>% 
  mutate(numeric_height = as.numeric(height)) %>%
  filter(is.na(numeric_height)) %>%
  head(10)

"We immediately see what is happening. Some of the students did not report their heights in inches as requested. We could discard these 
 data and continue. However, many of the entries follow patterns that, in principle, we can easily convert to inches. For example, in
 the output above, we see various cases that use the format x'y'' with x and y representing feet and inches, respectively. Each one of 
 these cases can be read and converted to inches by a human, for example 5'4'' is 5*12 + 4 = 64. So we could fix all the problematic 
 entries by hand. However, humans are prone to making mistakes, so an automated approach is preferable. Also, because we plan on 
 continuing to collect data, it will be convenient to write code that automatically does this."

# A first step in this type of task is to survey the problematic entries and try to define specific patterns followed by a large groups of
# entries. The larger these groups, the more entries we can fix with a single programmatic approach. We want to find patterns that can be
# accurately described with a rule, such as “a digit, followed by a feet symbol, followed by one or two digits, followed by an inches 
# symbol”.

"To look for such patterns, it helps to remove the entries that are consistent with being in inches and to view only the problematic 
 entries. We thus write a function to automatically do this. We keep entries that either result in NAs when applying as.numeric or are 
 outside a range of plausible heights. We permit a range that covers about 99.9999% of the adult population."
not_inches = function(x, smallest = 50, tallest = 84)
{
  inches = as.numeric(x) %>% suppressWarnings()
  indexes = is.na(inches) | inches < smallest | inches > tallest
  indexes
}
problems = reported_heights %>%
  filter(not_inches(height)) %>%
  pull(height)
head(problems, 20)

# We see three main patterns
# A pattern of the form x'y or x' y'' or x'y" with x and y representing feet and inches
# A pattern of the form x.y or x,y with x feet and y inches
# Entries that were reported in centimeters rather than inches

# Once we see these large groups following specific patterns, we can develop a plan of attack. Remember that there is rarely just one way 
# to perform these tasks. Here we pick one that helps us teach several useful techniques. But surely there is a more efficient way of 
# performing the task.

"Plan of attack: we will convert entries fitting the first two patterns into a standardized one. We will then leverage the standardization
 to extract the feet and inches and convert to inches. We will then define a procedure for identifying entries that are in centimeters and
 convert them to inches. After applying these steps, we will then check again to see what entries were not fixed and see if we can tweak 
 our approach to be more comprehensive."





# How to escape when defining strings -------------------------------------

# To achieve our goal, we will use a technique that enables us to accurately detect patterns and extract the parts we want: regular
# expressions (regex). But first, we quickly describe how to escape the function of certain characters so that they can be included 
# in strings.

"Strings can be defined with simple and double quotes"
"Hi" == 'Hi'

# Now, what happens if the string we want to define includes double quotes? For example, if we want to write 10 inches like 
# this 10"? In this case you can’t use:
s = "10""    # because the double quote opens another string until closed"
# To avoid this we can use sngle quotes
s = '10"'; s
# If we print out s we see that the double quotes are escaped with the backslash \

# Solution
s = '10"'; cat(s)
s = "10\""; cat(s)
cat("5'")

# but what if we want to write them together to represent 5 feet and 10 inches like this 5'10"? 
# neither the single nor double quotes will work.
"In this situation, we need to escape the function of the quotes with the backslash \. You can escape either character like this:"
cat('5\'10"'); cat("5'10\"") # the backslash avoids the following escape caused by a quote closing






# Regular expressions (regex) basics ---------------------------------------------

"Learn more about regex :"
https://www.regular-expressions.info/tutorial.html 
https://rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf
https://openclassrooms.com/fr/courses/918836-concevez-votre-site-web-avec-php-et-mysql/918834-memento-des-expressions-regulieres

"A regular expression (regex) is a way to describe specific patterns of characters of text. They can be used to determine if a given 
 string matches the pattern. The patterns supplied to the stringr functions can be a regex rather than a standard string."

# Throughout this section you will see that we create strings to test out our regex. To do this, we define patterns that we know should
# match and also patterns that we know should not. We will call them yes and no, respectively. This permits us to check for the two types
# of errors: failing to match and incorrectly matching.



"Technically any string is a regex, perhaps the simplest example is a single character. So the comma , used in the next code example is
 a simple example of searching with regex."
pattern = ","
str_detect(murders_raw$total, pattern) # returns TRUE for entries having commas
# If we want to suppress entries including cm for centimeters we spot them as follows
str_detect(reported_heights$height, "cm")

"Now let’s consider a slightly more complicated example. Which of the following strings contain the pattern cm or inches?"
yes = c("180 cm", "70 inches"); no = c("180", "70''"); s = c(yes, no)
str_detect(s, "cm") | str_detect(s, "inches")

 # However, we don’t need to do this. The main feature that distinguishes the regex language from plain strings is that we can use 
 # special characters. These are characters with a meaning. We start by introducing | which means or. So if we want to know if either
 # cm or inches appears in the strings, we can use the regex cm|inches:
str_detect(s, "cm|inches")

"Another special character that will be useful for identifying feet and inches values is \d which means any digit: 0, 1, 2, 3, 4, 5,
 6, 7, 8, 9. The backslash is used to distinguish it from the character d. In R, we have to escape the backslash \ so we actually have
 to use \\d to represent digits. Here is an example:"
yes = c("5", "6", "5'10", "5 feet", "4'11"); no = c("", ".", "Five", "six"); s = c(yes, no)
pattern = "\\d"
str_detect(s, pattern)

# We take this opportunity to introduce the str_view function, which is helpful for troubleshooting 
# as it shows us the first match for each string:
str_view(s, pattern)
# and str_view_all shows us all the matches, so 3'2 has two matches and 5'10 has three.
str_view_all(s, pattern)




# Regex character Classes, Anchors and Quantifiers -------------------------------------------------

"Character classes are used to define a series of characters that can be matched. We define character classes with square brackets []. 
 So, for example, if we want the pattern to match only if we have a 5 or a 6, we use the regex [56]:"
str_view(s, "[56]") 
identical(str_detect(s, "[56]"),str_detect(s, "5|6"))

# Suppose we want to match values between 4 and 7. A common way to define character classes is with ranges. So, for example, [0-9] is 
# equivalent to \\d. The pattern we want is therefore [4-7].
yes = as.character(4:7); no = as.character(1:3); s = c(yes, no)
str_detect(s, "[4-7]")
identical(str_detect(s, "[0-9]"), str_detect(s, "\\d"))

# However, it is important to know that in regex everything is a character; there are no numbers. So 4 is the character 4 not the number 
# four. Notice, for example, that [1-20] does not mean 1 through 20, it means the characters 1 through 2 or the character 0. So [1-20] 
# simply means the character class composed of 0, 1, and 2.
str_view_all(s, "[1-20]")

"Keep in mind that characters do have an order and the digits do follow the numeric order. So 0 comes before 1 which comes before 2 and 
 so on. For the same reason, we can define lower case letters as [a-z], upper case letters as [A-Z], and [a-zA-z] as both."



"What if we want a match when we have exactly 1 digit? This will be useful in our case study since feet are never more than 1 digit so
 a restriction will help us. One way to do this with regex is by using anchors, which let us define patterns that must start or end at
 a specific place. The two most common anchors are ^ and $ which represent the beginning and end of a string, respectively. So the 
 pattern ^\\d$ is read as “start of the string followed by one digit followed by end of string”."
pattern = "^\\d$" # means "+number+" -> one digit !
yes = c("1", "5", "9"); no = c("12", "123", " 1", "a4", "b"); s = c(yes, no)
str_view_all(s, pattern)
# The 1 does not match because it does not start with the digit but rather with a space, which is actually not easy to see. 



"For the inches part, we can have one or two digits. This can be specified in regex with quantifiers. This is done by following the 
 pattern with curly brackets containing the number of times the previous entry can be repeated. We use an example to illustrate. The 
 pattern for one or two digits is:"
yes = c("1", "5", "9", "12"); no = c("123", "a4", "b")
pattern = "^\\d{1,2}$" # 1 or 2 digits
str_view(c(yes, no), pattern)
# So to look for our feet and inches pattern, we can add the symbols for feet ' and inches " after the digits.

"With what we have learned, we can now construct an example for the pattern x'y\" with x feet and y inches."
pattern = "^[4-7]'\\d{1,2}\"$" # 4-7 feet + ' + any number of 2 digits + "
yes <- c("5'7\"", "6'2\"",  "5'12\""); no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
str_view(c(yes, no), pattern)
# For now, we are permitting the inches to be 12 or larger. We will add a restriction later as the regex for this is a bit more
# complex than we are ready to show.



# Another problem we have are spaces. For example, our pattern does not match 5' 4" because there is a space between ' and 4 
# which our pattern does not permit. Spaces are characters and R does not ignore them:
identical("Hi", "Hi ") #> FALSE

"In regex, \s represents white space. To find patterns like 5' 4, we can change our pattern to:"
pattern_2 = "^[4-7]'\\s\\d{1,2}\"$"
problems
str_subset(problems, pattern_2)

"However, this will not match the patterns with no space. So do we need more than one regex pattern? 
 It turns out we can use a quantifier for this as well."

# We want the pattern to permit spaces but not require them. Even if there are several spaces, like in this example 5' 4, 
# we still want it to match. There is a quantifier for exactly this purpose. In regex, the character * means zero or more 
# instances of the previous character. Here is an example:
yes <- c("AB", "A1B", "A11B", "A111B", "A1111B"); no <- c("A2B", "A21B")
str_view(c(yes, no), "A1*B")

"We can then improve our pattern by adding the * after the space character \s."
pattern_3 = "^[4-7]'\\s*\\d{1,2}\"$"

# There are two other similar quantifiers. For none or once, we can use ?, and for one or more, we can use +. 
" * -> none or any number of the character
  + -> one or more
  ? -> none or one "
# You can see how they differ with this example:
str_view(c(yes, no), "A1+B") # requires at least one
str_view(c(yes, no), "A1?B") # requires less than 2



"To specify patterns that we do not want to detect, we can use the ^ symbol but only inside square brackets. Remember that outside 
 the square bracket ^ means the start of the string. So, for example, if we want to detect digits that are preceded by anything 
 except a letter we can do the following:"
pattern = "[^a-zA-Z]\\d" # anything execpt a letter before a digit
yes <- c(".3", "+2", "-0","*4"); no <- c("A3", "B2", "C0", "E4")
str_view(c(yes, no), pattern)

"Another way to generate a pattern that searches for everything except is to use the upper case of the special character. 
 For example \\D means anything other than a digit, \\S means anything except a space, and so on."
str_view(c(yes, no), "\\D")
str_view(c(yes, no), "\\S")
str_view(c(yes, no), "\\W") # w = [a-zA-Z0-9_]



"Groups are a powerful aspect of regex that permits the extraction of values."
# We want to change heights written like 5.6 to 5'6. To avoid changing patterns such as 70.2, we will require that 
# the first digit be between 4 and 7 [4-7] and that the second be none or more digits \\d*
pattern_without_groups = "^[4-7],\\d*$"
pattern_with_groups = "^([4-7]),(\\d*)$"

"We encapsulate the part of the pattern that matches the parts we want to keep for later use. Adding groups does not affect 
 the detection, since it only signals that we want to save what is captured by the groups."
yes <- c("5,9", "5,11", "6,", "6,1"); no <- c("5'9", ",", "2,8", "6.1.1"); s <- c(yes, no)
str_detect(s, pattern_with_groups) %>% identical(str_detect(s, pattern_without_groups))
str_detect(s, pattern_with_groups)

"Once we define groups, we can use the function str_match to extract the values these groups define:"
str_match(s, pattern_with_groups) #> first column returns detected patterns
                                  #> next columns show groups

# Now we can understand the difference between the functions str_extract and str_match: str_extract extracts only strings that 
# match a pattern, not the values defined by groups:
str_extract(s, pattern_with_groups)







# Search and replace with regex -------------------------------------------

# Earlier we defined the object problems containing the strings that do not appear to be in inches. 
# We can see that not too many of our problematic strings match the pattern:
pattern <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems, pattern))
# To see why this is, we show some examples that expose why we don’t have more matches:
problems[c(2, 10, 11, 12, 15)] %>% str_view(pattern)

# An initial problem we see immediately is that some students wrote out the words “feet” and “inches”. 
# We can see the entries that did this with the str_subset function:
str_subset(problems, "inches")
# We also see that some entries used two single quotes '' instead of a double quote "
str_subset(problems, "''")

# To correct this, we can replace the different ways of representing inches and feet with a uniform symbol. We will use 
# ' for feet, whereas for inches we will simply not use a symbol since some entries were of the form x'y. Now, if we no 
# longer use the inches symbol, we have to change our pattern accordingly:
pattern = "^[4-7]'\\d{1,2}$"
problems %>%
  str_replace("feet|ft|foot", "'") %>%
  str_replace("inches|in|''|\"", "") %>%
  str_detect(pattern) %>% sum()

# For now, we improve our pattern by adding \\s* in front of and after the feet symbol ' to permit space between 
# the feet symbol and the numbers. Now we match a few more entries:
pattern = "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>%
  str_replace("feet|ft|foot", "'") %>%
  str_replace("inches|in|''|\"", "") %>%
  str_detect(pattern) %>% sum()

"We might be tempted to avoid doing this by removing all the spaces with str_replace_all. However, when doing such an 
operation we need to make sure that it does not have unintended effects. In our reported heights examples, this will 
be a problem because some entries are of the form x y with space separating the feet from the inches. If we remove all 
spaces, we will incorrectly turn x y into xy which implies that a 6 1 would become 61 inches instead of 73 inches."

# The second large type of problematic entries were of the form x.y, x,y and x y. We want to change all these to our common
# format x'y. But we can’t just do a search and replace because we would change values such as 70.5 into 70'5. Our strategy
# will therefore be to search for a very specific pattern that assures us feet and inches are being provided and then, for
# those that match, replace appropriately.

"Another powerful aspect of groups is that you can refer to the extracted values in a regex when searching and replacing."
"The regex special character for the i-th group is \\i. So \\1 is the value extracted from the first group, \\2 the value 
 from the second and so on"
# As a simple example, note that the following code will replace a comma with period, but only if it is between two digits:
pattern_with_groups =  "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1"); no <- c("5'9", ",", "2,8", "6.1.1"); s <- c(yes, no)
str_replace(s, pattern_with_groups, "\\1'\\2") # replaces commas without touching to groups


# We are now ready to define a pattern that helps us convert all the x.y, x,y and x y to our preferred format.
# We need to adapt pattern_with_groups to be a bit more flexible and capture all the cases.
pattern_with_groups = "^([4-7])\\s*[,\\.\\s+]\\s*(\\d{1,2})$" # [,\\.\\s+] = either , . or at least one space
matches = str_subset(problems, pattern_with_groups); matches
matches %>% str_replace(pattern_with_groups, "\\1'\\2") 







# Testing and improving ---------------------------------------------------

"Developing the right regex on the first try is often difficult. Trial and error is a common approach to finding the regex pattern that 
 satisfies all desired conditions."

# Let’s write a function that captures all the entries that can’t be converted into numbers remembering that some are in centimeters
not_inches_or_cm = function(x, smallest = 50, tallest = 84)
{
  inches = x %>% as.numeric() %>% suppressWarnings()
  indexes = !is.na(inches) & 
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !indexes # returns values indexes in inches or in cm that are out of normal heights
}

problems = reported_heights %>% 
  filter(not_inches_or_cm(height)) %>%
  pull(height)
length(problems) #> 200 problems detected

"Let’s see what proportion of these fit our pattern after the processing steps we developed above:"
converted = problems %>%
  str_replace("feet|foot|ft", "'") %>% # replace feet by '
  str_replace("inches|in|''|\"", "") %>% # replace inches, in, " and '' by nothing
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") # change the format

desired_pattern = "^([4-7])\\s*'\\s*\\d{1,2}$"
index = str_detect(converted, desired_pattern)
mean(index)
#> 61.5% of problems are detected by our regex pattern

"See remaining problems"
converted[!index]
# Four clear patterns arise:
# -> students measuring exactly 5 or 6 feet -> 5' or 6'
# -> or they only entered the number
# -> Some of the inches were entered with decimal points. For example 5'7.5''. Our pattern only looks for two digits.
# -> Some entries have spaces at the end, for example 5 ' 9 .
"Although not as common, we also see the following problems:"
# -> Some entries are in meters and some of these use European decimals: 1.6, 1,70.
# -> Two students added cm
# -> A student spelled out the numbers: Five foot eight inches

"For case 1, if we add a '0 after the first digit, for example, convert all 6 to 6'0, then our previously defined pattern will match. 
This can be done using groups:"
yes <- c("5", "6", "5"); no <- c("5'", "5''", "5'4"); s <- c(yes, no)
str_replace(s, "^([4-7])$", "\\1'0")
# We can change the second problem as well by adding the possibility to have a ' after the first digit
str_replace(s, "^([4-7])'*$", "\\1'0")

# Case 3 : Add the possibility of a . decimal
current_pattern = "^[4-7]\\s*'\\s*\\d{1,2}$"
desired_pattern = "^[4-7]\\s*'\\s*(\\d+\\.?\\d*)$"

# Meters with commas -> replace by dot
yes <- c("1,7", "1, 8", "2, " ); no <- c("5,8", "5,3,2", "1.7"); s <- c(yes, no)
str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1.\\2")

# Remove spaces at the end
str_trim("abc def   ")

"Often we want to match a word regardless of case. One approach to doing this is to first change everything to lower case and then 
 proceeding ignoring case"
str_to_lower("Five feet Eight inches")

# convert words to numbers
library(english)
words_to_numbers <- function(s){
  s <- str_to_lower(s)
  for(i in 0:11)
    s <- str_replace_all(s, words(i), as.character(i))
  s
}
words_to_numbers("Five feet Eight inches")

# Back to Case Study : reported heights -----------------------------------

convert_format = function(s)
{
  s %>% 
    words_to_numbers() %>%
    str_replace("feet|foot|ft", "'") %>%
    str_replace_all("inches|in|''|\"|cm|and", "") %>%
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>%
    str_replace("^([56])'?$", "\\1'0") %>%
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>%
    str_trim()
}

converted = problems %>% convert_format()
remaining_problems = converted[not_inches_or_cm(converted)]
desired_pattern = "^[4-7]\\s*'\\s*(\\d+\\.?\\d*)$"
index = str_detect(remaining_problems, pattern)
remaining_problems[!index]
# Apart from the cases reported as meters, which we will fix below, remaining issues all seem to be cases that are impossible to fix.







# String Splitting --------------------------------------------------------

filename = system.file("extdata/murders.csv", package = "dslabs")
lines = readLines(filename); head(lines)

"With this kind of data where columns are not separated, we have to split"
x = str_split(lines, pattern = ",")
# This returns a list of character vectors
x[[3]] %>% class() 

# Note that the first entry has the column names, so we can separate that out:
col_names = x[[1]]
# And remove this line from x
x = x[-1]

"To convert our list into a data frame, we can use a shortcut provided by the map functions in the purrr package. The map 
 function applies the same function to each element in a list. So if we want to extract the first entry of each element in
 x, we can write:"
as.data.frame(x) # doesn't work
library(purrr)
map(x, function(y) y[1]) %>% head() # extract the first character from each list element
"However, because this is such a common task, purrr provides a shortcut. If the second argument receives an integer instead 
 of a function, it assumes we want that entry. So the code above can be written more efficiently like this:"
map(x, 1) %>% head()

"To force map to return a character vector instead of a list, we can use map_chr. Similarly, map_int returns integers"
murders = tibble(map_chr(x, 1),
                 map_chr(x, 2),
                 map_chr(x, 3),
                 map_chr(x, 4),
                 map_chr(x, 5)) %>%
  mutate_all(parse_guess) %>% # guesses the class of columns -> converts characters to numerics
  setNames(col_names)
murders

# If you learn more about the purrr package, you will learn that you perform the above with the following, more efficient, code:
murders2 = x %>% transpose() %>%
  map(~ parse_guess(unlist(.))) %>%
  setNames(col_names) %>%
  as_tibble()
murders2

# It turns out that we can avoid all the work shown above after the call to str_split. Specifically, if we know that the data we are
# extracting can be represented as a table, we can use the argument simplify=TRUE and str_split returns a matrix instead of a list
y = str_split(lines, pattern = ",", simplify = T) 
colnam = y[1,]; y = y[-1,]
colnames(y) = colnam
y %>% as_tibble() %>%
  mutate_all(parse_guess) 






#  Case study 3: extracting tables from a PDF -----------------------------

# One of the datasets provided in dslabs shows scientific funding rates by gender in the Netherlands:
data("research_funding_rates")
research_funding_rates %>% select("discipline", "success_rates_men", "success_rates_women")

# The data comes from a paper published in a widely read scientific journal. However, the data is not provided in a spreadsheet; 
# it is in a table in a PDF document. Here is a screenshot of the table:
plot(load.image("https://rafalab.github.io/dsbook/wrangling/img/pnas-table-s1.png"))

"We could extract the numbers by hand, but this could lead to human error. Instead, we can try to wrangle the data using R. 
 We start by downloading the pdf document, then importing into R:"
library(pdftools)
temp = tempfile()
url = "https://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp)
txt = pdf_text(temp)
file.remove(temp)

txt
# If we examine the object text, we notice that it is a character vector with an entry for each 2 pages. So we keep the page we want:
raw_data = txt[2]; raw_data

# Examining the object raw_data we see that it is a long string and each line on the page, including the table rows, are 
# separated by the symbol for newline: \n. We therefore can create a list with the lines of the text as elements as follows:
tab = str_split(raw_data, "\n")
tab

# By examining tab we see that the information for the column names is the third and fourth entries:
names1 = tab[[1]][3]
names2 = tab[[1]][4]

names1
"We want to remove the leading space and anything following the comma. We use regex for the latter. Then we can obtain the elements
 by splitting strings separated by space. We want to split only when there are 2 or more spaces to avoid splitting Success rates. 
 So we use the regex \\s{2,}"
names1 = names1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>% # dots can be anything, so it removes the n and the %
  str_split("\\s{2,}", simplify = T) # at least 2 spaces # simplify returns a matrix instad of a vector
names1

names2
"Here we want to trim the leading space and then split by space as we did for the first line:"
names2 = names2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = T)
names2

"We can then join these to generate one name for each column:"
# We need to repeat three times the names of the first vector names1, remove Discipline, and then join them
temp_names = str_c(rep(names1, each = 3), names2[-1], sep = "_"); temp_names
# Now we add Discipline again and add a _ instead of space for the success rate columns
names = c(names2[1], temp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
names

# Now we are ready to get the actual data. By examining the tab object, we notice that the information is in 
# lines 6 through 14. We can use str_split again to achieve our goal:
new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% as_tibble()
# We don't have data anymore unfortunately. Results would be as int the dslabs data : 
#> # A tibble: 9 x 10
#>   discipline applications_to… applications_men applications_wo…
#>   <chr>                 <dbl>            <dbl>            <dbl>
#> 1 Chemical …              122               83               39
#> 2 Physical …              174              135               39
#> 3 Physics                  76               67                9
#> 4 Humanities              396              230              166
#> 5 Technical…              251              189               62
#> # … with 4 more rows, and 6 more variables: awards_total <dbl>,
#> #   awards_men <dbl>, awards_women <dbl>, success_rates_total <dbl>,
#> #   success_rates_men <dbl>, success_rates_women <dbl>






# Recoding ----------------------------------------------------------------

"Another common operation involving strings is recoding the names of categorical variables. Let’s say you have really long names for
 your levels and you will be displaying them in plots, you might want to use shorter versions of these names."
data("gapminder")
gapminder %>% 
  filter(region == "Caribbean") %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

# The plot is what we want, but much of the space is wasted to accommodate some of the long country names. We have four countries 
#  with names longer than 12 characters. These names appear once for each year in the Gapminder dataset
"We cannot simply change column names beacause it is tidy data for multiple years, so names appear for each year"
gapminder %>% filter(region=="Caribbean") %>%
  mutate(country = recode(country, 
                          `Antigua and Barbuda` = "Barbuda",
                          `Dominican Republic` = "DR",
                          `St. Vincent and the Grenadines` = "St. Vincent",
                          `Trinidad and Tobago` = "Trinidad")) %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

# There are other similar functions in other R packages, such as recode_factor and fct_recoder in the forcats package.






# Assessment Part 2 --------------------------------------------------------------

not_inches <- function(x, smallest = 50, tallest = 84) {
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest 
  ind
}

"Q1 we use the function not_inches to identify heights that were incorrectly entered
 what TWO types of values are identified as not being correctly formatted in inches?"
Values that result in NA’s when converted to numeric
Values less than 50 inches or greater than 84 inches

"Q2 Which of the following arguments, when passed to the function not_inches(), would return the vector FALSE?"
c(175) %>% not_inches()
c("5'8\"") %>% not_inches()
c(70) %>% not_inches()
c(85) %>% not_inches()

"Q3 Our function not_inches() returns the object ind. Which answer correctly describes ind?"
ind is a logical vector of TRUE and FALSE, equal in length to the vector x (in the arguments list). 
TRUE indicates that a height entry is incorrectly formatted.

"Q4 Given the following code"
# > s
# [1] "70"       "5 ft"     "4'11"     ""         "."        "Six feet"
"What pattern vector yields the following result?"
# str_view_all(s, pattern)
# 70
# 5 ft
# 4’11
# .
# Six feet
s = c("70", "5 ft", "4'11", "", ".", "Six feet")
str_view_all(s, "\\d|ft")

"Q5 You enter the following set of commands into your R console. What is your printed result?"
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]"
str_detect(animals, pattern)
TRUE  TRUE  TRUE FALSE # because MONKEY is [A-Z]

"Q6"
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[A-Z]$"
str_detect(animals, pattern) # result will be the opposite

"Q7"
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]{4,5}"
str_detect(animals, pattern)
F T T F # need 4 or 5 lower cases

"Q8 Which TWO “pattern” vectors would yield the following result?"
animals <- c("moose", "monkey", "meerkat", "mountain lion")
# [1] TRUE TRUE TRUE TRUE
str_detect(animals, "mo*") # there can be none or any amount of o
str_detect(animals, "mo?") # there can be none or one o

"Q9 You are working on some data from different universities. You have the following vector:"  
#   > schools
# [1] "U. Kentucky"                 "Univ New Hampshire"          "Univ. of Massachusetts"      "University Georgia"         
# [5] "U California"                "California State University"
"You want to clean this data to match the full names of each university:"
#   > final
# [1] "University of Kentucky"      "University of New Hampshire" "University of Massachusetts" "University of Georgia"         
# [5] "University of California"    "California State University"
"Replace U, U., Univ and Univ. by University of"
schools = c("U. Kentucky", "Univ New Hampshire", "Univ. of Massachusetts", "University Georgia", 
            "U California", "California State University")
schools %>% 
  str_replace("^Univ\\.?\\s|^U\\.?\\s", "University ") %>% 
  str_replace("^University of |^University ", "University of ")

"Q10 Rather than using the pattern_with_groups vector from the video, you accidentally write in the following code:"
problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

"Q11 You notice your mistake and correct your pattern regex to the following"
problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

"Q12 Which answer best describes the differences between the regex string we use as an argument in"
# str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") 
"and the regex string in"
# pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"?"
The regex used in str_replace() looks for either a comma, period or space between the feet and inches digits,
while the pattern regex just looks for an apostrophe; the regex in str_replace allows for none or more digits
to be entered as inches, while the pattern regex only allows for one or two digits.

"Q13 It seems like the problem may be due to spaces around the words feet|foot|ft and inches|in. What is another way you could fix this problem?"
yes <- c("5 feet 7inches", "5 7")
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)
converted <- s %>% 
  str_replace("feet|foot|ft", "'") %>% 
  str_replace("inches|in|''|\"", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_view_all(converted, pattern)
#> Answer
converted <- s %>% 
  str_replace("\\s*(feet|foot|ft)\\s*", "'") %>% 
  str_replace("\\s*(inches|in|''|\")\\s*", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")
str_view_all(converted, pattern)
# Don't be confused about () -> groups don't change detection






# Assessment Part 3 -------------------------------------------------------

"Q1 If you use the extract code from our video, the decimal point is dropped. What modification of the code would 
 allow you to put the decimals in a third column called “decimal”?"
s <- c("5'10", "6'1\"", "5'8inches", "5'7.5")
tab <- data.frame(x = s); tab
extract(data = tab, col = x, into = c("feet", "inches", "decimal"), 
        regex = "(\\d)'(\\d{1,2})(\\.\\d+)?")    


"Q2 Which two commands would properly split the text in the “staff” column into each individual name? 
Select ALL that apply."
staff = "Mandy, Chris and Laura"
str_split(staff, ", | and ")
str_split(staff, ",\\s|\\sand\\s")
str_split(staff, "\\s?(,|and)\\s?") # doesn't work because "and" is in "Mandy"

"Q3"
# > schedule
# day         staff
# Monday   	Mandy, Chris and Laura
# Tuesday 	Steve, Ruth and Frank
"What code would successfully turn your “Schedule” table into the following tidy table?"
# > tidy
# day     staff
# <chr>   <chr>
# Monday  Mandy
# Monday  Chris
# Monday  Laura
# Tuesday Steve
# Tuesday Ruth 
# Tuesday Frank
tidy <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ")) %>% 
  unnest() # turn the str_split list of chr vectors into columns in a data frame

"Q4 Using the gapminder data, you want to recode countries longer than 12 letters in the region “Middle Africa” 
 to their abbreviations in a new column, “country_short”. Which code would accomplish this?"
dat <- gapminder %>% filter(region == "Middle Africa") %>% 
  mutate(country_short = recode(country, 
                                "Central African Republic" = "CAR", 
                                "Congo, Dem. Rep." = "DRC",
                                "Equatorial Guinea" = "Eq. Guinea"))

# brexit polling data
library(rvest); library(tidyverse); library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE) # brexit polling data

"Q5 Some rows in this table do not contain polls. You can identify these by the lack of the percent sign (%) in 
 the Remain column. Update polls by changing the column names to "
# c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes") 
"and only keeping rows that have a percent sign (%) in the remain column.
How many rows remain in the polls data frame?"
names = c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")
polls2 = polls[-1,][str_detect(polls$Remain, "%"),] %>%
  set_names(names)
polls2 = polls2 %>% as_tibble(); polls2 
length(polls2$dates)

"Q6 Which of these commands converts the remain vector to a proportion between 0 and 1?"
parse_number(polls2$remain)/100
as.numeric(str_replace(polls2$remain, "%", ""))/100

"Q7 The undecided column has some N/A values. These N/As are only present when the remain and leave columns 
 total 100%, so they should actually be zeros.
 Use a function from stringr to convert "N/A" in the undecided column to 0. The format of your command
 should be function_name(polls$undecided, "arg1", "arg2")."
a = c("N/A")
str_replace(a, "N/A", "0")
#> Answer is str_replace N/A, 0

"Q8 Write a regular expression to extract the end day and month from dates. 
 Insert it into the skeleton code below:"
temp <- str_extract_all(polls$dates, _____)
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)

"\\d+\\s[a-zA-Z]+"
"[0-9]+\\s[a-zA-Z]+"
"\\d{1,2}\\s[a-zA-Z]+"
"\\d+\\s[a-zA-Z]{3,5}" # {3,5} means 3 to 5
