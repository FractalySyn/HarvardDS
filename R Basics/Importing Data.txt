rm(list=ls())
library(tidyverse)

#  Paths and the working directory ----------------------------------------

## This code does not read the data into R, it just copies a file. But once the file is copied, we 
## can import the data with a simple line of code. Here we use the read_csv function from the readr 
## package, which is part of the tidyverse.

## Change the default working directory
setwd("D:/Coco/Documents/R")
getwd() # change is confirmed

## Get a full path
filename = "murders.csv"
directory = system.file("extdata", package = "dslabs") # in dslabs - in extdata
fullpath = file.path(directory, filename) # returns the full path
## Alternative
system.file("extdata/murders.csv", package = "dslabs")

## The path of a file is a list of directory names that can be thought of as instructions on what folders 
## to click on, and in what order, to find the file. If these instructions are for finding the file from 
## the root directory we refer to it as the full path. If the instructions are for finding the file starting 
## in the working directory we refer to it as a relative path.
dir = system.file(package = "dslabs") # starts from the WD - relative path - but returns the full path
list.files(path = dir) # returns relative paths = elements in the directory

## Verify a file is in the folder
directory = system.file("extdata", package = "dslabs")
filename %in% list.files(directory)

## Copy the file in the wd
file.copy(from = fullpath, to = "murders.csv") # copies the data file in the default path
file.copy(fullpath, "murders.csv-test") # we can rename it
## Returns false if the file already exists in the destination folder
file.copy(from = fullpath, to = "murders.csv", overwrite = T)

## When the file is in the WD we can read it without specifyind its path
data = read.csv("murders.csv")




# The readr and readxl packages -------------------------------------------

library(readr)
## The readr library includes functions for reading data stored in text file spreadsheets into R. readr is part 
## of the tidyverse package
"read_table	  white space separated values	                  txt
 read_csv	  comma separated values	                        csv
 read_csv2	  semicolon separated values	                    csv
 read_tsv	  tab delimited separated values	                tsv
 read_delim	  general text file format, must define delimiter	txt"

## To get an idea about the function to use we can read some lines of the data file
read_lines("murders.csv", n_max = 5)
# -> coma separated - we also learn that the data has a header ([1] line)
data = read.csv("murders.csv", header = T) # header is TRUE by default
class(data)
## read.csv() != read_csv() which comes from the tidyverse and thus returns a tibble
data = read_csv("murders.csv")
class(data)


library(readxl)
"read_excel	 auto detect the format	  xls, xlsx
 read_xls	   original format	        xls
 read_xlsx	 new format	              xlsx"

## The Microsoft Excel formats permit you to have more than one spreadsheet in one file.
## These are referred to as sheets
example = system.file("extdata", package = "openxlsx") %>% file.path("loadExample.xlsx") 
excel_sheets(example) # returns the sheets names


# Downloading files -------------------------------------------------------

## Read from url link
url = "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
data = read_csv(url)

## Download the file -- Warning : it will overwrite existing files
download.file(url, destfile = "murders_downloaded.csv")
## We can create a random name though
fname = tempfile()
download.file(url, fname)
data = read.csv(fname)

## Remove the downloaded file
file.remove(fname) # FALSE if it doesn't exist

## Other functions
file.rename(from = "murders.csv-test", to = "murders-test.csv")
## Many others
?base::files()



# R-base importin functions ----------------------------------------------

## R-base equivalent functions read.table(), read.csv() and read.delim() present some differences
## An important difference is that the characters are converted to factors (unless we specify stringsasfactors = F)
data2 = read.csv("murders.csv")
class(data2$state)




# Text vs binary files ----------------------------------------------------

## The csv tables you have read are also text files. One big advantage of these files is that we can easily 
## "look" at them without having to purchase any kind of special software or follow complicated instructions.

## However, if you try to open, say, an Excel xls file, jpg or png file, you will not be able to see anything 
## immediately useful. These are binary files. Excel files are actually compressed folders with several text 
## files inside

## Although R includes tools for reading widely used binary files, such as xls files, in general you will want
## to find data sets stored in text files. Similarly, when sharing data you want to make it available as text 
## files as long as storage is not an issue (binary files are much more efficient at saving space on your disk).
## In general, plain-text formats make it easier to share data since commercial software is not required for 
## working with the data.




# Unicode vs ASCII --------------------------------------------------------

## !!! That was the source of the warning I got with me first econo R works

## A pitfall in data science is assuming a file is an ASCII text file when, in fact, it is something else that
## can look a lot like an ASCII text file: a Unicode text file.

## To understand the difference between these, remember that everything on a computer needs to eventually be 
## converted to 0s and 1s. ASCII is an encoding that maps characters to numbers. ASCII uses 7 bits (0s and 1s) 
## which results in  2^7 = 128 unique items, enough to encode all the characters on an English language 
## keyboard. However, other languages use characters not included in this encoding. For this reason, a new 
## encoding, using more than 7 bits, was defined: Unicode. When using Unicode, one can chose between 8, 16,
## and 32 bits abbreviated UTF-8, UTF-16, and UTF-32 respectively. RStudio actually defaults to UTF-8 encoding.



# Advices on Organizing data with spreadsheets ----------------------------

## Although there are R packages designed to read this format, if you are choosing a file format to save your 
## own data, you generally want to avoid Microsoft Excel. We recommend Google Sheets as a free software tool 
## for organizing data.

"Choose Good Names for Things -- do not use spaces, use underscores _ instead
 Write Dates as YYYY-MM-DD - To avoid confusion, we strongly recommend using this global ISO 8601 standard.
 No Empty Cells - Make It a Rectangle
 Create a Data Dictionary - If you need to explain things, do this in a separate file.
 No Calculations in the Raw Data Files 
 Make regular backups of your data.
 Save the Data as Text Files - Save files for sharing in comma or tab delimited format."





# Assessment --------------------------------------------------------------

"Q2 Which files could be opened in a basic text editor?"
data.txt .csv .tsv

# initials,state,age,time
# vib,MA,61,6:01
# adc,TX,45,5:45
# kme,CT,50,4:19
"Q3 What type of file is this?"
A comma-delimited/separated file with header

"Q4 Assume the following is the full path to the directory that a student wants to use as their working 
 directory in R: /Users/student/Documents/projects/. Which of the following lines of code CANNOT set the 
 working directory to the desired projects directory?"
setwd(/Users/student/Documents/projects/)
# "Correct ways to achieve this:"
setwd("~/Documents/projects/")
setwd("/Users/student/Documents/projects/")
dir = "/Users/student/Documents/projects"; setwd(dir)

# We want to copy the "murders.csv" file from the dslabs package into an existing folder "data", which is 
# located in our HarvardX-Wrangling projects folder. We first enter the code below into our RStudio console.
# > getwd()
# [1] "C:/Users/UNIVERSITY/Documents/Analyses/HarvardX-Wrangling"
# > filename <- "murders.csv"
# > path <- system.file("extdata", package = "dslabs")
"Q5 Which of the following commands would NOT successfully copy “murders.csv” into the folder “data”?"
file.copy(file.path(path, "murders.csv"), getwd())
# because the current working directory isn't "data" !

"Q6 You are not sure whether the murders.csv file has a header row. How could you check this?"
Open the file in a basic text editor.
In the RStudio “Files” pane, click on your file, then select “View File”.
Use the command read_lines (remembering to specify the number of rows with the n_max argument).

"Q7 What is one difference between read_excel() and read_xlsx()?"
read_excel() reads both .xls and .xlsx files by detecting the file format from its extension, 
while read_xlsx() only reads .xlsx files.


# You have a file called “times.txt” that contains race finish times for a marathon. The first four lines 
# of the file look like this:
# initials,state,age,time
# vib,MA,61,6:01
# adc,TX,45,5:45
# kme,CT,50,4:19
"Q8 Which line of code will NOT produce a tibble with column names “initials”, “state”, “age”, and “time”?"
race_times <- read.csv("times.txt")

# You also have access to marathon finish times in the form of an Excel document named “times.xlsx”. In the Excel
# document, different sheets contain race information for different years. The first sheet is named “2015”, the 
# second is named “2016”, and the third is named “2017”.
"Q9 Which line of code will NOT import the data contained in the “2016” tab of this Excel sheet?"
times_2016 <- read_xlsx("times.xlsx", sheet = “2”)
# sheet = "2016" or sheet = 2 are correct

# You have a comma-separated values file that contains the initials, home states, ages, and race finish times for 
# marathon runners. The runners’ initials contain three characters for the runners’ first, middle, and last names 
# (for example, “KME”).
# You read in the file using the following code.
# race_times <- read.csv(“times.csv”)
"Q10 What is the data type of the initials in the object race_times?"
The answer is factors because read.csv() is a R base function. However, since the R 4.0 update we have :
default.stringsAsFactors() #> = FALSE
So now strings remain characters with R base functions.

"Q11 Which of the following is NOT a real difference between the readr import functions and the base R import 
functions?"
The base R import functions can read .csv files, but cannot read files with other delimiters, such as .tsv files, 
or fixed-width files. #> it's false
"Differences are:"
# The import functions in the readr package all start as read_, while the import functions for base R all start 
# with read.
# Base R import functions automatically convert character columns to factors.
# Base R functions import data as a data frame, while readr functions import data as a tibble.

# race_times <- read.csv(“times.csv”, stringsAsFactors = F)
"Q12 What is the class of the object race_times?"
data frame

# url <- "https://raw.githubusercontent.com/MyUserName/MyProject/master/MyData.csv "
# dat <- read_csv(url)
# download.file(url, "MyData.csv")
"Q13 Select the answer choice that summarizes all of the actions that the following lines of code can perform."
Create a tibble in R called dat that contains the information contained in the csv file stored on Github. 
Download the csv file to the working directory and name the downloaded file “MyData.csv”.

library(readr)
"Q14 Inspect the file at the following URL:"
url = "http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
"Which readr function should be used to import this file?"
read_csv(url)

"Q15 Does this file have a header row? Does the readr function you chose need any additional arguments to import 
the data correctly?"
No, there is no header. The col_names=FALSE argument is necessary.
read_csv(url, col_names = F)

"How many rows are in the dataset? How many columns are in the dataset?"
dim(read_csv(url, col_names = F))
# There are 569 rows (be careful, the first question w/o argument returned 568 rows because the first was used as 
#                     a header)
# There are 32 columns












