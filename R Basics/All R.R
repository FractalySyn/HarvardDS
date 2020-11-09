library(dslabs)


# Basic Conditionnals -----------------------------------------------------

## if else
a = 2
if(a != 0) {print(1/a)} else {print("Pas d'inverse, a=0")}

data("murders"); murd = murders; attach(murd)
rate = total/population*10^5 

ind = which.min(rate) # indice de l'état avec le taux le plus faible
if(rate[ind]<0.5) {print(state[ind])} else {print("no rate under 0.5%")}

## ifelse() = fast if else
b = 0
ifelse(b>0, 1/b, NA)

## ifelse() works with vectors = ifelse for all datas
c = c(0,1,2,-4,5)
result = ifelse(c>0, 1/c, NA)
result

## ifelse() sur des données
data("na_example"); naex = na_example #data is a vector
sum(is.na(naex)) # 145 valeurs NA

no_nas = ifelse(is.na(naex), 0, naex) # remplacer les NA par 0 
sum(is.na(no_nas))

## the any() and all() functions evaluate logical vectors
z <- c(3,4,5)
any(z==3)
all(z==3)


# Functions ---------------------------------------------------------------
## moyenne arithmetique ou geometrique (capitalisation)
avg = function(x, arithmetic = T)
{
  n = length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n)-1)
}
sample = runif(100, min=0, max=1000); avg(sample)
returns = 1 + rnorm(20, 0, 0.2); avg(returns, arithmetic = FALSE)




# For Loops ---------------------------------------------------------------

## Exemple : somme d'une suite arithmétique
somme = function(n)
{
  x = 1:n
  sum(x)
}

m = 25
vec = vector(length = m)
for (i in 1:m)
{
  vec[i] = somme(i)
}
vec

j = 1:25
plot(vec~j, main="Summations")

lines(j, j*(j+1)/2) # comparaison avec la formule de la somme



# Other functions ---------------------------------------------------------

## sapply()
k = 1:10
sapply(k, somme) # apply the function somme to the vector k




# Packages functions ------------------------------------------------------

## Different packages may have identical functions or different but with the same name
detach(package:stats); detach(package:dplyr) # disable packages
library(stats)
library(dplyr)
## on remarque que stats avait deja deux fonctions nommées filter et lag, elles en sont masquées
## on utilise les fonctions du dernier package activé ET il faut detacher le package avant de le réactiver pour acceder a ses fonctions homonymes

## search() - donne l'ordre de recherche des fonctions
search() # dplyr est bien avant stats

## Alternative - forcer l'utilisation d'une fonction
stats::filter
dplyr::filter

## Le :: sert aussi a utiliser une fontion de n'importe quel package - sans avoir besoin de l'activer
fBasics::basicStats


















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
 read_csv	    comma separated values	                        csv
 read_csv2	  semicolon separated values	                    csv
 read_tsv	    tab delimited separated values	                tsv
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












install.packages("dslabs")
library(dplyr); library(dslabs)

data("murders"); dta = murders; head(dta)
attach(dta)



# Indexing ----------------------------------------------------------------

## index with logical operators
murder_rate = total / population * 100000 # en pourcentage
index = murder_rate <= 0.71 # index devient un vecteur donnant pour chaque observation Vrai ou Faux a la condition

state[index==T] # revoie l'état correspondant pour chaque donnée ayant un taux <= 0.71
sum(index==T); sum(index) # nombre d'observations remplissant la condition

west = region == "West"
safe = murder_rate <= 1

index2 = safe & west
state[index2]

## which()
indexm = which(state == "Massachusetts"); indexm #indice correspondant a la condition
murder_rate[indexm]

## match() = multiple which
indexes = match(c("Texas", "Florida"), state); indexes # attribut les indices des données des états spécifiés
murder_rate[indexes]

## %in% operator
x = c(1,2,3,4,5)
y = c(1,2,8)

y %in% x # pour chaque élément de y renvoie s'il est ou non contenu dans x

c("Boston", "Dakota", "Washington") %in% state # boston et dakota ne sont pas des états



# Basic Data Wrangling ----------------------------------------------------

## mutate() = add a column
murders = mutate(murders, rate = murder_rate) # add the rate column to murders
dta = murders; attach(dta)

## filter()
dta = filter(dta, rate < 0.7) # keep only the data that fill the conditions

## select() = data.frame()
dta = select(dta, state, region, rate) 
dta = data.frame(state, region, rate)

## pipe operator %>% = mutate then select then filter
data("murders"); dta = murders
dta %>% mutate(rate = murder_rate) %>% select(state, region, rate) %>% filter(rate <= 0.71)
## l'argument a gauche du pipe devient par défaut le premier argument des fonctions ensuite utilisées
16 %>% sqrt()
## le résultat sera l'argument de la fonction suivante
9 %>% sqrt() %>% factorial() # = factorial(sqrt(9))

## creating a data frame with stringAsFactors = FALSE
grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"), exam_1 = c(95, 80, 90, 85), exam_2 = c(90, 85, 85, 90), stringsAsFactors = F)
grades

  


# Basic plots -------------------------------------------------------------
data("murders"); dta = murders; attach(dta); murder_rate = total / population * 100000; dta = mutate(murders, rate = murder_rate); attach(dta)

##
pop = population/10^6
kills = total
plot(kills ~ pop)
hist(rate)
boxplot(rate~region)



# Exercices ---------------------------------------------------------------
data("heights"); taille = heights; attach(taille)
options(digits = 3) ## tous les résultats en trois chiffres
summary(taille)

avg = mean(height); avg
sum(height > avg)

cond = height > avg & sex == "Female"
sum(cond)

women = mean(sex=="Female"); women

min = min(height); min
ind = match(min, height); ind; height[1032]
sex[ind]

max = max(height); max
x = min:max; x
length(x)

x %in% height
sum(!(x %in% height))

taille = mutate(taille, cm = height*2.54); attach(taille)
cm[18]
mean(cm)
sum(sex=="Female")

girls = filter(taille, sex=="Female")
mean(girls$cm)

data("olive"); fat = olive; attach(olive)
plot(palmitic~palmitoleic)

hist(eicosenoic)

boxplot(palmitic~region)
















# What is Tidy data -------------------------------------------------------

## Data frames greatly facilitate the organization of information.
## We will focus on a specific data format referred to as tidy and 
## on specific collection of packages that are particularly helpful 
## for working with tidy data referred to as the tidyverse.
library(tidyverse)
## tidyverse contient plusieurs packages dont dplyr
tidyverse_packages()


## We say that a data table is in tidy format if each row represents one observation and columns represent 
## the different variables available for each of these observations. The murders dataset is an example of a tidy data frame.
library(dslabs); data("murders")
head(murders)
## Each row represent a state with each of the five columns providing a different variable related 
## to these states: name, abbreviation, region, population, and total murders

#>       country 1960 1961 1962
#> 1     Germany 2.41 2.44 2.47
#> 2 South Korea 6.16 5.99 5.79
this is not tidy because each row contains differrent informations
the year is a variable, here it is stored in the header
For the tidyverse packages to be optimally used, data need to be reshaped into tidy format
->>>> Data Wrangling part
#>       country year fertility
#> 1     Germany 1960      2.41
#> 2 South Korea 1960      6.16
#> 3     Germany 1961      2.44
#> 4 South Korea 1961      5.99
#> 5     Germany 1962      2.47
#> 6 South Korea 1962      5.79




# Summarizing data --------------------------------------------------------

## summarize() // same as summarise ==> both spelling work
library(dslabs)
data("heights"); heights = heights; attach(heights)
summarize(heights, avg = mean(height), stdev = sd(height))

# advanced tips in the Do() section

s = heights %>% filter(sex=="Female") %>% summarize(avg=mean(height), stdev=sd(height)); s # summarise for women only
# as we can see in the environement tab, s is a data frame (it's a table)
# we can access its components
s$avg


data("murders"); murders = murders
murders = murders %>% mutate(rate = total/population*100000); attach(murders)
usrate = murders %>% summarize(rate = sum(total)/sum(population)*10^5); usrate # us murder rate as a data frame

## pull() - récupérer les valeurs d'une colonne 
class(usrate)
a = pull(usrate, rate); a; class(a) ## devient un nombre
b = pull(murders, population); b # devient un vecteur
# - inutile pour l'instant car
b == murders$population # TRUE

## group_by()
grheights = group_by(heights, sex); grheights # grouped data frame !
# les données sont les meme mais seront traitées différemment par certaines fonctions de dplyr
summarize(grheights, mean = mean(height))

murders %>% group_by(region) %>% summarise(mean=mean(rate))




# Sorting data ------------------------------------------------------------

## sort() and order() and rank()
data("murders"); attach(murders)
total
sort(total, decreasing = F) # tri croissant du vecteur 

order(total, decreasing = F) # renvoie les indices dans l'ordre croissant
# -> verification
min(total) == total[46] # TRUE

rank(total) # renvoie le rang <=> classement de 1 en 1 des valeurs, fraction si meme valeurs (rang 2.5 si deux meme valeurs au rang 2)

## arrange() - sort entire table (matrix)
arrange(murders, population)
arrange(murders, desc(population)) # decroissant
## s'il y a des groupes on peut trier plusieurs fois
murders %>% mutate(rate = total/population*10^5) %>% arrange(region, rate) %>% head(15) # tri par region et par taux de meurtre au sein de chaque groupe
## sort a grouped data frame
data("heights"); attach(heights)
heights %>% group_by(sex) %>% arrange(height, .by_group=T)

## top_n() # filter by and show n top rows
top_n(murders, 7, population) # show top 7 cities ordered by population




# Tibbles -----------------------------------------------------------------

## we can see that after the use of group_by() we don't have a table with columns and there's written A Tibble : dimxdim
g = heights %>% group_by(sex); g
class(g)
# this grouped data frame is a kind of tibble
# summarize() also returns a tibble
# other functions like filter() select() arrange() etc. are different, 
# if they receive a regular data frame they return a regular data frame, 
# if they receive a tibble they return a tibble

## The print method for tibbles is more readable than that of a data frame
murders
as_tibble(murders) # conversion data frame to tibble  
# it adjusts to the console window size
# If you subset the columns of a data frame, you may get back an object that is not a data frame, such as a vector or scalar
a = murders[5,4]; a; class(a)
b = murders[,4]; b; class(b)
# With tibbles this does not happen, it returns a tibble
a = as_tibble(murders[5,4]); a; class(a)
b = as_tibble(murders[,4]); b; class(b)
# This is useful in the tidyverse since functions require data frames as input
# However it is still possible to get a vector or a scalar
a = as_tibble(murders)$population; a; class(a)
b = as_tibble(murders)$population[12]; b; class(b)

## A related feature is that tibbles will give you a warning if you try to access a column that does not exist
murders$Population # no warning = harder to debug
as_tibble(murders)$Population # warning

## While data frame columns need to be vectors of numbers, strings, or logical values, tibbles can have more complex objects, such as lists or functions
tibble(id = c(1, 2, 3), func = c(mean, median, sd))

## As already mentionned with the grouped tibbles,The tidyverse functions are aware of the group information (like summarize())

## Create a tibble
grades <- tibble(names = c("John", "Juan", "Jean", "Yao"), exam_1 = c(95, 80, 90, 85), exam_2 = c(90, 85, 85, 90))
class(grades$names)
# the data.drame() has the default to convert strings to factors without warning it
grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"), exam_1 = c(95, 80, 90, 85), exam_2 = c(90, 85, 85, 90))
class(grades$names)
# in this case we have to specify it to get characters
grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"), exam_1 = c(95, 80, 90, 85), exam_2 = c(90, 85, 85, 90), stringsAsFactors = F)
class(grades$names)




# Do() --------------------------------------------------------

## Most R functions do not recognize grouped tibbles nor do they return data frames
## The do() function is a bridge between R functions such as quantile and the tidyverse
## The do function understands grouped tibbles and always returns a data frame

## if we attempt to use quantile to obtain the min, median and max in one call, we will receive an error
library(dslabs); data("heights"); attach(heights)
heights %>% filter(sex == "Female") %>% summarize(range = quantile(height, c(0, 0.5, 1)))

## fix this
my_summary = function(data, vector)
{
  attach(data)
  x = quantile(vector, c(0, 0.5, 1))
  tibble(min = x[1], median = x[2], max = x[3])
}
# this function returns a tibble of these three quantiles
heights %>% group_by(sex) %>% my_summary(height)
# issue : my_summary() isn't part of the tidyverse and thus doesn't know about grouped data frames

## do() allows us to handle grouped data frames
heights %>% group_by(sex) %>% do(my_summary(.)) ##### syntax is important (.)




# Purr package ------------------------------------------------------------

## while sapply() can return different types of objects, map() will always return a list
library(purrr)
n = 1:25 
sum1 = function(n) {x = 1:n; sum(x)}
map(n, sum1)
## furthermore we can have more control on it
map_dbl(n, sum1) # vector
## to get a tibble
sum2 = function(n) {x = 1:n; tibble(sum = sum(x))}
map_df(n, sum2)



# Tidyverse conditionals --------------------------------------------------

## case_when()
library(dplyr); library(dslabs)
x = runif(10, -50, 50)
case_when(x < 0 ~ "Negative", x > 0 ~ "Positive", TRUE ~ "Zero") # TRUE means else

## example we want to compare murder rates by group
data("murders"); attach(murders)
murders = murders %>% mutate(group = case_when(region == "South" ~ "South",
                                              abb %in% c("ME", "NH", "VT", "MA", "RI", "CT") ~ "New England",
                                              abb %in% c("WA", "OR", "CA") ~ "West Coast",
                                              TRUE ~ "Other"))
attach(murders)
murders %>% group_by(group) %>% summarise(rate = sum(total) / sum(population) * 10^5)


## between() - is x between a and b ?
a = 0; b = 10; x = runif(10, 0, 10)
between(x, a, b)






# Assessment --------------------------------------------------------------

library(dslabs); library(tidyverse)
data("heights")
ifelse(heights$sex=="Male", 2, 1) %>% sum()

ifelse(heights$height>72, heights$height, 0) %>% mean()

inches_to_ft = function(inches) inches/12
inches_to_ft(144)
sum(heights$height %>% inches_to_ft() < 5)

fact = factorial(1:10); fact
