rm(list = ls())
options(digits = 4)
library(imager); library(gtools); library(tidyverse); library(ggplot2); library(dslabs)

# The data we need to answer a question is not always in a spreadsheet ready for us to read. For example, the US murders dataset we used 
# in the R Basics chapter originally comes from this Wikipedia page:
url = "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"
# Unfortunately, there is no link to a data file. To make the data frame that is loaded when we type data(murders), we had to do some 
# web scraping.

"Web scraping, or web harvesting, is the term we use to describe the process of extracting data from a website. The reason we can do this 
is because the information used by a browser to render webpages is received as a text file from a server. The text is code written in 
hyper text markup language (HTML)."
# Every browser has a way to show the html source code for a page, each one different. On Chrome, you can use Control-U





# HTML --------------------------------------------------------------------

# Because this code is accessible, we can download the HTML file, import it into R, and then write programs to extract the information we 
# need from the page. However, once we look at HTML code, this might seem like a daunting task. But we will show you some convenient tools 
# to facilitate the process. To get an idea of how it works, here are a few lines of code from the Wikipedia page that provides the US 
# murders data:
<table class="wikitable sortable">
  <tr>
  <th>State</th>
  <th><a href="/wiki/List_of_U.S._states_and_territories_by_population" 
title="List of U.S. states and territories by population">Population</a><br />
  <small>(total inhabitants)</small><br />
  <small>(2015)</small> <sup id="cite_ref-1" class="reference">
  <a href="#cite_note-1">[1]</a></sup></th>
  <th>Murders and Nonnegligent
<p>Manslaughter<br />
  <small>(total deaths)</small><br />
  <small>(2015)</small> <sup id="cite_ref-2" class="reference">
  <a href="#cite_note-2">[2]</a></sup></p>
  </th>
  <th>Murder and Nonnegligent
<p>Manslaughter Rate<br />
  <small>(per 100,000 inhabitants)</small><br />
  <small>(2015)</small></p>
  </th>
  </tr>
  <tr>
  <td><a href="/wiki/Alabama" title="Alabama">Alabama</a></td>
  <td>4,853,875</td>
  <td>348</td>
  <td>7.2</td>
  </tr>
  <tr>
  <td><a href="/wiki/Alaska" title="Alaska">Alaska</a></td>
  <td>737,709</td>
  <td>59</td>
  <td>8.0</td>
  </tr>
  <tr>


# You can actually see the data, except data values are surrounded by html code such as <td>. We can also see a pattern of how it is 
# stored. If you know HTML, you can write programs that leverage knowledge of these patterns to extract what we want. We also take 
# advantage of a language widely used to make webpages look “pretty” called Cascading Style Sheets (CSS)

"Although we provide tools that make it possible to scrape data without knowing HTML, as a data scientist it is quite useful to learn
some HTML and CSS. Not only does this improve your scraping skills, but it might come in handy if you are creating a webpage to showcase
your work. There are plenty of online courses and tutorials for learning these. Two examples are Codeacademy and W3schools."




# rvest package -----------------------------------------------------------

"The tidyverse provides a web harvesting package called rvest. The first step using this package is to import the webpage into R. 
The package makes this quite simple:"
library(rvest)
h = read_html(url); class(h)

# The rvest package is actually more general; it handles XML documents. XML is a general markup language (that’s what the ML 
# stands for) that can be used to represent any kind of data. HTML is a specific type of XML specifically developed for representing 
# webpages

# Now, how do we extract the table from the object h? If we print h, we don’t really see much
h

# We can see all the code that defines the downloaded webpage using the html_text function like this:
html_text(h)

# We don’t show the output here because it includes thousands of characters, but if we look at it, we can see the data we are after are 
# stored in an HTML table: you can see this in this line of the HTML code above <table class="wikitable sortable">.
"The different parts of an HTML document, often defined with a message in between < and > are referred to as nodes"

"The rvest package includes functions to extract nodes of an HTML document: html_nodes extracts all nodes of different types and 
 html_node extracts the first one. To extract the tables from the html code we use:"
tab = html_nodes(h, "table")
# Now, instead of the entire webpage, we just have the html code for the tables in the page:
tab

# The table we are interested is the first one:
tab[[1]]

"This is clearly not a tidy dataset, not even a data frame. In the code above, you can definitely see a pattern and writing code
 to extract just the data is very doable. In fact, rvest includes a function just for converting HTML tables into data frames:"
tab = html_table(tab[[1]])
head(tab); class(tab)

# We are now much closer to having a usable data table:
tab = tab %>%
  setNames(c("state", "population", "total", "rate"))
head(tab)

# We still have some wrangling to do. For example, we need to remove the commas and turn characters into numbers. Before continuing 
# with this, we will learn a more general approach to extracting information from web sites.




# CSS selectors -----------------------------------------------------------

# The default look of a webpage made with the most basic HTML is quite unattractive. The aesthetically pleasing pages we see today are 
# made using CSS to define the look and style of webpages. The fact that all pages for a company have the same style usually results 
# from their use of the same CSS file to define the style. The general way these CSS files work is by defining how each of the elements 
# of a webpage will look. The title, headings, itemized lists, tables, and links, for example, each receive their own style including 
# font, color, size, and distance from the margin. CSS does this by leveraging patterns used to define these elements, referred to as 
# selectors. An example of such a pattern, which we used above, is table, but there are many, many more.

"If we want to grab data from a webpage and we happen to know a selector that is unique to the part of the page containing this data, we
 can use the html_nodes function. However, knowing which selector can be quite complicated. In fact, the complexity of webpages has been 
 increasing as they become more sophisticated. For some of the more advanced ones, it seems almost impossible to find the nodes that 
 define a particular piece of data. However, selector gadgets actually make this possible."

"SelectorGadget is piece of software that allows you to interactively determine what CSS selector you need to extract specific components
from the webpage. If you plan on scraping data other than tables from html pages, we highly recommend you install it. "
# A Chrome extension is available which permits you to turn on the gadget and then, as you click through the page, it highlights parts and
# shows you the selector you need to extract these parts.





# JSON --------------------------------------------------------------------

# Sharing data on the internet has become more and more common. Unfortunately, providers use different formats, which makes it harder for 
# data scientists to wrangle data into R. Yet there are some standards that are also becoming more common. Currently, a format that is 
# widely being adopted is the JavaScript Object Notation or JSON. Because this format is very general, it is nothing like a spreadsheet. 
# This JSON file looks more like the code you use to define a list. Here is an example of information stored in a JSON format:

>
> Attaching package: 'jsonlite'
> The following object is masked from 'package:purrr':
>
>     flatten
> [
>   {
>     "name": "Miguel",
>     "student_id": 1,
>     "exam_1": 85,
>     "exam_2": 86
>   },
>   {
>     "name": "Sofia",
>     "student_id": 2,
>     "exam_1": 94,
>     "exam_2": 93
>   },
>   {
>     "name": "Aya",
>     "student_id": 3,
>     "exam_1": 87,
>     "exam_2": 88
>   },
>   {
>     "name": "Cheng",
>     "student_id": 4,
>     "exam_1": 90,
>     "exam_2": 91
>   }
> ]

"The file above actually represents a data frame. To read it, we can use the function fromJSON from the jsonlite package. Note that
 JSON files are often made available via the internet. Several organizations provide a JSON API or a web service that you can connect 
 directly to and obtain data. Here is an example:"
library(jsonlite)
citi_bike = fromJSON("http://citibikenyc.com/stations/json"); 
citi_bike[[1]]
citi_bike[[2]] %>% as_tibble() 

# You can learn much more by examining tutorials and help files from the jsonlite package. This package is intended for relatively simple
# tasks such as converging data into tables. For more flexibility, we recommend rjson.






# Assessment --------------------------------------------------------------

library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)

nodes <- html_nodes(h, "table")
html_table(nodes[[8]])


"Q1 Convert the first four tables in nodes to data frames and inspect them. Which of the first four nodes are 
 tables of team payroll?"
html_table(nodes[[1]]) %>% as_tibble()
html_table(nodes[[2]]) %>% as_tibble()
html_table(nodes[[3]]) %>% as_tibble()
html_table(nodes[[4]]) %>% as_tibble()
#> 2-3-4

"Q2 For the last 3 components of nodes, which of the following are true?"
length(nodes) #> 19 20 21 are the last three
html_table(nodes[[19]]) %>% as_tibble()
html_table(nodes[[20]]) %>% as_tibble()
html_table(nodes[[21]]) %>% as_tibble()
# All three entries are tables.
# The last entry shows the average across all teams through time, not payroll per team.

"Q3 Create a table called tab_1 using entry 10 of nodes. Create a table called tab_2 using entry 19 of nodes."
tab_1 = html_table(nodes[[10]]); head(tab_1)
tab_2 = html_table(nodes[[19]]); head(tab_2)
"Remove the extra column in tab_1, remove the first row of each dataset, and change the column names for 
 each table to Team, Payroll, Average. Use a full_join() by the Team to combine these two tables."
tab_1 = tab_1[-1, -1] %>%
  setNames(c("Team", "Payroll", "Average"))
tab_2 = tab_2[-1,] %>%
  setNames(c("Team", "Payroll", "Average"))
head(tab_1); head(tab_2)
full_join(tab_1, tab_2, by = "Team")
"Don't follow their next advice"

"The Wikipedia page on opinion polling for the Brexit referendum, in which the United Kingdom voted to 
leave the European Union in June 2016, contains several tables. One table contains the results of all polls 
regarding the referendum over 2016:"
url1 <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"

"Q4 Assign tab to be the html nodes of the table class."
brexit = read_html(url1) %>% html_nodes("table")
length(brexit)

"Q5 Inspect the first several html tables using html_table() with the argument fill=TRUE (you can read about
this argument in the documentation). Find the first table that has 9 columns with the first column named 
Date(s) conducted"
html_table(brexit[[5]], fill = T) %>% head() 
