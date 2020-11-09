rm(list = ls())
library(imager)

"Here, we describe how you can keep your projects organized so that rerunning
an analysis is straight-forward. We then demonstrate how to generate reproducible
reports with R markdown and the knitR package in a way that will greatly help with
recreating reports with minimal work. This is possible due to the fact that R markdown
documents permit code and textual descriptions to be combined into the same document,
and the figures and tables produced by the code are automatically added to the document."



# Rstudio projects --------------------------------------------------------

"RStudio provides a way to keep all the components of a data analysis project organized into
one folder and to keep track of information about this project, such as the Git status of
files, in one file. In Section 37.6 we demonstrate how RStudio facilitates the use of Git and
GitHub through RStudio projects. In this section we quickly demonstrate how to start a
new a project and some recommendations on how to keep these organized. RStudio projects
also permit you to have several RStudio sessions open and keep track of which is which."
New project -> New/Existing Directory -> select the type (project, package...)

# Now you will have to decide on the location of the folder that will be associated with your
# project, as well as the name of the folder. When choosing a folder name, just like with file
# names, make sure it is a meaningful name that will help you remember what the project
# is about. As with files, we recommend using lower case letters, no spaces, and hyphens
# to separate words. We will call the folder for this project my-first-project. This will then
# generate a Rproj file called my-first-project.Rproj in the folder associated with the project.
# We will see how this is useful a few lines below.

"When you start using RStudio with a project, you will see the project name in the upper
right corner. We can close it or change project from this section"

# One of the main advantages of using Projects is that after closing RStudio, if we wish to
# continue where we left off on the project, we simply double click or open the file saved when
# we first created the RStudio project. In this case, the file is called my-first-project.Rproj. If
# we open this file, RStudio will start up and open the scripts we were editing.



# R markdown --------------------------------------------------------------

"R markdown is a format for literate programming documents. It is based on markdown, a
markup language that is widely used to generate html pages. You can learn more about
markdown here: "
https://www.markdowntutorial.com/

# Unlike a word processor, such as Microsoft Word, where what you see is
# what you get, with R markdown, you need to compile the document into the final report.
# The R markdown document looks different than the final product. This seems like a disadvantage
# at first, but it is not because, for example, instead of producing plots and inserting
# them one by one into the word processing document, the plots are automatically added.

"In RStudio, you can start an R markdown document by clicking on File, New File, the R
Markdown. You will then be asked to enter a title and author for your document. We are
going to prepare a report on gun murders so we will give it an appropriate name. You can
also decide what format you would like the final report to be in: HTML, PDF, or Microsoft
Word. Later, we can easily change this, but here we select html as it is the preferred format
for debugging purposes:"

# This will generate a template file. Once you gain experience with R Markdown, you will be able to do 
# this without the template and can simply start from a blank template.

"The header" 
#> (optional) describes file informations
#> we can manually change the document type -> html_document to pdf_document for example

"R code chunks"
# It is where we put the code, we can use different languages
```{r cars}
summary(cars)
```
"These are the code chunks. When you compile the document, the R code inside the chunk,
in this case summary(pressure), will be evaluated and the result included in that position
in the final document"
# To add your own R chunks, you can type the characters above quickly with the key binding
# Ctrl-Alt-F (Ctrl-Alt-I default) on Windows or click insert>R
"Comments are titles, normal text remains normal text, and chunks return the code and its result
in the document"
# By default, the code will show up as well. To avoid having the code show up, you can use
# an argument. To avoid this, you can use the argument echo=FALSE.
```{r echo=FALSE}
summary(pressure)
```
# We recommend getting into the habit of adding a label to the R code chunks. This will be
# very useful when debugging, among other situations. It doesn't appear in the report
```{r pressure-summary}
summary(pressure)
```

"Global Options"
# One of the R chunks contains a complex looking call:
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
# As you become more experienced with R Markdown, you will learn the advantages of setting global options
# for the compilation process.

"We can also produce documents that render on GitHub using output: github_document.
This will produce a markdown file, with suffix md, that renders in GitHub. Because we have
uploaded these files to GitHub, you can click on the md file and you will see the report as a
webpage:"




# More on R markdown ------------------------------------------------------

• Book : https://bookdown.org/yihui/rmarkdown/
• RStudio’s tutorial: https://rmarkdown.rstudio.com
• The cheat sheet: https://www.rstudio.com/wp-content/uploads/2015/02/rmarkdowncheatsheet.
pdf







# Putting it all together -------------------------------------------------

"Terminal"
cd data-projects
mkdir murders
cd murders
mkdir data rdas

"Create a project"
# in the murders directory
# the working directory will be murders, so by default all created and dowloaded files will be in it

"R script download-data.r"
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dest_file <- "data/murders.csv"
download.file(url, destfile = dest_file)

"R script wrangle-data.r"
library(tidyverse)
murders <- read_csv("data/murders.csv")
murders <-murders %>% mutate(region = factor(region),
                             rate = total / population * 10^5)
save(murders, file = "rdas/murders.rda")
# In this file, we introduce an R command we have not seen: save. The save command in R
# saves objects into what is called an rda file: rda is short for R data. We recommend using
# the .rda suffix on files saving R objects.

"R script analysis.r"
library(tidyverse)
load("rdas/murders.rda")
murders %>% mutate(abb = reorder(abb, rate)) %>%
  ggplot(aes(abb, rate)) +
  geom_bar(width = 0.5, stat = "identity", color = "black") +
  coord_flip()

"Terminal"
mkdir figs

"R script analysis.r"
ggsave("figs/barplot.png")

"In Rstudio > README"
# New file > txt > README.txt
# We analyze US gun murder data collected by the FBI.
# download-data.R - Downloads csv file to data directory
# wrangle-data.R - Creates a derived dataset and saves as R object in rdas directory
# analysis.R - A plot is generated and saved in the figs directory.

"Terminal Git"
git init
git add README.txt
git commit -m "Add readme"
git remote add origin `https://github.com/FractalySyn/murders.git` #> First create this repo on Github
git push --set-upstream origin master

"We can continue adding and committing each file, but it might be easier to use RStudio.
To do this, start the project by opening the Rproj file. The git icons should appear and you
can add, commit and push using these."

All done !!!


























