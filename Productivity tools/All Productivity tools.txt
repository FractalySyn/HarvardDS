rm(list=ls())

"A data analysis project is not always a dataset and a script. A typical data analysis challenge
may involve several parts, each involving several data files, including files containing the
scripts we use to analyze data. Keeping all this organized can be challenging. We will learn
to use the Unix shell as a tool for managing files and directories on your computer system.
Using Unix will permit you to use the keyboard, rather than the mouse, when creating
folders, moving from directory to directory, and renaming, deleting, or moving files. We also
provide specific suggestions on how to keep the filesystem organized."

"The data analysis process is also iterative and adaptive. As a result, we are constantly editing
our scripts and reports. In this chapter, we introduce you to the version control system
Git, which is a powerful tool for keeping track of these changes. We also introduce you to
GitHub1, a service that permits you to host and share your code. We will demonstrate how
you can use this service to facilitate collaborations. Keep in mind that another positive
benefit of using GitHub is that you can easily showcase your work to potential
employers."

"Finally, we learn to write reports in R markdown, which permits you to incorporate text
and code into a single document. We will demonstrate how, using the knitr package, we can
write reproducible and aesthetically pleasing reports by running the analysis and generating
the report simultaneously."






rm(list = ls())
library(imager)

"I prefer using R and mouse to manage files
 See the end of this document and R basics module"

# Unix is the operating system of choice in data science. We will introduce you to the Unix way
# of thinking using an example: how to keep a data analysis project organized. We will learn
# some of the most commonly used commands along the way. However, we won’t go into the
# details here. We highly encourage you to learn more, especially when you find yourself using
# the mouse too much or performing a repetitive task often. In those cases, there is probably
# a more efficient way to do it in Unix. Here are some basic courses to get you started:
• https://www.codecademy.com/learn/learn-the-command-line
• https://www.edx.org/course/introduction-linux-linuxfoundationx-lfs101x-1
• https://www.coursera.org/learn/unix

# When searching for Unix resources, keep in mind that other terms used to describe what we
# will learn here are Linux, the shell and the command line. Basically, what we are learning is
# a series of commands and a way of thinking that facilitates the organization of files without
# using the mouse.




# The terminal ------------------------------------------------------------

"Install a shell terminal -> ubuntu, gitbash... = cmd.exe with unix commands
 Global options -> Terminal -> select default
 The it can be used in R"

"Instead of clicking, dragging, and dropping to organize our files and folders, we will be typing
Unix commands into the terminal. The way we do this is similar to how we type commands
into the R console, but instead of generating plots and statistical summaries, we will be
organizing files on our system."

# type this command into your command line:
echo "hello world"
# The command echo is similar to cat in R. Executing this line should print out hello world,
# then return back to the command line.

"Notice that you can’t use the mouse to move around in the terminal. You have to use the
keyboard. To go back to a command you previously typed, you can use the up arrow."




# The filesystem ----------------------------------------------------------

"We refer to all the files, folders, and programs on your computer as the filesystem. Keep in
mind that folders and programs are also files."

"In Unix, we refer to folders as directories. Directories that are inside other directories are
often referred to as subdirectories."

"The home directory is where all your stuff is kept, as opposed to the system files that come
with your computer, which are kept elsewhere."
# On your system, the name of your home directory is likely the same as your username on that system
#> Users>Coco for example

"Now suppose you can’t see your home directory on your screen. You would somehow need
to make it appear on your screen. One way to do this is to navigate from what is called the
root directory all the way to your home directory. Any filesystem will have what is called a
root directory, which is the directory that contains all directories."

# Note for Windows Users: The typical R installation will make your Documents directory
# your home directory in R. This will likely be different from your home directory in Git Bash.
# Generally, when we discuss home directories, we refer to the Unix home directory which for
# Windows, in this book, is the Git Bash Unix directory.

"In Unix, we don’t have the same visual cues, but the concept of a current location is indispensable.
We refer to this as the working directory. Each terminal window you have open
has a working directory associated with it."

# How do we know what is our working directory? To answer this, we learn our first Unix
# command: pwd, which stands for current directory. 
pwd

# We refer to the string returned by pwd as the full path of the working directory. The name
# comes from the fact that this string spells out the path you need to follow to get to the
# directory in question from the root directory. Every directory has a full path. Later, we will
# learn about relative paths, which tell us how to get to a directory from the working directory.

"In Unix, we use the shorthand ~ as a nickname for your home directory. So, for example,
if docs is a directory in your home directory, the full path for docs can be written like this
~/docs."
~/R

"In a point-and-click system, we know what is in a directory because we see it. In the terminal,
we do not see the icons. Instead, we use the command ls to list the directory content."
ls

"When we are preparing for a data science project, we will need to create directories. In Unix,
we can do this with the command mkdir, which stands for make directory."
# Because you will soon be working on several projects, we highly recommend creating a
# directory called projects in your home directory.
mkdir projects

# You can list more than one directory name like this:
mkdir docs teaching #> two folders are created

"If you made a mistake and need to remove the directory, you can use the command rmdir
to remove it."
mkdir junk
rmdir junk
# This will remove the directory as long as it is empty. If it is not empty, you will get an error
# message and the directory will remain untouched. To remove directories that are not empty,
# we will learn about the command rm later.


"Suppose we open a terminal and our working directory is our home directory. We want to
change our working directory to projects. We do this using the cd command, which stands
for change directory:"
cd projects
pwd

"When using cd, we can either type a full path, which will start with / or ~, or a relative
path. In the example above, in which we typed cd projects, we used a relative path. If
the path you type does not start with / or ~, Unix will assume you are typing a
relative path, meaning that it will look for the directory in your current working
directory."

"Now suppose we want to move back to the directory in which projects is a subdirectory,
referred to as the parent directory."
cd ..

# Important Pro Tip: In Unix you can auto-complete by hitting tab. This means that we
# can type cd d then hit tab. Unix will either auto-complete if docs is the only directory/file
# starting with d or show you the options.
cd p/tab/ #> cd projects

"Return to the home directory"
cd ~
"or just"
cd

# The working directory also has a nickname, which is a single ., so if you type
cd .
# you won't move

"When typing directory names, we can concatenate directories with the forward-slashes. So
if we want a command that takes us to the projects directory no matter where we are in
the filesystem, we can type:"
cd ~/projects
# which is equivalent to writing the entire path out.

"We can also concatenate directory names for relative paths. For instance, if we want to move
back to the parent directory of the parent directory of the working directory, we can type:"
cd ../..
# we move up 2 parent directories

"You can go back to whatever directory you just left by typing:"
cd -
# This can be useful if you type a very long path and then realize you want to go back to
# where you were, and that too has a very long path.
  
a # to complete cd - a and compute the following code
  
  

# Some examples -----------------------------------------------------------

plot(load.image("https://rafalab.github.io/dsbook/productivity/img/unix/filesystem-vertical.png"), axes=F)

"Now suppose our working directory is ~/projects and we want to move to reports in docs,
how can we do this?"
cd ../docs/reports
cd ~/docs/reports

"Let’s examine one more example. Suppose we are in ~/projects/project-1/figs and want
to change to ~/projects/project-2. Again, there are two ways."
cd ../../projects-2
cd ~/projects/projects-2



# More Unix commands ------------------------------------------------------

"Warning: mv will not ask “are you sure?” if your move results in overwriting a file."

"For example, if we want to move the file cv.tex from resumes to reports, you could use
the full paths like this:"
mv ~/docs/resumes/cv.tex ~/docs/reports #> mv location destination
"OR"
cd ~/docs/resumes
mv cv.tex ../reports

"We can also use mv to change the name of a file. To do this, instead of the second argument
being the destination directory, it also includes a filename. So, for example, to change the
name from cv.tex to resume.tex, we simply type:"
cd ~/docs/resumes
mv cv.tex resume.tex

"We can also combine the move and a rename. For example:"
cd ~/docs/resumes
mv cv.tex ../reports/resume.tex

"And we can move entire directories. To move the resumes directory into reports, we do as
follows:"
mv ~/docs/resumes ~/docs/reports/ # !!! It is important to add the last / to make it clear you do not want to 
                                  # rename the resumes directory to reports, but rather move it into the reports
                                  # directory.

"We can also copy the files the same way with cp"
cp ~/docs/resumes/cv.tex ~/docs/reports

"Warning: Unlike throwing files into the trash, rm is permanent. Be careful!"
rm file1 file2 ...
# To remove directories, you will have to learn about arguments, which we do later.

"Often you want to quickly look at the content of a file. If this file is a text file, the quickest
 way to do is by using the command less. To look a the file cv.tex, you do this:"
cd ~/docs/resumes
less cv.tex
"To exit the viewer, you type q."
# If you are wondering why the command is called less, it is because the original was called more, as in
# “show me more of this file”. The second version was called less because of the saying “less is more”.




# Preparing for a data science project ------------------------------------

# Our project relates to gun violence murders so we will call the directory for our project
# murders. It will be a subdirectory in our projects directories. In the murders directory, we
# will create two subdirectories to hold the raw data and intermediate data.
cd projects
mkdir murders
cd murders
mkdir data rdas
ls
pwd




# Advanced Unix -----------------------------------------------------------

"Most Unix commands can be run with arguments. Arguments are typically defined by using
a dash - or two dashes -- (depending on the command) followed by a letter or a word. An
example of an argument is the -r behind rm. The r stands for recursive and the result is
that files and directories are removed recursively"
rm -r dir
# all files, subdirectories, files in subdirectories, subdirectories in subdirectories, and so on,
# will be removed. This is equivalent to throwing a folder in the trash, except you can’t recover
# it. Once you remove it, it is deleted for good. Often, when you are removing directories, you
# will encounter files that are protected. In such cases, you can use the argument -f which
# stands for force.
rm -rf dir

"A command that is often called with argument is ls"
ls -a
# The a stands for all. This argument makes ls show you all files in the directory, including
# hidden files. In Unix, all files starting with a . are hidden. Many applications create hidden
# directories to store important information without getting in the way of your work. An
# example is git

"The l stands for long and the result is that more information about the files is shown."
ls -l
"It is often useful to see files in chronological order. For that we use:"
ls -t
"and to reverse the order of how files are shown you can use:"
ls -tr
ls -r # reverse alphabetical

"We can combine all these arguments to show more information for all files in reverse chronological
order:"
ls -lart

# As you may have noticed, Unix uses an extreme version of abbreviations. This makes it
# very efficient, but hard to guess how to call commands. To make up for this weakness, Unix
# includes complete help files or man pages (man is short for manual). In most systems, you
# can type man followed by the command name to get help. So for ls, we would type:
man ls "new window, q to quit"
#> press e to scroll down, y to scroll up
ls --help "on the console"

"The help pages are typically long and if you type the commands above to see the help, it
scrolls all the way to the end. It would be useful if we could save the help to a file and then
use less to see it. The pipe, written like this |, does something similar. It pipes the results
of a command to the command after the pipe. This is similar to the pipe %>% that we use
in R. To get more help we thus can type:"
ls --help | less
# This is also useful when listing files with many files. We can type:
ls -lart | less

"Some of the most powerful aspects of Unix are the wild cards. Suppose we want to remove
all the temporary html files produced while trouble shooting for a project. Imagine there
are dozens of files. It would be quite painful to remove them one by one. In Unix, we can
actually write an expression that means all the files that end in .html. To do this we type
wild card: *. As discussed in the data wrangling part of this book, this character means any
number of any combination of characters. Specifically, to list all html files, we would type:"
ls *.html
rm *.html

"The other useful wild card is the ? symbol. This means any single character. So if all the
files we want to erase have the form file-001.html with the numbers going from 1 to 999,
we can type:"
rm file-???.html

"We can combine wild cards. For example, to remove all files with the name file-001
regardless of suffix, we can type:"
rm file-001.*

# Warning: Combining rm with the * wild card can be dangerous. There are
# combinations of these commands that will erase your entire filesystem without
# asking “are you sure?”. Make sure you understand how it works before using
# this wild card with the rm command.

"Unix has settings that affect your command line environment. These are called environment
variables. The home directory is one of them. We can actually change some of these. In Unix,
variables are distinguished from other entities by adding a $ in front. The home directory
is stored in $HOME."
# Earlier we saw that echo is the Unix command for print. So we can see our home directory
# by typing:
echo $HOME
# You can see them all by typing:
env

"Much of what we use in this chapter is part of what is called the Unix shell. There are
actually different shells, but the differences are almost unnoticeable. They are also important,
although we do not cover those here. You can see what shell you are using by typing:"
echo $SHELL
# The most common one is bash

"In Unix, all programs are files. They are called executables. So ls, mv and git are all files.
But where are these program files? You can find out using the command which:"
which git
# That directory is probably full of program files. The directory /usr/bin usually holds many
# program files. If you type:
ls ../../usr/bin
"We can create executables and bash will find them if they are in this folder or some others
These folders are listed by:"
$PATH




# Commands you should learn -----------------------------------------------

start filename # open a file with the right application
nano # A bare-bones text editor.
awk/sed # These are two very powerful commands that permit you to find specific strings
        # in files and change them.



# Organizing with R -------------------------------------------------------


"We can also perform file management from within R. The key functions to learn about can
be seen by looking at the help file for ?files. Another useful function is unlink."
?files
#> create, remove, rename, copy...






rm(list = ls())
library(imager)

"• Codeacademy: https://www.codecademy.com/learn/learn-git 
 • GitHub Guides: https://guides.github.com/activities/hello-world/
 • Try Git tutorial: https://try.github.io/levels/1/challenges/1
 • Happy Git and GitHub for the useR: http://happygitwithr.com/"


"I prefer using my mouse and R -> refer to the related section"


# There are three main reasons to use Git and GitHub.

  # 1. Sharing: Even if we do not take advantage of the advanced and powerful version
  # control functionality, we can still use Git and GitHub to share our code. We have
  # already shown how we can do this with RStudio.

  # 2. Collaborating: Once you set up a central repo, you can have multiple people
  # make changes to code and keep versions synched. GitHub provides a free service
  # for centralized repos. GitHub also has a special utility, called a pull request, that
  # can be used by anybody to suggest changes to your code. You can easily either
  # accept or deny the request.

  # 3. Version control: The version control capabilities of Git permit us to keep track
  # of changes we make to our code. We can also revert back to previous versions of
  # files. Git also permits us to create branches in which we can test out ideas, then
  # decide if we merge the new branch with the original.

# Here we focus on the sharing aspects of Git and GitHub and refer the reader to the links
# above to learn more about this powerful tool.


# Connect to github -------------------------------------------------------

git config --global user.name "Your Name"
git config --global user.email "your@email.com"

"To avoid entering the password every time go to Options > Git > create RSA key"



# Repositories ------------------------------------------------------------

"As mentioned, one of the advantages of keeping code on a GitHub repository is that you
can easily share it with potential employers interested in seeing examples of your work.
Because many data science companies use version control systems, like Git, to collaborate
on projects, they might also be impressed that you already know at least the basics."

# You now have your first repo on GitHub. The next step will be to clone it on your computer
# and start editing and syncing using Git.
# To do this, it is convenient to copy the link provided by GitHub specifically to connect to
# this repo, using Git as shown below. We will later need to copy and paste this so make sure
# to remember this step.

"The main actions in Git are to:
  1. pull changes from the remote repo, in this case the GitHub repo
  2. add files, or as we say in the Git lingo stage files
  3. commit changes to the local repo
  4. push changes to the remote repo, in our case the GitHub repo"

# To effectively permit version control and collaboration in Git, files move across four different
# areas:
#   Working directory
#   Staging Areas
#   Local Repository
#   Upstream Repository




# Cloning a repo  ---------------------------------------------------------

"We are going to clone an existing Upstream Repository. You can see it on GitHub here:
https://github.com/rairizarry/murders. By visiting this page, you can see multiple files and
directories. This is the Upstream Repository. By clicking the green clone button, we can
copy the repo’s URL https://github.com/rairizarry/murders.git"

# But what does clone mean? Rather than download all these files to your computer, we
# are going to actually copy the entire Git structure, which means we will add the files and
# directories to each of the three local stages: Working Directory, Staging Area, and Local
# Repository. When you clone, all three are exactly the same to start.
mkdir git-example
cd git-example
git clone https://github.com/rairizarry/murders.git
ls
cd murders
ls

"Now we are going to make changes to these files. Eventually, we want these new versions
of the files to be tracked and synched with the upstream repo. But we don’t want to keep
track of every little change: we don’t want to sync until we are sure these versions are final
enough to share. For this reason, edits in the staging area are not kept by the version control
system."

# To demonstrate, we add a file to the staging area with the git add command. Below we
# create a file using the Unix echo command just as an example (in reality you would use
#                                                               RStudio):
echo "test" >> new-file.txt
echo "temporary" >> tmp.txt

"Now we can stage the file we eventually want to add to our repository:"
git add new-file.txt #> working to staging

# Notice what the status says now:
git status

"Because new-file.txt is staged, the current version of the file will get added to the local
repository next time we commit, which we do as follows:"
git commit -m "adding a new file" #> -m adds a message to specify the changes -> changelog
                                  #> staging to local

# However, if we edit that file again, it changes only in the working directory. To add to the
# local repo, we need to stage it and commit the changes that are added to the local repo:
echo "add text" >> new-file.txt
git add new-file.txt
git commit -m "adding a new line"

# Note that this step is often unnecessary in our uses of Git. We can skip the staging part if
# we add the file name to the commit command like this:
echo "adding a second line" >> new-file.txt
git commit -m "minor change to new-file" new-file.txt

"We can keep track of all the changes we have made with:"
git log new-file.txt

"To keep everything synced, the final step is to push the changes to the upstream repo. This
is done with the git push command like this:"
git push #> local to upstream
# However, in this particular example, you will not be able to do this because you do not have
# permission to edit the upstream repo. If this was your repo, you could.

"If this is a collaborative project, the upstream repo may change and become different than
our version. To update our local repository to be like the upstream repo, we use the command
fetch:"
git fetch #> upstream to local

"And then to make these copies to the staging and working directory areas, we use the
command:"
git merge #> local to staging and working

"However, we often just want to change both with one command. For this, we use:"
git pull #> upstream to all

# We will learn in Section 37.6 how RStudio has buttons to do all this. The details provided
# here should help you understand what happens in the background.





# Initializing a repo -----------------------------------------------------

"Suppose we already have a populated local directory and we want to turn this directory into
a collaborative GitHub repository. The most efficient way of achieving this is by initializing
the local directory."

# We start by creating a new repo on our GitHub page
# I will use the repo I used to save some screenshots of this program 
https://github.com/FractalySyn/harvardXdatascience.git

# Let's create a project in home directory (R hd is Documents)
mkdir data_projects
cp -r git-example/ data_projects/
cd data_projects/git-example
mkdir testforgit
cd testforgit
mkdir data rda works help sources
echo "Important infos" >> README.txt

# We then intialize the directory. This turns the directory into a Git directory and Git starts tracking:
git init

"All the files are now only in our working directory; no files are in our local repo or on
GitHub.
The next step is to connect the local repo with the GitHub repo. In a previous example, we
had RStudio do this for us. Now we need to do it ourselves. We can by adding any of the
files and committing it:"
git add README.txt
git commit -a

"We now have a file in our local repo and can connect it to the upstream repo"
git remote add origin https://github.com/FractalySyn/harvardXdatascience.git
git push




# Git with R --------------------------------------------------------------

"Now we are ready to start an RStudio project that uses version control and stores the code
on a GitHub repo. To do this, we start a project but, instead of New Directory, we will
select Version Control and then we will select Git as our version control system"
new project -> Version Control -> Git

"Use the URL of an existing GitHub repo and select the local location"
"You will see on the top right corner the name and type of project as well as
a new tab on the upper right pane titled Git."

# In RStudio, the status of the file as it relates to the remote and local repos are represented
# in the status symbols with colors. A yellow square means that Git knows nothing about
# this file. To sync with the GitHub repo, we need to add the file, then commit the change to
# our local Git repo, then push the change to the GitHub repo. Right now, the file is just on
# our computer. To add the file using RStudio, we click the Stage box. You will see that the
# status icon now changes to a green A.

"Now we are ready to commit the file to our local repo. In RStudio, we can use the Commit
button. This will open a new dialog window. With Git, whenever we commit a change, we
are required to enter a comment describing the changes being committed."

# There are buttons to commit, push..., to access branches

"Once we hit the commit button, we should see a message from Git with a summary of the
changes that were committed. Now we are ready to push these changes to the GitHub repo.
We can do this by clicking on the Push button on the top right corner:"

# The file commited is no longer in the Git window until we modify it

"If we now visit our repo on the web, we will see that it matches our local copy."



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










