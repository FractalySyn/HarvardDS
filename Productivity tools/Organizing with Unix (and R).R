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







