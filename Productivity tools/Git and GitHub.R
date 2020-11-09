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




