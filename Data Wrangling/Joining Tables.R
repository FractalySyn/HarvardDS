rm(list = ls())
options(digits = 4)
library(imager); library(gtools); library(tidyverse); library(dslabs)
library(ggplot2); library(ggrepel)


# Joins -------------------------------------------------------------------

"The join functions in the dplyr package make sure that the tables are combined so that matching rows are together. If you know SQL, you will
 see that the approach and syntax is very similar. The general idea is that one needs to identify one or more columns that will serve to match 
 the two tables."
data("murders"); data("polls_us_election_2016")
tab = left_join(murders, results_us_election_2016, by = "state") %>%
  select(-others) %>%
  rename(ev = electoral_votes)
head(tab)

# The data has been successfully joined and we can now, for example, make a plot to explore the relationship:
tab %>%
  ggplot(aes(population/10^6, ev, label = abb)) +
  geom_point() +
  geom_text_repel() +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm", se = T) # add a regression line
# We see the relationship is close to linear with about 2 electoral votes for every million persons, but with very small states getting 
# higher ratios.


"In practice, it is not always the case that each row in one table has a matching row in the other. For this reason, we have several versions
 of join. To illustrate this challenge, we will take subsets of the tables above. We create the tables tab1 and tab2 so that they have some 
 states in common but not all:"
tab1 = slice(murders, 1:6) %>% select(state, population); tab1
tab2 = results_us_election_2016 %>% 
  filter(state%in%c("Alabama", "Alaska", "Arizona", 
                    "California", "Connecticut", "Delaware")) %>% 
  select(state, electoral_votes) %>% rename(ev = electoral_votes); tab2

# Suppose we want a table like tab_1, but adding electoral votes to whatever states we have available. For this, we use left_join with tab_1
# as the first argument. We specify which column to use to match with the by argument.
left_join(tab1, tab2, by = "state")
# NAs are added to non available ev

"If instead of a table with the same rows as first table, we want one with the same rows as second table, we can use right_join:"
right = right_join(tab1, tab2, by = "state"); right
# Now the NAs are in the column coming from tab_1.
"left_join and right_join are interchangeable"
left = left_join(tab2, tab1, "state") %>% select(state, population, ev); left
right == left

"If we want to keep only the rows that have information in both tables, we use inner_join. You can think of this as an intersection:"
inner_join(tab1, tab2, by = "state")

"If we want to keep all the rows and fill the missing parts with NAs, we can use full_join. You can think of this as a union:"
full_join(tab1, tab2, by = "state")

"The semi_join function  keep the part of first table for which we have information in the second. It doesn't add the columns of the second:"
semi_join(tab1, tab2, by = "state")

"The anti_join is the opposite of semi_join. It keeps the elements of the first table for which there is no information in the second:"
anti_join(tab1, tab2, by = "state")

"The following diagram summarizes the above joins:"
plot(load.image("https://rafalab.github.io/dsbook/wrangling/img/joins.png"))






# Binding -----------------------------------------------------------------

"Although we have yet to use it in this book, another common way in which datasets are combined is by binding them. Unlike the join function,
 the binding functions do not try to match by a variable, but instead simply combine datasets. If the datasets don’t match by the appropriate 
 dimensions, one obtains an error."

"The dplyr function bind_cols binds two objects by making them columns in a tibble."
bind_cols(a = 1:3, b = 4:6)
# This function requires that we assign names to the columns. Here we chose a and b
# Note that there is an R-base function cbind with the exact same functionality. An important difference is that cbind() can create different 
# types of objects, while bind_cols always produces a data frame.

"bind_cols can also bind two different data frames. For example, here we break up the tab data frame and then bind them back together:"
tab1 = tab[,1:3]; tab2 = tab[, 4:6]; tab3 = tab[, 7:8]
new_tab = bind_cols(tab1, tab2, tab3); head(new_tab)
mean(new_tab == tab) # 100% matching

"The bind_rows function is similar to bind_cols, but binds rows instead of columns:" # equivalent to rbind()
tab1 = tab[1:2,]; tab2 = tab[3:6,]
bind_rows(tab1, tab2)




# Set operators -----------------------------------------------------------

"Another set of commands useful for combining datasets are the set operators. When applied to vectors, these behave as their names suggest. 
 Examples are intersect, union, setdiff, and setequal. However, if the tidyverse, or more specifically dplyr, is loaded, these functions 
 can be used on data frames as opposed to just on vectors." #> base package => vectors only

"You can take intersections of vectors of any type:"
base::intersect(1:10, 6:15)
base::intersect(c("a","b","c"), c("b","c","d"))
"This function (dplyr) also returns the rows in common between two tables."
tab1 = tab[1:5,]; tab2 = tab[3:7,]
dplyr::intersect(tab1, tab2)

"Similarly union takes the union of vectors. For example:"
union(1:10, 5:15) # returns all values without repeating them (intersection)
"For tables rows"
dplyr::union(tab1, tab2)

"The set difference between a first and second argument can be obtained with setdiff. Unlike intersect and union, this function is not 
 symmetric:"
setdiff(1:10, 6:15) # returns all values of the first vector that are not in the second
#> [1] 1 2 3 4 5
setdiff(6:15, 1:10)
#> [1] 11 12 13 14 15
"For tables"
tab1 = tab[1:5,]; tab2 = tab[3:7,]
dplyr::setdiff(tab1, tab2) # 1st and 2nd line

"Finally, the function setequal tells us if two sets are the same, regardless of order. So notice that:"
setequal(1:5, 2:5)
setequal(1:6, c(1, 4, 5, 2, 6, 3))






# Assessment --------------------------------------------------------------

"Q1 You have created data frames tab1 and tab2 of state population and election data for which"
tab1 = data.frame(state = c("Alabama", "Alaska", "Arizona", "Delaware", "DC"), 
                  population = c(4780, 710, 6392, 898, 601))
tab2 = data.frame(state = c("Alabama", "Alaska", "Arizona", "California", "Colorado", "Connecticut"),
                  ev = c(9, 3, 11, 55, 9, 7))
dim(tab1)
# [1] 5 2
dim(tab2)
# [1] 6 2
"What are the dimensions of the table dat, created by the following command?"
dat = left_join(tab1, tab2, by = "state"); dat # keep tab1 rows
dim(dat)

"Q2 We are still using the tab1 and tab2 tables shown in question 1. What join command would create a 
 new table “dat” with three rows and two columns?"
right_join(tab1, tab2, by = "state") # keep tab2 rows
full_join(tab1, tab2, by = "state") # all rows
inner_join(tab1, tab2, by = "state") # rows that have all informations (no NAs)
semi_join(tab1, tab2, by = "state") # rows of tab1 for which we have tab2 info

"Q3 Which of the following are real differences between the join and bind functions?"
all
# Binding functions combine by position, while join functions match by variables
# Joining functions can join datasets of different dimensions, but the bind functions must match on the appropriate dimension 
# Bind functions can combine both vectors and dataframes, while join functions work for only for dataframes
# The join functions are a part of the dplyr package and have been optimized for speed, while the bind functions are inefficient base functions

"Q4 We have two simple tables, shown below, with columns x and y:"
# > df1
# x     y    
# a     a    
# b     a    
# > df2
# x     y    
# a     a    
# a     b  
"Which command would result in the following table?"
# > final
# x     y    
# b     a   
df1 = data.frame(x = c("a", "b"), y = c("a", "a")); df1
df2 = data.frame(x = c("a", "a"), y = c("a", "b")); df2
dplyr::union(df1, df2)
dplyr::setdiff(df1, df2)

"The Batting data frame contains the offensive statistics for all baseball players over several seasons.  
 Filter this data frame to define top as the top 10 home run (HR) hitters in 2016:"
library(Lahman)
top = Batting %>%
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%
  slice(1:10) %>%
  as_tibble()
head(top)
head(Master)

"Q5 Use the correct join or bind function to create a combined table of the names and statistics of the top 10
 home run (HR) hitters for 2016. This table should have the player ID, first name, last name, and number of HR
 for the top 10 players. Name this data frame top_names."
top_names = top %>%
  left_join(Master, by = "playerID") %>%
  select(playerID, nameFirst, nameLast, HR)
top_names

"Q6 Inspect the Salaries data frame. Filter this data frame to the 2016 salaries, then use the correct bind 
 join function to add a salary column to the top_names data frame from the previous question. Name the new 
 data frame top_salary. Use this code framework:"
top_salary = Salaries %>%
  filter(yearID == 2016) %>%
  right_join(top_names, by = "playerID") %>%
  select(nameFirst, nameLast, teamID, HR, salary)
top_salary

"Q7 Inspect the AwardsPlayers table. Filter awards to include only the year 2016."
head(AwardsPlayers)
ap = AwardsPlayers %>%
  filter(yearID == 2016); ap
"How many players from the top 10 home run hitters won at least one award in 2016?"
top_names %>% 
  select(playerID) %>%
  intersect(select(ap, playerID))
"How many players won an award in 2016 but were not one of the top 10 home run hitters in 2016?"
ap %>% 
  select(playerID) %>%
  setdiff(select(top_names, playerID)) %>%
  nrow()

