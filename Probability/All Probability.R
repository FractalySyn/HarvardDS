rm(list = ls())
options(digits = 4)
library(imager); library(gtools); library(tidyverse); library(ggplot2)

# Discrete Probability ----------------------------------------------------

## We start by covering some basic principles related to categorical data. The subset of probability is referred to as discrete 
## probability. It will help us understand the probability theory we will later introduce for numeric and continuous data, 
## which is much more common in data science applications. Discrete probability is more useful in card games and therefore we 
## use these as examples.

"Relative frequency"
## A more tangible way to think about the probability of an event is as the proportion of times the event occurs when we repeat
## the experiment an infinite number of times, independently, and under the same conditions



# Monte Carlo simulations for categorical data ----------------------------

## Computers provide a way to actually perform the simple random experiment described above: pick a bead at random from a bag 
##that contains three blue beads and two red ones. Random number generators permit us to mimic the process of picking at random.

## An example is the sample function in R. We demonstrate its use in the code below. First, we use the function rep to generate
## the urn:
beads = rep(c("red", "blue"), times = c(2, 3))
sample(beads, 1) # pick one bead

## This line of code produces one random outcome. We want to repeat this experiment an infinite number of times, but it is 
## impossible to repeat forever. Instead, we repeat the experiment a large enough number of times to make the results 
## practically equivalent to repeating forever. 
"This is an example of a Monte Carlo simulation"

## To perform our first Monte Carlo simulation, we use the replicate function, which permits us to repeat the same task any 
## number of times. Here, we repeat the random event 10000 times
events = replicate(10000, sample(beads, 1))
table(events)
# -> table uses the cross-classifying factors to build a contingency table of the counts at each combination of factor levels
prop.table(table(events))
# -> and prop.table gives us the proportions

"Although this is a simple and not very useful example, we will use Monte Carlo simulations to estimate probabilities in cases 
 in which it is harder to compute the exact ones"


"Setting the random seed"
## Throughout this book, we use random number generators. This implies that many of the results presented can actually change
## by chance, which then suggests that a frozen version of the book may show a different result than what you obtain when you 
## try to code as shown in the book. This is actually fine since the results are random and change from time to time. However,
## if you want to ensure that results are exactly the same every time you run them, you can set R's random number generation
## seed to a specific number. Above we set it to 1986. We want to avoid using the same seed everytime. A popular way to pick 
## the seed is the year - month - day. For example, we picked 2008 on April 08, 2020:  2020 - 08 - 04 = 2008
set.seed(2008)



"Replacement"
## The function sample has an argument that permits us to pick more than one element from the urn. However, by default, this
## selection occurs without replacement: after a bead is selected, it is not put back in the bag. Notice what happens when we 
## ask to randomly select five beads:
sample(beads, 5)
#> [1] "red"  "blue" "blue" "blue" "red"
sample(beads, 5)
#> [1] "red"  "red"  "blue" "blue" "blue"
sample(beads, 5)
#> [1] "blue" "red"  "blue" "red"  "blue"

## This results in rearrangements that always have three blue and two red beads. If we ask that six beads be selected, we get 
## an error:
sample(beads, 6)


"However, the sample function can be used directly, without the use of replicate, to repeat the same experiment of picking 1 
 out of the 5 beads, continually, under the same conditions. To do this, we sample with replacement: return the bead back to
 the urn after selecting it. We can tell sample to do this by changing the replace argument"
events = sample(beads, 10000, replace = T)
prop.table(table(events))




# Independence ------------------------------------------------------------

## We say two events are independent if the outcome of one does not affect the other. The classic example is coin tosses. 
## Every time we toss a fair coin, the probability of seeing heads is 1/2 regardless of what previous tosses have revealed.
## The same is true when we pick beads from an urn with replacement. In the example above, the probability of red is 0.40 
## regardless of previous draws.



# Conditional probabilities -----------------------------------------------

## When events are not independent, conditional probabilities are useful. We already saw an example of a conditional probability: 
## we computed the probability that a second dealt card is a King given that the first was a King. In probability, we use the 
## following notation:
"P(Card2 is a King | Card1 is a King) = 3/51"
## When two events, say A and B, are independent, we have:
"P(A | B) = P(A)"
## This is the mathematical way of saying: the fact that B happened does not affect the probability of A happening




# Addition and multiplication rules ---------------------------------------

"Multiplication rule - Intersection"
## If we want to know the probability of two events, say A and B, occurring, we can use the multiplication rule:
"P(A & B) = P(A).P(B | A) = P(B).P(A | B)"
## So, in a Blackjack game, to calculate the chances of getting a 21 by drawing an Ace and then a face card, we compute the 
## probability of the first being an Ace and multiply by the probability of drawing a face card or a 10 given that the first 
## was an Ace:
(1/13) * (16 / 51)

"Multiplication rule under independence"
"P(A & B & C) = P(A).P(B).P(C)"


"Conditional probabilities"
## Found by solving the equation above 
"P(A | B) = P(A & B) / P(B)"

"Addition rule - Reunion"
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/venn-diagram-addition-rule-1.png"))
"P(A or B) = P(A) + P(B) - P(A & B)"




# Combinations and permutations -------------------------------------------

## For more complicated cases, the computations are not as straightforward. For instance, what is the probability that if I 
## draw five cards without replacement, I get all cards of the same suit, what is known as a "flush" in poker? In a discrete 
## probability course you learn theory on how to make these computations. Here we focus on how to use R code to compute the answers.

## First, let's construct a deck of cards. For this, we will use the expand.grid and paste functions. We use paste to create 
## strings by joining smaller strings. To do this, we take the number and suit of a card and create the card name like this:
number = "Five"; suit = "Hearts"
paste(number, suit)
## paste also works on pairs of vectors performing the operation element-wise:
paste(letters[1:10], as.character(1:10))

## The function expand.grid gives us all the combinations of entries of two vectors. For example, if you have blue and black 
## pants and white, grey, and plaid shirts, all your combinations are: 
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))

"Here is how to generate a deck of cards"
suits = c("Diamonds", "Clubs", "Hearts", "Spades")
numbers = c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", 
             "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck = expand.grid(number = numbers, suit = suits) ## returns all possible combinations 
deck = paste(deck$number, deck$suit) ## returns all cards

## With the deck constructed, we can double check that the probability of a King in the first card is 1/13 by computing the 
## proportion of possible outcomes that satisfy our condition:
kings = paste("King", suits)
sum(deck %in% kings) / length(deck)


## Now, how about the conditional probability of the second card being a King given that the first was a King? Earlier, we 
## deduced that if one King is already out of the deck and there are 51 left, then this probability is 3/51. Let's confirm 
## by listing out all possible outcomes.

## To do this, we can use the permutations function from the gtools package. For any list of size n, this function computes 
## all the different combinations we can get when we select r items. Here are all the ways we can choose two numbers from a 
## list consisting of 1,2,3:
"FR Arrangements (avec ordre) sans répétition"; "n! / (n-p)!"
permutations(3, 2) # 2 among 3 without replacement
"Notice that the order matters here: 3,1 is different than 1,3. Also, note that (1,1), (2,2), and (3,3) do not appear because 
 once we pick a number, it can't appear again."

## Optionally, we can add a vector. If you want to see five random seven digit phone numbers out of all possible phone numbers 
## (without repeats), you can type:
allphonenumbers = permutations(10, 7, v = 0:9) # default is 1:n (here n is 10 and we prefer 0:(n-1))
n = nrow(allphonenumbers); index = sample(n, 5) # returns 5 random indexes in the list
allphonenumbers[index, ]

## To compute all possible ways we can choose two cards when the order matters, we type:
hands = permutations(52, 2, v = deck) # 52! / 50!

## This is a matrix with two columns and 2652 rows. With a matrix we can get the first and second cards like this:
card1 = hands[,1]; card2 = hands[,2]
## We have a king as the first card 2652 / 13 kings times = 204
sum(card1 %in% kings) / length(card1)

## To get the conditional probability of having a king given we have already one "P(A | B) = P(A & B) / P(A)"
sum(card1 %in% kings & card2 %in% kings) / sum(card1 %in% kings) # which is exactly 3/51
## The probability of getting two kings is "P(A & B) = P(A).P(B | A)"
sum(card1 %in% kings & card2 %in% kings) / length(hands)
## equivalent to
mean(card1 %in% kings & card2 %in% kings) 



## How about if the order doesn't matter? For example, in Blackjack if you get an Ace and a face card in the first draw, it is
## called a Natural 21 and you win automatically. If we wanted to compute the probability of this happening, we would enumerate 
## the combinations, not the permutations, since the order does not matter.
"FR Combinaisons (l'ordre ne compte pas) sans répétition"; "n! / (n-p)!p!"
combinations(3, 2) # (1,2) = (2,1)
## So to compute the probability of a Natural 21 in Blackjack, we can do this:
aces = paste("Ace", suits)
facecard = c("King", "Queen", "Jack", "Ten")
facecard = expand.grid(number = facecard, suit = suits)
facecard = paste(facecard$number, facecard$suit)

hands = combinations(52, 2, v = deck)
mean(hands[,1] %in% aces & hands[,2] %in% facecard)
"In the last line, we assume the Ace comes first. This is only because we know the way combination enumerates possibilities and it
 will list this case first. But to be safe, we could have written this and produced the same answer:"
mean((hands[,1] %in% aces & hands[,2] %in% facecard) | (hands[,2] %in% aces & hands[,1] %in% facecard))
"Indeed the second part is = to 0"
mean(hands[,2] %in% aces & hands[,1] %in% facecard)




# Monte Carlo example -----------------------------------------------------

"Instead of using combinations to deduce the exact probability of a Natural 21, we can use a Monte Carlo to estimate this probability"
## In this case, we draw two cards over and over and keep track of how many 21s we get. We can use the function sample to draw two 
## cards without replacements:"
hand = sample(deck, 2); hand
(hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
## If we repeat this 100,000 times, we get a very good approximation of the probability of a Natural 21.
blackjack = function()
{
  hand = sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
}
results = replicate(100000, blackjack())
mean(results)
#> [1] 0.048 ~ 0.0483



# Monty Hall problem ------------------------------------------------------

## We can use probability to show that if you stick with the original door choice, your chances of winning a prize remain 1 in 3.
## However, if you switch to the other door, your chances of winning double to 2 in 3! This seems counterintuitive. Many people 
## incorrectly think both chances are 1 in 2 since you are choosing between 2 options.
monty_hall = function(strategy)
{
  doors = as.character(1:3); prize = c("car", "goat", "goat")
  prize_door = doors[prize == "car"] # assign to lot to a door
  my_pick = sample(doors, 1) # I choose one door randomly
  
  show = sample(doors[!doors %in% c(my_pick, prize_door)], 1) # he shows a losing door other than mine
  
  stick_strategy = my_pick
  
  switch_strategy = doors[!doors %in% c(my_pick, show)]
  
  choice = ifelse(strategy == "stick", stick_strategy, switch_strategy)
  choice == prize_door
}

replicate(100000, monty_hall("stick")) %>% mean() # tends to 1/3
replicate(100000, monty_hall("switch")) %>% mean() # tends to 2/3




# Birthdat problem --------------------------------------------------------

## Suppose you are in a classroom with 50 people. If we assume this is a randomly selected group of 50 people, what is the chance 
## that at least two people have the same birthday? 
## First, note that birthdays can be represented as numbers between 1 and 365, so a sample of 50 birthdays can be obtained like this:
bdays = sample(365, 50, replace = T)

## To check if in this particular set of 50 people we have at least two with the same birthday, we can use the function duplicated, 
## which returns TRUE whenever an element of a vector appears more than one time. Here is an example:
duplicated(bdays)
duplicated(c(1,1,1))
## It's what we want here to know if at least two people have the same bday 
any(duplicated(bdays))

## To estimate the probability of a shared birthday in the group, we repeat this experiment by sampling sets of 50 birthdays over 
## and over:
same_birthday  = function(n)
{
  bdays = sample(1:365, n, replace = T)
  any(duplicated(bdays))
}
results = replicate(100000, same_birthday(50))
mean(results) # 0.97

## Say we want to use this knowledge to bet with friends about two people having the same birthday in a group of people. When are the
## chances larger than 50%? Larger than 75%?
compute_proba = function(n, rep = 10000)
{
  results = replicate(rep, same_birthday(n))
  mean(results)
}
n = seq(1, 60)
prob = sapply(n, compute_proba)

## We can now make a plot of the estimated probabilities of two people having the same birthday in a group of size n
qplot(n, prob)


"Mathematical computation"
## Now let's compute the exact probabilities rather than use Monte Carlo approximations. Not only do we get the exact answer using 
## math, but the computations are much faster since we don't have to generate experiments.

## To make the math simpler, instead of computing the probability of it happening, we will compute the probability of it not happening.
## For this, we use the multiplication rule.

## Let's start with the first person. The probability that person 1 has a unique birthday is 1. The probability that person 2 has a 
## unique birthday, given that person 1 already took one, is 364/365. Then, given that the first two people have unique birthdays, 
## person 3 is left with 363 days to choose from. We continue this way and find the chances of all 50 people having a unique birthday
## is:
"1 x (364 / 365) x ... x ((365-(n-1)) / 365)"
## We can write a function that does this for any number:
exact_prob = function(n)
{
  prob_unique = seq(365, 365-(n-1)) / 365
  1 - prod(prob_unique)
}
n = 1:60
ex_prob = sapply(n, exact_prob)
qplot(n, prob) + geom_line(aes(n, ex_prob), col = "red")
"This plot shows that the Monte Carlo simulation provided a very good estimate of the exact probability. Had it not been possible to 
 compute the exact probabilities, we would have still been able to accurately estimate the probabilities."





# Infinity in practice ----------------------------------------------------

"The theory described here requires repeating experiments over and over forever. In practice we can't do this. In the examples above,
 we used REP = 10,000 Monte Carlo experiments and it turned out that this provided accurate estimates. The larger this number, the more
 accurate the estimate becomes until the approximaton is so good that your computer can't tell the difference. But in more complex 
 calculations, 10,000 may not be nearly enough. Also, for some calculations, 10,000 experiments might not be computationally feasible.
 In practice, we won't know what the answer is, so we won't know if our Monte Carlo estimate is accurate. We know that the larger REP,
 the better the approximation. But how big do we need it to be? This is actually a challenging question and answering it often requires
 advanced theoretical statistics training."

## One practical approach we will describe here is to check for the stability of the estimate. The following is an example with the 
## birthday problem for a group of 25 people.
rep = 10^seq(1, 5, len = 100); rep
compute_prob = function(rep, n = 25)
{
  same_day = replicate(rep, same_birthday(n))
  mean(same_day)
}
prob = sapply(rep, compute_prob)
qplot(log10(rep), prob, geom = "line")
## In this plot, we can see that the values start to stabilize (that is, they vary less than .01) around 1000. Note that the exact 
## probability, which we know in this case, is 0.569.

"So the purpose here is to compute estimated probabilities for different number of repetitions (we compared a hundred Monte Carlo 
 experiments with reps from 10 to 100000). Plotting these probas help us find a threshold from which the estimated probabilities start
 to converge closely to the real probability of the event"





# DataCamp Assessment --------------------------------------------------------------


colors = c(rep("cyan", 3), rep("magenta", 5), rep("yellow", 7))
color_pick = function()
{
  a = sample(colors, 2)
  res = (a[1] == "cyan" & a[2] != "cyan")
  res
}
results = replicate(1000000, color_pick())
mean(results)


# This line of example code simulates four independent random games where the Celtics either lose or win. Copy this example code to 
## use within the `replicate` function.
simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
# The variable 'B' specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)
## Create an object called `celtic_wins` that replicates two steps for B iterations: (1) generating a random four-game series 
## `simulated_games` using the example code, then (2) determining whether the simulated series contains at least one win for 
## the Celtics.
celtic_wins = replicate(B, any(sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))=="win"))
mean(celtic_wins)


"Two teams, say the Cavs and the Warriors, are playing a seven game championship series. The first to win four games wins the series. 
 The teams are equally good, so they each have a 50-50 chance of winning each game. 
 If the Cavs lose the first game, what is the probability that they win the series?"
# Assign a variable 'n' as the number of remaining games.
n = 6
# Assign a variable `outcomes` as a vector of possible game outcomes, where 0 indicates a loss and 1 indicates a win for the Cavs.
outcomes = c(0,1)
# Assign a variable `l` to a list of all possible outcomes in all remaining games. Use the `rep` function on `list(outcomes)` to 
# create list of length `n`.
l = rep(list(outcomes), n)
# Create a data frame named 'possibilities' that contains all combinations of possible outcomes for the remaining games.
possibilities = expand.grid(l)
# Create a vector named 'results' that indicates whether each row in the data frame 'possibilities' contains enough wins for the Cavs
# to win the series.
results = rowSums(possibilities) >= 4
mean(results)


"Confirm the results of the previous question with a Monte Carlo simulation to estimate the probability of the Cavs winning the series 
 after losing the first game."
# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)
# Create an object called `results` that replicates for `B` iterations a simulated series and determines whether that series contains
## at least four wins for the Cavs.
results = replicate(B, sum(sample(c(0, 1), 6, replace = TRUE, prob = c(0.5, 0.5))) >= 4)
results
# Calculate the frequency out of `B` iterations that the Cavs won at least four games in the remainder of the series. Print your 
# answer to the console.
mean(results)



"Team A has a p>0.5 to win each game"
# Let's assign the variable 'p' as the vector of probabilities that team A will win.
p <- seq(0.5, 0.95, 0.025)
# Given a value 'p', the probability of winning the series for the underdog team B can be computed with the following function based 
## on a Monte Carlo simulation:
prob_win <- function(p){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), 7, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=4
  })
  mean(result)
}
# Apply the 'prob_win' function across the vector of probabilities that team A will win to determine the probability that team B will
# win. Call this object 'Pr'.
Pr = sapply(p, prob_win)
# Plot the probability 'p' on the x-axis and 'Pr' on the y-axis.
plot(p, Pr)




"Repeat the previous exercise, but now keep the probability that team A wins fixed at p <- 0.75 and compute the probability for 
 different series lengths. For example, wins in best of 1 game, 3 games, 5 games, and so on through a series that lasts 25 games."
# Given a value 'p', the probability of winning the series for the underdog team B can be computed with the following function based 
## on a Monte Carlo simulation:
prob_win <- function(N, p=0.75){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), N, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=(N+1)/2
  })
  mean(result)
}
# Assign the variable 'N' as the vector of series lengths. Use only odd numbers ranging from 1 to 25 games.
N = seq(1, 25, 2)
# Apply the 'prob_win' function across the vector of series lengths to determine the probability that team B will win. Call this 
## object `Pr`.
Pr = sapply(N, prob_win)
plot(N,Pr)




# edX Assessment ----------------------------------------------------------


"In the 200m dash finals in the Olympics, 8 runners compete for 3 medals (order matters). In the 2012 Olympics, 3 of the 8 runners 
 were from Jamaica and the other 5 were from different countries. The three medals were all won by Jamaica (Usain Bolt, Yohan Blake,
 and Warren Weir)."
# How many different ways can the 3 medals be distributed across 8 runners?
factorial(8) / factorial(8-3)
permutations(8, 3)[,1] %>% length()
# How many different ways can the three medals be distributed among the 3 runners from Jamaica?
factorial(3)
# What is the probability that all 3 medals are won by Jamaica?
factorial(3) / (factorial(8) / factorial(8-3))
# Run a Monte Carlo simulation on this vector representing the countries of the 8 runners in this race:
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
# For each iteration of the Monte Carlo simulation, within a replicate() loop, select 3 runners representing the 3 medalists and check 
# whether they are all from Jamaica. Repeat this simulation 10,000 times. Set the seed to 1 before running the loop.
set.seed(1)
results = replicate(10000, sum(sample(runners, 3) == "Jamaica") == 3)
mean(results)



"A restaurant manager wants to advertise that his lunch special offers enough choices to eat different meals every day of the year. 
 He doesn't think his current special actually allows that number of choices, but wants to change his special if needed to allow at 
 least 365 choices.
 A meal at the restaurant includes 1 entree, 2 sides, and 1 drink. He currently offers a choice of 1 entree from a list of 6 options, 
 a choice of 2 different sides from a list of 6 options, and a choice of 1 drink from a list of 2 options."
# How many meal combinations are possible with the current menu?
a = length(combinations(6, 1)[,1]) * length(combinations(6, 2)[,1]) * length(combinations(2, 1)[,1]); a 
a == (factorial(6) / factorial(5)) * (factorial(6) / factorial(4) / factorial(2)) * factorial(2)
a == 6 * (6*5)/2 * 2; a == 6*15*2
# How many combinations are possible if he expands his original special to 3 drink options?
6 * 15 * factorial(3)/factorial(2)
# How many meal combinations are there if customers can choose from 6 entrees, 3 drinks, and select 3 sides from the current 6 options?
6 * (factorial(6) / factorial(3)^2) * 3
# Write a function that takes a number of entree choices and returns the number of meal combinations possible given that number of 
# entree options, 3 drink choices, and a selection of 2 sides from 6 options.
meals = function(entree_choices)
{
  entrees = length(combinations(entree_choices, 1)[,1])
  sides = length(combinations(6, 2)[,1])
  drinks = length(combinations(3, 1)[,1])
  entrees*sides*drinks
}
# Use sapply() to apply the function to entree option counts ranging from 1 to 12. What is the minimum number of entree options required
# in order to generate more than 365 combinations?
choices = 2:12
results = sapply(choices, meals); data.frame(choices, results)
plot(choices, results)
# Write a function that takes a number of side choices and returns the number of meal combinations possible given 6 entree choices, 3 
# drink choices, and a selection of 2 sides from the specified number of side choices."
meals2 = function(side_choices)
{
  entrees = length(combinations(6, 1)[,1])
  sides = length(combinations(side_choices, 2)[,1])
  drinks = length(combinations(3, 1)[,1])
  entrees*sides*drinks
}
results2 = sapply(choices, meals2); data.frame(choices, results2)



"Case-control studies help determine whether certain exposures are associated with outcomes such as developing cancer. The built-in 
 dataset esoph contains data from a case-control study in France comparing people with esophageal cancer (cases, counted in ncases) 
 to people without esophageal cancer (controls, counted in ncontrols) that are carefully matched on a variety of demographic and 
 medical characteristics. The study compares alcohol intake in grams per day (alcgp) and tobacco intake in grams per day (tobgp) 
 across cases and controls grouped by age range (agegp)."
data("esoph"); head(esoph)
# How many groups are in the study?
str(esoph)
# How many cases are there?
all_cases = length(esoph[,1]) * mean(esoph$ncases); all_cases
# How many controls are there?
all_controls = length(esoph[,1]) * mean(esoph$ncontrols); all_controls
# What is the probability that a subject in the highest alcohol consumption group is a cancer case?
highest_alcohol = filter(esoph, alcgp == "120+")
cancer_prob = sum(highest_alcohol$ncases) / (sum(highest_alcohol$ncases) + sum(highest_alcohol$ncontrols))
# What is the probability that a subject in the lowest alcohol consumption group is a cancer case?
lowest_alcohol = filter(esoph, alcgp == "0-39g/day")
cancer_prob2 = sum(lowest_alcohol$ncases) / (sum(lowest_alcohol$ncases) + sum(lowest_alcohol$ncontrols))
# Given that a person is a case, what is the probability that they smoke 10g or more a day?
ten_g_smokers = filter(esoph, tobgp != "0-9g/day")
length(ten_g_smokers[,1]) * mean(ten_g_smokers$ncases) / all_cases
# Given that a person is a control, what is the probability that they smoke 10g or more a day?
length(ten_g_smokers[,1]) * mean(ten_g_smokers$ncontrols) / all_controls
# For cases, what is the probability of being in the highest alcohol group?
sum(highest_alcohol$ncases) / all_cases
# For cases, what is the probability of being in the highest tobacco group?
highest_tb = filter(esoph, tobgp == "30+")
sum(highest_tb$ncases) / all_cases
# For cases, what is the probability of being in the highest alcohol group and the highest tobacco group?
highest_both = filter(esoph, tobgp == "30+" & alcgp == "120+")
sum(highest_both$ncases) / all_cases
# For cases, what is the probability of being in the highest alcohol group or the highest tobacco group?
sum(highest_tb$ncases) / all_cases + sum(highest_alcohol$ncases) / all_cases - sum(highest_both$ncases) / all_cases # reunion
# For controls, what is the probability of being in the highest alcohol group?
sum(highest_alcohol$ncontrols) / all_controls
# How many times more likely are cases than controls to be in the highest alcohol group?
(sum(highest_alcohol$ncases) / all_cases) / (sum(highest_alcohol$ncontrols) / all_controls)
# For controls, what is the probability of being in the highest tobacco group?
sum(highest_tb$ncontrols) / all_controls
# For controls, what is the probability of being in the highest alcohol group and the highest tobacco group?
sum(highest_both$ncontrols) / all_controls
# For controls, what is the probability of being in the highest alcohol group or the highest tobacco group?
sum(highest_alcohol$ncontrols) / all_controls + sum(highest_tb$ncontrols) / all_controls - sum(highest_both$ncontrols) / all_controls
# How many times more likely are cases than controls to be in the highest alcohol group or the highest tobacco group?
(sum(highest_tb$ncases) / all_cases + sum(highest_alcohol$ncases) / all_cases - sum(highest_both$ncases) / all_cases) / 
  (sum(highest_alcohol$ncontrols) / all_controls + sum(highest_tb$ncontrols) / all_controls - sum(highest_both$ncontrols) / all_controls)











rm(list = ls())
options(digits = 4)
library(imager); library(gtools); library(tidyverse); library(ggplot2)
library(dslabs)



# CDF ---------------------------------------------------------------------


## Just as when using distributions to summarize numeric data, it is much more practical to define a function that operates on intervals 
## rather than single values. The standard way of doing this is using the cumulative distribution function (CDF).
"CDF = répartition"

## As an example, we earlier defined the height distribution for adult male students. Here, we define the vector x to contain these heights:
data("heights")
x = heights %>% filter(sex == "Male") %>% pull(height)

f = function(a) mean(x <= a) # which, for any value a, gives the proportion of values in the list x that are smaller or equal than a.
##  if I pick one of the male students at random, what is the chance that he is at least 70.5 inches?
1 - f(70.5)



# Theoretical continuous distributions ------------------------------------

## We say that a random quantity is normally distributed with average m and standard deviation s if its probability distribution is 
## defined by:
"F(a) = pnorm(a, m, s)"

## This is useful because if we are willing to use the normal approximation for, say, height, we don't need the entire dataset to answer 
## questions such as: what is the probability that a randomly selected student is taller then 70 inches? We just need the average height 
## and standard deviation:
1 - pnorm(70.5, mean(x), sd(x))


"Theoretical distributions as approximations"
## Data is always, technically speaking, discrete. For example, we could consider our height data categorical with each specific height 
## a unique category. The probability distribution is defined by the proportion of students reporting each height. Here is a plot of that
## probability distribution:
plot(load.image("https://rafalab.github.io/dsbook/book_files/figure-html/plot-of-height-frequencies-1.png"))
## While most students rounded up their heights to the nearest inch, others reported values with more precision. One student reported his
## height to be 69.6850393700787, which is 177 centimeters. The probability assigned to this height is 0.001 or 1 in 812. The probability 
## for 70 inches is much higher at 0.106, but does it really make sense to think of the probability of being exactly 70 inches as being
## different than 69.6850393700787? Clearly it is much more useful for data analytic purposes to treat this outcome as a continuous numeric
## variable, keeping in mind that very few people, or perhaps none, are exactly 70 inches, and that the reason we get more values at 70 is
## because people round to the nearest inch.

## In cases like height, in which the data is rounded, the normal approximation is particularly useful if we deal with intervals that include
## exactly one round number. For example, the normal distribution is useful for approximating the proportion of students reporting values in
## intervals like the following three:
mean(x <= 68.5) - mean(x <= 67.5)
#> [1] 0.115
mean(x <= 69.5) - mean(x <= 68.5)
#> [1] 0.119
mean(x <= 70.5) - mean(x <= 69.5)
#> [1] 0.122
"Note how close we get with the normal approximation:"; m = mean(x); s = sd(x)
pnorm(68.5, m, s) - pnorm(67.5, m, s) 
#> [1] 0.103
pnorm(69.5, m, s) - pnorm(68.5, m, s) 
#> [1] 0.11
pnorm(70.5, m, s) - pnorm(69.5, m, s) 
#> [1] 0.108
"However, the approximation is not as useful for other intervals. otice how the approximation breaks down when we try to estimate:"
mean(x <= 70.9) - mean(x<=70.1)
#> [1] 0.0222
pnorm(70.9, m, s) - pnorm(70.1, m, s)
#> [1] 0.0836
"In general, we call this situation discretization. Although the true height distribution is continuous, the reported heights tend to be
 more common at discrete values, in this case, due to rounding. As long as we are aware of how to deal with this reality, the normal 
 approximation can still be a very useful tool."



# Monte Carlo simulations for continuous variables ------------------------

## how we could generate data that looks like our reported heights:
n = length(x); m = mean(x); s = sd(x)
simulated_heights = rnorm(n, m, s)
hist(simulated_heights, border = "white", col = "black")

"Rnorm() is one of the most useful functions in R as it will permit us to generate data that mimics natural events and answers questions
 related to what could happen by chance by running Monte Carlo simulations."

## If, for example, we pick 800 males at random, what is the distribution of the tallest person? How rare is a seven footer in a group of 
## 800 males? The following Monte Carlo simulation helps us answer that question:
tallest = replicate(10000, {simulated_heights = rnorm(800, m, s)
                            max(simulated_heights)})
mean(tallest >= 7*12) # 1.66 %
hist(tallest, border = "white", col = "black")



# Continuous distributions ------------------------------------------------

"We introduced the normal distribution in Section 8.8 and used it as an example above. The normal distribution is not the only useful
 theoretical distribution. Other continuous distributions that we may encounter are the student-t, Chi-square, exponential, gamma, beta,
 and beta-binomial. R provides functions to compute the density, the quantiles, the cumulative distribution functions and to generate 
 Monte Carlo simulations. R uses a convention that lets us remember the names, namely using the letters d, q, p, and r in front of a 
 shorthand for the distribution."

## Plot the normal density
x <- seq(-5, 5, length.out = 100)
qplot(x, f, geom = "line", data = data.frame(x, f = dnorm(x)))
## dnorm returns the density for quantiles x applying the gaussian formula



# DataCamp Assessment -----------------------------------------------------

"The distribution of IQ scores is approximately normally distributed. The average is 100 and the standard deviation is 15. Suppose you
 want to know the distribution of the person with the highest IQ in your school district, where 10,000 people are born each year.
 Generate 10,000 IQ scores 1,000 times using a Monte Carlo simulation. Make a histogram of the highest IQ scores."
# The variable `B` specifies the number of times we want the simulation to run.
B <- 1000
# Use the `set.seed` function to make sure your answer matches the expected result after random number generation.
set.seed(1)
# Create an object called `highestIQ` that contains the highest IQ score from each random distribution of 10,000 people.
highestIQ = replicate(B, max(rnorm(10000, 100, 15)))
# Make a histogram of the highest IQ scores.
hist(highestIQ)




# edX Assessment ---------------------------------------------------------

"The ACT is a standardized college admissions test used in the United States. The four multi-part questions in this assessment all
 involve simulating some ACT test scores and answering probability questions about them.
 For the three year period 2016-2018, ACT standardized test scores were approximately normally distributed with a mean of 20.9 and 
 standard deviation of 5.7. (Real ACT scores are integers between 1 and 36, but we will ignore this detail and use continuous values 
 instead.)"
## First we'll simulate an ACT test score dataset and answer some questions about it. Set the seed to 16, then use rnorm() to generate 
## a normal distribution of 10000 tests with a mean of 20.9 and standard deviation of 5.7. Save these values as act_scores. You'll be 
## using this dataset throughout these four multi-part questions.
set.seed(16, sample.kind = "Rounding")
act_scores = rnorm(10000, 20.9, 5.7)
# What is the mean of act_scores? What is the standard deviation of act_scores?
mean(act_scores); sd(act_scores)
# A perfect score is 36 or greater. In act_scores, how many perfect scores are there out of 10,000 simulated tests?
length(act_scores[which(act_scores >= 36)])
sum(act_scores >= 36)
# In act_scores, what is the probability of an ACT score greater than 30?
length(act_scores[which(act_scores > 30)]) / length(act_scores)
mean(act_scores > 30)
# In act_scores, what is the probability of an ACT score less than or equal to 10?
length(act_scores[which(act_scores <= 10)]) / length(act_scores)
mean(act_scores <= 10)
# Set x equal to the sequence of integers 1 to 36. Use dnorm to determine the value of the probability density function
# over x given a mean of 20.9 and standard deviation of 5.7; save the result as f_x. Plot x against f_x.
x = 1:36
qplot(x, f_x, data = data.frame(x = x, f_x = dnorm(x, 20.9, 5.7)), geom = "line")
# Convert act_scores to Z-scores.
z_scores = scale(act_scores)
# What is the probability of a Z-score greater than 2 (2 standard deviations above the mean)?
mean(z_scores > 2)
# What ACT score value corresponds to 2 standard deviations above the mean (Z = 2)?
mean(act_scores) + 2*sd(act_scores)
# A Z-score of 2 corresponds roughly to the 97.5th percentile. Use qnorm() to determine the 97.5th percentile of normally distributed 
# data with the mean and standard deviation observed in act_scores.
qnorm(0.975, 20.9, 5.7) # ~ mean + 2sd
# Write a function that takes a value and produces the probability of an ACT score less than or equal to that value (the CDF). Apply this
# function to the range 1 to 36.
cdf = function(a) mean(act_scores <= a)
a = 1:36
data.frame(a, sapply(a, cdf))
# Use qnorm() to determine the expected 95th percentile
qnorm(0.95, 20.9, 5.7)
# Make a vector containing the quantiles for p <- seq(0.01, 0.99, 0.01), the 1st through 99th percentiles of the act_scores data. 
p = seq(0.01, 0.99, 0.01)
sample_quantiles = quantile(act_scores, p)
data.frame(p, sample_quantiles)
# In what percentile is a score of 26?
"Your answer should be an integer (i.e. 60), not a percent or fraction. Note that a score between the 98th and 99th percentile should 
 be considered the 98th percentile, answer is 82 = between 82 and 83"
data.frame(p, sample_quantiles)[82:83,]
# Make a corresponding set of theoretical quantiles using qnorm() over the interval p <- seq(0.01, 0.99, 0.01) with mean 20.9 and standard 
# deviation 5.7. Save these as theoretical_quantiles. Make a QQ-plot graphing sample_quantiles on the y-axis versus theoretical_quantiles 
# on the x-axis.
theoretical_quantiles = qnorm(p, 20.9, 5.7)
qplot(theoretical_quantiles, sample_quantiles)








rm(list = ls())
options(digits = 4)
library(imager); library(gtools); library(tidyverse); library(ggplot2)

## Being able to quantify the uncertainty introduced by randomness is one of the most important jobs of a data analyst. Statistical 
## inference offers a framework, as well as several practical tools, for doing this. The first step is to learn how to mathematically
## describe random variables.

## In this chapter, we introduce random variables and their properties starting with their application to games of chance. We then
## describe some of the events surrounding the financial crisis of 2007-200850 using probability theory. This financial crisis was 
## in part caused by underestimating the risk of certain securities51 sold by financial institutions. Specifically, the risks of 
## mortgage-backed securities (MBS) and collateralized debt obligations (CDO) were grossly underestimated. These assets were sold 
## at prices that assumed most homeowners would make their monthly payments, and the probability of this not occurring was calculated 
## as being low. A combination of factors resulted in many more defaults than were expected, which led to a price crash of these
## securities. As a consequence, banks lost so much money that they needed government bailouts to avoid closing down completely.



# Random Variables --------------------------------------------------------

"Random variables are numeric outcomes resulting from random processes"
## For example, define X to be 1 if a bead is blue and red otherwise:
beads = rep(c("red", "blue"), times = c(2,3))
X = ifelse(sample(beads, 1) == "blue", 1, 0)
## Here X is a random variable: every time we select a new bead the outcome changes randomly
X = replicate(100, ifelse(sample(beads, 1) == "blue", 1, 0)); X



# Sampling models ---------------------------------------------------------

## Many data generation procedures, those that produce the data we study, can be modeled quite well as draws from an urn.  In 
## epidemiological studies, we often assume that the subjects in our study are a random sample from the population of interest. 
## The data related to a specific outcome can be modeled as a random sample from an urn containing the outcome for the entire 
## population of interest. Similarly, in experimental research, we often assume that the individual organisms we are studying, 
## for example worms, flies, or mice, are a random sample from a larger population.

## Suppose a very small casino hires you to consult on whether they should set up roulette wheels. To keep the example simple, 
## we will assume that 1,000 people will play and that the only game you can play on the roulette wheel is to bet on red or black. 
## The casino wants you to predict how much money they will make or lose. They want a range of values and, in particular, they want 
## to know what’s the chance of losing money. If this probability is too high, they will pass on installing roulette wheels.

## We are going to define a random variable S that will represent the casino’s total winnings. Let’s start by constructing the urn.
## A roulette wheel has 18 red pockets, 18 black pockets and 2 green ones. So playing a color in one game of roulette is equivalent
## to drawing from this urn:
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2))
## The 1,000 outcomes from 1,000 people playing are independent draws from this urn. If red comes up, the gambler wins and the casino
## loses a dollar, so we draw a -$1. Otherwise, the casino wins a dollar and we draw a $1. 
X = sample(ifelse(color == "Red", -1, 1), 1000, replace = T); X[1:10] # sample understands the data to randomize from is color
## We can also generate results like this
X = sample(c(1,-1), 1000, replace = T, prob = c(18/38, 20/38))
"We call this a sampling model since we are modeling the random behavior of roulette with the sampling of draws from an urn. 
 The total winnings S is simply the sum of these 1,000 independent draws:"
S = sum(X); S
## We can then compute the estimated expected gain through a Monte Carlo experiment
gains = replicate(10000, sample(c(-1, 1), 1000, replace = T, prob = c(18/38, 20/38)) %>% sum())
mean(gains)



# The probability distribution of a random variable -----------------------

"If you run the code above, you see that S changes every time. This is, of course, because S is a random variable."

"Note that if we can define a cumulative distribution function F(a) = P(S <= a), then we will be able to answer any question 
 related to the probability of events defined by our random variable S, including the event S<0. We call this F the random 
 variable’s distribution function."


"We can estimate the distribution function for the random variable S by using a Monte Carlo simulation to generate many realizations
 of the random variable."
S = replicate(10000, sample(c(-1, 1), 1000, replace = T, prob = c(18/38, 20/38)) %>% sum())
mean(S)
## Now we can ask the following: in our simulations, how often did we get sums less than or equal to a? This will be a very good 
## approximation of F(a) 
mean(S < 0)
hist(S, freq = F, breaks = 25)

## We see that the distribution appears to be approximately normal. A qq-plot will confirm that the normal approximation is close 
## to a perfect approximation for this distribution. If, in fact, the distribution is normal, then all we need to define the 
## distribution is the average and the standard deviation. Because we have the original values from which the distribution is 
## created, we can easily compute these with mean(S) and sd(S). The blue curve you see added to the histogram above is a normal 
## density with this average and standard deviation.
ggplot(data.frame(S), aes(sample = scale(S))) +
  geom_qq() + geom_abline()
"This average and this standard deviation have special names. They are referred to as the expected value and standard error of 
 the random variable S"


"Statistical theory provides a way to derive the distribution of random variables defined as independent random draws from an urn.
 Specifically, in our example above, we can show that  (S+n)/2 follows a binomial distribution. We therefore do not need to run 
 for Monte Carlo simulations to know the probability distribution of S :
 P(S < 0) = P((S + n)/2 < (a + n)/2)"
## P(S < 0) proba to lose after 1000 games
pbinom(1000 / 2, size = 1000, prob = 20/38)
## Because this is a discrete probability function,to get P(S<0) rather than P(S<=0), we write:
pbinom(1000 / 2 - 1, size = 1000, prob = 20/38)
## Before with Monte Carlo we found that on 1000 games we won on average of 52-53
## P(S < 52.5) proba to win less than 100 after 10000 games
pbinom((1000 + 52.5) / 2 - 1, size = 1000, prob = 20/38) # the probability to win less than 52.5 is a bit under 50%






# The expected value and standard error -----------------------------------

"Using the definition of standard deviation, we can derive, with a bit of math, that if an urn contains two values a and b with 
 proportions p and (1-p), the standard deviation is:"
                                                        "|b - a|.(p(1-p))^1/2"
## In the roulette example :
2 * sqrt(10/19 * (9/19))
## the random variable defined by one draw has an expected value of 0.05 and a standard error of about 1. This makes sense since we 
## either get 1 or -1, with 1 slightly favored over -1.

"Empirical"
gains = sample(c(-1, 1), 1000000, replace = T, prob = c(18/38, 20/38))
mean(gains)
sd(gains)
## Over 1,000,000 games
sum(gains)


"Theoretical"
"E[X] = a.p + b.(1-p)"
expected = 1*20/38 - 1*18/38
stdev = 2 * sqrt(10/19 * (9/19))
## Over 1,000,000 games
1000000*expected
"If our draws are independent, then the standard error of the sum is given by the equation"
sqrt(1000000) * stdev ## standard error

## As a result, when 1,000,000 people bet on red, the casino is expected to win $52,631 with a standard error of about $1,000. 
## It therefore seems like a safe bet. But we still haven’t answered the question: how likely is it to lose money? Here the Central
## Limit Theorem will help.

"Advanced note: Before continuing we should point out that exact probability calculations for the casino winnings can be performed 
 with the binomial distribution. However, here we focus on the CLT, which can be generally applied to sums of random variables in a
 way that the binomial distribution can’t."


#  Population SD versus the sample SD -------------------------------------

## sd() does not return the sd of the list, but rather uses a formula that estimates standard deviations of a population from a 
## random sample X1,...,Xn which divide the sum of squares by n-1 (this is because when we work with samples we also estimate
## the mean, so we lose a degree of freedom)
"adjusted standard error = sqrt(1 / (n-1)) * standard deviation" # écart-type corrigé

## For all the theory discussed here, you need to compute the actual standard deviation as defined: 
"sqrt(mean((x-m)^2))" # non adjusted standard error





#  Central Limit Theorem --------------------------------------------------

"The Central Limit Theorem (CLT) tells us that when the number of draws, also called the sample size, is large, the probability 
 distribution of the sum of the independent draws is approximately normal. Because sampling models are used for so many data 
 generation processes, the CLT is considered one of the most important mathematical insights in history."

## Therefore the theoretical values above match those obtained with a Monte Carlo simulation
## So using the CLT, we can skip the Monte Carlo simulation and instead compute the probability of the casino losing money using
## the approximation of the mean and the standard error

"How large is large in the Central Limit Theorem?"
## The CLT works when the number of draws is large. But large is a relative term. In many circumstances as few as 30 draws is 
## enough to make the CLT useful. In some specific instances, as few as 10 is enough. However, these should not be considered 
## general rules. Note, for example, that when the probability of success is very small, we need much larger sample sizes.




# Statistical properties of averages --------------------------------------

## The expected value of the sum of random variables is the sum of each random variable’s expected value. We can write it like this:
"E[X1 + ... + Xn] = E[X1] + ... + E[Xn]"
## If the X are independent draws from the urn, then they all have the same expected value. Let’s call it μ and thus: 
"E[X1 + ... + Xn] = nμ"

## The expected value of a non-random constant times a random variable is the non-random constant times the expected value of a
## random variable.
"E[aX] = a.E[X]"

## For independent variables :
"SE[X1 + ... + Xn] = √(SE[X1]^2 + ... + SE[Xn]^2) = √(V[X1] + ... + V[Xn])"

## The standard error of a non-random constant times a random variable is the non-random constant times the standard error.
"SE[aX] = a.SE[X] <=> V[aX] = a^2.V[X]"

## If X is normally distributed ans a and b are constants, then aX + b is also normally distributed
## All we are doing is changing the units of the random variable by multiplying by a then shifting the center by b



# Law of large numbers ----------------------------------------------------

"An important implication of the final result is that the standard error of the average becomes smaller and smaller as n grows larger.
 When n is very large, then the standard error is practically 0 and the average of the draws converges to the average of the urn"

## The law of averages is sometimes misinterpreted. For example, if you toss a coin 5 times and see a head each time, you might hear 
## someone argue that the next toss is probably a tail because of the law of averages: on average we should see 50% heads and 50% 
## tails. A similar argument would be to say that red “is due” on the roulette wheel after seeing black come up five times in a row. 
## These events are independent so the chance of a coin landing heads is 50% regardless of the previous 5. This is also the case for
## the roulette outcome. The law of averages applies only when the number of draws is very large and not in small samples. After a 
## million tosses, you will definitely see about 50% heads regardless of the outcome of the first five tosses.





# DataCamp Assessment -----------------------------------------------------

"Betting on green in the roulette game"
# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38
# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green
# Define the number of bets using the variable 'n'
n <- 100
# Calculate 'avg', the expected outcome of 100 spins if you win $17 when the ball lands on green and you lose $1 when the ball doesn't land on green
avg <- n * (17*p_green + -1*p_not_green)
# Compute 'se', the standard error of the sum of 100 outcomes
se <- sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)
# Using the expected value 'avg' and standard error 'se', compute the probability that you win money betting on green 100 times.
1 - pnorm(0, avg, se)

"Monte Carlo"
# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Create an object called `S` that replicates the sample code for `B` iterations and sums the outcomes.
S = replicate(B, sum(sample(c(17, -1), n, replace = T, prob = c(p_green, p_not_green))))
# Compute the average value for 'S'
mean(S)
# Calculate the standard deviation of 'S'
sd(S)
# Calculate the proportion of outcomes in the vector `S` that exceed $0
mean(S>0)

"Average winnings per bet"
# Define the number of bets using the variable 'n'
n <- 10000
# Create a vector called `X` that contains the outcomes of `n` bets
X = sample(c(17, -1), n, prob = c(p_green, p_not_green), replace = T)
# Define a variable `Y` that contains the mean outcome per bet. Print this mean to the console.
Y = mean(X)

# Calculate the expected outcome of `Y`, the mean outcome per bet in 10,000 bets
p_green*17 - p_not_green
# Compute the standard error of 'Y', the mean outcome per bet from 10,000 bets.
abs(17 - -1)*sqrt(p_green*p_not_green) / sqrt(n)

# We defined the average using the following code
avg <- 17*p_green + -1*p_not_green
# We defined standard error using this equation
se <- 1/sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)
# Given this average and standard error, determine the probability of winning more than $0. Print the result to the console.
1 - pnorm(0, avg, se)


"Create a Monte Carlo simulation that generates 10,000 outcomes of S, the average outcome from 10,000 bets on green."
# The variable `n` specifies the number of independent bets on green
n <- 10000
# The variable `B` specifies the number of times we want the simulation to run
B <- 10000
# Use the `set.seed` function to make sure your answer matches the expected result after random number generation
set.seed(1)
# Generate a vector `S` that contains the the average outcomes of 10,000 bets modeled 10,000 times
S = replicate(B, mean(sample(c(17,-1), n, prob = c(p_green, p_not_green), replace = T)))
# Compute the average of `S`
mean(S)
# Compute the standard deviation of `S`
sd(S)
# Compute the proportion of outcomes in the vector 'S' where you won more than $0
mean(S>0)
"Approximations are now much closer"





# edX Assessment ----------------------------------------------------------

"An old version of the SAT college entrance exam had a -0.25 point penalty for every incorrect answer and awarded 1 point for a correct answer. 
 The quantitative test consisted of 44 multiple-choice questions each with 5 answer choices. Suppose a student chooses answers by guessing for 
 all questions on the test."
# What is the probability of guessing correctly for one question?
0.2
# What is the expected value of points for guessing on one question?
avg = 0.2*1-0.8*0.25 # 0
# What is the expected score of guessing on all 44 questions?
44 * avg
# What is the standard error of guessing on all 44 questions?
se = sqrt(44) * abs(1 - -0.25)*sqrt(0.2*0.8)
# Use the Central Limit Theorem to determine the probability that a guessing student scores 8 points or higher on the test.
1-pnorm(8, avg, se)
# Set the seed to 21, then run a Monte Carlo simulation of 10,000 students guessing on the test.
set.seed(21, sample.kind = "Rounding")
Y = replicate(10000, sum(sample(c(-0.25, 1), 44, replace = T, prob = c(0.8, 0.2))))
mean(Y>=8)

"The SAT was recently changed to reduce the number of multiple choice options from 5 to 4 and also to eliminate the penalty for guessing."
# What is the expected value of the score when guessing on this new test?
avg2 = 0.25*1 # 0.2
44 * avg2
# Consider a range of correct answer probabilities representing a range of student skills. What is the lowest p such that the probability of 
# scoring over 35 exceeds 80%?
p <- seq(0.25, 0.95, 0.05)
X = 44 * p
prob_over_35 = 1 - pnorm(35, X, sqrt(p*(1-p))) %>% round(3); data.frame(p, prob_over_35)

"A casino offers a House Special bet on roulette, which is a bet on five pockets (00, 0, 1, 2, 3) out of 38 total pockets. The bet pays out 6 
 to 1. In other words, a losing bet yields -$1 and a successful bet yields $6. A gambler wants to know the chance of losing money if he places
 500 bets on the roulette House Special."
# What is the expected value of the payout for one bet?
avg = 6*5/38 - 33/38; avg
# What is the standard error of the payout for one bet?
se = abs(6 - -1) * sqrt(5/38 * 33/38); se
# What is the expected value of the average payout over 500 bets?
avg
# What is the standard error of the average payout over 500 bets?
se_over500 = (1/sqrt(500)) * se; se_over500
# What is the expected value of the sum of 500 bets?
avg_sum = avg*500; avg_sum
# What is the standard error of the sum of 500 bets?
se_sum = se * sqrt(500); se_sum
# Use pnorm() with the expected value of the sum and standard error of the sum to calculate the probability of losing money over 500 bets
pnorm(0, avg_sum, se_sum)














rm(list = ls())
options(digits = 4)
library(imager); library(gtools); library(tidyverse); library(ggplot2)



# Interest rates explained with chance model ------------------------------

## More complex versions of the sampling models we have discussed are also used by banks to decide interest rates. Suppose you run a small 
## bank that has a history of identifying potential homeowners that can be trusted to make payments. In fact, historically, in a given year,
## only 2% of your customers default, meaning that they donât pay back the money that you lent them. However, you are aware that if you 
## simply loan money to everybody without interest, you will end up losing money due to this 2%. Although you know 2% of your clients will 
## probably default, you donât know which ones. Yet by charging everybody just a bit extra in interest, you can make up the losses incurred 
## due to that 2% and also cover your operating costs. You can also make a profit, but if you set the interest rates too high, your clients
## will go to another bank. We use all these facts and some probability theory to decide what interest rate you should charge.

## Suppose your bank will give out 1,000 loans for $180,000 this year. Also, after adding up all costs, suppose your bank loses $200,000 per 
## foreclosure. For simplicity, we assume this includes all operational costs. A sampling model for this scenario can be coded like this:
n_loans = 1000; loss = -200000; prob_default = 0.02; loan = 180000
defaults = sample(c(0,1), n_loans, prob = c(1-prob_default, prob_default), replace = T)
sum(defaults * loss) ## all losses over 1000 loans

## Note that the total loss defined by the final sum is a random variable. Every time you run the above code, you get a different answer. We 
## can easily construct a Monte Carlo simulation to get an idea of the distribution of this random variable.
losses = replicate(10000, {defaults = sample(c(0,1), n_loans, prob = c(1-prob_default, prob_default), replace = T)
                            sum(defaults * loss)})

## We donât really need a Monte Carlo simulation though. Using what we have learned, the CLT tells us that because our losses are a sum of 
## independent draws, its distribution is approximately normal with expected value and standard errors given by:
avg = n_loans * (prob_default*loss - 0); avg
se = sqrt(n_loans) * abs(loss)*sqrt(prob_default*(1-prob_default)); se

## We can now set an interest rate to guarantee that, on average, we break even. So we introduce x that will represent the interest paid by
## borrowers -> x verifies loss.p + x(1-p) = 0 <=> x = -(loss.p)/(1-p)
x = -(loss * prob_default) / (1 - prob_default); x
i = x/loan; i

## However, we still have a problem. Although this interest rate guarantees that on average we break even, there is a 50% chance that we 
## lose money. If our bank loses money, we have to close it down. We therefore need to pick an interest rate that makes it unlikely for
## this to happen. At the same time, if the interest rate is too high, our clients will go to another bank so we must be willing to take
## some risks. So letâs say that we want our chances of losing money to be 1 in 100.
"We want P(gains<0) = 0.01"
I = seq(i, 0.05, 0.0001)
X = loan*I
avg_sum = n_loans * (prob_default*loss + (1-prob_default)*X)
se_sum = sqrt(n_loans) * abs(X - loss)*sqrt(prob_default * (1-prob_default))
"Approaching"
prob_lose = pnorm(0, avg_sum, se_sum)
data.frame(I, X, prob_lose) %>% round(5) #> ~0.3468 -> x = 6242
"Mathematically"
# Now we are going to use a mathematical âtrickâ that is very common in statistics. We add and subtract the same quantities to both sides
## of the event S < 0 so that the probability does not change and we end up with a standard normal random variable on the left, which will
## then permit us to write down an equation with only x as an unknown. This âtrickâ is as follows:
"P(S<0) = 0.01 <=> P((S-E[S]) / SE[S] < -E[S] / SE[S]) = 0.01 
               <=> P(Z < -E[S] / SE[S]) = 0.01
               <=> -E[S] / SE[S] = z as P(Z < z) = 0.01
               <=> -E[S] / SE[S] = z(0.01)
               <=> -n.(loss.p + x(1-p) / (sqrt(n).|x-loss|.sqrt(p(1-p)))) = z
resolving leads to  x = -l.[(np-z.sqrt(np(1-p))) / (n(l-p) + z.sqrt(np(1-p))] "
z = qnorm(0.01); n = n_loans; l = loss; p = prob_default
x_1percent = -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p))); x_1percent
x_1percent/loan
#> 6249 - 3.472% ang the expected return is
n*(l*p + x_1percent*(1-p))
## This is still a very competitive interest rate. By choosing this interest rate, we now have an expected profit per loan of:
(l*p + x_1percent*(1-p))*n #> 2,124,198
"Monte Carlo verification"
profit = replicate(10000, {draws = sample(c(x_1percent, l), n, prob = c(1-p, p), replace = T)
                           sum(draws)})
mean(profit) #> ~2,130,000
mean(profit<0) #> lose in 1.35% of cases




# The big short -----------------------------------------------------------

## One of your employees points out that since the bank is making 2,124 dollars per loan, the bank should give out more loans! Why just 
## n ? You explain that finding those n clients was hard. You need a group that is predictable and that keeps the chances of defaults 
## low. He then points out that even if the probability of default is higher, as long as our expected value is positive, you can minimize
## your chances of losses by increasing n and relying on the law of large numbers.

## Even if the default rate is higher, we can minimize our chances of losing money by simply increasing n since :
"P(S<0) = P(Z < -E[S]/SE[S]) with E[S] = n.Î¼ and SE[S] = â(n)Ï
<=> z(0.01) = -(n.Î¼) / (â(n)Ï) = -(â(n).Î¼) / Ï"
"So we are guaranteed to have a probability of less than 0.01 if we let :
                n â¥ z^2.Ï^2/Î¼^2 <=> n â¥ (z.Ï/Î¼)^2"
## The implication is that, as long as Î¼ is positive, we can find an n that minimizes the probability of a loss. This is a form of the 
## law of large numbers: when n is large, our average earnings per loan converges to the expected earning Î¼.

## With x fixed to a 5% interest and the probability of default being 4%, now we can ask what n do we need for the probability to 
## be 0.01? In our example, if we give out:
z = qnorm(0.01); x = 0.05*180000; p = 0.04
n = ceiling((z^2 * (x-l)^2*p*(1-p)) / (l*p + x*(1-p))^2); n #> this config works better if we make 22163 loans or more
"ceiling() returns the nearest integer higher than the result e.g. ceiling(2.1) = 3"
## Now we expect to win:
n*(l*p + x*(1-p)) # 14,184,320 against


"If events are dependent and thus defaults probs change, the standard deviation increases a lot and with it the proba to lose money"
## Your colleagueâs scheme was mainly based on this mathematical formula: SE[(X1 + ... + Xn)/n] = Ï/ân
## By making n large, we minimize the standard error of our per-loan profit. However, for this rule to hold, the Xs must be independent 
## draws: one person defaulting must be independent of others defaulting. Note that in the case of averaging the same event over and over,
## an extreme example of events that are not independent, we get a standard error that is ân times bigger:
## SE[(X1 + ... + Xn)/n] = SE[nX/n] = Ï > Ï/ân

## To construct a more realistic simulation than the original one your colleague ran, letâs assume there is a global event that affects
## everybody with high-risk mortgages and changes their probability. We will assume that with 50-50 chance, all the probabilities go up
## or down slightly to somewhere between 0.03 and 0.05. But it happens to everybody at once, not just one person. These draws are no 
## longer independent.
p = 0.04; x = 0.05*180000
profit = replicate(10000, {new_p = 0.04 + sample(seq(-0.01, 0.01, length = 1000), 1)
                           draws = sample(c(x, loss), n, prob = c(1-new_p, new_p), replace = T)
                           sum(draws)})
## Note that our expected profit is still large. However, the probability of the bank having negative earnings shoots up
mean(profit); mean(profit<0) #> 35% to lose
## Even scarier is that the probability of losing more than 10 million dollars is:
mean(profit < -10^7) #> 24%

## To understand how this happens look at the distribution:
data.frame(profits_millions = profit / 10^6) %>%
  ggplot(aes(profits_millions)) +
  geom_histogram(color = "black", binwidth = 5)

## The theory completely breaks down and the random variable has much more variability than expected. The financial meltdown of 2007 was
## due, among other things, to financial âexpertsâ assuming independence when there was none.















