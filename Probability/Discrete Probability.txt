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





