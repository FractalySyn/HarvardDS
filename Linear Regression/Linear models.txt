rm(list = ls())
x = c("tidyverse", "ggplot2", "imager", "Lahman", "HistData", "broom")
lapply(x, library, character.only = TRUE)
options(digits = 3)



"Since Galton’s original development, regression has become one of the most widely used
tools in data science. One reason has to do with the fact that regression permits us to find
relationships between two variables taking into account the effects of other variables that
affect both. This has been particularly popular in fields where randomized experiments are
hard to run, such as economics and epidemiology."

# When we are not able to randomly assign each individual to a treatment or control group,
# confounding is particularly prevalent. For example, consider estimating the effect of eating
# fast foods on life expectancy using data collected from a random sample of people in a
# jurisdiction. Fast food consumers are more likely to be smokers, drinkers, and have lower
# incomes. Therefore, a naive regression model may lead to an overestimate of the negative
# health effect of fast food.




# Case Study : Moneyball --------------------------------------------------

"Moneyball: The Art of Winning an Unfair Game is a book by Michael Lewis about the
Oakland Athletics (A’s) baseball team and its general manager, the person tasked with
building the team, Billy Beane."

# Traditionally, baseball teams use scouts to help them decide what players to hire. These
# scouts evaluate players by observing them perform. Scouts tend to favor athletic players
# with observable physical abilities. For this reason, scouts tend to agree on who the best
# players are and, as a result, these players tend to be in high demand. This in turn drives up
# their salaries.

"From 1989 to 1991, the A’s had one of the highest payrolls in baseball. They were able
to buy the best players and, during that time, they were one of the best teams. However,
in 1995 the A’s team owner changed and the new management cut the budget drastically,
leaving then general manager, Sandy Alderson, with one of the lowest payrolls in baseball.
He could no longer afford the most sought-after players. Alderson began using a statistical
approach to find inefficiencies in the market. Alderson was a mentor to Billy Beane, who
succeeded him in 1998 and fully embraced data science, as opposed to scouts, as a method for
finding low-cost players that data predicted would help the team win. Today, this strategy
has been adapted by most baseball teams. As we will see, regression plays a large role in
this approach."

# As motivation for this chapter, we will pretend it is 2002 and try to build a baseball team
# with a limited budget, just like the A’s had to do. To appreciate what you are up against,
# note that in 2002 the Yankees’ payroll of $125,928,583 more than tripled the Oakland A’s
# $39,679,746




# Sabermetrics ------------------------------------------------------------

"Statistics have been used in baseball since its beginnings. The dataset we will be using,
included in the Lahman library, goes back to the 19th century. For example, a summary
statistics we will describe soon, the batting average, has been used for decades to summarize
a batter’s success. Other statistics such as home runs (HR), runs batted in (RBI), and
stolen bases (SB) are reported for each player in the game summaries included in the sports
section of newspapers, with players rewarded for high numbers. Although summary statistics
such as these were widely used in baseball, data analysis per se was not"

# This changed with Bill James. In the late 1970s, this aspiring writer and baseball fan
# started publishing articles describing more in-depth analysis of baseball data. He named
# the approach of using data to predict what outcomes best predicted if a team would win
# sabermetrics. Until Billy Beane made sabermetrics the center of his baseball operation, Bill
# James’ work was mostly ignored by the baseball world. Currently, sabermetrics popularity
# is no longer limited to just baseball; other sports have started to use this approach as well.

"The goal of a baseball game is to score more runs (points) than the other team. Each team
has 9 batters that have an opportunity to hit a ball with a bat in a predetermined order.
After the 9th batter has had their turn, the first batter bats again, then the second, and
so on. Each time a batter has an opportunity to bat, we call it a plate appearance (PA).
At each PA, the other team’s pitcher throws the ball and the batter tries to hit it. The PA
ends with an binary outcome: the batter either makes an out (failure) and returns to the
bench or the batter doesn’t (success) and can run around the bases, and potentially score a
run (reach all 4 bases). Each team gets nine tries, referred to as innings, to score runs and
each inning ends after three outs (three failures).
If the batter hits it hard enough, it is a HR, the best possible outcome as
the batter gets at least one automatic run. But sometimes, due to chance, the batter hits
the ball very hard and a defender catches it, resulting in an out."

# • Bases on balls (BB) - the pitcher fails to throw the ball through a predefined area considered
# to be hittable (the strikezone), so the batter is permitted to go to first base.
# • Single - Batter hits the ball and gets to first base.
# • Double (2B) - Batter hits the ball and gets to second base.
# • Triple (3B) - Batter hits the ball and gets to third base.
# • Home Run (HR) - Batter hits the ball and goes all the way home and scores a run.

"While the batter is on base, the batter can also try to steal a
base (SB). If a batter runs fast enough, the batter can try to go from one base to the
next without the other team tagging the runner."



"Historically, the batting average has been considered the most important offensive statistic.
To define this average, we define a hit (H) and an at bat (AB). Singles, doubles, triples, and
home runs are hits. The fifth way to be successful, BB, is not a hit. An AB is the number of
times you either get a hit or make an out; BBs are excluded. The batting average is simply
H/AB and is considered the main measure of a success rate. Today this success rate ranges
from 20% to 38%. We refer to the batting average in thousands so, for example, if your
success rate is 28%, we call it batting 280."


# One of Bill James’ first important insights is that the batting average ignores BB, but
# a BB is a success. He proposed we use the on base percentage (OBP) instead of batting
# average. He defined OBP as (H+BB)/(AB+BB) which is simply the proportion of plate
# appearances that don’t result in an out, a very intuitive measure. He noted that a player
# that gets many more BB than the average player might not be recognized if the batter does
# not excel in batting average. But is this player not helping produce runs? No award is given
# to the player with the most BB. However, bad habits are hard to break and baseball did
# not immediately adopt OBP as an important statistic. In contrast, total stolen bases were
# considered important and an award8 given to the player with the most. But players with
# high totals of SB also made more outs as they did not always succeed. Does a player with
# high SB total help produce runs? Can we use data science to determine if it’s better to pay
# for players with high BB or SB?




# BB or SB ? --------------------------------------------------------------

"One of the challenges in this analysis is that it is not obvious how to determine if a player
produces runs because so much depends on his teammates. We do keep track of the number
of runs scored by a player. However, remember that if a player X bats right before someone
who hits many HRs, batter X will score many runs. But these runs don’t necessarily happen
if we hire player X but not his HR hitting teammate. However, we can examine team-level
statistics. How do teams with many SB compare to teams with few? How about BB? We
have data! Let’s examine some."

# Let’s start with an obvious one: HRs. Do teams that hit more home runs score more runs? We
# examine data from 1961 to 2001. The visualization of choice when exploring the relationship
# between two variables, such as HRs and wins, is a scatterplot:
library(Lahman)
Teams %>% filter(yearID %in% 1961:2001) %>%
   mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
   ggplot(aes(HR_per_game, R_per_game)) +
   geom_point(alpha = 0.5) + 
   geom_smooth(method='lm')
"The plot shows a strong association: teams with more HRs tend to score more runs. Now
let’s examine the relationship between stolen bases and runs:"
Teams %>% filter(yearID %in% 1961:2001) %>%
   mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
   ggplot(aes(SB_per_game, R_per_game)) +
   geom_point(alpha = 0.5) + 
   geom_smooth(method='lm')
# Here the relationship is not as clear. Finally, let’s examine the relationship between BB and
# runs:
Teams %>% filter(yearID %in% 1961:2001) %>%
   mutate(BB_per_game = BB/G, R_per_game = R/G) %>%
   ggplot(aes(BB_per_game, R_per_game)) +
   geom_point(alpha = 0.5) +
   geom_smooth(method = "lm")


"!!!!!!!!!!!!!!
Here again we see a clear association. But does this mean that increasing a team’s BBs
causes an increase in runs? One of the most important lessons you learn in this book is
that association is not causation.
!!!!!!!!!!!!!!"

# In fact, it looks like BBs and HRs are also associated:
Teams %>% filter(yearID %in% 1961:2001 ) %>%
   mutate(HR_per_game = HR/G, BB_per_game = BB/G) %>%
   ggplot(aes(HR_per_game, BB_per_game)) +
   geom_point(alpha = 0.5)

"We know that HRs cause runs because, as the name “home run” implies, when a player hits
a HR they are guaranteed at least one run. Could it be that HRs also cause BB and this
makes it appear as if BB cause runs? When this happens we say there is confounding, an
important concept we will learn more about throughout this chapter"

# Linear regression will help us parse all this out and quantify the associations. This will then
# help us determine what players to recruit. Specifically, we will try to predict things like how
# many more runs will a team score if we increase the number of BBs, but keep the HRs fixed?
# Regression will help us answer questions like this one.





# Regression applied to baseball statistics -------------------------------

"Can we use regression with these data? First, notice that the HR and Run data appear to
be bivariate normal."
library(Lahman)
p <- Teams %>% filter(yearID %in% 1961:2001 ) %>%
   mutate(HR_per_game = HR/G, R_per_game = R/G) %>%
   ggplot(aes(HR_per_game, R_per_game)) +
   geom_point(alpha = 0.5); p
"The qq-plots confirm that the normal approximation is useful here"
Teams %>% filter(yearID %in% 1961:2001 ) %>%
   mutate(z_HR = round((HR - mean(HR))/sd(HR)),
          R_per_game = R/G) %>%
   filter(z_HR %in% -2:3) %>%
   ggplot() +
   stat_qq(aes(sample=R_per_game)) +
   facet_wrap(~z_HR)

"Now we are ready to use linear regression to predict the number of runs a
team will score if
we know how many home runs the team hits. All we need to do is compute the five summary
statistics:"
summary_stats <- Teams %>%
   filter(yearID %in% 1961:2001 ) %>%
   mutate(HR_per_game = HR/G, R_per_game = R/G) %>%
   summarize(avg_HR = mean(HR_per_game),
             s_HR = sd(HR_per_game),
             avg_R = mean(R_per_game),
             s_R = sd(R_per_game),
             r = cor(HR_per_game, R_per_game))
summary_stats
"and use the formulas given above to create the regression lines:"
reg_line <- summary_stats %>% summarize(slope = r*s_R/s_HR,
                                        intercept = avg_R - slope*avg_HR)
p + geom_abline(intercept = reg_line$intercept, slope = reg_line$slope)

"A simpler method is to add le regression line with geom_smooth()"
p + geom_smooth(method = "lm")

# In the example above, the slope is 1.845. So this tells us that teams that hit 1 more HR per
# game than the average team, score 1.845 more runs per game than the average team. Given
# that the most common final score is a difference of a run, this can certainly lead to a large
# increase in wins. Not surprisingly, HR hitters are very expensive. Because we are working
# on a budget, we will need to find some other way to increase wins. So in the next section
# we move our attention to BB




# Confounding -------------------------------------------------------------

"Previously, we noted a strong relationship between Runs and BB. If we find the regression
line for predicting runs from bases on balls, we a get slope of:"
get_slope <- function(x, y) cor(x, y) / (sd(x) * sd(y))
bb_slope <- Teams %>%
   filter(yearID %in% 1961:2001 ) %>%
   mutate(BB_per_game = BB/G, R_per_game = R/G) %>%
   summarize(slope = get_slope(R_per_game, BB_per_game))
bb_slope
#> slope
#> 1 2.12

"!!!!!
We are again reminded that association is not causation. The data does provide strong
evidence that a team with two more BB per game than the average team, scores 4.2 runs
per game. But this does not mean that BB are the cause.
!!!!!"

# Note that if we compute the regression line slope for singles we get 1.3
# which is a lower value than what we obtain for BB.
"Also, notice that a single gets you to first base just like a BB. Those that know about
baseball will tell you that with a single, runners on base have a better chance of scoring
than with a BB. So how can BB be more predictive of runs? The reason this happen is
because of confounding. Here we show the correlation between HR, BB, and singles:"
Teams %>%
   filter(yearID %in% 1961:2001 ) %>%
   mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%
   summarize(cor(BB, HR), cor(Singles, HR), cor(BB, Singles))

# It turns out that pitchers, afraid of HRs, will sometimes avoid throwing strikes to HR hitters.
# As a result, HR hitters tend to have more BBs and a team with many HRs will also have
# more BBs. Although it may appear that BBs cause runs, it is actually the HRs that cause
# most of these runs. We say that BBs are confounded with HRs. Nonetheless, could it be
# that BBs still help? To find out, we somehow have to adjust for the HR effect. Regression
# can help with this as well.




# Understanding confounding through stratification ------------------------

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
   mutate(HR_strata = round(HR/G, 1),
          BB_per_game = BB / G,
          R_per_game = R / G) %>%
   filter(HR_strata >= 0.4 & HR_strata <= 1.2)
"Stratified BB vs Runs"
dat %>%
   ggplot(aes(BB_per_game, R_per_game)) +
   geom_point(alpha = 0.5) +
   geom_smooth(method = "lm") +
   facet_wrap( ~ HR_strata)
# Remember that the regression slope for predicting runs with BB was 2.1
"Once we stratify by HR, these slopes are substantially different:"
dat %>%
   group_by(HR_strata) %>%
   summarize(slope = get_slope(BB_per_game, R_per_game))
# The slopes are reduced, but they are not 0, which indicates that BBs are helpful for producing
# runs, just not as much as previously thought.

"Although our understanding of the application tells us that HR cause BB but not the other
way around, we can still check if stratifying by BB makes the effect of BB go down. To do
this, we use the same code except that we swap HR and BBs to get this plot:"
dat2 = Teams %>% filter(yearID %in% 1961:2001) %>%
   mutate(BB_strata = round(BB/G, 1),
          HR_per_game = HR / G,
          R_per_game = R / G) %>%
   filter(BB_strata >= 2.8 & BB_strata <= 3.9) 
dat2 %>%
   ggplot(aes(HR_per_game, R_per_game)) +
   geom_point(alpha = 0.5) +
   geom_smooth(method = "lm") +
   facet_wrap( ~ BB_strata)
# In this case, the slopes do not change much from the original:
dat2 %>% group_by(BB_strata) %>%
   summarize(slope = get_slope(HR_per_game, R_per_game))
# They are reduced a bit, which is consistent with the fact that BB do in fact cause some
# runs.


# Regardless, it seems that if we stratify by HR, we have bivariate distributions for runs versus
# BB. Similarly, if we stratify by BB, we have approximate bivariate normal distributions for
# HR versus runs.





# Least squares estimates -------------------------------------------------

"Given how we wrote the model above, the intercept β0 is not very interpretable as it is the
predicted height of a son with a father with no height. Due to regression to the mean, the prediction
will usually be a bit larger than 0. To make the slope parameter more interpretable,
we can rewrite the model so that X, or X and Y, are centered to their mean"


library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
   filter(gender == "male") %>%
   group_by(family) %>%
   sample_n(1) %>%
   ungroup() %>%
   select(father, childHeight) %>%
   rename(son = childHeight)

# Let’s write a function that computes the RSS for any pair of values β0 and β1.
"RSS is the sum of squared residuals / errors -> sum(Y-E[Y])^2"

rss <- function(beta0, beta1, data){
   resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
   return(sum(resid^2))
}
# So for any pair of values, we get an RSS. Here is a plot of the RSS as a function of β1 when
# we keep the β0 fixed at 25.
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() +
   geom_line(aes(beta1, rss))

"We find the best coefficients by minimizing the RSS"

fit <- lm(son ~ father, data = galton_heights)
summary(fit)

"Although we do not show examples in this book, hypothesis testing with regression models
is commonly used in epidemiology and economics to make statements such as “the effect
of A on B was statistically significant after adjusting for X, Y, and Z”. However, several
assumptions have to hold for these statements to be true."





# Least Squares Estimates (LSE) are random variables ----------------------------------------------------------------------

B <- 1000
N <- 50
lse <- replicate(B, {
   sample_n(galton_heights, N, replace = TRUE) %>%
      lm(son ~ father, data = .) %>%
      .$coef
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])

hist(lse$beta_1)






# Linear regression in the tidyverse --------------------------------------

"!!!!
The lm function ignores the group_by. This is expected
because lm is not part of the tidyverse and does not know how to handle the outcome of
a grouped tibble.
The do functions serves as a bridge between R functions, such
as lm, and the tidyverse.
!!!!"

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
   mutate(HR = round(HR/G, 1),
          BB = BB/G,
          R = R/G) %>%
   select(HR, BB, R) %>%
   filter(HR >= 0.4 & HR<=1.2)

# For a useful data frame to be constructed, the output of the function must be a data frame
# too. We could build a function that returns only what we want in the form of a data frame:
get_slope <- function(data){
   fit <- lm(R ~ BB, data = data)
   data.frame(slope = fit$coefficients[2],
              se = summary(fit)$coefficient[2,2])
}
dat %>%
   group_by(HR) %>%
   do(get_slope(.))

# This is not very useful, so let’s cover one last feature of do. If the data frame being returned
# has more than one row, these will be concatenated appropriately. Here is an example in
# which we return both estimated parameters:
get_lse <- function(data){
   fit <- lm(R ~ BB, data = data)
   data.frame(term = names(fit$coefficients),
              slope = fit$coefficients,
              se = summary(fit)$coefficient[,2])
}
dat %>%
   group_by(HR) %>%
   do(get_lse(.))





# The broom package -------------------------------------------------------

"If you think this is all a bit too complicated, you are not alone. To simplify things, we
introduce the broom package which was designed to facilitate the use of model fitting
functions, such as lm, with the tidyverse."

# The broom package has three main functions, all of which extract information from the
# object returned by lm and return it in a tidyverse friendly data frame. These functions are
# tidy, glance, and augment. The tidy function returns estimates and related information
# as a data frame:
library(broom)
fit = lm(R ~ BB, data = dat)
tidy(fit)
"We can add other important summaries, such as confidence intervals:"
tidy(fit, conf.int = T)


"Because the outcome is a data frame, we can immediately use it with do to string together
the commands that produce the table we are after. Because a data frame is returned, we
can filter and select the rows and columns we want, which facilitates working with ggplot2:"
dat %>%
   group_by(HR) %>%
   do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
   filter(term == "BB") %>%
   select(HR, estimate, conf.low, conf.high) %>%
   ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
   geom_errorbar() +
   geom_point()

# The other functions provided by broom, glance, and augment, relate to model-specific
# and observation-specific outcomes, respectively. Here, we can see the model fit summaries
# glance returns:
glance(fit)





# Case study: Moneyball (continued) ---------------------------------------

# In trying to answer how well BBs predict runs, data exploration led us to a model:
"E[R|BB, HR] = β0 + β1x1 + β2x2"
#☻ Here, the data is approximately normal and conditional distributions were also normal.
# Thus, we are justified in using a linear model:
"Yi = β0 + β1xi,1 + β2xi,2 + εi"
# with Yi runs per game for team i, xi,1 walks per game, and xi,2. To use lm here, we need to
# let the function know we have two predictor variables. So we use the + symbol as follows:


fit <- Teams %>%
   filter(yearID %in% 1961:2001) %>%
   mutate(BB = BB/G, HR = HR/G, R = R/G) %>%
   lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = T)

fit <- Teams %>%
   filter(yearID %in% 1961:2001) %>%
   mutate(BB = BB / G,
          singles = (H - X2B - X3B - HR) / G,
          doubles = X2B / G,
          triples = X3B / G,
          HR = HR / G,
          R = R / G) %>%
   lm(R ~ BB + singles + doubles + triples + HR, data = .)
tidy(fit)

"To see how well our metric actually predicts runs, we can predict the number of runs for
each team in 2002 using the function predict, then make a plot:"

Teams %>%
   filter(yearID %in% 2002) %>%
   mutate(BB = BB/G,
          singles = (H-X2B-X3B-HR)/G,
          doubles = X2B/G,
          triples =X3B/G,
          HR=HR/G,
          R=R/G) %>%
   mutate(R_hat = predict(fit, newdata = .)) %>%
   ggplot(aes(R_hat, R, label = teamID)) +
   geom_point() +
   geom_text(nudge_x=0.1, cex = 2) +
   geom_abline()

"Our model does quite a good job as demonstrated by the fact that points from the observed
versus predicted plot fall close to the identity line"


# So instead of using batting average, or just number of HR, as a measure of picking players,
# we can use our fitted model to form a metric that relates more directly to run production.
"To define a player-specific metric, we have a bit more work to do. A challenge here is that we
derived the metric for teams, based on team-level summary statistics"
# For players, a rate that takes into account opportunities is the per-plate-appearance rate.

"To make the per-game team rate comparable to the per-plate-appearance player rate, we
compute the average number of team plate appearances per game:"
pa_per_game <- Batting %>% filter(yearID == 2002) %>%
   group_by(teamID) %>%
   summarize(pa_per_game = sum(AB+BB)/max(G)) %>%
   pull(pa_per_game) %>%
   mean

# We compute the per-plate-appearance rates for players available in 2002 on data from 1997-
# 2001. To avoid small sample artifacts, we filter players with less than 200 plate appearances
# per year. Here is the entire calculation in one line:
players <- Batting %>% filter(yearID %in% 1997:2001) %>%
   group_by(playerID) %>%
   mutate(PA = BB + AB) %>%
   summarize(G = sum(PA)/pa_per_game,
             BB = sum(BB)/G,
             singles = sum(H-X2B-X3B-HR)/G,
             doubles = sum(X2B)/G,
             triples = sum(X3B)/G,
             HR = sum(HR)/G,
             AVG = sum(H)/sum(AB),
             PA = sum(PA)) %>%
   filter(PA >= 1000) %>%
   select(-G) %>%
   mutate(R_hat = predict(fit, newdata = .))

"The player-specific predicted runs computed here can be interpreted as the number of runs
we predict a team will score if all batters are exactly like that player. The distribution shows
that there is wide variability across players:"
qplot(R_hat, data = players, binwidth = 0.5, color = I("black"))





# Adding salary and position information ----------------------------------

"To actually build the team, we will need to know their salaries as well as their defensive
position. For this, we join the players data frame we just created with the player information
data frame included in some of the other Lahman data tables"
players <- Salaries %>%
   filter(yearID == 2002) %>%
   select(playerID, salary) %>%
   right_join(players, by="playerID")

# Next, we add their defensive position. This is a somewhat complicated task because players
# play more than one position each year. The Lahman package table Appearances tells how
# many games each player played in each position, so we can pick the position that was
# most played using which.max on each row. We use apply to do this. However, because
# some players are traded, they appear more than once on the table, so we first sum their
# appearances across teams. Here, we pick the one position the player most played using the
# top_n function. To make sure we only pick one position, in the case of ties, we pick the first
# row of the resulting data frame. We also remove the OF position which stands for outfielder,
# a generalization of three positions: left field (LF), center field (CF), and right field (RF).
# We also remove pitchers since they don’t bat in the league in which the A’s play
position_names <-
   paste0("G_", c("p","c","1b","2b","3b","ss","lf","cf","rf", "dh"))

tmp <- Appearances %>%
   filter(yearID == 2002) %>%
   group_by(playerID) %>%
   summarize_at(position_names, sum) %>%
   ungroup()

pos <- tmp %>%
   select(position_names) %>%
   apply(., 1, which.max)

players <- tibble(playerID = tmp$playerID, POS = position_names[pos]) %>%
   mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
   filter(POS != "P") %>%
   right_join(players, by="playerID") %>%
   filter(!is.na(POS) & !is.na(salary))

# Finally, we add their first and last name:
players <- Master %>%
   select(playerID, nameFirst, nameLast, debut) %>%
   mutate(debut = as.Date(debut)) %>%
   right_join(players, by="playerID")

players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>%
   arrange(desc(R_hat)) %>% top_n(10)


# On average, players with a higher metric have higher salaries
players %>% ggplot(aes(salary, R_hat, color = POS)) +
   geom_point() +
   scale_x_log10()


"We can search for good deals by looking at players who produce many more runs than others
with similar salaries. We can use this table to decide what players to pick and keep our total
salary below the 40 million dollars Billy Beane had to work with. This can be done using
what computer scientists call linear programming. This is not something we teach"




# easurement error models -------------------------------------------------

# Up to now, all our linear regression examples have been applied to two or more random
# variables. We assume the pairs are bivariate normal and use this to motivate a linear model.
# This approach covers most real-life examples of linear regression. The other major application
# comes from measurement errors models. In these applications, it is common to have a
# non-random covariate, such as time, and randomness is introduced from measurement error
# rather than sampling or natural variability.

"To understand these models, imagine you are Galileo in the 16th century trying to describe
the velocity of a falling object. An assistant climbs the Tower of Pisa and drops a ball,
while several other assistants record the position at different times. Let’s simulate some
data using the equations we know today and adding some measurement error. The dslabs
function rfalling_object generates these simulations"
library(dslabs)
falling_object = rfalling_object()

# The assistants hand the data to Galileo and this is what he sees:
falling_object %>%
   ggplot(aes(time, observed_distance)) +
   geom_point() +
   ylab("Distance in meters") +
   xlab("Time in seconds")

"Galileo does not know the exact equation, but by looking at the plot above, he deduces that
the position should follow a parabola, which we can write like this
      f(x) = β0 + β1x + β2x^2 "

# The data does not fall exactly on a parabola. Galileo knows this is due to measurement error.
# His helpers make mistakes when measuring the distance. To account for this, he models the
# data with:
"    Y = β0 + β1x + β2x^2 + ε
with Yi representing distance in meters, xi representing time in seconds, and ε accounting
for measurement error. The measurement error is assumed to be random, independent from
each other, and having the same distribution for each i. We also assume that there is no
bias, which means the expected value E[ε] = 0."

fit <- falling_object %>%
   mutate(time_sq = time^2) %>%
   lm(observed_distance~time+time_sq, data=.)
tidy(fit)

"Let’s check if the estimated parabola fits the data. The broom function augment lets us do
this easily:"
augment(fit) %>%
   ggplot() +
   geom_point(aes(time, observed_distance)) +
   geom_line(aes(time, .fitted), col = "blue")

# Thanks to my high school physics teacher, I know that the equation for the trajectory of a
# falling object is:
 "  d = h0 + v0t − 0.5 × 9.8t2"
 # with h0 and v0 the starting height and velocity, respectively. The data we simulated above
 # followed this equation and added measurement error to simulate n observations for dropping
 # the ball (v0 = 0) from the tower of Pisa (h0 = 55.86).
 # These are consistent with the parameter estimates:
 tidy(fit, conf.int = TRUE)
"The Tower of Pisa height is within the confidence interval for β0, the initial velocity 0 is
in the confidence interval for β1 (note the p-value is larger than 0.05), and the acceleration
constant is in a confidence interval for −2 × β2."




# Assessment: Introduction to Linear Models -------------------------------

"Q1 As described in the videos, when we stratified our regression lines for runs 
 per game vs. bases on balls by the number of home runs, what happened?"
The slope of runs per game vs. bases on balls within each stratum was reduced
because we removed confounding by home runs
 
 "Q2 We run a linear model for sons’ heights vs. fathers’ heights using the 
 Galton height data, and get the following results:"
#  Coefficients:
# (Intercept)    father  
#  35.71       0.50 
For every inch we increase the father’s height, the predicted son’s height grows by 0.5 inches.
 
"Q3 We want the intercept term for our model to be more interpretable, so we run 
the same model as before but now we subtract the mean of fathers’ heights from 
each individual father’s height to create a new variable centered at zero."
# Call:
#    lm(formula = son ~ father_centered, data = galton_heights)
# 
# Coefficients:
# (Intercept)    father_centered  
# 70.45          0.50  
The height of a son of a father of average height is 70.45 inches.
 
"Q4 Suppose we fit a multivariate regression model for expected runs based on BB and HR:
E[R|BB=x1,HR=x2]=β0+β1x1+β2x2 
Suppose we fix  BB=x1 . Then we observe a linear relationship between runs and HR 
with intercept of:"
β0+β1x1

"Q5 Which of the following are assumptions for the errors  ϵi  in a linear regression model?"
The  ϵi  are independent of each other
The  ϵi  have expected value 0
The variance of  ϵi  is a constant






# Assessment: Least Squares Estimates -------------------------------------

galton_heights = GaltonFamilies %>%
   filter(gender == "male") %>%
   group_by(family) %>%
   sample_n(1) %>%
   ungroup() %>%
   select(father, childHeight) %>%
   rename(son = childHeight)


# The following code was used in the video to plot RSS with  β0=25 .
beta1 = seq(0, 1, len=nrow(galton_heights))
rss <- function(beta0, beta1, data){
   resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
   return(sum(resid^2))}
   
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
   geom_line(aes(beta1, rss), col=2)

"Q1 In a model for sons’ heights vs fathers’ heights, what is the least squares 
estimate (LSE) for  β1  if we assume  β^0  is 36?"
0.5 # the min(RSS)

"Q2 Load the Lahman library and filter the Teams data frame to the years 1961-2001.
Run a linear model in R predicting the number of runs per game based on both the
number of bases on balls per game and the number of home runs per game."
data = Teams %>%
   filter(yearID %in% 1961:2001) %>%
   mutate(RG = R/G, BBG = BB/G, HRG = HR/G)
lm(RG ~ BBG + HRG, data = data) %>%
   summary()

"Q4 What does the central limit theorem tell us about the variables beta_0 and beta_1?"
They are approximately normally distributed.
The expected value of each is the true value of  β0  and  β1 


"Q6 Which R code(s) below would properly plot the predictions and confidence 
intervals for our linear model of sons’ heights?"
galton_heights %>% ggplot(aes(father, son)) +
   geom_point() +
   geom_smooth(method = "lm")

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)
ggplot(data, aes(x = father, y = fit)) +
   geom_line(color = "blue", size = 1) + 
   geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
   geom_point(data = galton_heights, aes(x = father, y = son))




set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
   filter(gender == "female") %>%     
   group_by(family) %>%     
   sample_n(1) %>%     
   ungroup() %>%     
   select(mother, childHeight) %>%     
   rename(daughter = childHeight)

"Q7 Fit a linear regression model predicting the mothers' heights using daughters' heights."
attach(female_heights)
lm(mother ~ daughter) %>% tidy()

"Q8 What is the predicted height of the first mother in the dataset?
What is the actual height of the first mother in the dataset?"
predict(lm(mother ~ daughter))[1]
mother[1]



# We have shown how BB and singles have similar predictive power for scoring runs. 
# Another way to compare the usefulness of these baseball metrics is by assessing 
# how stable they are across the years. Because we have to pick players based on 
# their previous performances, we will prefer metrics that are more stable. In 
# these exercises, we will compare the stability of singles and BBs

# Before we get started, we want to generate two tables: one for 2002 and another 
# for the average of 1999-2001 seasons. We want to define per plate appearance 
# statistics, keeping only players with more than 100 plate appearances. Here is 
# how we create the 2002 table:
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
   mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
   filter(pa >= 100) %>%
   select(playerID, singles, bb)

"Now compute a similar table but with rates computed over 1999-2001. Keep only rows 
from 1999-2001 where players have 100 or more plate appearances, calculate each player's
single rate and BB rate per season, then calculate the average single rate (mean_singles)
and average BB rate (mean_bb) per player over those three seasons."
bat = Batting %>% filter(yearID %in% 1999:2001) %>%
   mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
   filter(pa >= 100) %>%
   select(playerID, singles, bb) %>%
   group_by(playerID) %>% # calculate averages over three seasons
   mutate(mean_singles = mean(singles),
          mean_bb = mean(bb)) %>%
   ungroup()

"Q9 How many players had a single rate mean_singles of greater than 0.2 per plate
appearance over 1999-2001?
How many players had a BB rate mean_bb of greater than 0.2 per plate appearance 
over 1999-2001?"
bat[bat$mean_singles > 0.2, ]$playerID %>% 
   as.factor() %>% str()
bat[bat$mean_bb > 0.2, ]$playerID %>% 
   as.factor() %>% str()

"Use inner_join() to combine the bat_02 table with the table of 1999-2001 rate 
averages you created in the previous question."
# remove duplicate in bat
bat = bat %>% distinct()
all = inner_join(bat, bat_02, by = "playerID")
"Q10 What is the correlation between 2002 singles rates and 1999-2001 average singles rates?
What is the correlation between 2002 BB rates and 1999-2001 average BB rates?"
cor(all$mean_singles, all$singles.y)
cor(all$mean_bb, all$bb.y)

"Q11 Make scatterplots of mean_singles versus singles and mean_bb versus bb"
attach(all)
plot(mean_singles~singles.y)
plot(mean_bb~bb.y)
# Both distributions are bivariate normal.

"Q12 Fit a linear model to predict 2002 singles given 1999-2001 mean_singles.
 Fit a linear model to predict 2002 bb given 1999-2001 mean_bb."
lm(singles.y~mean_singles) %>% tidy()
lm(bb.y~mean_bb) %>% tidy()





# Assessment: Tibbles, do, and broom --------------------------------------

"Q1 As seen in the videos, what problem do we encounter when we try to run a linear 
 model on our baseball data, grouping by home runs?"
The lm() function does not know how to handle grouped tibbles.

"Q3"
Tibbles display better.
If you subset a tibble, you always get back a tibble.
Tibbles can have complex entries.
Tibbles can be grouped.

"Q4 What are two advantages of the do() command, when applied to the tidyverse?"
It understands grouped tibbles.
It always returns a data.frame.

"You want to take the tibble dat, which we used in the video on the do() function, 
and run the linear model R ~ BB for each strata of HR. Then you want to add three 
new columns to your grouped tibble: the coefficient, standard error, and p-value 
for the BB term in the model."
#♥ You’ve already written the function get_slope(), shown below.
get_slope <- function(data) {
   fit <- lm(R ~ BB, data = data)
   sum.fit <- summary(fit)
   
   data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
              se = sum.fit$coefficients[2, "Std. Error"],
              pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}
"Q5 What additional code could you write to accomplish your goal?"
dat %>% 
   group_by(HR) %>% 
   do(get_slope(.))

"Q6 The output of a broom function is always what?"
Dataframe

"You want to know whether the relationship between home runs and runs per game 
 varies by baseball league. You create the following dataset:"
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
   mutate(HR = HR/G,
          R = R/G) %>%
   select(lgID, HR, BB, R) 
"Q7 What code would help you quickly answer this question?"
dat %>% 
   group_by(lgID) %>% 
   do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
   filter(term == "HR") 

# We have investigated the relationship between fathers' heights and sons' heights. 
# But what about other parent-child relationships? Does one parent's height have a 
# stronger association with child height? How does the child's gender affect this 
# relationship in heights? Are any differences that we observe statistically significant?

# The galton dataset is a sample of one male and one female child from each family in 
# the GaltonFamilies dataset. The pair column denotes whether the pair is father and
# daughter, father and son, mother and daughter, or mother and son.

data("GaltonFamilies")
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
   group_by(family, gender) %>%
   sample_n(1) %>%
   ungroup() %>% 
   gather(parent, parentHeight, father:mother) %>%
   mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
   unite(pair, c("parent", "child"))

galton

"Q8 Group by pair and summarize the number of observations in each group."
galton %>% group_by(pair) %>% .$pair %>% as.factor() %>% summary()

"Q9 Calculate the correlation coefficients for fathers and daughters, fathers and 
 sons, mothers and daughters and mothers and sons."
galton %>% group_by(pair) %>%
   summarise(cor(childHeight, parentHeight))

"Q10 Use lm() and the broom package to fit regression lines for each parent-child 
pair type. Compute the least squares estimates, standard errors, confidence 
intervals and p-values for the parentHeight coefficient for each pair."
galton %>% group_by(pair) %>%
   do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = T)) %>%
   filter(term != "(Intercept)")

"Q11 Which sets of parent-child heights are significantly correlated at a p-value
cut off of .05?"
All
# All of the confidence intervals overlap each other.
# The confidence intervals involving mothers' heights are larger than the confidence intervals 
# involving fathers' heights.
# The data are consistent with inheritance of height being independent of the child's gender.
# The data are consistent with inheritance of height being independent of the parent's gender.





# Assessment: Regression and Baseball, ------------------------------------

"Q2 We want to estimate runs per game scored by individual players, not just by teams. 
What summary metric do we calculate to help estimate this?"
pa_per_game <- Batting %>% 
   filter(yearID == 2002) %>% 
   group_by(teamID) %>%
   summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
   .$pa_per_game %>% 
   mean
# pa_per_game: the number of plate appearances per team per game, averaged across 
# all teams correct


fit <- Teams %>%
   filter(yearID %in% 1961:2001) %>%
   mutate(BB = BB / G,
          singles = (H - X2B - X3B - HR) / G,
          doubles = X2B / G,
          triples = X3B / G,
          HR = HR / G,
          R = R / G) %>%
   lm(R ~ BB + singles + doubles + triples + HR, data = .)
tidy(fit)
"Q3 Which team scores more runs, as predicted by our model?"
2*0.371+4*0.519+0.771+1.44
0.371+6*0.519+2*0.771+1.24

# Since the 1980s, sabermetricians have used a summary statistic different from
# batting average to evaluate players. They realized walks were important and 
# that doubles, triples, and HRs, should be weighed more than singles. As a
# result, they proposed the following metric:
"  BB/PA + (Singles + 2Doubles + 3Triples + 4HR)/AB
They called this on-base-percentage plus slugging percentage (OPS)"

"Q4 The on-base-percentage plus slugging percentage (OPS) metric gives the most weight to:"
HR

"Q5 What statistical concept properly explains the “sophomore slump”?"
# A sophomore slump or sophomore jinx or sophomore jitters refers to an instance in 
# which a second, or sophomore, effort fails to live up to the standards of the first effort.
Regression to the mean

"Q6 In our model of time vs. observed_distance in the video "'Measurement Error Models'", 
the randomness of our data was due to:"
# In these applications, it is common to have a non-random covariate, such as time, 
# and randomness is introduced from measurement error rather than sampling or natural variability.

"Q7 "
he measurement error is random
The measurement error is independent
The measurement error has the same distribution for each time  i

"Q8 Which of the following scenarios would violate an assumption of our measurement error model?"
There was one position where it was particularly difficult to see the dropped ball



# Use the Teams data frame from the Lahman package. Fit a multivariate linear regression 
# model to obtain the effects of BB and HR on Runs (R) in 1971. Use the tidy() function 
# in the broom package to obtain the results in a data frame.
library(Lahman); library(broom)
data = Teams %>%
   filter(yearID == 1971) %>%
   mutate(BBG = BB/G, HRG = HR/G, RG = R/G)
"Q9a effects"
lm(data$RG ~ data$HRG + data$BBG) %>%
   tidy(conf.int = T)

"Q9b Interpret the p-values for the estimates using a cutoff of 0.05."
HR has a significant effect on runs, but the evidence is not strong enough to suggest BB also does.


# Repeat the above exercise to find the effects of BB and HR on runs (R) for every 
# year from 1961 to 2018 using do() and the broom package.
# Make a scatterplot of the estimate for the effect of BB on runs over time and add
# a trend line with confidence intervals.
data2 = Teams %>%
   filter(yearID %in% 1961:2018) %>%
   group_by(yearID) 
data2 %>%
   do(tidy(lm(R ~ HR + BB, data = .))) %>%
   filter(term == "BB") %>%
   ggplot(aes(yearID, estimate)) +
   geom_point() + 
   geom_smooth(method = "lm")

"Q10"
The effect of BB on runs has increased unsubmitted  over time.

"Q11 Fit a linear model on the results from Question 10 to determine the effect 
of year on the impact of BB."
data2 %>%
   do(tidy(lm(R ~ HR + BB, data = .))) %>%
   filter(term == "BB") %>%
   lm(estimate ~ yearID, data = .) %>%
   tidy()





# Assessment: Linear Models -----------------------------------------------

# Load the Lahman library. The Teams data frame contains an attendance column. This
# is the total attendance for the season. To calculate average attendance, divide 
# by the number of games played, as follows:

library(broom); library(Lahman)
Teams_small <- Teams %>% 
   filter(yearID %in% 1961:2001) %>% 
   mutate(avg_attendance = attendance/G)

"Q1a Use runs (R) per game to predict average attendance"
Teams_small %>%
   mutate(R = R/G) %>%
   lm(avg_attendance ~ R, data = .) %>%
   tidy()
"Use home runs (HR) per game to predict average attendance."
Teams_small %>%
   mutate(HR = HR/G) %>%
   lm(avg_attendance ~ HR, data = .) %>%
   tidy()

"Q1b Use number of wins to predict average attendance; do not normalize for number of games"
Teams_small %>%
   lm(avg_attendance ~ W, data = .) %>%
   tidy()

"Q1c Use year to predict average attendance."
Teams_small %>%
   lm(avg_attendance ~ yearID, data = .) %>%
   tidy()

# Game wins, runs per game and home runs per game are positively correlated with 
# attendance. We saw in the course material that runs per game and home runs per 
# game are correlated with each other. Are wins and runs per game or wins and home
# runs per game correlated?

"Q2 What is the correlation coefficient for wins and runs per game?
What is the correlation coefficient for wins and home runs per game?"
Teams_small %>%
   mutate(R = R/G) %>%
   select(R, W) %>%
   cor()
Teams_small %>%
   mutate(HR = HR/G) %>%
   select(HR, W) %>%
   cor()

"Stratify Teams_small by wins: divide number of wins by 10 and then round to the
nearest integer. Keep only strata 5 through 10, which have 20 or more data points."
data = Teams_small %>%
   mutate(W = round(0.1*W, 0)) %>%
   filter(W %in% 5:10) %>%
   group_by(W) %>%
   filter(n() >= 20)
   
"Q3a How many observations are in the 8 win strata?"
sum(data$W == 8)
data$W %>% as.factor() %>% summary()

"Q3b Calculate the slope of the regression line predicting average attendance 
given runs per game for each of the win strata."
data %>% 
   mutate(avg_attendance = attendance/G,
          R = R/G) %>%
   do(tidy(lm(avg_attendance~R, data = .))) %>%
   arrange(estimate)
"Calculate the slope of the regression line predicting average attendance given
HR per game for each of the win strata."
data %>% 
   mutate(avg_attendance = attendance/G,
          HR = HR/G) %>%
   do(tidy(lm(avg_attendance~HR, data = .))) %>%
   filter(term == "HR") %>%
   arrange(estimate)
# Across all win strata, runs per game are positively correlated with average attendance. correct
# Home runs per game have the strongest effect on attendance when a team does not win many games. correct
# Among teams with similar numbers of wins, teams with more home runs per game have larger average attendance. correct


"Q4 Fit a multivariate regression determining the effects of runs per game, home runs 
per game, wins, and year on average attendance. Use the original Teams_small wins
column, not the win strata from question 3."
multi = Teams_small %>%
   mutate(R = R/G, HR = HR/G, 
          avg_attendance = attendance/G) %>%
   lm(avg_attendance ~ R + HR + W + yearID, data = .) %>%
   tidy()
multi

"Q5 Use the multivariate regression model from Question 4. Suppose a team averaged
5 runs per game, 1.2 home runs per game, and won 80 games in a season."
fit = Teams_small %>%
   mutate(R = R/G, HR = HR/G, 
          avg_attendance = attendance/G) %>%
   lm(avg_attendance ~ R + HR + W + yearID, data = .)
predict(fit, newdata = rbind(c(5,1.2,80,2002),
                                        c(5,1.2,80,1960)) %>%
                     as.data.frame() %>%
                     setNames(c("R", "HR", "W", "yearID")))


"Q5 Use your model from Question 4 to predict average attendance for teams in 
2002 in the original Teams data frame.
What is the correlation between the predicted attendance and actual attendance?"
forecast = Teams %>% 
   filter(yearID == 2002) %>%
   mutate(R = R/G, HR = HR/G) %>%
   predict(fit, newdata = .)
cor(Teams %>% filter(yearID == 2002) %>% .$attendance,
    forecast)   






