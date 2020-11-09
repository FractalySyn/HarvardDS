rm(list = ls())
libs = c("tidyverse", "ggplot2", "imager", "broom", "caret", "dslabs", 
         "matrixStats", "purrr", "rpart", "randomForest", "lubridate")
lapply(libs, library, character.only = TRUE)
options(digits = 3)
im = function(url) plot(imager::load.image(url), axes = F)
seed = function(x) set.seed(x, sample.kind = "Rounding")






# Code from previous courses ----------------------------------------------

data("movielens")
seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.2,
                                  list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

RMSE <- function(true_ratings, predicted_ratings){
   sqrt(mean((true_ratings - predicted_ratings)^2, na.rm = T))
}

mu_hat <- mean(train_set$rating)
naive_rmse <- RMSE(test_set$rating, mu_hat)
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
   group_by(movieId) %>% 
   summarize(b_i = mean(rating - mu))
predicted_ratings <- mu + test_set %>% 
   left_join(movie_avgs, by='movieId') %>%
   .$b_i
model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))

user_avgs <- test_set %>% 
   left_join(movie_avgs, by='movieId') %>%
   group_by(userId) %>%
   summarize(b_u = mean(rating - mu - b_i))
predicted_ratings <- test_set %>% 
   left_join(movie_avgs, by='movieId') %>%
   left_join(user_avgs, by='userId') %>%
   mutate(pred = mu + b_i + b_u) %>%
   .$pred
model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))




# Motivation --------------------------------------------------------------

"Despite the large movie to movie variation, our improvement in RMSE was only about 5%.
Let’s explore where we made mistakes in our first model, using only movie effects bi. Here
are the 10 largest mistakes:"
test_set %>%
   left_join(movie_avgs, by = "movieId") %>%
   mutate(residual = rating - (mu + b_i)) %>%
   arrange(desc(residual)) %>%
   slice(1:10) %>% pull(title)

# These all seem like obscure movies. Many of them have large predictions. Let’s look at
# the top 10 worst and best movies based onˆbi. First, let’s create a database that connects
# movieId to movie title:
movie_titles = movielens %>%
   select(movieId, title) %>%
   distinct()

"Here are the 10 best movies according to our estimate:"
movie_avgs %>%
   left_join(movie_titles, by = "movieId") %>%
   arrange(desc(b_i)) %>%
   slice(1:10) %>% pull(title)
"And here are the 10 worst"
movie_avgs %>%
   left_join(movie_titles, by = "movieId") %>%
   arrange(b_i) %>%
   slice(1:10) %>% pull(title)

# They all seem to be quite obscure. Let’s look at how often they are rated.
train_set %>% dplyr::count(movieId) %>%
   left_join(movie_avgs, by="movieId") %>%
   left_join(movie_titles, by="movieId") %>%
   arrange(desc(b_i)) %>%
   slice(1:10) %>%
   pull(n)
train_set %>% dplyr::count(movieId) %>%
   left_join(movie_avgs, by="movieId") %>%
   left_join(movie_titles, by="movieId") %>%
   arrange(b_i) %>%
   slice(1:10) %>%
   pull(n)


"The supposed “best” and “worst” movies were rated by very few users, in most cases just
1. These movies were mostly obscure ones. This is because with just a few users, we have
more uncertainty. Therefore, larger estimates of bi, negative or positive, are more likely."

# In previous sections, we computed standard error and constructed confidence intervals to
# account for different levels of uncertainty. However, when making predictions, we need one
# number, one prediction, not an interval. For this, we introduce the concept of regularization.

"Regularization permits us to penalize large estimates that are formed using small sample
sizes. It has commonalities with the Bayesian approach that shrunk predictions described
in Section 16.4."








# Penalized least squares -------------------------------------------------

"The general idea of penalized regression is to control the total variability of the 
movie effects: ∑(bi^2)
Specifically, instead of minimizing the least squares equation, we minimize an equation
that adds a penalty:
                     1/N ∑(yu,i - μ - bi)^2 + λ∑(bi^2)   "


# The first term is just least squares and the second is a penalty that gets larger when many
# bi are large. Using calculus we can actually show that the values of bi that minimize this
# equation are
"                   ^bi(λ) = 1/(λ+ni) ∑(Yu,i - ^μ)   
where ni is the number of ratings made for movie i. This approach will have our desired
effect: when our sample size ni is very large, a case which will give us a stable estimate,
then the penalty λ is effectively ignored since ni + λ ≈ ni. However, when the ni is small,
then the estimateˆbi(λ) is shrunken towards 0. The larger λ, the more we shrink"

# Let’s compute these regularized estimates of bi using λ = 3.
lambda = 3
mu = mean(train_set$rating)
movie_reg_avgs = train_set %>%
   group_by(movieId) %>%
   summarise(b_i = sum(rating - mu) / (n() + lambda), n_i = n())

"To see how the estimates shrink, let’s make a plot of the regularized estimates versus the
least squares estimates"
tibble(original = movie_avgs$b_i,
       regularized = movie_reg_avgs$b_i,
       n = movie_reg_avgs$n_i) %>%
   ggplot(aes(original, regularized, size = sqrt(n))) +
   geom_point(shape = 1, alpha = 0.5) +
   geom_abline(color = "red", lwd = 1.5)
# We see that for smaller number of ratings the bi effect is regularized
# i.e. they get smaller or bigger in order to atenuate the bias and converge
# to the average


"Now, let’s look at the top 10 best movies based on the penalized estimatesˆbi(λ):"
train_set %>%
   dplyr::count(movieId) %>%
   left_join(movie_reg_avgs, by = "movieId") %>%
   left_join(movie_titles, by = "movieId") %>%
   arrange(desc(b_i)) %>%
   slice(1:10) %>% pull(title)

# These make much more sense! These movies are watched more and have more ratings. Here
# are the top 10 worst movies:
train_set %>%
   dplyr::count(movieId) %>%
   left_join(movie_reg_avgs, by = "movieId") %>%
   left_join(movie_titles, by = "movieId") %>%
   arrange(b_i) %>%
   slice(1:10) %>% pull(title)

"Do we improve our results?"
forecast = test_set %>%
   left_join(movie_reg_avgs, by = "movieId") %>%
   mutate(pred = mu + b_i) %>% .$pred
RMSE(forecast, test_set$rating) 
# "The penalized estimates provide a large improvement over the least squares estimates."





# Choosing the penalty terms ----------------------------------------------

"Note that λ is a tuning parameter. We can use cross-validation to choose it."
lambdas = seq(0, 10, 0.05)
just_the_sum <- train_set %>%
   group_by(movieId) %>%
   summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){
   predicted_ratings <- test_set %>%
      left_join(just_the_sum, by='movieId') %>%
      mutate(b_i = s/(n_i+l)) %>%
      mutate(pred = mu + b_i) %>%
      pull(pred)
   return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)
lambdas[which.min(rmses)]

# "While we show this as an illustration, in practice we should be using full crossvalidation
# just on the train set, without using the test set until the final assessment. The
# test set should never be used for tuning."

"We can use regularization for the estimate user effects as well. We are minimizing
         1/N ∑(yu,i - μ - bi - bu)^2 + λ(∑bi^2 + ∑bu^2)  "

# The estimates that minimize this can be found similarly to what we did above. Here we use
# cross-validation to pick a λ:
lambdas = seq(0, 10, 0.1)
rmses = sapply(lambdas, function(l){
   b_i = train_set %>%
      group_by(movieId) %>%
      summarise(b_i = sum(rating - mu) / (n() + l))
   b_u = train_set %>%
      left_join(b_i, by = "movieId") %>%
      group_by(userId) %>%
      summarise(b_u = sum(rating - b_i - mu) / (n() + l))
   forecast = test_set %>%
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      mutate(pred = mu + b_i + b_u) %>%
      .$pred
   return(RMSE(forecast, test_set$rating))
}) 
qplot(lambdas, rmses)
"For the full model, the optimal λ is:"
lambda = lambdas[which.min(rmses)]
lambda






# Assessment --------------------------------------------------------------

options(digits = 7)

"An education expert is advocating for smaller schools. The expert bases this 
recommendation on the fact that among the best performing schools, many are small
schools. Let's simulate a dataset for 1000 schools. First, let's simulate the 
number of students in each school, using the following code:"
seed(1986)
n = round(2^rnorm(1000, 8, 1))

"Now let's assign a true quality for each school that is completely independent 
from size. This is the parameter we want to estimate in our analysis. The true 
quality can be assigned using the following code:"
seed(1)
mu = round(80 + 2*rt(1000, 5))
range(mu)
schools = data.frame(id = paste("PS", 1:1000),
                     size = n,
                     quality = mu,
                     rank = rank(-mu))
head(schools)
schools %>% top_n(10, quality) %>% arrange(desc(quality))

"Now let's have the students in the school take a test. There is random variability 
in test taking, so we will simulate the test scores as normally distributed with the 
average determined by the school quality with a standard deviation of 30 percentage 
points. This code will simulate the test scores:"
scores = sapply(1:nrow(schools), function(i){
   scores = rnorm(schools$size[i], schools$quality[i], 30)
   scores
})
schools = schools %>%
   mutate(score = sapply(scores, mean))

"Q1 What are the top schools based on the average score? Show just the ID, size, and 
the average score."
top10 = schools %>%
   arrange(desc(score)) %>%
   select(id, size, score) %>%
   slice(1:10)

"Q2 Compare the median school size to the median school size of the top 10 schools 
based on the score."
median(schools$size)
median(top10$size)

"Q3 According to this analysis, it appears that small schools produce better test
scores than large schools. Four out of the top 10 schools have 100 or fewer students.
But how can this be? We constructed the simulation so that quality and size were 
independent. Repeat the exercise for the worst 10 schools."
bad10 = schools %>%
   arrange(score) %>%
   select(id, size, score) %>%
   slice(1:10)
median(bad10$size)

"Q4 From this analysis, we see that the worst schools are also small. Plot the average
score versus school size to see what's going on. Highlight the top 10 schools based on
the true quality."
schools %>%
   ggplot(aes(size, score, color = quality)) +
   geom_point() 
# The standard error of the score has larger variability when the school is smaller, 
# which is why both the best and the worst schools are more likely to be small.
schools %>% ggplot(aes(size, score)) +
   geom_point(alpha = 0.5) +
   geom_point(data = filter(schools, rank<=10), col = 2)


"Let's use regularization to pick the best schools. Remember regularization shrinks 
deviations from the average towards 0. To apply regularization here, we first need 
to define the overall average for all schools, using the following code:"
overall = mean(sapply(scores, mean))
# Then, we need to define, for each school, how it deviates from that average.

"Q5 Write code that estimates the score above the average for each school but dividing 
by  n+α  instead of  n , with  n  the school size and  α  a regularization parameter.
Try  α=25 ."
alpha = 25
bias = sapply(scores, function(x){
   sum(x - overall) / (length(x) + alpha)
})
schools = schools %>%
   mutate(reg_score = overall + bias)
top10 = schools %>%
   arrange(desc(reg_score)) %>%
   select(id, size, reg_score) %>%
   slice(1:10)
top10

"Q6 Notice that this improves things a bit. The number of small schools that are not
highly ranked is now lower. Is there a better  α ? Using values of  α  from 10 to 250, 
find the  α  that minimizes the RMSE."
RMSE <- function(true_ratings, predicted_ratings){
   sqrt(mean((true_ratings - predicted_ratings)^2, na.rm = T))
}
alphas = seq(10, 250)
rmses = sapply(alphas, function(alpha){
   bias = sapply(scores, function(x){
      sum(x - overall) / (length(x) + alpha)
   })
   RMSE(schools$quality, overall + bias)
})
plot(alphas, rmses)
alphas[which.min(rmses)]

"Q7 Rank the schools based on the average obtained with the best  α . Note that no 
small school is incorrectly included."
alpha = 135
bias = sapply(scores, function(x){
   sum(x - overall) / (length(x) + alpha)
})
schools = schools %>%
   mutate(reg_score = overall + bias)
top10 = schools %>%
   arrange(desc(reg_score)) %>%
   select(id, size, reg_score) %>%
   slice(1:10)
top10

"Q8 A common mistake made when using regularization is shrinking values towards 0 that 
are not centered around 0. For example, if we don't subtract the overall average before
shrinking, we actually obtain a very similar result. Confirm this by re-running the code
from the exercise in Q6 but without removing the overall mean."
rmses = sapply(alphas, function(alpha){
   bias = sapply(scores, function(x){
      sum(x) / (length(x) + alpha)
   })
   RMSE(schools$quality, bias)
})
plot(alphas, rmses)
alphas[which.min(rmses)]














