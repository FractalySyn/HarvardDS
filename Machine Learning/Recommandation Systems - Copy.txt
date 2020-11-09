rm(list = ls())
libs = c("tidyverse", "ggplot2", "imager", "broom", "caret", "dslabs", 
         "matrixStats", "purrr", "rpart", "randomForest", "lubridate")
lapply(libs, library, character.only = TRUE)
options(digits = 3)
im = function(url) plot(imager::load.image(url), axes = F)
seed = function(x) set.seed(x, sample.kind = "Rounding")




"Recommendation systems use ratings that users have given items to make specific recommendations.
Companies that sell many products to many customers and permit these
customers to rate their products, like Amazon, are able to collect massive datasets that can
be used to predict what rating a particular user will give a specific item. Items for which a
high rating is predicted for a given user are then recommended to that user."

# Netflix uses a recommendation system to predict how many stars a user will give a specific
# movie. One star suggests it is not a good movie, whereas five stars suggests it is an excellent
# movie. Here, we provide the basics of how these recommendations are made, motivated by
# some of the approaches taken by the winners of the Netflix challenges

"In October 2006, Netflix offered a challenge to the data science community: improve our recommendation
algorithm by 10% and win a million dollars. In September 2009, the winners
were announced. You can read a good summary of how the winning algorithm was put together
here: http://blog.echen.me/2011/10/24/winning-the-netflix-prize-a-summary/ and
a more detailed explanation here: http://www.netflixprize.com/assets/GrandPrize2009_
BPC_BellKor.pdf. We will now show you some of the data analysis strategies used by
the winning team."





# Movielens data ----------------------------------------------------------

"The Netflix data is not publicly available, but the GroupLens research lab generated their
own database with over 20 million ratings for over 27,000 movies by more than 138,000
users. We make a small subset of this data available via the dslabs package:"
data("movielens")
movielens %>% as_tibble()
# Each row represents a rating given by one user to one movie.

"We can see the number of unique users that provided ratings and how many unique movies
were rated:"
movielens %>% summarise(n_users = n_distinct(userId),
                        n_movies = n_distinct(movieId))

# If we multiply those two numbers, we get a number larger than 5 million, yet our data table
# has about 100,000 rows. This implies that not every user rated every movie. So we can think
# of these data as a very large matrix, with users on the rows and movies on the columns,
# with many empty cells. The gather function permits us to convert it to this format, but if
# we try it for the entire matrix, it will crash R. 
"userId | Forrest Gump | Pulp Fiction | Shawshank Redemption | Silence of the Lambs
   13        5.0             3.5                4.5                     NA
   15        1.0             5.0                2.0                     5.0
........."


# You can think of the task of a recommendation system as filling in the NAs in the table above.
# To see how sparse the matrix is, here is the matrix for a random sample of 100 movies and
# 100 users with yellow indicating a user/movie combination for which we have a rating.
im("https://rafalab.github.io/dsbook/book_files/figure-html/sparsity-of-movie-recs-1.png")

"This machine learning challenge is more complicated than what we have studied up to now
because each outcome Y has a different set of predictors. To see this, note that if we are
predicting the rating for movie i by user u, in principle, all other ratings related to movie
i and by user u may be used as predictors, but different users rate different movies and
a different number of movies. Furthermore, we may be able to use information from other
movies that we have determined are similar to movie i or from users determined to be similar
to user u. In essence, the entire matrix can be used as predictors for each cell."

# The first thing we notice is that some movies get rated more than others. Below is the
# distribution. This should not surprise us given that there are blockbuster movies watched
# by millions and artsy, independent movies watched by just a few. Our second observation is
# that some users are more active than others at rating movies:
im("https://rafalab.github.io/dsbook/book_files/figure-html/movie-id-and-user-hists-1.png")







# Recommendation systems as a machine learning challenge ------------------

"To see how this is a type of machine learning, notice that we need to build an algorithm
with data we have collected that will then be applied outside our control, as users look for
movie recommendations. So let’s create a test set to assess the accuracy of the models we
implement."
seed(755)
index = createDataPartition(y  = movielens$rating, p = 0.2, list = F)
train_set = movielens[-index, ]; test_set = movielens[index, ]

# To make sure we don’t include users and movies in the test set that do not appear in the
# training set, we remove these entries using the semi_join function:
test_set = test_set %>%
   semi_join(train_set, by = "movieId") %>%
   semi_join(train_set, by = "userId")






# Loss function -----------------------------------------------------------

"The Netflix challenge used the typical error loss: they decided on a winner based on the
residual mean squared error (RMSE) on a test set."

# Remember that we can interpret the RMSE similarly to a standard deviation: it is the
# typical error we make when predicting a movie rating. If this number is larger than 1, it
# means our typical error is larger than one star, which is not good.

"Let’s write a function that computes the RMSE for vectors of ratings and their corresponding
predictors:"
RMSE = function(actual, predicted) sqrt(mean((actual - predicted)^2))






# A first model -----------------------------------------------------------

"Let’s start by building the simplest possible recommendation system: we predict the same
rating for all movies regardless of user. What number should this prediction be? We can
use a model based approach to answer this. A model that assumes the same rating for all
movies and users with all the differences explained by random variation would look like this:
Yu,i = μ + εu,i"

# We know that the estimate that minimizes the RMSE is the
# least squares estimate of μ and, in this case, is the average of all ratings
mu_hat = mean(train_set$rating); mu_hat

"If we predict all unknown ratings with ˆμ we obtain the following RMSE"
naive_rmse = RMSE(test_set$rating, mu_hat); naive_rmse
"Keep in mind that if you plug in any other number, you get a higher RMSE"

# From looking at the distribution of ratings, we can visualize that this is the standard deviation
# of that distribution. We get a RMSE of about 1. To win the grand prize of $1,000,000,
# a participating team had to get an RMSE of about 0.857. So we can definitely do better!

"As we go along, we will be comparing different approaches. Let’s start by creating a results
table with this naive approach:"
rmse_results = tibble(method = "Average rating", RMSE = naive_rmse)





# Modeling movie effects --------------------------------------------------

"We know from experience that some movies are just generally rated higher than others. This
intuition, that different movies are rated differently, is confirmed by data. We can augment
our previous model by adding the term bi to represent average ranking for movie i:
Yu,i = μ + bi + εu,i"

# Statistics textbooks refer to the bs as effects. However, in the Netflix challenge papers, they
# refer to them as “bias”, thus the b notation.

"We can again use least squares to estimate the bi in the following way:"
"Don't run"
fit_ls = lm(rating ~ as.factor(movieId), data = movielens)

# Because there are thousands of bi as each movie gets one, the lm() function will be very
# slow here. We therefore don’t recommend running the code above. But in this particular
# situation, we know that the least squares estimate ˆbi is just the average of Yu,i − ˆμ for each
# movie i. So we can compute them this way
movie_avgs = train_set %>%
   group_by(movieId) %>%
   summarise(b_i = mean(rating - mu_hat))

"We can see that these estimates vary substantially"
qplot(b_i, data = movie_avgs, bins = 10, color = I("black"))
# Remember ˆμ = 3.5 so a bi = 1.5 implies a perfect five star rating.

predicted_rating = mu_hat + test_set %>%
   left_join(movie_avgs, by = "movieId") %>%
   pull(b_i)
RMSE(predicted_rating, test_set$rating)

"We already see an improvement. But can we make it better?"








# User effects ------------------------------------------------------------

"Let’s compute the average rating for user u for those that have rated over 100 movies"
train_set %>%
   group_by(userId) %>%
   summarize(b_u = mean(rating)) %>%
   filter(n()>=100) %>%
   ggplot(aes(b_u)) +
   geom_histogram(bins = 30, color = "black")

"Notice that there is substantial variability across users as well: some users are very cranky
and others love every movie. This implies that a further improvement to our model may be:
Yu,i = μ + bi + bu + εu,i"

# To fit this model, we could again use lm like this:
lm(rating ~ as.factor(movieId) + as.factor(userId))

"but, for the reasons described earlier, we won’t. Instead, we will compute an approximation
by computing ˆμ andˆbi and estimatingˆbu as the average of yu,i − ˆμ −ˆbi:"
user_avgs <- train_set %>%
   left_join(movie_avgs, by='movieId') %>%
   group_by(userId) %>%
   summarize(b_u = mean(rating - mu_hat - b_i))

# We can now construct predictors and see how much the RMSE improves
predicted_ratings <- test_set %>%
   left_join(movie_avgs, by='movieId') %>%
   left_join(user_avgs, by='userId') %>%
   mutate(pred = mu_hat + b_i + b_u) %>%
   pull(pred)
RMSE(predicted_ratings, test_set$rating)





# Assessment --------------------------------------------------------------

data("movielens")

"Q1 Compute the number of ratings for each movie and then plot it against the year 
the movie came out. Use the square root transformation on the counts."
dat = movielens %>%
   group_by(year) %>%
   mutate(n = n()) 
dat %>%
   ggplot(aes(year)) +
   geom_bar() +
   scale_y_sqrt()
dat %>% filter(n > 6000)

"Q2 Among movies that came out in 1993 or later, select the top 25 movies with the 
highest average number of ratings per year (n/year), and caculate the average rating 
of each of them. To calculate number of ratings per year, use 2018 as the end year."
movielens %>%
   filter(year %in% 1993:2018) %>%
   group_by(movieId) %>%
   mutate(n_year = n()/(2018-min(year))) %>%
   summarise(avg = mean(rating), n = n_year, title = title) %>%
   unique() %>%
   arrange(desc(n)) %>%
   .[1:25, ]

"Q3 From the table constructed in Q2, we can see that the most frequently rated movies
tend to have above average ratings. This is not surprising: more people watch popular 
movies. To confirm this, stratify the post-1993 movies by ratings per year and compute
their average ratings. To calculate number of ratings per year, use 2018 as the end year.
Make a plot of average rating versus ratings per year and show an estimate of the trend."
movielens %>%
   filter(year %in% 1993:2018) %>%
   group_by(movieId) %>%
   mutate(n_year = n()/(2018-min(year))) %>%
   summarise(avg = mean(rating), n = n_year, title = title) %>%
   unique() %>%
   ggplot(aes(n, avg)) +
   geom_point()

"Q4 Suppose you are doing a predictive analysis in which you need to fill in the 
missing ratings with some value.
Given your observations in the exercise in Q3, which of the following strategies 
would be most appropriate?"
Because a lack of ratings is associated with lower ratings, it would be most 
appropriate to fill in the missing value with a lower value than the average. 
You should try out different values to fill in the missing value and evaluate 
prediction in a test set.

"Q5 The movielens dataset also includes a time stamp. This variable represents the 
time and data in which the rating was provided. The units are seconds since January 
1, 1970. Create a new column date with the date."
movielens = movielens %>%
   mutate(date = as_datetime(timestamp))

"Q6 Compute the average rating for each week and plot this average against date."
movielens %>%
   mutate(week = round_date(date, "week")) %>%
   group_by(week) %>%
   summarise(date = week, avg_rating = mean(rating)) %>%
   ggplot(aes(date, avg_rating)) +
   geom_point()

"Q8 The movielens data also has a genres column. This column includes every genre 
that applies to the movie. Some movies fall under several genres. Define a category
as whatever combination appears in this column. Keep only categories with more 
than 1,000 ratings. Then compute the average and standard error for each category.
Plot these as error bar plots."
movielens %>%
   group_by(genres) %>%
   mutate(n = n()) %>%
   filter(n > 1000) %>%
   summarise(avg = mean(rating), sd = sd(rating)) %>%
   ggplot(aes(genres, avg)) +
   geom_bar(stat = "identity")
















