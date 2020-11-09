rm(list = ls())
libs = c("tidyverse", "ggplot2", "imager", "broom", "caret", "dslabs", 
         "matrixStats", "purrr", "rpart", "randomForest", "lubridate")
lapply(libs, library, character.only = TRUE)
options(digits = 3)
im = function(url) plot(imager::load.image(url), axes = F)
seed = function(x) set.seed(x, sample.kind = "Rounding")





"Matrix factorization is a widely used concept in machine learning. It is very much related
to factor analysis, singular value decomposition (SVD), and principal component analysis
(PCA). Here we describe the concept in the context of movie recommendation systems."

"We have described how the model:
                        Yu,i = μ + bi + bu + εu,i
accounts for movie to movie differences through the bi and user to user differences through
the bu. But this model leaves out an important source of variation related to the fact that
groups of movies have similar rating patterns and groups of users have similar rating patterns
as well. We will discover these patterns by studying the residuals:
                        ru,i = yu,i −ˆbi −ˆbu"

# To see this, we will convert the data into a matrix so that each user gets a row, each movie
# gets a column, and yu,i is the entry in row u and column i. For illustrative purposes, we will
# only consider a small subset of movies with many ratings and users that have rated many
# movies. We also keep Scent of a Woman (movieId == 3252) because we use it for a specific
# example:
train_small = movielens %>%
   group_by(movieId) %>%
   filter(n() >= 50 | movieId == 3252) %>%
   ungroup() %>%
   group_by(userId) %>%
   filter(n() >= 50) %>%
   ungroup()
y = train_small %>%
   select(userId, movieId, rating) %>%
   spread(movieId, rating) %>%
   as.matrix()
head(y %>% as_tibble())

# We add row names and column names:
rownames(y) = y[, 1]
y = y[, -1]
movie_titles = movielens %>%
   select(movieId, title) %>%
   distinct()
colnames(y) = with(movie_titles, title[match(colnames(y), movieId)])

"and convert them to residuals by removing the column and row effects:"
y = sweep(y, 2, colMeans(y, na.rm = T))
y = sweep(y, 1, rowMeans(y, na.rm=TRUE))
head(y %>% as_tibble())

"If the model above explains all the signals, and the ε are just noise, then the residuals for
different movies should be independent from each other. But they are not. Here are some
examples:"
m1 = "Godfather, The"
m2 = "Godfather: Part II, The"
p1 = qplot(y[, m1], y[, m2], xlab = m1, ylab = m2)
m3 = "Goodfellas"
p2 = qplot(y[, m1], y[, m3], xlab = m1, ylab = m3)
gridExtra::grid.arrange(p1, p2, ncol = 2)

# This plot says that users that liked The Godfather more than what the model expects them
# to, based on the movie and user effects, also liked The Godfather II more than expected. A
# similar relationship is seen when comparing The Godfather and Goodfellas. Although not as
# strong, there is still correlation

"By looking at the correlation between movies, we can see a pattern (we rename the columns
to save print space)"
x = y[, c(m1, m2, m2)]
short_names <- c("Godfather", "Godfather2", "Goodfellas")
colnames(x) = short_names
cor(x, use = "pairwise.complete") %>% 
   knitr::kable()

# There seems to be people that like romantic comedies more than expected, while others that
# like gangster movies more than expected.
# These results tell us that there is structure in the data. But how can we model this?






# Factors analysis --------------------------------------------------------

m4 <- "You've Got Mail" 
m5 <- "Sleepless in Seattle" 
seed(1)
Q = matrix(c(1,1,1,-1,-1), ncol = 1)
rownames(Q) = c(m1, m2, m3, m4, m5)
P = matrix(rep(c(2,0,-2), c(3,5,4)), ncol = 1)
rownames(P) = 1:nrow(P)

"Here is an illustration, using a simulation, of how we can use some structure to predict the
ru,i. Suppose our residuals r look like this:"
X = jitter(P%*%t(Q)) %>% round(1)
X %>% knitr::kable(align = "c")

# There seems to be a pattern here. In fact, we can see very strong correlation patterns:
cor(X)
# We can see a pattern. Movies are narrowed down to two groups : romance and gangster
t(Q)
# And we can reduce users to three groups 
t(P)
# those that like gangster movies and dislike romance movies (coded as 2), those that like
# romance movies and dislike gangster movies (coded as -2), and those that don’t care (0)

"The main point here is that we can almost reconstruct r, which has 60 values, with
a couple of vectors totaling 17 values. The two vectors p and q can be used to form
the matrix of 60 values. We reduced to factors
                  res ~ p*q  
This implies that we can explain more variability by modifying our previous model for movie
recommendations to:
                  Yu,i = μ + bi + bu + puqi + εu,i"

# However, we motivated the need for the pu*qi term with a simple simulation. The structure
# found in data is usually more complex. For example, in this first simulation we assumed
# there were was just one factor p that determined which of the two genres movie u belongs
# to. But the structure in our movie data seems to be much more complicated than gangster
# movie versus romance. We may have many other factors. Here we present a slightly more
# complex simulation. We now add a sixth movie.
seed(1)
m6 = "Scent of a Woman"
Q = cbind(c(1 , 1, 1, -1, -1, -1), 
          c(1 , 1, -1, -1, -1, 1))
rownames(Q) = c(m1, m2, m3, m4, m5, m6)
P = cbind(rep(c(2,0,-2), c(3,5,4)), 
           c(-1,1,1,0,0,1,1,1,0,-1,-1,-1))/2
rownames(P) = 1:nrow(X)
"This movie adds another factor, the admiration for Al Paccino"
X <- jitter(P%*%t(Q), factor = 1)
X %>% knitr::kable(align = "c")

# So we have two movie factors : gangster vs romance, Al Paccino movie or not
# And two sets of coefficients to describe the users
t(Q); t(P)

"The model with two factors has 36 parameters that can be used to explain much of the
variability in the 72 ratings:
         Yu,i = μ + bi + bu + pu,1q1,i + pu,2q2,i + εu,i"

# Note that in an actual data application, we need to fit this model to data. To explain the
# complex correlation we observe in real data, we usually permit the entries of p and q to
# be continuous values, rather than discrete ones as we used in the simulation. For example,
# rather than dividing movies into gangster or romance, we define a continuum. Also note
# that this is not a linear model and to fit it we need to use an algorithm other than the one
# used by lm to find the parameters that minimize the least squares. The winning algorithms
# for the Netflix challenge fit a model similar to the above and used regularization to penalize
# for large values of p and q, rather than using least squares. Implementing this approach is
# beyond the scope of this book.







# Connection to SVD and PCAConnection to SVD and PCA ----------------------

"The decomposition:     ru,i ≈ pu,1.q1,i + pu,2.q2,i
is very much related to SVD and PCA. SVD and PCA are complicated concepts, but one
way to understand them is that SVD is an algorithm that finds the vectors p and q that
permit us to rewrite the matrix r with m rows and n columns as:
         ru,i = pu,1.q1,i + pu,2.q2,i + · · · + pu,m.qm,i
with the variability of each term decreasing and with the ps uncorrelated. The algorithm also
computes this variability so that we can know how much of the matrices, total variability is
explained as we add new terms. This may permit us to see that, with just a few terms, we
can explain most of the variability."


# Let’s see an example with the movie data. To compute the decomposition, we will make the
# residuals with NAs equal to 0:
y[is.na(y)] = 0
pca = prcomp(y)

"The q vectors are called the principal components and they are stored in this matrix:"
dim(pca$rotation)

# While the p, or the user effects, are here
dim(pca$x)

"We can see the variability of each of the vectors"
qplot(1:nrow(pca$x), pca$sdev, xlab = "PC")

# "We also notice that the first two principal components are related to the structure in opinions
# about movies:"
im("https://rafalab.github.io/dsbook/book_files/figure-html/movies-pca-1.png")

"Just by looking at the top 10 in each direction, we see a meaningful pattern. The first PC
shows the difference between critically acclaimed movies on one side:"
#> [1] "Pulp Fiction" "Seven (a.k.a. Se7en)"
#> [3] "Fargo" "2001: A Space Odyssey"
#> [5] "Silence of the Lambs, The" "Clockwork Orange, A"
#> [7] "Taxi Driver" "Being John Malkovich"
#> [9] "Royal Tenenbaums, The" "Shining, The"
"and Hollywood blockbusters on the other:"
#> [1] "Independence Day (a.k.a. ID4)" "Shrek"
#> [3] "Spider-Man" "Titanic"
#> [5] "Twister" "Armageddon"
#> [7] "Harry Potter and the Sorcer..." "Forrest Gump"
#> [9] "Lord of the Rings: The Retu..." "Enemy of the State"
"While the second PC seems to go from artsy, independent films:"
#> [1] "Shawshank Redemption, The" "Truman Show, The"
#> [3] "Little Miss Sunshine" "Slumdog Millionaire"
#> [5] "Amelie (Fabuleux destin d'A..." "Kill Bill: Vol. 1"
#> [7] "American Beauty" "City of God (Cidade de Deus)"
#> [9] "Mars Attacks!" "Beautiful Mind, A"
"to nerd favorites:"
#> [1] "Lord of the Rings: The Two ..." "Lord of the Rings: The Fell..."
#> [3] "Lord of the Rings: The Retu..." "Matrix, The"
#> [5] "Star Wars: Episode IV - A N..." "Star Wars: Episode VI - Ret..."
#> [7] "Star Wars: Episode V - The ..." "Spider-Man 2"
#> [9] "Dark Knight, The" "Speed"

"Fitting a model that incorporates these estimates is complicated. For those interested in
implementing an approach that incorporates these ideas, we recommend trying the recommenderlab
package. The details are beyond the scope of this book."







# Assessment --------------------------------------------------------------

"In this exercise set, we will be covering a topic useful for understanding matrix 
factorization: the singular value decomposition (SVD). SVD is a mathematical result 
that is widely used in machine learning, both in practice and to understand the 
mathematical properties of some algorithms. This is a rather advanced topic and 
to complete this exercise set you will have to be familiar with linear algebra 
concepts such as matrix multiplication, orthogonal matrices, and diagonal matrices."

"The SVD tells us that we can decompose an  N×p  matrix Y with p<N as 
                  Y = U.D.t(V) 
with U and V orthogonal of dimensions N×p and p×p respectively and D a p×p diagonal
matrix with the values of the diagonal decreasing: 
                  d1,1≥d2,2≥…dp,p"

# In this exercise, we will see one of the ways that this decomposition can be useful. 
# To do this, we will construct a dataset that represents grade scores for 100 students
# in 24 different subjects. The overall average has been removed so this data represents
# the percentage point each student received above or below the average test score. So 
# a 0 represents an average grade (C), a 25 is a high grade (A+), and a -25 represents
# a low grade (F).
seed(1987)
n = 100; k = 8
sigma = 64 * matrix(c(1, 0.75, 0.5, 0.75, 1, 0.5, 0.5, 0.5, 1), 3, 3)
m = MASS::mvrnorm(n, rep(0, 3), sigma)
m = m[order(rowMeans(m), decreasing = T), ]
y = m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) = c(paste(rep("Math",k), 1:k, sep="_"),
                paste(rep("Science",k), 1:k, sep="_"),
                paste(rep("Arts",k), 1:k, sep="_"))

"Our goal is to describe the student performances as succinctly as possible. For 
example, we want to know if these test results are all just a random independent
numbers. Are all students just about as good? Does being good in one subject  
imply you will be good in another? How does the SVD help with all this? We will 
go step by step to show that with just three relatively small pairs of vectors 
we can explain much of the variability in this  100×24  dataset."


"Q1 You can visualize the 24 test scores for the 100 students by plotting an image:"
my_image = function(x, zlim = range(x), ...)
{
   colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
   cols = 1:ncol(x); rows = 1:nrow(x)
   image(cols, rows, t(x[rev(rows), , drop = F]), xaxt = "n", yaxt = "n",
                       xlab="", ylab="", col = colors, zlim = zlim, ...)
   abline(h = rows + 0.5, v = cols + 0.5)
   axis(side = 1, cols, colnames(x), las = 2)
}
my_image(y)
# The students that test well are at the top of the image and there seem to be three 
# groupings by subject.

"Q2 You can examine the correlation between the test scores directly like this:"
my_image(cor(y), zlim = c(-1, 1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
# There is correlation among all tests, but higher if the tests are in science and
# math and even higher within each subject.


"Remember that orthogonality means that  t(U).U  and  t(V).V  are equal to the identity matrix. 
This implies that we can also rewrite the decomposition as
            Y.V = U.D  or  t(U).Y = D.t(V)
We can think of  Y.V  and  t(U)V  as two transformations of  Y  that preserve the total 
variability of  Y  since  U  and  V  are orthogonal."

"Q3 Use the function svd() to compute the SVD of y. This function will return  U ,  V , 
and the diagonal entries of  D ."
s = svd(y)
names(s)
"You can check that the SVD works by typing:"
y_svd = s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))
"Compute the sum of squares of the columns of  Y  and store them in ss_y. Then compute
the sum of squares of columns of the transformed  YV  and store them in ss_yv. Confirm 
that sum(ss_y) is equal to sum(ss_yv)."
ss_y = colSums(y^2)
ss_yv = colSums((s$u %*% diag(s$d))^2)
sum(ss_y); sum(ss_yv)

"Q4 We see that the total sum of squares is preserved. This is because  V  is orthogonal. 
Now to start understanding how  YV  is useful, plot ss_y against the column number and 
then do the same for ss_yv"
plot(1:ncol(y), ss_y)
plot(1:ncol(y), ss_yv)
# ss_yv is decreasing and close to 0 for the 4th column and beyond.

"Q5 Now notice that we didn't have to compute ss_yv because we already have the answer. 
How? Remember that  YV=UD  and because  U  is orthogonal, we know that the sum of squares
of the columns of  UD  are the diagonal entries of  D  squared. Confirm this by plotting
the square root of ss_yv versus the diagonal entries of  D ."
plot(s$d, sqrt(ss_yv))

"Q6 So from the above we know that the sum of squares of the columns of  Y  (the total
sum of squares) adds up to the sum of s$d^2 and that the transformation  YV  gives us 
columns with sums of squares equal to s$d^2. Now compute the percent of the total 
variability that is explained by just the first three columns of  YV ."
sum(ss_yv[1:3]) / sum(ss_yv)

"Q7 Before we continue, let's show a useful computational trick to avoid creating the 
matrix diag(s$d). To motivate this, we note that if we write  U  out in its columns 
[U1,U2,…,Up]  then  UD  is equal to UD=[U1d1,1,U2d2,2,…,Updp,p] 
Use the sweep function to compute  UD  without constructing diag(s$d) or using matrix 
multiplication."
identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))

"Q8 We know that  U1d1,1 , the first column of  UD , has the most variability of all
the columns of  UD . Earlier we looked at an image of  Y  using my_image(y), in which
we saw that the student to student variability is quite large and that students that
are good in one subject tend to be good in all. This implies that the average (across 
all subjects) for each student should explain a lot of the variability. Compute the 
average score for each student, plot it against  U1d1,1 , and describe what you find."
avg_score = rowMeans(y)
u1d1 = (s$u %*% diag(s$d))[, 1]
plot(u1d1, avg_score)
# There is a linear relationship between the average score for each student and  U1d1,1 .

"Q9 "
im("https://github.com/FractalySyn/harvardXdatascience/raw/master/2020-08-21_19h00_05.png")
my_image(s$v)
# The first column is very close to being a constant, which implies that the first column 
# of YV is the sum of the rows of Y multiplied by some constant, and is thus proportional 
# to an average.
















