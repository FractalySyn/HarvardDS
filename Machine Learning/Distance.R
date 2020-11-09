rm(list = ls())
libs = c("tidyverse", "ggplot2", "imager", "broom", "caret", "dslabs", "matrixStats")
lapply(libs, library, character.only = TRUE)
options(digits = 3)




"Many of the analyses we perform with high-dimensional data relate directly or indirectly
to distance. Most clustering and machine learning techniques rely on being able to define
distance between observations, using features or predictors"




# Euclidean distance ------------------------------------------------------

"The Euclidean distance between A and B is simply:
dist(A,B) = sqrt((Ax − Bx)2 + (Ay − By)2))"

"This definition applies to the case of one dimension, in which the distance between two
numbers is simply the absolute value of their difference. So if our two one-dimensional
numbers are A and B, the distance is:
dist(A,B) = sqrt((A − B)2) = |A − B|"





# Distance in higher dimensions -------------------------------------------

if(!exists("mnist")) mnist <- read_mnist()
set.seed(1995, sample.kind = "Rounding")
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]

"For the purposes of, for example, smoothing, we are interested in describing distance between
observation; in this case, digits. Later, for the purposes of selecting features, we might also
be interested in finding pixels that behave similarly across samples."

# To define distance, we need to know what points are since mathematical distance is computed
# between points. With high dimensional data, points are no longer on the Cartesian plane.
# Instead, points are in higher dimensions. We can no longer visualize them and need to think
# abstractly. For example, predictors Xi are defined as a point in 784 dimensional space:
#    Xi = t(xi,1, . . . , xi,784).

"Once we define points this way, the Euclidean distance is defined very similarly as it was
for two dimensions. For example, the distance between the predictors for two observations,
say observations i = 1 and i = 2, is:
      dist(1, 2) = sqrt( sum[j in 1:784] ((x1,j − x2,j)^2) ) "



# The labels for the first three observations are:
y[1:3]
#> [1] 2 7 2
# The vectors of predictors for each of these observations are:
x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]
"The first two numbers are seven and the third one is a 2. We expect the distances between
the same number:"
sqrt(sum((x_1 - x_3)^2))
#> [1] 1576
"to be smaller than between different numbers:"
sqrt(sum((x_1 - x_2)^2))
#> [1] 3036
sqrt(sum((x_2 - x_3)^2))
#> [1] 3043
"As expected, the 7s are closer to each other."

# "A faster way to compute this is using matrix algebra:"
sqrt(crossprod(x_1 - x_2))

"We can also compute all the distances at once relatively quickly using the function dist,
which computes the distance between each row and produces an object of class dist:"
d = dist(x)
class(d)

# There are several machine learning related functions in R that take objects of class dist
# as input. To access the entries using row and column indices, we need to coerce it into a
# matrix. We can see the distance we calculated above like this
as.matrix(d)[1:3, 1:3]
image(as.matrix(d))

"If we order this distance by the labels, we can see that, in general, the twos are closer to
each other and the sevens are closer to each other:"
image(as.matrix(d)[order(y), order(y)])



# We can also compute distances between predictors. If N is the number of observations, the
# distance between two predictors, say 1 and 2, is:
"   dist(1, 2) = sqrt( sum[i in 1:N] ((xi,1 − xi,2)^2) )
To compute the distance between all pairs of the 784 predictors, we can transpose the matrix
first and then use dist:"
d <- dist(t(x))


# Predictor Space ---------------------------------------------------------

"Predictor space is a concept that is often used to describe machine learning algorithms. The
term space refers to a mathematical definition that we don’t describe in detail here. Instead,
we provide a simplified explanation to help understand the term predictor space when used
in the context of machine learning algorithms."

# The predictor space can be thought of as the collection of all possible vectors of predictors
# that should be considered for the machine learning challenge in question. Each member of
# the space is referred to as a point. For example, in the 2 or 7 dataset, the predictor space
# consists of all pairs (x1, x2) such that both x1 and x2 are within 0 and 1. This particular
# space can be represented graphically as a square. In the MNIST dataset the predictor space
# consists of all 784-th dimensional vectors with each vector element an integer between 0
# and 256. An essential element of a predictor space is that we need to define a function that
# provides the distance between any two points. In most cases we use Euclidean distance,
# but there are other possibilities. A particular case in which we can’t simply use Euclidean
# distance is when we have categorical predictors.

"Defining a predictor space is useful in machine learning because we do things like define
neighborhoods of points, as required by many smoothing techniques. For example, we can
define a neighborhood as all the points that are within 2 units away from a predefined
center. If the points are two-dimensional and we use Euclidean distance, this neighborhood
is graphically represented as a circle with radius 2. In three dimensions the neighborhood is
a sphere. We will soon learn about algorithms that partition the space into non-overlapping
regions and then make different predictions for each region using the data in the region"








# Assessment --------------------------------------------------------------

data("tissue_gene_expression")

# This dataset includes a matrix x:
dim(tissue_gene_expression$x)

# This matrix has the gene expression levels of 500 genes from 189 biological samples
# representing seven different tissues. The tissue type is stored in y:
table(tissue_gene_expression$y)

"Q1 Which of the following lines of code computes the Euclidean distance between each 
observation and stores it in the object d?"
d <- dist(tissue_gene_expression$x)
as.matrix(d)[1,2]
as.matrix(d)[39,40]
as.matrix(d)[73,74]
Yes, the samples from the same tissue type are closest to each other.


"Q3 Make a plot of all the distances using the image() function to see if the pattern 
you observed in Q2 is general."
image(as.matrix(d))








