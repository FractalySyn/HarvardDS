rm(list = ls())
libs = c("tidyverse", "ggplot2", "imager", "broom", "caret", "dslabs", "matrixStats")
lapply(libs, library, character.only = TRUE)
options(digits = 3)



"Machine learning problems often involve datasets that are as large or larger than the MNIST
dataset. There is a variety of computational techniques and statistical concepts that are
useful for the analysis of large datasets. In this chapter we scratch the surface of these
techniques and concepts by describing matrix algebra, dimension reduction, regularization
and matrix factorization. We use recommendation systems related to movie ratings as a
motivating example."




# Matrix algebra ----------------------------------------------------------

# In machine learning, situations in which all predictors are numeric, or can be converted to
# numeric in a meaningful way, are common. The digits data set is an example: every pixel
# records a number between 0 and 255. Let’s load the data:
if(!exists("mnist")) mnist <- read_mnist()

"In these cases, it is often convenient to save the predictors in a matrix and the outcome
in a vector rather than using a data frame. You can see that the predictors are saved as a
matrix:"
class(mnist$train$images)

# This matrix represents 60,000 digits, so for the examples in this chapter, we will take a more
# manageable subset. We will take the first 1,000 predictors x and labels y:
x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]

"The main reason for using matrices is that certain mathematical operations needed to develop
efficient code can be performed using techniques from a branch of mathematics called
linear algebra. In fact, linear algebra and matrix notation are key elements of the language
used in academic papers describing machine learning techniques. We will not cover linear
algebra in detail here, but will demonstrate how to use matrices in R so that you can apply
the linear algebra techniques already implemented in base R or other packages."

# To motivate the use of matrices, we will pose five questions/challenges:
# 1. Do some digits require more ink than others? Study the distribution of the total pixel
# darkness and how it varies by digits.
# 2. Are some pixels uninformative? Study the variation of each pixel and remove predictors
# (columns) associated with pixels that don’t change much and thus can’t provide much
# information for classification.
# 3. Can we remove smudges? First, look at the distribution of all pixel values. Use this to
# pick a cutoff to define unwritten space. Then, set anything below that cutoff to 0.
# 4. Binarize the data. First, look at the distribution of all pixel values. Use this to pick a
# cutoff to distinguish between writing and no writing. Then, convert all entries into either 1
# or 0, respectively.
# 5. Scale each of the predictors in each entry to have the same average and standard deviation.
# To complete these, we will have to perform mathematical operations involving several variables.

"In R vectors don't have dimensions"
dim(y)
dim(as.matrix(y))





# Converting a vector to a matrix -----------------------------------------

my_vector <- 1:15
mat <- matrix(my_vector, 5, 3)
mat

mat_t <- matrix(my_vector, 3, 5, byrow = TRUE)
mat_t
identical(t(mat), mat_t)

# "The matrix function recycles values in the vector"
matrix(my_vector, 4, 5)

# if you select one column or one row, the result is no longer a matrix but a vector
x[, 1] %>% class()
# However, we can preserve the matrix class by using the argument drop=FALSE
x[, 1, drop = F] %>% class()

"To put the pixel intensities of our, say, 3rd entry, which is a 4 into grid, we can use:"
grid = matrix(x[3,], 28, 28) # there are 784 intensities i.e. 784 pixels per image

"To confirm that in fact we have done this correctly, we can use the function image, which
shows an image of its third argument. The top of this plot is pixel 1, which is shown at the
bottom so the image is flipped. To code below includes code showing how to flip it back:"
image(1:28, 1:28, grid)
image(1:28, 1:28, grid[, 28:1])





# Row and column summaries ------------------------------------------------

sums = rowSums(x)
avg = rowMeans(x)

tibble(labels = as.factor(y), row_averages = avg) %>%
   qplot(labels, row_averages, data = ., geom = "boxplot")

"This boxplot shows the average intensity distribution for each number
From this plot we see that, not surprisingly, 1s use less ink than the other digits"

# We can compute the column sums and averages using the function colSums and colMeans,
# respectively.
# The matrixStats package adds functions that performs operations on each row or column
# very efficiently, including the functions rowSds and colSds.





# apply -------------------------------------------------------------------

"The functions just described are performing an operation similar to what sapply and the
purrr function map do: apply the same function to a part of your object. In this case, the
function is applied to either each row or each column. The apply function lets you apply
any function, not just sum or mean, to a matrix. The first argument is the matrix, the second
is the dimension, 1 for rows, 2 for columns, and the third is the function. So, for example,
rowMeans can be written as:"
avgs <- apply(x, 1, mean)

# But notice that just like with sapply and map, we can perform any function. So if we wanted
# the standard deviation for each column, we could write:
sds <- apply(x, 2, sd)
   
"The tradeoff for this flexibility is that these operations are not as fast as dedicated functions
such as rowMeans."





# Filtering columns based on summaries ------------------------------------

"We now turn to task 2: studying the variation of each pixel and removing columns associated
with pixels that don’t change much and thus do not inform the classification. Although a
simplistic approach, we will quantify the variation of each pixel with its standard deviation
across all entries. Since each column represents a pixel, we use the colSds function from
the matrixStats package"
sds = colSds(x)

# A quick look at the distribution of these values shows that some pixels have very low entry
# to entry variability:
qplot(sds, bins = "30", color = I("black"))

"This makes sense since we don’t write in some parts of the box. Here is the variance plotted
by location:"
image(1:28, 1:28, matrix(sds, 28, 28)[, 28:1])

"We could remove features that have no variation since these can’t help us predict"
# Only the columns for which the standard deviation is above 60 are kept, which removes over
# half the predictors.
new_x = x[, colSds(x) > 60]
dim(new_x)






# Indexing with matrices --------------------------------------------------

"We can quickly make a histogram of all the values in our dataset. We saw how we can turn
vectors into matrices. We can also undo this and turn matrices into vectors. The operation
will happen by row:"
mat <- matrix(1:15, 5, 3)
as.vector(mat)

# To see a histogram of all our predictor data, we can use:
qplot(as.vector(new_x), bins = 30, color = I("black"))

"We notice a clear dichotomy which is explained as parts of the image with ink and parts
without. If we think that values below, say, 50 are smudges, we can quickly make them zero
using:"
new_x[new_x < 50] = 0






# Binarizing the data -----------------------------------------------------

"The histogram above seems to suggest that this data is mostly binary. A pixel either has
ink or does not. Using what we have learned, we can binarize the data using just matrix
operations:"
bin_x <- new_x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1
qplot(as.vector(bin_x), bins = 30, color = I("black"))






# Vectorization for matrices ----------------------------------------------

"In R, if we subtract a vector from a matrix, the first element of the vector is subtracted
from the first row, the second element from the second row, and so on."
mat = matrix(1:15, 3, 5)
mat

mat - c(1, 2, 3)
mat * 2
mat * c(1, 2, 3)

"Scaling"
mat = matrix(rnorm(15), 5, 5)
(mat - rowMeans(mat)) / rowSds(mat)

"If you want to scale each column, be careful since this approach does not work for columns."
t(t(mat) - colMeans(mat))

"We can also use a function called sweep that works similarly to apply. It takes each entry
of a vector and subtracts it from the corresponding row or column."
sweep(mat, 2, colMeans(mat)) # 2 is the dimension for columns, colmeans is the vector to substract

"The function sweep actually has another argument that lets you define the arithmetic operation.
So to divide by the standard deviation, we do the following:"
sweep(mat, 2, colMeans(mat)) %>%
   sweep(2, colSds(mat), FUN = "/")

# Check for row operations i.e. dimension 1
identical((mat - rowMeans(mat)) / rowSds(mat),
          sweep(mat, 1, rowMeans(mat)) %>%
             sweep(1, rowSds(mat), FUN = "/"))





# Matrix algebra operations -----------------------------------------------

"Matrix multiplication is done with %*%. For example, the cross product is:"
t(mat) %*% mat

"We can compute the cross product directly with the function:"
crossprod(mat)

"To compute the inverse of a function, we use solve. "
solve(mat)
matlib::Ginv(mat)

"The QR decomposition is readily available by using the qr function"
qr(mat)






# Assessment --------------------------------------------------------------

"Q1 Which line of code correctly creates a 100 by 10 matrix of randomly generated normal 
 numbers and assigns it to x?"
x = matrix(rnorm(1000), 100, 10)

"Q2"
dim(x)
nrow(x)
ncol(x)

"Q3 Which of the following lines of code would add the scalar 1 to row 1, the scalar 2
to row 2, and so on, for the matrix x?"
x + 1:100
x + seq(nrow(x))
sweep(x, 1, 1:nrow(x),"+")

"Q4 Which of the following lines of code would add the scalar 1 to column 1, the scalar 2
to column 2, and so on, for the matrix x?Q"
sweep(x, 2, 1:ncol(x), FUN = "+")


"Q5 Which code correctly computes the average of each row of x?"
rowMeans(x)
"Which code correctly computes the average of each column of x?"
colMeans(x)

"Q6 What proportion of the 60000*784 pixels in the mnist training data are in the grey 
area overall, defined as values between 50 and 205? Report your answer to at least 3 
significant digits."
(data %>% between(51, 204)) %>% mean()





