rm(list = ls())
libs = c("tidyverse", "ggplot2", "imager", "broom", "caret", "dslabs", 
         "matrixStats", "purrr", "rpart", "randomForest", "lubridate",
         "MASS")
lapply(libs, library, character.only = TRUE)
options(digits = 3)
im = function(url) plot(imager::load.image(url), axes = F)
seed = function(x) set.seed(x, sample.kind = "Rounding")


# A typical machine learning challenge will include a large number of predictors, which makes
# visualization somewhat challenging. We have shown methods for visualizing univariate and
# paired data, but plots that reveal relationships between many variables are more complicated
# in higher dimensions. For example, to compare each of the 784 features in our predicting
# digits example, we would have to create, for example, 306,936 scatterplots. Creating one
# single scatter-plot of the data is impossible due to the high dimensionality.

"Here we describe powerful techniques useful for exploratory data analysis, among other
things, generally referred to as dimension reduction. The general idea is to reduce the dimension
of the dataset while preserving important characteristics, such as the distance
between features or observations. With fewer dimensions, visualization then becomes more
feasible. The technique behind it all, the singular value decomposition, is also useful in other
contexts. Principal component analysis (PCA) is the approach we will be showing. Before
applying PCA to high-dimensional datasets, we will motivate the ideas behind with a simple
example."





# Preserving distance -----------------------------------------------------

"We consider an example with twin heights. Some pairs are adults, the others are children.
Here we simulate 100 two-dimensional points that represent the number of standard deviations
each individual is from the mean height. Each point is a pair of twins. We use the
mvrnorm function from the MASS package to simulate bivariate normal data."

seed(1988)
n = 100
sigma = matrix(c(9, 9*0.9, 9*0.92, 9), 2, 2)
x = rbind(mvrnorm(n/2, c(69, 69), sigma),
          mvrnorm(n/2, c(55, 55), sigma)) #A children

# A scatterplot quickly reveals that the correlation is high and that there are two groups of
# twins, the adults (upper right points) and the children (lower left points):
x %>% as.data.frame() %>%
   ggplot(aes(.[,1], .[,2])) + 
   geom_point() + 
   geom_abline(color = "red")

"Our features are N two-dimensional points, the two heights, and, for illustrative purposes,
we will act as if visualizing two dimensions is too challenging. We therefore want to reduce
the dimensions from two to one, but still be able to understand important characteristics of
the data, for example that the observations cluster into two groups: adults and children."

# Let’s consider a specific challenge: we want a one-dimensional summary of our predictors
# from which we can approximate the distance between any two observations.
d = dist(x)
as.matrix(d)[1:5, 1:5]

"This distance is based on two dimensions and we need a distance approximation based on
just one."

# Let’s start with the naive approach of simply removing one of the two dimensions. Let’s
# compare the actual distances to the distance computed with just the first dimension:
z = x[,1]
"Here are the approximate distances versus the original distances:"
data.frame(d_x = as.vector(d),
           d_z = as.vector(dist(z))) %>%
   ggplot(aes(d_x, d_z)) +
   geom_abline(color = "red") +
   geom_point(alpha = 0.2)

# "The plot looks about the same if we use the second dimension. We obtain a general underestimation.
# This is to be expected because we are adding more positive quantities in the
# distance calculation as we increase the number of dimensions. If instead we use an average,
# like this
#     j = 2;   √(1/2 ∑(Xi,j - Xi,j)^2) =  distance / √2 then the underestimation goes away.
"
So we approximate the 2-dimensional distance dx by the 1-dimensional distance dz / √2
"
data.frame(d_x = as.vector(d),
           d_z = as.vector(dist(z) * sqrt(2))) %>%
   ggplot(aes(d_x, d_z)) +
   geom_point() +
   geom_abline(color = "red", lwd = 1.2) 


# This actually works pretty well and we get a typical difference of:
sd(dist(x) - dist(z)*sqrt(2))

"If we look back at the previous scatterplot and visualize a line between any pair of points,
the length of this line is the distance between the two points. These lines tend to go along
the direction of the diagonal. Notice that if we instead plot the difference versus the average
we can see how the distance between points is mostly explained by the first dimension: the
average."
data.frame(average = (x[,1] + x[,2])/2,
           difference = x[,2] - x[,1]) %>%
   ggplot(aes(average, difference)) +
   geom_point() +
   ylim(c(-15, 15))
# Indeed the distance between clusters is explained by the average dimension
# Our twins heights dataset contains two different height means and the difference
# between columns is small and explained by the natural variability

"This means that we can ignore the second dimension and not lose too much information.
We can then use a one dimensional distance to approximate the 2-dim distance and to 
improve our precision we will use the average instead of one column"
data.frame(dist_avg = as.vector(dist((x[,1] + x[,2])/2) * sqrt(2)),
           dist_x = as.vector(d)) %>%
              ggplot(aes(dist_x, dist_avg)) +
              geom_point() +
              geom_abline(color = "red")

# with the typical difference improved by about 35%:
sd(dist(x) - dist((x[,1] + x[,2])/2) * sqrt(2))

"Later we learn that dist_avg is the first principal component of the matrix x."






# Linear transformations (advanced) ---------------------------------------

im("https://github.com/FractalySyn/harvardXdatascience/raw/master/2020-08-23_11h27_52.png")


# Orthogonal transformations (advanced) ------------------------------------

im("https://github.com/FractalySyn/harvardXdatascience/raw/master/2020-08-23_11h50_07.png")
im("https://github.com/FractalySyn/harvardXdatascience/raw/master/2020-08-23_11h50_29.png")
im("https://github.com/FractalySyn/harvardXdatascience/raw/master/2020-08-23_11h50_40.png")






# Principal Component Analysis --------------------------------------------

im("https://github.com/FractalySyn/harvardXdatascience/raw/master/2020-08-23_12h04_27.png")

# We can compute v1 and v2 using:
colMeans(x^2)
# and we can show mathematically that if we apply an orthogonal transformation as above,
# then the total variation remains the same:
sum(colMeans(x^2))
z = cbind((x[,2] + x[,1]) / sqrt(2), (x[,2] - x[,1]) / sqrt(2))
sum(colMeans(z^2))

"However, while the variability in the two columns of X is about the same, in the transformed
version Z 99% of the variability is included in just the first dimension:"
v = colMeans(z^2)
v/sum(v) 
# The first principal component (PC) of a matrix X is the linear orthogonal transformation
# of X that maximizes this variability. The function prcomp provides this info
pca = prcomp(x)
pca$rotation

"Note that the first PC is almost the same as that provided by the (X1 + X2)/√2 we used
earlier (except perhaps for a sign change that is arbitrary)."

# The function PCA returns both the rotation needed to transform X so that the variability of
# the columns is decreasing from most variable to least (accessed with $rotation) as well as
# the resulting new matrix (accessed with $x). By default the columns of X are first centered.

"So, using the matrix multiplication shown above, we have that the following are the same
(demonstrated by a difference between elements of essentially zero):"
a = sweep(x, 2, colMeans(x))
b = pca$x %*% t(pca$rotation)
max(abs(a-b))

# The rotation is orthogonal which means that the inverse is its transpose. So we also have
# that these two are identical:
a = sweep(x, 2, colMeans(x)) %*% pca$rotation
b = pca$x
max(abs(a-b))

"We can visualize these to see how the first component summarizes the data. In the plot
below red represents high values and blue negative values (later we learn why we call these
weights and patterns):"
im("https://rafalab.github.io/dsbook/book_files/figure-html/illustrate-pca-twin-heights-1.png")

# For a multidimensional matrix with X with p columns, we can find a transformation that
# creates Z that preserves distance between rows, but with the variance of the columns in
# decreasing order. The second column is the second principal component, the third column
# is the third principal component, and so on. As in our example, if after a certain number
# of columns, say k, the variances of the columns of Zj , j > k are very small, it means
# these dimensions have little to contribute to the distance and we can approximate distance
# between any two points with just k dimensions. If k is much smaller than p, then we can
# achieve a very efficient summary of our data.

"https://www.youtube.com/watch?v=FgakZw6K1QQ"





# Iris example ------------------------------------------------------------

data("iris")
# The iris data is a widely used example in data analysis courses. It includes four botanical
# measurements related to three flower species
names(iris)
head(iris) # the data is ordered by the species

"Let’s compute the distance between each observation. You can clearly see the three species
with one species very different from the other two:"
x = iris[, 1:4]
d = dist(x)
image(as.matrix(d), col = rev(RColorBrewer::brewer.pal(9, "RdBu")))

# Our predictors here have four dimensions, but three are very correlated:
cor(x) %>% knitr::kable(align = "c")
"If we apply PCA, we should be able to approximate this distance with just two dimensions,
compressing the highly correlated dimensions. Using the summary function we can see the
variability explained by each PC:"
pca = prcomp(x)
summary(pca)

# The first two dimensions account for 97% of the variability. Thus we should be able to
# approximate the distance very well with two dimensions. We can visualize the results of
# PCA:
im("https://rafalab.github.io/dsbook/book_files/figure-html/illustrate-pca-twin-heights-iris-1.png")

"And see that the first pattern is sepal length, petal length, and petal width (red) in one
direction and sepal width (blue) in the other. The second pattern is the sepal length and
petal width in one direction (blue) and petal length and petal width in the other (red).
You can see from the weights that the first PC1 drives most of the variability and it clearly
separates the first third of samples (setosa) from the second two thirds (versicolor and
virginica). If you look at the second column of the weights, you notice that it somewhat
separates versicolor (red) from virginica (blue)."

# We can see this better by plotting the first two PCs with color representing the species
data.frame(pca$x[, 1:2], Species = iris$Species) %>%
   ggplot(aes(PC1, PC2, fill = Species)) +
   geom_point(cex = 3, pch = 21) +
   coord_fixed(ratio = 1)

"We see that the first two dimensions preserve the distance:"
d_hat = dist(pca$x[, 1:2])
qplot(d, d_hat) + geom_abline(color = "red", lwd = 1.3)

# This example is more realistic than the first artificial example we used, since we showed how
# we can visualize the data using two dimensions when the data was four-dimensional





# MNIST example -----------------------------------------------------------

mnist = if(!exists("mnist")) mnist = read_mnist()
"The written digits example has 784 features. Is there any room for data reduction? Can we
create simple machine learning algorithms using fewer features?"

# Because the pixels are so small, we expect pixels close to each other on the grid to be
# correlated, meaning that dimension reduction should be possible.
# Let’s try PCA and explore the variance of the PCs. This will take a few seconds as it is a
# rather large matrix.

col_means = colMeans(mnist$test$images[1:2000, ])
pca = prcomp(mnist$train$images[1:10000, ])

pc = 1:ncol(mnist$test$images)
qplot(pc, pca$sdev)

"We can see that the first few PCs already explain a large percent of the variability:"
summary(pca)$importance[, 1:8]

# And just by looking at the first two PCs we see information about the class. Here is a
# random sample of 2,000 digits:
data.frame(PC1 = pca$x[,1], PC2 = pca$x[,2],
           label=factor(mnist$train$label)) %>%
   sample_n(2000) %>%
   ggplot(aes(PC1, PC2, fill=label)) +
   geom_point(cex=3, pch=21)

"We can also see the linear combinations on the grid to get an idea of what is getting weighted:"
im("https://rafalab.github.io/dsbook/book_files/figure-html/mnist-pca-1-4-1.png")
# The lower variance PCs appear related to unimportant variability in the corners
im("https://rafalab.github.io/dsbook/book_files/figure-html/mnist-pca-last,-1.png")


"Now let’s apply the transformation we learned with the training data to the test data, reduce
the dimension and run knn on just a small number of dimensions."
# We try 36 dimensions since this explains about 80% of the data. First fit the model:
k = 36
x_train = pca$x[, 1:k]
y = factor(mnist$train$labels[1:10000])
fit = knn3(x_train, y, k = k)

"Now transform the test set:"
x_test = sweep(mnist$test$images[1:2000,], 2, col_means) %*% pca$rotation
x_test = x_test[, 1:k]

# And we are ready to predict and see how we do:
y_hat = predict(fit, x_test, type = "class")
confusionMatrix(y_hat, factor(mnist$test$labels[1:2000]))$overall

"After PCA dimension reduction resulting in only 36 features (PCs) describing
80% of the variability, we get 90% accuracy with K-nn training"







# Assessment --------------------------------------------------------------

"We want to explore the tissue_gene_expression predictors by plotting them."
data("tissue_gene_expression")
dim(tissue_gene_expression$x)

"Q1 We want to get an idea of which observations are close to each other, but, 
as you can see from the dimensions, the predictors are 500-dimensional, making
plotting difficult. Plot the first two principal components with color representing 
tissue type."
pca_gene = prcomp(tissue_gene_expression$x)
data.frame(PC1 = pca_gene$x[, 1], PC2 = pca_gene$x[, 2],
           tissue = factor(tissue_gene_expression$y)) %>%
   ggplot(aes(PC1, PC2, fill = tissue)) +
   geom_point(cex = 4, pch = 21)

"Q2 The predictors for each observation are measured using the same device and 
experimental procedure. This introduces biases that can affect all the predictors 
from one observation. For each observation, compute the average across all predictors,
and then plot this against the first PC with color representing tissue. Report the 
correlation."
data.frame(row_means = rowMeans(tissue_gene_expression$x),
           PC1 = pca_gene$x[, 1],
           tissue = factor(tissue_gene_expression$y)) %>%
   ggplot(aes(PC1, row_means, color = tissue)) +
   geom_point(size = 3)
cor(rowMeans(tissue_gene_expression$x), pca_gene$x[, 1])

"Q3 We see an association with the first PC and the observation averages. Redo the PCA
but only after removing the center. Part of the code is provided for you."
x = with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc = prcomp(x)
data.frame(pc1 = pc$x[,1], pc2 = pc$x[,2],
           tissue = tissue_gene_expression$y) %>%
   ggplot(aes(pc1, pc2, color = tissue)) +
   geom_point()

"Q4 For the first 10 PCs, make a boxplot showing the values for each tissue."
data.frame(pc7 = pc$x[,7],
           tissue = tissue_gene_expression$y) %>%
   ggplot(aes(y = pc7, color = tissue)) +
   geom_boxplot()

"Q5 Plot the percent variance explained by PC number"
explained_cum = summary(pc)$importance[3,]
pcs = 1:189
plot(pcs, explained_cum)
explained_cum[1:5]