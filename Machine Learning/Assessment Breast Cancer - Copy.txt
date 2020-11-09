rm(list = ls())
libs = c("MASS", "ggplot2", "imager", "broom", "caret", "dslabs", 
         "matrixStats", "purrr", "rpart", "randomForest", "lubridate",
         "tidyverse")
lapply(libs, library, character.only = TRUE)
options(digits = 3)
im = function(url) plot(imager::load.image(url), axes = F)
seed = function(x) set.seed(x, sample.kind = "Rounding")


"The brca dataset from the dslabs package contains information about breast cancer 
diagnosis biopsy samples for tumors that were determined to be either benign (not 
cancer) and malignant (cancer). The brca object is a list consisting of:

brca$y: a vector of sample classifications ("'B'" = benign or "'M'" = malignant)
brca$x: a matrix of numeric features describing properties of the shape and size
of cell nuclei extracted from biopsy microscope images"

data("brca")



"Q1 Dimensions properties"
dim(brca$x)
mean(brca$y == "M")
col_means = colMeans(brca$x); which.max(col_means)
col_sds = colSds(brca$x); which.min(col_sds)

"Q2 Use sweep() two times to scale each column: subtract the column mean, then
divide by the column standard deviation."
x = brca$x
x = sweep(x, 2, col_means)
x = sweep(x, 2, col_sds, FUN = "/")
sd(x[,1])
median(x[,1])

"Q3 Calculate the distance between all samples using the scaled matrix"
d = dist(x)
d_benign = as.matrix(d)[which(brca$y == "B"), which(brca$y == "B")] 
d_malignant = as.matrix(d)[c(1, which(brca$y == "M")),c(1, which(brca$y == "M")) ] 
mean(d_benign[1, 2:ncol(d_benign)])
mean(d_malignant[1, 2:ncol(d_malignant)])

"Q4 Make a heatmap of the relationship between features using the scaled matrix."
d_features = dist(t(x))
heatmap(as.matrix(d_features))

"Q5 Perform hierarchical clustering on the 30 features. Cut the tree into 5 groups."
h_clust = hclust(d_features)
h_clust %>% plot()

"Q6 Perform a principal component analysis of the scaled matrix."
pca = prcomp(x)
summary(pca)$importance

"Q7 Plot the first two principal components with color representing tumor type (benign/malignant)"
data.frame(pc1 = pca$x[,1], pc2 = pca$x[,2],
           tumor_type = brca$y %>% factor()) %>%
   ggplot(aes(pc1, pc2, color = tumor_type)) +
   geom_point(size = 3)
# Malignant tumors tend to have larger values of PC1 than benign tumors.

"Q8 Make a boxplot of the first 10 PCs grouped by tumor type."
pca$x[, 1:10] %>% 
   as.data.frame() %>%
   gather(PC) %>%
   mutate(tumor_type = rep(brca$y, 10)) %>%
   ggplot(aes(y = value, fill = tumor_type)) +
   geom_boxplot() +
   facet_wrap(PC ~ ., ncol = 5)



seed(1)
test_index = createDataPartition(brca$y, p = 0.2, list = F)
test_x = x[test_index, ]; test_y = brca$y[test_index]
train_x = x[-test_index, ]; train_y = brca$y[-test_index]

"Q9 Check that the training and test sets have similar proportions of benign and malignant tumors"
mean(train_y == "B")
mean(test_y == "B")

# The predict_kmeans() function defined here takes two arguments - a matrix of observations 
# x and a k-means object k - and assigns each row of x to a cluster from k.
predict_kmeans = function(x, k){
   centers = k$centers
   distances = sapply(1:nrow(x), function(i){
      apply(centers, 1, function(y) dist(rbind(x[i,], y)))
   })
   max.col(-t(distances))
}

"Q10a Set the seed to 3. Perform k-means clustering on the training set with 2 centers 
and assign the output to k. Then use the predict_kmeans() function to make predictions 
on the test set."
k = kmeans(train_x, centers = 2)
y_hat = ifelse(predict_kmeans(test_x, k) == 1, "B", "M")
confusionMatrix(factor(y_hat, levels = levels(test_y)), factor(test_y)) %>%
   .$overall

"Q10b What proportion of benign/malignant tumors are correctly identified?"
sensitivity(factor(y_hat, levels = levels(test_y)), factor(test_y))
specificity(factor(y_hat, levels = levels(test_y)), factor(test_y))

"Q11 Fit a logistic regression model on the training set with caret::train() using all 
predictors. Ignore warnings about the algorithm not converging. Make predictions on the 
test set."
fit_logit = train(train_x,train_y,  method = "glm")
confusionMatrix(predict(fit_logit, test_x) %>% factor(levels = levels(test_y)),
                factor(test_y))$overall

"Q12 Train an LDA model and a QDA model on the training set."
fit_lda = train(train_x,train_y,  method = "lda")
confusionMatrix(predict(fit_lda, test_x) %>% factor(levels = levels(test_y)),
                factor(test_y))$overall
fit_qda = train(train_x,train_y,  method = "qda")
confusionMatrix(predict(fit_qda, test_x) %>% factor(levels = levels(test_y)),
                factor(test_y))$overall

"Q13 Set the seed to 5, then fit a loess model on the training set"
seed(5)
fit_loess = train(train_x, train_y, method = "gamLoess")
confusionMatrix(predict(fit_loess, test_x) %>% factor(levels = levels(test_y)),
                factor(test_y))$overall

"Q14 Set the seed to 7, then train a k-nearest neighbors model on the training set"
seed(7)
fit_knn = train(train_x, train_y, method = "knn", 
                tuneGrid = data.frame(k = seq(3, 21, 2)))
confusionMatrix(predict(fit_knn, test_x) %>% factor(levels = levels(test_y)),
                factor(test_y))$overall
fit_knn$bestTune

"Q15a Set the seed to 9, then train a random forest model on the training set. Use the argument
importance = TRUE so that feature importance can be extracted"
seed(9)
fit_rf = train(train_x, train_y, method = "rf",
               tuneGrid = data.frame(mtry = c(3, 5, 7, 9)),
               importance = T)
confusionMatrix(predict(fit_rf, test_x) %>% factor(levels = levels(test_y)),
                factor(test_y))$overall
fit_rf$bestTune
varImp(fit_rf)

"Q15b Consider the top 10 most important variables in the random forest model.
Which set of features is most important for determining tumor type?"
# worst values

"Q16a Create an ensemble using the predictions from the 7 models created in the previous 
exercises: k-means, logistic regression, LDA, QDA, loess, k-nearest neighbors, and random
forest. Use the ensemble to generate a majority prediction of the tumor type (if most 
models suggest the tumor is malignant, predict malignant)."
ensemble = data.frame(k_means = y_hat, 
                      logit = predict(fit_logit, test_x) %>% factor(levels = levels(test_y)),
                      lda = predict(fit_lda, test_x) %>% factor(levels = levels(test_y)),
                      qda = predict(fit_qda, test_x) %>% factor(levels = levels(test_y)),
                      loess = predict(fit_loess, test_x) %>% factor(levels = levels(test_y)),
                      knn = predict(fit_knn, test_x) %>% factor(levels = levels(test_y)),
                      rf = predict(fit_rf, test_x) %>% factor(levels = levels(test_y)))
ensemble = data.matrix(ensemble)-1
y_hat_ensemble = ifelse(rowSums(ensemble) > 3, "M", "B")
confusionMatrix(y_hat_ensemble %>% factor(levels = levels(test_y)),
                factor(test_y))$overall

"Q16b Make a table of the accuracies of the 7 models and the accuracy of the ensemble model"
y_hat_all = ifelse(ensemble == 1, "M", "B") %>% cbind(y_hat_ensemble)
apply(y_hat_all, 2, function(i) {
   confusionMatrix(i %>% factor(levels = levels(test_y)),
                   factor(test_y))$overall["Accuracy"]
   })











