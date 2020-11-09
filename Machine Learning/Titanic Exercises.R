rm(list = ls())
libs = c("tidyverse", "ggplot2", "imager", "broom", "caret", "dslabs", 
         "matrixStats", "purrr", "rpart", "randomForest")
lapply(libs, library, character.only = TRUE)
options(digits = 3)
im = function(url) plot(imager::load.image(url), axes = F)
seed = function(x) set.seed(x, sample.kind = "Rounding")

# The Titanic was a British ocean liner that struck an iceberg and sunk on its maiden voyage in 
# 1912 from the United Kingdom to New York. More than 1,500 of the estimated 2,224 passengers and
# crew died in the accident, making this one of the largest maritime disasters ever outside of war.
# The ship carried a wide range of passengers of all ages and both genders, from luxury travelers
# in first-class to immigrants in the lower classes. However, not all passengers were equally 
# likely to survive the accident. You will use real data about a selection of 891 passengers to
# predict which passengers survived.

library(titanic)
titanic_clean = titanic_train %>%
   mutate(Survived = factor(Survived),
          Embarked = factor(Embarked),
          Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
          FamilySize = SibSp + Parch + 1) %>%    # count family members
   select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)
head(titanic_clean)

"Q1 Set the seed to 42, then use the caret package to create a 20% data partition based on the 
Survived column. Assign the 20% partition to test_set and the remaining 80% partition to train_set."
set.seed(42, sample.kind = "Rounding")
ind = createDataPartition(titanic_clean$Survived, p = 0.2, list = F)
train_set = titanic_clean[-ind, ]; test_set = titanic_clean[ind, ]
mean(train_set$Survived == 1)

# The simplest prediction method is randomly guessing the outcome without using additional 
# predictors. These methods will help us determine whether our machine learning algorithm 
# performs better than chance. How accurate are two methods of guessing Titanic passenger
# survival?

"Q2 Set the seed to 3. For each individual in the test set, randomly guess whether that 
person survived or not by sampling from the vector c(0,1)"
seed(3)
y_hat_guess = sample(c(0, 1), nrow(test_set), replace = T)
confusionMatrix(y_hat_guess %>% factor(levels = levels(test_set$Survived)),
                test_set$Survived)$overall["Accuracy"]

"Q3a Use the training set to determine whether members of a given sex were more likely to 
survive or die. Apply this insight to generate survival predictions on the test set"
mean(train_set$Survived[train_set$Sex == "female"] == 1, na.rm = T)
mean(train_set$Survived[train_set$Sex == "male"] == 1, na.rm = T)

"Q3b Predict survival using sex on the test set: if the survival rate for a sex is over 
0.5, predict survival for all individuals of that sex, and predict death if the survival 
rate for a sex is under 0.5."
y_hat_by_sex = ifelse(test_set$Sex == "female", 1, 0)
confusionMatrix(y_hat_by_sex %>% factor(levels = levels(test_set$Survived)),
                test_set$Survived)$overall["Accuracy"]


"Q4a In the training set, which class(es) (Pclass) were passengers more likely to 
survive than die?"
train_set %>%
   ggplot(aes(Pclass, fill = Survived)) +
   geom_bar(position = position_fill())

"Q4b Predict survival using passenger class on the test set: predict survival if the 
survival rate for a class is over 0.5, otherwise predict death."
y_hat_by_class = ifelse(test_set$Pclass == 1, 1, 0)
confusionMatrix(y_hat_by_class %>% factor(levels = levels(test_set$Survived)),
                test_set$Survived)$overall["Accuracy"]

ggplot(titanic, aes(Age, ..count.., fill = Survived)) +
   geom_density() +
   facet_grid(Sex ~ Pclass)

"Q4c Use the training set to group passengers by both sex and passenger class."
ggplot(train_set, aes(Age, ..count.., fill = Survived)) +
   geom_density() +
   facet_grid(Sex ~ Pclass)
train_set %>% select(Sex, Pclass, Survived) %>%
   group_by(Sex, Pclass) %>%
   table()

"Q4d Predict survival using both sex and passenger class on the test set. Predict survival
if the survival rate for a sex/class combination is over 0.5, otherwise predict death"
y_hat_by_sex_class = ifelse(test_set$Sex == "female" & test_set$Pclass %in% 1:2, 1, 0)
confusionMatrix(y_hat_by_sex_class %>% factor(levels = levels(test_set$Survived)),
                test_set$Survived)$overall["Accuracy"]

"Q5a Use the confusionMatrix() function to create confusion matrices for the sex model, 
class model, and combined sex and class model. You will need to convert predictions 
and survival status to factors to use this function."
confusionMatrix(y_hat_by_sex %>% factor(levels = levels(test_set$Survived)),
                test_set$Survived)$positive
# Sensitivity
sensitivity(y_hat_by_sex %>% factor(levels = levels(test_set$Survived)), test_set$Survived)
sensitivity(y_hat_by_class %>% factor(levels = levels(test_set$Survived)), test_set$Survived)
sensitivity(y_hat_by_sex_class %>% factor(levels = levels(test_set$Survived)), test_set$Survived)
# specificity
specificity(y_hat_by_sex %>% factor(levels = levels(test_set$Survived)), test_set$Survived)
specificity(y_hat_by_class %>% factor(levels = levels(test_set$Survived)), test_set$Survived)
specificity(y_hat_by_sex_class %>% factor(levels = levels(test_set$Survived)), test_set$Survived)
# balanced acciracy
confusionMatrix(y_hat_by_sex %>% factor(levels = levels(test_set$Survived)),
                test_set$Survived)$byClass["Balanced Accuracy"]
confusionMatrix(y_hat_by_class %>% factor(levels = levels(test_set$Survived)),
                test_set$Survived)$byClass["Balanced Accuracy"]
confusionMatrix(y_hat_by_sex_class %>% factor(levels = levels(test_set$Survived)),
                test_set$Survived)$byClass["Balanced Accuracy"]

"Q5b What is the maximum value of balanced accuracy?"
0.806

"Q6 Use the F_meas() function to calculate  F1  scores for the sex model, class model, 
and combined sex and class model."
F_meas(y_hat_by_sex %>% factor(levels = levels(test_set$Survived)),
                test_set$Survived)
F_meas(y_hat_by_class %>% factor(levels = levels(test_set$Survived)),
       test_set$Survived)
F_meas(y_hat_by_sex_class %>% factor(levels = levels(test_set$Survived)),
       test_set$Survived)



"Q7 Set the seed to 1. Train a model using LDA and QDA with fare as the only predictor"
seed(1)
fit_lda = train(Survived ~ Fare, data = train_set, method = "lda")
y_hat_lda = predict(fit_lda, test_set)
confusionMatrix(y_hat_lda, test_set$Survived)$overall["Accuracy"]
seed(1)
fit_qda = train(Survived ~ Fare, data = train_set, method = "qda")
y_hat_qda = predict(fit_qda, test_set)
confusionMatrix(y_hat_qda, test_set$Survived)$overall["Accuracy"]

"Q8 Set the seed to 1. Train a logistic regression with age as the only predictor"
seed(1)
fit_logit = train(Survived ~ Age, data = train_set, method = "glm")
y_hat_logit = predict(fit_logit, test_set)
confusionMatrix(y_hat_logit, test_set$Survived)$overall["Accuracy"]
"With four predictors: sex, class, fare, and age"
seed(1)
fit_logit2 = train(Survived ~ Age + Pclass + Fare + Sex, data = train_set, method = "glm")
y_hat_logit2 = predict(fit_logit2, test_set)
confusionMatrix(y_hat_logit2, test_set$Survived)$overall["Accuracy"]
"Using all predictors"
seed(1)
fit_logit3 = train(Survived ~ ., data = train_set, method = "glm")
y_hat_logit3 = predict(fit_logit3, test_set)
confusionMatrix(y_hat_logit3, test_set$Survived)$overall["Accuracy"]


"Q9a Set the seed to 6. Train a kNN model on the training set using the caret train function. 
Try tuning with k = seq(3, 51, 2)"
seed(6)
fit_knn = train(Survived ~ ., data = train_set, method = "knn",
                tuneGrid = data.frame(k = seq(3, 51, 2)))
attach(fit_knn$results)
k[which.max(Accuracy)]

"Q9b Plot the kNN model to investigate the relationship between the number of neighbors 
and accuracy on the training set. Of these values of  k , which yields the highest accuracy?"
plot(fit_knn)
11

"Q9c What is the accuracy of the kNN model on the test set?"
confusionMatrix(predict(fit_knn, test_set), test_set$Survived)$overall["Accuracy"]

"Q10 Set the seed to 8 and train a new kNN model. Instead of the default training control, 
use 10-fold cross-validation where each partition consists of 10% of the total. Try tuning 
with k = seq(3, 51, 2)"
seed(8)
fit_knn_cv = train(Survived ~ ., data = train_set, method = "knn",
                tuneGrid = data.frame(k = seq(3, 51, 2)),
                trControl = trainControl(method = "cv", number = 10, p = 0.9))
attach(fit_knn_cv$results)
k[which.max(Accuracy)]
confusionMatrix(predict(fit_knn_cv, test_set), test_set$Survived)$overall["Accuracy"]

"Q11a Set the seed to 10. Use caret to train a decision tree with the rpart method. 
Tune the complexity parameter with cp = seq(0, 0.05, 0.002)"
seed(10)
fit_tree = train(Survived ~ ., data = train_set, method = "rpart",
                 tuneGrid = data.frame(cp = seq(0, 0.05, 0.02)))
attach(fit_tree$results)
cp[which.max(Accuracy)]
confusionMatrix(predict(fit_tree, test_set), test_set$Survived)$overall["Accuracy"]

"Q11b Inspect the final model and plot the decision tree
Which variables are used in the decision tree?"
fit_tree$finalModel
fit_tree_best = rpart(Survived ~ ., data = train_set, cp = 0.02)
plot(fit_tree_best); text(fit_tree_best)

"Q11c Using the decision rules generated by the final model, predict whether the following 
individuals would survive"
fit_tree$finalModel
# 28 yo male
no
# 2nd class female
yes
# etc.

"Q12 Set the seed to 14. Use the caret train() function with the rf method to train a 
random forest. Test values of mtry ranging from 1 to 7. Set ntree to 100."
seed(14)
fit_rf = train(Survived ~ ., data = train_set, method = "rf",
               tuneGrid = data.frame(mtry = seq(1, 7)),
               ntree = 100)
attach(fit_rf$results)
mtry[which.max(Accuracy)]
confusionMatrix(predict(fit_rf, test_set), test_set$Survived)$overall["Accuracy"]
"Use varImp() on the random forest model object to determine the importance of various 
predictors to the random forest model."
varImp(fit_rf)




