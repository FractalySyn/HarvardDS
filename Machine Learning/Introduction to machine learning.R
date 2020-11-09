rm(list = ls())
x = c("tidyverse", "ggplot2", "imager", "broom", "caret", "dslabs")
lapply(x, library, character.only = TRUE)
options(digits = 3)


"Perhaps the most popular data science methodologies come from the field of machine learning.
Machine learning success stories include the handwritten zip code readers implemented
by the postal service, speech recognition technology such as Apple’s Siri, movie recommendation
systems, spam and malware detectors, housing price predictors, and driverless cars.
Although today Artificial Intelligence and machine learning are often used interchangeably,
we make the following distinction: while the first artificial intelligence algorithms, such as
those used by chess playing machines, implemented decision making based on programmable
rules derived from theory or first principles, in machine learning decisions are based on algorithms
built with data."





# Notation ----------------------------------------------------------------

"In machine learning, data comes in the form of:
1. the outcome we want to predict and
2. the features that we will use to predict the outcome

We want to build an algorithm that takes feature values as input and returns a prediction
for the outcome when we don’t know the outcome. The machine learning approach is to
train an algorithm using a dataset for which we do know the outcome, and then apply this
algorithm in the future to make a prediction when we don’t know the outcome"

# Here we will use Y to denote the outcome and X1, . . . ,Xp to denote features. Note that
# features are sometimes referred to as predictors or covariates. We consider all these to be
# synonyms.

# Prediction problems can be divided into categorical and continuous outcomes. For categorical
# outcomes, Y can be any one of K classes. The number of classes can vary greatly across
# applications. For example, in the digit reader data, K = 10 with the classes being the digits
# 0, 1, 2, 3, 4, 5, 6, 7, 8, and 9. In speech recognition, the outcomes are all possible words or
# phrases we are trying to detect. Spam detection has two outcomes: spam or not spam. In
# this book, we denote the K categories with indexes k = 1, . . . ,K. However, for binary data
# we will use k = 0, 1 for mathematical conveniences that we demonstrate later.

"To build a model that provides a prediction for any set of observed values X1 = x1,X2 =
x2, . . .X5 = x5, we collect data for which we know the outcome:"

# y1 x1,1 x1,2 x1,3 x1,4 x1,5
# y2 x2,1 x2,2 x2,3 x2,4 x2,5
# ...
# ...
# ...
# ...
# ...
# ...
# yn xn,1 xn,2 xn,3 xn,4 xn,5

"When the output is continuous we refer to the machine learning task as PREDICTION, and the
main output of the model is a function f that automatically produces a prediction, denoted
with ˆy, for any set of predictors: ˆy = f(x1, x2, . . . , xp). We use the term actual outcome
to denote what we ended up observing. So we want the prediction ˆy to match the actual
outcome y as well as possible. Because our outcome is continuous, our predictions ˆy will
not be either exactly right or wrong, but instead we will determine an error defined as the
difference between the prediction and the actual outcome y − ˆy."

"When the outcome is categorical, we refer to the machine learning task as CLASSIFICATION, and
the main output of the model will be a decision rule which prescribes which of the K classes
we should predict. In this scenario, most models provide functions of the predictors for each
class k, fk(x1, x2, . . . , xp), that are used to make this decision. When the data is binary a
typical decision rules looks like this: if f1(x1, x2, . . . , xp) > C, predict category 1, if not the
other category, with C a predetermined cutoff. Because the outcomes are categorical, our
predictions will be either right or wrong"

# Notice that these terms vary among courses, text books, and other publications. Often
# prediction is used for both categorical and continuous outcomes, and the term regression
# can be used for the continuous case. Here we avoid using regression to avoid confusion with
# our previous use of the term linear regression. In most cases it will be clear if our outcomes
# are categorical or continuous, so we will avoid using these terms when possible.





# Example -----------------------------------------------------------------

"Let’s consider the zip code reader example. The first step in handling mail received in the
post office is sorting letters by zip code:"

# Originally, humans had to sort these by hand. To do this, they had to read the zip codes on
# each letter. Today, thanks to machine learning algorithms, a computer can read zip codes
# and then a robot sorts the letters. In this part of the book, we will learn how to build
# algorithms that can read a digit.

"The first step in building an algorithm is to understand what are the outcomes and features.
Below are three images of written digits. These have already been read by a human and
assigned an outcome Y . These are considered known and serve as the training set."

"https://rafalab.github.io/dsbook/book_files/figure-html/digit-images-example-1.png" %>%
   load.image() %>% plot(axes = F)

# The images are converted into 28 × 28 = 784 pixels and, for each pixel, we obtain a grey
# scale intensity between 0 (white) and 255 (black), which we consider continuous for now.
# The following plot shows the individual features for each image:
"https://rafalab.github.io/dsbook/book_files/figure-html/example-images-1.png" %>%
   load.image() %>% plot(axes = F)

"For each digitized image i, we have a categorical outcome Yi which can be one of
10 values (0, 1, 2, 3, 4, 5, 6, 7, 8, 9), and features Xi,1, . . . ,Xi,784. We use bold face Xi =
(Xi,1, . . . ,Xi,784) to distinguish the vector of predictors from the individual predictors"




# Exercises ---------------------------------------------------------------

"1. For each of the following, determine if the outcome is continuous or categorical:
a. Digit reader
b. Movie recommendations
c. Spam filter
d. Hospitalizations
e. Siri (speech recognition)"
# a. categorical
# b. categorical
# c. categorical binary
# d. continuous (discrete actually)
# e. categorical

"2. How many features are available to us for prediction in the digits dataset?"
# 28 x 28 pixels = 784

"3. In the digit reader example, the outcomes are stored here:"
library(dslabs)
data("mnist_27")
y <- mnist_27$train$y
y
"Do the following operations have a practical meaning?"
y[5] + y[6]
y[5] > y[6]
# No, because these are labels representing a category not a number. A 9 represents
# a class not the number 9. It's a factors vector






# Evaluation metrics ------------------------------------------------------

"Before we start describing approaches to optimize the way we build algorithms, we first need
to define what we mean when we say one approach is better than another. In this section, we
focus on describing ways in which machine learning algorithms are evaluated. Specifically,
we need to quantify what we mean by “'better'”"

library(caret); library(dslabs)
data("heights")

# We start by defining the outcome and predictors.
y = heights$sex
x = heights$height

"In this case, we have only one predictor, height, and y is clearly a categorical outcome since
observed values are either Male or Female. We know that we will not be able to predict Y
very accurately based on X because male and female average heights are not that different
relative to within group variability. But can we do better than guessing? To answer this
question, we need a quantitative definition of better."

# A standard way of generating the training and test sets is by randomly splitting the data.
# The caret package includes the function createDataPartition that helps us generates
# indexes for randomly splitting the data into training and test sets:

set.seed(2007)
test_index = createDataPartition(y, times = 1, p = 0.5, list = F)
# one sample (times), uniform probability, returned as a vector

test_set = heights[test_index, ]
train_set = heights[-test_index, ]

"We will now develop an algorithm using only the training set. Once we are done developing
the algorithm, we will freeze it and evaluate it using the test set. The simplest way to evaluate
the algorithm when the outcomes are categorical is by simply reporting the proportion of
cases that were correctly predicted in the test set. This metric is usually referred to as
OVERALL ACCURACY."

# To demonstrate the use of overall accuracy, we will build two competing algorithms and
# compare them.
# Let’s start by developing the simplest possible machine algorithm: guessing the outcome.

y_hat = sample(c("Male", "Female"), length(test_index), replace = TRUE) %>%
   factor(levels = levels(test_set$sex))
# In machine learning applications, it is useful to use factors to represent the categorical
# outcomes because R functions developed for machine learning, such as those in the caret
# package, require or recommend that categorical outcomes be coded as factors

"The overall accuracy is simply defined as the overall proportion that is predicted correctly:"
mean(y_hat == test_set$sex)
# Not surprisingly, our accuracy is about 50%. We are guessing!"

"Can we do better? Exploratory data analysis suggests we can because, on average, males
are slightly taller than females:"
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))

# But how do we make use of this insight? Let’s try another simple approach: predict Male if
# height is within two standard deviations from the average male:
y_hat <- ifelse(x > 62, "Male", "Female") %>%
   factor(levels = levels(test_set$sex))
mean(y == y_hat)


"But can we do even better? In the example above, we used a cutoff of 62, but we can
examine the accuracy obtained for other cutoffs and then pick the value that provides the
best results. But remember, it is important that we optimize the cutoff using only
the training set: the test set is only for evaluation. Although for this simplistic example
it is not much of a problem, later we will learn that evaluating an algorithm on the training
set can lead to overfitting, which often results in dangerously over-optimistic assessments"

cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
   y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
      factor(levels = levels(test_set$sex))
   mean(y_hat == train_set$sex)
})
plot(accuracy)

# The maximum accuracy is obtained for a threshold of 65 
max(accuracy)
cutoff[which.max(accuracy)]

"We can now test this cutoff on our test set to make sure our accuracy is not overly optimistic:"
y_hat <- ifelse(test_set$height > 65, "Male", "Female") %>%
   factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

# We see that it is a bit lower than the accuracy observed for the training set, but it is still
# better than guessing. And by testing on a dataset that we did not train on, we know our
# result is not due to cherry-picking a good result





# The confusion matrix ----------------------------------------------------
"
The prediction rule we developed in the previous section predicts Male if the student is
taller than 64 inches. Given that the average female is about 64 inches, this prediction rule
seems wrong. What happened? If a student is the height of the average female, shouldn’t
we predict Female?
Generally speaking, overall accuracy can be a deceptive measure. To see this, we will start
by constructing what is referred to as the confusion matrix, which basically tabulates each
combination of prediction and actual value. We can do this in R using the function table:"
table(predicted = y_hat, actual = test_set$sex)

# If we study this table closely, it reveals a problem. If we compute the accuracy separately
# for each sex, we get:
test_set %>%
   mutate(y_hat = y_hat) %>%
   group_by(sex) %>%
   summarize(accuracy = mean(y_hat == sex))
"There is an imbalance in the accuracy for males and females: too many females are predicted
to be male. We are calling almost half of the females male! How can our overall accuracy be
so high then? This is because the prevalence of males in this dataset is high. These heights
were collected from three data sciences courses, two of which had more males enrolled:"
prev <- mean(y == "Male")
prev


"!!!!!!!!!!!!!!!!!!
So when computing overall accuracy, the high percentage of mistakes made for females is
outweighed by the gains in correct calls for men. This can actually be a big problem in
machine learning. If your training data is biased in some way, you are likely to develop
algorithms that are biased as well. The fact that we used a test set does not matter because
it is also derived from the original biased dataset. This is one of the reasons we look at
metrics other than overall accuracy when evaluating a machine learning algorithm.
!!!!!!!!!!!!!!!!!!!"

# There are several metrics that we can use to evaluate an algorithm in a way that prevalence
# does not cloud our assessment, and these can all be derived from the confusion matrix. A
# general improvement to using overall accuracy is to study sensitivity and specificity separately.






# Sensitivity and specificity ---------------------------------------------

"categorical, we can define these terms for a specific category. In the digits example, we can
ask for the specificity in the case of correctly predicting 2 as opposed to some other digit.
Once we specify a category of interest, then we can talk about positive outcomes, Y = 1,
and negative outcomes, Y = 0.

In general, sensitivity is defined as the ability of an algorithm to predict a positive outcome
when the actual outcome is positive: ˆ Y = 1 when Y = 1. Because an algorithm that calls
everything positive ( ˆ Y = 1 no matter what) has perfect sensitivity, this metric on its own
is not enough to judge an algorithm. For this reason, we also examine specificity, which is
generally defined as the ability of an algorithm to not predict a positive ˆ Y = 0 when the
actual outcome is not a positive Y = 0. We can summarize in the following way:

• High sensitivity: Y = 1 =⇒ ˆ Y = 1
• High specificity: Y = 0 =⇒ ˆ Y = 0

Although the above is often considered the definition of specificity, another way to think of
specificity is by the proportion of positive calls that are actually positive:

• High specificity: ˆ Y = 1 =⇒ Y = 1."


# To provide precise definitions, we name the four entries of the confusion matrix:

#                       Actually Positive       Actually Negative
# Predicted positive    True positives (TP)     False positives (FP)
# Predicted negative    False negatives (FN)    True negatives (TN)

"Sensitivity is typically quantified by TP/(TP +FN), the proportion of actual positives (the
first column = TP +FN) that are called positives (TP). This quantity is referred to as the
true positive rate (TPR) or recall.

Specificity is defined as TN/(TN +FP) or the proportion of negatives (the second column
= FP + TN) that are called negatives (TN). This quantity is also called the true negative
rate (TNR).

There is another way of quantifying specificity which is TP/(TP + FP) or
the proportion of outcomes called positives (the first row or TP + FP) that are actually
positives (TP). This quantity is referred to as positive predictive value (PPV) and also as
precision. Note that, unlike TPR and TNR, precision depends on prevalence since higher
prevalence implies you can get higher precision even when guessing."
# 
# Sensitivity   TPR   Recall     TP/(TP+FN)   P(^Y=1 | Y=1)
# Specificity   TNR   1-FPR      TN/(TN+FP)   P(^Y=0 | Y=0)
# Specificity   PPV   Precision  TP/(TP+FP)   P(Y=1 | ^Y=1)

"Here TPR is True Positive Rate, FPR is False Positive Rate, and PPV is Positive Predictive
Value. The caret function confusionMatrix computes all these metrics for us once we
define what category “positive” is. The function expects factors as input, and the first level
is considered the positive outcome or Y = 1. In our example, Female is the first level
because it comes before Male alphabetically. If you type this into R you will see several
metrics including accuracy, sensitivity, specificity, and PPV."
cm = confusionMatrix(data = y_hat, reference = test_set$sex)
cm$overall
cm$byClass[c("Sensitivity","Specificity", "Prevalence")]

# We can see that the high overall accuracy is possible despite relatively low sensitivity. As
# we hinted at above, the reason this happens is because of the low prevalence (0.23): the
# proportion of females is low. Because prevalence is low, failing to predict actual females as
# females (low sensitivity) does not lower the accuracy as much as failing to predict actual
# males as males (low specificity). This is an example of why it is important to examine
# sensitivity and specificity and not just accuracy. Before applying this algorithm to general
# datasets, we need to ask ourselves if prevalence will be the same.




# Balanced accuracy and F1 score ------------------------------------------

"Although we usually recommend studying both specificity and sensitivity, very often it is
useful to have a one-number summary, for example for optimization purposes. One metric
that is preferred over overall accuracy is the average of specificity and sensitivity, referred to
as BALANCED ACCURACY. Because specificity and sensitivity are rates, it is more appropriate to
compute the harmonic average. In fact, the F1-score, a widely used one-number summary,
is the HARMONIC AVERAGE of precision and recall"

"F1-score = 2 x (precision x recall) / (precision + recall)"

# Remember that, depending on the context, some types of errors are more costly than others.
# For example, in the case of plane safety, it is much more important to maximize sensitivity
# over specificity: failing to predict a plane will malfunction before it crashes is a much more
# costly error than grounding a plane when, in fact, the plane is in perfect condition. In a
# capital murder criminal case, the opposite is true since a false positive can lead to executing
# an innocent person. The F1-score can be adapted to weigh specificity and sensitivity differently.
# To do this, we define β to represent how much more important sensitivity is compared
# to specificity and consider a WEIGHTED HARMONIC AVERAGE:

"weighted F1-score = 1 / ((β^2/(1+β^2)) x (1/recall) + (1/(1+β^2)) x (1/precision))"
"The F_meas function in the caret package computes this summary with beta defaulting to 1"

# Let’s rebuild our prediction algorithm, but this time maximizing the F-score instead of
# overall accuracy:
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
   y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
      factor(levels = levels(test_set$sex))
   F_meas(data = y_hat, reference = factor(train_set$sex))
})
# We see that it is maximized at F1 value of:
max(F_1)
best = cutoff[which.max(F_1)]; best
y_hat <- ifelse(test_set$height > best, "Male", "Female") %>%
   factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)

"We now see that we do much better than guessing, that both sensitivity and specificity are
relatively high, and that we have built our first machine learning algorithm. It takes height
as a predictor and predicts female if you are 65 inches or shorter."






# Prevalence matters in practice ------------------------------------------

"A machine learning algorithm with very high sensitivity and specificity may not be useful
in practice when prevalence is close to either 0 or 1. To see this, consider the case of a
doctor that specializes in a rare disease and is interested in developing an algorithm for
predicting who has the disease. The doctor shares data with you and you then develop an
algorithm with very high sensitivity. You explain that this means that if a patient has the
disease, the algorithm is very likely to predict correctly. You also tell the doctor that you
are also concerned because, based on the dataset you analyzed, 1/2 the patients have the
disease: Pr( ˆ Y = 1). The doctor is neither concerned nor impressed and explains that what
is important is the precision of the test: Pr(Y = 1| ˆ Y = 1). Using Bayes theorem, we can
connect the two measures:
            Pr(Y = 1 | ˆ Y = 1) = Pr(ˆY = 1 | Y = 1)*Pr(Y = 1)/Pr( ˆ Y = 1)"

# The doctor knows that the prevalence of the disease is 5 in 1,000, which implies that 
# Pr(Y = 1) / Pr( ˆ Y = 1) = 1/100 and therefore the precision of your algorithm is less 
# than 0.01. The doctor does not have much use for your algorithm.





# ROC and precision-recall curves -----------------------------------------

"When comparing the two methods (guessing versus using a height cutoff), we looked at
accuracy and F1. The second method clearly outperformed the first. However, while we considered
several cutoffs for the second method, for the first we only considered one approach:
guessing with equal probability. Note that guessing Male with higher probability would give
us higher accuracy due to the bias in the sample:"
p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>%
   factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

# But, as described above, this would come at the cost of lower sensitivity. The curves we
# describe in this section will help us see this

# Remember that for each of these parameters, we can get a different sensitivity and specificity.
# For this reason, a very common approach to evaluating methods is to compare them
# graphically by plotting both
"A widely used plot that does this is the receiver operating characteristic (ROC) curve"


"The ROC curve plots sensitivity (TPR) versus 1 - specificity or the false positive rate (FPR).
Here we compute the TPR and FPR needed for different probabilities of guessing male:"
probs <- seq(0.1, 1, length.out = 10)
guessing <- map_df(probs, function(p){
   y_hat <-
      sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>%
      factor(levels = c("Female", "Male"))
   list(method = "Guessing",
        FPR = 1 - specificity(y_hat, test_set$sex),
        TPR = sensitivity(y_hat, test_set$sex),
        value = p)
})

"We can use similar code to compute these values for our our second approach. By plotting
both curves together, we are able to compare sensitivity for different values of specificity:"
cutoff <- seq(61, 70)
cutoff_method <- map_df(cutoff, function(x){
   y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
      factor(levels = levels(test_set$sex))
   list(method = "Cutoff",
        FPR = 1 - specificity(y_hat, test_set$sex),
        TPR = sensitivity(y_hat, test_set$sex),
        value = x)
})

# My code to plot the comparison of these two methods
comparison = rbind(guessing, cutoff_method)
comparison %>%
   ggplot(aes(FPR, TPR, color = method, label = value)) +
   geom_point() + geom_line() +
   ggrepel::geom_label_repel()
"We can see that we obtain higher sensitivity with this approach for all values of specificity,
which implies it is in fact a better method. Note that ROC curves for guessing always fall
on the identiy line. Also note that when making ROC curves, it is often nice to add the
cutoff associated with each point"

# The packages pROC and plotROC are useful for generating these plots

"ROC curves have one weakness and it is that neither of the measures plotted depends on
prevalence. In cases in which prevalence matters, we may instead make a precision-recall
plot. The idea is similar, but we instead plot precision against recall:"

"https://rafalab.github.io/dsbook/book_files/figure-html/precision-recall-1-1.png" %>%
   load.image() %>% plot(axes = F)
# From this plot we immediately see that the precision of guessing is not high. This is because
# the prevalence is low. We also see that if we change positives to mean Male instead of Female,
# the ROC curve remains the same, but the precision recall plot changes.






# Assessment Practice with Machine Learning ---------------------------------------------------------------

"The reported_heights and heights datasets were collected from three classes taught in the 
Departments of Computer Science and Biostatistics, as well as remotely through the Extension 
School. The Biostatistics class was taught in 2016 along with an online version offered by 
the Extension School. On 2016-01-25 at 8:15 AM, during one of the lectures, the instructors 
asked student to fill in the sex and height questionnaire that populated the reported_heights
dataset. The online students filled out the survey during the next few days, after the lecture
was posted online. We can use this insight to define a variable which we will call type, to 
denote the type of student, inclass or online."

library(dslabs); library(dplyr); library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
   filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
   mutate(type = ifelse(day(date_time) == 25 & 
                           hour(date_time) == 8 & between(minute(date_time), 15, 30),
                        "inclass","online")) %>%
   select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

"Q1 What proportion of the inclass group is female? What proportion of the online group is female?"
data = data.frame(sex = y, type = x)
data %>% filter(type == "inclass") %>% summarise(mean(sex == "Female"))
data %>% filter(type == "online") %>% summarise(mean(sex == "Female"))

"Q2 In the course videos, height cutoffs were used to predict sex. Instead of height, use the 
type variable to predict sex. Assume that for each class type the students are either all male
or all female, based on the most prevalent sex in each class type you calculated in Q1. Report
the accuracy of your prediction of sex based on type. You do not need to split the data into 
training and test sets."
data = data %>%
   mutate(y_hat = ifelse(type == "inclass", "Female", "Male")) %>%
   mutate(y_hat = factor(y_hat, c("Female", "Male")))
confusionMatrix(data$y_hat, data$sex)


"Q3 Write a line of code using the table() function to show the confusion matrix between y_hat
and y. Use the exact format function(a, b) for your answer and do not name the columns and rows
Your answer should have exactly one space."
table(y, y_hat)

"Q4 What is the sensitivity of this prediction?
 Q5 specificity"
data %>% summarise(sensitivity = sensitivity(y_hat, sex),
                   specificity = specificity(y_hat, sex))


"Q6 What is the prevalence (% of females) in the dat dataset defined above? "
mean(data$sex == "Female")


# We will practice building a machine learning algorithm using a new dataset, iris, that 
# provides multiple predictors for us to use to train. To start, we will remove the setosa
# species and we will focus on the versicolor and virginica iris species 

data("iris")
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2, sample.kind="Rounding") 
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

"Q8 Using only the train iris dataset, for each feature, perform a simple search to find
the cutoff that produces the highest accuracy, predicting virginica if greater than the
cutoff and versicolor otherwise. Use the seq function over the range of each feature by 
intervals of 0.1 for this search."
summary(train)
cutoff_sl = seq(4.9, 7.9, 0.1); cutoff_sw = seq(2, 3.8, 0.1)
cutoff_pl = seq(3, 6.9, 0.1); cutoff_pw = seq(1, 2.5, 0.1)

map_df(cutoff_sl, function(x){
   forecast = ifelse(train$Sepal.Length < x, "versicolor", "virginica") %>%
      factor()
   acc = confusionMatrix(forecast, train$Species)$overall[["Accuracy"]]
   data.frame(cutoff = x,
              accuracy = acc)
}) %>% summarise(max(accuracy), cutoff[which.max(accuracy)])
map_df(cutoff_sw, function(x){
   forecast = ifelse(train$Sepal.Width < x, "versicolor", "virginica") %>%
      factor()
   acc = confusionMatrix(forecast, train$Species)$overall[["Accuracy"]]
   data.frame(cutoff = x,
              accuracy = acc)
}) %>% summarise(max(accuracy), cutoff[which.max(accuracy)])
map_df(cutoff_pl, function(x){
   forecast = ifelse(train$Petal.Length < x, "versicolor", "virginica") %>%
      factor()
   acc = confusionMatrix(forecast, train$Species)$overall[["Accuracy"]]
   data.frame(cutoff = x,
              accuracy = acc)
}) %>% summarise(max(accuracy), cutoff[which.max(accuracy)])
map_df(cutoff_pw, function(x){
   forecast = ifelse(train$Petal.Width < x, "versicolor", "virginica") %>%
      factor()
   acc = confusionMatrix(forecast, train$Species)$overall[["Accuracy"]]
   data.frame(cutoff = x,
              accuracy = acc)
}) %>% summarise(max(accuracy), cutoff[which.max(accuracy)])


"Q9 For the feature selected in Q8, use the smart cutoff value from the training data to
calculate overall accuracy in the test data. What is the overall accuracy?"
test %>% 
   mutate(forecast = ifelse(Petal.Length < 4.8, "versicolor", "virginica") %>%
             factor()) %>%
   summarise(acc = confusionMatrix(forecast, Species%>%factor())$overall[["Accuracy"]])

"Q10 Notice that we had an overall accuracy greater than 96% in the training data, but 
the overall accuracy was lower in the test data. This can happen often if we overtrain.
In fact, it could be the case that a single feature is not the best choice. For example,
a combination of features might be optimal. Using a single feature and optimizing the 
cutoff as we did on our training data can lead to overfitting.

Given that we know the test data, we can treat it like we did our training data to see
if the same feature with a different cutoff will optimize our predictions.
"
test %>% 
   mutate(forecast = ifelse(Petal.Width < 1.6, "versicolor", "virginica") %>%
             factor()) %>%
   summarise(acc = confusionMatrix(forecast, Species%>%factor())$overall[["Accuracy"]])


"Q11 Now we will perform some exploratory data analysis on the data."
plot(iris,pch=21,bg=iris$Species)
# Notice that Petal.Length and Petal.Width in combination could potentially be more information 
# than either feature alone.
"report the overall accuracy when applied to the test dataset by creating a rule that predicts
virginica if Petal.Length is greater than the length cutoff OR Petal.Width is greater than the
width cutoff, and versicolor otherwise."
# 4.8, 1.7
test %>% 
   mutate(forecast = ifelse(Petal.Width < 1.6 | Petal.Length < 4.8,
                            "versicolor", "virginica") %>% factor()) %>%
   summarise(acc = confusionMatrix(forecast, Species%>%factor())$overall[["Accuracy"]])
# 0.88??







# The loss function -------------------------------------------------------

"Up to now we have described evaluation metrics that apply exclusively to categorical data.
Specifically, for binary outcomes, we have described how sensitivity, specificity, accuracy
and F1 can be used as quantification. However, these metrics are not useful for continuous
outcomes. In this section, we describe how the general approach to defining “best” in machine
learning is to define a loss function, which can be applied to both categorical and continuous
data."


# The most commonly used loss function is the squared loss function. If ˆy is our predictor and
# y is the observed outcome, the squared loss function is simply:
"   (^y - y)^2  "

"Because we often have a test set with many observations, say N, we use the mean squared
error (MSE):
               MSE = (1/N)*RSS "

# In practice, we often report the root mean squared error (RMSE), which is √MSE, because
# it is in the same units as the outcomes. But doing the math is often easier with the MSE and
# it is therefore more commonly used in textbooks, since these usually describe theoretical
# properties of algorithms.

"If the outcomes are binary, both RMSE and MSE are equivalent to accuracy, since (ˆy−y)2 is
0 if the prediction was correct and 1 otherwise. In general, our goal is to build an algorithm
that minimizes the loss so it is as close to 0 as possible."

# Because our data is usually a random sample, we can think of the MSE as a random variable
# and the observed MSE can be thought of as an estimate of the expected MSE E[MSE] which 
# would be the mean of several calculated MSE on different datasets

"Note that there are loss functions other than the squared loss. For example, the Mean
Absolute Error uses absolute values, |ˆYi − Yi| instead of squaring the errors (ˆYi − Yi)^2.
However, in this book we focus on minimizing square loss since it is the most widely used."





# Conditional probabilities and expectations ------------------------------

"In machine learning applications, we rarely can predict outcomes perfectly. For example,
spam detectors often miss emails that are clearly spam, Siri often misunderstands the words
we are saying, and your bank at times thinks your card was stolen when it was not. The
most common reason for not being able to build perfect algorithms is that it is impossible."

"conditional probabilities for each class k:
Pr(Y = k | X1 = x1, . . . ,Xp = xp), for k = 1, . . . ,K"

"These probabilities guide the construction of an algorithm that makes the best prediction:
for any given x, we will predict the class k with the largest probability among
p1(x), p2(x), . . . pK(x). In mathematical notation, we write it like this: ˆ Y = maxk pk(x)."

# So what we will predict depends on two things: 1) how close are the maxk pk(x) to 1 or 0
# (perfect certainty) and 2) how close our estimates ˆpk(x) are to pk(x). We can’t do anything
# about the first restriction as it is determined by the nature of the problem, so our energy
# goes into finding ways to best estimate conditional probabilities. The first restriction does
# imply that we have limits as to how well even the best possible algorithm can perform. You
# should get used to the idea that while in some challenges we will be able to achieve almost
# perfect accuracy, with digit readers for example, in others our success is restricted by the
# randomness of the process, with movie recommendations for example.

"Before we continue, it is important to remember that defining our prediction by maximizing
the probability is not always optimal in practice and depends on the context. As discussed
above, sensitivity and specificity may differ in importance. But even in these cases, having
a good estimate of the pk(x), k = 1, . . . ,K will suffice for us to build optimal prediction
models, since we can control the balance between specificity and sensitivity"


"For binary data, you can think of the probability Pr(Y = 1 | X = x) as the proportion
of 1s in the stratum of the population for which X = x. Many of the algorithms we will
learn can be applied to both categorical and continuous data due to the connection between
conditional probabilities and conditional expectations.

Because the expectation is the average of values y1, . . . , yn in the population, in the case in
which the ys are 0 or 1, the expectation is equivalent to the probability of randomly picking
a one since the average is simply the proportion of ones:

E(Y | X = x) = Pr(Y = 1 | X = x)."

# As a result, we often only use the expectation to denote both the conditional probability
# and conditional expectation.



"Why do we care about the conditional expectation in machine learning? This is because the
expected value has an attractive mathematical property: it minimizes the MSE. Specifically,
of all possible predictions ˆ Y ,
ˆY = E(Y | X = x) minimizes E{(ˆY − Y )2 | X = x}"

# Due to this property, a succinct description of the main task of machine learning is that we
# use data to estimate:
#    f(x) ≡ E(Y | X = x)















# Assessment Conditional Probabilities ------------------------------------

"In a previous module, we covered Bayes' theorem and the Bayesian paradigm. Conditional 
probabilities are a fundamental part of this previous covered rule.
               P(A|B) = P(B|A) * P(A) / P(B)"

"Q1 Assume a patient comes into the doctor’s office to test whether they have a 
particular disease."
# The test is positive 85% of the time when tested on a patient with the disease 
# (high sensitivity - true positives):  P(test+|disease)=0.85 
"TPR = P(+|ill) = sensitivity"
positive_when_disease = 0.85
# The test is negative 90% of the time when tested on a healthy patient 
# (high specificity - true negatives):  P(test−|heathy)=0.90
"TNR = P(-|healthy = specificity)"
negative_when_healthy = 0.9
# The disease is prevalent in about 2% of the community:  P(disease)=0.02
"P(ill) = prevalence"
prevalence = 0.02
"Using Bayes' theorem, calculate the probability that you have the disease if the
test is positive."
# P(disease | +) = P(+ | disease) * P(disease) / P(+)
# P(+) = P(+ | disease) * P(disease) + P(+ | healthy) * P(healthy)
positive = positive_when_disease * prevalence + (1 - negative_when_healthy) * (1 - prevalence)
disease_when_positive = positive_when_disease * prevalence / positive
disease_when_positive


"We have a hypothetical population of 1 million individuals with the same 
conditional probabilities as described in question 1:"
set.seed(1, sample.kind = "Rounding")
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

"Q2 What is the probability that a test is positive?"
mean(test == 1)

"Q3 What is the probability that an individual has the disease if the test is negative?"
mean(disease[test==0])

"Q4 What is the probability that you have the disease if the test is positive?"
mean(disease[test==1])

"Q5 Compare the prevalence of disease in people who test positive to the overall 
prevalence of disease.
If a patient's test is positive, how much does that increase their risk of having the disease?"
0.147/0.02


"Q6 We are now going to write code to compute conditional probabilities for being male 
in the heights dataset. Round the heights to the closest inch. Plot the estimated conditional
probability  P(x)=Pr(Male|height=x)  for each  x ."
data("heights")
heights %>%
   mutate(height = round(height)) %>%
   group_by(height) %>%
   summarise(p = mean(sex == "Male")) %>%
   qplot(height, p, data = .)

"Q7 In the plot we just made in Q6 we see high variability for low values of height. This
is because we have few data points. This time use the quantile  0.1,0.2,…,0.9  and the cut()
function to assure each group has the same number of points. Note that for any numeric vector
x, you can create groups based on quantiles like this:"
# cut(x, quantile(x, seq(0, 1, 0.1)), include.lowest = TRUE)
ps <- seq(0, 1, 0.1)
heights %>% 
   mutate(g = cut(height, quantile(height, ps), include.lowest = T)) %>%
   group_by(g) %>%
   summarize(p = mean(sex == "Male"), height = mean(height)) %>%
   qplot(height, p, data =.)

"Q8 You can generate data from a bivariate normal distrubution using the MASS package 
using the following code:"
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
   data.frame() %>% setNames(c("x", "y"))
plot(dat)
"Using an approach similar to that used in the previous exercise, let's estimate the 
conditional expectations and make a plot. Part of the code has again been provided for you:"
ps <- seq(0, 1, 0.1)
dat %>% 
   mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
   group_by(g) %>%
   summarize(y = mean(y), x = mean(x)) %>%
   qplot(x, y, data =.)



