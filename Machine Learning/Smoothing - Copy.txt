rm(list = ls())
x = c("tidyverse", "ggplot2", "imager", "broom", "caret", "dslabs")
lapply(x, library, character.only = TRUE)
options(digits = 3)





"We are going to learn a few new algorithms based on different ideas and concepts. But
what they all have in common is that they permit more flexible approaches. We will start
by describing nearest neighbor and kernel approaches. To introduce the concepts behinds
these approaches, we will again start with a simple one-dimensional example and describe
the concept of smoothing."

# Other names given to this technique are curve fitting and low pass filtering. It is designed
# to detect trends in the presence of noisy data in cases in which the shape of the trend is
# unknown. The smoothing name comes from the fact that to accomplish this feat, we assume
# that the trend is smooth, as in a smooth surface. In contrast, the noise, or deviation from
# the trend, is unpredictably wobbly:

"To understand why we cover this topic, remember that the concepts behind
smoothing techniques are extremely useful in machine learning because conditional
expectations/probabilities can be thought of as trends of unknown shapes
that we need to estimate in the presence of uncertainty."

# we try to estimate the time trend in the 2008 US popular vote poll margin (difference
# between Obama and McCain).
data("polls_2008")
qplot(day, margin, data = polls_2008)

# To think of this as a machine learning problem, consider that we want to predict Y given
# a day x. If we knew the conditional expectation f(x) = E(Y | X = x), we would use it.
# But since we don’t know this conditional expectation, we have to estimate it. Let’s use
# regression, since it is the only method we have learned up to now.
"https://rafalab.github.io/dsbook/book_files/figure-html/linear-regression-not-flexible-1.png" %>%
   load.image() %>% plot(axes = F)

"The line we see does not appear to describe the trend very well. For example, on September
4 (day -62), the Republican Convention was held and the data suggest that it gave John
McCain a boost in the polls. However, the regression line does not capture this potential
trend. To see the lack of fit more clearly, we note that points above the fitted line (blue) and
those below (red) are not evenly distributed across days. We therefore need an alternative,
more flexible approach."






# Bin Smoothing -----------------------------------------------------------

"The general idea of smoothing is to group data points into strata in which the value of f(x)
can be assumed to be constant. We can make this assumption because we think f(x) changes
slowly and, as a result, f(x) is almost constant in small windows of time. An example of
this idea for the poll_2008 data is to assume that public opinion remained approximately
the same within a week’s time. With this assumption in place, we have several data points
with the same expected value."

# If we fix a day to be in the center of our week, call it x0, then for any other day x such that
# |x − x0| ≤ 3.5, we assume f(x) is a constant f(x) = μ. This assumption implies that:
#    E[Yi|Xi = xi] ≈ μ if |xi − x0| ≤ 3.5
"In smoothing, we call the size of the interval satisfying |xi − x0| ≤ 3.5 the window size,
bandwidth or span. Later we will see that we try to optimize this parameter.
The idea behind bin smoothing is to make this calculation with each value of x as the center."

# In the poll example, for each day, we would compute the average of the values within a week
# with that day in the center. Here are two examples: x0 = −125 and x0 = −55. The blue
# segment represents the resulting average.

"https://rafalab.github.io/dsbook/book_files/figure-html/binsmoother-expained-1.png" %>%
   load.image() %>% plot(axes = F)

# By computing this mean for every point, we form an estimate of the underlying curve f(x).
span = 7 # a week
fit = with(polls_2008, 
           ksmooth(day, margin, kernel = "box", bandwidth = span)) #♣ generate f(x)

polls_2008 %>%
   mutate(smooth = fit$y) %>%
   ggplot(aes(day, margin)) +
   geom_point(size = 3, alpha = 0.5, color = "grey") +
   geom_line(aes(day, smooth), color = "red", size = 1)








# Kernels -----------------------------------------------------------------

"The final result from the bin smoother is quite wiggly. One reason for this is that each time
the window moves, two points change. We can attenuate this somewhat by taking weighted
averages that give the center point more weight than far away points, with the two points
at the edges receiving very little weight."

# You can think of the bin smoother approach as a weighted average
# in which each point receives a weight of either 0 or 1/N0, with N0 the number of points in
# the week. In the code above, we used the argument kernel="box" in our call to the function
# ksmooth. This is because the weight function looks like a box. The ksmooth function provides
# a “smoother” option which uses the normal density to assign weights.

"https://rafalab.github.io/dsbook/book_files/figure-html/gaussian-kernel-1.png" %>%
   load.image() %>% plot(axes = F)

span = 7 
fit = with(polls_2008, 
           ksmooth(day, margin, kernel = "normal", bandwidth = span))
polls_2008 %>%
   mutate(smooth = fit$y) %>%
   ggplot(aes(day, margin)) +
   geom_point(size = 3, alpha = 0.5, color = "grey") +
   geom_line(aes(day, smooth), color = "red", size = 1)

"There are several functions in R that implement bin smoothers. One example is ksmooth,
shown above. In practice, however, we typically prefer methods that use slightly more complex
models than fitting a constant. The final result above, for example, is still somewhat
wiggly in parts we don’t expect it to be (between -125 and -75, for example). Methods such
as loess, which we explain next, improve on this"







# Local weighted regression (loess) ---------------------------------------

"A limitation of the bin smoother approach just described is that we need small windows
for the approximately constant assumptions to hold. As a result, we end up with a small
number of data points to average and obtain imprecise estimates ˆ f(x). Here we describe
how local weighted regression (loess) permits us to consider larger window sizes. To do this,
we will use a mathematical result, referred to as Taylor’s theorem, which tells us that if you
look closely enough at any smooth function f(x), it will look like a line."

# Instead of assuming the function is approximately constant in a window, we assume the
# function is locally linear. We can consider larger window sizes with the linear assumption
# than with a constant. Instead of the one-week window, we consider a larger one in which
# the trend is approximately linear. We start with a three-week window and later consider
# and evaluate other options:
  
" E[Yi|Xi = xi] = β0 + β1(xi − x0) if |xi − x0| ≤ 21
For every point x0, loess defines a window and fits a line within that window. Here is an
example showing the fits for x0 = −125 and x0 = −55:"

"https://rafalab.github.io/dsbook/book_files/figure-html/loess-1.png" %>%
   load.image() %>% plot(axes = F)

total_days = diff(range(polls_2008$day))
span = 21/total_days # 3 weeks

fit = loess(margin ~ day, degree = 1, span = span, data = polls_2008)

polls_2008 %>%
   mutate(smooth = fit$fitted) %>%
   ggplot(aes(day, margin)) +
   geom_point(size = 3, alpha = 0.5, color = "grey") +
   geom_line(aes(day, smooth), color = "red", size = 1)

"!!!!!!!!!!!!
loess fits a linear regression on a time span (3 weeks here) arround each data point (each day here)
!!!!!!!!!!!!"

# Different spans give us different estimates. We can see how different window sizes lead to
# different estimates:
"https://rafalab.github.io/dsbook/book_files/figure-html/loess-final-1.png" %>%
   load.image() %>% plot(axes = F)



"There are three other differences between loess and the typical bin smoother."

# 1. Rather than keeping the bin size the same, loess keeps the number of points used in
# the local fit the same. This number is controlled via the span argument, which expects a
# proportion. For example, if N is the number of data points and span=0.5, then for a given
# x, loess will use the 0.5 * N closest points to x for the fit.

# 2. When fitting a line locally, loess uses a weighted approach. Basically, instead of using
# least squares, we minimize a weighted version:
# sum(w0(xi)*[Yi − {β0 + β1(xi − x0)}]^2)
# However, instead of the Gaussian kernel, loess uses a function called the Tukey tri-weight:
#    W(u) = 1 − |u|^3^3 if |u| ≤ 1 and W(u) = 0 if |u| > 1
# This kernel differs from the Gaussian kernel in that more points get values closer to the
# max:
"https://rafalab.github.io/dsbook/book_files/figure-html/triweight-kernel-1.png" %>%
   load.image() %>% plot(axes = F)

# 3. loess has the option of fitting the local model robustly. An iterative algorithm
# is implemented in which, after fitting a model in one iteration, outliers are detected
# and down-weighted for the next iteration. To use this option, we use the argument
# family="symmetric".



# Fitting parabolas -------------------------------------------------------

"Taylor’s theorem also tells us that if you look at any mathematical function closely enough,
it looks like a parabola. The theorem also states that you don’t have to look as closely when
approximating with parabolas as you do when approximating with lines. This means we can
make our windows even larger and fit parabolas instead of lines."

# This is actually the default procedure of the function loess. You may have noticed that when
# we showed the code for using loess, we set degree = 1. This tells loess to fit polynomials
# of degree 1, a fancy name for lines. If you read the help page for loess, you will see that
# the argument degree defaults to 2.

span = 28/total_days
fit1 = loess(margin ~ day, degree = 1, span = span, data = polls_2008)
fit2 = loess(margin ~ day, degree = 2, span = span, data = polls_2008)
polls_2008 %>%
   mutate(smooth1 = fit1$fitted, smooth2 = fit2$fitted) %>%
   ggplot(aes(day, margin)) +
   geom_point(size = 3, alpha = .5, color = "grey") +
   geom_line(aes(day, smooth1), color="red", lty = 2, size = 1) +
   geom_line(aes(day, smooth2), color="orange", lty = 1, size = 1)
"The degree = 2 gives us more wiggly results. We actually prefer degree = 1 as it is less
prone to this kind of noise."





# geom_smooth uses loess --------------------------------------------------

"ggplot uses loess in its geom_smooth function:"
polls_2008 %>% ggplot(aes(day, margin)) +
   geom_point() +
   geom_smooth()

"But be careful with default parameters as they are rarely optimal. However, you can conveniently
change them:"
polls_2008 %>% ggplot(aes(day, margin)) +
   geom_point() +
   geom_smooth(span = 0.15, method = "loess", method.args = list(degree=1))






# Assessment --------------------------------------------------------------

"In the Wrangling course of this series, PH125.6x, we used the following code to obtain mortality counts for Puerto Rico for 2015-2018"
library(tidyverse); library(lubridate); library(purrr); library(pdftools)
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
   s <- str_trim(s)
   header_index <- str_which(s, "2015")[1]
   tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
   month <- tmp[1]
   header <- tmp[-1]
   tail_index  <- str_which(s, "Total")
   n <- str_count(s, "\\d+")
   out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
   s[-out] %>%
      str_remove_all("[^\\d\\s]") %>%
      str_trim() %>%
      str_split_fixed("\\s+", n = 6) %>%
      .[,1:5] %>%
      as_data_frame() %>% 
      setNames(c("day", header)) %>%
      mutate(month = month,
             day = as.numeric(day)) %>%
      gather(year, deaths, -c(day, month)) %>%
      mutate(deaths = as.numeric(deaths))
}) %>%
   mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                         "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
   mutate(date = make_date(year, month, day)) %>%
   dplyr::filter(date <= "2018-05-01")


"Q1 Use the loess() function to obtain a smooth estimate of the expected number of deaths as 
a function of date. Plot this resulting smooth function. Make the span about two months long."
with(dat, max(date) - min(date))
dat %>% ggplot(aes(date, deaths)) +
   geom_point(alpha = 0.5) +
   geom_smooth(span = 60/1216, method = "loess", method.args = list(degree=1), size = 2)


"Q2 Work with the same data as in Q1 to plot smooth estimates against day of the year, all on
the same plot, but with different colors for each year."
dat %>% 
   mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
   ggplot(aes(day, smooth, col = year)) +
   geom_line(lwd = 2)


# Suppose we want to predict 2s and 7s in the mnist_27 dataset with just the second covariate. 
# Can we do this? On first inspection it appears the data does not have much predictive power.
# 
# In fact, if we fit a regular logistic regression the coefficient for x_2 is not significant!
#    This can be seen using this code:
library(broom)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()

# Plotting a scatterplot here is not useful since y is binary:
qplot(x_2, y, data = mnist_27$train)

"Q3 Fit a loess line to the data above and plot the results. What do you observe?"
mnist_27$train %>% as_tibble() %>% head()
fit = mnist_27$train %>%
   mutate(y = as.numeric(y)) %>%
   loess(y~x_2, degree = 1, span = 0.1, data = .)
mnist_27$train %>% 
   mutate(smooth = fit$fitted) %>%
   ggplot(aes(x_2, y)) +
   geom_point() + 
   geom_line(aes(x_2, smooth))

# There is predictive power and the conditional probability is non-linear.
mnist_27$train %>% 
   mutate(y = ifelse(y=="7", 1, 0)) %>%
   ggplot(aes(x_2, y)) + 
   geom_point() +
   geom_smooth(method = "loess")




















