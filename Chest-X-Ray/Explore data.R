rm(list = ls())
libs = c("MASS", "ggplot2", "imager", "broom", "caret", "dslabs", 
         "matrixStats", "purrr", "rpart", "randomForest", "lubridate",
         "tidyverse")
lapply(libs, library, character.only = TRUE)
options(digits = 3)
im = function(url) plot(imager::load.image(url), axes = F)
seed = function(x) set.seed(x, sample.kind = "Rounding")

# https://www.kaggle.com/nih-chest-xrays/data?select=Data_Entry_2017.csv

data_entry = read.csv("data/Data_Entry_2017.csv")

data_entry$Finding.Labels %>% factor() %>% levels()

data_entry %>%
   mutate(Finding.Labels = factor(Finding.Labels)) %>%
   group_by(Finding.Labels) %>%
   summarise(n = n()) %>%
   arrange(desc(n))

data_entry$Image.Index[which(data_entry$Finding.Labels == "Infiltration")]

# Read images test
xray_path = "C:/Users/Utilisateur/Documents/images/"
xray_test = load.image(paste(xray_path, "00000030_000.png", sep = ""))
plot(xray_test)
as.matrix(xray_test) %>% dim()

# Resize test
xray_test_resize = imresize(xray_test, scale = 0.05)
plot(xray_test_resize)
xray_test_resize %>% as.matrix() %>% dim()

test = resize(xray_test, 64, 64, 1, 1)
plot(test)
test %>% as.matrix() %>% dim()
image(matrix(test, 64, 64))

# Naive Crop
test_crop = as.cimg(matrix(test[5:60,3:30], 56, 28))
plot(test_crop)

# Automated Crop
sums = colSums(test %>% as.matrix())
hist(sums)
mean(sums)

crop_auto = as.matrix(test)[, -which(sums > mean(sums) + 1)] %>%
   as.cimg() %>%
   resize(32, 32, 1, 1)
plot(crop_auto)
























