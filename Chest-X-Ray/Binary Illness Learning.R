rm(list = ls())
libs = c("MASS", "ggplot2", "imager", "broom", "caret", "dslabs", 
         "matrixStats", "purrr", "rpart", "randomForest", "lubridate",
         "tidyverse")
lapply(libs, library, character.only = TRUE)
options(digits = 3)
im = function(url) plot(imager::load.image(url), axes = F)
seed = function(x) set.seed(x, sample.kind = "Rounding")


# Load Data ---------------------------------------------------------------

data_entry = read.csv("data/Data_Entry_2017.csv")
xray_path = "C:/Users/Utilisateur/Documents/images/"

data = data_entry %>%
   mutate(health_status = ifelse(.$Finding.Labels == "No Finding", "healthy", "ill")) %>%
   select(Image.Index, Patient.Gender, Patient.Age, health_status) 


# Prepare data ----------------------------------------------

seed(18)
rnd_xrays = sample(data_entry$Image.Index, 10000)
xrays = matrix(0, 10000, 1024) 

for(i in 1:10)
{
   xray = paste(xray_path, rnd_xrays[i], sep = "") %>%
      load.image() %>%
      resize(64, 64, 1, 1) 
   sums = colSums(xray %>% as.matrix())
   xray = as.matrix(xray)[, -which(sums > mean(sums) + 1)] %>%
      as.cimg() %>%
      resize(32, 32, 1, 1)
   plot(xray)
   xrays[i,] = xray %>% as.numeric
}
save(xrays, file = "data/xrays.RData")

sds = colSds(xrays)
hist(sds, breaks = 50)

pixels_to_exclude = which(sds < 0.12)

xrays = xrays[, -pixels_to_exclude]

# Merge data 
col_names = c("image", "gender", "age", "status", 
              paste("pixel", as.character(1:ncol(xrays)), sep = ""))

data = data %>%
   filter(Image.Index %in% rnd_xrays)
data_final = cbind(as.matrix(data), xrays)
data_final = as_tibble(data_final)
colnames(data_final) = col_names
data_final = data_final %>%
   mutate(gender = ifelse(gender == "M", 0, 1))

save(data_final, file = "data/data_10k_sds.RData")



# Dimension Reduction - PCA ----------------------------------------------

load(file = "data/xrays.RData")
data = data %>%
   filter(Image.Index %in% rnd_xrays)

pca = prcomp(xrays, rank. = 15)
data.frame(pc1 = pca$x[,1], pc2 = pca$x[,2],
           status = data$health_status) %>%
   ggplot(aes(pc1, pc2, color = status)) +
   geom_point()

# Scree plot, PCs choice
summary(pca)[, 1:10]
plot(1:15, pca$sdev[1:15])

col_names = c("image", "gender", "age", "status", 
              paste("PC", as.character(1:10), sep = ""))

data_final = cbind(as.matrix(data), pca$x[, 1:10])
data_final = as_tibble(data_final)
colnames(data_final) = col_names
data_final

save(data_final, file = "data/data_10k_pca.RData")



# Data --------------------------------------------------------------------

rm(list = ls())
libs = c("MASS", "ggplot2", "imager", "broom", "caret", "dslabs", 
         "matrixStats", "purrr", "rpart", "randomForest", "lubridate",
         "tidyverse")
lapply(libs, library, character.only = TRUE)
options(digits = 3)
im = function(url) plot(imager::load.image(url), axes = F)
seed = function(x) set.seed(x, sample.kind = "Rounding")

load(file = "data/data_10k_sds.RData")
# data_final = data_final[which(data_final$pixel1 !=0), ]
index = createDataPartition(data_final$status, p = 0.2, list = F)
train_set = data_final[-index,]; test_set = data_final[index,]
outcome = train_set$status; predictors = train_set[, -c(1,4)]
rm(data_final)

save(train_set, file = "data/train.RData")
save(test_set, file = "data/test.RData")

# Train models ------------------------------------------------------------

load("data/train.RData")
load("data/test.RData")

train_logit = train(predictors[1:500,], outcome[1:500] %>% factor(), method = "glm")
logit = predict(train_logit, test_set)
confusionMatrix(logit, test_set$status)$overall["Accuracy"]













