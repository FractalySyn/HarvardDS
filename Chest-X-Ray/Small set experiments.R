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

data_entry %>%
   mutate(Finding.Labels = factor(Finding.Labels)) %>%
   group_by(Finding.Labels) %>%
   summarise(n = n()) %>%
   arrange(desc(n)) 

data = data_entry %>%
   select(Image.Index, Patient.Gender, Patient.Age, Finding.Labels) %>%
   filter(Finding.Labels %in% c("Consolidation", "No Finding")) 
index = createDataPartition(data$Finding.Labels, p = 0.01, list = F)
data = data[index,]



# Wrangling ----------------------------------------------------

xrays = matrix(0, nrow(data), 64^2) 

for(i in 1:nrow(data))
{
   xray = paste(xray_path, data$Image.Index[i], sep = "") %>%
      load.image() %>%
      resize(64, 64, 1, 1) %>%
      as.numeric()
   xrays[i,] = xray
}
save(xrays, file = "data/small_xrays.RData")   

col_names = c("image", "status", "gender", "age", 
              paste("pixel", as.character(1:ncol(xrays)), sep = ""))   

data_final = cbind(as.matrix(data), xrays)
data_final = data_final %>%
   as_tibble() %>%
   mutate(Patient.Gender = ifelse(Patient.Gender == "M", 0, 1))
data_final = data_final[,c(1,4)] %>%
   cbind(apply(as.matrix(data_final[, -c(1,4)]), 2, function(x){
      as.numeric(x)
   }))
data_final = as_tibble(data_final)
colnames(data_final) = col_names

save(data_final, file = "data/data_small.RData")   



# Partitionning -----------------------------------------------------------

testind = createDataPartition(data_final$status, p = 0.2, list = F)
test = data_final[testind,]; outcome = data_final[-testind, 2]; predictors = data_final[-testind, -c(1,2)]

pca = prcomp(predictors, rank. = 10)
summary(pca)
plot(1:10, pca$sdev[1:10])

fit_knn = knn3(predictors, outcome$status %>% factor())
fit_rf = train(predictors, outcome$status %>% factor(), method = "rf")











