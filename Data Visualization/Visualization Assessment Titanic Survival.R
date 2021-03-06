## The Titanic was a British ocean liner that struck an iceberg and sunk on its maiden voyage in 1912 from the United Kingdom to New York. More than
## 1,500 of the estimated 2,224 passengers and crew died in the accident, making this one of the largest maritime disasters ever outside of war. The 
## ship carried a wide range of passengers of all ages and both genders, from luxury travelers in first-class to immigrants in the lower classes.
## However, not all passengers were equally likely to survive the accident. We use real data about a selection of 891 passengers to learn who was 
## on the Titanic and which passengers were more likely to survive.

rm(list = ls())
library(titanic); library(tidyverse); library(ggplot2); library(ggridges)
options(digits = 3)

titanic = titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))


ggplot(titanic, aes(Age, ..count.., fill = Sex)) +
  geom_density(alpha=0.2) +
  facet_grid(Sex ~ .)
ggplot(titanic, aes(Age, fill = Sex)) +
  geom_density(alpha=0.2) +
  facet_grid(Sex ~ .)
ggplot(titanic, aes(Age, Sex, fill = Sex)) +
  geom_density_ridges(jittered_points = TRUE, scale = 1, alpha = 0.5)
hist(titanic$Age, breaks = 30)
plot(density(titanic$Age, na.rm = T, bw = 1))

summary(titanic$Sex)
summary(titanic[which(titanic$Age == 40),]$Sex)
summary(titanic[which(titanic$Age > 18 & titanic$Age < 35),]$Sex)
summary(titanic[which(titanic$Age <17),]$Sex)

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))
ggplot(titanic, aes(sample = Age)) +
  geom_qq(dparams = params) +
  geom_abline()

ggplot(titanic) +
  geom_bar(aes(Survived, fill = Sex), position = position_dodge())

filter(titanic, Survived == 1) %>%
  group_by(Sex) %>% summary()


ggplot(titanic, aes(Age, ..count.., fill = Survived)) +
  geom_density(alpha = 0.2)

filter(titanic, Fare > 0) %>%
  ggplot(aes(Survived, Fare)) +
  scale_y_continuous(trans = "log2") +
  geom_boxplot() +
  geom_jitter(alpha = 0.05)
filter(titanic, Fare > 0) %>%
  ggplot(aes(Survived, Fare)) +
  geom_boxplot() +
  geom_point(alpha = 0.05)

filter(titanic, Fare < 13 & Fare > 6) %>% summary()

## fill barplot
ggplot(titanic, aes(Pclass, fill = Survived)) +
  geom_bar()
ggplot(titanic, aes(Pclass, fill = Survived)) +
  geom_bar(position = position_fill())
ggplot(titanic, aes(Survived, fill = Pclass)) +
  geom_bar(position = position_fill())

ggplot(titanic, aes(Age, ..count.., fill = Survived)) +
  geom_density() +
  facet_grid(Sex ~ Pclass)
