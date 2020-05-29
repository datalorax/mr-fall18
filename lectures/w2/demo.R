library(rio)
library(here)
library(tidyverse)

d <- import(here("data", "elemapi.sav"),
            setclass = "tbl_df")
View(d)

ggplot(d, aes(mealcat, api00)) +
  geom_boxplot()

ggplot(d, aes(factor(mealcat), api00)) +
  geom_boxplot()

ggplot(d, aes(factor(mealcat), api00)) +
  geom_boxplot(color = yr_rnd)

ggplot(d, aes(factor(mealcat), api00)) +
  geom_boxplot(color = "yr_rnd")

ggplot(d, aes(factor(mealcat), api00)) +
  geom_boxplot(aes(color = yr_rnd))

ggplot(d, aes(factor(mealcat), api00)) +
  geom_boxplot(aes(color = factor(yr_rnd)))

ggplot(d, aes(factor(mealcat), api00)) +
  geom_boxplot(aes(fill = factor(yr_rnd)))

# Continuous
ggplot(d, aes(api99, api00)) +
  geom_point()

ggplot(d, aes(api99, api00)) +
  geom_smooth()

ggplot(d, aes(api99, api00)) +
  geom_point() +
  geom_smooth(method = "lm")
