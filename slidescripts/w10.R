library(here)
library(tidyverse)
library(rio)

theme_set(theme_minimal())

## ----forward-selection---------------------------------------------------
simple <- lm(mpg ~ 1, data = mtcars)
full <- formula(lm(mpg ~ ., mtcars)) # note call wrapped in `formula`
fwd_mod <- step(simple, 
                scope = full,
                direction = "forward")

## ----forward-anova-------------------------------------------------------
arm::display(fwd_mod, detail = TRUE)
fwd_mod$anova

## ----backward-selection--------------------------------------------------
full <- lm(mpg ~ ., data = mtcars)
simple <- formula(lm(mpg ~ 1, mtcars)) # note call wrapped in `formula`
back_mod <- step(full, 
                scope = simple,
                direction = "backward")

## ----back-anova----------------------------------------------------------
arm::display(back_mod, detail = TRUE)
back_mod$anova

## ----stepwise-selection--------------------------------------------------
simple <- lm(mpg ~ 1, data = mtcars)
full <- formula(lm(mpg ~ ., mtcars))  # note call wrapped in `formula`
step_mod <- step(simple, 
                scope = full,
                direction = "both")

## ----step-anova----------------------------------------------------------
arm::display(step_mod, detail = TRUE)
step_mod$anova

## ----train-test----------------------------------------------------------
set.seed(8675309)

train <- mtcars %>%
  sample_frac(.8)

test <- anti_join(mtcars, train)
nrow(train)
nrow(test)

## ----train-mods----------------------------------------------------------
m1 <- lm(mpg ~ hp, train)
m2 <- lm(mpg ~ hp + disp, train)
m3 <- lm(mpg ~ hp + disp + cyl, train)
sundry::aic_weights(m1, m2, m3)

## ----test_preds----------------------------------------------------------
test <- test %>%
  mutate(pred_mpg = predict(m2, newdata = test))

test

## ----diff----------------------------------------------------------------
test %>%
  mutate(diff = pred_mpg - mpg)

## ----mse-----------------------------------------------------------------
test %>%
  summarize(mse = mean((pred_mpg - mpg)^2))

## ----rmse----------------------------------------------------------------
test %>%
  summarize(rmse = sqrt(mean((pred_mpg - mpg)^2)))

## ----k-fold-cv-----------------------------------------------------------
library(modelr)

iris %>%
  crossv_kfold(10) %>%
  mutate(model = map(train, ~lm(Sepal.Length ~ Petal.Length, data=.)))

## ----k-fold-cv-rmse------------------------------------------------------
iris %>%
  crossv_kfold(10) %>%
  mutate(model = map(train, ~lm(Sepal.Length ~ Petal.Length, data=.)),
         rmse = map2_dbl(model, test, rmse)) #<<

## ----k-fold-cv-rmse-summary----------------------------------------------
iris %>%
  crossv_kfold(10) %>%
  mutate(model = map(train, 
                     ~lm(Sepal.Length ~ Petal.Length, 
                         data=.)),
         rmse = map2_dbl(model, test, rmse)) %>%
  summarize(mean_rmse = mean(rmse)) #<<

## ----k-fold-cv-rmse-nonlinear--------------------------------------------
iris %>%
  crossv_kfold(10) %>%
  mutate(model = map(train, 
                     ~lm(Sepal.Length ~ poly(Petal.Length, 3), 
                         data=.)),
         rmse = map2_dbl(model, test, rmse)) %>%
  summarize(mean_rmse = mean(rmse)) #<<

