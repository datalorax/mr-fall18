library(here)
library(tidyverse)
library(rio)

theme_set(theme_minimal())

## ----load-elemapi--------------------------------------------------------
elemapi <- import(here("data", "elemapi.sav"),
                  setclass = "tbl_df") %>%
  characterize()

head(elemapi)

## ----cor-----------------------------------------------------------------
elemapi %>%
  select(api00, meals, ell, mobility, acs_k3, acs_46, emer, enroll) %>%
  cor(use = "complete.obs") %>%
  round(2)

## ----mtcars-cor----------------------------------------------------------
mtcars %>%
  cor() %>%
  round(2)

## ----mod-----------------------------------------------------------------
m <- lm(mpg ~ hp + cyl + disp, mtcars)
arm::display(m, detail = TRUE)

## ------------------------------------------------------------------------
hp_mod <- lm(mpg ~ hp, mtcars)
summary(hp_mod)$r.squared

cyl_mod <- lm(mpg ~ cyl, mtcars)
summary(cyl_mod)$r.squared

disp_mod <- lm(mpg ~ disp, mtcars)
summary(disp_mod)$r.squared

## ----standard-errors----------------------------------------------------
arm::display(m, digits = 4)

arm::display(hp_mod, digits = 4)
arm::display(cyl_mod, digits = 4)

arm::display(disp_mod, digits = 4)

## ----bootstrapping---------------------------------------------------------------
set.seed(8675309)
samples <- replicate(2500, 
                     mtcars[sample(1:nrow(mtcars),
                              size = nrow(mtcars),
                              replace = TRUE), ],
                     simplify = FALSE)

## ---- fit-models---------------------------------------------------------
models <- map(samples, ~lm(mpg ~ hp + cyl + disp, .))

## ----boot-se---------------------------------------------------------------
boot_se <- map_df(models, 
                    ~data.frame(coef = coef(.),
                                param = c("intercept",
                                          "b1_hp",
                                          "b2_cyl",
                                          "b3_disp")), 
                    .id = "iteration") %>% 
  tbl_df() %>%
  group_by(param) %>%
  summarize(se   = sd(coef)) %>%
  spread(param, se)

## ------------------------------------------------------------------------
boot_se
arm::display(m, digits = 4)

## ------------------------------------------------------------------------
hp_tol_mod <- lm(hp ~ cyl + disp, mtcars)
1 - summary(hp_tol_mod)$r.squared

## ----vif-hp--------------------------------------------------------------
1 / (1 - summary(hp_tol_mod)$r.squared)

## ----car-vif-------------------------------------------------------------
car::vif(m)

## ----car-tol-------------------------------------------------------------
 1 / car::vif(m)

## ----quad---------------------------------------------
d <- import(here("data", "simdata.csv"),
            setclass = "tbl_df")
ggplot(d, aes(x, y)) +
	geom_point() +
	geom_smooth()

## ----squared-var---------------------------------------------------------
d <- d %>%
	mutate(x2 = x^2)
d

## ----quadratic---------------------------------------------------------------
q1 <- lm(y ~ x + x2, d)
arm::display(q1, detail = TRUE)

## ----q1-fitted-plot------------------------------------
d <- d %>%
	mutate(fitted_q1 = fitted(q1)) 

ggplot(d, aes(x, y)) +
	geom_point() +
	geom_line(aes(y = fitted_q1),
	          color = "magenta", 
	          size = 2)

## ----squared-df1---------------------------------------------------------
data_frame(x = 0:10,
           x2 = x^2,
           b1 = coef(q1)[2],
           b2 = coef(q1)[3])

## ----squared-df2---------------------------------------------------------
data_frame(x = 0:10,
           x2 = x^2,
           b1 = coef(q1)[2],
           b2 = coef(q1)[3],
           y_hat = x*b1 + x2*b2)

## ----q2-fit--------------------------------------------------------------
q2 <- lm(y ~ poly(x, 2, raw = TRUE), d)
arm::display(q2, detail = TRUE)

## ----q1-display-again----------------------------------------------------
arm::display(q1, detail = TRUE)

## ----orthogonal----------------------------------------------------------
q2o <- lm(y ~ poly(x, 2), d)
arm::display(q2o)

## ----collinear-q1--------------------------------------------------------
car::vif(q1)

## ----cor-squared---------------------------------------------------------
cor(d$x, d$x2)

## ----q1-display----------------------------------------------------------
arm::display(q1, digits = 4)

## ----center-quadratic----------------------------------------------------
d <- d %>%
	mutate(xc = x - mean(x),
	       x2c = xc^2)
cor(d$xc, d$x2c)

## ----refit-centered-quadratic--------------------------------------------
q1c <- lm(y ~ xc + x2c, d)
arm::display(q1c, detail = TRUE, digits = 4)

## ----vars----------------------------------------------------------------
vcov(q1)
vcov(q1c)

## ----vif-by-hand---------------------------------------------------------
vcov(q1)[2, 2] /
vcov(q1c)[2, 2]
car::vif(q1)

## ----se-increase--------------------------------------------------------
summary(q1)$coefficients[, "Std. Error"]["x"] 
summary(q1c)$coefficients[, "Std. Error"]["xc"]

summary(q1)$coefficients[, "Std. Error"]["x"] /
summary(q1c)$coefficients[, "Std. Error"]["xc"]

sqrt(car::vif(q1))

## ----depression------------------------------------------
ggplot(elemapi, aes(mobility, api00)) +
	geom_point() +
	geom_smooth() 

## ----q3------------------------------------------------------------------
q3 <- lm(api00 ~ poly(mobility, 2, raw = TRUE), elemapi)
arm::display(q3, detail = TRUE)

## ----q3-visualize-------------------------------------
elemapi <- elemapi %>%
	mutate(fitted_q3 = fitted(q3))

ggplot(elemapi, aes(mobility, api00)) +
	geom_point() +
	geom_line(aes(y = fitted_q3),
	          color = "magenta",
	          size = 2)


## ------------------------------------------------------------------------
linear_api <- lm(api00 ~ mobility, elemapi)
anova(linear_api, q3)
sundry::aic_weights(linear_api, q3)

## ----cubic---------------------------------------------------------------
cubic1 <- lm(api00 ~ poly(mobility, 3, raw = TRUE), elemapi)
arm::display(cubic1, detail = TRUE)

## ----visualize-cubic----------------------------------
elemapi <- elemapi %>%
	mutate(fitted_cubic1 = fitted(cubic1))

ggplot(elemapi, aes(mobility, api00)) +
	geom_point() +
	geom_line(aes(y = fitted_cubic1),
	          color = "magenta",
	          size = 2)

## ----hist-raw--------------------------------------------
ggplot(elemapi, aes(mobility)) +
	geom_histogram()

## ----hist-log------------------------------------------------------------
ggplot(elemapi, aes(log(mobility))) +
	geom_histogram()

## ----log-----------------------------------------------------------------
elemapi <- elemapi %>%
	mutate(log_mobility = log(mobility)) 

elemapi %>%
	select(mobility, log_mobility)

## ----linear-transform----------------------------------------------------
ggplot(elemapi, aes(mobility, log_mobility)) +
	geom_point()


## ----log-fit-------------------------------------------------------------
l1 <- lm(api00 ~ log_mobility, elemapi)
arm::display(l1, detail = TRUE)

## ----visualize-log------------------------------------
elemapi <- elemapi %>%
	mutate(fitted_l1 = fitted(l1))

ggplot(elemapi, aes(mobility, api00)) +
	geom_point() +
	geom_line(aes(y = fitted_l1),
	          color = "magenta",
	          size = 2)

## ----compare-mods--------------------------------------------------------
sundry::aic_weights(q3, cubic1, l1)

