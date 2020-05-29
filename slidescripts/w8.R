library(here)
library(tidyverse)
library(rio)

theme_set(theme_minimal())

benchmarks <- import(here("data", "benchmarks.xlsx"),
              setclass = "tbl_df") 

m1 <- lm(rdg_spr ~ rdg_fall, benchmarks)
arm::display(m1, detail = TRUE)

## ----lmSupport-----------------------------------------------------------
library(lmSupport)
modelEffectSizes(m1)

## ----m0_fit--------------------------------------------------------------
m0 <- lm(rdg_spr ~ 1, benchmarks)
arm::display(m0, detail = TRUE, digits = 4)

## ----mean----------------------------------------------------------------
mean(benchmarks$rdg_spr)

## ----fitted--------------------------------------------------------------
benchmarks %>%
  mutate(m0_fitted = fitted(m0))

## ----m1-display-----------------------------------
summary(m1)

## ----m0-m1-compare---------------------------------
anova(m0, m1)

## ------------------------------------------------------------------------
m2 <- lm(rdg_spr ~ rdg_fall + sped, benchmarks)
arm::display(m2)

## ----r2-diff-------------------------------------------------------------
summary(m1)$r.squared
summary(m2)$r.squared

## ----sped-ssr------------------------------------------------------------
summary(m2)$r.squared - summary(m1)$r.squared

## ----m1-m2-compare-------------------------------------------------------
anova(m1, m2)

## ----sped-mod------------------------------------------------------------
sped_mod <- lm(rdg_spr ~ sped, benchmarks)
summary(m2)$r.squared - summary(sped_mod)$r.squared
modelEffectSizes(m2)

## ----sped-mod-m2-compare-------------------------------------------------
anova(m2, sped_mod)

## ----scipen-off, echo = FALSE--------------------------------------------
options(scipen = 999)

## ----compare-pvals-------------------------------------------------------
anova(m2, sped_mod)$`Pr(>F)`[2]
summary(m2)$coefficients["rdg_fall", "Pr(>|t|)"]

## ----scipen-on, echo = FALSE---------------------------------------------
options(scipen = 2)

## ----aic-----------------------------------------------------------------
AIC(m0)
AIC(m1)
AIC(m2)

## ----m012-compare--------------------------------------------------------
anova(m0, m1, m2)

## ----m01-aic-compare-----------------------------------------------------
AIC(m1) - AIC(m0)

## ----m12-aic-compare-----------------------------------------------------
AIC(m2) - AIC(m1)

## ----sundry-install, eval = FALSE----------------------------------------
## # You may have to install {remotes} first
install.packages("remotes")
remotes::install_github("datalorax/sundry")

## ----aic-compare---------------------------------------------------------
sundry::aic_weights(m0, m1, m2)

## ----diag----------------------------------------------------------------
diag <- import(here("data", "diagnostics.sav"),
               setclass = "tbl_df") %>% 
  characterize()

head(diag)

## ----block-predictor1, highlight.output = 8------------------------------
yc_mod <- lm(ycweight ~ ycage + ycheight, diag)
arm::display(yc_mod, detail = TRUE)

## ----block-predictor2, highlight.output = 12-----------------------------
father_mod <- lm(ycweight ~ ycage + ycheight + 
                            fage + fheight + fweight,
                 data = diag)
arm::display(father_mod, detail = TRUE)

## ----father-mod-update---------------------------------------------------
father_mod <- update(yc_mod, ~ . + fage + fheight + fweight)

## ----block-predictor3, highlight.output = 12-----------------------------
mother_mod <- lm(ycweight ~ ycage + ycheight + 
                            mage + mheight + mweight,
                 data = diag)
arm::display(mother_mod, detail = TRUE)

## ----compare-father-mother-----------------------------------------------
anova(father_mod, yc_mod)
anova(mother_mod, yc_mod)

## ----compare-father-mother-aic-------------------------------------------
sundry::aic_weights(father_mod, mother_mod, yc_mod)

## ----block-predictor4, highlight.output = 15-----------------------------
parent_mod <- lm(ycweight ~ ycage + ycheight + 
                            fage  + fheight  + fweight +
                            mage  + mheight  + mweight, 
                 data = diag)
arm::display(parent_mod, detail = TRUE)

## ----compare-parent------------------------------------------------------
anova(yc_mod, parent_mod)

## ----compare-parent-aic--------------------------------------------------
sundry::aic_weights(yc_mod, father_mod, mother_mod, parent_mod)

## ----block-predictor5----------------------------------------------------
family_mod <- lm(ycweight ~ ycage + ycheight +
                            fage  + fheight  + fweight +
                            mage  + mheight  + mweight +
                            ocage + ocheight + ocweight, 
                 data = diag)

## ----family-block-display-------------------------
arm::display(family_mod, detail = TRUE)

## ----compare-family------------------------------------------------------
anova(yc_mod, family_mod)

## ----compare-family-aic--------------------------------------------------
sundry::aic_weights(yc_mod, father_mod, mother_mod, 
                    parent_mod, family_mod)

## ----prep----------------------------------------------------------------
benchmarks <- benchmarks %>%
  mutate(rdg_fall_c = rdg_fall - mean(rdg_fall))

## ----ell-plot--------------------------------------------
ggplot(benchmarks, aes(rdg_fall, rdg_spr)) +
  geom_point(color = "gray70") +
  geom_smooth(aes(color = ell),
              method = "lm")

## ----ell-interaction-----------------------------------------------------
ell_int <- lm(rdg_spr ~ rdg_fall_c + ell + rdg_fall_c:ell, benchmarks)
arm::display(ell_int, detail = TRUE, digits = 3)

## ----ell-parallel--------------------------------------------------------
ell_parallel <- lm(rdg_spr ~ rdg_fall_c + ell, benchmarks)
arm::display(ell_parallel, detail = TRUE)

## ----ell-compare---------------------------------------------------------
anova(ell_parallel, ell_int)

## ----ell-weights---------------------------------------------------------
sundry::aic_weights(ell_parallel, ell_int)

## ------------------------------------------------------------------------
glo <- import(here("data", "glo_sim.sav"),
              setclass = "tbl_df") %>%
  characterize()

head(glo)

## ----glo-control---------------------------------------------------------
glo_m1 <- lm(sex_risk ~ age + 
                        parent_education + parent_income + 
                        parent_eth + parent_race + 
                        teen_eth + teen_race,
             data = glo)

## ----glo-main-effects----------------------------------------------------
glo_m2 <- lm(sex_risk ~ internalizing + externalizing + aces +
                        age + 
                        parent_education + parent_income + 
                        parent_eth + parent_race + 
                        teen_eth + teen_race,
             data = glo)

## ----glo-interactions----------------------------------------------------
glo_m3 <- lm(sex_risk ~ internalizing + externalizing + aces +
                        internalizing:aces + externalizing:aces +
                        age + 
                        parent_education + parent_income + 
                        parent_eth + parent_race + 
                        teen_eth + teen_race,
             data = glo)

## ----compare-glo12-------------------------------------------------------
anova(glo_m1, glo_m2)

## ----compare-aic-glo12---------------------------------------------------
sundry::aic_weights(glo_m1, glo_m2)

## ----compare-glo23-------------------------------------------------------
anova(glo_m2, glo_m3)

## ----compare-aic-glo23---------------------------------------------------
sundry::aic_weights(glo_m1, glo_m2, glo_m3)


