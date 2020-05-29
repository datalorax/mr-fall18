## ----setup, include = FALSE----------------------------------------------
library(here)
library(tidyverse)
library(rio)

theme_set(theme_minimal())

## ----m1------------------------------------------------------------------
diag <- import(here("data", "diagnostics.sav"),
            setclass = "tbl_df") %>%
      characterize()
      
m1 <- lm(ycweight ~ mweight, diag)
arm::display(m1, detail = TRUE)  
confint(m1)

## ----scatter1, fig.height = 6--------------------------------------------
ggplot(diag, aes(mweight, ycweight)) +
  geom_point() +
  geom_smooth(method = "lm")

## ----scatter2, fig.height = 5.5------------------------------------------
ggplot(diag, aes(mweight, ycweight)) +
  geom_point() +
  geom_smooth(method = "lm",
              fullrange = TRUE) +
  xlim(0, 245)

## ----scatter3, fig.height = 4.5------------------------------------------
ggplot(diag, aes(mweight, ycweight)) +
  geom_point() +
  geom_smooth(method = "lm",
              fullrange = TRUE) +
  xlim(0, 245) +
  geom_point(data = data.frame(mweight = 0,
                               ycweight = coef(m1)[1]),
             size = 7,
             color = "magenta")

## ----mutate--------------------------------------------------------------
diag <- diag %>%
  mutate(weight200 = mweight - 200)

## ----scatters1a, fig.height = 6------------------------------------------
ggplot(diag, aes(mweight, ycweight)) +
  geom_point()

## ----scatters1b, fig.height = 6------------------------------------------
ggplot(diag, aes(weight200, ycweight)) +
  geom_point()

## ----scatters2a, fig.height = 6------------------------------------------
ggplot(diag, aes(mweight, ycweight)) +
  geom_point() +
  geom_smooth(method = "lm")

## ----scatters2b, fig.height = 6------------------------------------------
ggplot(diag, aes(weight200, ycweight)) +
  geom_point() +
  geom_smooth(method = "lm", color = "magenta")


## ----mod-centered--------------------------------------------------------
m1c_200 <- lm(ycweight ~ weight200, diag)
arm::display(m1c_200, detail = TRUE)

## ----mod1-summary--------------------------------------------------------
arm::display(m1, detail = TRUE)


## ----centerd-exercise----------------------------------------------------
diag <- diag %>%
  mutate(age_c = ycage - mean(ycage),
         weight_c = mweight - mean(mweight))

m2 <- lm(ycweight ~ age_c + weight_c, diag)

## ----display-m2----------------------------------------------------------
arm::display(m2, detail = TRUE)

## ----standardize1--------------------------------------------------------
diag <- diag %>%
  mutate(mweight_z = (mweight - mean(mweight)) / sd(mweight))

mean(diag$mweight_z)
sd(diag$mweight_z)

## ----m1z-----------------------------------------------------------------
m1z_a <- lm(ycweight ~ mweight_z, diag)

## ----display-m1z_a-------------------------------------------------------
arm::display(m1z_a)


## ----standardized-model--------------------------------------------------
diag <- diag %>%
  mutate(mweight_z = scale(mweight),
         ycweight_z = scale(ycweight))

## ----m1z_b---------------------------------------------------------------
m1z_b <- lm(ycweight_z ~ mweight_z, diag)

## ----display-m1z_b-------------------------------------------------------
arm::display(m1z_b, detail = TRUE)

## ----cor-----------------------------------------------------------------
cor(diag$mweight, diag$ycweight)

## ----lmbeta1-------------------------------------------------------------
# install.packages("lm.beta") # Uncomment this line and run to install
library(lm.beta)
lm.beta(m1) 

## ----lmbeta2-------------------------------------------------------------
m_complex <- lm(ycweight ~ mage + mheight + mweight + 
                           fage + fheight + fweight +
                           ycage + ycheight, 
                           data = diag)
lm.beta(m_complex)

## ----interaction-plot1-eval, echo = FALSE, fig.height = 9----------------
benchmarks <- import(here("data", "benchmarks.xlsx"),
                     setclass = "tbl_df")

ggplot(benchmarks, aes(rdg_fall, rdg_spr)) +
  geom_point(color = "gray70") +
  geom_smooth(aes(color = ell),
              method = "lm")

## ----parallel------------------------------------------------------------
benchmarks <- benchmarks %>%
  mutate(ell = factor(ell),
         ell = relevel(ell, ref = "Non-ELL"))

m3a <- lm(rdg_spr ~ rdg_fall + ell, benchmarks)
arm::display(m3a, detail = TRUE)

## ----center-rdg_fall-----------------------------------------------------
benchmarks <- benchmarks %>%
  mutate(rdg_fall_c = scale(rdg_fall, scale = FALSE)) 

## ----interaction-mod1----------------------------------------------------
m3b <- lm(rdg_spr ~ rdg_fall_c + ell + rdg_fall_c:ell, benchmarks)

## ----display-m3b, highlight.output = 7:8---------------------------------
arm::display(m3b, detail = TRUE, digits = 3)

## ----m3b-visualized, echo = FALSE, fig.height = 9------------------------
benchmarks <- benchmarks %>%
  mutate(m3b_fitted = fitted(m3b))

ggplot(benchmarks, aes(rdg_fall_c, rdg_spr)) +
  geom_point(color = "gray80") +
  geom_line(aes(color = ell, y = m3b_fitted),
            size = 2) +
  labs(title = "Our fitted model",
       subtitle = "Note this looks basically the exact same as our model when
       we let ggplot2 do it")

## ----m3b-raw, highlight.output = c(3, 5:6)-------------------------------
m3b_raw <- lm(rdg_spr ~ rdg_fall + ell + rdg_fall:ell, benchmarks)
arm::display(m3b_raw, detail = TRUE, digits = 3)


## ----semipartials-m3b----------------------------------------------------
library(lmSupport)
modelEffectSizes(m3b)

## ----hsb-----------------------------------------------------------------
hsb <- import(here("data", "hsb2.sav"),
                  setclass = "tbl_df") %>%
  characterize() %>%
  mutate(ses = factor(ses),
         ses = relevel(ses, ref = "middle"),
         schtyp = factor(schtyp),
         schtyp = relevel(schtyp, ref = "public"))
head(hsb)


## ----m4------------------------------------------------------------------
m4 <- lm(math ~ schtyp + ses + schtyp:ses, hsb)
arm::display(m4, detail = TRUE, digits = 3)

## ----m4_plot, fig.height = 5---------------------------------------------
hsb <- hsb %>%
  mutate(fitted_m4 = fitted(m4)) 

ggplot(hsb, aes(as.numeric(schtyp) - 1, fitted_m4, color = ses)) +
  geom_line()

## ----glo-----------------------------------------------------------------
glo <- import(here("data", "glo_sim.sav"),
            setclass = "tbl_df") %>%
      characterize()
head(glo)

## ------------------------------------------------------------------------
glo <- glo %>%
  mutate(intern_cut = cut(internalizing, 3),
         extern_cut = cut(externalizing, 3))
glo %>%
  select(internalizing, intern_cut, externalizing, extern_cut)

## ----internalizing_int_plot----------------------------------------------
ggplot(glo, aes(aces, sex_risk, color = intern_cut)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  theme(legend.position = "top")

## ----externalizing_int_plot----------------------------------------------
ggplot(glo, aes(aces, sex_risk, color = extern_cut)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  theme(legend.position = "top")

## ----cont_int------------------------------------------------------------
m5 <- lm(sex_risk ~ internalizing + externalizing + aces + 
                    aces:externalizing + aces:internalizing, 
                    data = glo)

## ----m5-display----------------------------------------------------------
arm::display(m5, detail = TRUE, digits = 3)

## ----cis-m5--------------------------------------------------------------
confint(m5)

## ----semipartials-m5-----------------------------------------------------
modelEffectSizes(m5)

## ----fig1, fig.height = 5.5----------------------------------------------
library(visreg)
visreg(m5, xvar = "aces", by = "externalizing", overlay = TRUE)

## ----fig2----------------------------------------------------------------
visreg(m5, xvar = "aces", by = "internalizing", overlay = TRUE)

