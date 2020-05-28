m <- lm(depression_score ~ health + income + age + sex, d)
summary(m)
modelEffectSizes(m)

m2 <- lm(depression_score ~ health + income + age, d)
m3 <- lm(depression_score ~ health + income + sex, d)
m4 <- lm(depression_score ~ health  + age + sex, d)
m5 <- lm(depression_score ~ income + age + sex, d)
summary(m5)

sum(
c(
summary(m)$r.squared - summary(m2)$r.squared,
summary(m)$r.squared - summary(m3)$r.squared,
summary(m)$r.squared - summary(m4)$r.squared,
summary(m)$r.squared - summary(m5)$r.squared
)
)

health <- d %>% 
  select(id, health) %>% 
  mutate(d1 = 1) %>% 
  spread(health, d1, fill = 0)

sex <- d %>% 
  select(id, sex) %>% 
  mutate(d1 = 1) %>% 
  spread(sex, d1, fill = 0)

d %>% 
  select(id, depression_score, income, age) %>% 
  left_join(health) %>% 
  left_join(sex) %>% 
  select(-id) %>% 
  cor()
