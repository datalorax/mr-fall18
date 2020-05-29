library(tidyverse)
library(rio)
library(here)
library(haven)
library(synthpop)

esvis::benchmarks %>%
  select(-math, -cohort) %>% 
  spread(season, reading) %>% 
  select(-Winter) %>% 
  tbl_df() %>% 
  na.omit() %>% 
  rename(rdg_fall = Fall,
         rdg_spr = Spring) %>% 
  export(here("data", "benchmarks.xlsx"))

set.seed(8675309)
x <- 0:100
y <- 5 + 10*x + -0.07*x^2 + rnorm(length(x), 0, 20)
y <- c(y, unlist(replicate(4, y + runif(101, -20, 20), simplify = FALSE)))
y <- ifelse(y < 0, 0, y)

sim_data <- data_frame(x = rep(x, 5), y) %>%
	arrange(x)

write_csv(sim_data, here("data", "simdata.csv"))

d <- import(here("data", "diagnostics.sav"),
            setclass = "tbl_df") %>% 
  characterize()

d %>% 
  rename(mfvc = fmvc) %>% 
  gather(group, val, -1:-2) %>% 
  mutate(var = str_extract(group, "sex|age|height|weight|fvc|fev1"),
         group = str_replace(group, "sex|age|height|weight|fvc|fev1", "")) %>% 
  spread(var, val) %>% 
  write_csv(here("data", "tidy_diagnostics.csv"))

d <- read_sav("/Users/Daniel/BRT/leslie/Kovensky/data/KovenskyWPA20171122.sav") %>% 
  janitor::clean_names("old_janitor") %>% 
  as_factor() %>% 
  dplyr::select(-p591intt, -p591inr, -p591extt, -p591exr, -p11112)

names(d) <- c("id", "condition", "age", "aces", "sex_risk", "internalizing", 
              "externalizing", "parent_education", "parent_income", "parent", 
              "teen_eth", "teen_race", "parent_eth", "parent_race")
d <- d %>% 
  filter(!is.na(sex_risk),
         !is.na(age),
         !is.na(internalizing),
         !is.na(externalizing),
         !is.na(aces))
d_sim <- d %>% 
  dplyr::select(-id) %>% 
  syn() 

d_sim$syn %>% 
  mutate(id = rownames(.)) %>%
  rio::export(here("data", "glo_sim.sav"))



tmp <- import("/Users/Daniel/Dropbox/EDUC 642/DataSets/depressAfifi.sav",
              setclass = "tbl_df") %>% 
  characterize()
tmp %>% 
  select(-matches("C\\d")) %>% 
  janitor::clean_names() %>% 
  select(id:relig, drink:chronill, cesd) %>% 
  rename(depression_score = cesd) %>% 
  export(here("data", "depression.xlsx"))

kahn <- import("/Users/Daniel/Desktop/anderson-data-project/data/E&R_candidate_assignment_sample_data.csv",
               setclass = "tbl_df")
kahn_sim <- syn(kahn)
kahn_sim$syn %>% 
  tbl_df() %>% 
  dplyr::select(-contains("level"), -contains("met")) %>% 
  mutate(frl = ifelse(frl_status, "FRL", "Non-FRL"),
         ell = ifelse(ell_status, "ELL", "Non-ELL"),
         sch_type = ifelse(magnet_school, "Magnet", "Neighborhood"),
         grade = parse_number(grade),
         video_hours = log(video_minutes + 1),
         practice_hours = log(skill_minutes + 1)) %>% 
  rename(math17 = math_scale_score_2017,
         math18 = math_scale_score_2018) %>% 
  dplyr::select(student, gender, race, frl, ell, 
         practice_hours, video_hours, math17, math18) %>% 
  filter(practice_hours >= 4) %>% 
  filter(video_hours >= 2) %>% 
  na.omit() %>% 
  write_csv(here("data", "online-training.csv"))


alt <- import("/Users/Daniel/BRT/Manuscripts/AltAssessGrowth/ReadG345_VF.csv",
              setclass = "tbl_df") %>% 
  janitor::clean_names()

alt <- alt %>% 
  dplyr::select(-n_time_pts) %>% 
  gather(disability, dummy, id:sld) %>% 
  filter(dummy == 1) %>% 
  dplyr::select(-dummy, -miss_pat) %>% 
  mutate(sex = ifelse(male == 1, "Male", "Female"),
         ethnicity = ifelse(white == 1, "White", "Non-White")) %>% 
  gather(timepoint, score, starts_with("rit")) %>% 
  mutate(timepoint = parse_number(timepoint) - 11) %>% 
  filter(score != 999) %>% 
  dplyr::select(-male, -white)

set.seed(10)
alt_syn <- syn(alt)$syn %>% 
  write_csv(here("data", "alt-assess.csv"))


diss <- import("/Users/Daniel/BRT/Manuscripts/Dissertation/TeacherSchoolEffects/analysis/g345V2.Rda")

diss <- diss %>% 
  janitor::clean_names() %>% 
  dplyr::select(sid, cohort, sc_id, grade, gender, sped_cat, gifted, eth, 
                frl, occasion, r_rit, m_rit) %>% 
  tbl_df() %>% 
  mutate(wave = case_when(occasion == "g3f" ~ 0,
                          occasion == "g3w" ~ 1,
                          occasion == "g3s" ~ 2,
                          occasion == "g4f" ~ 3,
                          occasion == "g4w" ~ 4,
                          occasion == "g4s" ~ 5,
                          occasion == "g5f" ~ 6,
                          occasion == "g5w" ~ 7,
                          occasion == "g5s" ~ 8,
                          TRUE ~ NA_real_),
         sped_cat = ifelse(sped_cat == " ", "No Disability", sped_cat),
         gifted = ifelse(gifted == 1, "G/T", "Non-G/T"),
         frl = ifelse(frl == 1, "FRL", "Non-FRL")) %>% 
  filter(grepl("A", cohort)) %>% 
  filter(!is.na(m_rit) & !is.na(r_rit)) %>% 
  dplyr::select(-cohort, -occasion, -sped_cat)

diss_syn <- diss %>% 
  rename(scid = sc_id,
         reading = r_rit,
         math = m_rit) %>% 
  dplyr::select(sid:frl, wave, reading, math) %>% 
  syn() 

diss_syn <- diss_syn$syn %>% 
  group_by(sid) %>% 
  add_count() %>% 
  filter(n == 9) %>% 
  dplyr::select(-n) 

sample <- data.frame(sid = as.numeric(sample(levels(factor(diss_syn$sid)), 100)))
semi_join(diss_syn, sample) %>% 
  arrange(sid, wave) %>% 
  write_csv(here("data", "longitudinal.csv"))
  
semi_join(diss_syn, sample) %>% 
ggplot(aes(wave, math)) +
    geom_point() +
    geom_smooth()

