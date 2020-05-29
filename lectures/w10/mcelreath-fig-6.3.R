# Code from Richard McElreath's book, chapter 6
sppnames <- c( "afarensis","africanus","habilis","boisei",
    "rudolfensis","ergaster","sapiens")
brainvolcc <- c( 438 , 452 , 612, 521, 752, 871, 1350 )
masskg <- c( 37.0 , 35.5 , 34.5 , 41.5 , 55.5 , 61.0 , 53.5 )
d <- data.frame( species=sppnames , brain=brainvolcc , mass=masskg )

## R code 6.2
m6.1 <- lm( brain ~ mass , data=d )

## R code 6.4
m6.2 <- lm( brain ~ mass + I(mass^2) , data=d )

## R code 6.5
m6.3 <- lm( brain ~ mass + I(mass^2) + I(mass^3) , data=d )
m6.4 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) ,
    data=d )
m6.5 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) +
    I(mass^5) , data=d )
m6.6 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) +
    I(mass^5) + I(mass^6) , data=d )


#### My code
library(tidyverse)
library(gganimate)
#devtools::install_github("thomasp85/transformr") # May need this too

theme_set(theme_minimal(base_size = 15))
mods <- list(m6.1, m6.2, m6.3, m6.4, m6.5, m6.6)
mod_preds <- map2_df(mods, seq_along(mods),
                     ~data_frame(mass = seq(30, 65, 0.1),
                                 title_lab = 
                                  paste0("Polynomial: ", .y,
                                         "; R^2: ", 
                                         round(summary(.x)$r.squared, 2)),
                 pred = predict(.x, 
                                newdata = 
                                  data.frame(mass = seq(30, 65, 0.1)))),
     .id = "model")
  
ggplot(mod_preds, aes(mass, pred)) +
  geom_line(color = "cornflowerblue", 
            lwd = 1.2) +
  xlim(25, 62) +
  ylim(-500, 2000) +
  geom_hline(yintercept = 0, 
             lwd = 1.4, 
             lty = 2, 
             color = "firebrick") +
  geom_point(aes(x = mass, y = brain), data = d) +
  transition_states(model,
                    transition_length = 0.5,
                    state_length = 0.5) +
  transition_states(title_lab,
                    transition_length = 0.5, 
                    state_length = 0.5) +
  labs(title = "{closest_state}",
       x = "body mass (kg)",
       y = "brain volume (cc)")
  