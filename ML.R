
# load_libraries ------------------------------------------

library(caret)
library(sna)
library(tidyquant)
library(tsibble)
library(tidyverse)
library(data.table)
library(broom)
# SEMMA ---------------------------------------------------

data(coleman)

coleman %>% 
    summary()

df <-   diamonds %>% 
        group_by(color) %>% 
        nest()

df<-df %>% 
    mutate(
        model=map(data,
                 ~glance(lm(carat~cut+depth,data = .x))
                 ) %>% 
    mutate(
        rsquare= map_dbl(
            model,
            'r.squared'
        )
    )
