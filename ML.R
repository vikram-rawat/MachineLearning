# load_libraries ------------------------------------------
rm(list = ls())
library(plotly)
library(gmp)
library(caret)
library(sna)
library(pryr)
library(speedglm)
library(tidyquant)
library(tsibble)
library(tidyverse)
library(data.table)
library(broom)
library(TSstudio)

# SEMMA ---------------------------------------------------

data(coleman)

coleman %>%
    summary()


df <-diamonds %>%
    group_by(color) %>%
    nest() %>%
    mutate(model = map(data,
                       ~ glance(speedlm(
                           carat ~ cut + depth, data = .x
                       )))) %>%
    mutate(rsquare = map_dbl(model,
                             'r.squared')) %>% 
    mutate(
        graph=map(data
        ,~((ggplot(data = .x
                 ,aes(cut,clarity,fill=price))+
            geom_tile())
        ))
    ) 
df
