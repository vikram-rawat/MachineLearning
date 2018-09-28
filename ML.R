# load_libraries ------------------------------------------

library(plotly)
library(caret)
library(sna)
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

babynames::babynames %>% 
    lm(data = .,name~year+sex+n+prop) %>% 
    microbenchmark::microbenchmark(times = 100)

babynames::babynames %>% 
    lm(data = .,name~year+sex+n+prop) %>% 
    microbenchmark::microbenchmark(times = 100)



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
