# load_libraries ------------------------------------------
rm(list = ls())
pacman::p_load(plotly,
               pacman,
            colorfindr,               
            gmp,
            infer,
            caret,
            sna,
            pryr,
            speedglm,
            tidyquant,
            tsibble,
            tidyverse,
            data.table,
            broom,
            TSstudio,
            rqdatatable,
            proto)

# SEMMA ---------------------------------------------------

data(coleman)

coleman %>%
    summary()


df <- diamonds %>%
    group_by(color) %>%
    nest() %>%
    mutate(model = map(data,
                       ~ glance(speedlm(
                           carat ~ cut + depth, data = .x
                       )))) %>%
    mutate(rsquare = map_dbl(model,
                             'r.squared')) %>%
    mutate(graph = map(data
                       ,  ~ ((
                           ggplot(data = .x
                                  , aes(cut, clarity, fill = price))+
                               geom_tile()
                       )))) %>%
    mutate(discription =
               map(data,
                   ~ (DescTools::Desc(.x$depth, plotit = F))))

df

