# load_libraries ------------------------------------------
rm(list = ls())
pacman::p_load(
    proto,
    promises,
    future,
    plotly,
    pacman,
    colorfindr,
    gmp,
    tidyquant,
    tsibble,
    infer,
    caret,
    sna,
    pryr,
    speedglm,
    broom,
    TSstudio,
    rqdatatable,
    furrr,
    tidyverse,
    data.table
)


# SEMMA ---------------------------------------------------

data(coleman)

coleman %>%
    summary()


dmd_df <- diamonds %>%
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
                                  , aes(cut, clarity, fill = price)) +
                               geom_tile()
                       )))) %>%
    mutate(discription =
               map(data,
                   ~ (DescTools::Desc(.x$depth, plotit = F))))




# market_basket -------------------------------------------
plan(strategy = multicore)
