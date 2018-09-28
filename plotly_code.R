# Load Library ------------------------------------------------------------

rm(list = ls())
library(ggplot2)
library(plotly)
library(GGally)
library(trelliscopejs)
library(tidyverse)
library(data.table)
library(tidyquant)
library(speedglm)
library(htmltools)
library(crosstalk)

# Data Munging ------------------------------------------------------------


mtcars$carb <- as.numeric(mtcars$hp)

# Learn Basic Plot --------------------------------------------------------

mtcars %>%
    SharedData$new() %>% 
    plot_ly(x =  ~ mpg,
            y =  ~ disp,
            color =  ~ as.factor(cyl)) %>%
    add_markers(
        size =  ~ hp,
        alpha = .5,
        sizes = c(50, 2000),
        colors = c('black', 'darkgrey', 'maroon'),
        hoverinfo = "none"
    ) %>%
    config(displaylogo = FALSE ,
           displayModeBar = FALSE) %>%
    layout(
        xaxis = list(color = 'darkgreen', fixedrange = TRUE),
        yaxis = list(color = 'darkblue', fixedrange = TRUE),
        plot_bgcolor = '#EFEFFF',
        paper_bgcolor = '#FEFFF9'
    ) %>%
    hide_colorbar() %>%
    hide_legend() %>%
    hide_guides() %>% 
    highlight(off = "plotly_doubleclick",
              color = 'red',
              opacityDim = .4)



# With Purrr ------------------------------------------------------

mtcars %>%
    split(.$cyl) %>%
    map(function(df) {
        plot_ly(
            data = df,
            size = I(30),
            alpha = .3,
            x =  ~ mpg,
            y =  ~ disp
        ) %>%
            add_markers(showscale = FALSE) %>%
            hide_legend()
    }) %>%
    subplot(shareY = TRUE)

# lines -------------------------------------------------------------------
txhousing %>% setDT()
top5 <-
    txhousing[, mean(sales, na.rm = T), city][order(-V1), ][1:5,]


txhousing %>% setkey(city)
top5 %>% setkey(city)

txhousing %>%
    plot_ly() %>%
    add_lines(
        x =  ~ date,
        y =  ~ median,
        alpha = .2,
        color =  ~ city,
        colors = 'black'
    ) %>%
    add_lines(
        data = (top5[txhousing][!is.na(V1),]),
        x = ~ date,
        y =  ~ median,
        color =  ~ city,
        colors = I('red'),
        size = I(2)
    ) %>%
    hide_legend() %>%
    config(displayModeBar = F)

# Density -----------------------------------------------------------------

density <- txhousing$median %>%
    density(na.rm = TRUE)

plot_ly() %>%
    add_lines(x = density$x, y = density$y)


# CandleStick -------------------------------------------------------------


google <- tq_get("GOOG")

google %>%
    plot_ly(
        x =  ~ date,
        xend =  ~ date,
        color = ~ close > open,
        colors = c("red", "black"),
        hoverinfo = "none"
    ) %>%
    add_segments(y =  ~ low,
                 yend =  ~ high,
                 size = I(1)) %>%
    add_segments(y =  ~ open,
                 yend =  ~ close,
                 size = I(3)) %>%
    rangeslider()

# Linear Model ------------------------------------------------------------

model <- lm(mpg ~ wt, data = mtcars)
broom::augment(model) %>%
    plot_ly(x =  ~ wt, showlegend = FALSE) %>%
    add_markers(
        y =  ~ mpg,
        color = I("blue"),
        size = I(30),
        alpha = .4
    ) %>%
    add_lines(y =  ~ .fitted, color = I("red")) %>%
    add_ribbons(
        ymin = ~ .fitted - 1.96 * .se.fit,
        ymax = ~ .fitted + 1.96 * .se.fit,
        color = I('grey')
    ) %>%
    config(displayModeBar = FALSE)

# BarGraph ----------------------------------------------------------------
diamonds %>%
    plot_ly(x =  ~ price) %>%
    add_histogram(color = I("lightblue"))

diamonds %>%
    plot_ly(x =  ~ cut) %>%
    add_histogram(color = I("lightblue"))

diamonds %>%
    setDT() %>%
    .[, .N, cut] %>%
    plot_ly(x =  ~ cut, y =  ~ N) %>%
    add_bars(color = I("lightblue"))

plot_ly(diamonds, x = ~ cut, color = ~ clarity) %>%
    add_histogram()

diamonds %>%
    SharedData$new() %>%
    plot_ly(x = ~ cut, color = ~ clarity) %>%
    add_histogram()



diamonds[1:1000, ] %>%
    setDT() %>%
    .[, .N, cut] %>%
    SharedData$new() %>%
    plot_ly(x =  ~ cut, y =  ~ N) %>%
    add_bars() %>%
    layout(barmode = "overlay") %>%
    highlight(off = "plotly_doubleclick",
              color = 'red',
              opacityDim = .4)



# BoxPlots ----------------------------------------------------------------

diamonds %>%
    plot_ly(y =  ~ price,
            color = I('lightblue'),
            alpha = .4) %>%
    add_boxplot(x =  ~ cut)

diamonds %>%
    plot_ly(y =  ~ price, x =  ~ interaction(clarity, cut)) %>%
    add_boxplot(color =  ~ clarity)


# SubPlot ----------------------------------------------------------------

economics %>%
    setDT() %>%
    melt('date') %>%
    split(.$variable) %>%
    map(function(df) {
        df %>%
            plot_ly(x =  ~ date, y =  ~ value) %>%
            add_lines(name =  ~ variable) %>%
            hide_legend() %>%
            layout(title =  ~ variable)
    }) %>%
    subplot(nrows = 5)


# trelliscope -------------------------------------------------------------


qplot(cty, hwy, data = mpg) +
    xlim(7, 37) + ylim(9, 47) + theme_bw() +
    facet_trelliscope(
        ~ manufacturer + class,
        nrow = 2,
        ncol = 4,
        as_plotly = TRUE,
        plotly_args = list(dynamicTicks = T)
    )

# crosstalk ---------------------------------------------------------------
sd <- SharedData$new(txhousing,  ~ year)

(
    ggplot(sd, aes(month, median)) +
        geom_line(aes(group = year)) +
        geom_smooth(data = txhousing, method = "gam") +
        facet_wrap( ~ city)
) %>%
    ggplotly(p, tooltip = "year") %>%
    highlight(defaultValues = 2015, color = "red")

d <- SharedData$new(iris)
(GGally::ggpairs(d, aes(color = Species), columns = 1:4)) %>%
    highlight(ggplotly(p), on = "plotly_selected")


(ggplot(sd, aes(date, median)) + geom_line()) %>%
    ggplotly(tooltip = "city") %>%
    highlight(dynamic = T, persistent = T)


sd <- SharedData$new(txhousing, ~ city, group = "Choose a city")
plot_ly(sd, x = ~ date, y = ~ median) %>%
    group_by(city) %>%
    add_lines(text = ~ city, hoverinfo = "text") %>%
    highlight(on = "plotly_click",
              persistent = TRUE,
              selectize = TRUE)


p1 <-   plot_ly(sd, color = I("black")) %>%
    group_by(city) %>%
    summarise(has = sum(is.na(median))) %>%
    filter(has > 0) %>%
    arrange(has) %>%
    add_bars(x = ~ has,
             y = ~ factor(city, levels = city) ,  hoverinfo = "none") %>% layout(
                 barmode = "overlay",
                 xaxis = list(title = "Number of months missing"),
                 yaxis = list(title = "")
             )



p2 <- base %>%
    add_lines(x = ~ date,
              y = ~ median,
              alpha = 0.3) %>%
    layout(xaxis = list(title = ""))

subplot(p1, p2, titleX = TRUE, widths = c(0.3, 0.7)) %>%
    layout(margin = list(l = 120)) %>%
    highlight(color = "red")


mtcars %>%
    SharedData$new() %>%
    plot_ly(x =  ~ mpg, y =  ~ disp) %>%
    add_markers() %>%
    subplot(mtcars %>%
                SharedData$new() %>%
                plot_ly(y =  ~ disp) %>%
                add_boxplot()) %>%
    highlight("plotly_selected") %>%
    hide_legend()


a <- SharedData$new(mpg)
p <- ggplot(a, aes(displ, hwy, colour = class)) +
    geom_point() +
    geom_smooth(se = FALSE, method = "lm")
ggplotly(p) %>% highlight("plotly_click")

# Animation ---------------------------------------------------------------

data(gapminder, package = "gapminder")

gapminder %>%
    plot_ly(
        x =  ~ gdpPercap,
        y =  ~ lifeExp,
        size =  ~ pop,
        text =  ~ country,
        color =  ~ continent
    ) %>%
    layout(xaxis = list(type = 'log')) %>%
    add_markers(frame =  ~ year #,ids=~country
                ,
                sizes = c(200, 2000),
                alpha = .4) %>%
    animation_opts(2000, easing = 'elastic', redraw = FALSE)

