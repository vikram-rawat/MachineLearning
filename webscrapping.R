
# Load_libraries ------------------------------------------------

library(rvest)
library(httr)
library(tidyverse)
library(data.table)


# jabong Jdata -----------------------------------------

Jdata <- fread('data/brand.csv', sep = ',')

Jdata[, brand := str_replace_all(string = brand
                                ,
                                pattern = ' '
                                ,
                                replacement = '-')]

Jdata[, Bcount := 0]

for (i in seq_along(Jdata$brand)) {
    Jdata$Bcount[i] <- paste('http://www.jabong.com/find/'
                            , Jdata[i, 1], '?', sep = '') %>%
        read_html() %>%
        html_nodes('span.product-count') %>%
        html_text() %>%
        .[1]
}

Jdata[,Bcount:=str_replace(Bcount
                          ," products"
                          ,'') %>% 
                as.integer()]


    paste('http://www.jabong.com/find/'
      , Jdata[1, 1], sep = '') %>%
    read_html() %>%
    html_nodes('.wishlist') %>% 
    html_children() %>% 
    html_attr(name = 'data-sku') 

    



Jdata

# myntra Jdata ------------------------------------------------


Mdata <- fread('data/brand.csv', sep = ',')

Mdata[, brand := str_replace_all(string = brand
                                ,
                                pattern = ' '
                                ,
                                replacement = '-')]
Mdata[, Bcount := 0]

# Mdata$Bcount[i] <- 
    
    read_html('https://www.myntra.com/jealous-21') %>%
    html_nodes('.horizontal-filters-sub') %>%
    html_text() 
    
    
    read_html('http://www.jabong.com/find/jealous-21') %>%
    html_nodes('span.product-count') %>%
    html_text() %>%
    .[1]
    