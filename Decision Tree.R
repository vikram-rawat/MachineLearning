
# Load_libraries ---------------------------------------------------
library(readxl)
library(rpart)
library(partykit)
library(data.table)
library(tidyverse)
library(broomstick)
library(rattle)
library(FFTrees)

# First ------------------------------------------------------------

btree <-
    read_excel("data/DT-02-02-Credit-1.xls", 1)

btree<-btree %>% 
    mutate(RESPONSE=as.factor(ifelse(RESPONSE==1,"Yes","No")))


btree %>% glimpse()

plot(btree$RESPONSE)

model<-rpart(formula = RESPONSE~.
      ,data = btree
      ,control = rpart.control(minsplit = 60
                               ,minbucket = 30
                               ,maxdepth = 4)) 

fancyRpartPlot(model)


# second -----------------------------------------------------------

btree2<-read_excel('data/DT02-06-Wine-Quality-Data.xlsx',2
                   ,trim_ws = TRUE)

btree2<-btree2 %>% 
    mutate(quality=as.factor(ifelse(quality>6,"yes","no")))

glimpse(btree2)

plot(btree2$quality)

model2<-rpart(formula = quality~.
             ,data = btree2
             ,control = rpart.control(minsplit = 60
                                      ,minbucket = 30
                                      ,maxdepth = 4)) 


fancyRpartPlot(model2)
tidy(model2)

# third -------------------------------------------------------------------

blood<-read_excel("data/Blood-Donator-Data-Questions.xlsx",2)

blood<-blood %>% 
    mutate(Donated_in_Mar07=ifelse(Donated_in_Mar07==1,"Yes","No"))

model_blood<-rpart(formula = Donated_in_Mar07~.
                   ,data = blood
                   ,control = rpart.control(minsplit = 60
                                            ,minbucket = 30
                                            ,maxdepth = 4))

as.party(model_blood) %>% plot()


fancyRpartPlot(model_blood)

which.min(model_blood$cptable[,"xerror"]) %>%
    model_blood$cptable[.,"CP"] %>% 
    prune(tree = model_blood,cp=.) %>% 
    fancyRpartPlot()

# CART -----------------------------------------------------------------

cart<-read_excel('data/CART-Example.xlsx',1)

model_cart<-rpart(formula = Weight~Height
      ,data = cart
      ,method = "anova"
      ,control = rpart.control(
          minsplit = 6
          ,minbucket = 3
          ,maxdepth = 2
      ))

as.party(model_cart) %>% plot()
fancyRpartPlot(model_cart)

# FFTress ---------------------------------------------------------------
heart.fft <- FFTrees(formula = diagnosis ~., 
                     data = heart.train,
                     data.test = heart.test)

# Plot the best tree applied to the test data
inwords(heart.fft)
summary(heart.fft)

plot(heart.fft,
     data = "test",
     main = "Heart Disease", 
     decision.labels = c("Healthy", "Disease"))

plot(heart.fft
     ,what='cues'
     ,data="train")


