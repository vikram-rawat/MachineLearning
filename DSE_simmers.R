library(simmer)
library(simmer.plot)
library(simmer.bricks)
library(simmer.optim)
library(simmer.json)
library(simmer.mon)


set.seed(421)

env <- simmer("SuperDuperSim")
env

patient <- trajectory("patients' path") %>%
    ## add an intake activity 
    seize("nurse", 1) %>%
    timeout(function() rnorm(1, 15)) %>%
    release("nurse", 1) %>%
    ## add a consultation activity
    seize("doctor", 1) %>%
    timeout(function() rnorm(1, 20)) %>%
    release("doctor", 1) %>%
    ## add a planning activity
    seize("administration", 1) %>%
    timeout(function() rnorm(1, 5)) %>%
    release("administration", 1)

env %>%
    add_resource("nurse", 1) %>%
    add_resource("doctor", 2) %>%
    add_resource("administration", 1) %>%
    add_generator("patient", patient, function() rnorm(1, 10, 2))


env %>% 
    run(80) %>% 
    now()

env %>%
    stepn() %>% # 1 step
    print() %>%
    stepn(3) 

env %>% peek(Inf, verbose=TRUE)

env %>% 
    run(120) %>%
    now()


env %>% 
    reset() %>% 
    run(80) %>%
    now()


envs <- lapply(1:100, function(i) {
    simmer("SuperDuperSim") %>%
        add_resource("nurse", 1) %>%
        add_resource("doctor", 2) %>%
        add_resource("administration", 1) %>%
        add_generator("patient", patient, function() rnorm(1, 10, 2)) %>%
        run(80)
})

