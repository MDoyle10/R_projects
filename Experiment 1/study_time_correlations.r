library(vcdExtra)
library(tidyverse)
library(psych)

obtain_data <- function(file_name, ID){
  data <- read.csv(file_name)
  data <- read.csv("Data/A15.csv")
  data <- data %>%
    mutate(data, ReactionTime = ReactionTime/1000) %>%
    mutate(ReactionTimeInterval = 
             case_when(ReactionTime >= 6 ~ 13, 
                       ReactionTime >= 5.5 ~ 12,
                       ReactionTime >= 5 ~ 11,
                       ReactionTime >= 4.5 ~ 10,
                       ReactionTime >= 4 ~ 9,
                       ReactionTime >= 3.5 ~ 8,
                       ReactionTime >= 3 ~ 7,
                       ReactionTime >= 2.5 ~ 6,
                       ReactionTime >= 2 ~ 5,
                       ReactionTime >= 1.5 ~ 4,
                       ReactionTime >= 1 ~ 3,
                       ReactionTime >= .5 ~ 2,
                       ReactionTime >= 0 ~ 1,)
    )
  ## Stimulus lists (Need these in a folder called Stimuli in "Data")
  comp <- read.table("Data/Stimuli/Compound word pairs.txt")
  hs <- read.table("Data/Stimuli/High strength word pairs.txt")
  ls <- read.table("Data/Stimuli/Low strength word pairs.txt")
  
  ## Obtain study time gamma correlations
  ## Select the study time intervals and outcome for intact pairs for each pair type
  a1 <- 0
  a2 <- 0
  a3 <- 0
  b1 <- 0
  b2 <- 0
  b3 <- 0
  c <- 1
  
  for (i in 25:36) {
    x <- str_which(data$Trial, comp$V2[i])
    x2 <- str_which(data$Trial, hs$V2[i])
    x3 <- str_which(data$Trial, ls$V2[i])
    a1[c] <- c(data$ReactionTimeInterval[x[1]])
    a2[c] <- c(data$ReactionTimeInterval[x2[1]])
    a3[c] <- c(data$ReactionTimeInterval[x3[1]])
    b1[c] <- data$Outcome[x[2]]
    b2[c] <- data$Outcome[x2[2]]
    b3[c] <- data$Outcome[x3[2]]
    c <- c + 1
  }
  
  comp_g <- GKgamma(table(b1, a1))[[1]]
  hs_g <- GKgamma(table(b2, a2))[[1]]
  ls_g <- GKgamma(table(b3, a3))[[1]]
  
  comp_i <- data %>%
    filter(Phase %in% "Test") %>%
    filter(CorrResponse == "Old") %>%
    slice(1:12) %>%
    select(ReactionTimeInterval,Outcome)
  comp_i_g <- GKgamma(table(comp_i$ReactionTimeInterval,comp_i$Outcome))[[1]]
  
  hs_i <- data %>%
    filter(Phase %in% "Test") %>%
    filter(CorrResponse == "Old") %>%
    slice(13:24) %>%
    select(ReactionTimeInterval,Outcome)
  hs_i_g <- GKgamma(table(hs_i$ReactionTimeInterval,hs_i$Outcome))[[1]]
  
  ls_i <- data %>%
    filter(Phase %in% "Test") %>%
    filter(CorrResponse == "Old") %>%
    slice(25:36) %>%
    select(ReactionTimeInterval,Outcome)
  ls_i_g <- GKgamma(table(ls_i$ReactionTimeInterval,ls_i$Outcome))[[1]]
  
  comp_r <- data %>%
    filter(Phase %in% "Test") %>%
    filter(CorrResponse == "New") %>%
    slice(1:12) %>%
    select(ReactionTimeInterval,Outcome)
  comp_r_g <- GKgamma(table(comp_r$ReactionTimeInterval,comp_r$Outcome))[[1]]
  
  hs_r <- data %>%
    filter(Phase %in% "Test") %>%
    filter(CorrResponse == "New") %>%
    slice(13:24) %>%
    select(ReactionTimeInterval,Outcome)
  hs_r_g <- GKgamma(table(hs_r$ReactionTimeInterval,hs_r$Outcome))[[1]]
  
  ls_r <- data %>%
    filter(Phase %in% "Test") %>%
    filter(CorrResponse == "New") %>%
    slice(25:36) %>%
    select(ReactionTimeInterval,Outcome)
  ls_r_g <- GKgamma(table(ls_r$ReactionTimeInterval,ls_r$Outcome))[[1]]
  gammas <- c(ID, comp_g, hs_g, ls_g, comp_i_g, hs_i_g, ls_i_g, comp_r_g, hs_r_g, ls_r_g)
}

data <- data.frame(matrix(1:20, ncol = 10, dimnames = list(c("1","2"),
                                                           c("ID", "comp_g", "hs_g", "ls_g", "comp_i_g", "hs_i_g", "ls_i_g", "comp_r_g", "hs_r_g", "ls_r_g"))))

for (x in 1:32){
  data[x,] <- obtain_data(str_glue("Data/A", x, ".csv"), x)
}

## Descriptive stats for Study time correlation to performance
descriptives_study_time <- data %>%
  select(ID,comp_g:ls_g) %>%
  describe()

## Descriptive stats for response latency correlation to performance
descriptives_response_latency <- data %>%
  select(ID,comp_i_g:ls_r_g) %>%
  describe()

