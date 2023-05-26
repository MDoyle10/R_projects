setwd("C:/Users/Mario/iCloudDrive/Documents/Ph.d/Data analysis")

library(tidyverse)
library(psych)
library(rstatix)
library(vcdExtra)       #For gamma statistic

Obtain_data <- function(Experiment_number, File_name, ID){
  participant_file <- str_glue("Data/Sorted participant files/Experiment ",Experiment_number,"/",File_name)
  data <- read.csv(participant_file)
  
  Subset <- select(data,Trial,Judgment,Outcome)
  
  ## Stimulus lists (Need these in a folder called Stimuli in "Data")
  List1 <- readr::read_delim("Data/Stimuli/Experiment 7/List1_words.txt",
                                      delim = "\t", col_names = "words",
                                      col_types = "c", trim_ws = TRUE)
  List2 <- readr::read_delim("Data/Stimuli/Experiment 7/List2_words.txt",
                                      delim = "\t", col_names = "words",
                                      col_types = "c", trim_ws = TRUE)
  
  ## Obtain JOL gamma correlations
  ## Select the JOLs and outcome for intact pairs for each pair type
  Judgment_temp <- 0
  Outcome_temp <- 0
  Judgment2_temp <- 0
  Outcome2_temp <- 0
  
  a <- 1
  for (x in 16:30) {
    location <- str_which(Subset$Trial, List1$words[x])
    location2 <- str_which(Subset$Trial, List2$words[x])
    Judgment_temp[a] <- c(Subset$Judgment[location[1]])
    Outcome_temp[a] <- Subset$Outcome[location[2]]
    Judgment2_temp[a] <- c(Subset$Judgment[location2[1]])
    Outcome2_temp[a] <- Subset$Outcome[location2[2]]
    a <- a + 1
  }
  
  ## Calculate Gamma and other stats for each pair type
  if (location[1]>45){
    Picture1 <- table(Judgment_temp, Outcome_temp)
    P_JOL <- GKgamma(Picture1)[[1]]
    Word1 <- table(Judgment2_temp, Outcome2_temp)
    W_JOL <- GKgamma(Word1)[[1]]
  } else {
    Picture1 <- table(Judgment2_temp, Outcome2_temp)
    P_JOL <- GKgamma(Picture1)[[1]]
    Word1 <- table(Judgment_temp, Outcome_temp)
    W_JOL <- GKgamma(Word1)[[1]]
  }
  
  ## Obtain CJ gamma correlations
  ## Picture trials
  P_intact_trials <- data %>%
    filter(Phase %in% "Test") %>%
    filter(CorrResponse == "Old") %>%
    slice(1:15) %>%
    select(Judgment,Outcome)
  P_I <- table(P_intact_trials$Judgment,P_intact_trials$Outcome)
  P_I_CJ <- GKgamma(P_I)[[1]]
  
  P_rearranged_trials <- data %>%
    filter(Phase %in% "Test") %>%
    filter(CorrResponse == "New") %>%
    slice(1:15) %>%
    select(Judgment,Outcome)
  P_R <- table(P_rearranged_trials$Judgment,P_rearranged_trials$Outcome)
  P_R_CJ <- GKgamma(P_R)[[1]]
  
  ## Word trials
  W_intact_trials <- data %>%
    filter(Phase %in% "Test") %>%
    filter(CorrResponse == "Old") %>%
    slice(16:30) %>%
    select(Judgment,Outcome)
  W_I <- table(W_intact_trials$Judgment,W_intact_trials$Outcome)
  W_I_CJ <- GKgamma(W_I)[[1]]
  
  W_rearranged_trials <- data %>%
    filter(Phase %in% "Test") %>%
    filter(CorrResponse == "New") %>%
    slice(16:30) %>%
    select(Judgment,Outcome)
  W_R <- table(W_rearranged_trials$Judgment,W_rearranged_trials$Outcome)
  W_R_CJ <- GKgamma(W_R)[[1]]
  
  ## report
  gammas <- c(ID,P_JOL,W_JOL,P_I_CJ,P_R_CJ,W_I_CJ,W_R_CJ)
}

data <- data.frame(matrix(1:14, ncol = 7, dimnames = list(c("1","2"),
                                                          c("ID","P_JOL","W_JOL","P_I_CJ","P_R_CJ","W_I_CJ","W_R_CJ"))))

## Work around because of a problem with file G35.csv and G39.csv
for (x in 1:34){
  data[x,] <- Obtain_data(7, str_glue("G", x, ".csv"), x)
}
for (x in 36:38){
  data[x,] <- Obtain_data(7, str_glue("G", x, ".csv"), x)
}
for (x in 40:46){
  data[x,] <- Obtain_data(7, str_glue("G", x, ".csv"), x)
}
data$ID[35] <- 35
data$ID[39] <- 39

## Remove participants
exclude <- c(4,12,20,22,35,36,37,39)
data <- data[-which(data$ID %in% exclude),]

## JOLs
descriptives_JOL <- data %>%
  select(ID,P_JOL,W_JOL) %>%
  describe()

## CJs
descriptives_CJs <- data %>%
  select(ID,P_I_CJ,P_R_CJ,W_I_CJ,W_R_CJ) %>%
  describe()
