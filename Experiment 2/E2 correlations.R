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
  Abstract_pairs <- readr::read_delim("Data/Stimuli/Experiment 5/Abstract word list.txt",
                                      delim = "\t", col_names = c("word1", "word2"),
                                      col_types = "cc", trim_ws = TRUE, skip = 1)
  Concrete_pairs <- readr::read_delim("Data/Stimuli/Experiment 5/Concrete word list.txt",
                                      delim = "\t", col_names = c("word1", "word2"),
                                      col_types = "cc", trim_ws = TRUE, skip = 1)
  
  ## Obtain JOL gamma correlations
  ## Select the JOLs and outcome for intact pairs for each pair type
  Judgment_temp <- 0
  Outcome_temp <- 0
  Judgment2_temp <- 0
  Outcome2_temp <- 0
  
  a <- 1
  for (x in 31:45) {
    location <- str_which(Subset$Trial, str_to_lower(Abstract_pairs$word2[x],))
    location2 <- str_which(Subset$Trial, str_to_lower(Concrete_pairs$word2[x],))
    Judgment_temp[a] <- c(Subset$Judgment[location[1]])
    Outcome_temp[a] <- Subset$Outcome[location[2]]
    Judgment2_temp[a] <- c(Subset$Judgment[location2[1]])
    Outcome2_temp[a] <- Subset$Outcome[location2[2]]
    a <- a + 1
  }
  
  ## Calculate Gamma and other stats for each pair type
  Abstract1 <- table(Judgment_temp, Outcome_temp)
  A_JOL <- GKgamma(Abstract1)[[1]]
  Concrete1 <- table(Judgment2_temp, Outcome2_temp)
  C_JOL <- GKgamma(Concrete1)[[1]]
  
  
  ## Obtain CJ gamma correlations
  ## Abstract trials
  A_intact_trials <- data %>%
    filter(Phase %in% "Test") %>%
    filter(CorrResponse == "Old") %>%
    slice(1:15) %>%
    select(Judgment,Outcome)
  A_I <- table(A_intact_trials$Judgment,A_intact_trials$Outcome)
  A_I_CJ <- GKgamma(A_I)[[1]]
  
  A_rearranged_trials <- data %>%
    filter(Phase %in% "Test") %>%
    filter(CorrResponse == "New") %>%
    slice(1:15) %>%
    select(Judgment,Outcome)
  A_R <- table(A_rearranged_trials$Judgment,A_rearranged_trials$Outcome)
  A_R_CJ <- GKgamma(A_R)[[1]]
  
  ## Concrete trials
  C_intact_trials <- data %>%
    filter(Phase %in% "Test") %>%
    filter(CorrResponse == "Old") %>%
    slice(16:30) %>%
    select(Judgment,Outcome)
  
  C_I <- table(C_intact_trials$Judgment,C_intact_trials$Outcome)
  C_I_CJ <- GKgamma(C_I)[[1]]
  
  C_rearranged_trials <- data %>%
    filter(Phase %in% "Test") %>%
    filter(CorrResponse == "New") %>%
    slice(16:30) %>%
    select(Judgment,Outcome)
  
  C_R <- table(C_rearranged_trials$Judgment,C_rearranged_trials$Outcome)
  C_R_CJ <- GKgamma(C_R)[[1]]
  
  ## report
  gammas <- c(ID,A_JOL,C_JOL,A_I_CJ,A_R_CJ,C_I_CJ,C_R_CJ)
}

data <- data.frame(matrix(1:14, ncol = 7, dimnames = list(c("1","2"),
                                                         c("ID","A_JOL","C_JOL","A_I_CJ","A_R_CJ","C_I_CJ","C_R_CJ"))))

## Work around because of a problem with file F22.csv
for (x in 1:21){
  data[x,] <- Obtain_data(6, str_glue("F", x, ".csv"), x)
}
for (x in 23:32){
  data[x,] <- Obtain_data(6, str_glue("F", x, ".csv"), x)
}
data$ID[22] <- 22

## Remove participants
exclude <- c(4,5,10,12,19,22,28)
data <- data[-which(data$ID %in% exclude),]

## JOLs
descriptives_JOL <- data %>%
  select(ID,A_JOL,C_JOL) %>%
  describe()

## CJs
descriptives_CJs <- data %>%
  select(ID,A_I_CJ,A_R_CJ,C_I_CJ,C_R_CJ) %>%
  describe()
