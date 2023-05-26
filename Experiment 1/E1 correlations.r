setwd("~/Documents/Ph.d/Data analysis")

library(tidyverse)
library(psych)
library(rstatix)
library(vcdExtra)       #For gamma statistic

Obtain_data <- function(Experiment_number, File_name, ID){
  participant_file <- str_glue("Data/Sorted participant files/Experiment ",Experiment_number,"/",File_name)
  
  data <- read.csv(participant_file)
  Subset <- select(data,Trial,Judgment,Outcome)
  
  ## Stimulus lists (Need these in a folder called Stimuli in "Data")
  #Compound_pairs <- readr::read_delim("Data/Stimuli/Experiment 1/Compound word pairs.txt",
  #                                    delim = " ",col_names = c("word1","word2"),
  #                                    col_types = "cc", trim_ws = TRUE)
  #High_pairs <- readr::read_delim("Data/Stimuli/Experiment 1/High strength word pairs.txt",
  #                                delim = " ",col_names = c("word1","word2"),
  #                                col_types = "cc", trim_ws = TRUE)
  #Low_pairs <- readr::read_delim("Data/Stimuli/Experiment 1/Low strength word pairs.txt",
  #                               delim = " ",col_names = c("word1","word2"),
  #                               col_types = "cc", trim_ws = TRUE)
  Compound_pairs <- read.table("Data/Stimuli/Experiment 1/Compound word pairs.txt", col.names = c("word1","word2"))
  High_pairs <- read.table("Data/Stimuli/Experiment 1/High strength word pairs.txt", col.names = c("word1","word2"))
  Low_pairs <- read.table("Data/Stimuli/Experiment 1/Low strength word pairs.txt", col.names = c("word1","word2"))
  
  ## Obtain JOL gamma correlations
  ## Select the JOLs and outcome for intact pairs for each pair type
  Judgment_temp <- 0
  Outcome_temp <- 0
  Judgment2_temp <- 0
  Outcome2_temp <- 0
  Judgment3_temp <- 0
  Outcome3_temp <- 0
  a <- 1
  for (x in 25:36) {
    location <- str_which(Subset$Trial, Compound_pairs$word2[x])
    location2 <- str_which(Subset$Trial, High_pairs$word2[x])
    location3 <- str_which(Subset$Trial, Low_pairs$word2[x])
    Judgment_temp[a] <- c(Subset$Judgment[location[1]])
    Outcome_temp[a] <- Subset$Outcome[location[2]]
    Judgment2_temp[a] <- c(Subset$Judgment[location2[1]])
    Outcome2_temp[a] <- Subset$Outcome[location2[2]]
    Judgment3_temp[a] <- c(Subset$Judgment[location3[1]])
    Outcome3_temp[a] <- Subset$Outcome[location3[2]]
    a <- a + 1
  }
  
  ## Calculate Gamma and other stats for each pair type
  Comp <- table(Outcome_temp, Judgment_temp)
  C_JOL <- GKgamma(Comp)[[1]]
  High <- table(Outcome2_temp, Judgment2_temp)
  HS_JOL <- GKgamma(High)[[1]]
  Low <- table(Outcome3_temp, Judgment3_temp)
  LS_JOL <- GKgamma(Low)[[1]]
  
  ## Obtain CJ gamma correlations
  ## Placeholders for Intact pair variables
  I_C_CJ <- 0
  I_C_Out <- 0
  I_HS_CJ <- 0
  I_HS_Out <- 0
  I_LS_CJ <- 0
  I_LS_Out <- 0
  
  ## Find CJ and their outcome for each Intact pair type
  a <- 1
  for (x in 25:36) {
    location <- str_which(Subset$Trial, Compound_pairs$word2[x])
    location2 <- str_which(Subset$Trial, High_pairs$word2[x])
    location3 <- str_which(Subset$Trial, Low_pairs$word2[x])
    I_C_CJ[a] <- c(Subset$Judgment[location[2]])
    I_C_Out[a] <- Subset$Outcome[location[2]]
    I_HS_CJ[a] <- c(Subset$Judgment[location2[2]])
    I_HS_Out[a] <- Subset$Outcome[location2[2]]
    I_LS_CJ[a] <- c(Subset$Judgment[location3[2]])
    I_LS_Out[a] <- Subset$Outcome[location3[2]]
    a <- a + 1
  }
  
  ## Placeholders for rearranged pair variables
  R_C_CJ <- 0
  R_C_Out <- 0
  R_HS_CJ <- 0
  R_HS_Out <- 0
  R_LS_CJ <- 0
  R_LS_Out <- 0
  
  ## Find CJ and their outcome for each Rearranged pair type
  b <- 1
  for (y in seq(2, 24, by = 2)) {
    location <- str_which(Subset$Trial, Compound_pairs$word2[y])
    location2 <- str_which(Subset$Trial, High_pairs$word2[y])
    location3 <- str_which(Subset$Trial, Low_pairs$word2[y])
    R_C_CJ[b] <- c(Subset$Judgment[location[2]])
    R_C_Out[b] <- Subset$Outcome[location[2]]
    R_HS_CJ[b] <- c(Subset$Judgment[location2[2]])
    R_HS_Out[b] <- Subset$Outcome[location2[2]]
    R_LS_CJ[b] <- c(Subset$Judgment[location3[2]])
    R_LS_Out[b] <- Subset$Outcome[location3[2]]
    b <- b + 1
  }
  
  ## Calculate Gamma and other stats for each Intact pair type
  C_I <- table(I_C_CJ, I_C_Out)
  C_I_CJ <- GKgamma(C_I)[[1]]
  HS_I <- table(I_HS_CJ, I_HS_Out)
  HS_I_CJ <- GKgamma(HS_I)[[1]]
  LS_I <- table(I_LS_CJ, I_LS_Out)
  LS_I_CJ <- GKgamma(LS_I)[[1]]
  
  ## Calculate Gamma and other stats for each Rearranged pair type
  C_R <- table(R_C_CJ, R_C_Out)
  C_R_CJ <- GKgamma(C_R)[[1]]
  HS_R <- table(R_HS_CJ, R_HS_Out)
  HS_R_CJ <- GKgamma(HS_R)[[1]]
  LS_R <- table(R_LS_CJ, R_LS_Out)
  LS_R_CJ <- GKgamma(LS_R)[[1]]
  
  ## Variable containing all gamma correlations as a vector
  Gamma = c(ID,C_JOL,HS_JOL,LS_JOL,C_I_CJ,C_R_CJ,HS_I_CJ,HS_R_CJ,LS_I_CJ,LS_R_CJ)
}

data <- data.frame(matrix(1:20, ncol = 10, dimnames = list(c("1","2"),
                                                          c("ID","C_JOL","HS_JOL","LS_JOL","C_I_CJ","C_R_CJ","HS_I_CJ","HS_R_CJ","LS_I_CJ","LS_R_CJ"))))

for (x in 1:32){
  data[x,] <- Obtain_data(1, str_glue("A", x, ".csv"), x)
}

## JOLs
descriptives_JOL <- data %>%
  select(ID,C_JOL:LS_JOL) %>%
  describe()

## CJs
descriptives_CJs <- data %>%
  select(ID,C_I_CJ:LS_R_CJ) %>%
  describe()
