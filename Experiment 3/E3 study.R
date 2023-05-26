setwd("C:/Users/Mario/iCloudDrive/Documents/Ph.d/Data analysis")

library(tidyverse)
library(psych)
library(rstatix)

Obtain_data <- function(Experiment_number, File_name, ID){
  file1 <- str_glue("Data/Sorted participant files/Experiment ",Experiment_number,"/",File_name)
  
  data <- read.csv(file1)
  
  P_trials <- data %>%
    filter(Phase %in% "Study") %>%
    slice(1:45)
  
  W_trials <- data %>%
    filter(Phase %in% "Study") %>%
    slice(46:90)
  
  P_JOL <- mean(P_trials$Judgment,na.rm = TRUE)
  W_JOL <- mean(W_trials$Judgment,na.rm = TRUE)
  P_ST <- mean(P_trials$ReactionTime,na.rm = TRUE)/1000
  W_ST <- mean(W_trials$ReactionTime,na.rm = TRUE)/1000
  
  means <- c(ID,P_JOL,W_JOL,P_ST,W_ST)
}

## Get means for all participants
study_data <- data.frame(matrix(1:10, ncol = 5, dimnames = list(c("1","2"),
                                                                c("ID","P_JOL","W_JOL","P_ST","W_ST"))))
for (x in 1:46){
  study_data[x,] <- Obtain_data(7, str_glue("G", x, ".csv"), x)
}

## Remove participants
exclude <- c(4,12,20,22,35,36,37,39)
study_data <- study_data[-which(study_data$ID %in% exclude),]

## Analyze JOLs
study_data_JOL <- study_data %>%
  select(ID:W_JOL) %>%
  pivot_longer(cols = P_JOL:W_JOL, names_to = "Pair_Type") %>%
  convert_as_factor(ID, Pair_Type) %>%
  arrange(Pair_Type)

descriptives_JOL <- study_data_JOL %>%
  pivot_wider(names_from = Pair_Type, values_from = value) %>%
  select(P_JOL,W_JOL) %>%
  describe()

ANOVA_JOL <- aov(value ~ Pair_Type, data = study_data_JOL)
summary(ANOVA_JOL)
partial_eta_squared(ANOVA_JOL)

leveneTest(value ~ Pair_Type, data = study_data_JOL)

ANOVA_JOL_residuals <- residuals(object = ANOVA_JOL)
shapiro.test(x = ANOVA_JOL_residuals)

kruskal.test(value ~ Pair_Type, data = study_data_JOL)

## Analyze Study Time
study_data_ST <- study_data %>%
  select(ID,P_ST,W_ST) %>%
  pivot_longer(cols = P_ST:W_ST, names_to = "Pair_Type") %>%
  convert_as_factor(ID, Pair_Type)

identify_outliers(study_data_ST, value)     #No outliers

descriptives_ST <- study_data_ST %>%
  pivot_wider(names_from = Pair_Type, values_from = value) %>%
  select(P_ST,W_ST) %>%
  describe()

ANOVA_ST <- aov(value ~ Pair_Type, data = study_data_ST)
summary(ANOVA_ST)
partial_eta_squared(ANOVA_ST)

leveneTest(value ~ Pair_Type, data = study_data_ST)

ANOVA_ST_residuals <- residuals(object = ANOVA_ST)
shapiro.test(x = ANOVA_ST_residuals)