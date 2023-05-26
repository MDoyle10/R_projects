setwd("C:/Users/Mario/iCloudDrive/Documents/Ph.d/Data analysis")

library(tidyverse)
library(psych)
library(rstatix)
library(car)

Obtain_data <- function(Experiment_number, File_name, ID){
  file1 <- str_glue("Data/Sorted participant files/Experiment ",Experiment_number,"/",File_name)
  
  data <- read.csv(file1)
  
  A_trials <- data %>%
    filter(Phase %in% "Study") %>%
    filter(ResponseType == "Instructions") %>%
    slice(1:45)
  
  C_trials <- data %>%
    filter(Phase %in% "Study") %>%
    filter(ResponseType == "Instructions") %>%
    slice(46:90)
  
  A_JOL <- mean(A_trials$Judgment,na.rm = TRUE)
  C_JOL <- mean(C_trials$Judgment,na.rm = TRUE)
  A_ST <- mean(A_trials$ReactionTime,na.rm = TRUE)/1000
  C_ST <- mean(C_trials$ReactionTime,na.rm = TRUE)/1000
  
  means <- c(ID,A_JOL,C_JOL,A_ST,C_ST)
}

## Get means for all participants
study_data <- data.frame(matrix(1:10, ncol = 5, dimnames = list(c("1","2"),
                                                                      c("ID","A_JOL","C_JOL","A_ST","C_ST"))))
for (x in 1:32){
  study_data[x,] <- Obtain_data(6, str_glue("F", x, ".csv"), x)
}

## Remove participants
exclude <- c(4,5,10,12,19,22,28)
study_data <- study_data[-which(study_data$ID %in% exclude),]

## Analyze JOLs
study_data_JOL <- study_data %>%
  select(ID:C_JOL) %>%
  pivot_longer(cols = A_JOL:C_JOL, names_to = "Pair_Type") %>%
  convert_as_factor(ID, Pair_Type) %>%
  arrange(Pair_Type)

descriptives_JOL <- study_data_JOL %>%
  pivot_wider(names_from = Pair_Type, values_from = value) %>%
  select(A_JOL,C_JOL) %>%
  describe()

ANOVA_JOL <- aov(value ~ Pair_Type, data = study_data_JOL)
summary(ANOVA_JOL)
partial_eta_squared(ANOVA_JOL)

leveneTest(value ~ Pair_Type, data = study_data_JOL)

ANOVA_JOL_residuals <- residuals(object = ANOVA_JOL)
shapiro.test(x = ANOVA_JOL_residuals)

pwr.anova.test(k = 2, n = 25, f = 0.25)

## Analyze Study Time
study_data_ST <- study_data %>%
  select(ID,A_ST,C_ST) %>%
  pivot_longer(cols = A_ST:C_ST, names_to = "Pair_Type") %>%
  convert_as_factor(ID, Pair_Type) %>%
  filter(is_extreme(value) == "FALSE")

identify_outliers(study_data_ST, value)     #Two outliers

descriptives_ST <- study_data_ST %>%
  pivot_wider(names_from = Pair_Type, values_from = value) %>%
  select(A_ST,C_ST) %>%
  describe()

ANOVA_ST <- aov(value ~ Pair_Type, data = study_data_ST)
summary(ANOVA_ST)
partial_eta_squared(ANOVA_ST)

leveneTest(value ~ Pair_Type, data = study_data_ST)

ANOVA_ST_residuals <- residuals(object = ANOVA_ST)
shapiro.test(x = ANOVA_ST_residuals)

kruskal.test(value ~ Pair_Type, data = study_data_ST)
