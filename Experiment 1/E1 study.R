#setwd("~/Documents/Ph.d/Data analysis")
setwd("C:/Users/Mario/iCloudDrive/Documents/Ph.d/Data analysis")

library(tidyverse)
library(psych)
library(rstatix)
library(car)
library(pwr)

Obtain_data <- function(Experiment_number, File_name, ID){
  file1 <- str_glue("Data/Sorted participant files/Experiment ",Experiment_number,"/",File_name)
  
  data <- read.csv(file1)
  
  C_trials <- data %>%
    filter(Phase %in% "Study") %>%
    filter(ResponseType == "Instructions") %>%
    slice(1:32)
  
  HS_trials <- data %>%
    filter(Phase %in% "Study") %>%
    filter(ResponseType == "Instructions") %>%
    slice(33:68)
  
  LS_trials <- data %>%
    filter(Phase %in% "Study") %>%
    filter(ResponseType == "Instructions") %>%
    slice(69:104)
  
  C_JOL <- mean(C_trials$Judgment,na.rm = TRUE)
  HS_JOL <- mean(HS_trials$Judgment,na.rm = TRUE)
  LS_JOL <- mean(LS_trials$Judgment,na.rm = TRUE)
  C_ST <- mean(C_trials$ReactionTime,na.rm = TRUE)/1000
  HS_ST <- mean(HS_trials$ReactionTime,na.rm = TRUE)/1000
  LS_ST <- mean(LS_trials$ReactionTime,na.rm = TRUE)/1000
  
  means <- c(ID,C_JOL,HS_JOL,LS_JOL,C_ST,HS_ST,LS_ST)
}

## Get means for all participants
study_data <- data.frame(matrix(1:14, ncol = 7, dimnames = list(c("1","2"),
                                                                c("ID","C_JOL","HS_JOL","LS_JOL","C_ST","HS_ST","LS_ST"))))
for (x in 1:32){
  study_data[x,] <- Obtain_data(1, str_glue("A", x, ".csv"), x)
}

## Analyze JOLs
study_data_JOL <- study_data %>%
  select(ID:LS_JOL) %>%
  pivot_longer(cols = C_JOL:LS_JOL, names_to = "Pair_Type") %>%
  convert_as_factor(ID, Pair_Type) %>%
  arrange(Pair_Type)
  
descriptives_JOL <- study_data_JOL %>%
  pivot_wider(names_from = Pair_Type, values_from = value) %>%
  select(C_JOL:LS_JOL) %>%
  describe()

ANOVA_JOL <- aov(value ~ Pair_Type, data = study_data_JOL)
summary(ANOVA_JOL)
partial_eta_squared(ANOVA_JOL)
## Test for homogeneity of variance (p > .05 means assumption IS met)
leveneTest(value ~ Pair_Type, data = study_data_JOL)

## Welch one-way test, does not require homogeneity of variance assumption
#oneway.test(value ~ Pair_Type, data = study_data_JOL)
#pairwise.t.test(study_data_JOL$value,study_data_JOL$Pair_Type,
#                p.adjust.method = "BH",pool.sd = FALSE)

## Test for normality (p < .05 means assumption IS met)
ANOVA_JOL_residuals <- residuals(object = ANOVA_JOL)
shapiro.test(x = ANOVA_JOL_residuals)

## Non-parametric alternative, Kruskal-Wallis rank sum test, if ANOVA
## assumptions are not met.
#kruskal.test(value ~ Pair_Type, data = study_data_JOL)

TukeyHSD(ANOVA_JOL)

pwr.anova.test(k = 3, n = 32, f = 0.1105661)

## Analyze Study Time
study_data_ST <- study_data %>%
  select(ID,C_ST:LS_ST) %>%
  pivot_longer(cols = C_ST:LS_ST, names_to = "Pair_Type") %>%
  convert_as_factor(ID, Pair_Type)

identify_outliers(study_data_ST, value)     #No outliers

descriptives_ST <- study_data_ST %>%
  pivot_wider(names_from = Pair_Type, values_from = value) %>%
  select(C_ST:LS_ST) %>%
  describe()  

ANOVA_ST <- aov(value ~ Pair_Type, data = study_data_ST)
summary(ANOVA_ST)
partial_eta_squared(ANOVA_ST)

leveneTest(value ~ Pair_Type, data = study_data_ST)

ANOVA_ST_residuals <- residuals(object = ANOVA_ST)
shapiro.test(x = ANOVA_ST_residuals)
