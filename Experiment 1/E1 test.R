setwd("C:/Users/Mario/iCloudDrive/Documents/Ph.d/Data analysis")

library(tidyverse)
library(psych)
library(rstatix)
library(car)

Obtain_data <- function(Experiment_number, File_name, ID){
  file1 <- str_glue("Data/Sorted participant files/Experiment ",Experiment_number,"/",File_name)
  
  data <- read.csv(file1)
  
  C_trials <- data %>%
    filter(Phase %in% "Test") %>%
    filter(ResponseType == "Old" | ResponseType == "New") %>%
    slice(1:24)
  
  C_HR_trials <- C_trials %>%
    filter(ResponseType == "Old" & CorrResponse == "Old")
  C_M_trials <- C_trials %>%
    filter(ResponseType == "New" & CorrResponse == "Old")
  C_CR_trials <- C_trials %>%
    filter(ResponseType == "New" & CorrResponse == "New")
  C_FAR_trials <- C_trials %>%
    filter(ResponseType == "Old" & CorrResponse == "New")
  
  HS_trials <- data %>%
    filter(Phase %in% "Test") %>%
    filter(ResponseType == "Old" | ResponseType == "New") %>%
    slice(25:48)
  
  HS_HR_trials <- HS_trials %>%
    filter(ResponseType == "Old" & CorrResponse == "Old")
  HS_M_trials <- HS_trials %>%
    filter(ResponseType == "New" & CorrResponse == "Old")
  HS_CR_trials <- HS_trials %>%
    filter(ResponseType == "New" & CorrResponse == "New")
  HS_FAR_trials <- HS_trials %>%
    filter(ResponseType == "Old" & CorrResponse == "New")
  
  LS_trials <- data %>%
    filter(Phase %in% "Test") %>%
    filter(ResponseType == "Old" | ResponseType == "New") %>%
    slice(49:72)
  
  LS_HR_trials <- LS_trials %>%
    filter(ResponseType == "Old" & CorrResponse == "Old")
  LS_M_trials <- LS_trials %>%
    filter(ResponseType == "New" & CorrResponse == "Old")
  LS_CR_trials <- LS_trials %>%
    filter(ResponseType == "New" & CorrResponse == "New")
  LS_FAR_trials <- LS_trials %>%
    filter(ResponseType == "Old" & CorrResponse == "New")
  
  
  C_HR_CJ <- mean(C_HR_trials$Judgment, na.rm = TRUE)
  C_CR_CJ <- mean(C_CR_trials$Judgment, na.rm = TRUE)
  C_M_CJ <- mean(C_M_trials$Judgment, na.rm = TRUE)
  C_FAR_CJ <- mean(C_FAR_trials$Judgment, na.rm = TRUE)
  HS_HR_CJ <- mean(HS_HR_trials$Judgment, na.rm = TRUE)
  HS_CR_CJ <- mean(HS_CR_trials$Judgment, na.rm = TRUE)
  HS_M_CJ <- mean(HS_M_trials$Judgment, na.rm = TRUE)
  HS_FAR_CJ <- mean(HS_FAR_trials$Judgment, na.rm = TRUE)
  LS_HR_CJ <- mean(LS_HR_trials$Judgment, na.rm = TRUE)
  LS_CR_CJ <- mean(LS_CR_trials$Judgment, na.rm = TRUE)
  LS_M_CJ <- mean(LS_M_trials$Judgment, na.rm = TRUE)
  LS_FAR_CJ <- mean(LS_FAR_trials$Judgment, na.rm = TRUE)
  
  C_HR_RT <- mean(C_HR_trials$ReactionTime, na.rm = TRUE)/1000
  C_CR_RT <- mean(C_CR_trials$ReactionTime, na.rm = TRUE)/1000
  C_M_RT <- mean(C_M_trials$ReactionTime, na.rm = TRUE)/1000
  C_FAR_RT <- mean(C_FAR_trials$ReactionTime, na.rm = TRUE)/1000
  HS_HR_RT <- mean(HS_HR_trials$ReactionTime, na.rm = TRUE)/1000
  HS_CR_RT <- mean(HS_CR_trials$ReactionTime, na.rm = TRUE)/1000
  HS_M_RT <- mean(HS_M_trials$ReactionTime, na.rm = TRUE)/1000
  HS_FAR_RT <- mean(HS_FAR_trials$ReactionTime, na.rm = TRUE)/1000
  LS_HR_RT <- mean(LS_HR_trials$ReactionTime, na.rm = TRUE)/1000
  LS_CR_RT <- mean(LS_CR_trials$ReactionTime, na.rm = TRUE)/1000
  LS_M_RT <- mean(LS_M_trials$ReactionTime, na.rm = TRUE)/1000
  LS_FAR_RT <- mean(LS_FAR_trials$ReactionTime, na.rm = TRUE)/1000
  
  means <- c(ID,C_HR_CJ,C_CR_CJ,C_M_CJ,C_FAR_CJ,HS_HR_CJ,HS_CR_CJ,HS_M_CJ,HS_FAR_CJ,LS_HR_CJ,LS_CR_CJ,LS_M_CJ,LS_FAR_CJ,
             C_HR_RT,C_CR_RT,C_M_RT,C_FAR_RT,HS_HR_RT,HS_CR_RT,HS_M_RT,HS_FAR_RT,LS_HR_RT,LS_CR_RT,LS_M_RT,LS_FAR_RT)
}

## Get means for all participants
test_data <- data.frame(matrix(1:50, ncol = 25, dimnames = list(c("1","2"),
                                                                c("ID","C_HR_CJ","C_CR_CJ","C_M_CJ","C_FAR_CJ","HS_HR_CJ","HS_CR_CJ","HS_M_CJ","HS_FAR_CJ","LS_HR_CJ","LS_CR_CJ","LS_M_CJ","LS_FAR_CJ",
                                                                  "C_HR_RT","C_CR_RT","C_M_RT","C_FAR_RT","HS_HR_RT","HS_CR_RT","HS_M_RT","HS_FAR_RT","LS_HR_RT","LS_CR_RT","LS_M_RT","LS_FAR_RT"))))
for (x in 1:32){
  test_data[x,] <- Obtain_data(1, str_glue("A", x, ".csv"), x)
}

## Analyze CJs
test_data_CJ <- test_data %>%
  select(ID:LS_FAR_CJ) %>%
  pivot_longer(cols = C_HR_CJ:LS_FAR_CJ, names_to = c("Pair_Type","Response_Type","Condition"),
               names_sep = "_") %>%
  convert_as_factor(ID, Pair_Type, Response_Type) %>%
  select(ID,Pair_Type,Response_Type,value) %>%
  arrange(Pair_Type) %>%
  arrange(Response_Type)

descriptives_CJ <- test_data_CJ %>%
  pivot_wider(names_from = c(Pair_Type, Response_Type), values_from = value) %>%
  describe()

ANOVA_CJ <- test_data_CJ %>%
  anova_test(
    dv = value, wid = ID, within = c(Pair_Type,Response_Type),
    type = 3,
    effect.size = "pes",
    detailed = TRUE,
  )

## For main effects analyses
## Pair Type
test_data_CJ2 <- test_data_CJ %>%
  pivot_wider(names_from = Response_Type, values_from = value)
avg <- c()
for (x in 1:96) {
  avg[x] <- (test_data_CJ2$CR[x] + test_data_CJ2$FAR[x] + test_data_CJ2$HR[x] + test_data_CJ2$M[x])/4
}
test_data_CJ2 <- cbind(test_data_CJ2,avg)

Pair_CJ <- test_data_CJ2 %>%
  t_test(
    avg ~ Pair_Type,
    paired = TRUE,
    p.adjust.method = "bonferroni"
  )

test_data_CJ2 %>%
  cohens_d(avg ~ Pair_Type, paired = TRUE)

descriptives_CJ2 <- test_data_CJ2 %>%
  pivot_wider(names_from = Pair_Type, values_from = avg) %>%
  select(C,HS,LS) %>%
  describe()

## Response Type
test_data_CJ3 <- test_data_CJ %>%
  pivot_wider(names_from = Pair_Type, values_from = value)
avg <- c()
for (x in 1:128) {
  avg[x] <- (test_data_CJ3$C[x] + test_data_CJ3$HS[x] + test_data_CJ3$LS[x])/3
}
test_data_CJ3 <- cbind(test_data_CJ3,avg)

Response_CJ <- test_data_CJ3 %>%
  t_test(
    avg ~ Response_Type,
    paired = TRUE,
    p.adjust.method = "bonferroni"
  )

test_data_CJ3 %>%
  cohens_d(avg ~ Response_Type, paired = TRUE)

descriptives_CJ3 <- test_data_CJ3 %>%
  pivot_wider(names_from = Response_Type, values_from = avg) %>%
  select(CR,FAR,HR,M) %>%
  describe()

## Analyze RTs
test_data_RT <- test_data %>%
  select(ID,C_HR_RT:LS_FAR_RT) %>%
  pivot_longer(cols = C_HR_RT:LS_FAR_RT, names_to = c("Pair_Type","Response_Type","Condition"),
               names_sep = "_") %>%
  convert_as_factor(ID, Pair_Type, Response_Type) %>%
  select(ID,Pair_Type,Response_Type,value)

outliers_RT <- identify_outliers(test_data_RT, value) %>%
  filter(is.extreme == TRUE) %>%
  select(ID, Pair_Type, Response_Type, value)

test_data_RT <- anti_join(test_data_RT, outliers_RT)

for (x in 1:nrow(outliers_RT)) {
  outliers_RT$value[x] <- NaN
}

test_data_RT <- test_data_RT %>%
  rbind(outliers_RT) %>%
  arrange(Pair_Type, Response_Type)

descriptives_RT <- test_data_RT %>%
  pivot_wider(names_from = c(Pair_Type, Response_Type), values_from = value) %>%
  describe()

ANOVA_RT <- test_data_RT %>%
  anova_test(
    dv = value, wid = ID, within = c(Pair_Type,Response_Type),
    type = 3,
    effect.size = "pes",
    detailed = TRUE
  )

## Main effect of Response Type analysis
test_data_RT2 <- test_data_RT %>%
  pivot_wider(names_from = Pair_Type, values_from = value)
avg <- c()
for (x in 1:128) {
  avg[x] <- (test_data_RT2$C[x] + test_data_RT2$HS[x] + test_data_RT2$LS)/3
}
test_data_RT2 <- cbind(test_data_RT2,avg)

Response_RT <- test_data_RT2 %>%
  t_test(
    avg ~ Response_Type,
    paired = TRUE,
    p.adjust.method = "bonferroni"
  )

test_data_RT2 %>%
  cohens_d(avg ~ Response_Type, paired = TRUE)

descriptives_RT2 <- test_data_RT2 %>%
  pivot_wider(names_from = Response_Type, values_from = avg) %>%
  select(CR,FAR,HR,M) %>%
  describe()
