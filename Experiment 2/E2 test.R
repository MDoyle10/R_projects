setwd("C:/Users/Mario/iCloudDrive/Documents/Ph.d/Data analysis")

library(tidyverse)
library(psych)
library(rstatix)
library(car)

Obtain_data <- function(Experiment_number, File_name, ID){
  file1 <- str_glue("Data/Sorted participant files/Experiment ",Experiment_number,"/",File_name)
  
  data <- read.csv(file1)
  
  A_trials <- data %>%
    filter(Phase %in% "Test") %>%
    filter(ResponseType == "Old" | ResponseType == "New") %>%
    slice(1:30)
  
  A_HR_trials <- A_trials %>%
    filter(ResponseType == "Old" & CorrResponse == "Old")
  A_M_trials <- A_trials %>%
    filter(ResponseType == "New" & CorrResponse == "Old")
  A_CR_trials <- A_trials %>%
    filter(ResponseType == "New" & CorrResponse == "New")
  A_FAR_trials <- A_trials %>%
    filter(ResponseType == "Old" & CorrResponse == "New")
    
  C_trials <- data %>%
    filter(Phase %in% "Test") %>%
    filter(ResponseType == "Old" | ResponseType == "New") %>%
    slice(31:60)
  
  C_HR_trials <- C_trials %>%
    filter(ResponseType == "Old" & CorrResponse == "Old")
  C_M_trials <- C_trials %>%
    filter(ResponseType == "New" & CorrResponse == "Old")
  C_CR_trials <- C_trials %>%
    filter(ResponseType == "New" & CorrResponse == "New")
  C_FAR_trials <- C_trials %>%
    filter(ResponseType == "Old" & CorrResponse == "New")
 
  
  A_HR_CJ <- mean(A_HR_trials$Judgment, na.rm = TRUE)
  A_CR_CJ <- mean(A_CR_trials$Judgment, na.rm = TRUE)
  A_M_CJ <- mean(A_M_trials$Judgment, na.rm = TRUE)
  A_FAR_CJ <- mean(A_FAR_trials$Judgment, na.rm = TRUE)
  C_HR_CJ <- mean(C_HR_trials$Judgment, na.rm = TRUE)
  C_CR_CJ <- mean(C_CR_trials$Judgment, na.rm = TRUE)
  C_M_CJ <- mean(C_M_trials$Judgment, na.rm = TRUE)
  C_FAR_CJ <- mean(C_FAR_trials$Judgment, na.rm = TRUE)
  
  A_HR_RT <- mean(A_HR_trials$ReactionTime, na.rm = TRUE)/1000
  A_CR_RT <- mean(A_CR_trials$ReactionTime, na.rm = TRUE)/1000
  A_M_RT <- mean(A_M_trials$ReactionTime, na.rm = TRUE)/1000
  A_FAR_RT <- mean(A_FAR_trials$ReactionTime, na.rm = TRUE)/1000
  C_HR_RT <- mean(C_HR_trials$ReactionTime, na.rm = TRUE)/1000
  C_CR_RT <- mean(C_CR_trials$ReactionTime, na.rm = TRUE)/1000
  C_M_RT <- mean(C_M_trials$ReactionTime, na.rm = TRUE)/1000
  C_FAR_RT <- mean(C_FAR_trials$ReactionTime, na.rm = TRUE)/1000

  means <- c(ID,A_HR_CJ,A_CR_CJ,A_M_CJ,A_FAR_CJ,C_HR_CJ,C_CR_CJ,C_M_CJ,C_FAR_CJ,
             A_HR_RT,A_CR_RT,A_M_RT,A_FAR_RT,C_HR_RT,C_CR_RT,C_M_RT,C_FAR_RT)
}

## Get means for all participants
test_data <- data.frame(matrix(1:34, ncol = 17, dimnames = list(c("1","2"),
                                                                      c("ID","A_HR_CJ","A_CR_CJ","A_M_CJ","A_FAR_CJ","C_HR_CJ","C_CR_CJ","C_M_CJ","C_FAR_CJ",
                                                                        "A_HR_RT","A_CR_RT","A_M_RT","A_FAR_RT","C_HR_RT","C_CR_RT","C_M_RT","C_FAR_RT"))))
for (x in 1:32){
  test_data[x,] <- Obtain_data(6, str_glue("F", x, ".csv"), x)
}

## Remove participants
exclude <- c(4,5,10,12,19,22,28)
test_data <- test_data[-which(test_data$ID %in% exclude),]

## Analyze CJs
test_data_CJ <- test_data %>%
  select(ID:C_FAR_CJ) %>%
  pivot_longer(cols = A_HR_CJ:C_FAR_CJ, names_to = c("Pair_Type","Response_Type","condition"),
               names_sep = "_") %>%
  convert_as_factor(ID, Pair_Type, Response_Type) %>%
  select(ID,Pair_Type,Response_Type,value)

descriptives_CJ <- test_data_CJ %>%
  pivot_wider(names_from = c(Pair_Type, Response_Type), values_from = value) %>%
  describe()

ANOVA_CJ <- test_data_CJ %>%
  anova_test(
    dv = value, wid = ID, within = c(Pair_Type,Response_Type),
    type = 3,
    effect.size = "pes",
    detailed = TRUE
  )

CJ_A <- (descriptives_CJ$mean[2] + descriptives_CJ$mean[3] + descriptives_CJ$mean[4] + descriptives_CJ$mean[5])/4
CJ_C <- (descriptives_CJ$mean[6] + descriptives_CJ$mean[7] + descriptives_CJ$mean[8] + descriptives_CJ$mean[9])/4

## Main effect of Response Type analysis
test_data_CJ2 <- test_data_CJ %>%
  pivot_wider(names_from = Pair_Type, values_from = value)
avg <- c()
for (x in 1:100) {
  avg[x] <- (test_data_CJ2$A[x] + test_data_CJ2$C[x])/2
}
test_data_CJ2 <- cbind(test_data_CJ2,avg)

Response_CJ <- test_data_CJ2 %>%
  t_test(
  avg ~ Response_Type,
  paired = TRUE,
  var.equal = TRUE,
  p.adjust.method = "bonferroni"
)

test_data_CJ2 %>%
  cohens_d(avg ~ Response_Type, paired = TRUE)

CJ_HR <- (descriptives_CJ$mean[2] + descriptives_CJ$mean[6])/2
CJ_FAR <- (descriptives_CJ$mean[5] + descriptives_CJ$mean[9])/2
CJ_CR <- (descriptives_CJ$mean[3] + descriptives_CJ$mean[7])/2
CJ_M <- (descriptives_CJ$mean[4] + descriptives_CJ$mean[8])/2

## Analyze RTs
test_data_RT <- test_data %>%
  select(ID,A_HR_RT:C_FAR_RT) %>%
  pivot_longer(cols = A_HR_RT:C_FAR_RT, names_to = c("Pair_Type","Response_Type","condition"),
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

RT_A <- (descriptives_RT$mean[2] + descriptives_RT$mean[3] + descriptives_RT$mean[4] + descriptives_RT$mean[5])/4
RT_C <- (descriptives_RT$mean[6] + descriptives_RT$mean[7] + descriptives_RT$mean[8] + descriptives_RT$mean[9])/4

## Main effect of Response Type analysis
test_data_RT2 <- test_data_RT %>%
  pivot_wider(names_from = Pair_Type, values_from = value)
avg <- c()
for (x in 1:100) {
  avg[x] <- (test_data_RT2$A[x] + test_data_RT2$C[x])/2
}
test_data_RT2 <- cbind(test_data_RT2,avg)

Response_RT <- test_data_RT2 %>%
  t_test(
    avg ~ Response_Type,
    paired = TRUE,
    var.equal = TRUE,
    p.adjust.method = "bonferroni"
  )

test_data_RT2 %>%
  cohens_d(avg ~ Response_Type, paired = TRUE)

RT_HR <- (descriptives_RT$mean[4] + descriptives_RT$mean[8])/2
RT_FAR <- (descriptives_RT$mean[3] + descriptives_RT$mean[7])/2
RT_CR <- (descriptives_RT$mean[2] + descriptives_RT$mean[6])/2
RT_M <- (descriptives_RT$mean[5] + descriptives_RT$mean[9])/2
