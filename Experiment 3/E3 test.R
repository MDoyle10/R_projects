setwd("C:/Users/Mario/iCloudDrive/Documents/Ph.d/Data analysis")

library(tidyverse)
library(psych)
library(rstatix)

Obtain_data <- function(Experiment_number, File_name, ID){
  file1 <- str_glue("Data/Sorted participant files/Experiment ",Experiment_number,"/",File_name)
  
  data <- read.csv(file1)
  
  P_trials <- data %>%
    filter(Phase %in% "Test") %>%
    filter(ResponseType == "Old" | ResponseType == "New") %>%
    slice(1:30)
  
  P_HR_trials <- P_trials %>%
    filter(ResponseType == "Old" & CorrResponse == "Old")
  P_M_trials <- P_trials %>%
    filter(ResponseType == "New" & CorrResponse == "Old")
  P_CR_trials <- P_trials %>%
    filter(ResponseType == "New" & CorrResponse == "New")
  P_FAR_trials <- P_trials %>%
    filter(ResponseType == "Old" & CorrResponse == "New")
  
  W_trials <- data %>%
    filter(Phase %in% "Test") %>%
    filter(ResponseType == "Old" | ResponseType == "New") %>%
    slice(31:60)
  
  W_HR_trials <- W_trials %>%
    filter(ResponseType == "Old" & CorrResponse == "Old")
  W_M_trials <- W_trials %>%
    filter(ResponseType == "New" & CorrResponse == "Old")
  W_CR_trials <- W_trials %>%
    filter(ResponseType == "New" & CorrResponse == "New")
  W_FAR_trials <- W_trials %>%
    filter(ResponseType == "Old" & CorrResponse == "New")
  
  
  P_HR_CJ <- mean(P_HR_trials$Judgment, na.rm = TRUE)
  P_CR_CJ <- mean(P_CR_trials$Judgment, na.rm = TRUE)
  P_M_CJ <- mean(P_M_trials$Judgment, na.rm = TRUE)
  P_FAR_CJ <- mean(P_FAR_trials$Judgment, na.rm = TRUE)
  W_HR_CJ <- mean(W_HR_trials$Judgment, na.rm = TRUE)
  W_CR_CJ <- mean(W_CR_trials$Judgment, na.rm = TRUE)
  W_M_CJ <- mean(W_M_trials$Judgment, na.rm = TRUE)
  W_FAR_CJ <- mean(W_FAR_trials$Judgment, na.rm = TRUE)
  
  P_HR_RT <- mean(P_HR_trials$ReactionTime, na.rm = TRUE)/1000
  P_CR_RT <- mean(P_CR_trials$ReactionTime, na.rm = TRUE)/1000
  P_M_RT <- mean(P_M_trials$ReactionTime, na.rm = TRUE)/1000
  P_FAR_RT <- mean(P_FAR_trials$ReactionTime, na.rm = TRUE)/1000
  W_HR_RT <- mean(W_HR_trials$ReactionTime, na.rm = TRUE)/1000
  W_CR_RT <- mean(W_CR_trials$ReactionTime, na.rm = TRUE)/1000
  W_M_RT <- mean(W_M_trials$ReactionTime, na.rm = TRUE)/1000
  W_FAR_RT <- mean(W_FAR_trials$ReactionTime, na.rm = TRUE)/1000
  
  means <- c(ID,P_HR_CJ,P_CR_CJ,P_M_CJ,P_FAR_CJ,W_HR_CJ,W_CR_CJ,W_M_CJ,W_FAR_CJ,
             P_HR_RT,P_CR_RT,P_M_RT,P_FAR_RT,W_HR_RT,W_CR_RT,W_M_RT,W_FAR_RT)
}

## Get means for all participants
test_data <- data.frame(matrix(1:34, ncol = 17, dimnames = list(c("1","2"),
                                                                c("ID","P_HR_CJ","P_CR_CJ","P_M_CJ","P_FAR_CJ","W_HR_CJ","W_CR_CJ","W_M_CJ","W_FAR_CJ",
                                                                  "P_HR_RT","P_CR_RT","P_M_RT","P_FAR_RT","W_HR_RT","W_CR_RT","W_M_RT","W_FAR_RT"))))
for (x in 1:46){
  test_data[x,] <- Obtain_data(7, str_glue("G", x, ".csv"), x)
}

## Remove participants
exclude <- c(4,12,20,22,35,36,37,39)
test_data <- test_data[-which(test_data$ID %in% exclude),]

## Analyze CJs
test_data_CJ <- test_data %>%
  select(ID:W_FAR_CJ) %>%
  pivot_longer(cols = P_HR_CJ:W_FAR_CJ, names_to = c("Pair_Type","Response_Type","Condition"),
               names_sep = "_") %>%
  convert_as_factor(ID, Pair_Type, Response_Type) %>%
  select(ID,Pair_Type,Response_Type,value)

descriptives_CJ <- test_data_CJ %>%
  pivot_wider(names_from = c(Pair_Type, Response_Type), values_from = value) %>%
  select(P_HR:W_FAR) %>%
  describe()

ANOVA_CJ <- test_data_CJ %>%
  anova_test(
    dv = value, wid = ID, within = c(Pair_Type,Response_Type),
    type = 3,
    effect.size = "pes",
    detailed = TRUE
  )

## Means for main effects
P_CJ <- mean(descriptives_CJ$mean[1:4])
W_CJ <- mean(descriptives_CJ$mean[5:8])

HR_CJ <- mean(c(descriptives_CJ$mean[1], descriptives_CJ$mean[5]))
FAR_CJ <- mean(c(descriptives_CJ$mean[4], descriptives_CJ$mean[8]))
CR_CJ <- mean(c(descriptives_CJ$mean[2], descriptives_CJ$mean[6]))
M_CJ <- mean(c(descriptives_CJ$mean[3], descriptives_CJ$mean[7]))

## Main effect of Response Type analysis
test_data_CJ2 <- test_data_CJ %>%
  pivot_wider(names_from = Pair_Type, values_from = value)
avg <- c()
for (x in 1:152) {
  avg[x] <- mean(c(test_data_CJ2$P[x], test_data_CJ2$W[x]))
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

descriptives_CJ2 <- test_data_CJ2 %>%
  pivot_wider(names_from = Response_Type, values_from = avg) %>%
  select(CR,FAR,HR,M) %>%
  describe()

## Analyze RTs
test_data_RT <- test_data %>%
  select(ID,P_HR_RT:W_FAR_RT) %>%
  pivot_longer(cols = P_HR_RT:W_FAR_RT, names_to = c("Pair_Type","Response_Type","Condition"),
               names_sep = "_") %>%
  convert_as_factor(ID, Pair_Type, Response_Type) %>%
  select(ID, Pair_Type, Response_Type, value) 

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
for (x in 1:152) {
  avg[x] <- (test_data_RT2$P[x] + test_data_RT2$W[x])/2
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

descriptives_RT2 <- test_data_RT2 %>%
  pivot_wider(names_from = Response_Type, values_from = avg) %>%
  select(CR,FAR,HR,M) %>%
  describe()
