# Sets up data from multiple participant files, then performs various analyses 
# Windows version
# Works for R version 4.1.1

# Location of data
setwd("C:/Users/Mario/iCloudDrive/Documents/Ph.d/Data analysis")

# Libraries used and the version test on
library(psych)          #psych_2.1.9
library(tidyverse)      #tidyverse_1.3.1
library(rstatix)        #rstatix_0.7.0
library(car)            #car_3.0-11

# Read in data and some preliminary arranging 
Obtain_data <- function(Experiment_number, File_name, ID){
  file1 <- str_glue("Data/Sorted participant files/Experiment ",Experiment_number,"/",File_name)
  
  data <- read.csv(file1)
  
  Test_trials <- filter(data,Phase %in% "Test")
  
  C_HR <- Test_trials %>%
    filter(CorrResponse == "Old") %>%
    slice(1:12)
  C_HR_mean <- (sum(C_HR$Outcome)+.5)/(12+1)
  
  C_FAR <- Test_trials %>%
    filter(CorrResponse == "New") %>%
    slice(1:12)
  C_FAR_mean <- 1-((sum(C_FAR$Outcome)+.5)/(12+1))
  
  HS_HR <- Test_trials %>%
    filter(CorrResponse == "Old") %>%
    slice(13:24)
  HS_HR_mean <- (sum(HS_HR$Outcome)+.5)/(12+1)
  
  HS_FAR <- Test_trials %>%
    filter(CorrResponse == "New") %>%
    slice(13:24)
  HS_FAR_mean <- 1-((sum(HS_FAR$Outcome)+.5)/(12+1))
  
  LS_HR <- Test_trials %>%
    filter(CorrResponse == "Old") %>%
    slice(25:36)
  LS_HR_mean <- (sum(LS_HR$Outcome)+.5)/(12+1)
  
  LS_FAR <- Test_trials %>%
    filter(CorrResponse == "New") %>%
    slice(25:36)
  LS_FAR_mean <- 1-((sum(LS_FAR$Outcome)+.5)/(12+1))
  
  results <- c(ID,C_HR_mean,C_FAR_mean,HS_HR_mean,HS_FAR_mean,LS_HR_mean,LS_FAR_mean)
  
}

# Create a data frame
performance_data <- data.frame(matrix(1:14, ncol = 7, dimnames = list(c("1","2"),
                                                                      c("ID","C_HR","C_FAR","HS_HR","HS_FAR","LS_HR","LS_FAR"))))
for (x in 1:32){
  performance_data[x,] <- Obtain_data(1, str_glue("A", x, ".csv"), x)
}

## Add columns for d' and C
performance_data <- mutate(performance_data,C_d = (qnorm(performance_data$C_HR)-qnorm(performance_data$C_FAR)))
performance_data <- mutate(performance_data,HS_d = (qnorm(performance_data$HS_HR)-qnorm(performance_data$HS_FAR)))
performance_data <- mutate(performance_data,LS_d = (qnorm(performance_data$LS_HR)-qnorm(performance_data$LS_FAR)))
performance_data <- mutate(performance_data,C_C = -(qnorm(performance_data$C_HR)+qnorm(performance_data$C_FAR))/2)
performance_data <- mutate(performance_data,HS_C = -(qnorm(performance_data$HS_HR)+qnorm(performance_data$HS_FAR))/2)
performance_data <- mutate(performance_data,LS_C = -(qnorm(performance_data$LS_HR)+qnorm(performance_data$LS_FAR))/2)

## Analyzing hits and false alarm data
data_a <- select(performance_data,ID:LS_FAR) %>%
  pivot_longer(cols = C_HR:LS_FAR, names_to = c("Pair_Type", "Probe_Type"),
               names_sep = "_")%>%
  convert_as_factor(ID, Pair_Type, Probe_Type) %>%
  arrange(Pair_Type) %>%
  arrange(Probe_Type)

descriptives_a <- data_a %>%
  pivot_wider(names_from = c(Pair_Type, Probe_Type), values_from = value) %>%
  select(C_FAR:LS_HR) %>%
  describe()

ANOVA_a <- data_a %>%
  anova_test(
  dv = value, wid = ID, within = c(Pair_Type, Probe_Type),
  type = 3,
  effect.size = "pes",
  detailed = TRUE)

data_a1 <- data_a %>%
  pivot_wider(names_from = Probe_Type, values_from = value)
avg <- c()
for (x in 1:96) {
  avg[x] <- (data_a1$FAR[x] + data_a1$HR[x])/2
}
data_a1 <- cbind(data_a1,avg)
Pair_Type_a <- data_a1 %>%
  t_test(
    avg ~ Pair_Type,
    p.adjust.method = "bonferroni",
    paired = TRUE,
    var.equal = TRUE
  )

data_a1 %>%
  cohens_d(avg ~ Pair_Type, paired = TRUE)

## Means for main effects
HR <- mean(descriptives_a$mean[4:6])
FAR <- mean(descriptives_a$mean[1:3])

C <- mean(c(descriptives_a$mean[1], descriptives_a$mean[4]))
HS <- mean(c(descriptives_a$mean[2], descriptives_a$mean[5]))
LS <- mean(c(descriptives_a$mean[3], descriptives_a$mean[6]))

## Analyzing d'
data_b <- select(performance_data,ID,C_d:LS_d) %>%
  pivot_longer(cols = C_d:LS_d, names_to = c("Pair_Type"))%>%
  convert_as_factor(ID, Pair_Type)
  
descriptives_b <- data_b %>%
  pivot_wider(names_from = Pair_Type, values_from = value) %>%
  select(C_d,HS_d,LS_d) %>%
  describe()

ANOVA_b <- aov(value ~ Pair_Type, data = data_b)
summary(ANOVA_b)
partial_eta_squared(ANOVA_b)

leveneTest(value ~ Pair_Type, data = data_b)

ANOVA_b_residuals <- residuals(object = ANOVA_b)
shapiro.test(x = ANOVA_b_residuals)

kruskal.test(value ~ Pair_Type, data = data_b)

## Analyzing C
data_c <- select(performance_data,ID,C_C:LS_C) %>%
  pivot_longer(cols = C_C:LS_C, names_to = c("Pair_Type")) %>%
  convert_as_factor(ID, Pair_Type)

descriptives_c <- data_c %>%
  pivot_wider(names_from = Pair_Type, values_from = value) %>%
  select(C_C,HS_C,LS_C) %>%
  describe()

# Follow up one sample t-tests
onesample_C <- performance_data %>%
  t_test(C_C ~ 1, mu = 0)
performance_data %>%
  cohens_d(C_C ~1, mu = 0)
onesample_HS <- performance_data %>%
  t_test(HS_C ~ 1, mu = 0)
performance_data %>%
  cohens_d(HS_C ~1, mu = 0)
onesample_LS <- performance_data %>%
  t_test(LS_C ~ 1, mu = 0)
performance_data %>%
  cohens_d(LS_C ~1, mu = 0)

# Figure 1 - HR and FAR for 3 pair types
Fig1_data <- data.frame(
  Pair_Types = c("Compound","High Strength","Low Strength","Compound","High Strength","Low Strength"),
  Probe_Types = c("2FAR","2FAR","2FAR","1HR","1HR","1HR"),
  B_W = c("grey","grey","grey","black","black","black"),
  Proportion = descriptives_a$mean[1:6],
  SD = descriptives_a$sd[1:6]
) %>%
  arrange(desc(Proportion))

ggplot(data = Fig1_data) + 
  geom_bar(mapping = aes(x = Pair_Types, y = Proportion, fill = Probe_Types), stat = "identity", position = "dodge") + 
  geom_errorbar(mapping = aes(x = Pair_Types, ymin = Proportion-SD, ymax = Proportion+SD, group = Probe_Types), width = .1, position = position_dodge(width = .9)) + 
  labs(x = NULL, fill = NULL) + 
  theme(text = element_text(size = 14), legend.position = "bottom") +
  scale_fill_grey(name = "Legend", labels = c("HR","FAR"))
