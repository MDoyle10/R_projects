setwd("C:/Users/Mario/iCloudDrive/Documents/Ph.d/Data analysis")

library(tidyverse)
library(psych)
library(rstatix)
library(car)

Obtain_data <- function(Experiment_number, File_name, ID){
  file1 <- str_glue("Data/Sorted participant files/Experiment ",Experiment_number,"/",File_name)
  
  data <- read.csv(file1)
  
  Test_trials <- filter(data,Phase %in% "Test")
  
  A_HR <- Test_trials %>%
    filter(CorrResponse == "Old") %>%
    slice(1:15)
  A_HR_mean <- (sum(A_HR$Outcome)+.5)/(15+1)
  
  A_FAR <- Test_trials %>%
    filter(CorrResponse == "New") %>%
    slice(1:15)
  A_FAR_mean <- 1-((sum(A_FAR$Outcome)+.5)/(15+1))
  
  C_HR <- Test_trials %>%
    filter(CorrResponse == "Old") %>%
    slice(16:30)
  C_HR_mean <- (sum(C_HR$Outcome)+.5)/(15+1)
  
  C_FAR <- Test_trials %>%
    filter(CorrResponse == "New") %>%
    slice(16:30)
  C_FAR_mean <- 1-((sum(C_FAR$Outcome)+.5)/(15+1))
  
  HR_data <- Test_trials %>%
    filter(CorrResponse == "Old") %>%
    select(Outcome)
  HR <- mean(HR_data$Outcome,na.rm = TRUE)
  
  FAR_data <- Test_trials %>% 
    filter(CorrResponse == "New") %>%
    select(Outcome)
  FAR <- 1 - mean(FAR_data$Outcome, na.rm = TRUE)
  
  if (HR-FAR >= 0.15) {
    x = 1} else {
      x = 2
    }
  results <- c(ID,C_HR_mean,C_FAR_mean,A_HR_mean,A_FAR_mean,x)
}

## Get means for all participants
performance_data <- data.frame(matrix(1:12, ncol = 6, dimnames = list(c("1","2"),
                                                         c("ID","C_HR","C_FAR","A_HR","A_FAR","x"))))
for (x in 1:32){
  performance_data[x,] <- Obtain_data(6, str_glue("F", x, ".csv"), x)
}

## Add columns for d' and C
performance_data <- mutate(performance_data,C_d = (qnorm(performance_data$C_HR)-qnorm(performance_data$C_FAR)))
performance_data <- mutate(performance_data,A_d = (qnorm(performance_data$A_HR)-qnorm(performance_data$A_FAR)))
performance_data <- mutate(performance_data,C_C = -(qnorm(performance_data$C_HR)+qnorm(performance_data$C_FAR))/2)
performance_data <- mutate(performance_data,A_C = -(qnorm(performance_data$A_HR)+qnorm(performance_data$A_FAR))/2)

## Remove participants with CR score <= .15
performance_data <- performance_data %>%
  filter(x != 2)

## Analyzing hits and false alarm data
data_a <- select(performance_data,ID:A_FAR) %>%
  pivot_longer(cols = C_HR:A_FAR, names_to = c("Pair_Type", "Probe_Type"),
               names_sep = "_")%>%
  convert_as_factor(ID, Pair_Type, Probe_Type) %>%
  arrange(Pair_Type) %>%
  arrange(Probe_Type)

descriptives_a <- data_a %>%
  pivot_wider(names_from = c(Pair_Type,Probe_Type), values_from = value) %>%
  select(A_FAR:C_HR) %>%
  describe()

ANOVA_a <- anova_test(
  data = data_a,
  dv = value, wid = ID, within = c(Pair_Type, Probe_Type),
  type = 3,
  effect.size = "pes",
  detailed = TRUE
)

## Means for main effects
HR <- mean(descriptives_a$mean[3:4])
FAR <- mean(descriptives_a$mean[1:2])

A <- mean(c(descriptives_a$mean[1], descriptives_a$mean[3]))
C <- mean(c(descriptives_a$mean[2], descriptives_a$mean[4]))

## Interaction
interaction_a <- data_a %>%
  group_by(Probe_Type) %>%
  t_test(
    value ~ Pair_Type,
    paired = TRUE,
    p.adjust.method = "bonferroni"
  ) %>%
  ungroup()

data_a %>%
  group_by(Probe_Type) %>%
  cohens_d(value ~ Pair_Type, paired = TRUE ) %>%
  ungroup()

## Analyzing d'
data_b <- select(performance_data,ID,C_d,A_d) %>%
  pivot_longer(cols = C_d:A_d, names_to = c("Pair_Type"))%>%
  convert_as_factor(ID, Pair_Type)

descriptives_b <- data_b %>%
  pivot_wider(names_from = Pair_Type, values_from = value) %>%
  select(C_d,A_d) %>%
  describe()

ANOVA_b <- aov(value ~ Pair_Type, data = data_b)
summary(ANOVA_b)
partial_eta_squared(ANOVA_b)

leveneTest(value ~ Pair_Type, data = data_b)

ANOVA_b_residuals <- residuals(object = ANOVA_b)
shapiro.test(x = ANOVA_b_residuals)

kruskal.test(value ~ Pair_Type, data = data_b)

## Analyzing C
data_c <- select(performance_data,ID,C_C,A_C) %>%
  pivot_longer(cols = C_C:A_C, names_to = c("Pair_Type"))%>%
  convert_as_factor(ID, Pair_Type)

descriptives_c <- data_c %>%
  pivot_wider(names_from = Pair_Type, values_from = value) %>%
  select(C_C,A_C) %>%
  describe()

# Follow up one sample t-tests
onesample_C <- performance_data %>%
  t_test(C_C ~ 1, mu = 0)
performance_data %>%
  cohens_d(C_C ~1, mu = 0)
onesample_A <- performance_data %>%
  t_test(A_C ~ 1, mu = 0)
performance_data %>%
  cohens_d(A_C ~1, mu = 0)

# Figure 2 - HR and FAR for Concrete and Abstract
Fig2_data <- data.frame(
  Pair_Types = c("Abstract","Concrete","Abstract","Concrete"),
  Probe_Types = c("2FAR","2FAR","1HR","1HR"),
  Proportion = descriptives_a$mean[1:4],
  SD = descriptives_a$sd[1:4]
) %>%
  arrange(desc(Proportion))

ggplot(data = Fig2_data) + 
  geom_bar(mapping = aes(x = Pair_Types, y = Proportion, fill = Probe_Types), stat = "identity", position = "dodge") + 
  geom_errorbar(mapping = aes(x = Pair_Types, ymin = Proportion-SD, ymax = Proportion+SD, group = Probe_Types), width = .1, position = position_dodge(width = .9)) + 
  labs(x = NULL, fill = NULL) + 
  theme(text = element_text(size = 14), legend.position = "bottom") +
  scale_fill_grey(name = "Legend", labels = c("HR","FAR"))
