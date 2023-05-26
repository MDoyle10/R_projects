setwd("C:/Users/Mario/iCloudDrive/Documents/Ph.d/Data analysis")

library(tidyverse)
library(psych)
library(rstatix)
library(car)

Obtain_data <- function(Experiment_number, File_name, ID){
  file1 <- str_glue("Data/Sorted participant files/Experiment ",Experiment_number,"/",File_name)
  
  data <- read.csv(file1)
  
  Test_trials <- filter(data,Phase %in% "Test")
  
  P_HR <- Test_trials %>%
    filter(CorrResponse == "Old") %>%
    slice(1:15)
  P_HR_mean <- (sum(P_HR$Outcome)+.5)/(15+1)
  
  P_FAR <- Test_trials %>%
    filter(CorrResponse == "New") %>%
    slice(1:15)
  P_FAR_mean <- 1-((sum(P_FAR$Outcome)+.5)/(15+1))
  
  W_HR <- Test_trials %>%
    filter(CorrResponse == "Old") %>%
    slice(16:30)
  W_HR_mean <- (sum(W_HR$Outcome)+.5)/(15+1)
  
  W_FAR <- Test_trials %>%
    filter(CorrResponse == "New") %>%
    slice(16:30)
  W_FAR_mean <- 1-((sum(W_FAR$Outcome)+.5)/(15+1))
  
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
  results <- c(ID,P_HR_mean,P_FAR_mean,W_HR_mean,W_FAR_mean,x)
}

## Get means for all participants
performance_data <- data.frame(matrix(1:12, ncol = 6, dimnames = list(c("1","2"),
                                                                      c("ID","P_HR","P_FAR","W_HR","W_FAR","x"))))
for (x in 1:46){
  performance_data[x,] <- Obtain_data(7, str_glue("G", x, ".csv"), x)
} 

## Add columns for d' and C
performance_data <- mutate(performance_data,W_d = (qnorm(performance_data$W_HR)-qnorm(performance_data$W_FAR)))
performance_data <- mutate(performance_data,P_d = (qnorm(performance_data$P_HR)-qnorm(performance_data$P_FAR)))
performance_data <- mutate(performance_data,W_C = -(qnorm(performance_data$W_HR)+qnorm(performance_data$W_FAR))/2)
performance_data <- mutate(performance_data,P_C = -(qnorm(performance_data$P_HR)+qnorm(performance_data$P_FAR))/2)

## Remove participants with CR score <= .15
performance_data <- performance_data %>%
  filter(x != 2)

## Analyzing hits and false alarm data
data_a <- performance_data %>%
  select(ID:W_FAR) %>%
  pivot_longer(cols = P_HR:W_FAR, names_to = c("Pair_Type", "Probe_Type"),
               names_sep = "_")%>%
  convert_as_factor(ID, Pair_Type, Probe_Type) %>%
  arrange(Pair_Type) %>%
  arrange(Probe_Type)

descriptives_a <- data_a %>%
  pivot_wider(names_from = c(Pair_Type, Probe_Type), values_from = value) %>%
  select(P_FAR:W_HR) %>%
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

P <- mean(c(descriptives_a$mean[1], descriptives_a$mean[3]))
W <- mean(c(descriptives_a$mean[2], descriptives_a$mean[4]))

## Interaction
interaction_a <- data_a %>%
  group_by(Probe_Type) %>%
  t_test(
    value ~ Pair_Type,
    paired = TRUE,
    var.equal = TRUE,
    p.adjust.method = "bonferroni"
  ) %>%
  ungroup()

data_a %>%
  group_by(Probe_Type) %>%
  cohens_d(value ~ Pair_Type, paired = TRUE ) %>%
  ungroup()

## Analyzing d'
data_b <- performance_data %>%
  select(ID, W_d, P_d) %>%
  pivot_longer(cols = W_d:P_d, names_to = c("Pair_Type"))%>%
  convert_as_factor(ID, Pair_Type)

descriptives_b <- data_b %>%
  pivot_wider(names_from = Pair_Type, values_from = value) %>%
  select(W_d,P_d) %>%
  describe() 

ANOVA_b <- aov(value ~ Pair_Type, data = data_b)
summary(ANOVA_b)
partial_eta_squared(ANOVA_b)

leveneTest(value ~ Pair_Type, data = data_b)

ANOVA_b_residuals <- residuals(object = ANOVA_b)
shapiro.test(x = ANOVA_b_residuals)

kruskal.test(value ~ Pair_Type, data = data_b)

## Analyzing C
data_c <- performance_data %>%
  select(ID,P_C,W_C) %>%
  pivot_longer(cols = W_C:P_C, names_to = c("Pair_Type"))%>%
  convert_as_factor(ID, Pair_Type)

descriptives_c <- data_c %>%
  pivot_wider(names_from = Pair_Type, values_from = value) %>%
  select(W_C,P_C) %>%
  describe() 

# Follow up one sample t-tests
onesample_W <- performance_data %>%
  t_test(W_C ~ 1, mu = 0)
performance_data %>%
  cohens_d(W_C ~1, mu = 0)
onesample_P <- performance_data %>%
  t_test(P_C ~ 1, mu = 0)
performance_data %>%
  cohens_d(P_C ~1, mu = 0)

# Figure 3 - HR and FAR for Pictures and Words
Fig3_data <- data.frame(
  Pair_Types = c("Pictures","Words","Pictures","Words"),
  Probe_Types = c("2FAR","2FAR","1HR","1HR"),
  Proportion = descriptives_a$mean[1:4],
  SD = descriptives_a$sd[1:4]
) %>%
  arrange(desc(Proportion))

ggplot(data = Fig3_data) + 
  geom_bar(mapping = aes(x = Pair_Types, y = Proportion, fill = Probe_Types), stat = "identity", position = "dodge") + 
  geom_errorbar(mapping = aes(x = Pair_Types, ymin = Proportion-SD, ymax = Proportion+SD, group = Probe_Types), width = .1, position = position_dodge(width = .9)) + 
  labs(x = NULL, fill = NULL) + 
  theme(text = element_text(size = 14), legend.position = "bottom") +
  scale_fill_grey(name = "Legend", labels = c("HR","FAR"))
 