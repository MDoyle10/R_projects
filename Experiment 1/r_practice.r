data <- read.csv("~/Documents/Ph.d/Data analysis/Data/Sorted participant files/Experiment 1/A1.csv")

study_data <- data %>%
filter(Phase == "Study") %>%
filter(Trial != "JOL 2")

plot <- ggscatter(study_data, x = "ReactionTime", y = "Judgment",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Study time (millisecond)", ylab = "JOLs")

res <- cor.test(study_data$Judgment, study_data$ReactionTime,
                    method = "kendall")