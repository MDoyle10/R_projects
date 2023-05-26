# Function: Takes a csv data file and sorts it to be ready for analysis
# Windows version

# Directory location of participant files
setwd("C:/Users/Mario/iCloudDrive/Documents/Ph.d/Data analysis")

# Function
Sort_data <- function(Experiment_number, File_name, temp){
  # Necessary libraries
  library(tidyverse)
  
  # Input and Output file names stored in temp objects
  # Note: make input file an argument for function
  ID_in <- str_glue("Data/Participant files/Experiment ",
                    Experiment_number, "/", File_name, ".csv")
  ID_out <- str_glue("Data/Sorted participant files/Experiment ",
                     Experiment_number, "/", File_name, temp, ".csv")
  
  # Read the data file and store it in temp object;
  # Skips the first few rows and renames the column names
  Data <- readr::read_delim(ID_in, delim = ",", skip = 1,
                          col_names = c("ID", "Phase","Trial","Judgment", "TotalTime","ResponseType",
                                         "Response","Extraneous","Outcome","ReactionTime","CorrResponse"),
                           col_types = ("dccddccdddc"))
  
  # Check and correct Judgment values
  extreme1 <- filter(Data,Phase %in% "Study" & Judgment > 100 | Judgment < 0)
  extreme3 <- filter(Data,Phase %in% "Test" & Judgment > 10 | Judgment < 0)
  Data1 <- Data %>%
    filter(Phase %in% "Study 1")
  Data2 <- Data %>%
    filter(Phase %in% "Study 2")
  Data3 <- Data %>%
    filter(Phase %in% "Test 1")
  Data4 <- Data %>%
    filter(Phase %in% "Test 2")
  Data1[which(Data1$Judgment %in% extreme1$Judgment),4] <- NA
  Data2[which(Data2$Judgment %in% extreme2$Judgment),4] <- NA
  Data3[which(Data3$Judgment %in% extreme3$Judgment),4] <- NA
  Data4[which(Data4$Judgment %in% extreme4$Judgment),4] <- NA
  
  # Change Judgment column to align properly
  # For E7
  # Data3 <- Data3 %>%
  #  mutate (Judgment = lead(Judgment))
  # Data1 <- bind_rows(Data2,Data3)
  
  # For E1-E6
  # Data1 <- mutate(Data1, Judgment = lead(Judgment))
  
  Data <- bind_rows(Data1,Data2,Data3,Data4)
  
  # Sort the data by Phase then Trial
  # Data1 <- arrange(Data1, Phase, Trial)
  
  # Save sorted data to a new text file
  write_excel_csv(Data, ID_out, na = "")
}

# Function used over all participant files
for (x in 1:30){
  Sort_data(1, str_glue("A",x), 2)
}
