require(lubridate)
filepath <-"~/Desktop/DBMI/Research/Pharmacogenomics/Data/PipelineResults/7_AntidepressantDrugs_Ordered_210212.csv"
ordered_drugs <- read.csv(filepath)
filepath <- "~/Desktop/DBMI/Research/Pharmacogenomics/Data/PipelineResults/7_AntidepressantDates_Ordered_210212.csv"
ordered_dates <- read.csv(filepath)

# Remove first column that had "X" 
ordered_dates <- ordered_dates[,-1]
ordered_drugs <- ordered_drugs[,-1]

# Sanity check that tables line up:
# sum(ordered_dates[,1]==ordered_drugs[,1])==dim(ordered_dates)[1]

# Create matrix for collecting outcome data
outcome_matrix <- matrix(0, nrow=dim(ordered_dates)[1], ncol=8)
colnames(outcome_matrix) <- c("STUDY_ID", "Date1", "Drug1", "Date2", "Drug2", "Interval(Days)", "Interval(Weeks)", "Outcome")

# Make it so we can add drug names to 
for (i in seq_along(1:dim(outcome_matrix)[2])){
  outcome_matrix[,i] <- as.character(outcome_matrix[,i])
}

# Make the first column STUDY_IDs
outcome_matrix[,1] <- ordered_dates[,1]

# Test 8 weeks
# weekthreshold <- 8
# Test 12 weeks
weekthreshold <- 12 

# Calculate intervals iteratively
for (i in seq_along(1:dim(outcome_matrix)[1])){
  # Add first date
  outcome_matrix[i,2] <- as.character(ordered_dates[i,2])
  
  # Add first drug
  outcome_matrix[i,3] <- as.character(ordered_drugs[i,2])
  
  # Add second date (make sure that its not the identical date as the first date)
  if (ordered_dates[i,2]!=ordered_dates[i,3]){
    outcome_matrix[i,4] <- as.character(ordered_dates[i,3])
    outcome_matrix[i,5] <- as.character(ordered_drugs[i,3])
  } else { 
    for (j in (4:dim(ordered_dates)[2])){
      if (ordered_dates[i,j]!=ordered_dates[i,2]){
        outcome_matrix[i,4] <- as.character(ordered_dates[i,j])
        outcome_matrix[i,5] <- as.character(ordered_drugs[i,j])
        break 
      }
    }
    }
  # Add interval (days)
  outcome_matrix[i,6] <- as.period(interval(outcome_matrix[i,2], outcome_matrix[i,4]), unit="day")$day
  
  # Add interval (weeks)
  outcome_matrix[i,7] <- (as.period(interval(outcome_matrix[i,2], outcome_matrix[i,4]), unit="day")$day)/7
  
  # Label outcome
  if ((as.numeric(outcome_matrix[i,7]) >= weekthreshold)|((outcome_matrix[i,3]==outcome_matrix[i,5])&(as.numeric(outcome_matrix[i,7])>=weekthreshold))){
    outcome_matrix[i,8] <- 1
  } else {
    outcome_matrix[i,8] <- 0
  }
  print(paste(i, "/", dim(outcome_matrix)[1]))
}

# Write the outcome matrix 
write.csv(outcome_matrix, file="~/Desktop/DBMI/Research/Pharmacogenomics/Data/PipelineResults/8_OutcomeLabels_210217_12Weeks.csv", row.names=F)

# Turn each column into numeric values 
numeric_outcome_matrix <- matrix(0, nrow=dim(outcome_matrix)[1], ncol=4)
colnames(numeric_outcome_matrix) <- c("STUDY_ID", "Interval(Days)", "Interval(Weeks)", "Outcome")
numeric_outcome_matrix[,2] <- as.numeric(outcome_matrix[,6])
numeric_outcome_matrix[,3] <- as.numeric(outcome_matrix[,7])
numeric_outcome_matrix[,4] <- as.numeric(outcome_matrix[,8])
numeric_outcome_matrix[,1] <- as.numeric(outcome_matrix[,1])

# Write the outcome matrix with numeric columns
write.csv(numeric_outcome_matrix, file="~/Desktop/DBMI/Research/Pharmacogenomics/Data/PipelineResults/8_NumericOutcomeLabels_210217_12Weeks.csv", row.names=F)

