# Description: This code checks if patients pass the 2/30/180 rule.
# 2/30/180 rule: evidence of depression for at least 2 distinct calendar days, 
#                                           at least 30 days apart
#                                           and no more than 180 days apart

# Input date file
filepath <- "~/3_DepressionDates.csv"
eMERGE_STUDYIDs_dates <- read.csv(filepath, header=T)

# Create matrix for data collection
pass_fail_230180rule <- matrix(0, nrow=dim(eMERGE_STUDYIDs_dates)[1], ncol=3)
pass_fail_230180rule[,1] <- eMERGE_STUDYIDs_dates[,1]
colnames(pass_fail_230180rule) <- c("STUDYID", "Pass230180", "Fail230180")
require(lubridate)

# Capture time intervals that do not pass 2/30/180 rule
failed_230180rule_intervals <- matrix(0, nrow=dim(eMERGE_STUDYIDs_dates)[1], ncol=3)
failed_230180rule_intervals[,1] <- eMERGE_STUDYIDs_dates[,1]
colnames(failed_230180rule_intervals) <- c("STUDYID", "Inside30Interval", "Outsideof180Interval")

# 0) Check that length of dates is greater than 1
for (i in seq_along(1:dim(eMERGE_STUDYIDs_dates)[1])){
  dates <- eMERGE_STUDYIDs_dates[i,(1+which(eMERGE_STUDYIDs_dates[i,2:dim(eMERGE_STUDYIDs_dates)[2]]!="1999-01-01"))]
  if (length(dates)>2){
    dates <- unique(dates)
    # 2) Order dates
    dates <- dates[order(dates)]
    # 3) Calculate intervals between consequtive dates
    intervals <- rep(0, (length(dates)-1))
    for (j in seq_along(1:length(intervals))){
      intervals[j] <- as.period(interval(dates[[j]], dates[[j+1]]), unit="day")$day
    }
    # 4) Test that there are intervals that are greater than 30 and some that are less than 180:
    if(any((intervals>30)&(intervals<180))){
      pass_fail_230180rule[i,2] <- 1
    } else {
      pass_fail_230180rule[i,3] <- 1
      failed_230180rule_intervals[i,2] <- max(intervals[which((intervals-30)<0)])
      failed_230180rule_intervals[i,3] <- min(intervals[which((intervals-180)>0)])
    }
  } else {
    pass_fail_230180rule[i,3] <- 1
  }
  # Print progress update
  print(paste(i, "/", dim(eMERGE_STUDYIDs_dates)[1]))
}
# Write file that collects whether patients pass or fail the 2/30/180 rule
write.csv(pass_fail_230180rule,  
            file="~/4_TwoThirty180_PassFail.csv", 
            sep=',', 
            row.names=F, 
            col.names=F )
# Write file that collects the time intervals between ICD-9/-10 codes that are closest to 30 days and 180 days for patients that fail the 2/30/180 rule 
write.csv(failed_230180rule_intervals,  
          file="~/4_Failed_TwoThirty180_Intervals.csv", 
          sep=',', 
          row.names=F, 
          col.names=F )

