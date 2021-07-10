# Description: This code pulls out the number of antidepressants prescribed by patients,
# with each STUDY_ID (that passed 2/30/180 rule) as rows, and antidepressants as columns.
# The matrix is populated with the number of times the antidepressant appears in the patients' EHR.
require(lubridate)
require("readxl")

# Orders
filepath <- "~/ORDERS.csv"
w <- read.csv(filepath)

# Fills
#filepath <- "~/FILLS.csv"
#v <- read.csv(filepath)

filepath <- "~/Inclusion_Meds.xlsx"
filepath <- "~/MasterDrugList.xlsx"
incl_meds_2 <- read_excel(filepath)
incl_meds <- as.matrix(incl_meds)
incl_meds <- toupper(paste0(incl_meds, "*"))
# Remove meds that were never found
incl_meds <- incl_meds[-c(2, 5, 7, 12, 13, 20, 22, 24, 25, 26, 28, 30, 32, 33, 35, 39, 40, 42, 43, 44, 51, 63, 64, 82, 83, 84, 85, 95, 107, 114, 116, 118, 119, 120, 121, 123, 124, 125, 130, 131, 133, 134, 135, 136, 138, 140, 142, 145, 150, 151, 153, 155, 156, 157, 158, 160, 161)]
incl_meds <- incl_meds[-which(as.numeric(checkdrugpresence[,2])==0)]

# Pull STUDY_IDs that pass 2/30/180 rule
filepath <- "~/4_TwoThirty180_PassFail.csv"
eMERGE_STUDYIDs <- read.csv(filepath)
eMERGE_STUDYIDs_2 <- eMERGE_STUDYIDs[(which(eMERGE_STUDYIDs[,2]==1)),1]

# Make a matrix to output antidepressant ordered
drugs <- matrix(0, nrow=length(eMERGE_STUDYIDs_2), ncol=(length(incl_meds)+1))

# Name the column based on antidepressant
colnames(drugs) <- c("STUDY_ID", incl_meds)

# Name the rows based on STUDY_ID
drugs[,1] <- eMERGE_STUDYIDs_2
rownames(drugs) <- eMERGE_STUDYIDs_2

# Only include STUDY_ID in Orders that pass 2/30/180 rule
filteredw <- w[which(w$STUDY_ID %in% rownames(drugs)),]

# To find drugs that were never in Orders
checkdrugpresence <- matrix(0, nrow=length(colnames(drugs)[2:dim(drugs)[2]]), ncol=2)
checkdrugpresence[,1] <- colnames(drugs)[2:dim(drugs)[2]]
for (i in seq_along(1:length(colnames(drugs)[2:dim(drugs)[2]]))){
  checkdrugpresence[i,2] <- sum(grepl(checkdrugpresence[i,1], filteredw[,3]))
  print(i)
}

for (i in (2:(dim(drugs)[2]))){

  # Search for particular antidepressant in Orders table
  drugpresence <- filteredw[which(grepl(colnames(drugs)[i], filteredw[,3])),1] 
  
  # Find the unique individuals
  howmanyuniqppl <- length(unique(drugpresence))
  
  # Table the number of times the code comes up for an individual
  tabled <- table(drugpresence)
  
  # For all unique STUDY_ID with presence of inclusion code,  
  for (j in seq_along(1:howmanyuniqppl)){
    
    # Find the patient's row in the data collection matrix
    rowtoplace <- which(rownames(drugs)==names(tabled[j])[[1]])
    
    # Assign value of the number of times found the inclusion code to the patient's row and column
    drugs[rowtoplace,i] <- tabled[[j]] 
    
    # Status check
    print(paste(i, j, ":", howmanyuniqppl))
  }
  
  # Write file iteratively
  drugs_append <- drugs[,1:i]
  write.csv(drugs_append,"~/5_AntidepressantPresence.csv", 
            row.names = FALSE, 
            quote=F)
}




