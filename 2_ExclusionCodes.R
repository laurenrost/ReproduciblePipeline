# Description: Check and document the presence of exclusion codes
# This code creates a matrix with each STUDY_ID as a row, and each ICD-9/10 exclusion code as columns.
# The resulting matrix is populated by the number of codes in each person's EHR.

require("readxl")
# Insert file containing exclusion ICD-9 codes
filepath <- "~/Desktop/DBMI/Research/Pharmacogenomics/Data/Final_R3_201005/Exclusion_icd9codes.v4.xlsx"
excl_icd9 <- read_excel(filepath)
excl_icd9 <- as.matrix(excl_icd9)
excl_icd9[,1] <- as.numeric(excl_icd9[,1])
excl_icd9[which(is.na(excl_icd9[,1]))[1],1] <- "V11.0"
excl_icd9[which(is.na(excl_icd9[,1]))[1],1] <- "V17.0"
excl_icd9[,1] <- paste0(excl_icd9[,1], "*")

# Input file containing exclusion ICD-10 codes
filepath <- "~/Desktop/DBMI/Research/Pharmacogenomics/Data/Final_R3_201005/Exclusion_icd10codes.v4.xlsx"
excl_icd10 <- read_excel(filepath)
excl_icd10 <- as.matrix(excl_icd10)
excl_icd10[,1] <- paste0(excl_icd10[,1],"*")
excl_icd10[,1] <- gsub("-", "", excl_icd10[,1])

# Input Diagnosis table
filepath <- "~/Desktop/DBMI/Research/Pharmacogenomics/Data/EHR/R3_2027_ROST_DIAGNOSES_2020_12_14.csv"
t <- read.csv(filepath)

# Input inclusion code matrix
filepath <- "~/Desktop/DBMI/Research/Pharmacogenomics/Data/PipelineResults/InclusionCodeResults_210201_3.csv"
eMERGE_STUDYIDs <- read.csv(filepath, header=T)

# Remove STUDY IDs with less than 2 inclusion ICD-9/10 codes 
eMERGE_STUDYIDs <- eMERGE_STUDYIDs[-which(rowSums(eMERGE_STUDYIDs[2:dim(eMERGE_STUDYIDs)[2]])<2),]

# Combine ICD-9 and ICD-10 exclusion codes into one matrix
excl_icd9_icd10_names <- rbind(excl_icd9, excl_icd10)

# Make matrix to record presence of exclusion criteria
excl_icd9_icd10 <- matrix(0, nrow=dim(eMERGE_STUDYIDs)[1], ncol=(1+dim(excl_icd9_icd10_names)[1]))

# Name the columns for matrix to record presence based on ICD-9/10 code
colnames(excl_icd9_icd10) <- c("STUDY_ID", excl_icd9_icd10_names[,1])


# Eliminate ICD-9/10 exclusion code columns with no presence in the Diagnosis dataset
codepresence_2 <- matrix(0, nrow=dim(excl_icd9_icd10_names)[1], ncol=3)
codepresence_2[,1:2] <- excl_icd9_icd10_names
for (i in (2:(dim(excl_icd9_icd10)[2]))){
  codepresence <- t[which(grepl(colnames(excl_icd9_icd10)[i], t[,3])),1]
  codepresence_2[(i-1),3] <- length(codepresence)
}
# Remove columns for codes that are not present in Diagnosis table
codepresence_2 <- codepresence_2[-which(codepresence_2[,3]==0),]
# There are __ codes not present in exclusion


# Name the row based on STUDY_ID
rownames(excl_icd9_icd10) <- eMERGE_STUDYIDs[,1]

# Make STUDY_ID its own column
excl_icd9_icd10[,1] <- eMERGE_STUDYIDs[,1]

# For each of the exclusion ICD-9/10 codes, search for the code in the entire Diagnosis table
for (i in (2:(dim(excl_icd9_icd10)[2]))){
  
  # Search for particular code in Diagnosis table
  codepresence <- t[which(grepl(colnames(excl_icd9_icd10)[i], t[,3])),1]

  # Find the unique individuals
  howmanyuniqppl <- length(unique(codepresence))
  
  # Table the number of times the code comes up for an individual
  tabled <- table(codepresence)
  
  # For all unique STUDY_ID with presence of inclusion code,  
  for (j in seq_along(1:howmanyuniqppl)){
    
    if (names(tabled[j]) %in% excl_icd9_icd10[,1]){ 
      
      # Find the patient's row in the data collection matrix
      rowtoplace <- which(excl_icd9_icd10[,1]==names(tabled[j]))[[1]]
      
      # Assign value of the number of times found the inclusion code to the patient's row and column
      excl_icd9_icd10[rowtoplace,i] <- tabled[[j]] 
      }

    # Status check
    print(paste(i, j, ":", howmanyuniqppl))
  }
  
  # Write file iteratively
  excl_ppl_append <- excl_icd9_icd10[,1:i]
  write.csv(incl_ppl_append,"~/Desktop/DBMI/Research/Pharmacogenomics/Data/PipelineResults/ExclusionCodeResults_21020.csv", 
            row.names = FALSE, 
            #col.names=F, 
            quote=F,
            append=F)
}
filepath <- "~/Desktop/DBMI/Research/Pharmacogenomics/Data/PipelineResults/ExclusionCodeResults_21020.csv"
check <- read.csv(filepath)