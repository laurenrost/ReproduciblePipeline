# Description: Check and document the presence of inclusion codes
# This code creates a matrix with each STUDY_ID as a row, and each ICD-9/10 inclusion code as columns.
# The resulting matrix is populated by the number of codes in each person's EHR. 

require("readxl")

# Input inclusion icd-10 codes
filepath <- "~/Desktop/DBMI/Research/Pharmacogenomics/Data/Final_R3_201005/Inclusion_icd10codes.v3.xlsx"
icl_icd10 <- read_excel(filepath)
icl_icd10 <- as.matrix(icl_icd10)
icl_icd10 <- gsub("-", "*", icl_icd10)

# Input inclusion icd-9 codes
filepath <- "~/Desktop/DBMI/Research/Pharmacogenomics/Data/Final_R3_201005/Inclusion_icd9codes.v3.xlsx"
icl_icd9 <- read_excel(filepath)
icl_icd9 <- as.matrix(icl_icd9)
icl_icd9[,1] <- paste0(icl_icd9[,1], "*")
icl_icd9 <- icl_icd9[-15,] # Because there were none of these codes present in Diagnosis table ("311.00*" "Depressive disorder, not elsewhere classified" )

# Input inclusion medications
filepath <- "~/Desktop/DBMI/Research/Pharmacogenomics/Data/Final_R3_201005/Inclusion_Meds_R32027.v6.xlsx"
incl_meds <- read_excel(filepath)
incl_meds <- as.matrix(incl_meds)

# Input diagnoses file to check for icd-9/10 codes in patients' records
filepath <- "~/Desktop/DBMI/Research/Pharmacogenomics/Data/EHR/R3_2027_ROST_DIAGNOSES_2020_12_14.csv"
t <- read.csv(filepath)


# Make list of unique STUDY IDs
ppl <- unique(t$STUDY_ID)

# Make a matrix to output ICD-9/10 code presence
incl_icd9_icd10 <- matrix(0, nrow=length(unique(t$STUDY_ID)), ncol=(1+dim(icl_icd9)[1]+dim(icl_icd10)[1]))

# Name the column based on ICD-9/10 code
colnames(incl_icd9_icd10) <- c("STUDY_ID", icl_icd9[,1], icl_icd10[,1])

# Name the rows based on STUDY_ID
rownames(incl_icd9_icd10) <- ppl
incl_icd9_icd10[,1] <- ppl


# For each of the inclusion ICD-9/10 codes, search for the code in the entire Diagnosis table
for (i in (2:(dim(incl_icd9_icd10)[2]))){

  # Search for particular code in Diagnosis table
  codepresence <- t[which(grepl(colnames(incl_icd9_icd10)[i], t[,3])),1]
  
  # Find the unique individuals
  howmanyuniqppl <- length(unique(codepresence))

  # Table the number of times the code comes up for an individual
  tabled <- table(codepresence)
  
  # For all unique STUDY_ID with presence of inclusion code,  
  for (j in seq_along(1:howmanyuniqppl)){
    
    # Find the patient's row in the data collection matrix
    rowtoplace <- which(incl_icd9_icd10[,1]==names(tabled[j]))[[1]]
    
    # Assign value of the number of times found the inclusion code to the patient's row and column
    incl_icd9_icd10[rowtoplace,i] <- tabled[[j]] 
    
    # Status check
    print(paste(i, j, ":", howmanyuniqppl))
    }
  
  # Write file iteratively
  incl_ppl_append <- incl_icd9_icd10[,1:i]
  write.csv(incl_ppl_append,"~/Desktop/DBMI/Research/Pharmacogenomics/Data/PipelineResults/InclusionCodeResults_210201_3.csv", 
            row.names = FALSE, 
            quote=F,
            append=F)
}



