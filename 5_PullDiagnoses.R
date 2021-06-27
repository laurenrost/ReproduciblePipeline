# Description: This code pulls out the number of times that the top 200 MDD ICD-9/-10 diagnosis codes occur in 
# patients' EHR record.

# Read in diagnosis file
filepath <- "~/Desktop/DBMI/Research/Pharmacogenomics/Data/EHR/R3_2027_ROST_DIAGNOSES_2020_12_14.csv"
d <- read.csv(filepath)

# Make a table of counts of all diagnoses across patients
dx_table <- table(d$DX_NAME)
dx_table <- dx_table[rev(order(dx_table))]

# Pull top 200 diagnoses
top_dx <- names(dx_table[1:200])

# Pull STUDY_IDs that pass 2/30/180 rule
filepath <- "~/Desktop/DBMI/Research/Pharmacogenomics/Data/PipelineResults/4_TwoThirty180_PassFail_210208.csv"
eMERGE_STUDYIDs <- read.csv(filepath)
eMERGE_STUDYIDs_2 <- eMERGE_STUDYIDs[(which(eMERGE_STUDYIDs[,2]==1)),1]

# Only include STUDY_ID in Diagnoses that pass 2/30/180 rule
filteredd <- d[which(d$STUDY_ID %in% eMERGE_STUDYIDs_2),]

# Check whether dx code is present in original EHR table 
checkdxpresence <- matrix(0, nrow=length(top_dx), ncol=2)
checkdxpresence[,1] <- top_dx
for (i in seq_along(1:length(top_dx))){
  checkdxpresence[i,2] <- sum(grepl(checkdxpresence[i,1], filteredd[,4], useBytes = T))
  print(i)
}
top_dx <- top_dx[-which(as.numeric(checkdxpresence[,2])==0)]

# Make a matrix to output antidepressant ordered
dxs <- matrix(0, nrow=length(eMERGE_STUDYIDs_2), ncol=(length(top_dx)+1))

# Name the column based on antidepressant
colnames(dxs) <- c("STUDY_ID", top_dx)

# Name the rows based on STUDY_ID
dxs[,1] <- eMERGE_STUDYIDs_2
rownames(dxs) <- eMERGE_STUDYIDs_2


for (i in (2:(dim(dxs)[2]))){
  
  # Search for particular antidepressant in Orders table
  # codepresence <- t[which(grepl(colnames(incl_icd9_icd10)[i], t[,3])),1]
  diagpresence <- filteredd[which(grepl(colnames(dxs)[i], filteredd[,4], useBytes=T)),1] 
  
  # Find the unique individuals
  howmanyuniqppl <- length(unique(diagpresence))
  
  # Table the number of times the code comes up for an individual
  tabled <- table(diagpresence)
  
  # For all unique STUDY_ID with presence of inclusion code,  
  for (j in seq_along(1:howmanyuniqppl)){
    
    # Find the patient's row in the data collection matrix
    rowtoplace <- which(rownames(dxs)==names(tabled[j])[[1]])
    
    # Assign value of the number of times found the inclusion code to the patient's row and column
    dxs[rowtoplace,i] <- tabled[[j]] 
    
    # Status check
    print(paste(i, ":", dim(dxs)[1]))
  }
  
  if (grepl(",", colnames(dxs)[i])==T){
    colnames(dxs)[i] <- gsub(",", "", colnames(dxs)[i])
  }
  
  # Write file iteratively
  dxs_append <- dxs[,1:i]
  write.csv(dxs_append,"~/Desktop/DBMI/Research/Pharmacogenomics/Data/PipelineResults/5_DiagnosisPresence_210619.csv", 
            row.names = FALSE, 
            quote=F)
}


