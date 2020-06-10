## Comparative Agendas Project
## Author: Rohnin Randles, University of Washington-Seattle
## Date: June 10, 2020
## 
## Purpose: The purpose of this code is to extract
## legislative information from JSON files in the ./Input/
## directory, process them and format them into a final
## CSV, and save it to the ./Output/ directory.
##
## Make sure to read included README for information on
## the specific file structure in the ./Input/ directory.
## This is VERY important, and the code will not work corretly
## if the directory is not properly formatted

library(rjson) # For parsing JSON files to List objects
library(dplyr) # For data cleaning and manipulation
library(tm) # For text-as-data functions related to creating the corpus and DocumentTermMatrix
library(randomForest) # For predicted classifications from a pre-trained Random Forest model

setwd(" ") # !IMPORTANT! Set the working directory to the location of the Comparative Agendas Project folder

# The USCongress Github identifies sponsors based on Bioguide IDs, whereas CAP uses ICPSR. This data table provides a crosswalk between these two formats
member_crosswalk <- read.csv("./Dependencies/HSall_members.csv", header = TRUE, stringsAsFactors = FALSE) 

groups <- list.files(path = "./Input/") # Get the list of all subdirectories of the ./Input/ folder

# Create a blank data frame to set the names of the columns to be addressed.
# This way we can just use names(blankDF) in the future, rather than writing out the list multiple times
blankDF <- data.frame(matrix(nrow = 0, ncol = 33))
names(blankDF) <- c("BillID",
                     "BillNum",
                     "BillType",
                     "Chamber",
                     "Cong",
                     "Cosponsor",
                     "IntrDate",
                     "Major",
                     "Minor",
                     "Mult",
                     "MultNo",
                     "PassH",
                     "PassS",
                     "PLaw",
                     "PLawDate",
                     "PLawNum",
                     "Private",
                     "ReportH",
                     "ReportS",
                     "Title",
                     "Veto",
                     "Age",
                     "DW1",
                     "DW2",
                     "MemberID",
                     "NameFirst",
                     "NameFull",
                     "NameLast",
                     "Party",
                     "PooleID",
                     "Postal",
                     "State",
                     "ImpBill")

outputDF <- blankDF # Create outputDF, which will ultimately be our final product

#######################################
#### SECTION A - BILL DATA PARSING ####
#######################################
##
##############################
## Overall program workflow ##
##############################
## Based on the structure of the ./Input/ directory,
## the code will open the first folder under ./Input/ (such as ./Input/hr, this is referred to in the code as a "cluster").
## It will then iterate through each folder in that directory,
## extract data.json, parse this file into a list and format the
## data according to the existing CBP codebook.
##############################

for(cluster in groups) {
  clusterFiles <- list.files(path = paste("./Input", cluster, sep = "/")) # Get the list of all folders within the particular cluster
  
  clusterDF <- blankDF # Create a separate dataframe to hold the bills within this cluster
  
  # Next, iterate over each folder in the cluster
  for(file in clusterFiles) {
    if("data.json" %in% list.files(path = paste("./Input", cluster, file, sep = "/"))) {
      
      # If the folder contains "data.json", create a blank dataframe to house that bill's data
      tempDF <- data.frame(matrix(nrow=1,ncol=33))
      names(tempDF) <- names(blankDF)
      
      billData <- fromJSON(file = paste("./Input", cluster, file, "data.json", sep = "/")) # Read the JSON file and create a list object from "data.json"
      
      tempDF$BillID <- paste(billData$congress, billData$bill_type, billData$number, sep = "-") # Format BillId as string in the form "Congress-BillType-BillNumber"
      tempDF$BillNum <- as.integer(billData$number)
      tempDF$BillType <- billData$bill_type
      tempDF$Chamber <- (substr(billData$bill_type,1,1) == "s") # Chamber = 0 if first character of bill_type is "h", 1 if "s"
      tempDF$Cong <- billData$congress
      # To save on repeating this later, the following index stores the row in member_crosswalk pertaining to the bill's sponsor
      sponsorIndex <- which(member_crosswalk$congress == tempDF$Cong & member_crosswalk$bioguide_id == billData$sponsor$bioguide_id) 
      tempDF$Cosponsor <- length(unique(billData$cosponsors)) # The number of cosponsors is equal to the length of the list's cosponsors vector, wrapping unique for redundancy
      tempDF$IntrDate <- billData$introduced_at
      
      refList <- c() # This vector will be used to hold all referring committees
      if(length(billData$committees) > 0)
        # If the list object has at least one committee, iterate through each
        # For each committee that has "referral" listed in the activity field, 
        # append it to the refList vector, wrapping unique to remove duplicate values
        for(n in 1:length(billData$committees))
          if("referral" %in% billData$committees[[n]]$activity)
            refList <- unique(c(refList, billData$committees[[n]]$committee))
      
      # Count the number of House and Senate committees in refList
      # If there is more than 1 of either, count this bill as a multiple referral
      # The number of referrals is set to the maximum number of referrals betweent
      # the House and the Senate
      ##
      ## NOTE ON STYLE: The MultNo field was chosen to be the MAXIMUM of the two
      ## houses because the field is supposed to represent the number of 
      ## "multiple referrals" (per the codebook). 
      ## To instead represent the TOTAL number of referrals, use the commented line (115)
      tempDF$Mult <- (sum(grepl("House", refList)) > 1 | sum(grepl("Senate", refList)) > 1) 
      tempDF$MultNo <- max(sum(grepl("House", refList)), sum(grepl("Senate", refList)) > 1)
      # tempDF$MultNo <- length(refList)
      
      # Set PassH and PassS to FALSE by default
      tempDF$PassH <- FALSE
      tempDF$PassS <- FALSE
      # Iterate through each action in the list
      # If there is at least one action containing both "PASS" and "HOUSE"/"SENATE",
      # then set the respective boolean to TRUE
      for(n in 1:length(billData$actions)) {
        if(sum(grepl("PASS.*HOUSE", billData$actions[[n]]$status)) > 0)
          tempDF$PassH <- TRUE
        if(sum(grepl("PASS.*SENATE", billData$actions[[n]]$status)) > 0)
          tempDF$PassS <- TRUE
      }
      
      tempDF$PLaw <- length(billData$enacted_as) > 0 # For bills that did NOT become law, "enacted_as" will be NULL (list of length 0). Thus, if length > 0, the bill has a PublicLaw code
      tempDF$PLawDate <- if(tempDF$PLaw) { billData$status_at } # Get the date corresponding to the final status update. For bills that become law, this will be the PublicLaw date
      tempDF$PLawNum <- if(tempDF$PLaw) { paste(billData$congress, billData$enacted_as$number, sep="-") } # Format PLawNum as "Congress-PLawNumber"
      
      # By default, set ReportH and ReportS to TRUE only if the bill passed that house
      # AND it was referred to at least one committee.
      # If a bill proceeded directly to floor consideration,
      # it did not get reported
      tempDF$ReportH <- (tempDF$PassH & sum(grepl("House", refList)) > 1)
      tempDF$ReportS <- (tempDF$PassS & sum(grepl("Senate", refList)) > 1)
      if(length(billData$committees) > 0)
        # Iterate through each committee
        # If there is at least one committee that has "reporting" listed in the activity field,
        # set ReportH/ReportS to TRUE
        for(n in 1:length(billData$committees)) {
          if("reporting" %in% billData$committees[[n]]$activity & substr(billData$committees[[n]]$committee,1,1) == "H")
            tempDF$ReportH <- TRUE
          if("reporting" %in% billData$committees[[n]]$activity & substr(billData$committees[[n]]$committee,1,1) == "S")
            tempDF$ReportS <- TRUE
        }
      
      tempDF$Title <- billData$titles[[1]]$title # The first title in the list is always the "Legal Title"
      tempDF$Veto <- substr(billData$status,1,4) == "VETO" # TRUE if the status field starts with "VETO"
      tempDF$Age <- as.integer(substr(tempDF$IntrDate,1,4)) - member_crosswalk$born[sponsorIndex][1] # Get sponsor age by subtracting year of introduction from sponsor birth year (from member_crosswalk)
      tempDF$DW1 <- member_crosswalk$nominate_dim1[sponsorIndex][1]
      tempDF$DW2 <- member_crosswalk$nominate_dim2[sponsorIndex][1]
      tempDF$PooleID <- member_crosswalk$icpsr[sponsorIndex][1]
      tempDF$Party <- member_crosswalk$party_code[sponsorIndex][1]
      tempDF$MemberID <- paste(tempDF$PooleID, tempDF$Cong, tempDF$Party, sep = "-") # Format MemberID as String in the form of "PooleID-Congress-PartyCode"
      tempDF$Postal <- member_crosswalk$state_abbrev[sponsorIndex][1]
      tempDF$State <- member_crosswalk$state_icpsr[sponsorIndex][1]
      
      # The sponsor name field contains ONLY the full name of the sponsor
      tempDF$NameFirst <- gsub(" .*", "", gsub(".*, ", "", billData$sponsor$name)) # Select only characters between ',' and the next following space (to eliminate the inclusion of middle names)
      tempDF$NameFull <- billData$sponsor$name
      tempDF$NameLast <- gsub(",.*", "", billData$sponsor$name) # Remove all characters after ','
      
      # TRUE if the legal title contains any of the listed terms (list found in codebook)
      tempDF$ImpBill <- grepl("medal|coin|name|technical correction|stamp|land exchange|suspend temporarily|extend the temporary|boundar|exchange of land", tolower(tempDF$Title))
      
      clusterDF <- bind_rows(clusterDF, tempDF) # Bind this bill's data to the dataframe containing the cluster and move to the next bill
    }
  }
  outputDF <- bind_rows(outputDF, clusterDF) # Once an entire cluster is finished, bind the cluster to the final dataframe and move to the next cluster
}

#####################################################
#### SECTION B - MACHINE LEARNING CLASSIFICATION ####
#####################################################

topicSubset <- outputDF %>% filter(BillType %in% c("hjres", "hr", "s", "sjres")) # We only assign topics to these bill types, so filter out the rest

trainedDTM <- readRDS("./Dependencies/trainedDTM_major.rds") # Load the trained DocumentTermMatrix
randomForest_classifierM <- readRDS("./Dependencies/rf_major.rds") # Load the RandomForest classifier object for Major topics
randomForest_classifierP <- readRDS("./Dependencies/rf_private.rds") # Load the RandomForest classifier object for Private issue labeling

# This code labels topics in two places, in the main output dataframe without probability data,
# and in a separate CSV with probability data included for manual inspection

# Here we create two identical dataframes, one is an interim dataframe
# that will hold the major topics, and the second
# is the full dataframe used for output
topicsDF <- data.frame(matrix(nrow = nrow(topicSubset), ncol = 6)) 
names(topicsDF) <- c("BillID",
                     "Major",
                     "MajorProb",
                     "Minor",
                     "MinorProb",
                     "Private")

outputTopics <- data.frame(matrix(nrow = 0, ncol = 6))
names(outputTopics) <- names(topicsDF)

topicsDF$BillID <- topicSubset$BillID

mlData <- topicSubset %>% select(Title) # Get the list of bill titles, to be used for classification

corpus <- VCorpus(VectorSource(mlData$Title)) # Create a Volatile Corpus from the titles
corpus <- tm_map(corpus, content_transformer(tolower)) # Transform the text to lowercase
corpus <- tm_map(corpus, removeNumbers) # Remove numbers
corpus <- tm_map(corpus, removePunctuation) # Remove punctuation
corpus <- tm_map(corpus, removeWords, stopwords()) # Remove stop words
corpus <- tm_map(corpus, stemDocument) # Stem all words (e.g. "running" -> "run" + "running")
corpus <- tm_map(corpus, stripWhitespace) # Strip all whitespace

# Create a DocumentTermMatrix from the corpus, using the specific terms
# from the training data created by mlCode.R
# A DocumentTermMatrix is a matrix where each row is an observation (title),
# and each column is a specific character string. The value in each cell
# represents the number of times that string appears in the title
dtm <- DocumentTermMatrix(corpus, control = list(dictionary = Terms(trainedDTM)))
documentFrame <- as.data.frame(as.matrix(dtm)) # Convert matrix to dataframe

predictedTopics <- predict(randomForest_classifierM, documentFrame) # Predict the most likely classification for each observation, based on the pre-trained RandomForest model
topicsDF$Major <- as.numeric(levels(predictedTopics))[predictedTopics] # Classification predictions are categorical, but CBP data uses integer topics
probFrame <- as.data.frame(predict(randomForest_classifierM, documentFrame, type = "prob")) # Produce a data.frame containing the probabilities for each topic and observation
topicsDF$MajorProb <- do.call(pmax, probFrame) # The predict() function assigns the most likely classification, so the probability associated with each prediction is the maximum predicted probability

predictedPrivate <- predict(randomForest_classifierP, documentFrame) # Predict the classification for private issues, based on the pre-trained RandomForest model
topicsDF$Private <- as.numeric(levels(predictedPrivate))[predictedPrivate] # Classification predictions are categorical, but CBP data uses integers for Private issue

# For each major topic, filter for only the bills in that major topic.
# Next, get the RandomForest model that has been trained specifically
# for that major topic and predict the subtopic classification (with probabilities)

for(topic in unique(topicsDF$Major)) {
  # Subset the topicsDF dataframe for the given major topic
  subTopicDF <- topicsDF %>% filter(Major == topic)
  # Major topics 9 and 23 do not have subtopics, and major topic 99 is for Other, so we do not need to handle those
  if(!(topic %in% c(9,23,99))) {
    randomForest_classifierS <- readRDS(paste("./Dependencies/rf_subNo", topic, ".rds", sep = "")) # Load RandomForest model
    
    subFrame <- documentFrame[which(topicsDF$Major == topic),] # Get the corresponding observations from our DocumentTermMatrix dataframe
    predictedTopics <- predict(randomForest_classifierS, subFrame) # Predict the most likely subtopic classification for each observation
    subTopicDF$Minor <- as.numeric(levels(predictedTopics))[predictedTopics] # Classification predictions are categorical, but CBP data uses integer subtopics
    # As above with major topics, get the maximum probability for subtopic classification
    probFrame <- as.data.frame(predict(randomForest_classifierS, subFrame, type = "prob")) 
    subTopicDF$MinorProb <- do.call(pmax, probFrame)
  } else {
    # If we are dealing with topics 9,23,99 append "00" to the end and set subtopic probability to 1
    subTopicDF$Minor <- as.integer(topic) * 100
    subTopicDF$MinorProb <- 1
  }
  
  outputTopics <- bind_rows(outputTopics, subTopicDF) # Bind the completed dataframe to the outputTopics dataframe and move to the next major topic
}           

outputTopics <- outputTopics %>% arrange(BillID) # Sort outputTopics by BillID

# Write the outputTopics dataframe to the ./Output/ directory, using the format "compiledTopics-YYYY_MM_DD.csv"
write.csv(outputTopics, paste("./Output/compiledTopics-", gsub("-", "_", Sys.Date()), ".csv", sep = ""), row.names = FALSE)

# Iterate through each bill in outputTopics, and assign the values for topics, subtopics, and private issue in the main output dataframe
for(i in 1:nrow(outputTopics)) {
  outputDF$Major[which(outputDF$BillID == outputTopics$BillID[i])] <- outputTopics$Major[i]
  outputDF$Minor[which(outputDF$BillID == outputTopics$BillID[i])] <- outputTopics$Minor[i]
  outputDF$Private[which(outputDF$BillID == outputTopics$BillID[i])] <- (outputTopics$Private[i] == 1)
}

# For bills that don't match one of the bill types for topic classification, set topic and subtopic to NA
outputDF$Major[which(!(outputDF$BillType %in% c("hr", "hjres", "s", "sjres")))] <- NA
outputDF$Minor[which(!(outputDF$BillType %in% c("hr", "hjres", "s", "sjres")))] <- NA

# Write the main dataframe to the ./Output/ directory, using the format "compiled-YYYY_MM_DD.csv"
write.csv(outputDF, paste("./Output/compiled-", gsub("-", "_", Sys.Date()), ".csv", sep=""), row.names = FALSE)
