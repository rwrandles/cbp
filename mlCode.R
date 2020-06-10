## Comparative Agendas Project
## Author: Rohnin Randles, University of Washington-Seattle
## Date: June 10, 2020
##
## Purpose: The purpose of this code is to train
## the supervised machine learning models used for predictive
## classification in main.R
##
## !!NOTE!! It is HIGHLY recommended to run this code
## on a machine with at least 8GB of memory.
## This code creates multiple very large text objects and
## can overflow system memory. I have done my best to optimize
## memory usage throughout the code, but RStudio may sometimes
## fail to be able to allocate a particular object, causing an error
##
## For reference purposes, this code was benchmarked on
## - Windows 10 Home Version 18363
## - 16.0GB Memory
## - 4.12GHz CPU, 12 cores
## - R Version 3.6.2

library(randomForest) # For randomForest training
library(caTools) 
library(rvest)
library(tm) # For text-as-data functions related to creating the corpus and DocumentTermMatrix
library(SnowballC)
library(dplyr) # For data cleaning and manipulation

setwd(" ") # !IMPORTANT! Set the working directory to the location of the Comparative Agendas Project folder

cbp_data <- read.csv("./Dependencies/bills97-114.csv", header = TRUE, stringsAsFactors = FALSE, sep = ";") # Import historical CBP data from the ./Dependencies/ directory
mlData <- cbp_data %>% filter(!is.na(Major), !is.na(Minor), Cong >= 108) %>% select(Title, Major, Minor, Private) # Filter out bills before 108th Congress, as well as those that are missing major or minor topics
rm(cbp_data) # Remove cbp_data
gc() # Flush memory

corpus <- VCorpus(VectorSource(mlData$Title)) # Create a Volatile Corpus from the titles
corpus <- tm_map(corpus, content_transformer(tolower)) # Transform the text to lowercase
corpus <- tm_map(corpus, removeNumbers) # Remove numbers
corpus <- tm_map(corpus, removePunctuation) # Remove punctuation
corpus <- tm_map(corpus, removeWords, stopwords()) # Remove stop words
corpus <- tm_map(corpus, stemDocument) # Stem all words (e.g. "running" -> "run" + "running")
corpus <- tm_map(corpus, stripWhitespace) # Strip all whitespace

# Create a DocumentTermMatrix from the corpus
# A DocumentTermMatrix is a matrix where each row is an observation (title),
# and each column is a specific character string. The value in each cell
# represents the number of times that string appears in the title
dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.9996) # Remove all but the top 0.04% most frequent terms. By default, this amounts to ~2400 terms

saveRDS(dtm, "./Dependencies/trainedDTM_major.rds") # Save the DocumentTermMatrix to the ./Dependencies/ directory
rm(corpus) # Remove corpus
gc() # Flush memory

data <- as.data.frame(as.matrix(dtm)) # Convert matrix to dataframe
data$Major <- factor(mlData$Major) # Convert major topic field to factor
data$Minor <- factor(mlData$Minor) # Convert minor topic field to factor
data$Private <- factor(mlData$Private) # Convert private issue field to factor
rm(dtm) # Remove DocumentTermMatrix
gc() # Flush memory

sample <- sample.split(data$Major, SplitRatio = 0.75) # Split the sample data into training and test data using a 75/25 ratio

# Create training and test dataframes
training <- subset(data, sample == TRUE) %>% select(-Minor)
training$Major <- factor(training$Major)
test <- subset(data, sample == FALSE) %>% select(-Minor)
test$Major <- factor(test$Major)

# Train RandomForest models based on training data, using 100 trees and trace output set to TRUE
# Note on parameters: Forest depth is directly correlated to predictive accuracy and length of training time.
# One should be careful to avoid underfitting the model for the sake of speed, while also acknowledging the
# rapidly diminishing returns from adding more trees. Setting do.trace to TRUE will produce an output in the console
# containing OOB error rates for each tree. I have this set as a personal preference, as it allows me to gauge
# the progress being made. Feel free to set this to FALSE if preferred
#
# For a further discussion on the depth of random forests, see Oshiro et. al (2012)
#
## NOTE ON BENCHMARKS: Training RandomForest objects is not a speedy process
## With default settings, the major topic model was trained in ~90 minutes.
## Given their smaller input sizes, the remaining models were trained in ~15-20 minutes.

rf_major <- randomForest(training[,setdiff(names(training), c("Major", "Private"))], y = training$Major, ntree = 100, do.trace = TRUE)
rf_private <- randomForest(training[,setdiff(names(training), c("Major", "Private"))], y = training$Private, ntree = 100, do.trace = TRUE)

# Save the major topic and private issue RandomForest models to the ./Dependencies/ directory
saveRDS(rf_major, "./Dependencies/rf_major.rds")
saveRDS(rf_private, "./Dependencies/rf_private.rds")

predictedTopics <- predict(rf_major, test) # Get classifier predictions for the test sample

# Report successful completion and out-of-sample accuracy
print("RandomForest classifier for Major Topics has been built!")
print(paste("Model predictive accuracy: ", sum(predictedTopics == test$Major), "/", length(predictedTopics), " = ", mean(predictedTopics == test$Major) * 100, "%", sep = ""))

# Iterate through each major topic
# Filter the sample data for bills labeled
# as the given major topic
# Proceed with training on the subtopic field

correct <- 0
count <- 0
for(topic in sort(unique(mlData$Major))) {
  # Major topics 9 and 23 do not have subtopics, and major topic 99 is for Other, so we do not need to handle those
  if(!(topic %in% c(9,23,99))) {
    data_sub <- data %>% filter(Major == topic) # Filter data for the given major topic
    
    sample <- sample.split(data_sub$Minor, SplitRatio = 0.75) # As before, create training and test data with a 75/25 split
    
    training_sub <- subset(data_sub, sample == TRUE) %>% select(-Major)
    training_sub$Minor <- factor(training_sub$Minor)
    test_sub <- subset(data_sub, sample == FALSE) %>% select(-Major)
    test_sub$Minor <- factor(test_sub$Minor)
    
    rf_sub <- randomForest(training_sub[,setdiff(names(training_sub), "Minor")], y = training_sub$Minor, ntree = 100, do.trace = TRUE) # Train the RandomForest model for minor topics, using the same parameters as above
    saveRDS(rf_sub, paste("./Dependencies/rf_subNo", topic, ".rds", sep = "")) # Save the minor topic model to the ./Dependencies/ directory, using the format "rf_subNoXX.rds", where XX represents the given major topic filter
    
    predictedTopics <- predict(rf_sub, test_sub) # Get classifier predictions for the test sample
    
    # Report succesful completion and out-of-sample accuracy
    correct <- correct + sum(predictedTopics == test_sub$Minor)
    count <- count + length(predictedTopics)
    
    print(paste("RandomForest classifier for Subtopic #", topic, " has been built!", sep = ""))
    print(paste("Model predictive accuracy: ", sum(predictedTopics == test_sub$Minor), "/", length(predictedTopics), " = ", mean(predictedTopics == test_sub$Minor) * 100, "%", sep = ""))
  }
}

# Report succesful completion of all subtopic models and report aggregate predictive accuracy for minor topic classification
print("RandomForest classifiers for Subtopics have been built!")
print(paste("Subtopic predictive accuracy: ", correct, "/", count, " = ", correct/count * 100, "%", sep = ""))
