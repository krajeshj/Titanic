# Goal:         (1) Fix missing values
#               (2) Fix data structures - add new variables
#               (3) Save new cleaned data sets
#

library(dplyr)
library(RSQLite)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(scales)
library(mixtools)
library(readxl)

# For fun. Let's do some eda 

# What are the files in the dir 

dir()

# list the sheets in the xl file 
excel_sheets("titanic3.xls")

# lets read it in
#titanic <- read_excel("titanic3.xls") 
# scratch that we need to read it in as csv
# saved .xls as .csv

# we read it in 
titanic <- read_csv("titanic_original.csv", n_max= 1309)
# Reading in directly as above :  could not locate the missing people with Embarkment  missing
# by default na = c("",NA)
# So I allow characters to be read in : so that I can wrangle it myself
# titanic <- read_csv("titanic_original.csv", na = character())


# titanic is avaliable in your workspace

# Check out the structure of titanic
str(titanic)

# In embarked col find missing entries
# I begin by seeing if "" appears sure does  if I read in the csv with  na = character()
levels(as.factor(titanic$embarked))

# Now locate the missing embarkers
missing_embarked_idx<- as.vector(which(is.na(titanic$embarked)))
missing_embarked_idx
str(missing_embarked_idx)

# print the missing ones 
for (entry in  missing_embarked_idx) {
  print( paste("here are are the missing embarkers indices", entry))  
  print( paste("here are their names ",                      titanic$name[entry]))
  print( paste("here are their embarking status ",           titanic$embarked[entry]))
  }

# Oops there are only 2 people the third record is all NAs - probabbly final row
# so go back to read_csv and add n_max = 1309

# lets substitute their embarking status to "S"
# examine the records 
#titanic[as.vector(missing_embarked_idx),] 
#titanic$embarked[as.vector(missing_embarked_idx)] <-"S"

# After doing this I am painfully aware that I can simply say 
titanic$embarked[is.na(titanic$embarked)]<-"S"

for (entry in  missing_embarked_idx) {
  print(paste("here are are the missing embarkers indices", entry))  
  print(paste("here are their names ", titanic$name[entry]))
  print(paste("here are their Cleaned embarking status ", titanic$embarked[entry]))
}



# age

# now locate the records with missing Age
missing_age_idx <- as.vector(which(is.na(titanic$age)))
for (entry in  missing_age_idx) {
  print(paste("Idx", entry, "names ", titanic$name[entry],"age", titanic$age[entry] ))  
  
}

print( paste(" Mean age is ", mean(titanic$age,na.rm = TRUE)))

age_mean <-mean(titanic$age,na.rm = TRUE)
# let's update the  missing entries with mean age
 
titanic$age[is.na(titanic$age)]<-mean(titanic$age,na.rm = TRUE)

 
for (entry in  missing_age_idx) {
   print(paste("Idx", entry, "name ", titanic$name[entry], "Age", titanic$age[entry]))
 }

# Let's clean up the boat column

miss_boat_idx <- as.vector(which(is.na(titanic$boat)))
 
titanic$boat[miss_boat_idx] <-"None"

 # 
titanic$boat

# If we have a cabin number, it is likely the passenger survived to give the information
# Others with None may imply passenger did not survive
# Lets add a variable using mutate
 
titanic <- titanic %>% mutate(has_cabin_number = as.numeric(!(is.na(cabin) == TRUE)))

                                          
# Use ggplot() for the first instruction
ggplot(titanic, aes(x = factor(pclass), fill = factor(sex))) + geom_bar( position = "dodge")


# Use ggplot() for the second instruction?>
ggplot(titanic, aes(x = factor(pclass), fill = factor(sex))) + geom_bar( position = "dodge") + facet_grid( . ~ survived)

# Position jitter (use below)
posn.j <- position_jitter(0.5, 0)

# Use ggplot() for the last instruction
# Most young male passengers died and were from pclass 3
# most survivors were women and older
#
ggplot(titanic, aes(x = factor(pclass), y = age,  col  = factor(sex))) + geom_jitter(alpha = 0.5, size= 3, position = posn.j) + facet_grid( . ~ survived)

# Use ggplot() for the last instruction
# female passengers with a cabin number
# survived  to provide a cabin number
ggplot(titanic, aes(x = factor(pclass), y = age,  col  = factor(sex), shape = as.factor(has_cabin_number))) + geom_jitter(alpha = 0.5, size= 3, position = posn.j) + facet_grid( . ~ survived)

# Use ggplot() for the last instruction
# first class seem to have cabin  numbers
# Of the people who survived  most were from 1st class  and were able to provide cabin numbers
ggplot(titanic, aes(x = factor(pclass), y = age,  col  = factor(has_cabin_number))) + geom_jitter(alpha = 0.5, size= 3, position = posn.j) + facet_grid( . ~ survived)


# Lets fix the one NA in fare 
titanic$fare[is.na(titanic$fare)]<-mean(titanic$fare,na.rm = TRUE)

# Write out the cleaned data fraome into a csv file
write_csv(titanic,"titanic_clean.csv", append = FALSE, col_names=TRUE)
 