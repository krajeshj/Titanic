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
 

# we read it in 
titanic <- read_csv("titanic_original.csv", n_max= 1309)

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
 
titanic$embarked[is.na(titanic$embarked)]<-"S"


# age

# calculate the mean age of the remaining passengers
print( paste(" Mean age is ", mean(titanic$age,na.rm = TRUE)))
age_mean <-mean(titanic$age,na.rm = TRUE)

# let's update the  missing entries with mean age
 titanic$age[is.na(titanic$age)]<-mean(titanic$age,na.rm = TRUE)

 # Let's clean up the boat column

miss_boat_idx <- as.vector(which(is.na(titanic$boat)))
titanic$boat[miss_boat_idx] <-"None"
 

# If we have a cabin number, it is likely the passenger survived to give the information
# Others with None may imply passenger did not survive
# Lets add a variable using mutate
 
titanic <- titanic %>% mutate(has_cabin_number = ifelse(is.na(titanic$cabin),0,1))
str(titanic)
                                          
# Use ggplot() for the first instruction
# There were more men passengers than women
# A mojority of them were pclass 3 passengers
ggplot(titanic, aes(x = factor(pclass), fill = factor(sex))) + geom_bar( position = "dodge")


# Use ggplot() for the second instruction
# There were more men who died than women
# Most men and women that died were travelling as pclass 3 passengers

ggplot(titanic, aes(x = factor(pclass), fill = factor(sex))) + geom_bar( position = "dodge") + facet_grid( . ~ survived, labeller = label_both)

# Position jitter (use below)
posn.j <- position_jitter(0.5, 0)

# Use ggplot() for the last instruction
# This chart highlights age and survivorship
# Far greater young men perished(green) than women( red)
# pclass 3 passengers perished the most
# pclass 1 passengers survived the most and were mostly women

ggplot(titanic, aes(x = factor(pclass), y = age,  col  = factor(sex))) + geom_jitter(alpha = 0.5, size= 3, position = posn.j) + facet_grid( . ~ survived, labeller=label_both)

# Use ggplot() for the last instruction

# This is a facet of survivorship and having a cabin number
# More first class passengers  seem to have cabin  numbers
# More people with cabin numbers seem to have survived to provide the cabin number info
ggplot(titanic, aes(x = factor(pclass), y = age,  col  = factor(survived), shape = as.factor(has_cabin_number))) + geom_jitter(alpha = 0.5, size= 3, position = posn.j) + facet_grid( . ~ has_cabin_number, labeller = label_both)

 

# Lets fix the one NA in fare 
titanic$fare[is.na(titanic$fare)]<-mean(titanic$fare,na.rm = TRUE)

# Write out the cleaned data fraome into a csv file
write_csv(titanic,"titanic_clean.csv", append = FALSE, col_names=TRUE)
 