# File:           Assigning_office_worker_proxy.R
# Date:           2024-11-07
# Author:         Annika Tillander annika.tillander@liu.se
# Purpose:        R code to assign an office worker proxy 
#                 to open ended question response regarding occupation.
#                 The office worker proxy is based on SSYK12 and 
#                 the translation key to go from the four digit code related 
#                 to the occupation in SSYK12 to white collar/blue collar 
#                 provided by Statistics Sweden
#                 https://www.scb.se/hitta-statistik/statistik-efter-amne/arbetsmarknad/sysselsattning-forvarvsarbete-och-arbetstider/yrkesregistret-med-yrkesstatistik/


# Required R packages
# install.packages(c('readxl','stringdist','fuzzyjoin','stringr','dplyr','writexl'))
library(readxl)
library(stringdist)
library(fuzzyjoin)
library(stringr)
library(dplyr)
library(writexl)

# Set the working directory to the project root
# Make sure to update the path to your project root
setwd("path/to/project_root")

# Load the your data with answers on occupation 
# Ensure the data file is in the 'Data' directory
# Load the data containing an identification variable with a unique ID for 
# each response (named ID) and the response to open ended question regarding 
# occupation (named Occupation_swe). Should be in the format with observations 
# as rows variables (ID and Occupation_swe) as columns. 
# As an illustrative example 
# we use Occupation_response.xlsx 
myData <- read_excel("Data/Occupation_response.xlsx")
# Or use csv
# myData <- read.csv("data/Occupation_response.txt", sep=";")

# Remove missing values
myData_clean <- na.omit(myData, "Occupation_swe")
dim(myData_clean)
# Set all letters to lower case
myData_clean$Occupation_swe <- str_to_lower(myData_clean$Occupation_swe)
# Replace the following punctuation/symbols "." "/" and "-" with blank space " "
myData_clean$Occupation_swe <- gsub("[./-]", " ", myData_clean$Occupation_swe)
# Remove triple and double spaces
myData_clean$Occupation_swe <- gsub("\\s+", " ", myData_clean$Occupation_swe)


# Load modified SSYK12
# Ensure the data file is in the 'Data' directory
ssyk <- read_excel("Data/ssyk12_modified.xlsx")
# Or use csv
# ssyk <- read.csv("data/ssyk12_modified.txt", sep=";")

# Run fuzzy text matching and calculate the string distance using method Jaro distance  
Occupation_ssyk <- myData_clean %>%
  stringdist_left_join(ssyk, by = c("Occupation_swe" = "Occupation title"), method = "jw", distance_col = "dist")

# Filter the data.frame to only keep the row with the shortest distance for each ID
Occupation_ssyk_one <- Occupation_ssyk %>%
  group_by(ID) %>%
  filter(dist == min(dist, na.rm = TRUE))

# Create two files; 1. perfect match and 2. distance with above 0 (not a perfect match)
Occupation_ssyk_perfect_match <- Occupation_ssyk_one[Occupation_ssyk_one$dist == 0, ]
Occupation_ssyk_distance_above0 <- Occupation_ssyk_one[Occupation_ssyk_one$dist > 0, ]

# Number of identified office workers in the perfectly matched data
table(Occupation_ssyk_perfect_match$`Office worker`)

# Save the data
write_xlsx(Occupation_ssyk_perfect_match, "Data/Occupation_response_with_office_worker_proxy.xlsx")

