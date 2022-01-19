library("readxl")
library("tidyquant")
library("plotly")
library("dplyr")
library("ggridges")
library("stringr")

raw_data <- read_excel("C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/Data Preprocessing/Coded_Segments.xlsx")

keeps <- c("Document name","Code","Beginning", "End")

cleaned_raw = raw_data[keeps]

#Taking out the InVivo Codes and Why:

words_to_exclude <- c('Other', 'Others', 'Why', 'What')
vector_to_check <- c(cleaned_raw$Code)

my_regex <- regex(paste("\\b", words_to_exclude, "\\b", sep = "", collapse = "|"))

drop_indices1 <- (str_detect(vector_to_check, my_regex) * -1) + 1

drop_indices2 <- which(drop_indices1==0)

cleaned_raw <- cleaned_raw[-drop_indices2,]

all_codes <- unique(cleaned_raw$Code)

#Take off all unique codes to programmers (only want same codes):

words_to_exclude <- c('familiar', 'launched the analysis', 'Daniel', 'shows me')
vector_to_check <- c(all_codes)

my_regex <- regex(paste("\\b", words_to_exclude, "\\b", sep = "", collapse = "|"))

drop_indices1 <- (str_detect(vector_to_check, my_regex) * -1) + 1

drop_indices2 <- which(drop_indices1==0)

all_codes_cleaned <- all_codes[-drop_indices2]
all_codes_cleaned

## Go trough all participants and only keep the entries with codes from 'all_codes_cleaned':

subset_cleaned_raw <- cleaned_raw %>% filter(Code %in% all_codes_cleaned)

## Removing Wierd InVivo Code left:

cleaned_subset_cleaned_raw <- subset(subset_cleaned_raw, nchar(subset_cleaned_raw$Beginning) > 4)

cleaned_subset_cleaned_raw$Beginning <- paste("01.01.2021", cleaned_subset_cleaned_raw$Beginning, sep=" ")
cleaned_subset_cleaned_raw$End <- paste("01.01.2021", cleaned_subset_cleaned_raw$End, sep=" ")

## Download P1:

write.csv(cleaned_subset_cleaned_raw,"C:/Users/leal0/OneDrive/Desktop/How_Codes_EventLog.csv", row.names = FALSE)





