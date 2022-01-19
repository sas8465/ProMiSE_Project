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

words_to_exclude <- c('Other', 'Others', 'Why')
vector_to_check <- c(cleaned_raw$Code)

my_regex <- regex(paste("\\b", words_to_exclude, "\\b", sep = "", collapse = "|"))

drop_indices1 <- (str_detect(vector_to_check, my_regex) * -1) + 1

drop_indices2 <- which(drop_indices1==0)

cleaned_raw <- cleaned_raw[-drop_indices2,]

all_codes <- unique(cleaned_raw$Code)

#Take off all unique codes to programmers (only want same codes):

words_to_exclude <- c('familiar', 'launched the analysis', 'Daniel', 'shows me', 'How')  #Added How here Because we only want the "what" coding
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

## Adding Times in seconds: 

time2sec <- function(x) {
  c(as.matrix(read.table(text = x, sep = ":")) %*% c(3600, 60, 1))
}

Beginning_Seconds <- transform(cleaned_subset_cleaned_raw, seconds = time2sec(cleaned_subset_cleaned_raw$Beginning))
cleaned_subset_cleaned_raw$Beginning_Seconds <- Beginning_Seconds$seconds

End_Seconds <- transform(cleaned_subset_cleaned_raw, seconds = time2sec(cleaned_subset_cleaned_raw$End))
cleaned_subset_cleaned_raw$End_Seconds <- End_Seconds$seconds

cleaned_subset_cleaned_raw$Rounded_Beginning_Seconds <- round(cleaned_subset_cleaned_raw$Beginning_Seconds, 0)
cleaned_subset_cleaned_raw$Rounded_End_Seconds <- round(cleaned_subset_cleaned_raw$End_Seconds, 0)



### Formatting Specific Participants: --------------------------------------------------------------------------------------------------------------------------------

cleaning_function <- function(Participant_name) {
  
  #Participant_name: string name.
  
  Participant_raw <- cleaned_subset_cleaned_raw[ which(cleaned_subset_cleaned_raw$`Document name`==Participant_name), ]
  Participant_raw <- Participant_raw[order(Participant_raw$Rounded_End_Seconds),]
  
  timeseries_raw <- setNames(data.frame(matrix(ncol = length((c(unique(cleaned_subset_cleaned_raw$Code)))), nrow = tail(Participant_raw$Rounded_End_Seconds, 1)+1)), c(unique(cleaned_subset_cleaned_raw$Code)))
  timeseries_raw$Time <- seq(0, tail(Participant_raw$Rounded_End_Seconds, 1), by=1)
  
  timeseries_raw <- timeseries_raw %>% relocate(Time, .before = colnames(timeseries_raw[1]))
  
  #Looping over Columns to change values:
  
  for(i in 2:ncol(timeseries_raw)) {       # for-loop over columns
    
    colname <- colnames(timeseries_raw)[i]
    
    checking_subset <- Participant_raw[ which(Participant_raw$`Code`==colnames(timeseries_raw)[i]), ]
    
    if (nrow(checking_subset) > 0) {   #Check the checking_subset is greater than zero. Ie. the participant used this code.
      
      for (i in 1:length(checking_subset$Rounded_Beginning_Seconds)) {
        
        timeseries_raw[[colname]][checking_subset$Rounded_Beginning_Seconds[i]:checking_subset$Rounded_End_Seconds[i]] = 1
        
      }
      
    }
    
  }
  
  timeseries_raw[is.na(timeseries_raw)] <- 0
  
  return(timeseries_raw)
  
}


## Repeated Analysis for Multiple Participants

P1 <- cleaning_function("P1_Celonis")
P2 <- cleaning_function('P2_Disco_Celonis')
P3 <- cleaning_function('P3')
P4 <- cleaning_function('P4')
P5 <- cleaning_function('P5_Celonis')
P6 <- cleaning_function('P6')
P7 <- cleaning_function('P7')
P8 <- cleaning_function('P8')
P9 <- cleaning_function('P9_Celonis')
P10 <- cleaning_function('P10')
P11 <- cleaning_function("P11")
P12 <- cleaning_function('P12')
P13 <- cleaning_function('P13_Celonis')
P14 <- cleaning_function('P14_Celonis')
P15 <- cleaning_function('P15_ProM')
P16 <- cleaning_function('P16')
P17 <- cleaning_function('P17_Celonis')
P18 <- cleaning_function('P18')
P19 <- cleaning_function('P19')
P20 <- cleaning_function('P20_Celonis')
P21 <- cleaning_function("P21")
P22 <- cleaning_function('P22')
P23 <- cleaning_function('P23_Celonis')
P24 <- cleaning_function('P24_Celonis')
P25 <- cleaning_function('P25')
P26 <- cleaning_function('P26')
P27 <- cleaning_function('P27_Disco_bupaR')
P28 <- cleaning_function('P28')
P29 <- cleaning_function('P29')
P30 <- cleaning_function('P30')
P31 <- cleaning_function("P31_bupaR")
P32 <- cleaning_function('P32_Disco_ProM')
P33 <- cleaning_function('P33')
P34 <- cleaning_function('P34')
#P35 <- cleaning_function('P5_Celonis') No P35
P36 <- cleaning_function('P36_Pm4Py_Disco')
P37 <- cleaning_function('P37')
P38 <- cleaning_function('P38')
P39 <- cleaning_function('P39')
#P40 <- cleaning_function('P40') No P40
P41 <- cleaning_function('P41')


#Combine participants to extract all possible code combinations for every second:

combined_participants <- rbind(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22, P23, P24, P25, P26, 
                               P27, P28, P29, P30, P31, P32, P33, P34, P36, P37, P38, P39, P41)

combined_participants_notime <- combined_participants[,-c(1)]
unique_combi <- combined_participants_notime%>%group_by_all%>%count

#Sort unique_combi by ascending order and only take top 40:

sorted_unique_combi <- unique_combi[order(unique_combi$n, decreasing = TRUE),]

uni_combi <- sorted_unique_combi[1:39,1:(ncol(unique_combi)-1)]

# Saving Participant TS DataFrames and all possible combinations:

write.csv(sorted_unique_combi,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/Unique_Second_Code_Combinations.csv", row.names = FALSE)

#write.csv(P1,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/P1_Code_TS.csv", row.names = FALSE)


## Converting Each Participant TS to Sequence: -----------------------------------

TS_to_Repeated_Sequence <- function(Participant) {
  
  num_codes <- ncol(uni_combi)
  
  ts_raw_notime <- Participant[,-c(1)]
  
  num_col_ts_raw <- ncol(ts_raw_notime) + 1
  
  
  for (i in 1:nrow(ts_raw_notime)){
    
    seq_num <- which(apply(uni_combi, 1, function(x) all(x == ts_raw_notime[i,1:num_codes])))
    ts_raw_notime[i,num_col_ts_raw] <- paste('V',seq_num,sep='')
    print(i) #To check progress. Loop is kind of slow.
    
  }
  
  Participant_raw_Sequence <- ts_raw_notime
  
  return(Participant_raw_Sequence)
}


TS_to_Unepeated_Sequence <- function(Participant, Repeated_Sequence) {
  
  ts_raw_notime <- Participant[,-c(1)]
  
  num_col_ts_raw <- ncol(ts_raw_notime) + 1
  
  #Convert to list of only individual sequences:
  Participant_list <- c(Repeated_Sequence[1,num_col_ts_raw])
  
  for (i in 2:nrow(Repeated_Sequence)){
    
    if (Repeated_Sequence[i,num_col_ts_raw]!=Repeated_Sequence[i-1,num_col_ts_raw]) {
      
      Participant_list <- c(Participant_list, Repeated_Sequence[i,num_col_ts_raw])
      
    }
    
  }
  
  return(Participant_list)
  
}



P1_Repeated_Sequence <- TS_to_Repeated_Sequence(P1)
P1_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P1, P1_Repeated_Sequence)
write.csv(P1_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P1_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P1_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P1_Unrepeated_Sequence.csv", row.names = FALSE)

P2_Repeated_Sequence <- TS_to_Repeated_Sequence(P2)
P2_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P2, P2_Repeated_Sequence)
write.csv(P2_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P2_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P2_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P2_Unrepeated_Sequence.csv", row.names = FALSE)

P3_Repeated_Sequence <- TS_to_Repeated_Sequence(P3)
P3_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P3, P3_Repeated_Sequence)
write.csv(P3_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P3_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P3_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P3_Unrepeated_Sequence.csv", row.names = FALSE)

P4_Repeated_Sequence <- TS_to_Repeated_Sequence(P4)
P4_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P4, P4_Repeated_Sequence)
write.csv(P4_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P4_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P4_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P4_Unrepeated_Sequence.csv", row.names = FALSE)

P5_Repeated_Sequence <- TS_to_Repeated_Sequence(P5)
P5_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P5, P5_Repeated_Sequence)
write.csv(P5_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P5_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P5_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P5_Unrepeated_Sequence.csv", row.names = FALSE)

P6_Repeated_Sequence <- TS_to_Repeated_Sequence(P6)
P6_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P6, P6_Repeated_Sequence)
write.csv(P6_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P6_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P6_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P6_Unrepeated_Sequence.csv", row.names = FALSE)

P7_Repeated_Sequence <- TS_to_Repeated_Sequence(P7)
P7_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P7, P7_Repeated_Sequence)
write.csv(P7_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P7_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P7_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P7_Unrepeated_Sequence.csv", row.names = FALSE)

P8_Repeated_Sequence <- TS_to_Repeated_Sequence(P8)
P8_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P8, P8_Repeated_Sequence)
write.csv(P8_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P8_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P8_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P8_Unrepeated_Sequence.csv", row.names = FALSE)

P9_Repeated_Sequence <- TS_to_Repeated_Sequence(P9)
P9_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P9, P9_Repeated_Sequence)
write.csv(P9_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P9_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P9_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P9_Unrepeated_Sequence.csv", row.names = FALSE)

P10_Repeated_Sequence <- TS_to_Repeated_Sequence(P10)
P10_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P10, P10_Repeated_Sequence)
write.csv(P10_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P10_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P10_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P10_Unrepeated_Sequence.csv", row.names = FALSE)

P11_Repeated_Sequence <- TS_to_Repeated_Sequence(P11)
P11_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P11, P11_Repeated_Sequence)
write.csv(P11_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P11_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P11_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P11_Unrepeated_Sequence.csv", row.names = FALSE)

P12_Repeated_Sequence <- TS_to_Repeated_Sequence(P12)
P12_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P12, P12_Repeated_Sequence)
write.csv(P12_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P12_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P12_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P12_Unrepeated_Sequence.csv", row.names = FALSE)

P13_Repeated_Sequence <- TS_to_Repeated_Sequence(P13)
P13_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P13, P13_Repeated_Sequence)
write.csv(P13_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P13_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P13_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P13_Unrepeated_Sequence.csv", row.names = FALSE)

P14_Repeated_Sequence <- TS_to_Repeated_Sequence(P14)
P14_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P14, P14_Repeated_Sequence)
write.csv(P14_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P14_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P14_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P14_Unrepeated_Sequence.csv", row.names = FALSE)

P15_Repeated_Sequence <- TS_to_Repeated_Sequence(P15)
P15_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P15, P15_Repeated_Sequence)
write.csv(P15_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P15_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P15_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P15_Unrepeated_Sequence.csv", row.names = FALSE)

P16_Repeated_Sequence <- TS_to_Repeated_Sequence(P16)
P16_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P16, P16_Repeated_Sequence)
write.csv(P16_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P16_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P16_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P16_Unrepeated_Sequence.csv", row.names = FALSE)

P17_Repeated_Sequence <- TS_to_Repeated_Sequence(P17)
P17_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P17, P17_Repeated_Sequence)
write.csv(P17_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P17_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P17_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P17_Unrepeated_Sequence.csv", row.names = FALSE)

P18_Repeated_Sequence <- TS_to_Repeated_Sequence(P18)
P18_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P18, P18_Repeated_Sequence)
write.csv(P18_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P18_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P18_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P18_Unrepeated_Sequence.csv", row.names = FALSE)

P19_Repeated_Sequence <- TS_to_Repeated_Sequence(P19)
P19_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P19, P19_Repeated_Sequence)
write.csv(P19_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P19_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P19_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P19_Unrepeated_Sequence.csv", row.names = FALSE)

P20_Repeated_Sequence <- TS_to_Repeated_Sequence(P20)
P20_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P20, P20_Repeated_Sequence)
write.csv(P20_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P20_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P20_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P20_Unrepeated_Sequence.csv", row.names = FALSE)

P21_Repeated_Sequence <- TS_to_Repeated_Sequence(P21)
P21_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P21, P21_Repeated_Sequence)
write.csv(P21_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P21_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P21_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P21_Unrepeated_Sequence.csv", row.names = FALSE)

P22_Repeated_Sequence <- TS_to_Repeated_Sequence(P22)
P22_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P22, P22_Repeated_Sequence)
write.csv(P22_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P22_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P22_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P22_Unrepeated_Sequence.csv", row.names = FALSE)

P23_Repeated_Sequence <- TS_to_Repeated_Sequence(P23)
P23_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P23, P23_Repeated_Sequence)
write.csv(P23_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P23_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P23_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P23_Unrepeated_Sequence.csv", row.names = FALSE)

P24_Repeated_Sequence <- TS_to_Repeated_Sequence(P24)
P24_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P24, P24_Repeated_Sequence)
write.csv(P24_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P24_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P24_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P24_Unrepeated_Sequence.csv", row.names = FALSE)

P25_Repeated_Sequence <- TS_to_Repeated_Sequence(P25)
P25_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P25, P25_Repeated_Sequence)
write.csv(P25_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P25_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P25_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P25_Unrepeated_Sequence.csv", row.names = FALSE)

P26_Repeated_Sequence <- TS_to_Repeated_Sequence(P26)
P26_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P26, P26_Repeated_Sequence)
write.csv(P26_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P26_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P26_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P26_Unrepeated_Sequence.csv", row.names = FALSE)

P27_Repeated_Sequence <- TS_to_Repeated_Sequence(P27)
P27_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P27, P27_Repeated_Sequence)
write.csv(P27_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P27_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P27_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P27_Unrepeated_Sequence.csv", row.names = FALSE)

P28_Repeated_Sequence <- TS_to_Repeated_Sequence(P28)
P28_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P28, P28_Repeated_Sequence)
write.csv(P28_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P28_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P28_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P28_Unrepeated_Sequence.csv", row.names = FALSE)

P29_Repeated_Sequence <- TS_to_Repeated_Sequence(P29)
P29_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P29, P29_Repeated_Sequence)
write.csv(P29_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P29_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P29_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P29_Unrepeated_Sequence.csv", row.names = FALSE)

P30_Repeated_Sequence <- TS_to_Repeated_Sequence(P30)
P30_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P30, P30_Repeated_Sequence)
write.csv(P30_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P30_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P30_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P30_Unrepeated_Sequence.csv", row.names = FALSE)

##################

P31_Repeated_Sequence <- TS_to_Repeated_Sequence(P31)
P31_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P31, P31_Repeated_Sequence)
write.csv(P31_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P31_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P31_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P31_Unrepeated_Sequence.csv", row.names = FALSE)

P32_Repeated_Sequence <- TS_to_Repeated_Sequence(P32)
P32_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P32, P32_Repeated_Sequence)
write.csv(P32_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P32_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P32_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P32_Unrepeated_Sequence.csv", row.names = FALSE)

P33_Repeated_Sequence <- TS_to_Repeated_Sequence(P33)
P33_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P33, P33_Repeated_Sequence)
write.csv(P33_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P33_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P33_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P33_Unrepeated_Sequence.csv", row.names = FALSE)

P34_Repeated_Sequence <- TS_to_Repeated_Sequence(P34)
P34_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P34, P34_Repeated_Sequence)
write.csv(P34_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P34_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P34_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P34_Unrepeated_Sequence.csv", row.names = FALSE)

P36_Repeated_Sequence <- TS_to_Repeated_Sequence(P36)
P36_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P36, P36_Repeated_Sequence)
write.csv(P36_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P36_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P36_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P36_Unrepeated_Sequence.csv", row.names = FALSE)

P37_Repeated_Sequence <- TS_to_Repeated_Sequence(P37)
P37_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P37, P37_Repeated_Sequence)
write.csv(P37_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P37_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P37_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P37_Unrepeated_Sequence.csv", row.names = FALSE)

P38_Repeated_Sequence <- TS_to_Repeated_Sequence(P38)
P38_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P38, P38_Repeated_Sequence)
write.csv(P38_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P38_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P38_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P38_Unrepeated_Sequence.csv", row.names = FALSE)

P39_Repeated_Sequence <- TS_to_Repeated_Sequence(P39)
P39_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P39, P39_Repeated_Sequence)
write.csv(P39_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P39_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P39_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P39_Unrepeated_Sequence.csv", row.names = FALSE)

P41_Repeated_Sequence <- TS_to_Repeated_Sequence(P41)
P41_Unrepeated_Sequence <- TS_to_Unepeated_Sequence(P41, P41_Repeated_Sequence)
write.csv(P41_Repeated_Sequence,"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P41_Cleaned.csv", row.names = FALSE)
write.csv(data.frame(P41_Unrepeated_Sequence),"C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 40 Codes)/P41_Unrepeated_Sequence.csv", row.names = FALSE)

