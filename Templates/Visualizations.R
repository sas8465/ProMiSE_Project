library(TraMineR)
library(tidyverse)

## Import Data: -------------------------------------------------------------------------------------------------------------------------------

#Set working directory to file where your data is located.
setwd('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/What Codes Cleaned Data (Top 10 Codes)')
 
#This will create a dataframe for every .csv file 
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))

## Preprocessing: -----------------------------------------------------------------------------------------------------------------------------

#This section takes in formats generated from the preprocessing script and converts to appropriate data structure for all visualizations. 

#This function converts each participant sequence (which comes as a vertical vector) to a horizontal vector. This allows for easier manipulation 
#later on.
generate_transpose <- function(df) {
  
  df_transpose<-as.data.frame(t(df))
  colnames(df_transpose) <- seq(1, ncol(df_transpose), by=1)
  
  return(df_transpose)
  
}

### For duration and standardized visualization: ###

# Initial editing to remove any additional unwanted codes #

# For this example, we are dropping "Nothing" (V2) and "Mixed" (V) Code:

P1_Cleaned.csv <- P1_Cleaned.csv$V22[P1_Cleaned.csv$V22 %in% c("V2", "V") == FALSE] 
P2_Cleaned.csv <- P2_Cleaned.csv$V22[P2_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P3_Cleaned.csv <- P3_Cleaned.csv$V22[P3_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P4_Cleaned.csv <- P4_Cleaned.csv$V22[P4_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P5_Cleaned.csv <- P5_Cleaned.csv$V22[P5_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P6_Cleaned.csv <- P6_Cleaned.csv$V22[P6_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P7_Cleaned.csv <- P7_Cleaned.csv$V22[P7_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P8_Cleaned.csv <- P8_Cleaned.csv$V22[P8_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P9_Cleaned.csv <- P9_Cleaned.csv$V22[P9_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P10_Cleaned.csv <- P10_Cleaned.csv$V22[P10_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P11_Cleaned.csv <- P11_Cleaned.csv$V22[P11_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P12_Cleaned.csv <- P12_Cleaned.csv$V22[P12_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P13_Cleaned.csv <- P13_Cleaned.csv$V22[P13_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P14_Cleaned.csv <- P14_Cleaned.csv$V22[P14_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P15_Cleaned.csv <- P15_Cleaned.csv$V22[P15_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P16_Cleaned.csv <- P16_Cleaned.csv$V22[P16_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P17_Cleaned.csv <- P17_Cleaned.csv$V22[P17_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P18_Cleaned.csv <- P18_Cleaned.csv$V22[P18_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P19_Cleaned.csv <- P19_Cleaned.csv$V22[P19_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P20_Cleaned.csv <- P20_Cleaned.csv$V22[P20_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P21_Cleaned.csv <- P21_Cleaned.csv$V22[P21_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P22_Cleaned.csv <- P22_Cleaned.csv$V22[P22_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P23_Cleaned.csv <- P23_Cleaned.csv$V22[P23_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P24_Cleaned.csv <- P24_Cleaned.csv$V22[P24_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P25_Cleaned.csv <- P25_Cleaned.csv$V22[P25_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P26_Cleaned.csv <- P26_Cleaned.csv$V22[P26_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P27_Cleaned.csv <- P27_Cleaned.csv$V22[P27_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P28_Cleaned.csv <- P28_Cleaned.csv$V22[P28_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P29_Cleaned.csv <- P29_Cleaned.csv$V22[P29_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P30_Cleaned.csv <- P30_Cleaned.csv$V22[P30_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P31_Cleaned.csv <- P31_Cleaned.csv$V22[P31_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P32_Cleaned.csv <- P32_Cleaned.csv$V22[P32_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P33_Cleaned.csv <- P33_Cleaned.csv$V22[P33_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P34_Cleaned.csv <- P34_Cleaned.csv$V22[P34_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
#P35 Does not exist
P36_Cleaned.csv <- P36_Cleaned.csv$V22[P36_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P37_Cleaned.csv <- P37_Cleaned.csv$V22[P37_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P38_Cleaned.csv <- P38_Cleaned.csv$V22[P38_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
P39_Cleaned.csv <- P39_Cleaned.csv$V22[P39_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]
#P40 Does not exist
P41_Cleaned.csv <- P41_Cleaned.csv$V22[P41_Cleaned.csv$V22 %in% c("V2", "V") == FALSE]

P1_repeated_transpose <- generate_transpose(P1_Cleaned.csv)
P2_repeated_transpose <- generate_transpose(P2_Cleaned.csv)
P3_repeated_transpose <- generate_transpose(P3_Cleaned.csv)
P4_repeated_transpose <- generate_transpose(P4_Cleaned.csv)
P5_repeated_transpose <- generate_transpose(P5_Cleaned.csv)
P6_repeated_transpose <- generate_transpose(P6_Cleaned.csv)
P7_repeated_transpose <- generate_transpose(P7_Cleaned.csv)
P8_repeated_transpose <- generate_transpose(P8_Cleaned.csv)
P9_repeated_transpose <- generate_transpose(P9_Cleaned.csv)
P10_repeated_transpose <- generate_transpose(P10_Cleaned.csv)
P11_repeated_transpose <- generate_transpose(P11_Cleaned.csv)
P12_repeated_transpose <- generate_transpose(P12_Cleaned.csv)
P13_repeated_transpose <- generate_transpose(P13_Cleaned.csv)
P14_repeated_transpose <- generate_transpose(P14_Cleaned.csv)
P15_repeated_transpose <- generate_transpose(P15_Cleaned.csv)
P16_repeated_transpose <- generate_transpose(P16_Cleaned.csv)
P17_repeated_transpose <- generate_transpose(P17_Cleaned.csv)
P18_repeated_transpose <- generate_transpose(P18_Cleaned.csv)
P19_repeated_transpose <- generate_transpose(P19_Cleaned.csv)
P20_repeated_transpose <- generate_transpose(P20_Cleaned.csv)
P21_repeated_transpose <- generate_transpose(P21_Cleaned.csv)
P22_repeated_transpose <- generate_transpose(P22_Cleaned.csv)
P23_repeated_transpose <- generate_transpose(P23_Cleaned.csv)
P24_repeated_transpose <- generate_transpose(P24_Cleaned.csv)
P25_repeated_transpose <- generate_transpose(P25_Cleaned.csv)
P26_repeated_transpose <- generate_transpose(P26_Cleaned.csv)
P27_repeated_transpose <- generate_transpose(P27_Cleaned.csv)
P28_repeated_transpose <- generate_transpose(P28_Cleaned.csv)
P29_repeated_transpose <- generate_transpose(P29_Cleaned.csv)
P30_repeated_transpose <- generate_transpose(P30_Cleaned.csv)
P31_repeated_transpose <- generate_transpose(P31_Cleaned.csv)
P32_repeated_transpose <- generate_transpose(P32_Cleaned.csv)
P33_repeated_transpose <- generate_transpose(P33_Cleaned.csv)
P34_repeated_transpose <- generate_transpose(P34_Cleaned.csv)
P36_repeated_transpose <- generate_transpose(P36_Cleaned.csv)
P37_repeated_transpose <- generate_transpose(P37_Cleaned.csv)
P38_repeated_transpose <- generate_transpose(P38_Cleaned.csv)
P39_repeated_transpose <- generate_transpose(P39_Cleaned.csv)
P41_repeated_transpose <- generate_transpose(P41_Cleaned.csv)

# Combine all participants into one dataframe:

participant_repeated_sequences <- P1_repeated_transpose %>% full_join(P2_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P3_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P4_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P5_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P6_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P7_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P8_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P9_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P10_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P11_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P12_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P13_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P14_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P15_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P16_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P17_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P18_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P19_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P20_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P21_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P22_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P23_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P24_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P25_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P26_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P27_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P28_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P29_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P30_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P31_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P32_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P33_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P34_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P36_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P37_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P38_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P39_repeated_transpose)
participant_repeated_sequences <- participant_repeated_sequences %>% full_join(P41_repeated_transpose)

### For Transition Visualization: ###

#Initial editing to remove any additional unwanted codes #

# For this example, we are dropping "Nothing" (V2) and "Mixed" (V) Code:

P1_Unrepeated_Sequence.csv <- P1_Unrepeated_Sequence.csv$P1_Unrepeated_Sequence.csv[P1_Unrepeated_Sequence.csv$P1_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE] 
P2_Unrepeated_Sequence.csv <- P2_Unrepeated_Sequence.csv$P2_Unrepeated_Sequence.csv[P2_Unrepeated_Sequence.csv$P2_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P3_Unrepeated_Sequence.csv <- P3_Unrepeated_Sequence.csv$P3_Unrepeated_Sequence.csv[P3_Unrepeated_Sequence.csv$P3_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P4_Unrepeated_Sequence.csv <- P4_Unrepeated_Sequence.csv$P4_Unrepeated_Sequence.csv[P4_Unrepeated_Sequence.csv$P4_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P5_Unrepeated_Sequence.csv <- P5_Unrepeated_Sequence.csv$P5_Unrepeated_Sequence.csv[P5_Unrepeated_Sequence.csv$P5_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P6_Unrepeated_Sequence.csv <- P6_Unrepeated_Sequence.csv$P6_Unrepeated_Sequence.csv[P6_Unrepeated_Sequence.csv$P6_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P7_Unrepeated_Sequence.csv <- P7_Unrepeated_Sequence.csv$P7_Unrepeated_Sequence.csv[P7_Unrepeated_Sequence.csv$P7_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P8_Unrepeated_Sequence.csv <- P8_Unrepeated_Sequence.csv$P8_Unrepeated_Sequence.csv[P8_Unrepeated_Sequence.csv$P8_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P9_Unrepeated_Sequence.csv <- P9_Unrepeated_Sequence.csv$P9_Unrepeated_Sequence.csv[P9_Unrepeated_Sequence.csv$P9_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P10_Unrepeated_Sequence.csv <- P10_Unrepeated_Sequence.csv$P10_Unrepeated_Sequence.csv[P10_Unrepeated_Sequence.csv$P10_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P11_Unrepeated_Sequence.csv <- P11_Unrepeated_Sequence.csv$P11_Unrepeated_Sequence.csv[P11_Unrepeated_Sequence.csv$P11_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P12_Unrepeated_Sequence.csv <- P12_Unrepeated_Sequence.csv$P12_Unrepeated_Sequence.csv[P12_Unrepeated_Sequence.csv$P12_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P13_Unrepeated_Sequence.csv <- P13_Unrepeated_Sequence.csv$P13_Unrepeated_Sequence.csv[P13_Unrepeated_Sequence.csv$P13_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P14_Unrepeated_Sequence.csv <- P14_Unrepeated_Sequence.csv$P14_Unrepeated_Sequence.csv[P14_Unrepeated_Sequence.csv$P14_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P15_Unrepeated_Sequence.csv <- P15_Unrepeated_Sequence.csv$P15_Unrepeated_Sequence.csv[P15_Unrepeated_Sequence.csv$P15_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P16_Unrepeated_Sequence.csv <- P16_Unrepeated_Sequence.csv$P16_Unrepeated_Sequence.csv[P16_Unrepeated_Sequence.csv$P16_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P17_Unrepeated_Sequence.csv <- P17_Unrepeated_Sequence.csv$P17_Unrepeated_Sequence.csv[P17_Unrepeated_Sequence.csv$P17_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P18_Unrepeated_Sequence.csv <- P18_Unrepeated_Sequence.csv$P18_Unrepeated_Sequence.csv[P18_Unrepeated_Sequence.csv$P18_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P19_Unrepeated_Sequence.csv <- P19_Unrepeated_Sequence.csv$P19_Unrepeated_Sequence.csv[P19_Unrepeated_Sequence.csv$P19_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P20_Unrepeated_Sequence.csv <- P20_Unrepeated_Sequence.csv$P20_Unrepeated_Sequence.csv[P20_Unrepeated_Sequence.csv$P20_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P21_Unrepeated_Sequence.csv <- P21_Unrepeated_Sequence.csv$P21_Unrepeated_Sequence.csv[P21_Unrepeated_Sequence.csv$P21_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P22_Unrepeated_Sequence.csv <- P22_Unrepeated_Sequence.csv$P22_Unrepeated_Sequence.csv[P22_Unrepeated_Sequence.csv$P22_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P23_Unrepeated_Sequence.csv <- P23_Unrepeated_Sequence.csv$P23_Unrepeated_Sequence.csv[P23_Unrepeated_Sequence.csv$P23_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P24_Unrepeated_Sequence.csv <- P24_Unrepeated_Sequence.csv$P24_Unrepeated_Sequence.csv[P24_Unrepeated_Sequence.csv$P24_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P25_Unrepeated_Sequence.csv <- P25_Unrepeated_Sequence.csv$P25_Unrepeated_Sequence.csv[P25_Unrepeated_Sequence.csv$P25_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P26_Unrepeated_Sequence.csv <- P26_Unrepeated_Sequence.csv$P26_Unrepeated_Sequence.csv[P26_Unrepeated_Sequence.csv$P26_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P27_Unrepeated_Sequence.csv <- P27_Unrepeated_Sequence.csv$P27_Unrepeated_Sequence.csv[P27_Unrepeated_Sequence.csv$P27_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P28_Unrepeated_Sequence.csv <- P28_Unrepeated_Sequence.csv$P28_Unrepeated_Sequence.csv[P28_Unrepeated_Sequence.csv$P28_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P29_Unrepeated_Sequence.csv <- P29_Unrepeated_Sequence.csv$P29_Unrepeated_Sequence.csv[P29_Unrepeated_Sequence.csv$P29_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P30_Unrepeated_Sequence.csv <- P30_Unrepeated_Sequence.csv$P30_Unrepeated_Sequence.csv[P30_Unrepeated_Sequence.csv$P30_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P31_Unrepeated_Sequence.csv <- P31_Unrepeated_Sequence.csv$P31_Unrepeated_Sequence.csv[P31_Unrepeated_Sequence.csv$P31_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P32_Unrepeated_Sequence.csv <- P32_Unrepeated_Sequence.csv$P32_Unrepeated_Sequence.csv[P32_Unrepeated_Sequence.csv$P32_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P33_Unrepeated_Sequence.csv <- P33_Unrepeated_Sequence.csv$P33_Unrepeated_Sequence.csv[P33_Unrepeated_Sequence.csv$P33_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P34_Unrepeated_Sequence.csv <- P34_Unrepeated_Sequence.csv$P34_Unrepeated_Sequence.csv[P34_Unrepeated_Sequence.csv$P34_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
#P35 Does not exist
P36_Unrepeated_Sequence.csv <- P36_Unrepeated_Sequence.csv$P36_Unrepeated_Sequence.csv[P36_Unrepeated_Sequence.csv$P36_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P37_Unrepeated_Sequence.csv <- P37_Unrepeated_Sequence.csv$P37_Unrepeated_Sequence.csv[P37_Unrepeated_Sequence.csv$P37_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P38_Unrepeated_Sequence.csv <- P38_Unrepeated_Sequence.csv$P38_Unrepeated_Sequence.csv[P38_Unrepeated_Sequence.csv$P38_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
P39_Unrepeated_Sequence.csv <- P39_Unrepeated_Sequence.csv$P39_Unrepeated_Sequence.csv[P39_Unrepeated_Sequence.csv$P39_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]
#P40 Does not exist
P41_Unrepeated_Sequence.csv <- P41_Unrepeated_Sequence.csv$P41_Unrepeated_Sequence.csv[P41_Unrepeated_Sequence.csv$P41_Unrepeated_Sequence.csv %in% c("V2", "V") == FALSE]

P1_unrepeated_transpose <- generate_transpose(P1_Unrepeated_Sequence.csv)
P2_unrepeated_transpose <- generate_transpose(P2_Unrepeated_Sequence.csv)
P3_unrepeated_transpose <- generate_transpose(P3_Unrepeated_Sequence.csv)
P4_unrepeated_transpose <- generate_transpose(P4_Unrepeated_Sequence.csv)
P5_unrepeated_transpose <- generate_transpose(P5_Unrepeated_Sequence.csv)
P6_unrepeated_transpose <- generate_transpose(P6_Unrepeated_Sequence.csv)
P7_unrepeated_transpose <- generate_transpose(P7_Unrepeated_Sequence.csv)
P8_unrepeated_transpose <- generate_transpose(P8_Unrepeated_Sequence.csv)
P9_unrepeated_transpose <- generate_transpose(P9_Unrepeated_Sequence.csv)
P10_unrepeated_transpose <- generate_transpose(P10_Unrepeated_Sequence.csv)
P11_unrepeated_transpose <- generate_transpose(P11_Unrepeated_Sequence.csv)
P12_unrepeated_transpose <- generate_transpose(P12_Unrepeated_Sequence.csv)
P13_unrepeated_transpose <- generate_transpose(P13_Unrepeated_Sequence.csv)
P14_unrepeated_transpose <- generate_transpose(P14_Unrepeated_Sequence.csv)
P15_unrepeated_transpose <- generate_transpose(P15_Unrepeated_Sequence.csv)
P16_unrepeated_transpose <- generate_transpose(P16_Unrepeated_Sequence.csv)
P17_unrepeated_transpose <- generate_transpose(P17_Unrepeated_Sequence.csv)
P18_unrepeated_transpose <- generate_transpose(P18_Unrepeated_Sequence.csv)
P19_unrepeated_transpose <- generate_transpose(P19_Unrepeated_Sequence.csv)
P20_unrepeated_transpose <- generate_transpose(P20_Unrepeated_Sequence.csv)
P21_unrepeated_transpose <- generate_transpose(P21_Unrepeated_Sequence.csv)
P22_unrepeated_transpose <- generate_transpose(P22_Unrepeated_Sequence.csv)
P23_unrepeated_transpose <- generate_transpose(P23_Unrepeated_Sequence.csv)
P24_unrepeated_transpose <- generate_transpose(P24_Unrepeated_Sequence.csv)
P25_unrepeated_transpose <- generate_transpose(P25_Unrepeated_Sequence.csv)
P26_unrepeated_transpose <- generate_transpose(P26_Unrepeated_Sequence.csv)
P27_unrepeated_transpose <- generate_transpose(P27_Unrepeated_Sequence.csv)
P28_unrepeated_transpose <- generate_transpose(P28_Unrepeated_Sequence.csv)
P29_unrepeated_transpose <- generate_transpose(P29_Unrepeated_Sequence.csv)
P30_unrepeated_transpose <- generate_transpose(P30_Unrepeated_Sequence.csv)
P31_unrepeated_transpose <- generate_transpose(P31_Unrepeated_Sequence.csv)
P32_unrepeated_transpose <- generate_transpose(P32_Unrepeated_Sequence.csv)
P33_unrepeated_transpose <- generate_transpose(P33_Unrepeated_Sequence.csv)
P34_unrepeated_transpose <- generate_transpose(P34_Unrepeated_Sequence.csv)
P36_unrepeated_transpose <- generate_transpose(P36_Unrepeated_Sequence.csv)
P37_unrepeated_transpose <- generate_transpose(P37_Unrepeated_Sequence.csv)
P38_unrepeated_transpose <- generate_transpose(P38_Unrepeated_Sequence.csv)
P39_unrepeated_transpose <- generate_transpose(P39_Unrepeated_Sequence.csv)
P41_unrepeated_transpose <- generate_transpose(P41_Unrepeated_Sequence.csv)

# Combine all participants into one dataframe:

participant_unrepeated_sequences <- P1_unrepeated_transpose %>% full_join(P2_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P3_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P4_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P5_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P6_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P7_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P8_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P9_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P10_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P11_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P12_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P13_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P14_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P15_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P16_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P17_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P18_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P19_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P20_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P21_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P22_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P23_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P24_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P25_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P26_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P27_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P28_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P29_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P30_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P31_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P32_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P33_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P34_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P36_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P37_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P38_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P39_unrepeated_transpose)
participant_unrepeated_sequences <- participant_unrepeated_sequences %>% full_join(P41_unrepeated_transpose)

## Generating Specific Visualizations: ---------------------------------------------------------------------------------------------------------------

### Duration Visualization ###

#Defining the Sequence Form 1:

uniq <- as.vector(as.matrix(participant_repeated_sequences))
print(unique(uniq))

#Defining the Sequence Form:

participant.alphab <- c("V4", "V5", "V6", "V1", "V7", "V3", "V9", "V8", "V10")

participant.lab <- c("Activity Description", "Entire Event Log", "Guiding Questions", "Variants", "Event Log Attributes", "Activities", "Temporal Contraints", "Data Attributes", "Activities+Temporal Aspects")

participant.scode <- c("V4", "V5", "V6", "V1", "V7", "V3", "V9", "V8", "V10")

participant.seq <- seqdef(participant_repeated_sequences, alphabet = participant.alphab, labels = participant.lab, states = participant.scode)

seqiplot(participant.seq, border = NA, main="Sample of P1 to P10 What Code Sequence", with.legend	="right")

#Ploting All Participants

seqiplot(participant.seq[1:10,], border = NA, main="Sample of P1 to P10 How Code Sequence", with.legend	="right")
seqiplot(participant.seq[11:20,], border = NA, main="Sample of P11 to P20 How Code Sequence", with.legend	="right")
seqiplot(participant.seq[21:30,], border = NA, main="Sample of P21 to P30 How Code Sequence", with.legend	="right")
seqiplot(participant.seq[31:39,], border = NA, main="Sample of P31 to P41 How Code Sequence", with.legend	="right")





### Transition Visualization ###

#Defining the Sequence Form 1:

uniq <- as.vector(as.matrix(participant_sequences))
print(unique(uniq))

#Defining the Sequence Form:

participant.alphab <- c("V4", "V5", "V6", "V1", "V7", "V3", "V9", "V8")

participant.lab <- c("Activity Description", "Entire Event Log", "Guiding Questions", "Variants", "Event Log Attributes", "Activities", "Temporal Contraints", "Data Attributes")

participant.scode <- c("V4", "V5", "V6", "V1", "V7", "V3", "V9", "V8")

participant.seq <- seqdef(participant_sequences, alphabet = participant.alphab, labels = participant.lab, states = participant.scode)

seqiplot(participant.seq, border = NA, main="Sample of P1 to P10 What Code Sequence", with.legend	="right")

#Ploting All Participants

seqiplot(participant.seq[1:10,], border = NA, main="Sample of P1 to P10 What Code Sequence", with.legend	="right")
seqiplot(participant.seq[11:20,], border = NA, main="Sample of P12 to P20 What Code Sequence", with.legend	="right")
seqiplot(participant.seq[21:30,], border = NA, main="Sample of P21 to P30 What Code Sequence", with.legend	="right")
seqiplot(participant.seq[31:39,], border = NA, main="Sample of P31 to P41 What Code Sequence", with.legend	="right")






### Standardized Visualtion: ###

#Function to Standardize Sequence Length:
standardize_sequence_length <- function(participant_sequence, fixed_length=1000, participant_number) {  
  #Fixed length parameter can be changed to change standardization length.
  
  participant_sequences[participant_number,]
  
  fill_participated_sequence <- as.data.frame(t(participant_sequences[participant_number, ] ))
  
  fill_participated_sequence <- fill_participated_sequence[complete.cases(fill_participated_sequence) ,]
  
  stand_factor <- length(fill_participated_sequence) / fixed_length
  
  rounded_stand_factor <- stand_factor
  
  df <- data.frame(fill_participated_sequence)
  
  standardized_sequence <- list()
  
  for (i in seq(from=0, to=length(fill_participated_sequence), by=rounded_stand_factor)) {
    
    start = i - rounded_stand_factor
    
    transform <- df[start:i,]
    
    standardized_sequence <- append(standardized_sequence, tail(names(sort(table(transform))), 1))
    
  }
  
  standardized_sequence <- unlist(standardized_sequence)
  
  standardized_sequence_df <- data.frame(standardized_sequence)
  
  standardized_sequence_df_transpose<-as.data.frame(t(standardized_sequence_df))
  colnames(standardized_sequence_df_transpose) <- seq(1, ncol(standardized_sequence_df_transpose), by=1)
  
  return(standardized_sequence_df_transpose)
  
}


P1_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 1)
P2_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 2)
P3_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 3)
P4_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 4)
P5_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 5)
P6_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 6)
P7_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 7)
P8_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 8)
P9_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 9)
P10_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 10)
P11_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 11)
P12_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 12)
P13_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 13)
P14_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 14)
P15_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 15)
P16_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 16)
P17_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 17)
P18_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 18)
P19_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 19)
P20_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 20)
P21_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 21)
P22_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 22)
P23_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 23)
P24_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 24)
P25_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 25)
P26_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 26)
P27_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 27)
P28_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 28)
P29_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 29)
P30_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 30)
P31_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 31)
P32_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 32)
P33_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 33)
P34_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 34)
P35_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 35)
P36_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 36)
P37_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 37)
P38_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 38)
P39_standard <- standardize_sequence_length(participant_repeated_sequences, 1000, 39)


#Joining them all into a single data frame:

standard_participant_sequences <- P1_standard %>% full_join(P2_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P3_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P4_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P5_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P6_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P7_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P8_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P9_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P10_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P11_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P12_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P13_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P14_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P15_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P16_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P17_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P18_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P19_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P20_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P21_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P22_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P23_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P24_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P25_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P26_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P27_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P28_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P29_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P30_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P31_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P32_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P33_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P34_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P35_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P36_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P37_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P38_standard)
standard_participant_sequences <- standard_participant_sequences %>% full_join(P39_standard)


#Defining the Sequence Form 1:

uniq <- as.vector(as.matrix(participant_sequences))
print(unique(uniq))


#Plotting Sequences:

participant.alphab <- c("V4", "V5", "V6", "V1", "V7", "V3", "V9", "V8", "V10")

participant.lab <- c("Activity Description", "Entire Event Log", "Guiding Questions", "Variants", "Event Log Attributes", "Activities", "Temporal Contraints", "Data Attributes", "Activities+Temporal Aspects")

participant.scode <- c("V4", "V5", "V6", "V1", "V7", "V3", "V9", "V8", "V10")

participant.seq <- seqdef(standard_participant_sequences, alphabet = participant.alphab, labels = participant.lab, states = participant.scode)

seqiplot(participant.seq, border = NA, main="Sample of P1 to P10 What Code Sequence", with.legend	="right")

#Ploting All Participants

seqiplot(participant.seq[1:10,], border = NA, main="Sample of P1 to P10 What Code Sequence", with.legend	="right")
seqiplot(participant.seq[11:20,], border = NA, main="Sample of P12 to P20 What Code Sequence", with.legend	="right")
seqiplot(participant.seq[21:30,], border = NA, main="Sample of P21 to P30 What Code Sequence", with.legend	="right")
seqiplot(participant.seq[31:39,], border = NA, main="Sample of P31 to P41 What Code Sequence", with.legend	="right")