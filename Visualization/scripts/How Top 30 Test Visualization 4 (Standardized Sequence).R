library(TraMineR)
library(dplyr)

P1 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P1_Cleaned.csv')
P2 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P2_Cleaned.csv')
P3 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P3_Cleaned.csv')
P4 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P4_Cleaned.csv')
P5 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P5_Cleaned.csv')
P6 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P6_Cleaned.csv')
P7 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P7_Cleaned.csv')
P8 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P8_Cleaned.csv')
P9 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P9_Cleaned.csv')
P10 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P10_Cleaned.csv')
P11 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P11_Cleaned.csv')
P12 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P12_Cleaned.csv')
P13 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P13_Cleaned.csv')
P14 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P14_Cleaned.csv')
P15 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P15_Cleaned.csv')
P16 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P16_Cleaned.csv')
P17 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P17_Cleaned.csv')
P18 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P18_Cleaned.csv')
P19 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P19_Cleaned.csv')
P20 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P20_Cleaned.csv')
P21 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P21_Cleaned.csv')
P22 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P22_Cleaned.csv')
P23 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P23_Cleaned.csv')
P24 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P24_Cleaned.csv')
P25 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P25_Cleaned.csv')
P26 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P26_Cleaned.csv')
P27 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P27_Cleaned.csv')
P28 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P28_Cleaned.csv')
P29 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P29_Cleaned.csv')
P30 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P30_Cleaned.csv')
P31 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P31_Cleaned.csv')
P32 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P32_Cleaned.csv')
P33 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P33_Cleaned.csv')
P34 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P34_Cleaned.csv')
P36 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P36_Cleaned.csv')
P37 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P37_Cleaned.csv')
P38 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P38_Cleaned.csv')
P39 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P39_Cleaned.csv')
P41 <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/P41_Cleaned.csv')

labeling_system <- read.csv('C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/How Codes Cleaned Data (Top 30 Codes)/Unique_Second_Code_Combinations.csv')


P1_transpose<-as.data.frame(t(P1$V22))
colnames(P1_transpose) <- seq(1, ncol(P1_transpose), by=1)

P2_transpose<-as.data.frame(t(P2$V22))
colnames(P2_transpose) <- seq(1, ncol(P2_transpose), by=1)

P3_transpose<-as.data.frame(t(P3$V22))
colnames(P3_transpose) <- seq(1, ncol(P3_transpose), by=1)

P4_transpose<-as.data.frame(t(P4$V22))
colnames(P4_transpose) <- seq(1, ncol(P4_transpose), by=1)

P5_transpose<-as.data.frame(t(P5$V22))
colnames(P5_transpose) <- seq(1, ncol(P5_transpose), by=1)

P6_transpose<-as.data.frame(t(P6$V22))
colnames(P6_transpose) <- seq(1, ncol(P6_transpose), by=1)

P7_transpose<-as.data.frame(t(P7$V22))
colnames(P7_transpose) <- seq(1, ncol(P7_transpose), by=1)

P8_transpose<-as.data.frame(t(P8$V22))
colnames(P8_transpose) <- seq(1, ncol(P8_transpose), by=1)

P9_transpose<-as.data.frame(t(P9$V22))
colnames(P9_transpose) <- seq(1, ncol(P9_transpose), by=1)

P10_transpose<-as.data.frame(t(P10$V22))
colnames(P10_transpose) <- seq(1, ncol(P10_transpose), by=1)

P11_transpose<-as.data.frame(t(P11$V22))
colnames(P11_transpose) <- seq(1, ncol(P11_transpose), by=1)

P12_transpose<-as.data.frame(t(P12$V22))
colnames(P12_transpose) <- seq(1, ncol(P12_transpose), by=1)

P13_transpose<-as.data.frame(t(P13$V22))
colnames(P13_transpose) <- seq(1, ncol(P13_transpose), by=1)

P14_transpose<-as.data.frame(t(P14$V22))
colnames(P14_transpose) <- seq(1, ncol(P14_transpose), by=1)

P15_transpose<-as.data.frame(t(P15$V22))
colnames(P15_transpose) <- seq(1, ncol(P15_transpose), by=1)

P16_transpose<-as.data.frame(t(P16$V22))
colnames(P16_transpose) <- seq(1, ncol(P16_transpose), by=1)

P17_transpose<-as.data.frame(t(P17$V22))
colnames(P17_transpose) <- seq(1, ncol(P17_transpose), by=1)

P18_transpose<-as.data.frame(t(P18$V22))
colnames(P18_transpose) <- seq(1, ncol(P18_transpose), by=1)

P19_transpose<-as.data.frame(t(P19$V22))
colnames(P19_transpose) <- seq(1, ncol(P19_transpose), by=1)

P20_transpose<-as.data.frame(t(P20$V22))
colnames(P20_transpose) <- seq(1, ncol(P20_transpose), by=1)

P21_transpose<-as.data.frame(t(P21$V22))
colnames(P21_transpose) <- seq(1, ncol(P21_transpose), by=1)

P22_transpose<-as.data.frame(t(P22$V22))
colnames(P22_transpose) <- seq(1, ncol(P22_transpose), by=1)

P23_transpose<-as.data.frame(t(P23$V22))
colnames(P23_transpose) <- seq(1, ncol(P23_transpose), by=1)

P24_transpose<-as.data.frame(t(P24$V22))
colnames(P24_transpose) <- seq(1, ncol(P24_transpose), by=1)

P25_transpose<-as.data.frame(t(P25$V22))
colnames(P25_transpose) <- seq(1, ncol(P25_transpose), by=1)

P26_transpose<-as.data.frame(t(P26$V22))
colnames(P26_transpose) <- seq(1, ncol(P26_transpose), by=1)

P27_transpose<-as.data.frame(t(P27$V22))
colnames(P27_transpose) <- seq(1, ncol(P27_transpose), by=1)

P28_transpose<-as.data.frame(t(P28$V22))
colnames(P28_transpose) <- seq(1, ncol(P28_transpose), by=1)

P29_transpose<-as.data.frame(t(P29$V22))
colnames(P29_transpose) <- seq(1, ncol(P29_transpose), by=1)

P30_transpose<-as.data.frame(t(P30$V22))
colnames(P30_transpose) <- seq(1, ncol(P30_transpose), by=1)

P31_transpose<-as.data.frame(t(P31$V22))
colnames(P31_transpose) <- seq(1, ncol(P31_transpose), by=1)

P32_transpose<-as.data.frame(t(P32$V22))
colnames(P32_transpose) <- seq(1, ncol(P32_transpose), by=1)

P33_transpose<-as.data.frame(t(P33$V22))
colnames(P33_transpose) <- seq(1, ncol(P33_transpose), by=1)

P34_transpose<-as.data.frame(t(P34$V22))
colnames(P34_transpose) <- seq(1, ncol(P34_transpose), by=1)

P36_transpose<-as.data.frame(t(P36$V22))
colnames(P36_transpose) <- seq(1, ncol(P36_transpose), by=1)

P37_transpose<-as.data.frame(t(P37$V22))
colnames(P37_transpose) <- seq(1, ncol(P37_transpose), by=1)

P38_transpose<-as.data.frame(t(P38$V22))
colnames(P38_transpose) <- seq(1, ncol(P38_transpose), by=1)

P39_transpose<-as.data.frame(t(P39$V22))
colnames(P39_transpose) <- seq(1, ncol(P39_transpose), by=1)

P41_transpose<-as.data.frame(t(P41$V22))
colnames(P41_transpose) <- seq(1, ncol(P41_transpose), by=1)

#Joining them all into a single data frame:

participant_sequences <- P1_transpose %>% full_join(P2_transpose)
participant_sequences <- participant_sequences %>% full_join(P3_transpose)
participant_sequences <- participant_sequences %>% full_join(P4_transpose)
participant_sequences <- participant_sequences %>% full_join(P5_transpose)
participant_sequences <- participant_sequences %>% full_join(P6_transpose)
participant_sequences <- participant_sequences %>% full_join(P7_transpose)
participant_sequences <- participant_sequences %>% full_join(P8_transpose)
participant_sequences <- participant_sequences %>% full_join(P9_transpose)
participant_sequences <- participant_sequences %>% full_join(P10_transpose)
participant_sequences <- participant_sequences %>% full_join(P11_transpose)
participant_sequences <- participant_sequences %>% full_join(P12_transpose)
participant_sequences <- participant_sequences %>% full_join(P13_transpose)
participant_sequences <- participant_sequences %>% full_join(P14_transpose)
participant_sequences <- participant_sequences %>% full_join(P15_transpose)
participant_sequences <- participant_sequences %>% full_join(P16_transpose)
participant_sequences <- participant_sequences %>% full_join(P17_transpose)
participant_sequences <- participant_sequences %>% full_join(P18_transpose)
participant_sequences <- participant_sequences %>% full_join(P19_transpose)
participant_sequences <- participant_sequences %>% full_join(P20_transpose)
participant_sequences <- participant_sequences %>% full_join(P21_transpose)
participant_sequences <- participant_sequences %>% full_join(P22_transpose)
participant_sequences <- participant_sequences %>% full_join(P23_transpose)
participant_sequences <- participant_sequences %>% full_join(P24_transpose)
participant_sequences <- participant_sequences %>% full_join(P25_transpose)
participant_sequences <- participant_sequences %>% full_join(P26_transpose)
participant_sequences <- participant_sequences %>% full_join(P27_transpose)
participant_sequences <- participant_sequences %>% full_join(P28_transpose)
participant_sequences <- participant_sequences %>% full_join(P29_transpose)
participant_sequences <- participant_sequences %>% full_join(P30_transpose)
participant_sequences <- participant_sequences %>% full_join(P31_transpose)
participant_sequences <- participant_sequences %>% full_join(P32_transpose)
participant_sequences <- participant_sequences %>% full_join(P33_transpose)
participant_sequences <- participant_sequences %>% full_join(P34_transpose)
participant_sequences <- participant_sequences %>% full_join(P36_transpose)
participant_sequences <- participant_sequences %>% full_join(P37_transpose)
participant_sequences <- participant_sequences %>% full_join(P38_transpose)
participant_sequences <- participant_sequences %>% full_join(P39_transpose)
participant_sequences <- participant_sequences %>% full_join(P41_transpose)


## Standardizing Sequence Length:

standardize_sequence_length <- function(participant_sequence, fixed_length=1000, participant_number) {
  
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


P1_standard <- standardize_sequence_length(participant_sequence, 1000, 1)
P2_standard <- standardize_sequence_length(participant_sequence, 1000, 2)
P3_standard <- standardize_sequence_length(participant_sequence, 1000, 3)
P4_standard <- standardize_sequence_length(participant_sequence, 1000, 4)
P5_standard <- standardize_sequence_length(participant_sequence, 1000, 5)
P6_standard <- standardize_sequence_length(participant_sequence, 1000, 6)
P7_standard <- standardize_sequence_length(participant_sequence, 1000, 7)
P8_standard <- standardize_sequence_length(participant_sequence, 1000, 8)
P9_standard <- standardize_sequence_length(participant_sequence, 1000, 9)
P10_standard <- standardize_sequence_length(participant_sequence, 1000, 10)
P11_standard <- standardize_sequence_length(participant_sequence, 1000, 11)
P12_standard <- standardize_sequence_length(participant_sequence, 1000, 12)
P13_standard <- standardize_sequence_length(participant_sequence, 1000, 13)
P14_standard <- standardize_sequence_length(participant_sequence, 1000, 14)
P15_standard <- standardize_sequence_length(participant_sequence, 1000, 15)
P16_standard <- standardize_sequence_length(participant_sequence, 1000, 16)
P17_standard <- standardize_sequence_length(participant_sequence, 1000, 17)
P18_standard <- standardize_sequence_length(participant_sequence, 1000, 18)
P19_standard <- standardize_sequence_length(participant_sequence, 1000, 19)
P20_standard <- standardize_sequence_length(participant_sequence, 1000, 20)
P21_standard <- standardize_sequence_length(participant_sequence, 1000, 21)
P22_standard <- standardize_sequence_length(participant_sequence, 1000, 22)
P23_standard <- standardize_sequence_length(participant_sequence, 1000, 23)
P24_standard <- standardize_sequence_length(participant_sequence, 1000, 24)
P25_standard <- standardize_sequence_length(participant_sequence, 1000, 25)
P26_standard <- standardize_sequence_length(participant_sequence, 1000, 26)
P27_standard <- standardize_sequence_length(participant_sequence, 1000, 27)
P28_standard <- standardize_sequence_length(participant_sequence, 1000, 28)
P29_standard <- standardize_sequence_length(participant_sequence, 1000, 29)
P30_standard <- standardize_sequence_length(participant_sequence, 1000, 30)
P31_standard <- standardize_sequence_length(participant_sequence, 1000, 31)
P32_standard <- standardize_sequence_length(participant_sequence, 1000, 32)
P33_standard <- standardize_sequence_length(participant_sequence, 1000, 33)
P34_standard <- standardize_sequence_length(participant_sequence, 1000, 34)
P35_standard <- standardize_sequence_length(participant_sequence, 1000, 35)
P36_standard <- standardize_sequence_length(participant_sequence, 1000, 36)
P37_standard <- standardize_sequence_length(participant_sequence, 1000, 37)
P38_standard <- standardize_sequence_length(participant_sequence, 1000, 38)
P39_standard <- standardize_sequence_length(participant_sequence, 1000, 39)


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


#Subsetting specific values:

standard_participant_sequences[standard_participant_sequences=="V8"] <- 'V'
standard_participant_sequences[standard_participant_sequences=="V9"] <- 'V'
standard_participant_sequences[standard_participant_sequences=="V10"] <- 'V'
standard_participant_sequences[standard_participant_sequences=="V11"] <- 'V'
standard_participant_sequences[standard_participant_sequences=="V12"] <- 'V'
standard_participant_sequences[standard_participant_sequences=="V13"] <- 'V'
standard_participant_sequences[standard_participant_sequences=="V14"] <- 'V'
standard_participant_sequences[standard_participant_sequences=="V15"] <- 'V'
standard_participant_sequences[standard_participant_sequences=="V16"] <- 'V'
standard_participant_sequences[standard_participant_sequences=="V17"] <- 'V'
standard_participant_sequences[standard_participant_sequences=="V18"] <- 'V'
standard_participant_sequences[standard_participant_sequences=="V19"] <- 'V'
standard_participant_sequences[standard_participant_sequences=="V20"] <- 'V'
standard_participant_sequences[standard_participant_sequences=="V21"] <- 'V'
standard_participant_sequences[standard_participant_sequences=="V22"] <- 'V'
standard_participant_sequences[standard_participant_sequences=="V23"] <- 'V'
standard_participant_sequences[standard_participant_sequences=="V24"] <- 'V'
standard_participant_sequences[standard_participant_sequences=="V25"] <- 'V'
standard_participant_sequences[standard_participant_sequences=="V26"] <- 'V'
standard_participant_sequences[standard_participant_sequences=="V27"] <- 'V'
standard_participant_sequences[standard_participant_sequences=="V28"] <- 'V'
standard_participant_sequences[standard_participant_sequences=="V29"] <- 'V'
standard_participant_sequences[standard_participant_sequences=="V30"] <- 'V'

#Defining the Sequence Form:

participant.alphab <- c("V", "V1", "V2", "V3", "V4", "V5", "V6", "V7")

participant.lab <- c("Miscellaneous", "Direct-Follow", "TabRepr", "None", "TextDocu", "TraceVarVis", "Dotted Chart", "Line Graph")

participant.scode <- c("V", "V1", "V2", "V3", "V4", "V5", "V6", "V7")

participant.seq <- seqdef(standard_participant_sequences, alphabet = participant.alphab, labels = participant.lab, states = participant.scode)

seqiplot(participant.seq, border = NA, main="Sample of P1 to P10 What Code Sequence", with.legend	="right")

#Ploting All Participants

seqiplot(participant.seq[1:10,], border = NA, main="Sample of P1 to P10 What Code Sequence", with.legend	="right")
seqiplot(participant.seq[11:20,], border = NA, main="Sample of P11 to P20 What Code Sequence", with.legend	="right")
seqiplot(participant.seq[21:30,], border = NA, main="Sample of P21 to P30 What Code Sequence", with.legend	="right")
seqiplot(participant.seq[31:39,], border = NA, main="Sample of P31 to P41 What Code Sequence", with.legend	="right")










