#Load in file 
df <- read.csv("C:\\Murray State University\\Graduate School\\Fall 2023\\Masters Thesis 2023-2024\\Data\\Cleaning\\cleaning.file.csv")


#Subset key columns 
df_ex <- data.frame(df[19:21], df[23:75], df[95:96], df[101:102], df[107], 
                    df[112:113], df[118:120], df[125:127], df[132:133], 
                    df[138:140], df[145:148], df[153:156], df[161:162], 
                    df[167], df[172:173], df[178:179], df[184:188], 
                    df[193:194], df[199:201], df[206:207], df[212:213], 
                    df[218:219], df[224:233], df[234:244])

#Remove rows 1&2: FOR NOW IT IS 24 ROWS
df_ex1 <- df_ex[3:24, ]

#Remove unnecessary data frames
remove(df_ex)
df <- df_ex1
rm(df_ex1)

pos_target_words <- c('proud', 'glad', 'merry', 'smile', 'excite', 'joy',
                      'glee', 'happy', 'cheer','jolly')

neg_target_words <- c('fear', 'guilt', 'hostile','scared', 'nervous','tense',
                      'upset', 'distress', 'afraid', 'frown')

pos_target_fragments <- c('ud', 'gl', 'mr', 'ile', 'cite','j', 'gl', 'ha', 'ee',
                          'jo')

neg_target_fragments <- c('ar', 'g', 'hos', 'sce', 'nerv','te', 'up', 'tress', 
                          'rai','fw')

# Create combined words
df$W1 <- tolower(paste0(df$WF1_1, df$WF1_2))
df$W2 <- tolower(paste0(df$WF2_1, df$WF2_2, df$WF2_3))
df$W3 <- tolower(paste0(df$WF3_1))
df$W4 <- tolower(paste0(df$WF4_1, df$WF4_2))
df$W5 <- tolower(paste0(df$WF5_1, df$WF5_2, df$WF5_3))
df$W6 <- tolower(paste0(df$WF5_1.1, df$WF5_2.1, df$WF5_3.1))
df$W7 <- tolower(paste0(df$WF7_1, df$WF7_2, df$WF7_3))
df$W8 <- tolower(paste0(df$WF8_1, df$WF8_2, df$WF8_3))
df$W9 <- tolower(paste0(df$WF9_1, df$WF9_2, df$WF9_3, df$WF9_4))
df$W10 <- tolower(paste0(df$WF10_1, df$WF10_2, df$WF10_3, df$WF10_4))
df$W11 <- tolower(paste0(df$WF11_1, df$WF11_2))
df$W12 <- tolower(paste0(df$WF12_1))
df$W13 <- tolower(paste0(df$WF13_1, df$WF13_2))
df$W14 <- tolower(paste0(df$WF14_1, df$WF14_2))
df$W15 <- tolower(paste0(df$WF15_1, df$WF15_2, df$WF15_3, df$WF15_4, df$WF15_5))
df$W16 <- tolower(paste0(df$WF16_1, df$WF16_2))
df$W17 <- tolower(paste0(df$WF17_1, df$WF17_2, df$WF17_3))
df$W18 <- tolower(paste0(df$WF18_1, df$WF18_2))
df$W19 <- tolower(paste0(df$WF19_1, df$WF19_2))
df$W20 <- tolower(paste0(df$WF20_1, df$WF20_2))

full_frame_pos <- tolower(data.frame(df['WF2_1'], df['WF4_1'],df['WF7_1'], 
                                     df['WF8_1'],df['WF10_1'], df['WF12_1'], 
                                     df['WF14_1'], df['WF16_1'],
                                     df['WF18_1'], df['WF20_1']))

full_frame_neg <- tolower(data.frame(df['WF1_1'], df['WF3_1'],  df['WF5_1'],
                                     df['WF5_1.1'], df['WF9_1'],
                                     df['WF11_1'], df['WF13_1'],
                                     df['WF15_1'], df['WF17_1'],
                                     df['WF19_1']))

pos_frag_vec <- c()
neg_frag_vec <- c()
pos_word_vec <- c()
neg_word_vec <- c()

for (r in 1:nrow(df)) {
  pos_frag_vec <- c(pos_frag_vec, 10-sum(is.na(match(df['W2','W4', 'W7','W8',
                                                        'W10','W12','W14','W16',
                                                        'W18','W20'], 
                                                    pos_target_fragments))))
  neg_frag_vec <- c(neg_frag_vec, 10-sum(is.na(match(df['W1','W3','W5','W6',
                                                        'W9','W11','W13','W15',
                                                        'W17','W19'], 
                                                     neg_target_fragments))))
  pos_word_vec <- c(pos_word_vec, 10-sum(is.na(match(full_frame_pos[r,], pos_target_words))))
  neg_word_vec <- c(neg_word_vec, 10-sum(is.na(match(full_frame_neg[r,], neg_target_words))))
}

df$pos_score <- pos_frag_vec + pos_word_vec
df$neg_score <- neg_frag_vec + neg_word_vec

#Block of code for word fragment completion & scoring
#WF1
#Set your word fragment
df_ex1$wf1 <- c("fe")
#Paste completed word
df_ex1$wf1c <- ifelse(nchar(df_ex1$WF1_1) == 1, 
                      paste0(df_ex1$wf1, df_ex1$WF1_1, df_ex1$WF1_2),
                      paste0(df_ex1$WF1_1))
#For fragments with multiple blanks,
#If they put multiple answers in one blank, adjust for signal 
x1 <- as.vector(df_ex1$wf1c)
x1[x1 == "ar"] <- "fear"
df_ex1$wf1c <- x1
#If they used upper case letters, set to lower case
df_ex1$wf1c <- tolower(df_ex1$wf1c)
#Create a score 
df_ex1$wf1s <- ifelse(df_ex1$wf1c == "fear", 1, 0)

#Do the same for all word fragments
#WF2
df_ex1$wf2 <- c("pro")
df_ex1$wf2c <- ifelse(nchar(df_ex1$WF2_1) == 1, 
                      paste0(df_ex1$wf2, df_ex1$WF2_1, df_ex1$WF2_2), 
                      paste0(df_ex1$WF2_1))
x2 <- as.vector(df_ex1$wf2c)
x2[x2 == "ud"] <- "proud"
df_ex1$wf2c <- x2
df_ex1$wf2c <- tolower(df_ex1$wf2c)
df_ex1$wf2s <- ifelse(df_ex1$wf2c == "proud", 1, 0)


#WF3
df_ex1$wf3 <- c("uilt")
df_ex1$wf3c <- ifelse(nchar(df_ex1$WF3_1) == 1, 
                      paste0(df_ex1$WF3_1, df_ex1$wf3), 
                      paste0(df_ex1$WF3_1))
df_ex1$wf3c <- tolower(df_ex1$wf3c)
df_ex1$wf3s <- ifelse(df_ex1$wf3c == "guilt", 1, 0)

#WF4
df_ex1$wf4 <- c("ad")
df_ex1$wf4c <- ifelse(nchar(df_ex1$WF4_1) == 1, 
                      paste0(df_ex1$WF4_1, df_ex1$WF4_2, df_ex1$wf4), 
                      paste0(df_ex1$WF4_1))
x4 <- as.vector(df_ex1$wf4c)
x4[x4 == "gl"] <- "glad"
df_ex1$wf4c <- x4
df_ex1$wf4c <- tolower(df_ex1$wf4c)
df_ex1$wf4s <- ifelse(df_ex1$wf4c == "glad", 1, 0)

#WF5
df_ex1$wf5 <- c("tile")
df_ex1$wf5c <- ifelse(nchar(df_ex1$WF5_1) == 1, 
                      paste0(df_ex1$WF5_1, df_ex1$WF5_2, df_ex1$WF5_3, 
                             df_ex1$wf5), 
                      paste0(df_ex1$WF5_1))
x5 <- as.vector(df_ex1$wf5c)
x5[x5 == "hos"] <- "hostile"
df_ex1$wf5c <- x5
df_ex1$wf5c <- tolower(df_ex1$wf5c)
df_ex1$wf5s <- ifelse(df_ex1$wf5c == "hostile", 1, 0)

#WF6  
df_ex1$wf6a <- c("ar")
df_ex1$wf6b <- c("d")
df_ex1$wf6c <- ifelse(nchar(df_ex1$WF5_1.1) == 1, 
                      paste0(df_ex1$WF5_1.1, df_ex1$WF5_2.1, df_ex1$wf6a, 
                             df_ex1$WF5_3.1, df_ex1$wf6b), 
                      paste0(df_ex1$WF5_1.1))
x6 <- as.vector(df_ex1$wf6c)
x6[x6 == "sce"] <- "scared"
df_ex1$wf6c <- x6
df_ex1$wf6c <- tolower(df_ex1$wf6c)
df_ex1$wf6s <- ifelse(df_ex1$wf6c == "scared", 1, 0)

#WF7
df_ex1$wf7a <- c("er")
df_ex1$wf7b <- c("y")
df_ex1$wf7c <- ifelse(nchar(df_ex1$WF7_1) == 1, 
                      paste0(df_ex1$WF7_1, df_ex1$wf7a, df_ex1$WF7_2, 
                             df_ex1$wf7b), 
                      paste0(df_ex1$WF7_1))
x7 <- as.vector(df_ex1$wf7c)
x7[x7 == "mr"] <- "merry"
df_ex1$wf7c <- x7
df_ex1$wf7c <- tolower(df_ex1$wf7c)
df_ex1$wf7s <- ifelse(df_ex1$wf7c == "merry", 1, 0)

#WF8
df_ex1$wf8 <- c("sm")
df_ex1$wf8c <- ifelse(nchar(df_ex1$WF8_1) == 1, 
                      paste0(df_ex1$wf8, df_ex1$WF8_1, df_ex1$WF8_2, 
                             df_ex1$WF8_3), 
                      paste0(df_ex1$WF8_1))
x8 <- as.vector(df_ex1$wf8c)
x8[x8 == "ile"] <- "smile"
df_ex1$wf8c <- x8
df_ex1$wf8c <- tolower(df_ex1$wf8c)
df_ex1$wf8s <- ifelse(df_ex1$wf8c == "smile", 1, 0)

#WF9
df_ex1$wf9 <- c("ous")
df_ex1$wf9c <- ifelse(nchar(df_ex1$WF9_1) == 1, 
                      paste0(df_ex1$WF9_1, df_ex1$WF9_2, df_ex1$WF9_3, 
                             df_ex1$WF9_4, df_ex1$wf9), 
                      paste0(df_ex1$WF9_1))
x9 <- as.vector(df_ex1$wf9c)
x9[x9 == "nerv"] <- "nervous"
df_ex1$wf9c <- x9
df_ex1$wf9c <- tolower(df_ex1$wf9c)
df_ex1$wf9s <- ifelse(df_ex1$wf9c == "nervous", 1, 0)

#WF10  
df_ex1$wf10 <- c("ex")
df_ex1$wf10c <- ifelse(nchar(df_ex1$WF10_1) == 1, 
                       paste0(df_ex1$wf10, df_ex1$WF10_1, df_ex1$WF10_2, 
                              df_ex1$WF10_3, df_ex1$WF10_4), 
                       paste0(df_ex1$WF10_1))
x10 <- as.vector(df_ex1$wf10c)
x10[x10 == "cite"] <- "excite"
df_ex1$wf10c <- x10
df_ex1$wf10c <- tolower(df_ex1$wf10c)
df_ex1$wf10s <- ifelse(df_ex1$wf10c == "excite", 1, 0)

#WF11  
df_ex1$wf11 <- c("nse")
df_ex1$wf11c <- ifelse(nchar(df_ex1$WF11_1) == 1, 
                       paste0(df_ex1$WF11_1, df_ex1$WF11_2, df_ex1$wf11), 
                       paste0(df_ex1$WF11_1))
x11 <- as.vector(df_ex1$wf11c)
x11[x11 == "te"] <- "tense"
df_ex1$wf11c <- x11
df_ex1$wf11c <- tolower(df_ex1$wf11c)
df_ex1$wf11s <- ifelse(df_ex1$wf11c == "tense", 1, 0)

#WF12
df_ex1$wf12 <- c("oy")
df_ex1$wf12c <- ifelse(nchar(df_ex1$WF12_1) == 1, 
                       paste0(df_ex1$WF12_1, df_ex1$wf12), 
                       paste0(df_ex1$WF12_1))
df_ex1$wf12c <- tolower(df_ex1$wf12c)
df_ex1$wf12s <- ifelse(df_ex1$wf12c == "joy", 1, 0)

#WF13  
df_ex1$wf13 <- c("set")
df_ex1$wf13c <- ifelse(nchar(df_ex1$WF13_1) == 1, 
                       paste0(df_ex1$WF13_1, df_ex1$WF13_2, df_ex1$wf13), 
                       paste0(df_ex1$WF13_1))
x13 <- as.vector(df_ex1$wf13c)
x13[x13 == "up"] <- "upset"
df_ex1$wf13c <- x13
df_ex1$wf13c <- tolower(df_ex1$wf13c)
df_ex1$wf13s <- ifelse(df_ex1$wf13c == "upset", 1, 0)

#WF14  
df_ex1$wf14 <- c("ee")
df_ex1$wf14c <- ifelse(nchar(df_ex1$WF14_1) == 1, 
                       paste0(df_ex1$WF14_1, df_ex1$WF14_2, df_ex1$wf14), 
                       paste0(df_ex1$WF14_1))
x14 <- as.vector(df_ex1$wf14c)
x14[x14 == "gl"] <- "glee"
df_ex1$wf14c <- x14
df_ex1$wf14c <- tolower(df_ex1$wf14c)
df_ex1$wf14s <- ifelse(df_ex1$wf14c == "glee", 1, 0)

#WF15  
df_ex1$wf15 <- c("dis")
df_ex1$wf15c <- ifelse(nchar(df_ex1$WF15_1) == 1, 
                       paste0(df_ex1$wf15, df_ex1$WF15_1, df_ex1$WF15_2, 
                              df_ex1$WF15_3, df_ex1$WF15_4, df_ex1$WF15_5), 
                       paste0(df_ex1$WF15_1))
x15 <- as.vector(df_ex1$wf15c)
x15[x15 == "tress"] <- "distress"
df_ex1$wf15c <- x15
df_ex1$wf15c <- tolower(df_ex1$wf15c)
df_ex1$wf15s <- ifelse(df_ex1$wf15c == "distress", 1, 0)

#WF16  
df_ex1$wf16 <- c("ppy")
df_ex1$wf16c <- ifelse(nchar(df_ex1$WF16_1) == 1, 
                       paste0(df_ex1$WF16_1, df_ex1$WF16_2, df_ex1$wf16), 
                       paste0(df_ex1$WF16_1))
x16 <- as.vector(df_ex1$wf16c)
x16[x16 == "ha"] <- "happy"
df_ex1$wf16c <- x16
df_ex1$wf16c <- tolower(df_ex1$wf16c)
df_ex1$wf16s <- ifelse(df_ex1$wf16c == "happy", 1, 0)

#WF17  
df_ex1$wf17a <- c("af")
df_ex1$wf17b <- c("d")
df_ex1$wf17c <- ifelse(nchar(df_ex1$WF17_1) == 1, 
                       paste0(df_ex1$wf17a, df_ex1$WF17_1, df_ex1$WF17_2, 
                              df_ex1$WF17_3, df_ex1$wf17b), 
                       paste0(df_ex1$WF17_1))
x17 <- as.vector(df_ex1$wf17c)
x17[x17 == "rai"] <- "afraid"
df_ex1$wf17c <- x17
df_ex1$wf17c <- tolower(df_ex1$wf17c)
df_ex1$wf17s <- ifelse(df_ex1$wf17c == "afraid", 1, 0)

#WF18  
df_ex1$wf18a <- c("ch")
df_ex1$wf18b <- c("r")
df_ex1$wf18c <- ifelse(nchar(df_ex1$WF18_1) == 1, 
                       paste0(df_ex1$wf18a, df_ex1$WF18_1, df_ex1$WF18_2, 
                              df_ex1$wf18b), 
                       paste0(df_ex1$WF18_1))
x18 <- as.vector(df_ex1$wf18c)
x18[x18 == "ha"] <- "happy"
df_ex1$wf18c <- x18
df_ex1$wf18c <- tolower(df_ex1$wf18c)
df_ex1$wf18s <- ifelse(df_ex1$wf18c == "cheer", 1, 0)

#WF19  
df_ex1$wf19a <- c("ro")
df_ex1$wf19b <- c("n")
df_ex1$wf19c <- ifelse(nchar(df_ex1$WF19_1) == 1, 
                       paste0(df_ex1$WF19_1, df_ex1$wf19a, df_ex1$WF19_2, 
                              df_ex1$wf19b), 
                       paste0(df_ex1$WF19_1))
x19 <- as.vector(df_ex1$wf19c)
x19[x19 == "fr"] <- "frown"
df_ex1$wf19c <- x19
df_ex1$wf19c <- tolower(df_ex1$wf19c)
df_ex1$wf19s <- ifelse(df_ex1$wf19c == "frown", 1, 0)

#WF20  
df_ex1$wf20 <- c("lly")
df_ex1$wf20c <- ifelse(nchar(df_ex1$WF20_1) == 1,
                       paste0(df_ex1$WF20_1, df_ex1$WF20_2, df_ex1$wf20), 
                       paste0(df_ex1$WF20_1))
x20 <- as.vector(df_ex1$wf20c)
x20[x20 == "jo"] <- "jolly"
df_ex1$wf20c <- x20
df_ex1$wf20c <- tolower(df_ex1$wf20c)
df_ex1$wf20s <- ifelse(df_ex1$wf20c == "jolly", 1, 0)

#Create your subscales and composite scores
#Trait Empathy 
df_TE_chr <- df_ex1[10:28]
df_TE_num <- data.frame(sapply(df_TE_chr, as.numeric))
df_TE_num$PT_avg <- rowMeans(df_TE_num[, c(1:6)])
df_TE_num$EC_avg <- rowMeans(df_TE_num[, c(7:12)])
df_TE_num$Econt_avg <- rowMeans(df_TE_num[, c(13:19)])
df_TE_num$TE_avg <- rowMeans(df_TE_num[, c(1:19)])

#State Empathy
df_SE_chr <- df_ex1[45:56]
df_SE_num <- data.frame(sapply(df_SE_chr, as.numeric))
df_SE_num$AE <- rowMeans(df_SE_num[, c(1:4)])
df_SE_num$CE <- rowMeans(df_SE_num[, c(5:8)])
df_SE_num$AsE <- rowMeans(df_SE_num[, c(9:12)])
df_SE_num$SE_avg <- rowMeans(df_SE_num[, c(1:12)])

#Word Fragment Scores 
df_ex1$WFPS <- rowSums(df_ex1[, c("wf2s", "wf4s", "wf6s", "wf8s", "wf10s", 
                                  "wf12s", "wf14s", "wf16s", "wf18s", "wf20s")])
df_ex1$WFNS <- rowSums(df_ex1[, c("wf1s", "wf3s", "wf5s", "wf7s", "wf9s", 
                                  "wf11s", "wf13s", "wf15s", "wf17s", "wf19s")])


#PANAS 
df_PAN_chr <- data.frame(df_ex1[106:115], df_ex1[116:126])
df_PAN_num <- data.frame(sapply(df_PAN_chr, as.numeric))
df_PAN_num$df_PAN_pos_score <- rowSums(df_PAN_num[, c("P1", "P3", "P5", "P9", 
                                                      "P10", "P14", "P16", 
                                                      "P18", "P19", "P21")])
df_PAN_num$df_PAN_neg_score <- rowSums(df_PAN_num[, c("P2", "P4", "P6", "P7", 
                                                      "P8", "P13", "P15", "P17",
                                                      "P20", "P22")])

#Return a master data frame with the bare minimum 
df <- data.frame(df_ex1[1:9])
df <- cbind(df, df_TE_num[, c(1:23)])
#Group 1 (High E X High DC): Better way to reorganize?
df$E_H_DC_H_Response <- df_ex1$HHResponse
df$E_H_DC_H_Name <- df_ex1$NameHH
df$E_H_DC_H_MC1 <- df_ex1$MCHH1
df$E_H_DC_H_MC2 <- df_ex1$MCHH2
#Group 2 (High E X Low DC)
df$E_H_DC_L_Response <- df_ex1$HLResponse
df$E_H_DC_L_Name <- df_ex1$NameHL
df$E_H_DC_L_MC1 <- df_ex1$MCHl1
df$E_H_DC_L_MC2 <- df_ex1$MCHL2
#Group 3 (Low E X High DC)
df$E_L_DC_H_Response <- df_ex1$LHResponse
df$E_L_DC_H_Name <- df_ex1$NameLH
df$E_L_DC_H_MC1 <- df_ex1$MCLH1
df$E_L_DC_H_MC2 <- df_ex1$MCLH2
#Group 4(Low E X Low DC)
df$E_L_DC_L_Response <- df_ex1$LLResponse
df$E_L_DC_L_Name <- df_ex1$NameLL
df$E_L_DC_L_MC1 <- df_ex1$MCLL1
df$E_L_DC_L_MC2 <- df_ex1$MCLL2
#State Empathy 
df <- cbind(df, df_SE_num[, c(1:16)])
#WFC Task 
df <- cbind(df, df_ex1[, c(192:193)])
#PANAS
df <- cbind(df, df_PAN_num[, c(1:23)])

#Clean Environment 
remove(df_PAN_chr, df_PAN_num, df_SE_chr, df_SE_num, df_TE_chr, df_TE_num)
remove(x1, x2, x4, x5, x6, x7, x8, x9, x10, x11, x13, x14, x15, x16, x17, x18,
       x19, x20)

#Export 
write.csv(df, file = "cleaned.thesis.data")