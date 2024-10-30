#set working directory
setwd("C:/Users/sherm/OneDrive/Desktop/PreAnalytics")

#library
library(dplyr)
library(ggplot2)

#read in data
data<-read.csv("cod.csv")
summary(data) #summary to overlook data


#Main focus now is clean data based on KD, score per min, headshots and time played. Get rid of outliers and
#make cleaned dataset "data_new". 
#Removed players with less than 10 games played as you cant get an accurate read on skill (may have previous 
#expirience or they have fluke games skewing their game metrics)

#We want to make a weighted value of player skill, would be categorical (Beginner, Intermediate, Advanced, Skilled)
#to see how skilled players are


#KD, score per min, headshots and time played.



#kdRatio dataset
# Filter rows where kdRatio is exactly 0 and deaths is 0
zero_kd_data <- data[!is.na(data$kdRatio) & data$kdRatio == 0 & data$deaths == 0, ]

# Display the players with 0 kdRatio
zero_kd_players <- zero_kd_data$`name`
print(zero_kd_players)

# Remove rows from data that have zero_kd_data
data_filtered_zkd <- anti_join(data, zero_kd_data, by = c("kdRatio", "name"))
print(data_filtered_zkd)




#headshots dataset
# Filter rows where headshots is exactly 0 and kills is 0
zero_headshots_data <- data[!is.na(data$headshots) & data$headshots == 0 & data$kills == 0, ]

# Display the players with 0 headshots
zero_headshots_players <- zero_headshots_data$`name`
print(zero_headshots_players)

# Remove rows from data that have zero_headshots_data
data_filtered_zhs <- anti_join(data, zero_headshots_data, by = c("headshots", "name"))
print(data_filtered_zhs)



#score per minute dataset
# Filter rows where score per minute is exactly 0 and kills is 0
zero_score_data <- data[!is.na(data$scorePerMinute) & data$scorePerMinute == 0 & data$kills == 0, ]

# Display the players with 0 scoreperminute
zero_score_players <- zero_score_data$`name`
print(zero_score_players)

# Remove rows from data that have zero_score_data
data_filtered_zspm <- anti_join(data, zero_score_data, by = c("scorePerMinute", "name"))
print(data_filtered_zspm)



#time played dataset
# Filter rows where timePlayed is exactly 0
zero_time_data <- data[!is.na(data$timePlayed) & data$timePlayed == 0, ]

#a_zero_time<- count(data[!is.na(data$timePlayed) & data$timePlayed == 0, ]) #210 with 0 hours played
#a_0_5_time<- count(data[!is.na(data$timePlayed) & data$timePlayed <= 5, ]) #423 with 5 or less hours
# Display the players with 0 timePlayed
zero_time_players <- zero_time_data$`name`
print(zero_time_players)

# Remove rows from data that have zero_time_data
data_filtered_ztp <- anti_join(data, zero_time_data, by = c("timePlayed", "name"))
print(data_filtered_ztp)



#gamesPlayed dataset
# Filter rows where gamesPlayed is 10 or less
zero_gp10_data <- data[!is.na(data$gamesPlayed) & data$gamesPlayed <= 10, ]

# Display the players with 0 games Played
zero_gp10_players <- zero_gp10_data$`name`
print(zero_gp10_players)

# Remove rows from data that have zero_gp_data
data_filtered_gp10 <- anti_join(data, zero_gp10_data, by = c("gamesPlayed", "name"))
print(data_filtered_gp10)



#remove data using anti_join to make new data
data_new <- anti_join(data, zero_kd_data, by = c("kdRatio", "name"))
data_new <- anti_join(data_new, zero_time_data, by = c("timePlayed", "name"))
data_new <- anti_join(data_new, zero_score_data, by = c("scorePerMinute", "name"))
data_new <- anti_join(data_new, zero_headshots_data, by = c("headshots", "name"))
data_new <- anti_join(data_new, zero_gp10_data, by = c("gamesPlayed", "name"))

summary(data_new)



#creating new column using weighted values for KD, Headshots, scorePerMin, timePlayed
#levels of skill: beginner, intermediate, advanced (listed as 0, 1, 2 for easy computation)
dataKD <- ifelse(data_new$kdRatio <= 0.5, 0, 
                 ifelse(data_new$kdRatio > 0.5 & data_new$kdRatio <= 1.3, 1, 2)) #Data for kd breakdown

dataHS <- ifelse(data_new$headshots <= 2000, 0, 
                 ifelse(data_new$headshots > 2000 & data_new$headshots <= 4000, 1, 2)) #Data for headshot breakdown

dataSPM <- ifelse(data_new$scorePerMinute <= 140, 0, 
                 ifelse(data_new$scorePerMinute > 140 & data_new$scorePerMinute <= 280, 1, 2)) #Data for scorePerMin breakdown

dataTP <- ifelse(data_new$timePlayed <= 500, 0, 
                  ifelse(data_new$timePlayed > 500 & data_new$timePlayed <= 1400, 1, 2)) #Data for timePlayed breakdown


#score of previous 4 values added together but also multiplied by their weight 
data_new$weighted <- dataKD + dataHS + dataSPM + dataTP    #original scores
data_new$weighted2 <- dataKD*1.2 + dataHS*1.4 + dataSPM*.3 + dataTP*1.1       #weighted values

#add win rate?
#see kd of each skill bracket. Total


# 8 is highest score so breakdown by 3
data_new$SkillLevel <- ifelse(data_new$weighted2 <= 3, "Beginner", 
                              ifelse(data_new$weighted2 > 3 & data_new$weighted2 <= 6, "Intermediate", "Advanced")) #Data for kd breakdown



#


#make count of each kdRatio for all players      get from github
# Sample KD ratios for multiple gamers

#figure out big 5 values (KD, Headshots, kills, deaths, average time per round)

#wins (changed due to squads carrying, or new player is actual skilled with lots of previous experience)









#make count of each kdRatio for Beginner players
# Sample KD ratios for multiple gamers

kd_ratio_beg <- subset(data_new, SkillLevel == 'Beginner')

# Create intervals of 0.5 and categorize the KD ratios
bin <- cut(kd_ratio_beg$kdRatio, breaks = seq(0, 3, by = 0.1), right = FALSE)

# Count the number of gamers in each KD bin
kd_count <- table(bin)

# Create a bar plot
barplot(kd_count, 
        main = "Gamer KD vs Count (0.5 Intervals)", 
        xlab = "KD Ratio Intervals", 
        ylab = "Count", 
        col = "lightblue", 
        border = "black")


#make count of each kdRatio for Intermediate players
# Sample KD ratios for multiple gamers

kd_ratio_int <- subset(data_new, SkillLevel == 'Intermediate')

# Create intervals of 0.5 and categorize the KD ratios
bin <- cut(kd_ratio_int$kdRatio, breaks = seq(0, 3, by = 0.1), right = FALSE)

# Count the number of gamers in each KD bin
kd_count <- table(bin)

# Create a bar plot
barplot(kd_count, 
        main = "Gamer KD vs Count (0.5 Intervals)", 
        xlab = "KD Ratio Intervals", 
        ylab = "Count", 
        col = "lightblue", 
        border = "black")




#make count of each kdRatio for advanced players
# Sample KD ratios for multiple gamers

kd_ratio_adv <- subset(data_new, SkillLevel == 'Advanced')

# Create intervals of 0.5 and categorize the KD ratios
bin <- cut(kd_ratio_adv$kdRatio, breaks = seq(0, 3, by = 0.1), right = FALSE)

# Count the number of gamers in each KD bin
kd_count <- table(bin)

# Create a bar plot
barplot(kd_count, 
        main = "Gamer KD vs Count (0.5 Intervals)", 
        xlab = "KD Ratio Intervals", 
        ylab = "Count", 
        col = "lightblue", 
        border = "black")


  

#make count of each headshot
# Sample headshots
hs_num <- data_new$headshots

# Create intervals of 0.5 and categorize the KD ratios
bin1 <- cut(hs_num, breaks = seq(0, 12000, by = 250), right = FALSE)

# Count the number of gamers in each KD bin
hs_count <- table(bin1)

# Create a bar plot
barplot(hs_count, 
        main = "Gamer HS vs Count", 
        xlab = "Headshots Intervals", 
        ylab = "Count", 
        col = "lightblue", 
        border = "black")



#make count of each ScorePerMinute
# Sample scorePerMinute
spm_ratio <- data_new$scorePerMinute

# Create intervals of 0.5 and categorize the KD ratios
bin2 <- cut(spm_ratio, breaks = seq(0, 415, by = 20), right = FALSE)

# Count the number of gamers in each KD bin
spm_count <- table(bin2)

# Create a bar plot
barplot(spm_count, 
        main = "Gamer spm vs Count", 
        xlab = "ScorePerMinute Intervals", 
        ylab = "Count", 
        col = "lightblue", 
        border = "black")

#make count of each timePlayed
# Sample timePlayed
tp_ratio <- data_new$timePlayed

# Create intervals of 0.5 and categorize the KD ratios
bin3 <- cut(tp_ratio, breaks = seq(0, 7480, by = 200), right = FALSE)

# Count the number of gamers in each KD bin
tp_count <- table(bin3)

# Create a bar plot
barplot(tp_count, 
        main = "Gamer tp vs Count", 
        xlab = "TimePlayed Intervals", 
        ylab = "Count", 
        col = "lightblue", 
        border = "black")
 

#make count of skill
skill_ratio <- data_new$weighted  #unweighted values

# Create intervals of 0.5 and categorize the KD ratios
bin4 <- cut(skill_ratio, breaks = seq(0, 9, by = 3), right = FALSE)

# Count the number of gamers in each KD bin
skill_count <- table(bin4)

# Create a bar plot of 3 skill levels
barplot(skill_count, 
        main = "Gamer Skill unweighted vs Count", 
        xlab = "KD Ratio Intervals", 
        ylab = "Count", 
        col = "lightblue", 
        border = "black")


#make count of skill
skill_ratio <- data_new$weighted2  #weighted values

# Create intervals of 0.5 and categorize the KD ratios
bin5 <- cut(skill_ratio, breaks = seq(0, 9, by = 3), right = FALSE)

# Count the number of gamers in each KD bin
skill_count <- table(bin5)

# Create a bar plot of 3 skill levels
barplot(skill_count, 
        main = "Gamer Skill weighted vs Count", 
        xlab = "Skill Ratio Intervals", 
        ylab = "Count", 
        col = "lightblue", 
        border = "black")










#gamesPlayed dataset
# Filter rows where gamesPlayed is exactly 0
#zero_gp_data <- data[!is.na(data$gamesPlayed) & data$gamesPlayed == 0, ]

# Display the players with 0 games Played
#zero_gp_players <- zero_gp_data$`name`
#print(zero_gp_players)

# Remove rows from data that have zero_gp_data
#data_filtered_gp <- anti_join(data, zero_gp_data, by = c("gamesPlayed", "name"))
#print(data_filtered_gp)







#597 with 0.00000000 score per minute with only 269 of those with any kills
#scorePerMinute_desc <- sort(data$scorePerMinute, decreasing = FALSE)
#print(scorePerMinute_desc)

#92 players have kd of 6 or less than 7
#717 players with kd between 0.5 and 1
#we have a bunch of people with 0 games played




#NOTES
#zero_headshots_data <- data[!is.na(data$headshots) & data$headshots == 0 & data$kills == 0, ]
#
#headshots_desc <- sort(data$headshots, decreasing = FALSE)
#print(headshots_desc)


#time_desc <- sort(data$timePlayed, decreasing = TRUE)
#print(time_desc)

#time_desc <- sort(data$timePlayed, decreasing = TRUE)
#print(time_desc)

#time_updated <- time_desc[-(1:4), ]

#df_sorted <- df[order(df$age), ]
#print(df_sorted)

#kdRatio_desc <- sort(data$kdRatio, decreasing = TRUE)
#print(kdRatio_desc)









