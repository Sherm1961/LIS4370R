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


#Old code for breakdown
#dataKD <- ifelse(data_new$kdRatio <= 0.5, 0, 
#                 ifelse(data_new$kdRatio > 0.5 & data_new$kdRatio <= 1.3, 1, 2)) #Data for kd breakdown

#dataHS <- ifelse(data_new$headshots <= 2000, 0, 
#                 ifelse(data_new$headshots > 2000 & data_new$headshots <= 4000, 1, 2)) #Data for headshot breakdown



#values selected for ranges are 1st Q, and Mean anything higher than mean is advanced range

#new creating new column using weighted values for (KD, Headshots, kills, deaths, average time per round)
#levels of skill: beginner, intermediate, advanced (listed as 0, 1, 2 for easy computation)
#uses first Quartile as first value, then mean    do we want mean or 3rd quartile to show advanced players
dataKD <- ifelse(data_new$kdRatio <= 0.8164, 0, 
                 ifelse(data_new$kdRatio > .8164 & data_new$kdRatio <= 1.0325, 1, 2)) #Data for kd breakdown

dataHS <- ifelse(data_new$headshots <= 255, 0, 
                 ifelse(data_new$headshots > 255 & data_new$headshots <= 1967, 1, 2)) #Data for headshot breakdown

dataK <-  ifelse(data_new$kills <= 1444, 0, 
                 ifelse(data_new$kills > 1444 & data_new$kills <= 11223, 1, 2)) #Data for kills breakdown

dataD <-  ifelse(data_new$deaths <= 1648, 0, 
                 ifelse(data_new$deaths > 1648 & data_new$deaths <= 11580, 1, 2)) #Data for death breakdown

dataAT <-  ifelse(data_new$averageTime <= 2.315, 0, 
                 ifelse(data_new$averageTime > 2.315 & data_new$averageTime <= 4.362, 1, 2)) #Data for death breakdown





#score of previous 4 values added together but also multiplied by their weight 
data_new$weighted <- dataKD + dataHS + dataK + dataD + dataAT    #original scores
data_new$weighted2 <- dataKD*1.3 + dataHS*1.4 + dataK*1.2 + dataD*.8 + dataAT*1.2      #weighted values

#add win rate?
#see kd of each skill bracket. Total


#11.8 is highest score so breakdown starting at 0 to 6 as beginner, 6 to 9 as intermediate and advanceed higher
data_new$SkillLevel <- ifelse(data_new$weighted2 <= 5, "Beginner", 
                              ifelse(data_new$weighted2 > 5 & data_new$weighted2 <= 10, "Intermediate", "Advanced")) #Data for kd breakdown

data_new$SkillLevel_bd <- ifelse(data_new$weighted2 <= 2, "Beginner.1", 
                              ifelse(data_new$weighted2 > 2 & data_new$weighted2 <= 4,
                                     "Beginner.2", 
                                     ifelse(data_new$weighted2 > 4 & data_new$weighted2 <= 5,
                                            "Beginner.3", 
                                            ifelse(data_new$weighted2 > 5 & data_new$weighted2 <= 7, 
                                                   "Intermediate.1", 
                                                   ifelse(data_new$weighted2 > 7 & data_new$weighted2 <= 9,
                                                          "Intermediate.2",
                                                          ifelse(data_new$weighted2 > 9 & data_new$weighted2 <= 10,
                                                                 "Intermediate.3",
                                                                 ifelse(data_new$weighted2 > 10 & data_new$weighted2 <= 10.6,
                                                                        "Advanced.1",
                                                                        ifelse(data_new$weighted2 > 10.6 & data_new$weighted2 <= 11.2,
                                                                               "Advanced.2",
                                                                               ifelse(data_new$weighted2 > 11.2 & data_new$weighted2 <= 11.8,
                                                                                      "Advanced.3",
                                                                                      "Advanced.3"
                                                                               )))))))))




#create count of each skill level
count_beginner <- sum(data_new$SkillLevel == "Beginner")
count_intermediate <- sum(data_new$SkillLevel == "Intermediate")
count_advanced <- sum(data_new$SkillLevel == "Advanced")



#Create 
# Specify split ratio
split_ratio <- .6  # 60% 

# Calculate the number of observations for training
train_indices <- sample(1:nrow(data_new), size = floor(split_ratio * nrow(data_new)))

# Split the data
train_data <- data_new[train_indices, ]
validation_data <- data_new[-train_indices, ]




#Graphs for personal use


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

# Create intervals of 0.5 and categorize the hs ratios
bin1 <- cut(hs_num, breaks = seq(0, 12000, by = 250), right = FALSE)

# Count the number of gamers in each hs bin
hs_count <- table(bin1)

# Create a bar plot
barplot(hs_count, 
        main = "Gamer HS vs Count", 
        xlab = "Headshots Intervals", 
        ylab = "Count", 
        col = "lightblue", 
        border = "black")



#make count of each kills
# Sample kills
kill_ratio <- data_new$kills

# Create intervals of 0.5 and categorize the kill ratios
bin2 <- cut(kill_ratio, breaks = seq(0, 66935, by = 5000), right = FALSE)

# Count the number of gamers in each kill bin
kill_count <- table(bin2)

# Create a bar plot
barplot(kill_count, 
        main = "Gamer kills vs Count", 
        xlab = "Kill Intervals", 
        ylab = "Count", 
        col = "lightblue", 
        border = "black")

#make count of each death
# Sample deaths
d_ratio <- data_new$deaths

# Create intervals of 0.5 and categorize the deaths ratios
bin3 <- cut(d_ratio, breaks = seq(0, 67888, by = 2000), right = FALSE)

# Count the number of gamers in each KD bin
d_count <- table(bin3)

# Create a bar plot
barplot(d_count, 
        main = "Gamer Deaths vs Count", 
        xlab = "Deaths Intervals", 
        ylab = "Count", 
        col = "lightblue", 
        border = "black")
 

#make count of each averageTime
# Sample averageTime
at_ratio <- data_new$averageTime

# Create intervals of 0.5 and categorize the at ratios
bin4 <- cut(at_ratio, breaks = seq(0, 96, by = 1), right = FALSE)

# Count the number of gamers in each KD bin
at_count <- table(bin4)

# Create a bar plot
barplot(d_count, 
        main = "Gamer Deaths vs Count", 
        xlab = "Deaths Intervals", 
        ylab = "Count", 
        col = "lightblue", 
        border = "black")



#make count of skill
skill_ratio <- data_new$weighted  #unweighted values

# Create intervals of 0.5 and categorize the KD ratios
bin5 <- cut(skill_ratio, breaks = seq(0, 10, by = 3), right = FALSE)

# Count the number of gamers in each KD bin
skill_count <- table(bin5)

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
bin6 <- cut(skill_ratio, breaks = seq(0, 12, by = 4), right = FALSE)

# Count the number of gamers in each KD bin
skill_count <- table(bin6)

# Create a bar plot of 3 skill levels
barplot(skill_count, 
        main = "Gamer Skill weighted vs Count", 
        xlab = "Skill Ratio Intervals", 
        ylab = "Count", 
        col = "lightblue", 
        border = "black")



#summary(data_new)
#count of advanced players with wins less than 340 (average of dataset)
count_unskilled_advanced <- sum(data_new$SkillLevel == "Advanced" & data_new$wins <= 340)

#9 unskilled advanced players
advanced_unskilled_players <- data_new$name[data_new$SkillLevel == "Advanced" & data_new$wins <= 340]
advanced_unskilled_players


