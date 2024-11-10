


#library
library(dplyr)
library(ggplot2)
library(caret)
library(class)
library(rpart) 
library(rpart.plot)
library(dplyr)

data <- read.csv("cod.csv")
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
zero_kd_data <- data[!is.na(data$kdRatio) &
                       data$kdRatio == 0 & data$deaths == 0, ]

# Display the players with 0 kdRatio
zero_kd_players <- zero_kd_data$`name`
print(zero_kd_players)

# Remove rows from data that have zero_kd_data
data_filtered_zkd <- anti_join(data, zero_kd_data, by = c("kdRatio", "name"))
print(data_filtered_zkd)




#headshots dataset
# Filter rows where headshots is exactly 0 and kills is 0
zero_headshots_data <- data[!is.na(data$headshots) &
                              data$headshots == 0 & data$kills == 0, ]

# Display the players with 0 headshots
zero_headshots_players <- zero_headshots_data$`name`
print(zero_headshots_players)

# Remove rows from data that have zero_headshots_data
data_filtered_zhs <- anti_join(data, zero_headshots_data, by = c("headshots", "name"))
print(data_filtered_zhs)



#score per minute dataset
# Filter rows where score per minute is exactly 0 and kills is 0
zero_score_data <- data[!is.na(data$scorePerMinute) &
                          data$scorePerMinute == 0 & data$kills == 0, ]

# Display the players with 0 scoreperminute
zero_score_players <- zero_score_data$`name`
print(zero_score_players)

# Remove rows from data that have zero_score_data
data_filtered_zspm <- anti_join(data, zero_score_data, by = c("scorePerMinute", "name"))
print(data_filtered_zspm)



#time played dataset
# Filter rows where timePlayed is exactly 0
zero_time_data <- data[!is.na(data$timePlayed) &
                         data$timePlayed == 0, ]

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
zero_gp10_data <- data[!is.na(data$gamesPlayed) &
                         data$gamesPlayed <= 10, ]

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
#uses first Quartile as first value, then 3rd quartile to show advanced players
dataKD <- ifelse(
  data_new$kdRatio <= 0.8164,
  0,
  ifelse(data_new$kdRatio > .8164 &
           data_new$kdRatio <= 1.0325, 1, 2)
) #Data for kd breakdown

dataHS <- ifelse(
  data_new$headshots <= 255,
  0,
  ifelse(data_new$headshots > 255 &
           data_new$headshots <= 1967, 1, 2)
) #Data for headshot breakdown

dataK <-  ifelse(data_new$kills <= 1444,
                 0,
                 ifelse(data_new$kills > 1444 &
                          data_new$kills <= 11223, 1, 2)) #Data for kills breakdown

dataD <-  ifelse(
  data_new$deaths <= 1648,
  0,
  ifelse(data_new$deaths > 1648 &
           data_new$deaths <= 11580, 1, 2)
) #Data for death breakdown

dataAT <-  ifelse(
  data_new$averageTime <= 2.315,
  0,
  ifelse(data_new$averageTime > 2.315 &
           data_new$averageTime <= 4.362, 1, 2)
) #Data for average time breakdown

dataHM <-  ifelse(
  data_new$accuracy_score <= 0.1569,
  0,
  ifelse(data_new$accuracy_score > 0.1569 &
           data_new$accuracy_score <= 0.2082, 1, 2)
) #Data for hit/miss breakdown

dataHS_rate <-  ifelse(
  data_new$hs_rate <= 0.04498,
  0,
  ifelse(data_new$hs_rate > 0.04498 &
           data_new$hs_rate <= 0.07878, 1, 2)
) #Data for hit/miss breakdown

datawin_rate <-  ifelse(
  data_new$win_rate <= 0.9117,
  0,
  ifelse(data_new$win_rate > 0.9117   &
           data_new$win_rate <= 0.9780, 1, 2)
) #Data for win_rate breakdown


#creating new variables
data_new$accuracy_score <-  (data_new$hits)/(data_new$misses + data_new$hits)
data_new$hs_rate <- (data_new$headshots)/(data_new$hits) 
data_new$win_rate <- (data_new$win)/(data_new$win+data_new$losses)


#score of previous 4 values added together but also multiplied by their weight
data_new$weighted <- dataKD + dataHS + dataK + dataD + dataAT    #original scores
data_new$weighted2 <- dataKD * 1.3 + dataHS * 1.4 + dataK * 1.2 + dataD *.8 + dataAT * 1.2      #weighted values



#11.8 is highest score so breakdown starting at 0 to 6 as beginner, 6 to 9 as intermediate and advanced higher
data_new$SkillLevel <- ifelse(
  data_new$weighted2 <= 4,
  "Beginner",
  ifelse(
    data_new$weighted2 > 4 &
      data_new$weighted2 <= 8,
    "Intermediate",
    "Advanced"
  )
) #Data for kd breakdown

data_new$SkillLevel_bd <- ifelse(
  data_new$weighted2 <= 2,
  "Beginner.1",
  ifelse(
    data_new$weighted2 > 2 & data_new$weighted2 <= 4,
    "Beginner.2",
    ifelse(
      data_new$weighted2 > 4 & data_new$weighted2 <= 5,
      "Beginner.3",
      ifelse(
        data_new$weighted2 > 5 & data_new$weighted2 <= 7,
        "Intermediate.1",
        ifelse(
          data_new$weighted2 > 7 & data_new$weighted2 <= 9,
          "Intermediate.2",
          ifelse(
            data_new$weighted2 > 9 & data_new$weighted2 <= 10,
            "Intermediate.3",
            ifelse(
              data_new$weighted2 > 10 & data_new$weighted2 <= 10.6,
              "Advanced.1",
              ifelse(
                data_new$weighted2 > 10.6 & data_new$weighted2 <= 11.2,
                "Advanced.2",
                ifelse(
                  data_new$weighted2 > 11.2 & data_new$weighted2 <= 11.8,
                  "Advanced.3",
                  "Advanced.3"
                )
              )
            )
          )
        )
      )
    )
  )
)


#new weighted frame max of 20
data_new$weighted3 <- dataKD * 1.3 + dataHS * 1.4 + dataK * 1.2 + dataD *.8 + dataAT * 1.2 + data_new$accuracy_score * 1.35 + data_new$hs_rate * 1.5 + data_new$win_rate * 1.25 #weighted values


#20 is highest score so breakdown starting at 0 to 6 as beginner, 6 to 9 as intermediate and advanceed higher
data_new$SkillLevel1 <- ifelse(
  data_new$weighted3 <= 3,
  "Beginner",
  ifelse(
    data_new$weighted3 > 3 &
      data_new$weighted3 <= 9,
    "Intermediate",
    "Advanced"
  )
) #Data for kd breakdown

data_new$SkillLevel_bd1 <- ifelse(
  data_new$weighted3 <= 2,
  "Beginner.1",
  ifelse(
    data_new$weighted3 > 2 & data_new$weighted3 <= 3,
    "Beginner.2",
    ifelse(
      data_new$weighted3 > 3 & data_new$weighted3 <= 4,
      "Beginner.3",
      ifelse(
        data_new$weighted3 > 4 & data_new$weighted3 <= 6,
        "Intermediate.1",
        ifelse(
          data_new$weighted3 > 6 & data_new$weighted3 <= 8,
          "Intermediate.2",
          ifelse(
            data_new$weighted3 > 8 & data_new$weighted3 <= 9,
            "Intermediate.3",
            ifelse(
              data_new$weighted3 > 9 & data_new$weighted3 <= 11,
              "Advanced.1",
              ifelse(
                data_new$weighted3 > 11 & data_new$weighted3 <= 13,
                "Advanced.2",
                ifelse(
                  data_new$weighted3 > 13 & data_new$weighted3 <= 14,
                  "Advanced.3",
                  "Advanced.3"
                )
              )
            )
          )
        )
      )
    )
  )
)


#create count of each skill level
count_beginner <- sum(data_new$SkillLevel == "Beginner")
count_intermediate <- sum(data_new$SkillLevel == "Intermediate")
count_advanced <- sum(data_new$SkillLevel == "Advanced")

#create count of each skill level1
count_beginner1 <- sum(data_new$SkillLevel1 == "Beginner")
count_intermediate1 <- sum(data_new$SkillLevel1 == "Intermediate")
count_advanced1 <- sum(data_new$SkillLevel1 == "Advanced")


#Create
# Specify split ratio
split_ratio <- .6  # 60%

# Calculate the number of observations for training
train_indices <- sample(1:nrow(data_new), size = floor(split_ratio * nrow(data_new)))

# Split the data
train_data <- data_new[train_indices, ]
validation_data <- data_new[-train_indices, ]




# Calculate the number of observations for training
train_indices <- sample(1:nrow(data_new), size = floor(split_ratio * nrow(data_new)))

# Split the data - EXAMPLE 
# train_data <- data_new[train_indices, ]
# validation_data <- data_new[-train_indices, ]



# dataKD + dataHS + dataK + dataD + dataAT 
# KD, HS, Kills, Deaths, Average Time 


# Creating a dataframe with selected columns
knn_df <- data_new[, c(3, 4, 11, 12, 19, 26)]
print(knn_df)

# Setting the seed for reproducibility
set.seed(11)
split_ratio <- 0.7 # Assuming a 70-30 split for training and validation

# Splitting the dataset
knn_train_indices <- sample(1:nrow(knn_df), size = floor(split_ratio * nrow(knn_df)))
train_data_knn <- knn_df[knn_train_indices, ]
validation_data_knn <- knn_df[-knn_train_indices, ]
train_labels <- knn_df$SkillLevel_bd[knn_train_indices]
test_labels <- knn_df$SkillLevel_bd[-knn_train_indices]




# Preprocess the data (excluding the label column)
norm.values <- preProcess(train_data_knn[, -6], method = c("center", "scale"))
train_norm <- predict(norm.values, train_data_knn[, -6])
validation_norm <- predict(norm.values, validation_data_knn[, -6])

# Ensure labels are factors with the same levels
train_labels <- factor(train_labels)
test_labels <- factor(test_labels, levels = levels(train_labels))

# Run k-NN with k = 3
set.seed(11)
knn.pred <- knn(train = train_norm, test = validation_norm, cl = train_labels, k = 3)

# Align levels of knn.pred to match test_labels
knn.pred <- factor(knn.pred, levels = levels(test_labels))

# Evaluate the model with confusion matrix
confusion <- confusionMatrix(knn.pred, test_labels)
print(confusion)


# Initialize a dataframe to store accuracy results 
  accuracy.df <- data.frame(k = 1:30, Accuracy = rep(0, 30)) 
  # Compute k-NN for different k values on the validation set 
  set.seed(11)
  for (i in 1:30) { knn.pred <- knn(train = train_norm, test = validation_norm, cl = train_labels, k = i) 
  accuracy.df[i, "Accuracy"] <- confusionMatrix(knn.pred, test_labels)$overall["Accuracy"] } 
  # Print the accuracy dataframe
  print(accuracy.df)


# accuracy.df <- data.frame(k = 1:30, Accuracy = rep(0, 30)) 
# # Compute k-NN for different k values on the validation set 
# for (i in 1:30) { knn.pred <- knn(train = train_norm, test = validation_norm, cl = train_labels, k = i) 
# accuracy.df[i, "Accuracy"] <- confusionMatrix(knn.pred, test_labels)$overall["Accuracy"] } 
# # Print the accuracy dataframe
# print(accuracy.df)


#


# Run k-NN with k = 3
  set.seed(11)
knn.pred_2 <- knn(train = train_norm, test = validation_norm, cl = train_labels, k = 7)

# Align levels of knn.pred to match test_labels
knn.pred_2 <- factor(knn.pred_2, levels = levels(test_labels))

# Evaluate the model with confusion matrix
confusion_2 <- confusionMatrix(knn.pred_2, test_labels)
print(confusion_2)


#Based on the information above, our model ranges between a %56 to %62 accuracy. 
#K = 1 having the highest at %62 and our runner up being K = 4. 
# We are going to investigate other variables that contribute more towards skill level
set.seed(11)
  train_control <- trainControl(method = "cv", number = 10)
  
  
  knn_model <- train(factor(SkillLevel_bd) ~ ., data = knn_df, 
                     method = "knn", trControl = train_control,
                     preProcess = c("center", "scale"), tuneLength = 10)
  
  print(knn_model)

  

  best_k <- knn_model$bestTune$k
  
  # Preprocess the test data using the same normalization from the training data
  norm.values <- preProcess(train_data_knn[, -which(names(train_data_knn) == "SkillLevel_bd")], 
                            method = c("center", "scale"))
  train_norm <- predict(norm.values, train_data_knn[, -which(names(train_data_knn) == "SkillLevel_bd")])
  test_norm <- predict(norm.values, train_data_knn[, -which(names(train_data_knn) == "SkillLevel_bd")])
  train_labels <- factor(train_data_knn$SkillLevel_bd)
  test_labels <- factor(train_data_knn$SkillLevel_bd, levels = levels(train_labels))
  
  # Run k-NN with the best k value on the test set
  set.seed(11)
  knn_pred <- knn(train = train_norm, test = test_norm, cl = train_labels, k = 3)
  
  # Align levels of knn_pred to match test_labels
  knn_pred <- factor(knn_pred, levels = levels(test_labels))
  
  # Evaluate the model with confusion matrix
  confusion <- confusionMatrix(knn_pred, test_labels)
  print(confusion)
  
  
  
  plot(knn(train = train_norm, test = test_norm, cl = train_labels, k = 3))
 plot(knn_pred)
  #----------------------------------------

  knn_df_2 <- data_new[, c(3, 4, 11, 12, 19, 20, 21,22, 29)]
  print(knn_df_2)
  
  set.seed(11)
  knn_train_indices_2 <- sample(1:nrow(knn_df_2), size = floor(split_ratio * nrow(knn_df_2)))
  
  # Setting the seed for reproducibility
  set.seed(11)
  split_ratio <- 0.7 # Assuming a 70-30 split for training and validation
  
  # Splitting the dataset
  knn_train_indices <- sample(1:nrow(knn_df_2), size = floor(split_ratio * nrow(knn_df_2)))
  train_data_knn <- knn_df_2[knn_train_indices, ]
  validation_data_knn <- knn_df_2[-knn_train_indices, ]
  train_labels <- knn_df_2$SkillLevel_bd1[knn_train_indices]
  test_labels <- knn_df_2$SkillLevel_bd1[-knn_train_indices]
  
  
  
  
  # Preprocess the data (excluding the label column)
  norm.values <- preProcess(train_data_knn[, -9], method = c("center", "scale"))
  train_norm <- predict(norm.values, train_data_knn[, -9])
  validation_norm <- predict(norm.values, validation_data_knn[, -9])
  
  # Ensure labels are factors with the same levels
  train_labels <- factor(train_labels)
  test_labels <- factor(test_labels, levels = levels(train_labels))
  
  # Run k-NN with k = 3
  set.seed(11)
  knn.pred <- knn(train = train_norm, test = validation_norm, cl = train_labels, k = 3)
  
  # Align levels of knn.pred to match test_labels
  knn.pred <- factor(knn.pred, levels = levels(test_labels))
  
  # Evaluate the model with confusion matrix
  confusion <- confusionMatrix(knn.pred, test_labels)
  print(confusion)
  
  
  set.seed(11)
  accuracy.df <- data.frame(k = 1:30, Accuracy = rep(0, 30))
  # Compute k-NN for different k values on the validation set
  set.seed(11)
  for (i in 1:30) { knn.pred <- knn(train = train_norm, test = validation_norm, cl = train_labels, k = i)
  accuracy.df[i, "Accuracy"] <- confusionMatrix(knn.pred, test_labels)$overall["Accuracy"] }
  # Print the accuracy dataframe
  print(accuracy.df)
  
  
  
  
  
  # Preprocess the test data using the same normalization from the training data
  norm.values <- preProcess(train_data_knn[, -which(names(train_data_knn) == "SkillLevel_bd1")],
                            method = c("center", "scale"))
  train_norm <- predict(norm.values, train_data_knn[, -which(names(train_data_knn) == "SkillLevel_bd1")])
  test_norm <- predict(norm.values, train_data_knn[, -which(names(train_data_knn) == "SkillLevel_bd1")])
  train_labels <- factor(train_data_knn$SkillLevel_bd1)
  test_labels <- factor(train_data_knn$SkillLevel_bd1, levels = levels(train_labels))
  
  # Run k-NN with the best k value on the test set
  set.seed(11)
  knn_pred <- knn(train = train_norm, test = test_norm, cl = train_labels, k = 3)
  
  # Align levels of knn_pred to match test_labels
  knn_pred <- factor(knn_pred, levels = levels(test_labels))
  
  # Evaluate the model with confusion matrix
  confusion <- confusionMatrix(knn_pred, test_labels)
  print(confusion)

  
  set.seed(11)
  train_control <- trainControl(method = "cv", number = 10)
  
  
  knn_model <- train(factor(SkillLevel_bd1) ~ ., data = knn_df_2, 
                     method = "knn", trControl = train_control,
                     preProcess = c("center", "scale"), tuneLength = 10)
  
  print(knn_model)
  


# #Graphs for personal use

# 
# 
# #make count of each kdRatio for Beginner players
# # Sample KD ratios for multiple gamers
# 
# kd_ratio_beg <- subset(data_new, SkillLevel == 'Beginner')
# 
# # Create intervals of 0.5 and categorize the KD ratios
# bin <- cut(kd_ratio_beg$kdRatio,
#            breaks = seq(0, 3, by = 0.1),
#            right = FALSE)
# 
# # Count the number of gamers in each KD bin
# kd_count <- table(bin)
# 
# # Create a bar plot
# barplot(
#   kd_count,
#   main = "Gamer KD vs Count (0.5 Intervals)",
#   xlab = "KD Ratio Intervals",
#   ylab = "Count",
#   col = "lightblue",
#   border = "black"
# )
# 
# 
# #make count of each kdRatio for Intermediate players
# # Sample KD ratios for multiple gamers
# 
# kd_ratio_int <- subset(data_new, SkillLevel == 'Intermediate')
# 
# # Create intervals of 0.5 and categorize the KD ratios
# bin <- cut(kd_ratio_int$kdRatio,
#            breaks = seq(0, 3, by = 0.1),
#            right = FALSE)
# 
# # Count the number of gamers in each KD bin
# kd_count <- table(bin)
# 
# # Create a bar plot
# barplot(
#   kd_count,
#   main = "Gamer KD vs Count (0.5 Intervals)",
#   xlab = "KD Ratio Intervals",
#   ylab = "Count",
#   col = "lightblue",
#   border = "black"
# )
# 
# 
# 
# 
# #make count of each kdRatio for advanced players
# # Sample KD ratios for multiple gamers
# 
# kd_ratio_adv <- subset(data_new, SkillLevel == 'Advanced')
# 
# # Create intervals of 0.5 and categorize the KD ratios
# bin <- cut(kd_ratio_adv$kdRatio,
#            breaks = seq(0, 3, by = 0.1),
#            right = FALSE)
# 
# # Count the number of gamers in each KD bin
# kd_count <- table(bin)
# 
# # Create a bar plot
# barplot(
#   kd_count,
#   main = "Gamer KD vs Count (0.5 Intervals)",
#   xlab = "KD Ratio Intervals",
#   ylab = "Count",
#   col = "lightblue",
#   border = "black"
# )
# 
# 
# 
# 
# #make count of each headshot
# # Sample headshots
# hs_num <- data_new$headshots
# 
# # Create intervals of 0.5 and categorize the hs ratios
# bin1 <- cut(hs_num, breaks = seq(0, 12000, by = 250), right = FALSE)
# 
# # Count the number of gamers in each hs bin
# hs_count <- table(bin1)
# 
# # Create a bar plot
# barplot(
#   hs_count,
#   main = "Gamer HS vs Count",
#   xlab = "Headshots Intervals",
#   ylab = "Count",
#   col = "lightblue",
#   border = "black"
# )
# 
# 
# 
# #make count of each kills
# # Sample kills
# kill_ratio <- data_new$kills
# 
# # Create intervals of 0.5 and categorize the kill ratios
# bin2 <- cut(kill_ratio,
#             breaks = seq(0, 66935, by = 5000),
#             right = FALSE)
# 
# # Count the number of gamers in each kill bin
# kill_count <- table(bin2)
# 
# # Create a bar plot
# barplot(
#   kill_count,
#   main = "Gamer kills vs Count",
#   xlab = "Kill Intervals",
#   ylab = "Count",
#   col = "lightblue",
#   border = "black"
# )
# 
# #make count of each death
# # Sample deaths
# d_ratio <- data_new$deaths
# 
# # Create intervals of 0.5 and categorize the deaths ratios
# bin3 <- cut(d_ratio,
#             breaks = seq(0, 67888, by = 2000),
#             right = FALSE)
# 
# # Count the number of gamers in each KD bin
# d_count <- table(bin3)
# 
# # Create a bar plot
# barplot(
#   d_count,
#   main = "Gamer Deaths vs Count",
#   xlab = "Deaths Intervals",
#   ylab = "Count",
#   col = "lightblue",
#   border = "black"
# )
# 
# 
# #make count of each averageTime
# # Sample averageTime
# at_ratio <- data_new$averageTime
# 
# # Create intervals of 0.5 and categorize the at ratios
# bin4 <- cut(at_ratio, breaks = seq(0, 96, by = 1), right = FALSE)
# 
# # Count the number of gamers in each KD bin
# at_count <- table(bin4)
# 
# # Create a bar plot
# barplot(
#   d_count,
#   main = "Gamer Deaths vs Count",
#   xlab = "Deaths Intervals",
#   ylab = "Count",
#   col = "lightblue",
#   border = "black"
# )
# 
# 
# 
# #make count of skill
# skill_ratio <- data_new$weighted  #unweighted values
# 
# # Create intervals of 0.5 and categorize the KD ratios
# bin5 <- cut(skill_ratio, breaks = seq(0, 10, by = 3), right = FALSE)
# 
# # Count the number of gamers in each KD bin
# skill_count <- table(bin5)
# 
# # Create a bar plot of 3 skill levels
# barplot(
#   skill_count,
#   main = "Gamer Skill unweighted vs Count",
#   xlab = "KD Ratio Intervals",
#   ylab = "Count",
#   col = "lightblue",
#   border = "black"
# )
# 
# 
# #make count of skill
# skill_ratio <- data_new$weighted2  #weighted values
# 
# # Create intervals of 0.5 and categorize the KD ratios
# bin6 <- cut(skill_ratio, breaks = seq(0, 12, by = 4), right = FALSE)
# 
# # Count the number of gamers in each KD bin
# skill_count <- table(bin6)
# 
# # Create a bar plot of 3 skill levels
# barplot(
#   skill_count,
#   main = "Gamer Skill weighted vs Count",
#   xlab = "Skill Ratio Intervals",
#   ylab = "Count",
#   col = "lightblue",
#   border = "black"
# )
# 
# 
# 
# #summary(data_new)
# #count of advanced players with wins less than 340 (average of dataset)
# count_unskilled_advanced <- sum(data_new$SkillLevel == "Advanced" &
#                                   data_new$wins <= 340)
# 
# #9 unskilled advanced players
# advanced_unskilled_players <- data_new$name[data_new$SkillLevel == "Advanced" &
#                                               data_new$wins <= 340]
# advanced_unskilled_players
