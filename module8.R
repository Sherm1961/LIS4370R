#Step 1 import data then run mean function using Sex as the category

#install.packages("pryr")
require(pryr)
require(ISLR)
require(boot)
#install.packages("plyr")
library(data.table)
library(plyr)

#step 1
Student_assignment_6 <- read.table("Assignment 6 Dataset.txt", header = TRUE) #leaves results as a single block
Student_assignment_6
StudentAverage = ddply(Student,"Sex",transform,Grade.Average=mean(Grade)) #student not found as 1 variable only

sex = Student$Sex #student not found as 1 variable only, values conjoined
mean(Sex) #Sex is a chr value so non numeric


#new code trying to keep core intentions step 1
Student_assignment_6 <- read.csv("Assignment 6 Dataset.txt", header = TRUE)
Student_assignment_6

StudentAverage = ddply(Student_assignment_6,"Sex",transform,Grade.Average=mean(Grade))

sex = Student_assignment_6$Sex

#replace Sex values of Male and Female with 1 and 0 to allow for mean

sex <- replace(sex, sex == "Male", 1)
sex <- replace(sex, sex== "Female", 0)

#convert values of chr to numeric using as.numeric
sex <- as.numeric(sex)
mean(sex) #result of 0.2 means only an average of 20% Males vs 80% Females







#step 2 and 3 Make dataframe of all names with i in them and put them in their own csv file
x <- read.table(file.choose(),header = TRUE,sep=",")
y = ddply(x,"Sex",transform, Grade.Average=mean(Grade))

ddply(x,"Sex",transform,Grade.Average=mean(Grade))

write.table(y,"Sorted_Average")
write.table(y,"Sorted_Average",sep=",")

newx = subset(x,grepl("[iI]",x$Name)) #creation of new names df with i within
subset(x,grepl("[iI]",x$Name))
write.table(newx,"DataSubset",sep=",")







