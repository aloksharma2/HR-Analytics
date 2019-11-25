setwd("D:/Analyticsvidhya/HR Analytics - Hackathon")
require(dplyr)
require(ggplot2)

#Loading both the data sets
train_hr <- read.csv("train_LZdllcl.csv")
test_hr <- read.csv("test_2umaH9m.csv")

#merging the data sets to perform EDA at the same time
master_data <- bind_rows(train_hr, test_hr)
View(master_data)

str(master_data)
summary(master_data)

master_data <- mutate_if(master_data, is.character, toupper)

#checking spaces and blanks
colSums(master_data == "" | master_data == " ", na.rm = T)

table(master_data$education)

ggplot(train_hr, aes(x=education, 
                     fill=as.factor(is_promoted))) + geom_bar(position = "fill")


master_data[which(master_data$education == "  ")] <- "primary"

View(master_data)
hh







#checking NA values
colSums(is.na(master_data))


ggplot(master_data, aes(x=previous_year_rating, 
                        fill=as.factor(is_promoted))) + geom_bar(position = "fill")

#creates a dataset containing NA values in previous year data
pyr_missing <- filter(master_data, is.na(master_data$previous_year_rating))
View(pyr_missing)

require(mice)
pyr_missing1 <- mice(master_data, is.na(master_data$previous_year_rating))













