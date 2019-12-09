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

#converted all to upper case#
master_data <- mutate_if(master_data, is.factor, as.character)
master_data <- mutate_if(master_data, is.character, toupper)
View(master_data)

#checking spaces and blanks
colSums(master_data == "" | master_data == " ", na.rm = T)

table(master_data$education)

#Treating the blank values by replacing with PRIMARY
master_data$education[which(master_data$education == "")] <- "PRIMARY"

View(master_data)

#checking NA values
colSums(is.na(master_data))

#trying Mice 
require(mice)
set.seed(122)
imp1 <- mice(master_data[,2:13], m = 5, method = 'pmm')

imp1$imp$previous_year_rating

imp2 <- complete(imp1,1)
View(imp2)
summary(imp2)
colSums(is.na(imp2))

# dt <- complete(imp1, "all")
# View(dt[["1"]])

View(master_data)
is_promoted <- master_data[ ,14]
View(is_promoted)

imp2 <- mutate(imp2, is_promoted)
View(imp2)
colSums(is.na(imp2))
# NA Treatment done now moves towards quantile

require(dummies)
final <- dummy.data.frame(imp2)
View(final)

#spliting
train <- final[1:54808,]
test <- final[54809:78298,]


log_1 <- glm(is_promoted ~ ., data = train[,-1], 
             family = 'binomial')

summary(log_1)

log_2 <- step(log_1, direction = 'both')

summary(log_2)

# Check VIF
require(car)
sort(vif(log_2))

log_3 <- glm(formula = is_promoted ~ departmentFINANCE + departmentHR + 
               departmentLEGAL + departmentOPERATIONS + departmentPROCUREMENT + 
               `departmentR&D` + `departmentSALES & MARKETING` + departmentTECHNOLOGY + 
               regionREGION_1 + regionREGION_10 + regionREGION_11 + regionREGION_12 + 
               regionREGION_13 + regionREGION_14 + regionREGION_15 + regionREGION_16 + 
               regionREGION_17 + regionREGION_19 + regionREGION_20 + 
               regionREGION_21 + regionREGION_22 + regionREGION_23 + regionREGION_24 + 
               regionREGION_25 + regionREGION_26 + regionREGION_27 + regionREGION_28 + 
               regionREGION_29 + regionREGION_3 + regionREGION_30 + regionREGION_31 + 
               regionREGION_32 + regionREGION_33 + regionREGION_4 + regionREGION_5 + 
               regionREGION_6 + regionREGION_7 + regionREGION_8 + `educationBACHELOR'S` + 
               `educationMASTER'S & ABOVE` + recruitment_channelREFERRED + 
               no_of_trainings + age + previous_year_rating + length_of_service + 
               KPIs_met..80. + awards_won. + avg_training_score, family = "binomial", 
             data = train[, -1])

summary(log_3)
sort(vif(log_3))

log_4 <- glm(formula = is_promoted ~ departmentFINANCE + departmentHR + 
               departmentLEGAL + departmentOPERATIONS + departmentPROCUREMENT + 
               `departmentR&D` + departmentTECHNOLOGY + 
               regionREGION_1 + regionREGION_10 + regionREGION_11 + regionREGION_12 + 
               regionREGION_13 + regionREGION_14 + regionREGION_15 + regionREGION_16 + 
               regionREGION_17 + regionREGION_19 + regionREGION_20 + regionREGION_21 + 
               regionREGION_22 + regionREGION_23 + regionREGION_24 + regionREGION_25 + 
               regionREGION_26 + regionREGION_27 + regionREGION_28 + regionREGION_29 + 
               regionREGION_3 + regionREGION_30 + regionREGION_31 + regionREGION_32 + 
               regionREGION_33 + regionREGION_4 + regionREGION_5 + regionREGION_6 + 
               regionREGION_7 + regionREGION_8 + `educationBACHELOR'S` + 
               `educationMASTER'S & ABOVE` + recruitment_channelREFERRED + 
               no_of_trainings + age + previous_year_rating + length_of_service + 
               KPIs_met..80. + awards_won. + avg_training_score, family = "binomial", 
             data = train[, -1])
summary(log_4)
sort(vif(log_4))

log_5 <- glm(formula = is_promoted ~ departmentFINANCE + departmentHR + 
               departmentLEGAL + departmentOPERATIONS + departmentPROCUREMENT + 
               `departmentR&D` + departmentTECHNOLOGY + regionREGION_1 + 
               regionREGION_10 + regionREGION_11 + regionREGION_12 + regionREGION_13 + 
               regionREGION_14 + regionREGION_15 + regionREGION_16 + regionREGION_17 + 
               regionREGION_19 + regionREGION_20 + regionREGION_21 + regionREGION_22 + 
               regionREGION_23 + regionREGION_24 + regionREGION_25 + regionREGION_26 + 
               regionREGION_27 + regionREGION_28 + regionREGION_29 + regionREGION_3 + 
               regionREGION_30 + regionREGION_31 + regionREGION_32 + regionREGION_33 + 
               regionREGION_4 + regionREGION_5 + regionREGION_6 + regionREGION_7 + 
               regionREGION_8 + `educationBACHELOR'S` + 
               recruitment_channelREFERRED + no_of_trainings + age + previous_year_rating + 
               length_of_service + KPIs_met..80. + awards_won. + avg_training_score, 
             family = "binomial", data = train[, -1])
summary(log_5)
sort(vif(log_5))

pred_promoted = predict(log_5, newdata = test, type = 'response')
summary(pred_promoted)

test$is_promoted <- ifelse(pred_promoted >= 0.50, 1, 0)

table(test$is_promoted)
View(test)

sample_sub <- read.csv('sample_submission.csv')
View(sample_sub)

pred_on_test <- test
View(pred_on_test)

employee_id <- test_hr[ ,1]

pred_on_test <- mutate(pred_on_test, employee_id)
View(pred_on_test)

final_sub <- select(pred_on_test, employee_id, is_promoted)
View(final_sub)

table(final_sub$is_promoted)

write.csv(final_sub, 'D:/Analyticsvidhya/HR Analytics - Hackathon/test_pred.csv', row.names = FALSE)

test_pred <- read.csv('D:/Analyticsvidhya/HR Analytics - Hackathon/test_pred.csv')
View(test_pred)

