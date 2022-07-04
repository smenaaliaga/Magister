# Libraries
library(tidyverse)
library(corrplot)
# Set Working Directory
setwd('~/Documents/Magister/Modelos Lineales para Clasificación/RL Tarea')
# Get Data
data <- read.csv('HR-Employee.csv')
#### AED ####
# Structure
str(data)
# View
View(data)
# Conversion
data <- data %>% mutate(across(c(Attrition, 
                         BusinessTravel,
                         Department,
                         EducationField,
                         Gender,
                         JobRole,
                         MaritalStatus,
                         OverTime
                         ), factor))
# NANs
sum(is.na(data))
# Uniques
sapply(data, n_distinct)
# Remove data 
drops <- c("EmployeeCount","EmployeeNumber", "StandardHours", "Over18", "StockOptionLevel")
data <- data[ , !(names(data) %in% drops)]

# Numerics 
num_cols <- select_if(data, is.numeric)
sapply(num_cols, n_distinct)

# Ordinals
ordinal_cols_name <- c("EnvironmentSatisfaction", "JobInvolvement", 
                      "JobSatisfaction", "PerformanceRating",
                      "RelationshipSatisfaction", "WorkLifeBalance",
                      "Education", "JobLevel")
ordinal_cols <- num_cols[ , (names(num_cols) %in% ordinal_cols_name)]

corr_ord <- cor(ordinal_cols, method='spearman')
corrplot(corr_ord, method = 'number')

# Continuos
cont_cols <- num_cols[ , !(names(num_cols) %in% ordinal_cols_name)]
pairs(cont_cols)
corr_cont <- cor(cont_cols)
corrplot(corr_cont, method = 'number')

# Categorics
cat_cols <- select_if(data, is.character)

plot(cat_cols)


### PLOTS

data$JobSatisfaction <- as.factor(data$JobSatisfaction)


ggplot(data, aes(x = JobSatisfaction, y = Age, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75)

ggplot(data, aes(x = JobSatisfaction, y = MonthlyIncome, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75)

ggplot(data, aes(x = JobSatisfaction, y = MonthlyRate, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75)

ggplot(data, aes(x = JobSatisfaction, y = DailyRate, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75)




ggplot(data, aes(x = JobSatisfaction, y = Age, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_grid(EducationField ~ Gender, margins = FALSE) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggplot(data, aes(x = JobSatisfaction, y = MonthlyIncome, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_grid(Department ~ Gender, margins = FALSE)

ggplot(data, aes(x = JobSatisfaction, y = MonthlyIncome, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_grid(EducationField ~ Gender, margins = FALSE) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))



table(data$JobSatisfaction, data$BusinessTravel)



ggplot(data, aes(x = JobSatisfaction, y = MonthlyIncome, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_grid(Department ~ JobRole, margins = FALSE) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggplot(data, aes(x = JobSatisfaction, y = MonthlyIncome, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_grid(EducationField ~ JobRole, margins = FALSE) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))



ggplot(data, aes(x = JobSatisfaction, y = MonthlyIncome, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_grid(BusinessTravel ~ JobRole, margins = FALSE) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggplot(data, aes(x = JobSatisfaction, y = MonthlyIncome, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_grid(BusinessTravel ~ EducationField, margins = FALSE) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))




ggplot(data, aes(x = JobSatisfaction, y = Age, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_grid(BusinessTravel ~ JobRole, margins = FALSE) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggplot(data, aes(x = JobSatisfaction, y = Age, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_grid(BusinessTravel ~ EducationField, margins = FALSE) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))








ggplot(data, aes(x = JobSatisfaction, y = MonthlyIncome, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_grid(BusinessTravel ~ OverTime, margins = FALSE) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggplot(data, aes(x = JobSatisfaction, y = Age, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_grid(BusinessTravel ~ OverTime, margins = FALSE) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggplot(data, aes(x = JobSatisfaction, y = DistanceFromHome, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_grid(BusinessTravel ~ OverTime, margins = FALSE) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))





# BUSSINES TRAVEL

ggplot(data, aes(x = JobSatisfaction, y = DistanceFromHome, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_wrap(~BusinessTravel) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggplot(data, aes(x = JobSatisfaction, y = Age, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_wrap(~BusinessTravel) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggplot(data, aes(x = JobSatisfaction, y = MonthlyIncome, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_wrap(~BusinessTravel) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggplot(data, aes(x = JobSatisfaction, y = DailyRate, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_wrap(~BusinessTravel) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# OVER TIME

ggplot(data, aes(x = JobSatisfaction, y = DistanceFromHome, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_wrap(~OverTime) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggplot(data, aes(x = JobSatisfaction, y = Age, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_wrap(~OverTime) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggplot(data, aes(x = JobSatisfaction, y = MonthlyIncome, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_wrap(~OverTime) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggplot(data, aes(x = JobSatisfaction, y = DailyRate, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_wrap(~OverTime) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# DEPARTMENT

ggplot(data, aes(x = JobSatisfaction, y = DistanceFromHome, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_wrap(~Department) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggplot(data, aes(x = JobSatisfaction, y = Age, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_wrap(~Department) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggplot(data, aes(x = JobSatisfaction, y = MonthlyIncome, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_wrap(~Department) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggplot(data, aes(x = JobSatisfaction, y = DailyRate, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_wrap(~Department) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# GENDER

ggplot(data, aes(x = JobSatisfaction, y = DistanceFromHome, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_wrap(~Gender) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggplot(data, aes(x = JobSatisfaction, y = Age, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_wrap(~Gender) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggplot(data, aes(x = JobSatisfaction, y = MonthlyIncome, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_wrap(~Gender) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggplot(data, aes(x = JobSatisfaction, y = DailyRate, fill = JobSatisfaction)) +   
  geom_boxplot(size = .75) +   
  facet_wrap(~Gender) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


### MODELO 
library(VGAM)

columns <- c("JobSatisfaction", "Age", "Gender", "BusinessTravel", 
             "DistanceFromHome", "Department", 
             "DailyRate", "OverTime", "MonthlyIncome")

data2 <- data[ , (names(data) %in% columns)]
str(data2)

sapply(data2, n_distinct)

# Convert data: Order
data2$JobSatisfaction <- as.ordered(data2$JobSatisfaction)
data2$Education <- as.ordered(data2$Education)
str(data2)

# Uniques
unique(data2$BusinessTravel)
unique(data2$Department)
unique(data2$OverTime)

# Relevel. Hipotesis
data2$BusinessTravel <- relevel(data$BusinessTravel, ref='Travel_Frequently')
data2$OverTime <- relevel(data$OverTime, ref='Yes')

# Matrix
x <- model.matrix(JobSatisfaction ~ ., data2)
y <- data2$JobSatisfaction

fit_vglm <- vglm(JobSatisfaction ~ ., data2, 
             family = cumulative(link = "logit", parallel = T))

summary(fit_vglm)


####

install.packages("fastDummies")
library(fastDummies)

rm(data_dummy)
data_dummy <- dummy_cols(data2, select_columns = "BusinessTravel")
data_dummy$BusinessTravel <- NULL
data_dummy <- dummy_cols(data_dummy, select_columns = "OverTime")
data_dummy$OverTime <- NULL
data_dummy <- dummy_cols(data_dummy, select_columns = "Department")
data_dummy$Department <- NULL
data_dummy <- dummy_cols(data_dummy, select_columns = "Gender")
data_dummy$Department <- NULL
View(data_dummy)


# INTERACT MODEL!

drops <- c("JobSatisfaction", "Age", 
           "BusinessTravel")
data_interact <- data_dummy[ , (names(data_dummy) %in% drops)]

data_interact$Age_NoTravel <- data_dummy$`BusinessTravel_Non-Travel` * data_dummy$Age
data_interact$Age_TravelRarely <- data_dummy$BusinessTravel_Travel_Rarely * data_dummy$Age
data_interact$Age_TravelFrequently <- data_dummy$BusinessTravel_Travel_Frequently * data_dummy$Age

str(data_interact)

fit_vglm_2 <- vglm(JobSatisfaction ~ ., data_interact, 
                 family = cumulative(link = "logit", parallel = T))

summary(fit_vglm_2)

