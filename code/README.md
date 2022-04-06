#Loading Necessary Libraries#
library(ggplot2)
library(descr)
library(dplyr)
library(readxl)
library(devtools)
#For those without ggthemr, install_github('cttobin/ggthemr')
library(ggthemr)
ggthemr("dust")
library(ggplot2)

#Loading Up Data Set#
myData <- read_excel("Edited_FOTM Survey Data.xlsx")

#Data Management on Variables of Interest: Education#
myData$education<-NA
myData$education[myData$EDIT_SCHOOL_LEVEL=="LESS THAN HIGH SCHOOL DEGREE"] <- "Less than High School"
myData$education[myData$EDIT_SCHOOL_LEVEL=="HIGH SCHOOL DEGREE OR EQUIVALENT (e.g. GED)"] <- "High School Degree"
myData$education[myData$EDIT_SCHOOL_LEVEL=="COLLEGE DEGREE OR MORE"] <- "College Degree or More"
myData$education[myData$EDIT_SCHOOL_LEVEL=="DON'T KNOW"] <- NA

freq(as.ordered(myData$education))

#Data Management on Variables of Interest: Age#
myData$dates <- as.POSIXct(myData$StartDate, format = "%m/%d/%Y %H:%M:%S")
myData$dates <- format(myData$dates, format="%Y")
myData$dates <- as.numeric(myData$dates)
myData$YOB <- as.numeric(myData$YOB)
myData$age <- myData$dates - myData$YOB

freq(as.ordered(myData$age))
summary(myData$age)
mean(myData$age, na.rm = TRUE)
sd(myData$age, na.rm = TRUE)

#Data Management on Variables of Interest: Education#
myData$GENDER[myData$GENDER=="Genderqueer or Non-Binary"] <- NA
freq(as.ordered(myData$GENDER))

#Data Management on Variables of Interest: Household Income#
myData$house_income <- NA
myData$house_income[myData$HHLD_INCOME1=="YES"|
                    myData$HHLD_INCOME2=="YES"|
                    myData$HHLD_INCOME3=="YES"|
                    myData$HHLD_INCOME4=="YES"|
                    myData$HHLD_INCOME5=="YES"|
                    myData$HHLD_INCOME6=="YES"] <- NA

myData$house_income[myData$HHLD_INCOME1=="DON'T KNOW"|
                    myData$HHLD_INCOME2=="DON'T KNOW"|
                    myData$HHLD_INCOME3=="DON'T KNOW"|
                    myData$HHLD_INCOME4=="DON'T KNOW"|
                    myData$HHLD_INCOME5=="DON'T KNOW"|
                    myData$HHLD_INCOME6=="DON'T KNOW"] <- NA
                    
myData$house_income[myData$HHLD_INCOME1=="NO"] <- "<30,000"
myData$house_income[myData$HHLD_INCOME2=="NO"] <- "30,000-40,000"
myData$house_income[myData$HHLD_INCOME3=="NO"] <- "40,000-51,000"
myData$house_income[myData$HHLD_INCOME4=="NO"] <- "51,000-61,000"
myData$house_income[myData$HHLD_INCOME5=="NO"] <- "61,000-71,000"
myData$house_income[myData$HHLD_INCOME5=="NO"] <- "71,000-166,000"

freq(as.ordered(myData$house_income))

#Data Management on Variables of Interest: Food Insecurity#
#Consists of FOOD_DIDNT_LAST, BALANCED_MEALS, SKIP_MEALS, OFTEN_SKIP_MEALS, EAT_LESS, HUNGRY which have slightly different values#
#will be scoring each question and summing them up together#

myData$scores1<-NA
myData$scores1[myData$FOOD_DIDNT_LAST=="DON'T KNOW" | myData$FOOD_DIDNT_LAST=="REFUSED"] <-NA
myData$scores1[myData$FOOD_DIDNT_LAST=="Never true for you in the last 12 months"]<-0
myData$scores1[myData$FOOD_DIDNT_LAST=="Sometimes, or"]<-1
myData$scores1[myData$FOOD_DIDNT_LAST=="Often"]<-2
myData$scores1<-as.numeric(myData$scores1)

myData$scores2<-NA
myData$scores2[myData$BALANCED_MEALS=="DON'T KNOW" | myData$BALANCED_MEALS=="REFUSED"] <-NA
myData$scores2[myData$BALANCED_MEALS=="Never true for you in the last 12 months"]<-0
myData$scores2[myData$BALANCED_MEALS=="Sometimes, or"]<-1
myData$scores2[myData$BALANCED_MEALS=="Often"]<-2
myData$scores2<-as.numeric(myData$scores2)

myData$scores3<-NA
myData$scores3[myData$SKIP_MEALS=="DON'T KNOW" | myData$SKIP_MEALS=="REFUSED" |
                              myData$OFTEN_SKIP_MEALS=="DON'T KNOW" | myData$OFTEN_SKIP_MEALS=="REFUSED"] <-NA
myData$scores3[myData$SKIP_MEALS=="NO"]<-0
myData$scores3[myData$SKIP_MEALS=="YES" & myData$OFTEN_SKIP_MEALS=="Only 1 or 2 months"]<-1
myData$scores3[myData$SKIP_MEALS=="YES" & myData$OFTEN_SKIP_MEALS=="Some months but not every month, or"]<-1
myData$scores3[myData$SKIP_MEALS=="YES" & myData$OFTEN_SKIP_MEALS=="Almost every month"]<-2
myData$scores3<-as.numeric(myData$scores3)

myData$scores4<-NA
myData$scores4[myData$EAT_LESS=="DON'T KNOW" | myData$EAT_LESS=="REFUSED"] <-NA
myData$scores4[myData$EAT_LESS=="NO"]<-0
myData$scores4[myData$EAT_LESS=="YES"]<-1
myData$scores4<-as.numeric(myData$scores4)

myData$scores5<-NA
myData$scores5[myData$HUNGRY=="DON'T KNOW" | myData$HUNGRY=="REFUSED"] <-NA
myData$scores5[myData$HUNGRY=="NO"]<-0
myData$scores5[myData$HUNGRY=="YES"]<-1
myData$scores5<-as.numeric(myData$scores5)

myData$scores_sum<-NA
myData$scores_sum<-myData$scores1+myData$scores2+myData$scores3+myData$scores4+myData$scores5

freq(as.ordered(myData$scores_sum))
summary(myData$scores_sum)
mean(myData$scores_sum, na.rm = TRUE)
sd(myData$scores_sum, na.rm = TRUE)

#Data Management on Variables of Interest: Nutrition Proxied by Dark Veggies Intake#
myData$nutrition_day<-NA
myData$nutrition_day<-(myData$VEG_DARK_GREEN_0_TEXT)* 7 * 4
myData$nutrition_day[is.na(myData$nutrition_day) == TRUE]<-0

myData$nutrition_week<-NA
myData$nutrition_week<-(myData$VEG_DARK_GREEN_1_TEXT) * 4
myData$nutrition_week[is.na(myData$nutrition_week) == TRUE]<-0

myData$nutrition_month<-NA
myData$nutrition_month<-(myData$VEG_DARK_GREEN_2_TEXT) * 1
myData$nutrition_month[is.na(myData$nutrition_month) == TRUE]<-0

myData$nutrition<-NA
myData$nutrition<-myData$nutrition_day + myData$nutrition_week + myData$nutrition_month
myData$nutrition[myData$VEG_DARK_GREEN=="DON'T KNOW" | myData$VEG_DARK_GREEN=="REFUSED"] <- NA
myData$nutrition[myData$VEG_DARK_GREEN=="NEVER"] <- 0


#Univariate Graphs#
ggplot(data=subset(myData, !is.na(education)))+
  geom_bar(aes(x=education))+ 
  xlab("Education Level") + ylab("Counts") + ggtitle("Education Level Distribution")
  
ggplot(data=subset(myData, !is.na(age)))+
  geom_histogram(aes(x=age),binwidth = 1) + 
  xlab("Age") + ylab("Counts") + ggtitle("Age Distribution")
  
ggplot(data=subset(myData, !is.na(GENDER)))+
  geom_bar(aes(x=GENDER))+ 
  xlab("Gender") + ylab("Counts") + ggtitle("Gender Distribution") 

ggplot(data=subset(myData, !is.na(house_income)))+
  geom_bar(aes(x=house_income))+ 
  xlab("Household Income") + ylab("Counts") + ggtitle("Household Income Distribution") +
  coord_flip()

ggplot(data=subset(myData, !is.na(scores_sum)))+
  geom_density(aes(x=scores_sum))+ 
  xlab("Food Insecurity Scores") + ylab("Counts") + ggtitle("Food Insecurity Scores Density")

ggplot(data=subset(myData, !is.na(nutrition)))+
  geom_bar(aes(x=nutrition))+ 
  xlab("Nutrition Level") + ylab("Counts") + ggtitle("Nutrition Level Distribution")
  
#Bivariate Graphs#
ggplot(data=subset(myData, !is.na(nutrition) & !is.na(house_income)))+
  geom_bar(aes(x = nutrition, fill = house_income), position="stack")+
  labs(y = "Counts", 
       fill = "Household Income",
       x = "Nutrition Level",
       title = "Nutrition Level by Household Income")

ggplot(data=subset(myData, !is.na(nutrition) & !is.na(education)))+
  geom_bar(aes(x = nutrition, fill = education), position="stack")+
  labs(y = "Counts", 
       fill = "Education Level",
       x = "Nutrition Level",
       title = "Nutrition Level by Education Level")

ggplot(data=subset(myData, !is.na(nutrition) & !is.na(GENDER)))+
  geom_bar(aes(x = nutrition, fill = GENDER), position="stack")+
  labs(y = "Counts", 
       fill = "Gender",
       x = "Nutrition Level",
       title = "Nutrition Level by Gender")

ggplot(data=subset(myData, !is.na(nutrition) & !is.na(scores_sum)))+
  geom_density(aes(x = scores_sum, fill = nutrition), alpha=0.4)+
  labs(y = "Density", 
       fill = "Nutrition Level",
       x = "Food Insecurity Scores",
       title = "Food Insecurity Scores Density by Nutrition Level")

ggplot(data=subset(myData, !is.na(nutrition) & !is.na(age)))+
  geom_density(aes(x = age, fill = nutrition), alpha=0.4)+
  labs(y = "Density", 
       fill = "Nutrition Level",
       x = "Age",
       title = "Age Density by Nutrition Level")



# Education on Nutrition Analysis       
myAnovaResults <- aov(myData$nutrition ~ myData$education, data = myData) 
  summary(myAnovaResults)

  # for post-hoc test
  myAnovaResults <- aov(QuantResponseVar ~ CategExplanatoryVar, data = myData) 
    TukeyHSD(myAnovaResults)



