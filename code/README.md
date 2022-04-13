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

myData$education<-factor(myData$education, levels=c("Less than High School",
                                                            "High School Degree",
                                                            "College Degree or More"))

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

#Data Management on Variables of Interest: Gender#
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
  xlab("Nutrition Scores") + ylab("Counts") + ggtitle("Nutrition Scores Distribution")
  
  
#Bivariate Analysis: Food Insecurity Scores#
cor.test(myData$nutrition, myData$scores_sum)

ggplot(data=subset(myData, !is.na(nutrition) & !is.na(scores_sum)))+
   geom_point(aes(x=scores_sum, y=nutrition))+
   geom_smooth(aes(x=scores_sum, y=nutrition), method="lm") +
     labs(y = "Nutrition",
       x = "Food Insecurity Scores",
       title = "Food Insecurity Scores and Nutrition")

#Bivariate Analysis: Household Income#
myAnovaResults_income <- aov(nutrition ~ house_income, data = myData) 
summary(myAnovaResults_income)
TukeyHSD(myAnovaResults_income)
       
ggplot(data=subset(myData, !is.na(nutrition) & !is.na(house_income)))+
  stat_summary(aes(x=house_income, y=nutrition), fun=mean, geom="bar")+
  labs(y = "Nutrition",
       x = "Household Income",
       title = "Nutrition Scores by Household Income")

#Bivariate Analysis: Education Level#
myAnovaResults_edu <- aov(nutrition ~ education, data = myData) 
summary(myAnovaResults_edu)
TukeyHSD(myAnovaResults_edu)
       
ggplot(data=subset(myData, !is.na(nutrition) & !is.na(education)))+
  stat_summary(aes(x=education, y=nutrition), fun=mean, geom="bar")+
  labs(y = "Nutrition",
       x = "Education Level",
       title = "Nutrition Scores by Education Level")

#Multivariate Analysis#
lm_proxy1<- lm(nutrition ~ factor(education) + age + factor(GENDER), data = myData) 
summary(lm_proxy1)

#####################################
#ROBUSTNESS CHECK: FRUITS#
#####################################

myData$nutrition_day_fruits<-NA
myData$nutrition_day_fruits<-(myData$FRUIT_0_TEXT)* 7 * 4
myData$nutrition_day_fruits[is.na(myData$nutrition_day_fruits) == TRUE]<-0

myData$nutrition_week_fruits<-NA
myData$nutrition_week_fruits<-(myData$FRUIT_1_TEXT) * 4
myData$nutrition_week_fruits[is.na(myData$nutrition_week_fruits) == TRUE]<-0

myData$nutrition_month_fruits<-NA
myData$nutrition_month_fruits<-(myData$FRUIT_2_TEXT) * 1
myData$nutrition_month_fruits[is.na(myData$nutrition_month_fruits) == TRUE]<-0

myData$nutrition_fr<-NA
myData$nutrition_fr<-myData$nutrition_day_fruits + myData$nutrition_week_fruits + myData$nutrition_month_fruits
myData$nutrition_fr[myData$FRUIT=="DON'T KNOW" | myData$FRUIT=="REFUSED"] <- NA
myData$nutrition_fr[myData$FRUIT=="NEVER"] <- 0

#The Analysis
cor.test(myData$nutrition_fr, myData$scores_sum)

myAnovaResults_income2 <- aov(nutrition_fr ~ house_income, data = myData) 
summary(myAnovaResults_income2)
TukeyHSD(myAnovaResults_income2)

myAnovaResults_edu2 <- aov(nutrition_fr ~ education, data = myData) 
summary(myAnovaResults_edu2)
TukeyHSD(myAnovaResults_edu2)

lm_proxy2<- lm(nutrition_fr ~ factor(house_income) + age + factor(GENDER), data = myData) 
summary(lm_proxy2)


#####################################
#ROBUSTNESS CHECK: ORANGE VEGETABLES#
#####################################

myData$nutrition_day_or<-NA
myData$nutrition_day_or<-(myData$VEG_ORANGE_0_TEXT)* 7 * 4
myData$nutrition_day_or[is.na(myData$nutrition_day_or) == TRUE]<-0

myData$nutrition_week_or<-NA
myData$nutrition_week_or<-(myData$VEG_ORANGE_1_TEXT) * 4
myData$nutrition_week_or[is.na(myData$nutrition_week_or) == TRUE]<-0

myData$nutrition_month_or<-NA
myData$nutrition_month_or<-(myData$VEG_ORANGE_2_TEXT) * 1
myData$nutrition_month_or[is.na(myData$nutrition_month_or) == TRUE]<-0

myData$nutrition_or<-NA
myData$nutrition_or<-myData$nutrition_day_or + myData$nutrition_week_or + myData$nutrition_month_or
myData$nutrition_or[myData$VEG_ORANGE=="DON'T KNOW" | myData$VEG_ORANGE=="REFUSED"] <- NA
myData$nutrition_or[myData$VEG_ORANGE=="NEVER"] <- 0

#The Analysis
cor.test(myData$nutrition_or, myData$scores_sum)

myAnovaResults_income3 <- aov(nutrition_or ~ house_income, data = myData) 
summary(myAnovaResults_income3)
TukeyHSD(myAnovaResults_income3)

myAnovaResults_edu3 <- aov(nutrition_or ~ education, data = myData) 
summary(myAnovaResults_edu3)
TukeyHSD(myAnovaResults_edu3)

lm_proxy3<- lm(nutrition_or ~ factor(education) + age + factor(GENDER), data = myData) 
summary(lm_proxy3)

#####################################
#ROBUSTNESS CHECK: OTHER VEGETABLES#
#####################################

myData$nutrition_day_ot<-NA
myData$nutrition_day_ot<-(myData$VEG_OTHER_0_TEXT)* 7 * 4
myData$nutrition_day_ot[is.na(myData$nutrition_day_ot) == TRUE]<-0

myData$nutrition_week_ot<-NA
myData$nutrition_week_ot<-(myData$VEG_OTHER_1_TEXT) * 4
myData$nutrition_week_ot[is.na(myData$nutrition_week_ot) == TRUE]<-0

myData$nutrition_month_ot<-NA
myData$nutrition_month_ot<-(myData$VEG_OTHER_2_TEXT) * 1
myData$nutrition_month_ot[is.na(myData$nutrition_month_ot) == TRUE]<-0

myData$nutrition_ot<-NA
myData$nutrition_ot<-myData$nutrition_day_ot + myData$nutrition_week_ot + myData$nutrition_month_ot
myData$nutrition_ot[myData$VEG_OTHER=="DON'T KNOW" | myData$VEG_OTHER=="REFUSED"] <- NA
myData$nutrition_ot[myData$VEG_OTHER=="NEVER"] <- 0

#The Analysis
cor.test(myData$nutrition_ot, myData$scores_sum)

myAnovaResults_income4 <- aov(nutrition_ot ~ house_income, data = myData) 
summary(myAnovaResults_income4)
TukeyHSD(myAnovaResults_income4)

myAnovaResults_edu4 <- aov(nutrition_ot ~ education, data = myData) 
summary(myAnovaResults_edu4)
TukeyHSD(myAnovaResults_edu4)

