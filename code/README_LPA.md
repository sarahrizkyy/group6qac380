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
library(tidyLPA)

#Loading Up Data Set#
myData <- read_excel("Edited_FOTM Survey Data.xlsx")


#################################################
#Initial Data Management#
#################################################

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

#######################################################
#Preparing the Dependent Variable: Nutrition Engagement
#######################################################

#Coding All Consumption: Dark Vegetables#
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
myData$nutrition[myData$nutrition>28] <- 28

#Coding All Consumption: Fruits#
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
myData$nutrition_fr[myData$nutrition_fr>28] <- 28

#Coding All Consumption: Orange Vegetables#
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
myData$nutrition_or[myData$nutrition_or>28] <- 28

#Coding All Consumption: Other Vegetables#
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
myData$nutrition_ot[myData$nutrition_ot>28] <- 28

#Coding All Consumption: Fruit Juice#
myData$nutrition_day_fj<-NA
myData$nutrition_day_fj<-(myData$FRUIT_JUICE_0_TEXT)* 7 * 4
myData$nutrition_day_fj[is.na(myData$nutrition_day_fj) == TRUE]<-0

myData$nutrition_week_fj<-NA
myData$nutrition_week_fj<-(myData$FRUIT_JUICE_1_TEXT) * 4
myData$nutrition_week_fj[is.na(myData$nutrition_week_fj) == TRUE]<-0

myData$nutrition_month_fj<-NA
myData$nutrition_month_fj<-(myData$FRUIT_JUICE_2_TEXT) * 1
myData$nutrition_month_fj[is.na(myData$nutrition_month_fj) == TRUE]<-0

myData$nutrition_fj<-NA
myData$nutrition_fj<-myData$nutrition_day_fj + myData$nutrition_week_fj + myData$nutrition_month_fj
myData$nutrition_fj[myData$FRUIT_JUICE=="DON'T KNOW" | myData$FRUIT_JUICE=="REFUSED"] <- NA
myData$nutrition_fj[myData$FRUIT_JUICE=="NEVER"] <- 0
myData$nutrition_fj[myData$nutrition_fj>28] <- 28

#Coding All Consumption: Beans#
myData$nutrition_day_be<-NA
myData$nutrition_day_be<-(myData$BEANS_0_TEXT)* 7 * 4
myData$nutrition_day_be[is.na(myData$nutrition_day_be) == TRUE]<-0

myData$nutrition_week_be<-NA
myData$nutrition_week_be<-(myData$BEANS_1_TEXT) * 4
myData$nutrition_week_be[is.na(myData$nutrition_week_be) == TRUE]<-0

myData$nutrition_month_be<-NA
myData$nutrition_month_be<-(myData$BEANS_2_TEXT) * 1
myData$nutrition_month_be[is.na(myData$nutrition_month_be) == TRUE]<-0

myData$nutrition_be<-NA
myData$nutrition_be<-myData$nutrition_day_be + myData$nutrition_week_be + myData$nutrition_month_be
myData$nutrition_be[myData$BEANS=="DON'T KNOW" | myData$BEANS=="REFUSED"] <- NA
myData$nutrition_be[myData$BEANS=="NEVER"] <- 0
myData$nutrition_be[myData$nutrition_be>28] <- 28


#LPA: Figuring Out How Many Classes + Creating Class Variable#
myData %>%
    select(nutrition, nutrition_or, nutrition_ot, nutrition_fr, nutrition_fj, nutrition_be) %>%
    single_imputation() %>%
    estimate_profiles(1:3, 
                      variances = "varying",
                      covariances = "varying") %>%
    compare_solutions(statistics = c("AIC", "BIC"))
#The Answer: Model 6 with 2 Classes are the best model according to AIC and BIC#
#...suggests the best solution is Model 6 with 2 classes#


m1<-myData %>%
    select(nutrition, nutrition_or, nutrition_ot, nutrition_fr, nutrition_fj, nutrition_be) %>%
    single_imputation() %>%
    estimate_profiles(2, 
                      variances = "varying",
                      covariances = "varying")
    

myData$class<-m1[["model_6_class_2"]][["model"]][["classification"]]
myData$class[myData$class==1]<-"Low Nutrition Engagement"
myData$class[myData$class==2]<-"High Nutrition Engagement"
freq(as.ordered(myData$class))

plot_profiles(m1)


###################################
#Univariate Graphs#
###################################

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

ggplot(data=subset(myData, !is.na(class)))+
  geom_bar(aes(x=class))+ 
  xlab("Nutrition Engagement Class") + ylab("Counts") + ggtitle("Nutrition Engagement Distribution")

###################################
#Bivariate Graphs#
###################################

ggplot(myData, 
       aes(x = education, 
           fill = class)) + 
  geom_bar(position = "stack") +
  ggtitle ("Nutrition Engagement Levels Within Education Groups")

ggplot(myData, 
       aes(x = house_income, 
           fill = class)) + 
  geom_bar(position = "stack") +
  ggtitle ("Nutrition Engagement Levels Within Income Groups")
  
ggplot(myData, 
       aes(x = GENDER, 
           fill = class)) + 
  geom_bar(position = "stack") +
  ggtitle ("Nutrition Engagement Levels by Gender")
  
ggplot(myData, aes(x = class, 
                     y = age)) +
  geom_boxplot(notch = TRUE, 
               alpha = .7) +
  labs(title = "Age Distribution by Nutrition Engagement Level")
  
ggplot(myData, aes(x = class, 
                     y = scores_sum)) +
  geom_boxplot(notch = TRUE, 
               alpha = .7) +
  labs(title = "Food Insecurity Scores Dsirtribution by Nutrition Engagement Level")

###################################
#Bivariate Analysis#
###################################

#Education Not Significant#
myChi_edu <- chisq.test(myData$class, myData$education) 
myChi_edu 

#Income Not Significant#
myChi_income <- chisq.test(myData$class, myData$house_income) 
myChi_income 

#Food Insecurity Not Significant#
myChi_food <- chisq.test(myData$class, myData$scores_sum) 
myChi_food

#Age Not Significant#
myChi_age <- chisq.test(myData$class, myData$age) 
myChi_age 

#Gender Not Significant#
myChi_GENDER <- chisq.test(myData$class, myData$GENDER) 
myChi_GENDER













