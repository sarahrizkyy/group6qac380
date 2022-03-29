# migrate SPSS data set with variable and value labels
library(haven)
library(expss)
library(foreign)
library(rmarkdown)
library(descr) # frequency & contingency tables
library(Hmisc) # variable labels
library(moments) # skewness & kurtosis
library(car) # recode
library(plyr) # data manipulation
library(readstata13)
library(labelled)

mydata<-read_stata("Z:/PSYC/LDierker/JROSE01/QAC consulting/Nunn K 01/Food on the move/data/FOTM_MERGED_02_20_19.dta")

# recode missing data
mydata[mydata==77|mydata==99] <- NA

# create age variable from year of birth (all surveys were conducted in 2018)
mydata$AGE<- 2018-mydata$YOB


# use value labels
labelled_vars <- names(mydata)[sapply(mydata, is.labelled)]
for (vars in labelled_vars){
  mydata[[vars]] = as_factor(mydata[[vars]])
}

str(mydata, list.len=300)


# Data management
mydata$MONTHS_SNAP_DES[mydata$MONTHS_SNAP_DES==15]<-12

mydata$GENDER_FEMALE<-(as.character(mydata$GENDER_FEMALE))
mydata$Female<-"FEMALE"
mydata$Female[is.na(mydata$GENDER_FEMALE)]<-"MALE"

mydata$White<-"White"
mydata$White[is.na(mydata$RACE_WHITE)]<-"Non-White"
freq(mydata$White, plot=F)

#freq(mydata$SCHOOL_LEVEL, plot=F)
mydata$School<-mydata$SCHOOL_LEVEL 
mydata$School[mydata$School=="NA"]<-NA
mydata$Education <- ifelse(mydata$School=="< HIGH SCHOOL DEGREE", "Less than high school",
                           ifelse(mydata$School=="HIGH SCHOOL DEGREE OR EQUIVALENT", "High school degree",
                                  ifelse(mydata$School=="SOME COLLEGE NO DEGREE", "Some college", 
                                         ifelse(mydata$School=="ASSOCIATE DEGREE"|
                                                  mydata$School=="BACHELOR DEGREE"|
                                                  mydata$School=="GRADUATE OR PROFESSIONAL DEGREE",
                                                "Post-secondary degree or higher", NA)))) 
mydata$Education <-as.factor(mydata$Education)
freq(mydata$Education, plot=F)

#freq(mydata$HHLDSIZE, plot=F)
mydata$Household_size<-mydata$HHLDSIZE
mydata$Household_size[mydata$HHLDSIZE==1]<-"One"
mydata$Household_size[mydata$HHLDSIZE==2]<-"Two"
mydata$Household_size[mydata$HHLDSIZE>2]<-"More than two"
mydata$Household_size <-as.factor(mydata$Household_size)
freq(mydata$Household_size, plot=F)

mydata$Household_income<-mydata$HHLD_INCOME
mydata$Household_income <- ifelse(mydata$Household_income=="< $10,000","< $10,000",
                                  ifelse(!is.na(mydata$Household_income),"$10,000 or more", NA))
mydata$Household_income <-as.factor(mydata$Household_income)
freq(mydata$Household_income, plot=F)

mydata$Months_receive_SNAP<- "Don't receive SNAP"
mydata$Months_receive_SNAP[mydata$MONTHS_SNAP_DES==12]<-"12 months"
mydata$Months_receive_SNAP[mydata$MONTHS_SNAP_DES< 12]<-"less than 12 months"
mydata$Months_receive_SNAP <-as.factor(mydata$Months_receive_SNAP)
freq(mydata$Months_receive_SNAP, plot=F)

mydata$SNAP_weeks<- "Don't receive SNAP"
mydata$SNAP_weeks[mydata$SNAP_WEEKS=="About 1 week or less"|mydata$SNAP_WEEKS=="About 2 weeks"]<-"2 weeks or less"
mydata$SNAP_weeks[mydata$SNAP_WEEKS=="About 3 weeks, or"|mydata$SNAP_WEEKS=="The entire month"]<-"3 weeks or more"
mydata$SNAP_weeks <-as.factor(mydata$SNAP_weeks)
freq(mydata$SNAP_weeks, plot=F)

# Where Shop most often
mydata$WhereShop <- ifelse(mydata$WHERESHOP=="LARGE CHAIN", "Large Chain",
                           ifelse(mydata$WHERESHOP=="DISCOUNT SUPERMARKET"|
                                    mydata$WHERESHOP=="DISCOUNT SUPERSTORE", "Discount Store", "Other")) 
mydata$WhereShop <-as.factor(mydata$WhereShop)
freq(mydata$WhereShop, plot=F)

mydata$OftenShopFOTM[mydata$OFTENSHOPFOTM=="NA"]<-NA
mydata$OftenShopFOTM <- ifelse(mydata$OFTENSHOPFOTM=="AT LEAST ONCE A WEEK", 1,0) 
mydata$OftenShopFOTM <-as.factor(mydata$OftenShopFOTM)
freq(mydata$OftenShopFOTM, plot=F)

# Health prevents going to store
mydata$Health_prev_store[mydata$HEALTHPREVENT_STORE=="Rarely or never"]<-0
mydata$Health_prev_store[mydata$HEALTHPREVENT_STORE=="Sometimes, or"|
                           mydata$HEALTHPREVENT_STORE=="Often"]<-1
mydata$Health_prev_store<-as.factor(mydata$Health_prev_store)
freq(mydata$Health_prev_store, plot=F)

mydata$Shop_FOTM_fv <- ifelse(mydata$WHEREFV=="FOOD ON THE MOVE","Shop for fv at FOTM",
                              ifelse(!is.na(mydata$WHEREFV),"Shop elsewhere for fv", NA))
mydata$Shop_FOTM_fv <-as.factor(mydata$Shop_FOTM_fv)
freq(mydata$Shop_FOTM_fv, plot=F)


mydata$Cost_important<- ifelse(mydata$COSTFV=="NOT IMPORTANT"|
                                 mydata$COSTFV=="SLIGHTLY IMPORTANT"|
                                 mydata$COSTFV=="MODERATELY IMPORTANT","Less important",
                               ifelse(mydata$COSTFV=="VERY IMPORTANT","Very important", 
                                      ifelse(mydata$COSTFV=="EXTREMELY IMPORTANT",
                                             "Extremely important", NA)))
mydata$Cost_important <-as.factor(mydata$Cost_important)
freq(mydata$Cost_important, plot=F)

mydata$Location_important<- ifelse(mydata$LOCATIONFV=="NOT IMPORTANT"|
                                     mydata$LOCATIONFV=="SLIGHTLY IMPORTANT"|
                                     mydata$LOCATIONFV=="MODERATELY IMPORTANT","Less important",
                                   ifelse(mydata$LOCATIONFV=="VERY IMPORTANT","Very important", 
                                          ifelse(mydata$LOCATIONFV=="EXTREMELY IMPORTANT",
                                                 "Extremely important", NA)))
mydata$Location_important <-as.factor(mydata$Location_important)
freq(mydata$Location_important, plot=F)

freq(mydata$STAFFFV,plot=F)
mydata$STAFF_important<- ifelse(mydata$STAFFFV=="NOT IMPORTANT"|
                                  mydata$STAFFFV=="SLIGHTLY IMPORTANT"|
                                  mydata$STAFFFV=="MODERATELY IMPORTANT","Less important",
                                ifelse(mydata$STAFFFV=="VERY IMPORTANT","Very important", 
                                       ifelse(mydata$STAFFFV=="EXTREMELY IMPORTANT",
                                              "Extremely important", NA)))
mydata$STAFF_important <-as.factor(mydata$STAFF_important)
freq(mydata$STAFF_important, plot=F)

mydata$SPECIAL_Trip<- ifelse(mydata$EFFORTFV==">ONCE A WEEK"|
                               mydata$EFFORTFV=="ONCE A WEEK", "At least once a week",
                             ifelse(mydata$EFFORTFV=="EVERY OTHER WEEK","Every other week",
                                    ifelse(mydata$EFFORTFV=="ONCE A MONTH"|
                                             mydata$EFFORTFV=="EVERY OTHER MONTH"|
                                             mydata$EFFORTFV=="2-3 TIMES/YEAR","Once a month or less",
                                           ifelse(mydata$EFFORTFV=="2-3 TIMES/YEAR","Once a month orless", 
                                                  ifelse(mydata$EFFORTFV=="NOT AT ALL","Not at all",NA)))))
mydata$SPECIAL_Trip <-as.factor(mydata$SPECIAL_Trip)
freq(mydata$SPECIAL_Trip, plot=F)

mydata$Housing<- ifelse(mydata$HOUSINGSTATUS=="PRIVATE RENTAL UNIT"|
                          mydata$HOUSINGSTATUS=="HOUSING THAT YOU OWN", "Rent/Own",
                        ifelse(mydata$HOUSINGSTATUS=="SENIOR HOUSING","Senior housing",
                               ifelse(mydata$HOUSINGSTATUS=="PUBLIC HOUSING","Public housing",NA)))
mydata$Housing <-as.factor(mydata$Housing)
freq(mydata$Housing, plot=F)

mydata$VARIETY_important<- ifelse(mydata$VARIETYFV=="NOT IMPORTANT"|
                                    mydata$VARIETYFV=="SLIGHTLY IMPORTANT"|
                                    mydata$VARIETYFV=="MODERATELY IMPORTANT","Less important",
                                  ifelse(mydata$VARIETYFV=="VERY IMPORTANT","Very important", 
                                         ifelse(mydata$VARIETYFV=="EXTREMELY IMPORTANT",
                                                "Extremely important", NA)))
mydata$VARIETY_important <-as.factor(mydata$VARIETY_important)
freq(mydata$VARIETY_important, plot=F)

mydata$Daily_fruits<-ifelse(mydata$FRUITDAILY=="1-2 CUPS","1-2 cups", 
                            ifelse(mydata$FRUITDAILY=="2-3 CUPS"|mydata$FRUITDAILY=="3-4 CUPS, OR"|
                                     mydata$FRUITDAILY=="4 CUPS OR MORE", "More than 2 cups", 
                                   ifelse(!is.na(mydata$FRUITDAILY), "Less than 1 cup", NA)))
mydata$Daily_fruits <-as.factor(mydata$Daily_fruits)
freq(mydata$Daily_fruits, plot=F)

mydata$Daily_veggies<- ifelse(mydata$VEGGIEDAILY=="1-2 CUPS","1-2 cups", 
                              ifelse(mydata$VEGGIEDAILY=="2-3 CUPS"|
                                       mydata$VEGGIEDAILY=="3-4 CUPS, OR"|
                                       mydata$VEGGIEDAILY=="4 CUPS OR MORE", "More than 2 cups",
                                     ifelse(!is.na(mydata$VEGGIEDAILY), "Less than 1 cup", NA)))
mydata$Daily_veggies <-as.factor(mydata$Daily_veggies)
freq(mydata$Daily_veggies, plot=F)

mydata$health<- ifelse(mydata$GENHEALTH=="EXCELLENT"|
                         mydata$GENHEALTH=="VERY GOOD"|
                         mydata$GENHEALTH=="GOOD","Good Health",
                       ifelse(mydata$GENHEALTH=="FAIR"|mydata$GENHEALTH=="POOR",
                              "Fair or Poor Health", NA))
mydata$health <-as.factor(mydata$health)
#freq(mydata$health, plot=F)

mydata$Weight<- ifelse(mydata$DESCRIBE_WEIGHT=="VERY UNDERWEIGHT"|
                         mydata$DESCRIBE_WEIGHT=="SLIGHTLY UNDERWEIGHT", "Underweight",
                       ifelse(mydata$DESCRIBE_WEIGHT=="ABOUT THE RIGHT WEIGHT","About Right Weight",
                              ifelse(mydata$DESCRIBE_WEIGHT=="SLIGHTLY OVERWEIGHT"|mydata$DESCRIBE_WEIGHT=="VERY OVERWEIGHT",
                                     "Overweight", NA)))
mydata$Weight <-as.factor(mydata$Weight)

########################################################################################################################
# Food insecurity measure
freq(mydata$FOOD_DIDNT_LAST, plot=F)
mydata$FI1[mydata$FOOD_DIDNT_LAST==0]<-0
mydata$FI1[mydata$FOOD_DIDNT_LAST>0]<-1
freq(mydata$FI1, plot=F)

freq(mydata$BALANCED_MEALS, plot=F)
mydata$FI2[mydata$BALANCED_MEALS==0]<-0
mydata$FI2[mydata$BALANCED_MEALS>0]<-1
freq(mydata$FI2, plot=F)

freq(mydata$SKIP_MEALS, plot=F)
mydata$FI3[mydata$SKIP_MEALS==0]<-0
mydata$FI3[mydata$SKIP_MEALS==1]<-1
freq(mydata$FI3, plot=F)

freq(mydata$OFTEN_SKIP_MEALS, plot=F)
mydata$FI4[mydata$FI3==0]<-0
mydata$FI4[mydata$OFTEN_SKIP_MEALS==0]<-1
mydata$FI4[mydata$OFTEN_SKIP_MEALS==1]<-1
mydata$FI4[mydata$OFTEN_SKIP_MEALS==2]<-0
freq(mydata$FI4, plot=F)

mydata$FI5[mydata$FI1==0 & mydata$FI2==0 & mydata$FI3==0]<-0
mydata$FI5[mydata$EAT_LESS==0]<-0
mydata$FI5[mydata$EAT_LESS==1]<-1
freq(mydata$FI5, plot=F)

freq(mydata$HUNGRY, plot=F)
mydata$FI6[mydata$FI1==0 & mydata$FI2==0 & mydata$FI3==0]<-0
mydata$FI6[mydata$HUNGRY==0]<-0
mydata$FI6[mydata$HUNGRY==1]<-1
freq(mydata$FI6, plot=F)

mydata$insecurity_sum<-mydata$FI1+mydata$FI2+mydata$FI3+mydata$FI4+mydata$FI5+mydata$FI6
freq(mydata$insecurity_sum)

mydata$food_security[mydata$insecurity_sum==0|mydata$insecurity_sum==1]<-"High food security"
mydata$food_security[mydata$insecurity_sum>=2 & mydata$insecurity_sum<=4]<-"Low food security"
mydata$food_security[mydata$insecurity_sum > 4]<-"Very low food security"
freq(mydata$food_security, plot=F)
########################################################################################################

mydata$Food_last[mydata$FOOD_DIDNT_LAST=="NEVER TRUE"]<-"NEVER TRUE"
mydata$Food_last[mydata$FOOD_DIDNT_LAST=="SOMETIMES TRUE"|
                   mydata$FOOD_DIDNT_LAST=="OFTEN TRUE"]<-"SOMETIMES OR OFTEN TRUE"
mydata$Food_last <-as.factor(mydata$Food_last)
freq(mydata$Food_last, plot=F)

mydata$Balanced_meals[mydata$BALANCED_MEALS=="NEVER TRUE"]<-"NEVER TRUE"
mydata$Balanced_meals[mydata$BALANCED_MEALS=="SOMETIMES TRUE"|
                        mydata$BALANCED_MEALS=="OFTEN TRUE,"]<-"SOMETIMES OR OFTEN TRUE"
mydata$Balanced_meals <-as.factor(mydata$Balanced_meals)
#freq(mydata$Balanced_meals, plot=F)

mydata$How_much_fv[mydata$PERCENT_FV_FOTM =="NONE"|
                     mydata$PERCENT_FV_FOTM =="ABOUT ONE-QUARTER"]<-"One quarter or less"
mydata$How_much_fv[mydata$PERCENT_FV_FOTM =="ABOUT ONE-HALF"]<- "One half"
mydata$How_much_fv[mydata$PERCENT_FV_FOTM =="ABOUT THREE-QUARTERS"]<-"Three quarters"
mydata$How_much_fv[mydata$PERCENT_FV_FOTM =="ALL"]<-"All"
mydata$How_much_fv <-as.factor(mydata$How_much_fv)

mydata$Eat_more_fv[mydata$EATMORE_FOTM =="EAT LESS FRUITS AND VEGETABLES"|
                     mydata$EATMORE_FOTM =="EAT ABOUT THE SAME AMOUNT OF FRUITS AND VEGETABLES"]<-"Eat the same or less fv"
mydata$Eat_more_fv[mydata$EATMORE_FOTM =="EAT MORE FRUITS AND VEGETABLES"]<-"Eat more fv"
mydata$Eat_more_fv <-as.factor(mydata$Eat_more_fv)


mydata$Buy_more_fv[mydata$BUYMORE_FOTM =="BUY LESS"|
                     mydata$BUYMORE_FOTM =="BUY SAME"]<-"Buy the same or less fv"
mydata$Buy_more_fv[mydata$BUYMORE_FOTM =="BUY MORE"]<-"Buy more fv"
mydata$Buy_more_fv <-as.factor(mydata$Buy_more_fv)

mydata$Discount_buy_more[mydata$DISCOUNTBUYMORE=="MORE FRUITS AND VEGETABLES"]<-"Buy more FV"
mydata$Discount_buy_more[mydata$DISCOUNTBUYMORE=="ABOUT SAME AMOUNT"|
                           mydata$DISCOUNTBUYMORE=="LESS FRUITS AND VEGETABLES"]<-"Buy the same or less FV"
mydata$Discount_buy_more<-as.factor(mydata$Discount_buy_more)
freq(mydata$Discount_buy_more, plot=F)

mydata$Discount_eat_more[mydata$EATMOREDISCOUNT=="MORE FRUITS AND VEGETABLES"]<-"Eat more FV"
mydata$Discount_eat_more[mydata$EATMOREDISCOUNT=="ABOUT THE SAME AMOUNT"|
                           mydata$EATMOREDISCOUNT=="LESS FRUITS AND VEGETABLES"]<-"Eat the same or less FV"
mydata$Discount_eat_more<-as.factor(mydata$Discount_eat_more)
freq(mydata$Discount_eat_more, plot=F)

