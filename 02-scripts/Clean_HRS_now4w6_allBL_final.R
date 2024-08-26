rm(list=ls())

my_packages <- c( "psych", "lme4", "merTools", "tidyverse", "sjstats")

#lapply(my_packages, install.packages)
lapply(my_packages, library, character.only = TRUE)

#This will tell you where everything is saved- please note this location!
setwd("P:\\Projects\\Personality\\Lonliness\\Sites\\HRS\\Data\\")
getwd()

#read data
#dataset1 <- read.csv("P:\\Projects\\Personality\\Lonliness\\Sites\\HRS\\Data\\randhrs1992_2016v2_SPSS\\randhrs1992_2016v2.csv")
#dataset2 <- read.csv("P:\\Projects\\Personality\\Lonliness\\Sites\\HRS\\Data\\HarmonizedHRS.vB\\H_HRS_b.csv")
#dataset3 <- read.csv("P:\\Projects\\Personality\\Lonliness\\Sites\\HRS\\Data\\randhrsfam1992_2014v1_SPSS\\randhrsfamr1992_2014v1.csv")
# dataset4.1 <- read.csv("P:\\Projects\\Personality\\Lonliness\\Sites\\HRS\\Data\\h02f2c_SPSS\\h02f2c.csv")
# dataset4.2 <- read.csv("P:\\Projects\\Personality\\Lonliness\\Sites\\HRS\\Data\\h04f1c_SPSS\\h04f1c.csv")
# dataset4.3 <- read.csv("P:\\Projects\\Personality\\Lonliness\\Sites\\HRS\\Data\\h06f3a_SPSS\\h06f3a.csv")
# dataset4.4 <- read.csv("P:\\Projects\\Personality\\Lonliness\\Sites\\HRS\\Data\\h08f3a_SPSS\\h08f3a.csv")
# dataset4.5 <- read.csv("P:\\Projects\\Personality\\Lonliness\\Sites\\HRS\\Data\\hd10f5e_SPSS\\hd10f5e.csv")
# dataset4.6 <- read.csv("P:\\Projects\\Personality\\Lonliness\\Sites\\HRS\\Data\\h12f2a_SPSS\\h12f2a.csv")
# dataset4.7 <- read.csv("P:\\Projects\\Personality\\Lonliness\\Sites\\HRS\\Data\\h14f2a_SPSS\\h14f2a.csv")


#save(dataset1, file = "randhrs1992_2016v2.Rdata") #rand
#save(dataset2, file = "H_HRS_b.Rdata") #harmonized
#save(dataset3, file = "randhrsfamr1992_2014v1") #family
# save(dataset4.1, file = "h02f2c.Rdata") #fat02 h
# save(dataset4.2, file = "h04f1c.Rdata") #fat04 J
# save(dataset4.3, file = "h06f3a.Rdata") #fat06 k
# save(dataset4.4, file = "h08f3a.Rdata") #fat08 l
# save(dataset4.5, file = "hd10f5e.Rdata") #fat10 M
# save(dataset4.6, file = "h12f2a.Rdata") #fat12 N
# save(dataset4.7, file = "h14f2a.Rdata") #fat14 O


load("P:\\Projects\\Personality\\Lonliness\\Sites\\HRS\\Data\\randhrs1992_2016v2.Rdata")
load("P:\\Projects\\Personality\\Lonliness\\Sites\\HRS\\Data\\H_HRS_b.Rdata")
load("P:\\Projects\\Personality\\Lonliness\\Sites\\HRS\\Data\\randhrsfamr1992_2014v1")
load("P:\\Projects\\Personality\\Lonliness\\Sites\\HRS\\Data\\h02f2c.Rdata")
load("P:\\Projects\\Personality\\Lonliness\\Sites\\HRS\\Data\\h04f1c.Rdata")
load("P:\\Projects\\Personality\\Lonliness\\Sites\\HRS\\Data\\h06f3a.Rdata")
load("P:\\Projects\\Personality\\Lonliness\\Sites\\HRS\\Data\\h08f3a.Rdata")
load("P:\\Projects\\Personality\\Lonliness\\Sites\\HRS\\Data\\hd10f5e.Rdata")
load("P:\\Projects\\Personality\\Lonliness\\Sites\\HRS\\Data\\h12f2a.Rdata")
load("P:\\Projects\\Personality\\Lonliness\\Sites\\HRS\\Data\\h14f2a.Rdata")


#need to keep only necessary varaibles because these files are stinkin huge
dataset1 <- dataset1 %>% 
  select(
    ï..HHIDPN,
    R6AGEY_B,
    R7AGEY_B,
    R8AGEY_B,
    R9AGEY_B,
    R10AGEY_B,
    R11AGEY_B,
    R12AGEY_B,
    RAGENDER,
    RAEDYRS,
    R6IEARN,
    R6BMI,
    R6SMOKEN,
    R6DRINK,
    R6MSTAT,
    R6DEPRES,
    R6EFFORT,
    R6SLEEPR,
    R6WHAPPY,
    R6MONEYA,
    R6MEDSA,
    R6SHOPA,
    R6MEALSA,
    R6DRESSA,
    R6BATHA,
    R6EATA,
    R6BEDA,
    R6IMRC,
    R6DLRC,
    R6HIBP,
    R6HEART,
    R6DIAB,
    R6MO,
    R6YR,
    R6DY,
    R6DW,
    R7IEARN,
    R7BMI,
    R7SMOKEN,
    R7DRINK,
    R7MSTAT,
    R7DEPRES,
    R7EFFORT,
    R7SLEEPR,
    R7WHAPPY,
    R7MONEYA,
    R7MEDSA,
    R7SHOPA,
    R7MEALSA,
    R7DRESSA,
    R7BATHA,
    R7EATA,
    R7BEDA,
    R7IMRC,
    R7DLRC,
    R7HIBP,
    R7HEART,
    R7DIAB,
    R7MO,
    R7YR,
    R7DY,
    R7DW,
    R8IEARN,
    R8BMI,
    R8SMOKEN,
    R8DRINK,
    R8MSTAT,
    R8DEPRES,
    R8EFFORT,
    R8SLEEPR,
    R8WHAPPY,
    R8MONEYA,
    R8MEDSA,
    R8SHOPA,
    R8MEALSA,
    R8DRESSA,
    R8BATHA,
    R8EATA,
    R8BEDA,
    R8IMRC,
    R8DLRC,
    R8HIBP,
    R8HEART,
    R8DIAB,
    R8MO,
    R8YR,
    R8DY,
    R8DW,
    R9IEARN,
    R9BMI,
    R9SMOKEN,
    R9DRINK,
    R9MSTAT,
    R9DEPRES,
    R9EFFORT,
    R9SLEEPR,
    R9WHAPPY,
    R9MONEYA,
    R9MEDSA,
    R9SHOPA,
    R9MEALSA,
    R9DRESSA,
    R9BATHA,
    R9EATA,
    R9BEDA,
    R9IMRC,
    R9DLRC,
    R9HIBP,
    R9HEART,
    R9DIAB,
    R9MO,
    R9YR,
    R9DY,
    R9DW,
    R10IEARN,
    R10BMI,
    R10SMOKEN,
    R10DRINK,
    R10MSTAT,
    R10DEPRES,
    R10EFFORT,
    R10SLEEPR,
    R10WHAPPY,
    R10MONEYA,
    R10MEDSA,
    R10SHOPA,
    R10MEALSA,
    R10DRESSA,
    R10BATHA,
    R10EATA,
    R10BEDA,
    R10IMRC,
    R10DLRC,
    R10HIBP,
    R10HEART,
    R10DIAB,
    R10MO,
    R10YR,
    R10DY,
    R10DW,
    R11IEARN,
    R11BMI,
    R11SMOKEN,
    R11DRINK,
    R11MSTAT,
    R11DEPRES,
    R11EFFORT,
    R11SLEEPR,
    R11WHAPPY,
    R11MONEYA,
    R11MEDSA,
    R11SHOPA,
    R11MEALSA,
    R11DRESSA,
    R11BATHA,
    R11EATA,
    R11BEDA,
    R11IMRC,
    R11DLRC,
    R11HIBP,
    R11HEART,
    R11DIAB,
    R11MO,
    R11YR,
    R11DY,
    R11DW,
    R12IEARN,
    R12BMI,
    R12SMOKEN,
    R12DRINK,
    R12MSTAT,
    R12DEPRES,
    R12EFFORT,
    R12SLEEPR,
    R12WHAPPY,
    R12MONEYA,
    R12MEDSA,
    R12SHOPA,
    R12MEALSA,
    R12DRESSA,
    R12BATHA,
    R12EATA,
    R12BEDA,
    R12IMRC,
    R12DLRC,
    R12HIBP,
    R12HEART,
    R12DIAB,
    R12MO,
    R12YR,
    R12DY,
    R12DW
  )
dataset1 <- dataset1 %>% dplyr::rename(id = ï..HHIDPN)

dataset2 <- dataset2 %>% 
  select(
    ï..hhidpn,
    r6complac,
    r6leftout,
    r6isolate,
    r7complac,
    r7leftout,
    r7isolate,
    r8complac,
    r8leftout,
    r8isolate,
    r9complac,
    r9leftout,
    r9isolate,
    r10complac,
    r10leftout,
    r10isolate,
    r11complac,
    r11leftout,
    r11isolate,
    r12complac,
    r12leftout,
    r12isolate,
    h6lvalone,
    h7lvalone,
    h8lvalone,
    h9lvalone,
    h10lvalone,
    h11lvalone,
    h12lvalone
  )
dataset2 <- dataset2 %>% dplyr::rename(id = ï..hhidpn)   

dataset3 <- dataset3 %>% 
  select(
    ï..HHIDPN,
    R6MCONTMO,
    R6FCONTMO,
    H6CONTKN,
    R7MCONTMO,
    R7FCONTMO,
    H7CONTKN,
    R8MCONTMO,
    R8FCONTMO,
    H8CONTKN,
    R9MCONTMO,
    R9FCONTMO,
    H9CONTKN,
    R10MCONTMO,
    R10FCONTMO,
    H10CONTKN,
    R11MCONTMO,
    R11FCONTMO,
    H11CONTKN,
    R12MCONTMO,
    R12FCONTMO,
    H12CONTKN
  )
dataset3 <- dataset3 %>% dplyr::rename(id = ï..HHIDPN)  
    
dataset4.1 <- dataset4.1 %>% 
  select(
    ï..hhidpn,
    hf176,
    hf177
  )
dataset4.1 <- dataset4.1 %>% dplyr::rename(id = ï..hhidpn)
dataset4.2 <- dataset4.2 %>% 
  select(
    ï..HHIDPN,
    JF176,
    JF177
  )
dataset4.2 <- dataset4.2 %>% dplyr::rename(id = ï..HHIDPN)
dataset4.3 <- dataset4.3 %>% 
  select(
    ï..hhidpn,
    kf176,
    kf177
  )
dataset4.3 <- dataset4.3 %>% dplyr::rename(id = ï..hhidpn)  
dataset4.4 <- dataset4.4 %>% 
  select(
    ï..lpn_sp,
    lf176,
    lf177
  )
dataset4.4 <- dataset4.4 %>% dplyr::rename(id = ï..lpn_sp)
dataset4.5 <- dataset4.5 %>% 
  select(
    ï..HHIDPN,
    MF176,
    MF177
  )
dataset4.5 <- dataset4.5 %>% dplyr::rename(id = ï..HHIDPN) 
dataset4.6 <- dataset4.6 %>% 
  select(
    ï..HHIDPN,
    NF176,
    NF177
  )
dataset4.6 <- dataset4.6 %>% dplyr::rename(id = ï..HHIDPN) 
dataset4.7 <- dataset4.7 %>% 
  select(
    ï..HHIDPN,
    OF176,
    OF177
  )
dataset4.7 <- dataset4.7 %>% dplyr::rename(id = ï..HHIDPN) 
dataset4a <- merge(x=dataset4.1, y=dataset4.2, by = "id", all.x=TRUE, all.y=TRUE)
dataset4b <-merge(x=dataset4a, y=dataset4.3, by = "id", all.x=TRUE, all.y=TRUE)
dataset4c <-merge(x=dataset4b, y=dataset4.4, by = "id", all.x=TRUE, all.y=TRUE)
dataset4d <-merge(x=dataset4c, y=dataset4.5, by = "id", all.x=TRUE, all.y=TRUE)
dataset4e <-merge(x=dataset4d, y=dataset4.6, by = "id", all.x=TRUE, all.y=TRUE)
dataset4f <-merge(x=dataset4e, y=dataset4.7, by = "id", all.x=TRUE, all.y=TRUE)


dataset12 <-merge(x = dataset1, y = dataset2, by = "id", all.x = TRUE)
dataset123 <-merge(x = dataset12, y = dataset3, by = "id", all.x = TRUE)
dataset <-merge(x = dataset123, y = dataset4f, by = "id", all.x = TRUE)

#create meta-data
i = "HRS" #study name
MO = "5" #number of measurement occasions
year = "2002" #baseline year
interval = "2" #average years between measurement occasions
followup = "14" #total years from baseline to final MO
continent = "North America"
lonely = "UCLA" #name of loneliness item, scale


#Data wrangle:
#Loneliness (high score = high loneliness)
dataset <- dataset %>% 
  mutate(
    r6complac=recode(r6complac, 
                     '1' = 3,
                     '2' = 2, 
                     '3' = 1),
    r7complac=recode(r7complac, 
                     '1' = 3,
                     '2' = 2, 
                     '3' = 1),
    r8complac=recode(r8complac, 
                     '1' = 3,
                     '2' = 2, 
                     '3' = 1),
    r9complac=recode(r9complac, 
                     '1' = 3,
                     '2' = 2, 
                     '3' = 1),
    r10complac=recode(r10complac, 
                      '1' = 3,
                      '2' = 2, 
                      '3' = 1),
    r11complac=recode(r11complac, 
                      '1' = 3,
                      '2' = 2, 
                      '3' = 1),
    r12complac=recode(r12complac, 
                      '1' = 3,
                      '2' = 2, 
                      '3' = 1),
    r6leftout=recode(r6leftout, 
                     '1' = 3,
                     '2' = 2, 
                     '3' = 1),
    r7leftout=recode(r7leftout, 
                     '1' = 3,
                     '2' = 2, 
                     '3' = 1),
    r8leftout=recode(r8leftout, 
                     '1' = 3,
                     '2' = 2, 
                     '3' = 1),
    r9leftout=recode(r9leftout, 
                     '1' = 3,
                     '2' = 2, 
                     '3' = 1),
    r10leftout=recode(r10leftout, 
                      '1' = 3,
                      '2' = 2, 
                      '3' = 1),
    r11leftout=recode(r11leftout, 
                      '1' = 3,
                      '2' = 2, 
                      '3' = 1),
    r12leftout=recode(r12leftout, 
                      '1' = 3,
                      '2' = 2, 
                      '3' = 1),
    r6isolate=recode(r6isolate, 
                     '1' = 3,
                     '2' = 2, 
                     '3' = 1),
    r7isolate=recode(r7isolate, 
                     '1' = 3,
                     '2' = 2, 
                     '3' = 1),
    r8isolate=recode(r8isolate, 
                     '1' = 3,
                     '2' = 2, 
                     '3' = 1),
    r9isolate=recode(r9isolate, 
                     '1' = 3,
                     '2' = 2, 
                     '3' = 1),
    r10isolate=recode(r10isolate, 
                      '1' = 3,
                      '2' = 2, 
                      '3' = 1),
    r11isolate=recode(r11isolate, 
                      '1' = 3,
                      '2' = 2, 
                      '3' = 1),
    r12isolate=recode(r12isolate, 
                      '1' = 3,
                      '2' = 2, 
                      '3' = 1)
  )
dataset <- dataset %>% 
  dplyr::mutate(lonely1 = rowMeans(select(., c("r6complac", "r6leftout", "r6isolate")),na.rm = TRUE)*3,
                lonely2 = rowMeans(select(., c("r7complac", "r7leftout", "r7isolate")),na.rm = TRUE)*3,
                lonely3 = rowMeans(select(., c("r8complac", "r8leftout", "r8isolate")),na.rm = TRUE)*3,
                lonely4 = rowMeans(select(., c("r9complac", "r9leftout", "r9isolate")),na.rm = TRUE)*3,
                lonely5 = rowMeans(select(., c("r10complac", "r10leftout", "r10isolate")),na.rm = TRUE)*3,
                lonely6 = rowMeans(select(., c("r11complac", "r11leftout", "r11isolate")),na.rm = TRUE)*3,
                lonely7 = rowMeans(select(., c("r12complac", "r12leftout", "r12isolate")),na.rm = TRUE)*3,
    lonelyfirst = ifelse(!is.na(lonely1), 1, 
                         ifelse(!is.na(lonely2), 2,  
                                ifelse(!is.na(lonely3), 3, 
                                       ifelse(!is.na(lonely4), 4, 
                                              ifelse(!is.na(lonely5), 5, 
                                                    ifelse(!is.na(lonely6), 6, 
                                                         ifelse(!is.na(lonely7), 7, 0))))))))

#separate out dataset by baseline loneliness wave
dataset.w1 <- filter(dataset,lonelyfirst==1)
dataset.w2 <- filter(dataset,lonelyfirst==2)
dataset.w3 <- filter(dataset,lonelyfirst==3)
dataset.w4 <- filter(dataset,lonelyfirst==4)
dataset.w5 <- filter(dataset,lonelyfirst==5)
dataset.w6 <- filter(dataset,lonelyfirst==6)
dataset.w7 <- filter(dataset,lonelyfirst==7)

#rename variables #
dataset.w1 <- dataset.w1 %>%
  dplyr::rename(income = R6IEARN,
                bmi = R6BMI,
                smoke = R6SMOKEN,
                drink = R6DRINK,
                marital = R6MSTAT,
                depressed = R6DEPRES,
                # effort = R6EFFORT,
                # sleep = R6SLEEPR,
                # happy = R6WHAPPY,
                money = R6MONEYA,
                meds = R6MEDSA,
                groceries = R6SHOPA,
                meal = R6MEALSA,
                dress = R6DRESSA,
                bath = R6BATHA,
                eat = R6EATA,
                bed = R6BEDA,
                episodic_i = R6IMRC,
                episodic_d = R6DLRC,
                momcont = R6MCONTMO,
                dadcont = R6FCONTMO,
                childcont = H6CONTKN,
                livealone = h6lvalone,
                v176 = hf176,
                v177 = hf177,
                RxHIBP = R6HIBP,
                RxHEART= R6HEART,
                RxDIAB= R6DIAB,
                RxMO= R6MO,
                RxYR= R6YR,
                RxDY= R6DY,
                RxDW= R6DW
  ) %>%
  select(id,R6AGEY_B,R7AGEY_B,R8AGEY_B,R9AGEY_B,R10AGEY_B,R11AGEY_B,R12AGEY_B,RAGENDER,RAEDYRS,income,bmi,smoke,drink,marital,
         depressed,money,meds,groceries,meal,dress,bath,eat,bed,episodic_i,episodic_d,
         momcont,dadcont,childcont,livealone,v176,v177,RxHIBP,RxHEART,RxDIAB,RxMO,RxYR,RxDY,RxDW,
         lonelyfirst,lonely1,lonely2,lonely3,lonely4,lonely5,lonely6,lonely7,
         r6complac, r6leftout, r6isolate, r7complac, r7leftout, r7isolate, r8complac, r8leftout, r8isolate, r9complac, r9leftout, r9isolate, 
         r10complac, r10leftout, r10isolate, r11complac, r11leftout, r11isolate, r12complac, r12leftout, r12isolate)
dataset.w2 <- dataset.w2 %>%
  dplyr::rename(income = R7IEARN,
                bmi = R7BMI,
                smoke = R7SMOKEN,
                drink = R7DRINK,
                marital = R7MSTAT,
                depressed = R7DEPRES,
                # effort = R7EFFORT,
                # sleep = R7SLEEPR,
                # happy = R7WHAPPY,
                money = R7MONEYA,
                meds = R7MEDSA,
                groceries = R7SHOPA,
                meal = R7MEALSA,
                dress = R7DRESSA,
                bath = R7BATHA,
                eat = R7EATA,
                bed = R7BEDA,
                episodic_i = R7IMRC,
                episodic_d = R7DLRC,
                momcont = R7MCONTMO,
                dadcont = R7FCONTMO,
                childcont = H7CONTKN,
                livealone = h7lvalone,
                v176 = JF176,
                v177 = JF177,
                RxHIBP = R7HIBP,
                RxHEART= R7HEART,
                RxDIAB= R7DIAB,
                RxMO= R7MO,
                RxYR= R7YR,
                RxDY= R7DY,
                RxDW= R7DW
  ) %>%
  select(id,R6AGEY_B,R7AGEY_B,R8AGEY_B,R9AGEY_B,R10AGEY_B,R11AGEY_B,R12AGEY_B,RAGENDER,RAEDYRS,income,bmi,smoke,drink,marital,
         depressed,money,meds,groceries,meal,dress,bath,eat,bed,episodic_i,episodic_d,
         momcont,dadcont,childcont,livealone,v176,v177,RxHIBP,RxHEART,RxDIAB,RxMO,RxYR,RxDY,RxDW,
         lonelyfirst,lonely1,lonely2,lonely3,lonely4,lonely5,lonely6,lonely7,
         r6complac, r6leftout, r6isolate, r7complac, r7leftout, r7isolate, r8complac, r8leftout, r8isolate, r9complac, r9leftout, r9isolate, 
         r10complac, r10leftout, r10isolate, r11complac, r11leftout, r11isolate, r12complac, r12leftout, r12isolate)
dataset.w3 <- dataset.w3 %>%
  dplyr::rename(income = R8IEARN,
                bmi = R8BMI,
                smoke = R8SMOKEN,
                drink = R8DRINK,
                marital = R8MSTAT,
                depressed = R8DEPRES,
                # effort = R8EFFORT,
                # sleep = R8SLEEPR,
                # happy = R8WHAPPY,
                money = R8MONEYA,
                meds = R8MEDSA,
                groceries = R8SHOPA,
                meal = R8MEALSA,
                dress = R8DRESSA,
                bath = R8BATHA,
                eat = R8EATA,
                bed = R8BEDA,
                episodic_i = R8IMRC,
                episodic_d = R8DLRC,
                momcont = R8MCONTMO,
                dadcont = R8FCONTMO,
                childcont = H8CONTKN,
                livealone = h8lvalone,
                v176 = kf176,
                v177 = kf177,
                RxHIBP = R8HIBP,
                RxHEART= R8HEART,
                RxDIAB= R8DIAB,
                RxMO= R8MO,
                RxYR= R8YR,
                RxDY= R8DY,
                RxDW= R8DW
  )%>%
  select(id,R6AGEY_B,R7AGEY_B,R8AGEY_B,R9AGEY_B,R10AGEY_B,R11AGEY_B,R12AGEY_B,RAGENDER,RAEDYRS,income,bmi,smoke,drink,marital,
         depressed,money,meds,groceries,meal,dress,bath,eat,bed,episodic_i,episodic_d,
         momcont,dadcont,childcont,livealone,v176,v177,RxHIBP,RxHEART,RxDIAB,RxMO,RxYR,RxDY,RxDW,
         lonelyfirst,lonely1,lonely2,lonely3,lonely4,lonely5,lonely6,lonely7,
         r6complac, r6leftout, r6isolate, r7complac, r7leftout, r7isolate, r8complac, r8leftout, r8isolate, r9complac, r9leftout, r9isolate, 
         r10complac, r10leftout, r10isolate, r11complac, r11leftout, r11isolate, r12complac, r12leftout, r12isolate)
dataset.w4 <- dataset.w4 %>%
  dplyr::rename(income = R9IEARN,
                bmi = R9BMI,
                smoke = R9SMOKEN,
                drink = R9DRINK,
                marital = R9MSTAT,
                depressed = R9DEPRES,
                # effort = R9EFFORT,
                # sleep = R9SLEEPR,
                # happy = R9WHAPPY,
                money = R9MONEYA,
                meds = R9MEDSA,
                groceries = R9SHOPA,
                meal = R9MEALSA,
                dress = R9DRESSA,
                bath = R9BATHA,
                eat = R9EATA,
                bed = R9BEDA,
                episodic_i = R9IMRC,
                episodic_d = R9DLRC,
                momcont = R9MCONTMO,
                dadcont = R9FCONTMO,
                childcont = H9CONTKN,
                livealone = h9lvalone,
                v176 = lf176,
                v177 = lf177,
                RxHIBP = R9HIBP,
                RxHEART= R9HEART,
                RxDIAB= R9DIAB,
                RxMO= R9MO,
                RxYR= R9YR,
                RxDY= R9DY,
                RxDW= R9DW
  )%>%
  select(id,R6AGEY_B,R7AGEY_B,R8AGEY_B,R9AGEY_B,R10AGEY_B,R11AGEY_B,R12AGEY_B,RAGENDER,RAEDYRS,income,bmi,smoke,drink,marital,
         depressed,money,meds,groceries,meal,dress,bath,eat,bed,episodic_i,episodic_d,
         momcont,dadcont,childcont,livealone,v176,v177,RxHIBP,RxHEART,RxDIAB,RxMO,RxYR,RxDY,RxDW,
         lonelyfirst,lonely1,lonely2,lonely3,lonely4,lonely5,lonely6,lonely7,
         r6complac, r6leftout, r6isolate, r7complac, r7leftout, r7isolate, r8complac, r8leftout, r8isolate, r9complac, r9leftout, r9isolate, 
         r10complac, r10leftout, r10isolate, r11complac, r11leftout, r11isolate, r12complac, r12leftout, r12isolate)
dataset.w5 <- dataset.w5 %>%
  dplyr::rename(income = R10IEARN,
                bmi = R10BMI,
                smoke = R10SMOKEN,
                drink = R10DRINK,
                marital = R10MSTAT,
                depressed = R10DEPRES,
                # effort = R10EFFORT,
                # sleep = R10SLEEPR,
                # happy = R10WHAPPY,
                money = R10MONEYA,
                meds = R10MEDSA,
                groceries = R10SHOPA,
                meal = R10MEALSA,
                dress = R10DRESSA,
                bath = R10BATHA,
                eat = R10EATA,
                bed = R10BEDA,
                episodic_i = R10IMRC,
                episodic_d = R10DLRC,
                momcont = R10MCONTMO,
                dadcont = R10FCONTMO,
                childcont = H10CONTKN,
                livealone = h10lvalone,
                v176 = MF176,
                v177 = MF177,
                RxHIBP = R10HIBP,
                RxHEART= R10HEART,
                RxDIAB= R10DIAB,
                RxMO= R10MO,
                RxYR= R10YR,
                RxDY= R10DY,
                RxDW= R10DW
  )%>%
  select(id,R6AGEY_B,R7AGEY_B,R8AGEY_B,R9AGEY_B,R10AGEY_B,R11AGEY_B,R12AGEY_B,RAGENDER,RAEDYRS,income,bmi,smoke,drink,marital,
         depressed,money,meds,groceries,meal,dress,bath,eat,bed,episodic_i,episodic_d,
         momcont,dadcont,childcont,livealone,v176,v177,RxHIBP,RxHEART,RxDIAB,RxMO,RxYR,RxDY,RxDW,
         lonelyfirst,lonely1,lonely2,lonely3,lonely4,lonely5,lonely6,lonely7,
         r6complac, r6leftout, r6isolate, r7complac, r7leftout, r7isolate, r8complac, r8leftout, r8isolate, r9complac, r9leftout, r9isolate, 
         r10complac, r10leftout, r10isolate, r11complac, r11leftout, r11isolate, r12complac, r12leftout, r12isolate)
dataset.w6 <- dataset.w6 %>%
  dplyr::rename(income = R11IEARN,
                bmi = R11BMI,
                smoke = R11SMOKEN,
                drink = R11DRINK,
                marital = R11MSTAT,
                depressed = R11DEPRES,
                # effort = R11EFFORT,
                # sleep = R11SLEEPR,
                # happy = R11WHAPPY,
                money = R11MONEYA,
                meds = R11MEDSA,
                groceries = R11SHOPA,
                meal = R11MEALSA,
                dress = R11DRESSA,
                bath = R11BATHA,
                eat = R11EATA,
                bed = R11BEDA,
                episodic_i = R11IMRC,
                episodic_d = R11DLRC,
                momcont = R11MCONTMO,
                dadcont = R11FCONTMO,
                childcont = H11CONTKN,
                livealone = h11lvalone,
                v176 = NF176,
                v177 = NF177,
                RxHIBP = R11HIBP,
                RxHEART= R11HEART,
                RxDIAB= R11DIAB,
                RxMO= R11MO,
                RxYR= R11YR,
                RxDY= R11DY,
                RxDW= R11DW
  )%>%
  select(id,R6AGEY_B,R7AGEY_B,R8AGEY_B,R9AGEY_B,R10AGEY_B,R11AGEY_B,R12AGEY_B,RAGENDER,RAEDYRS,income,bmi,smoke,drink,marital,
         depressed,money,meds,groceries,meal,dress,bath,eat,bed,episodic_i,episodic_d,
         momcont,dadcont,childcont,livealone,v176,v177,RxHIBP,RxHEART,RxDIAB,RxMO,RxYR,RxDY,RxDW,
         lonelyfirst,lonely1,lonely2,lonely3,lonely4,lonely5,lonely6,lonely7,
         r6complac, r6leftout, r6isolate, r7complac, r7leftout, r7isolate, r8complac, r8leftout, r8isolate, r9complac, r9leftout, r9isolate, 
         r10complac, r10leftout, r10isolate, r11complac, r11leftout, r11isolate, r12complac, r12leftout, r12isolate)
dataset.w7 <- dataset.w7 %>%
  dplyr::rename(income = R12IEARN,
                bmi = R12BMI,
                smoke = R12SMOKEN,
                drink = R12DRINK,
                marital = R12MSTAT,
                depressed = R12DEPRES,
                # effort = R12EFFORT,
                # sleep = R12SLEEPR,
                # happy = R12WHAPPY,
                money = R12MONEYA,
                meds = R12MEDSA,
                groceries = R12SHOPA,
                meal = R12MEALSA,
                dress = R12DRESSA,
                bath = R12BATHA,
                eat = R12EATA,
                bed = R12BEDA,
                episodic_i = R12IMRC,
                episodic_d = R12DLRC,
                momcont = R12MCONTMO,
                dadcont = R12FCONTMO,
                childcont = H12CONTKN,
                livealone = h12lvalone,
                v176 = OF176,
                v177 = OF177,
                RxHIBP = R12HIBP,
                RxHEART= R12HEART,
                RxDIAB= R12DIAB,
                RxMO= R12MO,
                RxYR= R12YR,
                RxDY= R12DY,
                RxDW= R12DW
  )%>%
  select(id,R6AGEY_B,R7AGEY_B,R8AGEY_B,R9AGEY_B,R10AGEY_B,R11AGEY_B,R12AGEY_B,RAGENDER,RAEDYRS,income,bmi,smoke,drink,marital,
         depressed,money,meds,groceries,meal,dress,bath,eat,bed,episodic_i,episodic_d,
         momcont,dadcont,childcont,livealone,v176,v177,RxHIBP,RxHEART,RxDIAB,RxMO,RxYR,RxDY,RxDW,
         lonelyfirst,lonely1,lonely2,lonely3,lonely4,lonely5,lonely6,lonely7,
         r6complac, r6leftout, r6isolate, r7complac, r7leftout, r7isolate, r8complac, r8leftout, r8isolate, r9complac, r9leftout, r9isolate, 
         r10complac, r10leftout, r10isolate, r11complac, r11leftout, r11isolate, r12complac, r12leftout, r12isolate)

dataset <- rbind(dataset.w1, dataset.w2, dataset.w3, dataset.w4, dataset.w5, dataset.w6, dataset.w7)

dataset <- dataset %>%
  dplyr::rename(id = id,
                age1 = R6AGEY_B,
                age2 = R7AGEY_B,
                age3 = R8AGEY_B,
                age4 = R9AGEY_B,
                age5 = R10AGEY_B,
                age6 = R11AGEY_B,
                age7 = R12AGEY_B,
                sex = RAGENDER,
                educ = RAEDYRS
)
#create constructed variables
#baseline isolation (code such that isolated = 1, not isolated = 0)
dataset <- dataset %>% 
  mutate(momcont_wk = momcont/4, #convert from monthly to weekly
         dadcont_wk= dadcont/4, #convert from monthly to weekly
         parentcont = ifelse(momcont_wk >= 1 | dadcont_wk >= 1,0,1),

         childcont= ifelse(childcont >= 1, 0, 1),
         livealone=recode(livealone, 
                          '1' = 1, 
                          '0' = 0),
         numbersoc= ifelse(v176 > 365, 0, v176),
         socpart=ifelse(numbersoc > 0, 0, 1)) %>% 
  mutate(b.isolated = rowMeans(cbind(childcont, livealone, parentcont, socpart), na.rm=T))  %>%  #use childcont, parentcont, socpart, livealone
  mutate(b.isolated = as.numeric(scale(b.isolated)))

         
        
         
#marital status  (ALL)
dataset <- dataset %>% 
  mutate(marital = ifelse(marital == 1, "A", #married = married
                          ifelse(marital == 3, "A", #partnered = married
                                 ifelse(marital == 5, "B", #divorced = divorced
                                        ifelse(marital == 7, "C", #widowed = widowed
                                               ifelse(marital == 8, "D", NA))))), #never married = never married
  married = ifelse(marital == "A", 1,0),
  divorced = ifelse(marital == "B", 1,0),
  widowed = ifelse(marital == "C", 1,0),
  never.married = ifelse(marital == "D", 1,0))

#gender
dataset <- dataset %>%
  mutate(sex = ifelse(sex == 1, 0, #male
                       ifelse(sex == 2, 1, NA) #female
  ))


#depression
dataset <- dataset %>% 
  mutate(
    depression = depressed)


#chronic conditions
dataset <- dataset %>% 
  mutate(hbp = ifelse(RxHIBP == 1 | RxHIBP == 3, 1, 0))  %>%  #3 =1; 4=0;
  mutate(heart = ifelse(RxHEART == 1 | RxHEART == 3, 1, 0))  %>%  #3 =1; 4=0;
  mutate(diab = ifelse(RxDIAB == 1 | RxDIAB == 3, 1, 0))  %>%  #3 =1; 4=0;
  mutate(cc = (hbp + heart + diab))


dataset <- dataset %>%
  mutate(functional = as.numeric(rowMeans(cbind(money, meds, groceries, meal, dress, bath, eat, bed), na.rm=T)*8))  %>% #these were all 0/1, but some are missing, so take mean
  mutate(functional = as.numeric(scale(functional)))


#cognitive variables
dataset <- dataset %>%
  mutate(mstat = rowMeans(cbind(RxMO, RxYR, RxDY, RxDW), na.rm=T)) %>%
  mutate(mstat = as.numeric(scale(mstat)))  %>%
  mutate(episodic = rowMeans(cbind(episodic_i, episodic_d), na.rm=T)) %>%
  mutate(episodic = as.numeric(scale(episodic)))


# Center Age
age.vars<- c("age1", "age2", "age3", "age4", "age5", "age6", "age7")
age.vars.recode <- dataset %>%
  dplyr::select(id, age.vars) %>%
  gather(key = "key", value = "value", -id) %>%
  mutate(raw = value,
         value = (value-60)) %>%
  filter(!is.na(value)) %>%
  gather(key = "var", value = "value", raw, value) %>%
  unite(key, key, var) %>%
  mutate(key = gsub("_value", "", key)) %>%
  spread(key = "key", value = "value")

# remove add modified age variables
dataset = dataset %>%
  dplyr::select(-which(names(.) %in% age.vars)) %>%
  full_join(age.vars.recode, by = "id")

cat("Age has been recoded. Units are now years since age 60.\n")


#create baseline age, z-score education/income/bmi
dataset <- dataset %>% 
  mutate(b.age = apply(dataset[,c("age1", "age2", "age3", "age4", "age5", "age6", "age7")], 1, min, na.rm = TRUE),
         b.age.yrs = apply(dataset[,c("age1_raw", "age2_raw", "age3_raw", "age4_raw", "age5_raw", "age6_raw", "age7_raw")], 1, min, na.rm = TRUE),
         educ = as.numeric(scale(educ)),
         income = as.numeric(scale(income)),
         bmi = as.numeric(scale(as.numeric(bmi)))
  )

#update b.age.yrs when all are missing
is.na(dataset$b.age) <- sapply(dataset$b.age, is.infinite)
is.na(dataset$b.age.yrs) <- sapply(dataset$b.age.yrs, is.infinite)

##If subject does not have three time points, drop them from analysis
dataset <- dataset  %>% 
  mutate(
 lonely1cat = ifelse(is.na(lonely1), 0, 1),
 lonely2cat = ifelse(is.na(lonely2), 0, 1),
 lonely3cat = ifelse(is.na(lonely3), 0, 1),
 lonely5cat = ifelse(is.na(lonely5), 0, 1),
 lonely7cat = ifelse(is.na(lonely7), 0, 1),
 sum = rowSums(cbind(lonely1cat, lonely2cat, lonely3cat, lonely5cat, lonely7cat)))  %>%
 filter (sum > 2)


dataset <- dataset %>% 
  select(
    id,
    b.age.yrs,
    age1,
    age2,
    age3,
    #age4,
    age5,
    #age6,
    age7,
    lonely1,
    lonely2,
    lonely3,
    #lonely4,
    lonely5,
    #lonely6,
    lonely7,
    sex,
    educ,
    income,
    bmi,
    smoke,
    drink,
    marital,
    married,
    divorced,
    widowed,
    never.married,
    # depressed,
    # effort,
    # sleep,
    # unhappy,
    # money,
    # meds,
    # groceries,
    # meal,
    # dress,
    # bath,
    # eat,
    # bed,
    # episodic_i,
    # episodic_d,
    # childcont, 
    # livealone, 
    # parentcont, 
    # socpart,
    # heart,
    # hbp,
    # diab,
    b.isolated,
    depression,
    cc,
    functional,
    mstat,
    episodic,
    b.age)


save(dataset, file = "HRS_clean_r.Rdata")



## HRS
### Load Data  
data_path <- "P:/Projects/Personality/Lonliness/All_Data"
wd <- "P:/Projects/Personality/Lonliness/Emorie_Update"
load(sprintf("%s/HRS_clean_r.RData", data_path))

### Recoding
dataset$SID <- as.character(dataset$id)
hrs_comb<-dataset
rm(dataset)

# functions 
pomp_fun <- function(x){
  r <- range(x, na.rm = T)
  10*((x - r[1])/(r[2] - r[1]))
} 

z_fun <- function(x) (x - mean(x, na.rm = T))/sd(x, na.rm = T)

#POMP score
dataset <- dataset %>%
  mutate_at(vars(lonely), pomp_fun)

### Save
save(hrs_comb, file = "P:/Projects/Personality/Lonliness/Emorie_Update/hrs_clean.Rdata")

