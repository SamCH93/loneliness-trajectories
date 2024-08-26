rm(list=ls())

#install and load packages
my_packages <- c( "psych", "lme4", "merTools", "tidyverse", "sjstats")

#lapply(my_packages, install.packages)
lapply(my_packages, library, character.only = TRUE)

#This will tell you where everything is saved- please note this location!
setwd("P:\\Projects\\Personality\\Lonliness\\Sites\\ELSA\\Data\\")
getwd()

#read data
#dataset <- read.csv("ELSA_clean.csv")
#save(dataset, file = "ELSA_clean.Rdata")
load("P:/Projects/Personality/Lonliness/Sites/ELSA/Data/ELSA_clean.Rdata")

#create meta-data
i = "ELSA" #study name
MO = "7" #number of measurement occasions
year = "2002" #baseline year
interval = "2" #average years between measurement occasions
followup = "14" #total years from baseline to final MO
continent = "Europe"
lonely = "UCLA" #name of loneliness item, scale


#Data wrangle:
#Loneliness (high score = high loneliness)
dataset <- dataset %>% 
  mutate(
    r2complac=recode(r2complac, 
                     "1.hardly ever or never" = 1,
                     "2.some of the time" = 2, 
                     "3.often" = 3),
    r3complac=recode(r3complac, 
                     "1.hardly ever or never" = 1,
                     "2.some of the time" = 2, 
                     "3.often" = 3),
    r4complac=recode(r4complac, 
                     "1.hardly ever or never" = 1,
                     "2.some of the time" = 2, 
                     "3.often" = 3),
    r5complac=recode(r5complac, 
                     "1.hardly ever or never" = 1,
                     "2.some of the time" = 2, 
                     "3.often" = 3),
    r6complac=recode(r6complac, 
                     "1.hardly ever or never" = 1,
                     "2.some of the time" = 2, 
                     "3.often" = 3),
    r7complac=recode(r7complac, 
                     "1.hardly ever or never" = 1,
                     "2.some of the time" = 2, 
                     "3.often" = 3),
    r8complac=recode(r8complac, 
                     "1.hardly ever or never" = 1,
                     "2.some of the time" = 2, 
                     "3.often" = 3),
    r2leftout=recode(r2leftout, 
                     "1.hardly ever or never" = 1,
                     "2.some of the time" = 2, 
                     "3.often" = 3),
    r3leftout=recode(r3leftout, 
                     "1.hardly ever or never" = 1,
                     "2.some of the time" = 2, 
                     "3.often" = 3),
    r4leftout=recode(r4leftout, 
                     "1.hardly ever or never" = 1,
                     "2.some of the time" = 2, 
                     "3.often" = 3),
    r5leftout=recode(r5leftout, 
                     "1.hardly ever or never" = 1,
                     "2.some of the time" = 2, 
                     "3.often" = 3),
    r6leftout=recode(r6leftout, 
                     "1.hardly ever or never" = 1,
                     "2.some of the time" = 2, 
                     "3.often" = 3),
    r7leftout=recode(r7leftout,
                     "1.hardly ever or never" = 1,
                     "2.some of the time" = 2, 
                     "3.often" = 3), 
    r8leftout=recode(r8leftout, 
                     "1.hardly ever or never" = 1,
                     "2.some of the time" = 2, 
                     "3.often" = 3),
    r2isolate=recode(r2isolate, 
                     "1.hardly ever or never" = 1,
                     "2.some of the time" = 2, 
                     "3.often" = 3),
    r3isolate=recode(r3isolate, 
                     "1.hardly ever or never" = 1,
                     "2.some of the time" = 2, 
                     "3.often" = 3),
    r4isolate=recode(r4isolate, 
                     "1.hardly ever or never" = 1,
                     "2.some of the time" = 2, 
                     "3.often" = 3),
    r5isolate=recode(r5isolate, 
                     "1.hardly ever or never" = 1,
                     "2.some of the time" = 2, 
                     "3.often" = 3),
    r6isolate=recode(r6isolate, 
                     "1.hardly ever or never" = 1,
                     "2.some of the time" = 2, 
                     "3.often" = 3),
    r7isolate=recode(r7isolate, 
                     "1.hardly ever or never" = 1,
                     "2.some of the time" = 2, 
                     "3.often" = 3),
    r8isolate=recode(r8isolate, 
                     "1.hardly ever or never" = 1,
                     "2.some of the time" = 2, 
                     "3.often" = 3)) 

#Loneliness
dataset <- dataset %>%
  dplyr::mutate(lonely1 = rowMeans(select(., c("r2complac", "r2leftout", "r2isolate")),na.rm = TRUE)*3,
                lonely2 = rowMeans(select(., c("r3complac", "r3leftout", "r3isolate")),na.rm = TRUE)*3,
                lonely3 = rowMeans(select(., c("r4complac", "r4leftout", "r4isolate")),na.rm = TRUE)*3,
                lonely4 = rowMeans(select(., c("r5complac", "r5leftout", "r5isolate")),na.rm = TRUE)*3,
                lonely5 = rowMeans(select(., c("r6complac", "r6leftout", "r6isolate")),na.rm = TRUE)*3,
                lonely6 = rowMeans(select(., c("r7complac", "r7leftout", "r7isolate")),na.rm = TRUE)*3,
                lonely7 = rowMeans(select(., c("r8complac", "r8leftout", "r8isolate")),na.rm = TRUE)*3,
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



#rename variables
dataset.w1 <- dataset.w1 %>%
  dplyr::rename(
                income = r2itearn,
                bmi = r2bmi,
                smoke = r2smoken,
                drink = r2drink,
                marital = r2mstat,
                depressed = r2depres,
                # effort = r2effort,
                # sleep = r2sleepr,
                # happy = r2whappy,
                money = r2moneya,
                meds = r2medsa,
                groceries = r2shopa,
                meal = r2mealsa,
                dress = r2dressa,
                bath = r2batha,
                eat = r2eata,
                bed = r2beda,
                episodic_i = r2imrc,
                episodic_d =  r2dlrc,
                childcont = r2kcnt,
                friendcont = r2fcnt,
                parentcont = r2rcnt,
                socpart = r2socyr,
                numlivehh = h2hhres,
                heart = r2hearte,
                hbp = r2hibpe,
                diab = r2diabe,
                rxmo = r2mo,
                rxyr = r2yr,
                rxdy = r2dy,
                rxdw = r2dw
  ) %>%
  select(idauniqc, r2agey, r3agey, r4agey, r5agey, r6agey, r7agey, r8agey, ragender, raeduc_e, income, bmi, smoke, drink, marital, depressed, 
         money, meds, groceries, meal, dress, bath, eat, bed, episodic_i, episodic_d, childcont, friendcont, parentcont, socpart, numlivehh,
         heart, hbp, diab, rxmo, rxyr, rxdy, rxdw,lonelyfirst,lonely1,lonely2,lonely3,lonely4,lonely5,lonely6,lonely7,
         r2complac, r2leftout, r2isolate, r3complac, r3leftout, r3isolate, r4complac, r4leftout, r4isolate, r5complac, r5leftout, r5isolate,
         r6complac, r6leftout, r6isolate, r7complac, r7leftout, r7isolate, r8complac, r8leftout, r8isolate)

dataset.w2 <- dataset.w2 %>%
  dplyr::rename(
    income = r3itearn,
    bmi = r4bmi, #no r3 bmi, so take following year
    smoke = r3smoken,
    drink = r3drink,
    marital = r3mstat,
    depressed = r3depres,
    # effort = r3effort,
    # sleep = r3sleepr,
    # happy = r3whappy,
    money = r3moneya,
    meds = r3medsa,
    groceries = r3shopa,
    meal = r3mealsa,
    dress = r3dressa,
    bath = r3batha,
    eat = r3eata,
    bed = r3beda,
    episodic_i = r3imrc,
    episodic_d =  r3dlrc,
    childcont = r3kcnt,
    friendcont = r3fcnt,
    parentcont = r3rcnt,
    socpart = r3socyr,
    numlivehh = h2hhres,
    heart = r3hearte,
    hbp = r3hibpe,
    diab = r3diabe,
    rxmo = r3mo,
    rxyr = r3yr,
    rxdy = r3dy,
    rxdw = r3dw
  ) %>%
  select(idauniqc, r2agey, r3agey, r4agey, r5agey, r6agey, r7agey, r8agey, ragender, raeduc_e, income, bmi, smoke, drink, marital, depressed,
         money, meds, groceries, meal, dress, bath, eat, bed, episodic_i, episodic_d, childcont, friendcont, parentcont, socpart, numlivehh,
         heart, hbp, diab, rxmo, rxyr, rxdy, rxdw,lonelyfirst,lonely1,lonely2,lonely3,lonely4,lonely5,lonely6,lonely7,
         r2complac, r2leftout, r2isolate, r3complac, r3leftout, r3isolate, r4complac, r4leftout, r4isolate, r5complac, r5leftout, r5isolate,
         r6complac, r6leftout, r6isolate, r7complac, r7leftout, r7isolate, r8complac, r8leftout, r8isolate)

dataset.w3 <- dataset.w3 %>%
  dplyr::rename(
    income = r4itearn,
    bmi = r4bmi,
    smoke = r4smoken,
    drink = r4drink,
    marital = r4mstat,
    depressed = r4depres,
    # effort = r4effort,
    # sleep = r4sleepr,
    # happy = r4whappy,
    money = r4moneya,
    meds = r4medsa,
    groceries = r4shopa,
    meal = r4mealsa,
    dress = r4dressa,
    bath = r4batha,
    eat = r4eata,
    bed = r4beda,
    episodic_i = r4imrc,
    episodic_d =  r4dlrc,
    childcont = r4kcnt,
    friendcont = r4fcnt,
    socpart = r4socyr,
    numlivehh = h2hhres,
    heart = r4hearte,
    hbp = r4hibpe,
    diab = r4diabe,
    rxmo = r4mo,
    rxyr = r4yr,
    rxdy = r4dy,
    rxdw = r4dw
  ) %>%
  mutate( #do not have composite parent contact at 4
    parentcont = ifelse(r4rcntf == "1.yes" | r4rcntm == "1.yes", "1.yes",
                        ifelse(r4rcntf == "0.no" & r4rcntm == "0.no", "0.no",
                               ifelse(r4rcntf == ".n:Not Applicable" & r4rcntm == ".n:Not Applicable", ".n:Not Applicable", NA))) 
  )%>%
  select(idauniqc, r2agey, r3agey, r4agey, r5agey, r6agey, r7agey, r8agey, ragender, raeduc_e, income, bmi, smoke, drink, marital, depressed,
         money, meds, groceries, meal, dress, bath, eat, bed, episodic_i, episodic_d, childcont, friendcont, parentcont, socpart, numlivehh,
         heart, hbp, diab, rxmo, rxyr, rxdy, rxdw,lonelyfirst,lonely1,lonely2,lonely3,lonely4,lonely5,lonely6,lonely7,
         r2complac, r2leftout, r2isolate, r3complac, r3leftout, r3isolate, r4complac, r4leftout, r4isolate, r5complac, r5leftout, r5isolate,
         r6complac, r6leftout, r6isolate, r7complac, r7leftout, r7isolate, r8complac, r8leftout, r8isolate)
    
dataset.w4 <- dataset.w4 %>%
  dplyr::rename(
    income = r5itearn,
    bmi = r6bmi,  #no r5 bmi, so take following year
    smoke = r5smoken,
    drink = r5drink,
    marital = r5mstat,
    depressed = r5depres,
    # effort = r5effort,
    # sleep = r5sleepr,
    # happy = r5whappy,
    money = r5moneya,
    meds = r5medsa,
    groceries = r5shopa,
    meal = r5mealsa,
    dress = r5dressa,
    bath = r5batha,
    eat = r5eata,
    bed = r5beda,
    episodic_i = r5imrc,
    episodic_d =  r5dlrc,
    childcont = r5kcnt,
    friendcont = r5fcnt,
    parentcont = r5rcnt,
    socpart = r5socyr,
    numlivehh = h2hhres,
    heart = r5hearte,
    hbp = r5hibpe,
    diab = r5diabe,
    rxmo = r5mo,
    rxyr = r5yr,
    rxdy = r5dy,
    rxdw = r5dw
  ) %>%
   select(idauniqc, r2agey, r3agey, r4agey, r5agey, r6agey, r7agey, r8agey, ragender, raeduc_e, income, bmi, smoke, drink, marital, depressed,
          money, meds, groceries, meal, dress, bath, eat, bed, episodic_i, episodic_d, childcont, friendcont, parentcont, socpart, numlivehh,
         heart, hbp, diab, rxmo, rxyr, rxdy, rxdw,lonelyfirst,lonely1,lonely2,lonely3,lonely4,lonely5,lonely6,lonely7,
         r2complac, r2leftout, r2isolate, r3complac, r3leftout, r3isolate, r4complac, r4leftout, r4isolate, r5complac, r5leftout, r5isolate,
         r6complac, r6leftout, r6isolate, r7complac, r7leftout, r7isolate, r8complac, r8leftout, r8isolate)

dataset.w5 <- dataset.w5 %>%
  dplyr::rename(
    income = r6itearn,
    bmi = r6bmi,
    smoke = r6smoken,
    drink = r6drink,
    marital = r6mstat,
    depressed = r6depres,
    # effort = r6effort,
    # sleep = r6sleepr,
    # happy = r6whappy,
    money = r6moneya,
    meds = r6medsa,
    groceries = r6shopa,
    meal = r6mealsa,
    dress = r6dressa,
    bath = r6batha,
    eat = r6eata,
    bed = r6beda,
    episodic_i = r6imrc,
    episodic_d =  r6dlrc,
    childcont = r6kcnt,
    friendcont = r6fcnt,
    parentcont = r6rcnt,
    socpart = r6socyr,
    numlivehh = h2hhres,
    heart = r6hearte,
    hbp = r6hibpe,
    diab = r6diabe,
    rxmo = r6mo,
    rxyr = r6yr,
    rxdy = r6dy,
    rxdw = r6dw
  ) %>%
  select(idauniqc, r2agey, r3agey, r4agey, r5agey, r6agey, r7agey, r8agey, ragender, raeduc_e, income, bmi, smoke, drink, marital, depressed, 
         money, meds, groceries, meal, dress, bath, eat, bed, episodic_i, episodic_d, childcont, friendcont, parentcont, socpart, numlivehh,
         heart, hbp, diab, rxmo, rxyr, rxdy, rxdw,lonelyfirst,lonely1,lonely2,lonely3,lonely4,lonely5,lonely6,lonely7,
         r2complac, r2leftout, r2isolate, r3complac, r3leftout, r3isolate, r4complac, r4leftout, r4isolate, r5complac, r5leftout, r5isolate,
         r6complac, r6leftout, r6isolate, r7complac, r7leftout, r7isolate, r8complac, r8leftout, r8isolate)

dataset.w6 <- dataset.w6 %>%
  dplyr::rename(
    income = r7itearn,
    bmi = r8bmi, #no r6 bmi, so take following year
    smoke = r7smoken,
    drink = r7drink,
    marital = r7mstat,
    depressed = r7depres,
    # effort = r7effort,
    # sleep = r7sleepr,
    # happy = r7whappy,
    money = r7moneya,
    meds = r7medsa,
    groceries = r7shopa,
    meal = r7mealsa,
    dress = r7dressa,
    bath = r7batha,
    eat = r7eata,
    bed = r7beda,
    episodic_i = r7imrc,
    episodic_d =  r7dlrc,
    childcont = r7kcnt,
    friendcont = r7fcnt,
    parentcont = r7rcnt,
    socpart = r7socyr,
    numlivehh = h2hhres,
    heart = r7hearte,
    hbp = r7hibpe,
    diab = r7diabe,
    rxmo = r7mo,
    rxyr = r7yr,
    rxdy = r7dy,
    rxdw = r7dw
  ) %>%
  select(idauniqc, r2agey, r3agey, r4agey, r5agey, r6agey, r7agey, r8agey, ragender, raeduc_e, income, bmi, smoke, drink, marital, depressed,
         money, meds, groceries, meal, dress, bath, eat, bed, episodic_i, episodic_d, childcont, friendcont, parentcont, socpart, numlivehh,
         heart, hbp, diab, rxmo, rxyr, rxdy, rxdw,lonelyfirst,lonely1,lonely2,lonely3,lonely4,lonely5,lonely6,lonely7,
         r2complac, r2leftout, r2isolate, r3complac, r3leftout, r3isolate, r4complac, r4leftout, r4isolate, r5complac, r5leftout, r5isolate,
         r6complac, r6leftout, r6isolate, r7complac, r7leftout, r7isolate, r8complac, r8leftout, r8isolate)

dataset.w7 <- dataset.w7 %>%
  dplyr::rename(
    income = r8itearn,
    bmi = r8bmi,
    smoke = r8smoken,
    drink = r8drink,
    marital = r8mstat,
    depressed = r8depres,
    # effort = r8effort,
    # sleep = r8sleepr,
    # happy = r8whappy,
    money = r8moneya,
    meds = r8medsa,
    groceries = r8shopa,
    meal = r8mealsa,
    dress = r8dressa,
    bath = r8batha,
    eat = r8eata,
    bed = r8beda,
    episodic_i = r8imrc,
    episodic_d =  r8dlrc,
    childcont = r8kcnt,
    friendcont = r8fcnt,
    parentcont = r8rcnt,
    socpart = r8socyr,
    numlivehh = h2hhres,
    heart = r8hearte,
    hbp = r8hibpe,
    diab = r8diabe,
    rxmo = r8mo,
    rxyr = r8yr,
    rxdy = r8dy,
    rxdw = r8dw
  ) %>%
  select(idauniqc, r2agey, r3agey, r4agey, r5agey, r6agey, r7agey, r8agey, ragender, raeduc_e, income, bmi, smoke, drink, marital, depressed, 
         money, meds, groceries, meal, dress, bath, eat, bed, episodic_i, episodic_d, childcont, friendcont, parentcont, socpart, numlivehh,
         heart, hbp, diab, rxmo, rxyr, rxdy, rxdw,lonelyfirst,lonely1,lonely2,lonely3,lonely4,lonely5,lonely6,lonely7,
         r2complac, r2leftout, r2isolate, r3complac, r3leftout, r3isolate, r4complac, r4leftout, r4isolate, r5complac, r5leftout, r5isolate,
         r6complac, r6leftout, r6isolate, r7complac, r7leftout, r7isolate, r8complac, r8leftout, r8isolate)

dataset <- rbind(dataset.w1, dataset.w2, dataset.w3, dataset.w4, dataset.w5, dataset.w6, dataset.w7)
dataset <- dataset %>%
  dplyr::rename(
      id = idauniqc,
      age1 = r2agey,
      age2 = r3agey,
      age3 = r4agey,
      age4 = r5agey,
      age5 = r6agey,
      age6 = r7agey,
      age7 = r8agey,
      sex = ragender,
      educ = raeduc_e)


#create constructed variables
#baseline isolation (code such that isolated = 1, not isolated = 0)
dataset <- dataset %>% 
  mutate(
    childcont=recode(childcont, 
                     ".k:no Kid" = 1,
                     "0.no" = 1, 
                     "1.yes" = 0),
    friendcont=recode(friendcont, 
                     ".n:Not Applicable" = 1,
                     "0.no" = 1, 
                     "1.yes" = 0),
    parentcont=recode(parentcont, 
                      ".n:Not Applicable" = 1,
                      "0.no" = 1, 
                      "1.yes" = 0),
    socpart=recode(socpart, 
                   "0.no" = 1, 
                   "1.yes" = 0),
    livealone=recode(numlivehh, 
                     '1' = 1, 
                     '2' = 0,
                     '3' = 0,
                     '4' = 0,
                     '5' = 0,
                     '6' = 0,
                     '7' = 0,
                     '8' = 0,
                     '9' = 0,
                     '10' = 0,
                     '11' = 0)) %>% 
  mutate(b.isolated = rowMeans(cbind(childcont, livealone, parentcont, socpart), na.rm=T))  %>%  #use childcont, parentcont, socpart, livealone
  mutate(b.isolated = as.numeric(scale(b.isolated)))




#marital status
dataset <- dataset %>% 
  mutate(
    marital=recode(marital, 
                   "1.married" = "A",
                   "3.partnered" = "A", 
                   "4.separated" = "B",
                   "5.divorced" = "B",
                   "7.widowed" = "C",
                   "8.never married" = "D",
                   ".d:DK" = "",
                   ".r:Refuse" = ""
                   ),
    
    married = ifelse(marital == "A", 1,0),
    divorced = ifelse(marital == "B", 1,0),
    widowed = ifelse(marital == "C", 1,0),
    never.married = ifelse(marital == "D", 1,0))
dataset$marital[dataset$marital==""] <- NA
#gender
dataset <- dataset %>%
  mutate(
    sex=recode(sex,
               "1.male" = 0,
               "2.female" = 1)
  )

#recode smoking status and drinking status
dataset <- dataset %>%
  mutate(
    smoke=recode(smoke,
                 "0.No" = 0,
                 "1.Yes" = 1),
    drink=recode(drink,
                 "0.no" = 0,
                 "1.yes" = 1)
  )


#recode education
dataset <- dataset %>%
  mutate(
    educ=recode(educ,
               "1.lt high-school" = 1,
               "3.high-school graduate" = 3,
               "4.some college" = 4,
               "5.college and above" = 5
               ))



#depression
dataset <- dataset %>% 
  mutate(
    depression=recode(depressed, 
                      "0.no" = 0,
                      "1.yes" = 1))

#chronic conditions
dataset <- dataset %>% 
  mutate(
    hbp=recode(hbp, 
               "0.no" = 0,
               "1.yes" = 1),
    heart=recode(heart, 
                 "0.no" = 0,
                 "1.yes" = 1),
    diab=recode(diab, 
                "0.no" = 0,
                "1.yes" = 1)) %>% 
  mutate(cc = as.numeric(rowMeans(cbind(hbp, heart, diab), na.rm=T)*3)) 



#functional status
dataset <- dataset %>%
  mutate(
    money=recode(money, 
                 "0.No" = 0,
                 "1.Yes" = 1),
    meds=recode(meds, 
                "0.No" = 0,
                "1.Yes" = 1),
    groceries=recode(groceries, 
                "0.No" = 0,
                "1.Yes" = 1),
    meal=recode(meal, 
                "0.No" = 0,
                "1.Yes" = 1),
    dress=recode(dress, 
                 "0.No" = 0,
                 "1.Yes" = 1),
    bath=recode(bath, 
                "0.No" = 0,
                "1.Yes" = 1),
    eat=recode(eat, 
               "0.No" = 0,
               "1.Yes" = 1),
    bed=recode(bed, 
               "0.No" = 0,
               "1.Yes" = 1)) %>%   
  mutate(functional = rowMeans(cbind(money,meds,groceries,meal,dress,bath,eat,bed), na.rm=T)*8) %>% 
  mutate(functional = as.numeric(scale(functional)))




#cognitive variables
dataset <- dataset %>%
  mutate(    
    rxmo=recode(rxmo, 
                "0.no" = 0,
                "1.yes" = 1),
    rxyr=recode(rxyr, 
                "0.no" = 0,
                "1.yes" = 1),
    rxdy=recode(rxdy, 
                "0.no" = 0,
                "1.yes" = 1),
    rxdw=recode(rxdw, 
                "0.no" = 0,
                "1.yes" = 1)) %>% 
  mutate(mstat = rowMeans(cbind(rxmo, rxyr, rxdy, rxdw), na.rm=T)) %>%
  mutate(mstat = as.numeric(scale(mstat)))  %>%
  mutate(
    episodic_i=recode(episodic_i, 
                      '1' = 1, 
                      '2' = 2,
                      '3' = 3,
                      '4' = 4,
                      '5' = 5,
                      '6' = 6,
                      '7' = 7,
                      '8' = 8,
                      '9' = 9),
    episodic_d=recode(episodic_d, 
                      '1' = 1, 
                      '2' = 2,
                      '3' = 3,
                      '4' = 4,
                      '5' = 5,
                      '6' = 6,
                      '7' = 7,
                      '8' = 8,
                      '9' = 9)) %>% 
  mutate(episodic = rowMeans(cbind(episodic_i, episodic_d), na.rm=T)) %>%
  mutate(episodic = as.numeric(scale(episodic)))



# Center Age
age.vars<- c("age1", "age2", "age3", "age4", "age5", "age6", "age7")
dataset <- dataset %>% 
  mutate(age1 = as.numeric(as.character(age1)),
         age2 = as.numeric(as.character(age2)),
         age3 = as.numeric(as.character(age3)),
         age4 = as.numeric(as.character(age4)),
         age5 = as.numeric(as.character(age5)),
         age6 = as.numeric(as.character(age6)),
         age7 = as.numeric(as.character(age7)))
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
         income = as.numeric(scale(as.numeric(as.character(income)))),
         bmi = as.numeric(scale(as.numeric(as.character(bmi))))
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
    lonely4cat = ifelse(is.na(lonely4), 0, 1),
    lonely5cat = ifelse(is.na(lonely5), 0, 1),
    lonely6cat = ifelse(is.na(lonely6), 0, 1),
    lonely7cat = ifelse(is.na(lonely7), 0, 1),
    sum = rowSums(cbind(lonely1cat, lonely2cat, lonely3cat, lonely4cat, lonely5cat, lonely6cat, lonely7cat)))  %>%
  filter (sum > 2)


dataset <- dataset %>% 
  select(
    id,
    b.age.yrs,
    age1,
    age2,
    age3,
    age4,
    age5,
    age6,
    age7,
    lonely1,
    lonely2,
    lonely3,
    lonely4,
    lonely5,
    lonely6,
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
    # parentcont,
    # friendcont,
    # socpart,
    # numlivehh,
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

save(dataset, file = "ELSA_clean_r.Rdata")

#source("Lonely Script.R")


## ELSA
### Load Data  
data_path <- "P:/Projects/Personality/Lonliness/All_Data"
wd <- "P:/Projects/Personality/Lonliness/Emorie_Update"
load(sprintf("%s/ELSA_clean_r.RData", data_path))

### Recoding
dataset$SID <- as.character(dataset$id)
dataset$marital <- as.character(dataset$marital)
elsa_comb<-dataset
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
save(elsa_comb, file = "P:/Projects/Personality/Lonliness/Emorie_Update/elsa_clean.Rdata")

load("P:/Projects/Personality/Lonliness/Emorie_Update/01-data/clean/elsa_clean.Rdata")
