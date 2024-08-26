rm(list=ls())

#install and load packages
my_packages <- c("psych", "lme4", "merTools", "tidyverse", "sjstats")

#lapply(my_packages, install.packages)
lapply(my_packages, library, character.only = TRUE)

#This will tell you where everything is saved- please note this location!
setwd("P:\\Projects\\Personality\\Lonliness\\Sites\\SHARE\\Data\\")
getwd()

#read data
#dataset <- read.csv("P:\\Projects\\Personality\\Lonliness\\test_data_HRS.csv")
#dataset <- read.csv("SHARE_clean.csv")
#save(dataset, file = "SHARE_clean.Rdata")
load("P:/Projects/Personality/Lonliness/Sites/SHARE/Data/SHARE_clean.Rdata")

#create meta-data
i = "SHARE" #study name
MO = "4" #number of measurement occasions
year = "2010" #baseline year
interval = "2" #average years between measurement occasions
followup = "6" #total years from baseline to final MO
continent = "Europe"
lonely = "UCLA" #name of loneliness item, scale
age = "Check with real data" #average age at baseline


#Data wrangle:
#rename variables
dataset <- dataset %>%
  dplyr::rename(id = nmergeid,
                age1 = r4agey,
                age2 = r5agey,
                age3 = r6agey,
                age4 = r7agey,
                sex = ragender,
                educ = raedyrs,
                income = r4itearn,
                bmi = r4bmi,
                smoke = r4smoken,
                drink = r4drink,
                marital = r4mstat,
                depressed = r2depress,
                effort = r2effort,
                sleep = r2sleep,
                happy = r2whappy,
                money = r4moneya,
                meds = r4medsa,
                groceries = r4shopa,
                meal = r4mealsa,
                dress = r4dressa,
                bath = r4batha,
                eat = r4eata,
                bed = r4beda,
                episodic_i = r4imrc,
                episodic_d = r4dlrc,
                childcont = h4kcnt,
                parentcont = r4pcnt,
                socpart = r4socyr,
                numlivehh = hh4hhres,
                heart = r4hearte,
                hbp = r4hibpe,
                diab = r4diabe
  )
                

#create constructed variables
#Loneliness (high score = high loneliness)
dataset <- dataset %>% 
  mutate(
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
                    "3.often" = 3)) 

dataset <- dataset %>%
  dplyr::mutate(lonely1 = rowMeans(select(., c("r4complac", "r4leftout", "r4isolate")),na.rm = TRUE)*3,   
                lonely2 = rowMeans(select(., c("r5complac", "r5leftout", "r5isolate")),na.rm = TRUE)*3,
                lonely3 = rowMeans(select(., c("r6complac", "r6leftout", "r6isolate")),na.rm = TRUE)*3,
                lonely4 = rowMeans(select(., c("r7complac", "r7leftout", "r7isolate")),na.rm = TRUE)*3
  )



#baseline isolation (code such that isolated = 1, not isolated = 0)
#table(dataset$childcont) #.k:no kids    0.no weekly contact    1.weekly contact 
#table(dataset$parentcont) #.n:no living parents    0.no weekly contact          1.weekly contact 
#table(dataset$socpart) #0.no yearly activities    1.yearly activities 
#table(dataset$numlivehh) # 1-12

dataset <- dataset %>% 
  mutate(
    childcont=recode(childcont, 
                     ".k:no kids" = 1,
                     "0.no weekly contact" = 1, 
                     "1.weekly contact" = 0),
    parentcont=recode(parentcont, 
                     ".n:no living parents" = 1,
                     "0.no weekly contact" = 1, 
                     "1.weekly contact" = 0),
    socpart=recode(socpart, 
                     "0.no yearly activities" = 1, 
                     "1.yearly activities" = 0),
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
                   '11' = 0,
                   '12' = 0)) %>% 
  mutate(b.isolated = rowMeans(cbind(childcont, livealone, parentcont, socpart), na.rm=T))  %>%   #use childcont, parentcont, socpart, livealone
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
                     ".m:Missing" = "",
                     ".r:Refuse" = ""),
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


#depression
dataset <- dataset %>% 
  mutate(
    depressed=recode(depressed, 
                     "0.no" = 0,
                     "1.yes" = 1),
    effort=recode(effort, 
                     "0.no" = 0,
                     "1.yes" = 1),
    sleep=recode(sleep, 
                     "0.no" = 0,
                     "1.yes" = 1),
    unhappy=recode(happy, 
                     "0.no" = 1,
                     "1.yes" = 0)) %>% 
  mutate(depression = rowMeans(cbind(depressed, effort, sleep, unhappy), na.rm=T)) %>% 
  mutate(depression = as.numeric(scale(depression)))




#chronic conditions (cc)
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
           r4mo=recode(r4mo, 
                        "0.incorrect" = 0,
                        "1.correct" = 1),
           r4yr=recode(r4yr, 
                       "0.incorrect" = 0,
                       "1.correct" = 1),
           r4dy=recode(r4dy, 
                      "0.incorrect" = 0,
                      "1.correct" = 1),
           r4dw=recode(r4dw, 
                      "0.incorrect" = 0,
                      "1.correct" = 1)) %>% 
  mutate(mstat = rowMeans(cbind(r4mo, r4yr, r4dy, r4dw), na.rm=T)) %>%
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
                   '9' = 9,
                   '10' = 10,
                   '11' = 11,
                   '12' = 12),
      episodic_d=recode(episodic_d, 
                        '1' = 1, 
                        '2' = 2,
                        '3' = 3,
                        '4' = 4,
                        '5' = 5,
                        '6' = 6,
                        '7' = 7,
                        '8' = 8,
                        '9' = 9,
                        '10' = 10,
                        '11' = 11,
                        '12' = 12)) %>% 
  mutate(episodic = rowMeans(cbind(episodic_i, episodic_d), na.rm=T)) %>%
  mutate(episodic = as.numeric(scale(episodic)))

# Center Age
age.vars<- c("age1", "age2", "age3", "age4")
dataset <- dataset %>% 
  mutate(age1 = as.numeric(as.character(age1)),
         age2 = as.numeric(as.character(age2)),
         age3 = as.numeric(as.character(age3)),
         age4 = as.numeric(as.character(age4)))
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
  mutate(b.age = apply(dataset[,c("age1", "age2", "age3", "age4")], 1, min, na.rm = TRUE),
         b.age.yrs = apply(dataset[,c("age1_raw", "age2_raw", "age3_raw", "age4_raw")], 1, min, na.rm = TRUE),
         educ = as.numeric(scale(educ)),
         income = as.numeric(scale(income)),
         bmi = as.numeric(scale(as.numeric(as.character(bmi))))
  )

#update b.age.yrs when all are missing
is.na(dataset$b.age) <- sapply(dataset$b.age, is.infinite)
is.na(dataset$b.age.yrs) <- sapply(dataset$b.age.yrs, is.infinite)

##If no "baseline" record, drop participant (i.e., if no "r2" timepoint, do not use this participant)
dataset <- dataset %>% drop_na(lonely1)


dataset <- dataset %>% 
  select(
  id,
  b.age.yrs,
  age1,
  age2,
  age3,
  age4,
  lonely1,
  lonely2,
  lonely3,
  lonely4,
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

save(dataset, file = "SHARE_clean_r.Rdata")

#source("Lonely Script.R")




### Load Data  
data_path <- "P:/Projects/Personality/Lonliness/All_Data"
wd <- "P:/Projects/Personality/Lonliness/Emorie_Update"
load(sprintf("%s/SHARE_clean_r.Rdata", data_path))


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
save(share, file = "P:/Projects/Personality/Lonliness/Emorie_Update/share_clean.Rdata")

load("P:/Projects/Personality/Lonliness/Emorie_Update/01-data/clean/share_clean.Rdata")

