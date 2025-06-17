library(tidyverse)
library(readxl)
library(plotly)
library(ggthemes)
library(rmarkdown)
library(flexdashboard)



# Importing the data
crs.d.sheets<-excel_sheets( # Death template 2023 excel sheets
  "DEATH TEMPLATE FROM JAN 2023.xlsx")


### January 2023 returns
crs.d.jan<-data.frame(read_excel(
  "DEATH TEMPLATE FROM JAN 2023.xlsx",
  sheet = crs.d.sheets[1], col_names = TRUE)
)

glimpse(crs.d.jan) # 238 cases vs 14 columns

crs.d.jan[duplicated(crs.d.jan), ] # No duplicates 
# in Jan 23 returns

crs.d.jan[!complete.cases(crs.d.jan),] # No 
# missing values

# Columns of interest
crs.d.jan<-crs.d.jan[,c(3,4,5,6,8,9,11)]

# Removing the NA rows
crs.d.jan<-crs.d.jan[c(-36,-60),]

# crs.d.jan$id<-as.integer(seq(from=1, to=,236 ,by=1))

# Renaming of columns
crs.d.jan<-
  crs.d.jan %>%
  rename(dod=Date.of.Death...dd.mm.yy.,
         sex=Sex...Indicate.whether.male.or.female.,
         age=Age....Indicate.number.eg.1.2.3.,
         age.unit=Unit.of.age....Days..Months.or.Years.,
         pod=Place.of.death....Indicate.the.name.of.exact.place.of.death.,
         place.type=Type.of.place...Indicate.whether.Home.or.Health.Institution.,
         cod=Cause.of.death....Indicate.the.underlying.cause.of.death.,
         )

# Column inspection
glimpse(crs.d.jan$dod) # date column: yyyy-mm-dd
glimpse(crs.d.jan$sex);unique(crs.d.jan$sex) # 4 levels:
# male,Male, female,Female, NA
glimpse(crs.d.jan$age); unique(crs.d.jan$age) # NA, Not Stated
glimpse(crs.d.jan$age.unit); unique(crs.d.jan$age.unit) 
# 7 levels: years,Years, Days,NA, Months, Not Stated,not stated
glimpse(crs.d.jan$pod); unique(crs.d.jan$pod)# 53 levels
glimpse(crs.d.jan$place.type);unique(crs.d.jan$place.type)# 2 levels
                                                          # Health Facility ,Home, NA
glimpse(crs.d.jan$cod);unique(crs.d.jan$cod)# 45 levels



# Column type convertion
crs.d.jan$sex<-factor(crs.d.jan$sex)
crs.d.jan$pod<-factor(crs.d.jan$pod)
crs.d.jan$place.type<-factor(crs.d.jan$place.type)
crs.d.jan$cod<-factor(crs.d.jan$cod)
crs.d.jan$age.unit<-factor(crs.d.jan$age.unit)


# Factor restructuring
crs.d.jan<-crs.d.jan %>% 
  mutate(sex=fct_recode(sex, "male"="Male",
                        "female"="Female"),
         age.unit=fct_recode(age.unit, "years"="Years",
                             "days"="Days","months"="Months",
                             "not stated"="Not Stated"),
         pod=fct_recode(pod, "West Reru"="west reru",
                        "West Kadinga"="west kadinga",
                        "West Kanyadwera"="west Kanyadwera",
                        "North Alungo"="north alungo",
                        "East Kanyadwera"="east kanyadwea",
                        "East Kanyadwera"="east kanyadwera",
                        "East Kadinga"="east kadinga",
                        "North Ratta"="north ratta","Newa"="newa",
                        "East Karateng"="east karateng",
                        "East Karateng"="East karateng",
                        "Korando B"="korando B",
                        "Kaila"="kaila", "Ang'oga"="Angoga",
            "chulaimbo county hospital"="Chulaimbo Hospital",
            "kombewa county hospital"="Kombewa County Hospital",
            "manyuanda sc hospital"="manyuanda hospital",
            "manyuanda sc hospital"="manyuanda s.c.hosp",
            "manyuanda sc hospital"="manyuanda hosp",
            "maseno mission hospital"="Maseno mission hospital",
            "port florence hospital"="Port Florence Hospital",
            "st jairus hospital"="St Jairus Hospital"),
         cod=fct_recode(cod, "Anaemia"="anaemia","Arthritis"="arthritis",
                        "Asthma"="asthma","Malaria"="malaria",
                    "Pneumonia"="Pneomonia","Pneumonia"="pneumonia","Tuberculosis"="tuberculosis",
                "Sudden Death"="Sudden death","Sudden Death"="sudden death",
                "Unspecified Cancer"="Unspecified cancer",
      "Birth Asphyxia And Birth Trauma"="Birth asphyxia and birth trauma",
      "Cervix Uteri Cancer"="Cervix uteri cancer",
      "Diabetes Mellitus"="Diabetes mellitus",
      "Hypertensive Disease"="Hypertensive disease",
      "Oesophageal Atresia"="Oesophageal atresia",
      "Oesophagus Cancer"="Oesophagus cancer",
      "Old Age"="old age","Prostate Cancer"="Prostate cancer",
      "Respiratory Infections"="Respiratory infections",
      "Tetanus"="tetanus","Urinary Obstruction"="urinay obstruction",
      "Wild Animal Attack"="Wild animal attack"))


levels(crs.d.jan$sex) # 2 levels
levels(crs.d.jan$age.unit) # 4 levels
levels(crs.d.jan$pod) # 44 places
levels(crs.d.jan$cod) # 35 causes


### February 2023 returns
crs.d.feb<-data.frame(read_excel(
  "DEATH TEMPLATE FROM JAN 2023.xlsx",
  sheet = crs.d.sheets[2], col_names = TRUE)
)

glimpse(crs.d.feb) # 120 cases vs 14 columns

crs.d.feb[duplicated(crs.d.feb), ] # No duplicates 
# in Feb 23 returns

crs.d.feb[!complete.cases(crs.d.feb),] # 34 rows of NAs

# Columns of interest
crs.d.feb<-crs.d.feb[,c(3,4,5,6,8,9,11)]

# Removing the NA rows
crs.d.feb<-crs.d.feb[-34,]

# crs.d.feb$id<-as.integer(seq(from=237, to=355 ,by=1))


# Renaming of columns
crs.d.feb<-
  crs.d.feb %>%
  rename(dod=Date.of.Death...dd.mm.yy.,
         sex=Sex...Indicate.whether.male.or.female.,
         age=Age....Indicate.number.eg.1.2.3.,
         age.unit=Unit.of.age....Days..Months.or.Years.,
         pod=Place.of.death....Indicate.the.name.of.exact.place.of.death.,
         place.type=Type.of.place...Indicate.whether.Home.or.Health.Institution.,
         cod=Cause.of.death....Indicate.the.underlying.cause.of.death.,
  )

# Column inspection
glimpse(crs.d.feb$dod) # date column: yyyy-mm-dd
glimpse(crs.d.feb$sex);unique(crs.d.feb$sex) # 2 levels: Male, Female
glimpse(crs.d.feb$age); unique(crs.d.feb$age)
glimpse(crs.d.feb$age.unit); unique(crs.d.feb$age.unit) # 2 levels: Years, Months
glimpse(crs.d.feb$pod); unique(crs.d.feb$pod)# 36 levels
glimpse(crs.d.feb$place.type);unique(crs.d.feb$place.type)# 2 levels: Health Facility ,Home
glimpse(crs.d.feb$cod);unique(crs.d.feb$cod)# 36 levels



# Column type convertion
crs.d.feb$sex<-factor(crs.d.feb$sex)
crs.d.feb$pod<-factor(crs.d.feb$pod)
crs.d.feb$place.type<-factor(crs.d.feb$place.type)
crs.d.feb$cod<-factor(crs.d.feb$cod)
crs.d.feb$age.unit<-factor(crs.d.feb$age.unit)


# Factor restructuring
crs.d.feb<-crs.d.feb %>% 
  mutate(sex=fct_recode(sex, "male"="Male",
                           "female"="Female"),
         age.unit=fct_recode(age.unit, "years"="Years",
                                      "months"="Months"),
         pod=fct_recode(pod, "Ang'oga"="Angoga",
                             "kombewa county hospital"="Kombewa Hospital",
                             "kombewa county hospital"="Kombewa County Hospital",
                             "port florence hospital"="Port Florence Hospital",
                        "port florence hospital"="Port Florence hospital",
                        "nyahera sc hospital"="Nyahera S C Hospital",
                        "maseno mission hospital"="Maseno Mission Hospital",
                        "masaba hospital limited"="Masaba Hospital Limited"),
         cod=fct_recode(cod, "Cerebrovascular Disease"="Cerebrovascular disease",
                        "Diabetes Mellitus"="Diabetes mellitus",
                        "Diarrhoeal Diseases"="Diarrhoeal diseases",
                        "Drowning"="drowning","Epilepsy"="epilepsy",
                        "Hypertensive Disease"="Hypertensive disease",
                        "Liver Cancer"="Liver cancer","Malaria"="malaria",
                        "Mob Justice"="mob justice","Liver Cirrhosis"="liver Cirrhosis",
                        "Mouth And Oropharynx Cancers"="Mouth and oropharynx cancers",
                        "Other Causes"="Other causes","Respiratory Infections"="Respiratory infections",
                        "Road Traffic Accidents"="road traffic accidents",
                        "Unspecified Cancer"="Unspecified cancer",
                        "Urinay Obstruction"="urinay obstruction",
                        "Wild Animal Attack"="Wild animal attack"))


levels(crs.d.feb$sex) # 2 levels
levels(crs.d.feb$age.unit) # 2 levels
levels(crs.d.feb$pod) # 34 places
levels(crs.d.feb$cod) # 33 causes



### March 2023 returns
crs.d.mar<-data.frame(read_excel(
  "DEATH TEMPLATE FROM JAN 2023.xlsx",
  sheet = crs.d.sheets[3], col_names = TRUE)
)

glimpse(crs.d.mar) # 145 cases vs 16 columns

crs.d.mar[duplicated(crs.d.mar), ] # No duplicates 
# in Mar 23 returns

crs.d.mar[!complete.cases(crs.d.mar),] # 145 rows of NAs

# Columns of interest
crs.d.mar<-crs.d.mar[,c(3,4,5,6,8,9,11)]

# Removing the NA rows
crs.d.mar<-crs.d.mar[-39,]

# crs.d.mar$id<-as.integer(seq(from=356, to=499, by=1))

# Renaming of columns
crs.d.mar<-
  crs.d.mar %>%
  rename(dod=Date.of.Death...dd.mm.yy.,
         sex=Sex...Indicate.whether.male.or.female.,
         age=Age....Indicate.number.eg.1.2.3.,
         age.unit=Unit.of.age....Days..Months.or.Years.,
         pod=Place.of.death....Indicate.the.name.of.exact.place.of.death.,
         place.type=Type.of.place...Indicate.whether.Home.or.Health.Institution.,
         cod=Cause.of.death....Indicate.the.underlying.cause.of.death.,
  )

# Column inspection
glimpse(crs.d.mar$dod) # date column: yyyy-mm-dd
glimpse(crs.d.mar$sex);unique(crs.d.mar$sex) # 2 levels: Male, Female
glimpse(crs.d.mar$age); unique(crs.d.mar$age)
glimpse(crs.d.mar$age.unit); unique(crs.d.mar$age.unit) # 5 levels: years,Years, months,Months, NA
glimpse(crs.d.mar$pod); unique(crs.d.mar$pod)# 54 levels
glimpse(crs.d.mar$place.type);unique(crs.d.mar$place.type)# 4 levels: Health Facility,Health facility ,Home,home
glimpse(crs.d.mar$cod);unique(crs.d.mar$cod)# 37 levels



# Column type convertion
crs.d.mar$sex<-factor(crs.d.mar$sex)
crs.d.mar$pod<-factor(crs.d.mar$pod)
crs.d.mar$place.type<-factor(crs.d.mar$place.type)
crs.d.mar$cod<-factor(crs.d.mar$cod)
crs.d.mar$age.unit<-factor(crs.d.mar$age.unit)


# Factor restructuring
crs.d.mar<-crs.d.mar %>% 
  mutate(sex=fct_recode(sex,"female"="Female"),
         age.unit=fct_recode(age.unit, "years"="Years",
                             "months"="Months"),
         place.type=fct_recode(place.type, "Health Facility"="Health facility","Home"="home"),
         pod=fct_recode(pod, "Kisian"="kisian",
                        "South Kapuonja"="south kapuonja",
                        "Upper Kombewa"="upper kombewa",
                        "North Ratta"="north ratta","East Ngere"="East ngere",
                        "Kitmikayi"="kitmikayi","East Othany"="East othany",
                        "Kaila"="kaila","East Karateng"="east karateng",
                        "Upper Kadongo"="upper kadongo","Lower Kadongo"="lower kadongo",
                        "Nyahera"="nyahera","Marera"="marera","East Kadinga"="east kadinga",
                        "Dago"="dago","Lower Osiri"="lower osiri","North Kapuonja"="north kapuonja",
                        "East Kolunje"="east kolunje","Ang'oga"="angoga","West Karateng"="west karateng",
                        "Kajulu Koker"="kajulu koker","masaba hospital limited"="masaba hospital",
                        "Korando A"="korando A","maseno mission hospital"="Maseno mission hospital",
                        "manyuanda sc hospital"="Manyuanda hospital",
                        "kombewa county hospital"="Kombewa county hosp",
                        "st jairus hospital"="St jairus hospital",
                        "West Karateng"="West karateng","Ang'oga"="Angoga",
                        "East Kadinga"="East kadinga","South Kapuonja"="South kapuonja",
                        "West Kadinga"="west kadinga","South Alungo"="South alungo",
                        "East Kanyadwera"="East kanyadwera","Lower Kombewa"="Lower kombewa",
                        "West Kanyadwera"="West kanyadwera","Upper Kombewa"="Upper kombewa",
                        "West Othany"="West othany","West Ngere"="West ngere","West Reru"="West reru",
                        "East Reru"="East reru","Upper Kanyawegi"="Upper kanyawegi",
                        "Upper Osiri"="Upper osiri","Lower Kadongo"="Lower kadongo","Lower Osiri"="Lower osiri",
                        "Kajulu Koker"="Kajulu koker"),
         cod=fct_recode(cod, "Alcoholism"="alcoholism","Anaemia"="anaemia","Arthritis"="arthritis",
                        "Asthma"="asthma","Cancer"="cancer","Cervix Uteri Cancer"="Cervix uteri cancer",
                        "Cholera"="cholera","Diabetes Mellitus"="diabetes mellitus","Diarrhoeal Diseases"="Diarrhoeal diseases",
                        "Electrocution_lightning"="Electrocution/ lightning","Head Injury"="head injury",
                        "HIV"="hiv","Liver Cirrhosis"="liver Cirrhosis","Malaria"="malaria","Pneumonia"="pneumonia",
                        "Meningitis"="meningitis","Oesophageal Atresia"="Oesophageal atresia","Oesophagus cancer"="Oesophagus Cancer",
                        "Old Age"="old age","Peuperal Sepsis"="peuperal sepsis","Pneumonia"="pneumonia","Prostate Cancer"="Prostate cancer",
                        "Road Traffic Accidents"="road traffic accidents","Road Traffic Accidents"="Road traffic accidents",
                        "Stroke"="stroke","Sudden Death"="sudden death","Tuberculosis"="tuberculosis","Ulcers"="ulcers","Urinay Obstruction"="urinay obstruction"
                        ))


levels(crs.d.mar$sex) # 2 levels
levels(crs.d.mar$age.unit) # 2 levels
levels(crs.d.mar$pod) # 42 places
levels(crs.d.mar$cod) # 33 causes


### April 2023
crs.d.apr<-data.frame(read_excel(
  "DEATH TEMPLATE FROM JAN 2023.xlsx",
  sheet = crs.d.sheets[4], col_names = TRUE)
)

# glimpse(crs.d.apr) # 117 cases vs 16 columns

# crs.d.apr[duplicated(crs.d.apr), ] # No duplicates 
# in Apr 23 returns

# crs.d.apr[!complete.cases(crs.d.apr),] # 117 rows of NAs

# Columns of interest
crs.d.apr<-crs.d.apr[,c(3,4,5,6,8,9,11)]

# Removing the NA rows
crs.d.apr<-crs.d.apr[-34,]

# crs.d.apr$id<-as.integer(seq(from=500, to=616, by=1))

# Renaming of columns
crs.d.apr<-
  crs.d.apr %>%
  rename(dod=Date.of.Death...dd.mm.yy.,
         sex=Sex...Indicate.whether.male.or.female.,
         age=Age....Indicate.number.eg.1.2.3.,
         age.unit=Unit.of.age....Days..Months.or.Years.,
         pod=Place.of.death....Indicate.the.name.of.exact.place.of.death.,
         place.type=Type.of.place...Indicate.whether.Home.or.Health.Institution.,
         cod=Cause.of.death....Indicate.the.underlying.cause.of.death.,
  )

# Column type convertion
crs.d.apr$sex<-factor(crs.d.apr$sex)
crs.d.apr$pod<-factor(crs.d.apr$pod)
crs.d.apr$place.type<-factor(crs.d.apr$place.type)
crs.d.apr$cod<-factor(crs.d.apr$cod)
crs.d.apr$age.unit<-factor(crs.d.apr$age.unit)


# Factor restructuring
crs.d.apr<-crs.d.apr %>% 
  mutate(sex=fct_recode(sex,"female"="Female","male"="Male"),
         age.unit=fct_recode(age.unit, "years"="Years",
                             "months"="Months", "NA"="Not Stated"),
         pod=fct_recode(pod, "manyuanda sc hospital"="Manyuanda S C Hospital",
                        "port florence hospital"="Port Florence Hospital",
                        "kombewa county hospital"="Kombewa County Hospital",
                        "masaba hospital limited"="Masaba Hospital Limited",
                        "Ang'oga"="Angoga"),
         cod=fct_recode(cod, "Urinay Obstruction"="urinay obstruction",
                        "Inflammatory Heart Diseases"="Inflammatory heart diseases",
                        "Cervix Uteri Cancer"="Cervix uteri cancer",
                        "Prematurity And Low Birth Weight"="Prematurity and low birth weight",
                        "Hypertensive Disease"="Hypertensive  disease",
                        "Maternal Sepsis"="Maternal sepsis","Pneumonia"="pneumonia",
                        "Respiratory Infections"="Respiratory infections",
                        "Other Causes"="Other causes","Diabetes Mellitus"="Diabetes mellitus")
  )

levels(crs.d.apr$cod) # 31 levels

### May 2023 returns
crs.d.may<-data.frame(read_excel(
  "DEATH TEMPLATE FROM JAN 2023.xlsx",
  sheet = crs.d.sheets[5], col_names = TRUE)
)

glimpse(crs.d.may) # 107 cases vs 16 columns

crs.d.may[duplicated(crs.d.may), ] # No duplicates 
# in May 23 returns

crs.d.may[!complete.cases(crs.d.may),] # 107 rows of NAs

# Columns of interest
crs.d.may<-crs.d.may[,c(3,4,5,6,8,9,11)]

# Removing the NA rows
crs.d.may<-crs.d.may[-23,]

# crs.d.may$id<-as.integer(seq(from=616, to=722, by=1))

# Renaming of columns
crs.d.may<-
  crs.d.may %>%
  rename(dod=Date.of.Death...dd.mm.yy.,
         sex=Sex...Indicate.whether.male.or.female.,
         age=Age....Indicate.number.eg.1.2.3.,
         age.unit=Unit.of.age....Days..Months.or.Years.,
         pod=Place.of.death....Indicate.the.name.of.exact.place.of.death.,
         place.type=Type.of.place...Indicate.whether.Home.or.Health.Institution.,
         cod=Cause.of.death....Indicate.the.underlying.cause.of.death.,
  )

# Column inspection
glimpse(crs.d.may$dod) # date column: yyyy-mm-dd
glimpse(crs.d.may$sex);unique(crs.d.may$sex) # 3 levels: male,Male, Female
glimpse(crs.d.may$age); unique(crs.d.may$age)# NA, n/a
glimpse(crs.d.may$age.unit); unique(crs.d.may$age.unit) # 3 levels: years,Years, NA, Months
glimpse(crs.d.may$pod); unique(crs.d.may$pod)# 33 levels
glimpse(crs.d.may$place.type);unique(crs.d.may$place.type)# 4 levels: Health Facility,Home,home, NA
glimpse(crs.d.may$cod);unique(crs.d.may$cod)# 29 levels



# Column type convertion
crs.d.may$sex<-factor(crs.d.may$sex)
crs.d.may$pod<-factor(crs.d.may$pod)
crs.d.may$place.type<-factor(crs.d.may$place.type)
crs.d.may$cod<-factor(crs.d.may$cod)
crs.d.may$age.unit<-factor(crs.d.may$age.unit)


# Factor restructuring
crs.d.may<-crs.d.may %>% 
  mutate(sex=fct_recode(sex,"male"="Male"),
         age.unit=fct_recode(age.unit, "years"="Years"),
         place.type=fct_recode(place.type, "Home"="home"),
         pod=fct_recode(pod, "Upper Kadongo"="upper kadongo","Ang'oga"="Angoga","Sunga"="Sunga ",
                        "West Katieno"="west katieno","Upper Kanyawegi"="upper kanyawegi",
                        "Ang'oga"="angoga","Kajulu Koker"="kajulu koker","South Ratta"="south ratta",
                        "Nyahera"="nyahera","East Kanyadwera"="East kanyadwera","Newa"="newa",
                        "East Othany"="East othany","West Karateng"="West karateng","Ojola"="ojolla",
                        "Kanyawegi"="kanyawegi","Kaila"="kaila","North Ratta"="north ratta",
                        "Lower Osiri"="lower osiri","Marera"="marera","st jairus hospital"="St jairus hospital",
                        "masaba hospital limited"="Masaba hospital","East Reru"="East reru",
                        "North Ratta"="North ratta","East Karateng"="East karateng",
                        "South Ratta"="South ratta","West Karateng"="west karateng","Maseno Township"="maseno township",
                        "Upper Osiri"="upper osiri","West Reru"="west reru","Sunga "="sunga",
                        "West Ngere"="west ngere"),
         cod=fct_recode(cod, "Sudden Death"="sudden death","Malaria"="malaria",
                        "Cancer"="cancer","Unspecified Cancer"="Unspecified cancer",
                        "Tuberculosis"="tuberculosis","Alcoholism"="alcoholism","Pneumonia"="pneumonia",
                        "Septicaemia"="septicaemia","Stroke"="stroke","Prostate Cancer"="Prostate cancer",
                        "Oesophagus Cancer"="Oesophagus cancer","Diabetes Mellitus"="Diabetes mellitus",
                        "Anaemia"="anaemia","HIV"="hiv","Candidiasis"="candidiasis",
                        "Arthritis"="arthritis","Asthma"="asthma","Tetanus"="tetanus",
                        "Mob Justice"="mob justice","Stomach Cancer"="Stomach cancer",
                        "Measles"="measles","Drowning"="drowning","Old Age"="old age")
  )

levels(crs.d.may$sex) # 2 levels
levels(crs.d.may$age.unit) # 3 levels
levels(crs.d.may$pod) # 30 places
levels(crs.d.may$cod) # 29 causes



### June 2023 returns
crs.d.jun<-data.frame(read_excel(
  "DEATH TEMPLATE FROM JAN 2023.xlsx",
  sheet = crs.d.sheets[6], col_names = TRUE)
)

glimpse(crs.d.jun) # 210 cases vs 14 columns

crs.d.jun[duplicated(crs.d.jun), ] # No duplicates 
# in June 23 returns

crs.d.jun[!complete.cases(crs.d.jun),] %>% nrow()# 52 rows have NAs

# Columns of interest
crs.d.jun<-crs.d.jun[,c(3,4,5,6,8,9,11)]

# Removing the NA rows
crs.d.jun<-crs.d.jun[-36,]

# crs.d.jun$id<-as.integer(seq(from=723, to=931, by=1))

# Renaming of columns
crs.d.jun<-
  crs.d.jun %>%
  rename(dod=Date.of.Death...dd.mm.yy.,
         sex=Sex...Indicate.whether.male.or.female.,
         age=Age....Indicate.number.eg.1.2.3.,
         age.unit=Unit.of.age....Days..Months.or.Years.,
         pod=Place.of.death....Indicate.the.name.of.exact.place.of.death.,
         place.type=Type.of.place...Indicate.whether.Home.or.Health.Institution.,
         cod=Cause.of.death....Indicate.the.underlying.cause.of.death.,
  )

# Column inspection
glimpse(crs.d.jun$dod) # date column: yyyy-mm-dd
glimpse(crs.d.jun$sex);unique(crs.d.jun$sex) # 3 levels: Male, Female, Not Stated
glimpse(crs.d.jun$age); unique(crs.d.jun$age)
glimpse(crs.d.jun$age.unit); unique(crs.d.jun$age.unit) # 3 levels: years,Years, NA, Months
glimpse(crs.d.jun$pod); unique(crs.d.jun$pod)# 44 levels
glimpse(crs.d.jun$place.type);unique(crs.d.jun$place.type)
glimpse(crs.d.jun$cod);unique(crs.d.jun$cod)# 42 levels



# Column type convertion
crs.d.jun[crs.d.jun$sex == "Not stated", 2]<-NA # Change of Not stated to NA
crs.d.jun$sex<-factor(crs.d.jun$sex)
crs.d.jun$pod<-factor(crs.d.jun$pod)
crs.d.jun$place.type<-factor(crs.d.jun$place.type)
crs.d.jun$cod<-factor(crs.d.jun$cod)
crs.d.jun$age.unit<-factor(crs.d.jun$age.unit)


# Factor restructuring
crs.d.jun<-crs.d.jun %>% 
  mutate(sex=fct_recode(sex,"male"="Male", "female"="Female"),
         age.unit=fct_recode(age.unit, "years"="Years", "months"="Months"),
         pod=fct_recode(pod, "Kapuonja"="KAPUONJA","West Karateng"="WEST KARATENG","Korando B"="KORANDO B",
                        "West Kolunje"="WEST KOLUNJE","West Othany"="WEST OTHANY","East Reru"="EAST RERU",
                        "Kanyawegi"="KOGADA KANYAWEGI","Karateng"="KARATENG","East Karateng"="EAST KANYADWERA",
                        "Kajulu Koker"="KAJULU KOKER", "Kitmikayi"="KIT MIKAYI", "East Othany"="EAST OTHANY",
                        "East Karateng"="EAST KARATENG","Kanyawegi"="KANYAWEGI","Bar A"="BAR A",
                        "Kaila"="KAILA","Lower Kombewa"="LOWER KOMBEWA","Marera"="MARERA",
                        "West Ngere"="WEST NGERE","West Kadinga"="WEST KADINGA","Nyahera"="NYAHERA",
                        "West Reru"="WEST RERU","maseno mission hospital"="MASENO MISSION HOSP",
                        "chulaimmbo county hospital"="CHULAIMBO C .HOSP","kombewa county hospital"="KOMBEWA C HOSPITAL",
                        "port florence hospital"="PORT FLORENCE HOSPITAL","masaba hospital limited"="MASABA HOSPITAL",
                        "st jairus hospital"="ST JAIRUS HOSPITAL","Kajulu Koker"="KAJULU-KOKER",
                        "Korando A"="KORANDO A","West Kanyadwera"="WEST KANYADWERA","Upper Kombewa"="UPPER KOMBEWA",
                        "Ojola"="OJOLA","Mkendwa"="MKENDWA","Dago"="DAGO","North Kapuonja"="NORTH KAPUONJA",
                        "Kadero"="KADERO","East Ngere"="EAST NGERE","South Kapuonja"="SOUTH KAPUONJA",
                        "Kogony"="KOGONY","Ang'oga"="ANGOGA","Alwala"="ALWALA","East Kadinga"="EAST KADINGA"),
         cod=fct_recode(cod, "Sudden Death"="Sudden death","Urinary Obstruction"="urinay obstruction",
                        "Diarrhoeal Diseases"="Diarrhoeal diseases","Breast Cancer"="Breast cancer",
                        "Renal Agenesis"="Renal agenesis","Peptic Ulcer"="Peptic ulcer",
                        "Road Traffic Accidents"="Road traffic accidents","Diabetes Mellitus"="Diabetes mellitus",
                        "Liver Cirrhosis"="liver Cirrhosis","Oesophageal Atresia"="Oesophageal atresia",
                        "Kidney Failure"="Kidney failure","HIV"="AIDS","HIV"="Aids",
                        "Prematurity And Low Birth Weight"="Prematurity and low birth weight")
  )

levels(crs.d.jun$sex) # 2 levels
levels(crs.d.jun$age.unit) # 3 levels
levels(crs.d.jun$pod) # 40 places
levels(crs.d.jun$cod) # 40 causes


### Jan-Jun 2023 deaths
crs.d.all.1<-merge(x=crs.d.jan, y=crs.d.feb, all=T)
crs.d.all.2<-merge(x=crs.d.mar, y=crs.d.apr,all=T)                
crs.d.all.3<-merge(x=crs.d.may, y=crs.d.jun)
crs.d.all.4<-merge(x=crs.d.all.1, y=crs.d.all.2, all=T)
jan.jun.d<-merge(x=crs.d.all.3, y=crs.d.all.4, all=T) # Merging all returns 

