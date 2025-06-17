library(rmarkdown)
library(flexdashboard)
library(tidyverse)
library(readxl)
library(plotly)
library(ggthemes)




# Importing the data
crs.b.sheets<-excel_sheets( # Birth template 2023 excel sheets
    "Revised Birth Template Tool 2023N .xlsx")

                      

### January 2023 
crs.b.jan<-data.frame(read_excel(
  "Revised Birth Template Tool 2023N .xlsx",
  sheet = crs.b.sheets[1], col_names = TRUE)
)

glimpse(crs.b.jan) # 533 cases vs 15 columns

crs.b.jan[duplicated(crs.b.jan), ] # No duplicates 
                                   # in Jan 23 returns

crs.b.jan[!complete.cases(crs.b.jan),] # No 
                                       # missing values

# Columns of interest
crs.b.jan<-crs.b.jan[,c(2,3,5,6,7)]
crs.b.jan$id<-as.integer(seq(from=1, to=,533 ,by=1))


# Renaming of columns
crs.b.jan<-
  crs.b.jan %>%
  rename(sex = Sex...Indicate.whether.male.or.female.,
         pob = Place.of.birth....Indicate.the.name.of.facility.or.Sublocation.of.birth.,
         place.type = Type.of.place...Indicate.whether.Home.or.Health.Facility.,
         mom.age = Age.of.the.mother....indicate.the.age.of.the.mother.at.the.birth.of.the.child.,
         nob = Nature.of.birth...Indicate.whether.born.alive.or.dead.)
  
# Column inspection
str(crs.b.jan$dob) 
glimpse(sex);levels(crs.b.jan$sex) # 4 levels:
# male,Male, female,Female
glimpse(crs.b.jan$pob)# 17 levels
# chulaimbo, Chulaimbo 
glimpse(crs.b.jan$place.type) # 2 levels

glimpse(crs.b.jan$mom.age) # text to integer convertion

glimpse(crs.b.jan$nob) 
# 4 levels: Alive, dead,Dead, not stated



# Column type convertion
crs.b.jan$sex<-factor(crs.b.jan$sex)
crs.b.jan$pob<-factor(crs.b.jan$pob)
crs.b.jan$place.type<-factor(crs.b.jan$place.type)
crs.b.jan$nob<-factor(crs.b.jan$nob)

unique(crs.b.jan$mom.age)# not stated to NA
crs.b.jan[crs.b.jan$mom.age == "not stated", 5]<-NA
# Column type convertion
crs.b.jan$mom.age<-as.numeric(crs.b.jan$mom.age)



# Factor restructuring
crs.b.jan<-crs.b.jan %>% 
 mutate(sex=fct_recode(sex, "female" = "Female","male" = "Male"),
        pob=fct_recode(pob, "airport health centre"="Airport health centre",
                       "chulaimbo county hospital"="Chulaimbo County Hospital",
                       "chulaimbo county hospital"="chulaimbo County Hospital",
                       "barkorwa health centre"="Barkorwa Health Centre",
                       "kombewa county hospital"="Kombewa County Hospital",
                       "manyuanda sc hospital"="manyuanda s.c. Hospital",
                       "maseno mission hospital"="Maseno Mission hospital",
                       "miranga sc hospital"="Miranga S C Hospital",
                       "ojola sc hospital"="Ojola S C Hospital",
                       "port florence hospital"="Port Florence Hospital",
                       "riat dispensary"="Riat Dispensary",
                       "st jairus hospital"="st jairus Hospital"),
        nob=fct_recode(nob, "dead"="Dead", "alive"="Alive"))
                 
levels(crs.b.jan$sex) # 2 levels
levels(crs.b.jan$nob) # 3 levels

jan.births<-crs.b.jan # Clean data re-assignment


# Age-bracket
jan.births$jan.age.cut<-cut(jan.births$mom.age,
                            breaks=c(-Inf,17,19,29,39,49,Inf), 
                            labels=c("Below 18 yrs","18-19","20-29","30-39","40-49","50+"))




### February 2023
crs.b.feb<-data.frame(read_excel(
  "Revised Birth Template Tool 2023N .xlsx",
  sheet = crs.b.sheets[2], col_names = TRUE))


# Columns of interest
crs.b.feb<-crs.b.feb[,c(2,3,5,6,7)]
crs.b.feb$id<-as.integer(seq(from=534, to=1144, by=1))

# Renaming of columns
crs.b.feb<-
  crs.b.feb %>%
  rename(sex = Sex...Indicate.whether.male.or.female.,
         pob = Place.of.birth....Indicate.the.name.of.facility.or.Sublocation.of.birth.,
         place.type = Type.of.place...Indicate.whether.Home.or.Health.Facility.,
         mom.age = Age.of.the.mother....indicate.the.age.of.the.mother.at.the.birth.of.the.child.,
         nob = Nature.of.birth...Indicate.whether.born.alive.or.dead.)


# Column inspection
glimpse(crs.b.feb$sex);unique(crs.b.feb$sex) # 2 levels:
# Male, Female
glimpse(crs.b.feb$pob)# 14 levels
                      # Maseno mission , Maseno Mission
glimpse(crs.b.feb$place.type) # 1 levels

glimpse(crs.b.feb$mom.age);unique(crs.b.feb$mom.age) # not stated and N/A

glimpse(crs.b.feb$nob) 
# 3 levels: Alive, Dead, not stated


# Column type convertion
crs.b.feb$sex<-factor(crs.b.feb$sex)
crs.b.feb$pob<-factor(crs.b.feb$pob)
crs.b.feb$place.type<-factor(crs.b.feb$place.type)

crs.b.feb[is.na(crs.b.feb$nob), 2]<-"not stated"
crs.b.feb$nob<-factor(crs.b.feb$nob)

crs.b.feb[crs.b.feb$mom.age == "not stated" | crs.b.feb$mom.age == "N/A", 5]<-NA
crs.b.feb$mom.age<-as.numeric(crs.b.feb$mom.age)




# Factor restructuring
crs.b.feb<-crs.b.feb %>% 
  mutate(sex=fct_recode(sex, "female" = "Female","male" = "Male"),
         pob=fct_recode(pob, "maseno mission hospital"="Maseno mission Hospital",
                             "alluc medical centre"="Alluc Medical Centre",
                             "barkorwa health centre"="Barkorwa Health centre",
                             "chulaimbo county hospital"="Chulaimbo County Hospital",
                        "kombewa county hospital"="Kombewa County Hospital",
                        "lwala kadawa health centre"="Lwala Kadawa H/C",
                        "manyuanda sc hospital"="Manyuanda S.C Hospital",
                        "masaba hospital limited"="Masaba Hospital Limited",
                        "maseno mission hospital"="Maseno Mission Hospital",
                        "miranga sc hospital"="Miranga S C Hospital",
                        "nyahera sc hospital"="Nyahera S.C.Hospital",
                        "ober kamoth sc hospital"="Ober Kamoth S C Hospital",
                        "port florence hospital"="Port Florence  Hospital",
                        "st jairus hospital"="St Jairus Hospital"),
         nob=fct_recode(nob, "dead"="Dead", "alive"="Alive"))

levels(crs.b.feb$sex) # 2 levels
levels(crs.b.feb$nob) # 3 levels

feb.births<-crs.b.feb # Clean data re-assignment

# Age-bracket
feb.births$feb.age.cut<-cut(feb.births$mom.age,
                            breaks=c(-Inf,17,19,29,39,49,Inf), 
                            labels=c("Below 18 yrs","18-19","20-29","30-39","40-49","50+"))



feb.births %>% 
  group_by(feb.age.cut) %>% 
  na.omit() %>% 
  summarise(Count=n(),
            pct=Count*100/584) %>% 
  ggplot()+
  geom_bar(stat='identity', aes(x=feb.age.cut,y=pct),
           fill="royalblue4", alpha=0.5)+
  labs(x="Age bracket",
       y="pct [%]")+
  coord_flip()+
  theme_pander()



                          


                    ### March 2023
crs.b.mar<-data.frame(read_excel(
  "Revised Birth Template Tool 2023N .xlsx",
  sheet = crs.b.sheets[3], col_names = TRUE))


# Columns of interest
crs.b.mar<-crs.b.mar[,c(2,3,5,6,7)]
crs.b.mar$id<-as.integer(seq(from=1145, to=1772, by=1))


# Renaming of columns
crs.b.mar<-
  crs.b.mar %>%
  rename(sex = Sex...Indicate.whether.male.or.female.,
         pob = Place.of.birth....Indicate.the.name.of.facility.or.Sublocation.of.birth.,
         place.type = Type.of.place...Indicate.whether.Home.or.Health.Facility.,
         mom.age = Age.of.the.mother....indicate.the.age.of.the.mother.at.the.birth.of.the.child.,
         nob = Nature.of.birth...Indicate.whether.born.alive.or.dead.)


# Column inspection
glimpse(crs.b.mar$sex);unique(crs.b.mar$sex) # 3 levels: # Male, Female, NA
glimpse(crs.b.mar$pob); unique(crs.b.mar$pob)# 21 levels
glimpse(crs.b.mar$place.type) ;unique(crs.b.mar$place.type)# 2 levels

glimpse(crs.b.mar$nob);unique(crs.b.mar$nob)
# 3 levels: Alive,alive, dead, NA

glimpse(crs.b.mar$mom.age);unique(crs.b.mar$mom.age)# not stated

glimpse(crs.b.mar$place.type) # 2 levels: health facility ,home


# Column type convertion
crs.b.mar$sex<-factor(crs.b.mar$sex)
crs.b.mar$pob<-factor(crs.b.mar$pob)
crs.b.mar$place.type<-factor(crs.b.mar$place.type)
crs.b.mar$nob<-factor(crs.b.mar$nob)

crs.b.mar[crs.b.mar$mom.age == "not stated" , 5]<-NA
crs.b.mar$mom.age<-as.numeric(crs.b.mar$mom.age)


# Factor restructuring
crs.b.mar<-crs.b.mar %>% 
  mutate(sex=fct_recode(sex, "female" = "Female","male" = "Male"),
         nob=fct_recode(nob, "alive"="Alive"),
         place.type=fct_recode(place.type, "Health Facility"="health facility",
                                            "Home"="home"),
         pob=fct_recode(pob, "maseno mission hospital"="Maseno mission hospital",
                        "ojola sc hospital"="ojolla sub county hospital",
                        "manyuanda sc hospital"="manyuanda s.c hospital",
                        "maseno university health centre"="maseno university H/C",
                        "lwala kadawa health centre"="Lwala kadawa",
                        "kombewa county hospital"="Kombewa  county hospital",
                        "chulaimbo county hospital"="chulaimbo s.c hospital",
                        "airport health centre"="Airport health centre",
                        "masaba hospital limited"="masaba hospital",
                        "st jairus hospital"="st. jairus hospital",
                        "miranga sc hospital"="Miranga s.c hospital",
                        "East Reru"="east reru",
                        "Kit Mikayi"="kitmikayi",
                        "Korando A"="korando A",
                        "Lower Kombewa"="lower kombewa"))


mar.births<-crs.b.mar # Clean data re-assignment

# Age-bracket
mar.births$mar.age.cut<-cut(mar.births$mom.age,
                            breaks=c(-Inf,17,19,29,39,49,Inf), 
                            labels=c("Below 18 yrs","18-19","20-29","30-39","40-49","50+"))





### April 2023
crs.b.apr<-data.frame(read_excel(
  "Revised Birth Template Tool 2023N .xlsx",
  sheet = crs.b.sheets[4]))


# Columns of interest
crs.b.apr<-crs.b.apr[,c(2,3,5,6,7)]
crs.b.apr$id<-as.integer(seq(from=1773, to=2294, by=1))


# Renaming of columns
crs.b.apr<-
  crs.b.apr %>%
  rename(sex = Sex...Indicate.whether.male.or.female.,
         pob = Place.of.birth....Indicate.the.name.of.facility.or.Sublocation.of.birth.,
         place.type = Type.of.place...Indicate.whether.Home.or.Health.Facility.,
         mom.age = Age.of.the.mother....indicate.the.age.of.the.mother.at.the.birth.of.the.child.,
         nob = Nature.of.birth...Indicate.whether.born.alive.or.dead.)


# Column inspection
glimpse(crs.b.apr$sex);unique(crs.b.apr$sex) # 2 levels:
# Male,male,  Female,female
glimpse(crs.b.apr$pob); unique(crs.b.apr$pob)# 14 levels
glimpse(crs.b.apr$place.type); unique(crs.b.apr$place.type) # 1 level
glimpse(crs.b.apr$nob);unique(crs.b.apr$nob)
# 2 levels: Alive, Dead

glimpse(crs.b.apr$mom.age);unique(crs.b.apr$mom.age) # N/A and not indicated



# Column type convertion
crs.b.apr$sex<-factor(crs.b.apr$sex)
crs.b.apr$pob<-factor(crs.b.apr$pob)
crs.b.apr$place.type<-factor(crs.b.apr$place.type)
crs.b.apr$nob<-factor(crs.b.apr$nob)

crs.b.apr[crs.b.apr$mom.age == "not indicated" | crs.b.apr$mom.age ==  "N/A" , 5]<-NA
crs.b.apr$mom.age<-as.numeric(crs.b.apr$mom.age)


# Factor restructuring
crs.b.apr<-crs.b.apr %>% 
  mutate(sex=fct_recode(sex, "female" = "Female","male" = "Male"),
         nob=fct_recode(nob, "alive"="Alive",
                        "dead"="Dead"),
         pob=fct_recode(pob, "otieno owala health centre"="Otieno Owala",
                        "maseno university health centre"="maseno university",
                        "st jairus hospital"="St. Jairus hospital",
                        "ratta health centre"="Ratta Health Centre",
                        "port florence hospital"="Port Florence Hospital",
                        "manyuanda sc hospital"="Manyuanda S C Hospital",
                        "asat beach dispensary"="Asat Beach Dispensary",
                        "kombewa county hospital"="Kombewa County Hopital",
                        "masaba hospital limited"="Masaba Hospital Limited",
                        "alluc medical centre"="Alluc Medical Centre",
                        "maseno mission hospital"="Maseno Mission Hospital",
                        "ober kamoth sc hospital"="Ober Kamoth S C Hospital",
                        "miranga sc hospital"="Miranga S C Hospital",
                        "chulaimbo county hospital"="Chulaimbo County Hospital"))


apr.births<-crs.b.apr # Clean data re-assignment

# Age-bracket
apr.births$apr.age.cut<-cut(apr.births$mom.age,
                            breaks=c(-Inf,17,19,29,39,49,Inf), 
                            labels=c("Below 18 yrs","18-19","20-29","30-39","40-49","50+"))




### May 2023
crs.b.may<-data.frame(read_excel(
  "Revised Birth Template Tool 2023N .xlsx",
  sheet = crs.b.sheets[5], col_names = TRUE))


# Columns of interest
crs.b.may<-crs.b.may[,c(2,3,5,6,7)]
crs.b.may$id<-as.integer(seq(from=2295, to=2861, by=1))


# Renaming of columns
crs.b.may<-
  crs.b.may %>%
  rename(sex = Sex...Indicate.whether.male.or.female.,
         pob = Place.of.birth....Indicate.the.name.of.facility.or.Sublocation.of.birth.,
         place.type = Type.of.place...Indicate.whether.Home.or.Health.Facility.,
         mom.age = Age.of.the.mother....indicate.the.age.of.the.mother.at.the.birth.of.the.child.,
         nob = Nature.of.birth...Indicate.whether.born.alive.or.dead.)

# Column inspection
glimpse(crs.b.may$sex);unique(crs.b.may$sex) # 2 levels:
# male, Female and NA
glimpse(crs.b.may$pob); unique(crs.b.may$pob)# 19 levels

glimpse(crs.b.may$nob);unique(crs.b.may$nob)
# 2 levels: alive, Dead, NA
glimpse(crs.b.may$mom.age);unique(crs.b.may$mom.age)
glimpse(crs.b.may$place.type);unique(crs.b.may$place.type) # 2 level: Health facility, Home 

# Column type convertion
crs.b.may$sex<-factor(crs.b.may$sex)
crs.b.may$pob<-factor(crs.b.may$pob)
crs.b.may$place.type<-factor(crs.b.may$place.type)
crs.b.may$nob<-factor(crs.b.may$nob)
crs.b.may$mom.age<-as.numeric(crs.b.may$mom.age)


# Factor restructuring
crs.b.may<-crs.b.may %>% 
  mutate(sex=fct_recode(sex, "female" = "Female"),
         place.type=fct_recode(place.type, "Health Facility"="Health facility"),
         nob=fct_recode(nob, "dead" = "Dead"),
         pob=fct_recode(pob, "chulaimbo county hospital"="Chulaimbo County Hospital",
                        "ober kamoth sc hospital"="Ober kamoth s.c Hospital",
                        "ratta health centre"="Ratta H/C",
                        "st jairus hospital"="St jairus hospital",
                        "lwala kadawa health centre"="Lwala kadawa hc",
                        "maseno university health centre"="Maseno university H/C",
                        "maseno mission hospital"="Maseno mission hospital",
                        "bodi health centre"="Bodi Health Centre",
                        "manyuanda sc hospital"="Manyuanda S.C.Hospital",
                        "barkorwa health centre"="Barkorwa C.M.Hospital",
                        "port florence hospital"="Port Florence hospital",
                        "kombewa county hospital"="Kombewa County Hospital",
                        "alluc medical centre"="Alluc Medical Centre",
                        "masaba hospital limited"="Masaba Hospital",
                        "otieno owala health centre"="Otieno Owala H/C",
                        "ojola sc hospital"="Ojolla S.C. Hospital",
                        "Upper Osiri"="upper osiri", "Upper Kanyawegi"="upper kanyawegi",
                        "Lower Kanyawegi"="Lower kanyawegi"))


may.births<-crs.b.may # Clean data re-assignment

# Age-bracket
may.births$may.age.cut<-cut(may.births$mom.age,
                            breaks=c(-Inf,17,19,29,39,49,Inf), 
                            labels=c("Below 18 yrs","18-19","20-29","30-39","40-49","50+"))




### June 2023
crs.b.jun<-data.frame(read_excel(
  "Revised Birth Template Tool 2023N .xlsx",
  sheet = crs.b.sheets[6], col_names = TRUE))


# Columns of interest
crs.b.jun<-crs.b.jun[,c(2,3,5,6,7)]
crs.b.jun$id<-as.integer(seq(from=2862, to=3427, by=1))


# Renaming of columns
crs.b.jun<-
  crs.b.jun %>%
  rename(sex = Sex...Indicate.whether.male.or.female.,
         pob = Place.of.birth....Indicate.the.name.of.facility.or.Sublocation.of.birth.,
         place.type = Type.of.place...Indicate.whether.Home.or.Health.Facility.,
         mom.age = Age.of.the.mother....indicate.the.age.of.the.mother.at.the.birth.of.the.child.,
         nob = Nature.of.birth...Indicate.whether.born.alive.or.dead.)


# Column inspection
glimpse(crs.b.jun$sex);unique(crs.b.jun$sex) # 2 levels:
                                             # Male and Female
glimpse(crs.b.jun$pob); unique(crs.b.jun$pob)# 17 levels
glimpse(crs.b.jun$nob);unique(crs.b.jun$nob)
# 3 levels: Alive, Dead, NA

glimpse(crs.b.jun$mom.age);unique(crs.b.jun$mom.age)

glimpse(crs.b.jun$place.type) # 2 level: Health Facility, Home 


# Column type convertion
crs.b.jun$sex<-factor(crs.b.jun$sex)
crs.b.jun$pob<-factor(crs.b.jun$pob)
crs.b.jun$place.type<-factor(crs.b.jun$place.type)
crs.b.jun$nob<-factor(crs.b.jun$nob)
crs.b.jun$mom.age<-as.numeric(crs.b.jun$mom.age)


# Factor restructuring
crs.b.jun<-crs.b.jun %>% 
  mutate(sex=fct_recode(sex, "female" = "Female",
                        "male"="Male"),
         nob=fct_recode(nob, "dead" = "Dead", "alive"="Alive"),
         pob=fct_recode(pob, "maseno mission hospital"="MASENO MISSION HOSP",
                        "miranga sc hospital"="MIRANGA S C HOSPITAL",
                        "rodi health centre"="RODI H C",
                        "ober kamoth sc hospital"="OBER KAMOTH S. C. H",
                        "riat dispensary"="RIAT DISPENSARY",
                        "manyuanda sc hospital"="MANYUANDA S C HOSP",
                        "maseno university health centre"="MASENO UNIVERSITY",
                        "alluc medical centre"="ALLUC MEDICAL CENTRE",
                        "chulaimbo county hospital"="CHULAIMBO HOSPITAL",
                        "langi kawino dispensary"="LANGI KAWINO DISPENSARY",
                        "kombewa county hospital"="KOMBEWA C HOSPITAL",
                        "port florence hospital"="PORT FLORENCE HOSPITAL",
                        "masaba hospital limited"="MASABA HOSPITAL",
                        "st jairus hospital"="ST.JAIRUS HOSPITAL",
                        "Kogony"="KOGONY S/LOC", "Korando A"="KORANDO A", "Alwala"="ALWALA"))


jun.births<-crs.b.jun # Clean data re-assignment


# Age-bracket
jun.births$jun.age.cut<-cut(jun.births$mom.age,
                            breaks=c(-Inf,17,19,29,39,49,Inf), 
                            labels=c("Below 18 yrs","18-19","20-29","30-39","40-49","50+"))



### Jan-June 2023
crs.b.all.1<-merge(x=crs.b.jan, y=crs.b.feb, all=T)# 533, 611
crs.b.all.2<-merge(x=crs.b.mar, y=crs.b.apr,all=T)# 628, 522                  
crs.b.all.3<-merge(x=crs.b.may, y=crs.b.jun,all=T)# 567, 566
crs.b.all.4<-merge(x=crs.b.all.1, y=crs.b.all.2, all=T)
jan.jun.b<-merge(x=crs.b.all.3, y=crs.b.all.4, all=T) # Merging all returns 


### Jan-June 2023 data cleaning
glimpse(jan.jun.b)

# sex
levels(jan.jun.b$sex)# male ,female

# nob
levels(jan.jun.b$nob)# alive, dead, not stated

# place.type
levels(jan.jun.b$place.type)# Health Facility, Home

# mom.age
unique(jan.jun.b$mom.age)# 34 distinct ages

# pob
levels(jan.jun.b$pob)# 36 distinct places

View(jan.jun.b[duplicated(jan.jun.b),]) # no duplicates

# Age-bracket
jan.jun.b$age.grp<-cut(jan.jun.b$mom.age,
                            breaks=c(-Inf,17,19,29,39,49,Inf), 
                            labels=c("Below 18 yrs","18-19","20-29","30-39","40-49","50+"))






