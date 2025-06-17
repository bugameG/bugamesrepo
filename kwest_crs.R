library(tidyverse)
library(readxl)
library(plotly)
library(ggthemes)
library(rmarkdown)
library(flexdashboard)



# Importing the data
crs.b.sheets<-excel_sheets( # Birth template 2023 excel sheets
    "Revised Birth Template Tool 2023N .xlsx")

                      

          ### January 2023 returns
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
crs.b.jan<-crs.b.jan[,c(1,2,3,5,6,7,8,11)]

# Renaming of columns
crs.b.jan<-
  crs.b.jan %>%
  rename(dob = Date.of.Birth...dd.mm.yy.,
         sex = Sex...Indicate.whether.male.or.female.,
         pob = Place.of.birth....Indicate.the.name.of.facility.or.Sublocation.of.birth.,
         place.type = Type.of.place...Indicate.whether.Home.or.Health.Facility.,
         marital.s = Marital.status,
        education = Mother.s.level.of.Education,
        mom.age = Age.of.the.mother....indicate.the.age.of.the.mother.at.the.birth.of.the.child.,
        nob = Nature.of.birth...Indicate.whether.born.alive.or.dead.)
  
# Column type convertion
crs.b.jan$sex<-factor(crs.b.jan$sex)
crs.b.jan$pob<-factor(crs.b.jan$pob)
crs.b.jan$place.type<-factor(crs.b.jan$place.type)
crs.b.jan$marital.s<-factor(crs.b.jan$marital.s)
crs.b.jan$education<-factor(crs.b.jan$education)
crs.b.jan$nob<-factor(crs.b.jan$nob)
crs.b.jan$dob<-as.Date(crs.b.jan$dob, format=c("%y-%m-%d"))


# Column inspection
str(crs.b.jan$dob) 
glimpse(sex);levels(crs.b.jan$sex) # 4 levels:
                                    # male,Male, female,Female
glimpse(crs.b.jan$pob)# 17 levels
glimpse(crs.b.jan$place.type) # 2 levels
glimpse(crs.b.jan$marital.s); levels(crs.b.jan$marital.s) 
# 7 levels: divorced,Divorced, married,Married
 #          single, widowed,Widowed
glimpse(crs.b.jan$education); levels(crs.b.jan$education)
# 7 levels: Not stated, Primary, secondary,Secondary,
 # Tertiary, university,University

glimpse(crs.b.jan$mom.age) # text to integer convertion

glimpse(crs.b.jan$nob) 
# 4 levels: Alive, dead,Dead, not stated

crs.b.jan[crs.b.jan$mom.age == "not stated", 6]<-NA

# Column type convertion
crs.b.jan$mom.age<-as.numeric(crs.b.jan$mom.age)


# Factor restructuring
crs.b.jan<-crs.b.jan %>% 
 mutate(sex=fct_recode(sex, "female" = "Female","male" = "Male"),
        marital.s=fct_recode(marital.s, "divorced"="Divorced",
                                        "married"="Married",
                                        "widowed"="Widowed"),
        education=fct_recode(education, "primary"="Primary",
                                        "secondary"="Secondary",
                                        "tertiary"="Tertiary",
                                        "university"="University"),
        nob=fct_recode(nob, "dead"="Dead", "alive"="Alive"))
                 
levels(crs.b.jan$sex) # 2 levels
levels(crs.b.jan$marital.s) # 4 levels
levels(crs.b.jan$education)# 5 levels
levels(crs.b.jan$nob) # 3 levels

jan.births<-crs.b.jan # Clean data re-assignment







           ###


# Analysis
jan.births[jan.births$dob < "2022-07-01", ] # there are no 
                                            # late births recorded

# Sex of children
jan.births %>% 
  group_by(sex) %>%
  count() %>% 
  arrange(desc(n))

# Marital status
jan.births %>% 
  group_by(marital.s) %>% 
  count() %>% 
  arrange(desc(n))

# Place of birth
jan.births %>% 
  group_by(place.type) %>% 
  count() %>% 
  arrange(desc(n))


# Place of birth (General)
jan.births %>% 
  filter(place.type == 'Health Facility') %>%
  group_by(pob) %>% 
  count() %>% 
  arrange(desc(n))

# Place of birth (Home births)
jan.births %>% 
  filter(place.type == 'Home') %>%
  group_by(pob) %>% 
  count() %>% 
  arrange(desc(n))

# Level of education
jan.births %>% 
  group_by(education) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  ggplot()+
  geom_bar(data=NULL,aes(x=reorder(education,n),y=n),stat='identity')
  
# Obtaining percentages
jan.births %>% 
  group_by(education) %>% 
  summarise(Count=n(),
            pct=Count*100/nrow(jan.births)) %>% 
  arrange(desc(Count))

hf<-jan.births %>% 
  filter(place.type == 'Health Facility') %>%
  group_by(pob) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  ggplot()+
  geom_bar(aes(x=reorder(pob,n),y=n),fill="royalblue4", alpha=0.5, stat='identity')+
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  labs(x="Health facility",
       y="Frequency")+
  theme_pander()

ggplotly(hf)


527*100/(533) # health facility
6*100/(533) # home 

# Nature of births
jan.births %>% 
  group_by(nob) %>% 
  count()

jan.births %>% 
  group_by(nob) %>% 
  summarise(Count=n(),
            pct=Count*100/nrow(jan.births)) %>% 
  arrange(desc(Count))





        ### February 2023
crs.b.feb<-data.frame(read_excel(
  "Revised Birth Template Tool 2023N .xlsx",
  sheet = crs.b.sheets[2], col_names = TRUE))


# Columns of interest
crs.b.feb<-crs.b.feb[,c(1,2,3,5,6,7,8,11)]


# Renaming of columns
crs.b.feb<-
  crs.b.feb %>%
  rename(dob = Date.of.Birth...dd.mm.yy.,
         sex = Sex...Indicate.whether.male.or.female.,
         pob = Place.of.birth....Indicate.the.name.of.facility.or.Sublocation.of.birth.,
         place.type = Type.of.place...Indicate.whether.Home.or.Health.Facility.,
         marital.s = Marital.status,
         education = Mother.s.level.of.Education,
         mom.age = Age.of.the.mother....indicate.the.age.of.the.mother.at.the.birth.of.the.child.,
         nob = Nature.of.birth...Indicate.whether.born.alive.or.dead.)

# Column type convertion
crs.b.feb$sex<-factor(crs.b.feb$sex)
crs.b.feb$pob<-factor(crs.b.feb$pob)
crs.b.feb$place.type<-factor(crs.b.feb$place.type)
crs.b.feb$marital.s<-factor(crs.b.feb$marital.s)
crs.b.feb$education<-factor(crs.b.feb$education)
crs.b.feb[is.na(crs.b.feb$nob), 3]<-"not stated"
crs.b.feb$nob<-factor(crs.b.feb$nob)
crs.b.feb$dob<-as.Date(crs.b.feb$dob, format=c("%y-%m-%d"))
crs.b.feb[crs.b.feb$mom.age == "not stated" | crs.b.feb$mom.age == "N/A", 6]<-NA
crs.b.feb$mom.age<-as.numeric(crs.b.feb$mom.age)


# Column inspection
str(crs.b.feb$dob) 
glimpse(crs.b.feb$sex);levels(crs.b.feb$sex) # 2 levels:
# Male, Female
glimpse(crs.b.feb$pob)# 14 levels
glimpse(crs.b.feb$place.type) # 1 levels
glimpse(crs.b.feb$marital.s); levels(crs.b.feb$marital.s) 
# 4 levels: Married, not stated,
#          Single, widowed,
glimpse(crs.b.feb$education); levels(crs.b.feb$education)
# 5 levels: Not Stated, Primary, Secondary,
#           Tertiary, University

glimpse(crs.b.feb$mom.age) 

glimpse(crs.b.feb$nob) 
# 3 levels: Alive, dead,Dead, not stated




# Factor restructuring
crs.b.feb<-crs.b.feb %>% 
  mutate(sex=fct_recode(sex, "female" = "Female","male" = "Male"),
         marital.s=fct_recode(marital.s, "single"="Single",
                              "married"="Married"),
         education=fct_recode(education, "primary"="Primary",
                              "secondary"="Secondary",
                              "tertiary"="Tertiary",
                              "university"="University"),
         nob=fct_recode(nob, "dead"="Dead", "alive"="Alive"))

levels(crs.b.feb$sex) # 2 levels
levels(crs.b.feb$marital.s) # 4 levels
levels(crs.b.feb$education)# 5 levels
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
crs.b.mar<-crs.b.mar[,c(1,2,3,5,6,7,8,11)]


# Renaming of columns
crs.b.mar<-
  crs.b.mar %>%
  rename(dob = Date.of.Birth...dd.mm.yy.,
         sex = Sex...Indicate.whether.male.or.female.,
         pob = Place.of.birth....Indicate.the.name.of.facility.or.Sublocation.of.birth.,
         place.type = Type.of.place...Indicate.whether.Home.or.Health.Facility.,
         marital.s = Marital.status,
         education = Mother.s.level.of.Education,
         mom.age = Age.of.the.mother....indicate.the.age.of.the.mother.at.the.birth.of.the.child.,
         nob = Nature.of.birth...Indicate.whether.born.alive.or.dead.)

# Column type convertion
crs.b.mar$sex<-factor(crs.b.mar$sex)
crs.b.mar$pob<-factor(crs.b.mar$pob)
crs.b.mar$place.type<-factor(crs.b.mar$place.type)
crs.b.mar$marital.s<-factor(crs.b.mar$marital.s)
crs.b.mar$education<-factor(crs.b.mar$education)
#crs.b.feb[is.na(crs.b.feb$nob), 3]<-"not stated"
crs.b.mar$nob<-factor(crs.b.mar$nob)
crs.b.mar$dob<-as.Date(crs.b.mar$dob, format=c("%y-%m-%d"))


# Column inspection
str(crs.b.mar$dob) 
glimpse(crs.b.mar$sex);levels(crs.b.mar$sex) # 2 levels:
# Male, Female
glimpse(crs.b.mar$pob)# 21 levels
glimpse(crs.b.mar$place.type) # 2 levels
glimpse(crs.b.mar$marital.s); levels(crs.b.mar$marital.s) 
# 4 levels: married, not stated,
#          mingle, widowed,
glimpse(crs.b.mar$education); levels(crs.b.mar$education)
# 5 levels: not Stated, primary, secondary,
#           tertiary, university

glimpse(crs.b.feb$nob);levels(crs.b.feb$nob)
# 3 levels: alive, dead, not stated

glimpse(crs.b.mar$mom.age);unique(crs.b.mar$mom.age)

glimpse(crs.b.mar$place.type) # 2 levels: health facility ,home

crs.b.mar[crs.b.mar$mom.age == "not stated" , 6]<-NA
crs.b.mar$mom.age<-as.numeric(crs.b.mar$mom.age)


# Factor restructuring
crs.b.mar<-crs.b.mar %>% 
  mutate(sex=fct_recode(sex, "female" = "Female","male" = "Male"),
         place.type=fct_recode(place.type, "Health Facility"="health facility",
                                            "Home"="home"))


mar.births<-crs.b.mar # Clean data re-assignment

# Age-bracket
mar.births$mar.age.cut<-cut(mar.births$mom.age,
                            breaks=c(-Inf,17,19,29,39,49,Inf), 
                            labels=c("Below 18 yrs","18-19","20-29","30-39","40-49","50+"))





                    ### April 2023
crs.b.apr<-data.frame(read_excel(
  "Revised Birth Template Tool 2023N .xlsx",
  sheet = crs.b.sheets[4], col_names = TRUE))


# Columns of interest
crs.b.apr<-crs.b.apr[,c(1,2,3,5,6,7,8,11)]


# Renaming of columns
crs.b.apr<-
  crs.b.apr %>%
  rename(dob = Date.of.Birth...dd.mm.yy.,
         sex = Sex...Indicate.whether.male.or.female.,
         pob = Place.of.birth....Indicate.the.name.of.facility.or.Sublocation.of.birth.,
         place.type = Type.of.place...Indicate.whether.Home.or.Health.Facility.,
         marital.s = Marital.status,
         education = Mother.s.level.of.Education,
         mom.age = Age.of.the.mother....indicate.the.age.of.the.mother.at.the.birth.of.the.child.,
         nob = Nature.of.birth...Indicate.whether.born.alive.or.dead.)

# Column type convertion
crs.b.apr$sex<-factor(crs.b.apr$sex)
crs.b.apr$pob<-factor(crs.b.apr$pob)
crs.b.apr$place.type<-factor(crs.b.apr$place.type)
crs.b.apr$marital.s<-factor(crs.b.apr$marital.s)
crs.b.apr$education<-factor(crs.b.apr$education)
crs.b.apr$nob<-factor(crs.b.apr$nob)
crs.b.apr$dob<-as.Date(crs.b.apr$dob, format=c("%y-%m-%d"))


# Column inspection
str(crs.b.apr$dob) ## All NAs 
glimpse(crs.b.apr$sex);levels(crs.b.apr$sex) # 2 levels:
# Male,male,  Female,female
glimpse(crs.b.apr$pob)# 14 levels
glimpse(crs.b.apr$place.type) # 1 level
glimpse(crs.b.apr$marital.s); levels(crs.b.apr$marital.s) 
# 5 levels: Divorced, Married, single,Single, Widowed 
glimpse(crs.b.apr$education); levels(crs.b.apr$education)
# 5 levels: Not Stated, primary,Primary, secondary,Secondary,
#           tertiary,Tertiary, University

glimpse(crs.b.apr$nob);unique(crs.b.apr$nob)
# 2 levels: Alive, Dead


glimpse(crs.b.apr$mom.age);unique(crs.b.apr$mom.age)

glimpse(crs.b.apr$place.type) # 1 level: Health Facility 

crs.b.apr[crs.b.apr$mom.age == "not indicated" | crs.b.apr$mom.age ==  "N/A" , 6]<-NA
crs.b.apr$mom.age<-as.numeric(crs.b.apr$mom.age)


# Factor restructuring
crs.b.apr<-crs.b.apr %>% 
  mutate(sex=fct_recode(sex, "female" = "Female","male" = "Male"),
         nob=fct_recode(nob, "alive"="Alive",
                        "dead"="Dead"),
         marital.s=fct_recode(marital.s, "divorced"="Divorced",
                               "married"="Married", 
                               "single"="Single","widowed"="Widowed"
                               ),
         education=fct_recode(education, "primary"="Primary",
                              "secondary"="Secondary","tertiary"="Tertiary",
                              "university"="University"))


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
crs.b.may<-crs.b.may[,c(1,2,3,5,6,7,8,11)]


# Renaming of columns
crs.b.may<-
  crs.b.may %>%
  rename(dob = Date.of.Birth...dd.mm.yy.,
         sex = Sex...Indicate.whether.male.or.female.,
         pob = Place.of.birth....Indicate.the.name.of.facility.or.Sublocation.of.birth.,
         place.type = Type.of.place...Indicate.whether.Home.or.Health.Facility.,
         marital.s = Marital.status,
         education = Mother.s.level.of.Education,
         mom.age = Age.of.the.mother....indicate.the.age.of.the.mother.at.the.birth.of.the.child.,
         nob = Nature.of.birth...Indicate.whether.born.alive.or.dead.)

# Column type convertion
crs.b.may$sex<-factor(crs.b.may$sex)
crs.b.may$pob<-factor(crs.b.may$pob)
crs.b.may$place.type<-factor(crs.b.may$place.type)
crs.b.may$marital.s<-factor(crs.b.may$marital.s)
crs.b.may$education<-factor(crs.b.may$education)
crs.b.may$nob<-factor(crs.b.may$nob)
crs.b.may$dob<-as.Date(crs.b.may$dob, format=c("%y-%m-%d"))


# Column inspection
str(crs.b.may$dob)  
glimpse(crs.b.apr$sex);levels(crs.b.apr$sex) # 2 levels:
# male and Female
glimpse(crs.b.may$pob)# 19 levels
glimpse(crs.b.may$place.type) # 2 level
glimpse(crs.b.may$marital.s); levels(crs.b.may$marital.s) 
# 5 levels: divorced, married,Married, single, widowed 
glimpse(crs.b.may$education); levels(crs.b.may$education)
# 4 levels: primary, secondary,
#           Tertiary, university

glimpse(crs.b.may$nob);unique(crs.b.may$nob)
# 2 levels: alive, Dead, NA


glimpse(crs.b.may$mom.age);unique(crs.b.may$mom.age)

glimpse(crs.b.may$place.type) # 2 level: Health Facility, Home 

crs.b.may$mom.age<-as.numeric(crs.b.may$mom.age)


# Factor restructuring
crs.b.may<-crs.b.may %>% 
  mutate(sex=fct_recode(sex, "female" = "Female"),
         marital.s=fct_recode(marital.s, "married" = "Married"),
         education=fct_recode(education, "tertiary" = "Tertiary"),
         nob=fct_recode(nob, "dead" = "Dead"))


may.births<-crs.b.may # Clean data re-assignment

# Age-bracket
may.births$may.age.cut<-cut(may.births$mom.age,
                            breaks=c(-Inf,17,19,29,39,49,Inf), 
                            labels=c("Below 18 yrs","18-19","20-29","30-39","40-49","50+"))


may.births %>% 
  group_by(sex)  %>% 
  summarise(number = n())





                                ### June 2023
crs.b.jun<-data.frame(read_excel(
  "Revised Birth Template Tool 2023N .xlsx",
  sheet = crs.b.sheets[6], col_names = TRUE))


# Columns of interest
crs.b.jun<-crs.b.jun[,c(1,2,3,5,6,7,8,11)]


# Renaming of columns
crs.b.jun<-
  crs.b.jun %>%
  rename(dob = Date.of.Birth...dd.mm.yy.,
         sex = Sex...Indicate.whether.male.or.female.,
         pob = Place.of.birth....Indicate.the.name.of.facility.or.Sublocation.of.birth.,
         place.type = Type.of.place...Indicate.whether.Home.or.Health.Facility.,
         marital.s = Marital.status,
         education = Mother.s.level.of.Education,
         mom.age = Age.of.the.mother....indicate.the.age.of.the.mother.at.the.birth.of.the.child.,
         nob = Nature.of.birth...Indicate.whether.born.alive.or.dead.)

# Column type convertion
crs.b.jun$sex<-factor(crs.b.jun$sex)
crs.b.jun$pob<-factor(crs.b.jun$pob)
crs.b.jun$place.type<-factor(crs.b.jun$place.type)
crs.b.jun$marital.s<-factor(crs.b.jun$marital.s)
crs.b.jun$education<-factor(crs.b.jun$education)
crs.b.jun$nob<-factor(crs.b.jun$nob)
crs.b.jun$dob<-as.Date(crs.b.jun$dob, format=c("%y-%m-%d"))


# Column inspection
str(crs.b.jun$dob)  # All NAs 
glimpse(crs.b.jun$sex);levels(crs.b.jun$sex) # 2 levels:
# Male and Female
glimpse(crs.b.jun$pob)# 17 levels
glimpse(crs.b.jun$place.type) # 2 level
glimpse(crs.b.jun$marital.s); levels(crs.b.jun$marital.s) 
# 5 levels: Married, MARRIED,
#           Single, SINGLE, WIDOWED
glimpse(crs.b.jun$education); levels(crs.b.jun$education)
# 5 levels: Not Stated, Primary, Secondary,
#           Tertiary, University

glimpse(crs.b.jun$nob);unique(crs.b.jun$nob)
# 3 levels: alive, Dead, NA


glimpse(crs.b.may$mom.age);unique(crs.b.may$mom.age)

glimpse(crs.b.may$place.type) # 2 level: Health Facility, Home 

crs.b.may$mom.age<-as.numeric(crs.b.may$mom.age)


# Factor restructuring
crs.b.may<-crs.b.may %>% 
  mutate(sex=fct_recode(sex, "female" = "Female"),
         marital.s=fct_recode(marital.s, "married" = "Married"),
         education=fct_recode(education, "tertiary" = "Tertiary"),
         nob=fct_recode(nob, "dead" = "Dead"))


may.births<-crs.b.may # Clean data re-assignment

# Age-bracket
may.births$may.age.cut<-cut(may.births$mom.age,
                            breaks=c(-Inf,17,19,29,39,49,Inf), 
                            labels=c("Below 18 yrs","18-19","20-29","30-39","40-49","50+"))


may.births %>% 
  group_by(sex)  %>% 
  summarise(number = n())
