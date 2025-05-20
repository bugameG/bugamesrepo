library(tidyverse)
library(readxl)
library(rmarkdown)
library(moments)

# Importing the data
eb6 = read_excel("CLASS 8,7,6 EBUSSAMBA.xlsx",  # Class 6
      col_names = FALSE, sheet = 3) 
eb7 = read_excel("CLASS 8,7,6 EBUSSAMBA.xlsx", # Class 7 
      col_names = FALSE, sheet = 2) 
eb8 = read_excel("CLASS 8,7,6 EBUSSAMBA.xlsx", # Class 8
      col_names = FALSE, sheet = 1)

###############
##  CLASS VI ##
###############

## DATA RESTRUCTURING
eb6 = eb6[-c(1,2,3,4,130,131,132,133,134),-c(1,2,8)] 
eb6 = eb6 %>% 
  rename("eng"="...3", "kis"="...4", "math"="...5",
         "sci"="...6", "ssre"="...7", "strm"="...9") %>% 
  mutate(ID = seq(1,nrow(eb6),1))

## DATA CLEANING
# DUPLICATES
eb6[duplicated(eb6), ] # No Duplicates

# MISSING VALUES
eb6[is.na(eb6$eng), ] # 2
eb6[is.na(eb6$kis), ]  # 1
eb6[is.na(eb6$math), ] # 1
eb6[is.na(eb6$sci), ]
eb6[is.na(eb6$ssre), ] 
eb6[is.na(eb6$strm), ]

# DATA-TYPE CONVERSION
# strm
eb6$strm = factor(eb6$strm); levels(eb6$strm)

# ssre
eb6$ssre = as.numeric(eb6$ssre); str(eb6$ssre)

# scie
eb6$sci = as.numeric(eb6$sci)

# math
eb6$math = as.numeric(eb6$math)
eb6[33, 3] <- round(mean(eb6$math, na.rm = TRUE),0) # FILLING IN THE MISING VALUES

# kis
eb6$kis = as.numeric(eb6$kis)
eb6[122, 2] <- round(mean(eb6$kis, na.rm = TRUE),0) # FILLING IN THE MISING VALUES

# eng
eb6$eng = as.numeric(eb6$eng)
eb6[c(120,125), 1] <- round(mean(eb6$eng, na.rm = TRUE),0) # FILLING IN THE MISING VALUES


## ADDING THE TOTALS COLUMN
eb6$total <- eb6$eng + eb6$kis + eb6$math + eb6$sci + eb6$ssre

## OBTAINING RANKS
eb6=eb6 %>% 
  mutate(rank = rank(-total, ties.method = "min")) %>% 
  arrange(rank) 

# FINAL
eb6=tibble("ID"=eb6$ID,"strm"=eb6$strm,"eng"=eb6$eng,"kis"=eb6$kis,
           "math"=eb6$math,"sci"=eb6$sci,"ssre"=eb6$ssre,
           "total"=eb6$total,"rank"=eb6$rank)
# EXPORTING
# write_csv(eb6, "eb6.csv") 




################
##  CLASS VII ##
################

## DATA RESTRUCTURING
eb7 = eb7[-c(1,2,3,4,132,133,134,135),-c(1,2,8)] 
eb7 = eb7 %>% 
  rename("eng"="...3", "kis"="...4", "math"="...5",
         "sci"="...6", "ssre"="...7", "strm"="...9") %>% 
  mutate(ID = seq(1,nrow(eb7),1))

## DATA CLEANING
# DUPLICATES
eb7[duplicated(eb7), ] # No Duplicates

# MISSING VALUES
eb7[is.na(eb7$eng), ] 
eb7[is.na(eb7$kis), ]
eb7[is.na(eb7$math), ] 
eb7[is.na(eb7$sci), ]
eb7[is.na(eb7$ssre), ] # 7
eb7[is.na(eb7$strm), ]

# DATA-TYPE CONVERSION
# strm
eb7$strm = factor(eb7$strm); levels(eb7$strm)

# ssre
eb7$ssre = as.numeric(eb7$ssre)
eb7[c(82,103,112,123,125,126,127),5] <- round(mean(eb7$ssre,na.rm=TRUE),0)

# scie
eb7$sci = as.numeric(eb7$sci)

# math
eb7$math = as.numeric(eb7$math)

# kis
eb7$kis = as.numeric(eb7$kis)

# eng
eb7$eng = as.numeric(eb7$eng)


## ADDING THE TOTALS COLUMN
eb7$total <- eb7$eng + eb7$kis + eb7$math + eb7$sci + eb7$ssre

## OBTAINING RANKS
eb7=eb7 %>% 
  mutate(rank = rank(-total, ties.method = "min")) %>% 
  arrange(rank) 

# FINAL
eb7=tibble("ID"=eb7$ID,"strm"=eb7$strm,"eng"=eb7$eng,"kis"=eb7$kis,
           "math"=eb7$math,"sci"=eb7$sci,"ssre"=eb7$ssre,
           "total"=eb7$total,"rank"=eb7$rank)
# EXPORTING
# write_csv(eb7, "eb7.csv") 





#################
##  CLASS VIII ##
################

## DATA RESTRUCTURING
eb8 = eb8[-c(1,2,3,4,107,108,109,110),-c(1,2,8)] 
eb8 = eb8 %>% 
  rename("eng"="...3", "kis"="...4", "math"="...5",
         "sci"="...6", "ssre"="...7", "strm"="...9") %>% 
  mutate(ID = seq(1,nrow(eb8),1))

## DATA CLEANING
# DUPLICATES
eb8[duplicated(eb8), ] # No Duplicates

# MISSING VALUES
eb8[is.na(eb8$eng), ] 
eb8[is.na(eb8$kis), ]
eb8[is.na(eb8$math), ] 
eb8[is.na(eb8$sci), ]
eb8[is.na(eb8$ssre), ] # 2
eb8[is.na(eb8$strm), ]

# DATA-TYPE CONVERSION
# strm
eb8$strm = factor(eb8$strm); levels(eb8$strm)

# ssre
eb8$ssre = as.numeric(eb8$ssre)
eb8[c(85,102),5] <- round(mean(eb8$ssre,na.rm=TRUE),0)

# scie
eb8$sci = as.numeric(eb8$sci)

# math
eb8$math = as.numeric(eb8$math)

# kis
eb8$kis = as.numeric(eb8$kis)

# eng
eb8$eng = as.numeric(eb8$eng)


## ADDING THE TOTALS COLUMN
eb8$total <- eb8$eng + eb8$kis + eb8$math + eb8$sci + eb8$ssre

## OBTAINING RANKS
eb8=eb8 %>% 
  mutate(rank = rank(-total, ties.method = "min")) %>% 
  arrange(rank) 

# FINAL
eb8=tibble("ID"=eb8$ID,"strm"=eb8$strm,"eng"=eb8$eng,"kis"=eb8$kis,
           "math"=eb8$math,"sci"=eb8$sci,"ssre"=eb8$ssre,
           "total"=eb8$total,"rank"=eb8$rank)
# EXPORTING
# write_csv(eb8, "eb8.csv") 
