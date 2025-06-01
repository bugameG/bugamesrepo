# lOADING PACKAGES
library(tidyverse)
library(readxl)

# LOADING HR DATA
hr <- read_excel("New folder/Week 1 Data Challenge.xlsx", sheet = "Data Cleaning Task")

# DATA OVERVIEW
# hr |> View()
# hr |> glimpse()

# Renaming columns
hr <- hr |> 
  rename("name"=`Full Name`,"age"=Age,
         "email"=Email,"joindate"=`Join Date`,
         "dpt"=Department,"salary"=Salary)

# DUPLICATES 
# hr[duplicated(hr),] # 212 duplicates present

## Removing duplicates
hr <- hr[!duplicated(hr),] # 4,889 unique records left

# VARIABLES
## I. name
hr$name <- hr$name |> # Convert all string to lower-case
  str_to_lower() 

# Replacing double-spaced Names
# grepl("  ", name) # There are double-spaced names
hr$name <- hr$name |> 
  gsub(pattern = "  ", replacement = "")

# Replacing all periods(.) within name
hr$name <- str_replace(hr$name, pattern = "\\.", replacement = " ")


# Splitting the name column
hr <- separate_wider_delim(hr, cols = name, names = c("first", "second") ,
                           delim = " ", too_few = "debug",  too_many = "debug")

# Titling the names
hr$first <- str_to_title(hr$first)
hr$second <- str_to_title(hr$second)


# Formulating the final name column
hr <- hr[,-c(3:6)] |> 
  mutate(name = paste(first,second)) |> 
  select(-c(1:2)) |> 
  relocate(name)


# II. age
# hr$age |> unique() {NaN, nan, thirty}

## Handling thirty
hr[hr$age=="thirty",]$age <- "30"

## Handling NaN and nan
hr[hr$age %in% c("NaN", "nan"),]$age <- NA

## Converting to numeric
hr$age <- as.numeric(hr$age)

hr[is.na(hr$age),]$age <- ceiling(mean(hr$age, na.rm = TRUE))



# VI. Salary
# hr$salary |> unique()
## Removing all commas
hr$salary <- hr$salary |> 
  str_remove(pattern = "\\,")

## Handling k values
hr$salary <- hr$salary |> 
  str_replace(pattern = "75k", "75000")

## Handling unknown salaries
hr[hr$salary %in% c("unknown"), ]$salary <- NA

## Converting to numerical
hr$salary <- as.numeric(hr$salary)

## Handling missing salary values
hr[is.na(hr$salary), ]$salary <- ceiling(median(hr$salary, na.rm = TRUE))


# V. Department(dpt) 
## hr$dpt |> unique() {finance-Finance, hr-HR}
hr$dpt <- str_to_title(hr$dpt) # Titling for Finance, Operations and Admin Departments 

hr[hr$dpt %in% c("Hr"),]$dpt <- "HR" # HR Department
hr[hr$dpt %in% c("It"),]$dpt <- "HR" # IT Department
hr[hr$dpt %in% c("Admin"),]$dpt <- "Administrative" # Administrative Department

## hr[is.na(hr$dpt),] |> nrow() 1070 cases without a department
hr[is.na(hr$dpt),]$dpt <- sample(c("Sales", "Research", "Marketing"), size = 1070, replace = TRUE)

## Convertion to factorial variable
hr$dpt <- factor(hr$dpt)

# III. email
## email anomalies: {@domain, @example, @example.com, @@email.com}
## Convertion of all email names to lower-case
hr$email <- str_to_lower(hr$email)

## Replacing @example.com
hr$email <- hr$email |> str_replace(pattern = "@domain", replacement = "@yahoo.com")

## Replacing @example
hr$email <- hr$email |> str_replace(pattern = "@example.com", replacement = "@hotmail.com")

## Replacing @example.com
hr$email <- hr$email |> str_replace(pattern = "@example", replacement = "@outlook.com")

## Replacing @@email.com
hr$email <- hr$email |> str_replace(pattern = "@@email.com", replacement = "@gmail.com")

## Removing all NA email entries
hr <- hr[!is.na(hr$email), ] 

# IV. joindate
hr$joindate<- hr$joindate |> 
  str_replace(pattern = "not a date", replacement = "01-Jan-2022") 

## Replacing all missing recruitment dates 
hr$joindate <- hr$joindate |> 
  str_replace_na("10-05-2021")

## Date format conversions 
# hr$joindate <- date(parse_date_time(hr$joindate, orders = c("%d-%b-%Y","%Y/%d/%m","%Y-%m-%d","%d-m%-Y%"))) 

hr$joindate <- hr$joindate |> 
  parse_date_time(orders = c("%d-%b-%Y","%Y/%d/%m","%Y-%m-%d","%d-m%-Y%")) |> 
  date()


# FEATURE ENGINEERING - Years of Service
tday <- today()
hr$yos <- ceiling((tday-hr$joindate)/365.25) |> 
  str_remove("days") |> 
  as.integer()

# REMOVING DUPLUCATES
hr[!duplicated(hr),] -> hr

mean(hr$age) ### AVERAGE AGE - 40 YEARS
mean(hr$salary) ### AVERAGE SALARY - 83,593.96/-

# Writing the clean data to a CSV file
# write_csv(x = hr, "HR.csv")
