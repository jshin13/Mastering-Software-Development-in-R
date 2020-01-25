library(dplyr)
library(tidyr)
library(readr)

daily14 <- read_csv("daily_SPEC_2014.csv.bz2")

options(dplyr.width=Inf)
options("max.print" = 1000)

head(daily14)
names(daily14)

prob1 <- daily14 %>%
  select("State Name", "Parameter Name", "Arithmetic Mean") %>%
  filter(grepl("Wisconsin", daily14$`State Name`), grepl("Bromine PM2.5 LC", daily14$`Parameter Name`)) %>% 
  print()

mean(prob1$`Arithmetic Mean`, na.rm=TRUE) 

#Question 1 ans: 0.003960482

prob2 <- daily14 %>%
  select("State Name", "Parameter Name", "Arithmetic Mean") %>%
  group_by(`Parameter Name`, `Arithmetic Mean`) %>%
  summarize(N = n()) %>%
  arrange(desc(`Arithmetic Mean`)) %>%
  print() 

#Question 2 ans: OC CSN Unadjusted PM2.5 LC TOT

prob3 <- daily14 %>%
  select("Parameter Name", "State Code", "County Code", "Site Num", "Arithmetic Mean") %>%
  filter(daily14$`Parameter Name` == "Sulfate PM2.5 LC") %>%
  arrange(desc(`Arithmetic Mean`)) %>%
  print() 

#Question 3 ans: 39/81/17 (highest in the answer choices)

prob4_1 <- daily14 %>%
  select("State Name", "Parameter Name", "Arithmetic Mean") %>%
  filter(`State Name` == "California" & `Parameter Name` == "EC PM2.5 LC TOR") %>%
  print()

prob4_2 <- daily14 %>%
  select("State Name", "Parameter Name", "Arithmetic Mean") %>%
  filter(`State Name` == "Arizona" & `Parameter Name` == "EC PM2.5 LC TOR") %>%
  print()

mean(prob4_1$`Arithmetic Mean`) - mean(prob4_2$`Arithmetic Mean`) 

#Question 4 ans: 0.01856696

prob5 <- daily14 %>%
  select("Longitude", "Parameter Name", "Arithmetic Mean") %>%
  filter(`Longitude` < -100 & `Parameter Name` == "OC PM2.5 LC TOR") %>%
  print()

median(prob5$`Arithmetic Mean`, na.rm=TRUE)

#Question 5 ans: 0.43

library(readxl)

aqs <- read_xlsx("aqs_sites.xlsx")

names(aqs)

prob6 <- aqs %>%
  select(`Land Use`, `Location Setting`) %>%
  filter(`Land Use` == "RESIDENTIAL" & `Location Setting` == "SUBURBAN") %>%
  nrow() %>%
  print()

#Question 6 ans: 3527

prob7_loc <- aqs %>%
  select(`Land Use`, `Location Setting`, `Longitude`) %>%
  filter(`Land Use` == "RESIDENTIAL" & `Location Setting` == "SUBURBAN" & `Longitude` >= -100) %>%
  print()

prob7_num <- daily14 %>%
  select(`Parameter Name`, `Arithmetic Mean`, `Longitude`) %>%
  filter(`Parameter Name` == "EC PM2.5 LC TOR" & `Longitude` >= -100) %>%
  print()

prob7_merg <- merge(prob7_loc, prob7_num, by = c("Longitude")) %>%
  print()

median(prob7_merg$`Arithmetic Mean`, na.rm=TRUE)

#Question 7 ans: 0.61

prob8_loc <- aqs %>%
  select(`Land Use`, `Longitude`) %>%
  filter(`Land Use` == "COMMERCIAL") %>%
  print()

prob8_num <- daily14 %>%
  select(`Parameter Name`, `Arithmetic Mean`, `Longitude`, `Date Local`) %>%
  filter(`Parameter Name` == "Sulfate PM2.5 LC") %>%
  print()

prob8_merg <- merge(prob8_loc, prob8_num, by = c("Longitude")) %>%
  arrange(desc(`Arithmetic Mean`)) %>%
  print()

#Question 8 ans: 2014-06-10

prob9_loc <- aqs %>%
  select(`State Code`, `County Code`, `Site Number`, `Longitude`) %>%
  filter(`State Code` == 6, `County Code` == 65, `Site Number` == 8001) %>%
  print()

prob9_num <- daily14 %>%
  select(`Longitude`, `Parameter Name`, `Arithmetic Mean`, `Longitude`, `Date Local`) %>%
  filter((`Parameter Name` == "Sulfate PM2.5 LC" | `Parameter Name` ==  "Total Nitrate PM2.5 LC")) %>%
  print()

prob9_merg <- merge(prob9_loc, prob9_num, by = c("Longitude")) %>%
  select(`Parameter Name`, `Date Local` ,`Arithmetic Mean`) %>%
  arrange(desc(`Date Local`)) %>%
  group_by(`Date Local`, `Parameter Name`) %>%
  summarize(`total mean` = mean(`Arithmetic Mean`)) %>%
  summarize(`total sum` = sum(`total mean`)) %>%
  filter(`total sum` > 10) %>%
  arrange(desc(`total sum`)) %>%
  print()

length(prob9_merg$`total sum`)

#Question 9 ans: 11

prob10_Sul <- daily14 %>%
  select(`State Code`, `County Code`, `Site Num`, `Parameter Name`, `Arithmetic Mean`, `Date Local`) %>%
  filter(`Parameter Name` == "Sulfate PM2.5 LC") %>%
  group_by(`State Code`, `County Code`, `Site Num`,`Date Local`) %>%
  arrange(desc(`Date Local`)) %>%
  summarize(`total mean` = mean(`Arithmetic Mean`)) %>%
  print()

prob10_Nit <- daily14 %>%
  select(`State Code`, `County Code`, `Site Num`, `Parameter Name`, `Arithmetic Mean`, `Date Local`) %>%
  filter(`Parameter Name` ==  "Total Nitrate PM2.5 LC") %>%
  group_by(`State Code`, `County Code`, `Site Num`,`Date Local`) %>%
  arrange(desc(`Date Local`)) %>%
  summarize(`total mean` = mean(`Arithmetic Mean`)) %>%
  print()

prob10_merg <- merge(prob10_Sul, prob10_Nit, by = c("State Code", "County Code", "Site Num", "Date Local")) %>%
  group_by(`State Code`, `County Code`, `Site Num`) %>%
  summarize(`total corr` = cor(`total mean.x`, `total mean.y`)) %>%
  arrange(desc(`total corr`)) %>%
  print()

#Question 10 ans: 02           090           0035              0.898
