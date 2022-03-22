library(ggplot2)
library(tidyverse)


## HIV and AIDs project


data = read.csv('HIV_AIDS_Diagnoses_by_Neighborhood__Sex__and_Race_Ethnicity.csv')
summary(data)
str(data)
##Cleaning the data 

sum(is.na(data$TOTAL.NUMBER.OF.CONCURRENT.HIV.AIDS.DIAGNOSE))

#changing all columns to their appropriate data types:
# * will be coerced into NA during this step
data$TOTAL.NUMBER.OF.HIV.DIAGNOSES = 
  strtoi(data$TOTAL.NUMBER.OF.HIV.DIAGNOSES)
data$HIV.DIAGNOSES.PER.100.000.POPULATION =
  as.double(data$HIV.DIAGNOSES.PER.100.000.POPULATION)
data$TOTAL.NUMBER.OF.CONCURRENT.HIV.AIDS.DIAGNOSE =
  strtoi(data$TOTAL.NUMBER.OF.CONCURRENT.HIV.AIDS.DIAGNOSE)
data$PROPORTION.OF.CONCURRENT.HIV.AIDS.DIAGNOSES.AMONG.ALL.HIV.DIAGNOSES =
  strtoi(data$PROPORTION.OF.CONCURRENT.HIV.AIDS.DIAGNOSES.AMONG.ALL.HIV.DIAGNOSES)
data$TOTAL.NUMBER.OF.AIDS.DIAGNOSES =
  strtoi(data$TOTAL.NUMBER.OF.AIDS.DIAGNOSES)
data$AIDS.DIAGNOSES.PER.100.000.POPULATION =
  as.double(data$AIDS.DIAGNOSES.PER.100.000.POPULATION)
data$TOTAL.NUMBER.OF.CONCURRENT.HIV.AIDS.DIAGNOSES =
  as.double(data$TOTAL.NUMBER.OF.CONCURRENT.HIV.AIDS.DIAGNOSES)

#filter NA's from Total number of hiv diagnoses
data = data %>%
  filter(!is.na(TOTAL.NUMBER.OF.HIV.DIAGNOSES))
#filter NA from race ethnicity 
data = data %>%
  filter(!is.na(RACE.ETHNICITY))
#filter NA from SEX
data = data %>%
  filter(!is.na(SEX))

#for AIDS diadnosis
#filter NA's from Total number of AIDS diagnoses
data = data %>%
  filter(!is.na(TOTAL.NUMBER.OF.AIDS.DIAGNOSES))



##Comparing the average diagnosis of HIV and AIDS in NYC neighberhoods

N = data %>% 
  group_by(Neighborhood..U.H.F.)%>%
  summarise(Average = mean(TOTAL.NUMBER.OF.HIV.DIAGNOSES, na.rm= TRUE))
head(arrange(N, desc(Average)), 3)


Aids = data %>% 
  group_by(Neighborhood..U.H.F.)%>%
  summarise(AvG = mean(TOTAL.NUMBER.OF.AIDS.DIAGNOSES, na.rm= TRUE))
head(arrange(Aids, desc(AvG)), 3)


## What is the % of the race that has the most HIV diagnoses and AIDS Diagnosis


#Total number hiv diagnoses:
#Dataset of ethnicity and it's total 
ethnicity= data %>% 
  group_by(RACE.ETHNICITY) %>%
  summarise(Total_HIV = sum(TOTAL.NUMBER.OF.HIV.DIAGNOSES))
head(arrange(ethnicity, desc(Total_HIV)))


## Using a graph, which gender has the highest average of HIV diagnosis? Male or Female?


Gender= data %>% 
  group_by(SEX) %>%
  summarise(Total = mean(TOTAL.NUMBER.OF.HIV.DIAGNOSES))

GenderAids = data %>% 
  group_by(SEX) %>%
  summarise(Total = mean(TOTAL.NUMBER.OF.AIDS.DIAGNOSES))

#Gender and total number of HIV and AIDS diagnoses using bar graph

BySexHIV = data %>% 
  group_by(SEX) %>%
  summarise(Total = sum(TOTAL.NUMBER.OF.HIV.DIAGNOSES))

BySexAIDS = data %>% 
  group_by(SEX) %>%
  summarise(Total = sum(TOTAL.NUMBER.OF.AIDS.DIAGNOSES))


BySexHIV %>%
  ggplot(aes(x = SEX, y = Total)) +
  geom_bar(stat="identity")

BySexAIDS %>%
  ggplot(aes(x = SEX, y = Total)) +
  geom_bar(stat="identity")

BySexHIV$TotalAIDS = BySexAIDS$Total

ggplot(data=BySexHIV, aes(x=SEX)) +
  geom_bar(aes(y=Total), stat="identity", fill='lightblue') +
  geom_bar(aes(y=TotalAIDS), stat="identity", fill='pink')

#ethnicity and total number of HIV and AIDS diagnoses using bar graph
ByEthnicityHIV = data %>% 
  group_by(RACE.ETHNICITY) %>%
  summarise(Total = sum(TOTAL.NUMBER.OF.HIV.DIAGNOSES))

ByEthnicityIDS = data %>% 
  group_by(RACE.ETHNICITY) %>%
  summarise(Total = sum(TOTAL.NUMBER.OF.AIDS.DIAGNOSES))


ByEthnicityHIV %>%
  ggplot(aes(x = RACE.ETHNICITY, y = Total)) +
  geom_bar(stat="identity")

ByEthnicityIDS %>%
  ggplot(aes(x = RACE.ETHNICITY, y = Total)) +
  geom_bar(stat="identity")

ByEthnicityHIV$TotalAIDS = ByEthnicityIDS$Total

ggplot(data=ByEthnicityHIV, aes(x=RACE.ETHNICITY)) +
  geom_bar(aes(y=Total), stat="identity", fill='lightblue') +
  geom_bar(aes(y=TotalAIDS), stat="identity", fill='pink')

str(ByEthnicityHIV)

#unselected the All, unknown, and native american categories
ByEthnicityHIV_filt = ByEthnicityHIV[c(2,3,4,5,8),c("RACE.ETHNICITY","Total","TotalAIDS")]

ggplot(data=ByEthnicityHIV_filt, aes(x=RACE.ETHNICITY)) +
  geom_bar(aes(y=Total), stat="identity", fill='lightblue') +
  geom_bar(aes(y=TotalAIDS), stat="identity", fill='pink')

#solutions - condoms availability etc - FINAL graph
#The map dataset 
solution_map = read.csv('NYC_Condom_Availability_Program_-_HIV_condom_distribution_locations.csv')
str(solution_map)
glimpse(solution_map)


loc = "DataSet/ZIP_CODE_040114"
NYCmap = st_read(dsn = loc, layer = "ZIP_CODE_040114")
glimpse(NYCmap)
plot(st_geometry(NYCmap))
summary(solution_map)
str(NYCmap)
str(solution_map)

(serviceCount = solution_map %>%
    group_by(Zipcode) %>%
    count())

(serviceCount = serviceCount %>%
    rename(ZIPCODE = Zipcode))
glimpse(serviceCount)

NYCmap$ZIPCODE = 
  strtoi(NYCmap$ZIPCODE)

(NYCZipServiceCount = left_join(x = NYCmap, y=serviceCount))

plot(NYCZipServiceCount["n"])