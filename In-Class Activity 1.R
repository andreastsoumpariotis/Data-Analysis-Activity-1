
##### Exercise 1. Class survey data #####

class = read.csv("Documents/2019-20/Fall/STA 9750 Basic Software Tools/In-Class Activities/survey9750.csv")
class

# What percentage of students had used R before?
used_R_before = (class$Have.you.used.R.before.)
used_R_before
cond = class$Have.you.used.R.before. == "Yes"
round(100*sum(cond)/51, 2)
# Ans = 58.82

# What percentage of students are from NYC?
from_NYC = (class$Are.you.from.NYC.)
from_NYC
cond2 = class$Are.you.from.NYC. == "Yes"
round(100*sum(cond2)/51, 2)
# Ans = 49.02

# What percentage of students from Queens support the Mets?
qm = prop.table(table(class$If.you.are.from.NYC..where.from. == "Queens", class$Which.baseball.team.do.you.prefer. == "Mets"), 1)
round(100*qm,2)
# Ans = 11.11

# What percentage of students from the Bronx support the Yankees?
by = prop.table(table(class$If.you.are.from.NYC..where.from. == "The Bronx", class$Which.baseball.team.do.you.prefer. == "Yankees"), 1)
round(100*by,2)
# Ans = 33.33

# What percentage of students speak only one language?
oneLanguage = (class$How.many.languages.do.you.speak. == "1")
justEnglish = (class$How.many.languages.do.you.speak. == "Just English")
one = class$How.many.languages.do.you.speak. == "one"
oneLanguage
justEnglish
one
allones = sum(oneLanguage)+sum(justEnglish)+sum(one)
round(100*allones/51,2)
# Ans = 17.65

# What percentage of respondents "care" about at least one of the NYC baseball/basketball/football teams?
sports = (class$Which.basketball.team.do.you.prefer. =="Knicks") |
  (class$Which.basketball.team.do.you.prefer. =="Nets") |
  (class$Which.football.team.do.you.prefer. == "Giants") |
  (class$Which.football.team.do.you.prefer. == "Jets") |
  (class$Which.baseball.team.do.you.prefer. == "Mets") |
  (class$Which.baseball.team.do.you.prefer. == "Yankees")
sum = sum(sports == TRUE)
sum
round(100*sum/51,2)
# Ans = 45.10

##### Exercise 2. NBA players 13-14 #####

nba = read.csv("Documents/2019-20/Fall/STA 9750 Basic Software Tools/In-Class Activities/nba_ht_wt.csv")
nba

# What is the maximum BMI in the sample? (the value, not the player)
weight = (nba$Weight)/2.20462442
height = (nba$Height)/39.3700787
tab = data.frame(height,weight)
tab
nba$BMI = (tab$weight/tab$height)/tab$height
round(sort(nba$BMI),2)
# Ans = 30.97

# Who has the highest BMI?
nba %>% arrange(desc(BMI)) %>% select(Player, BMI)
# Ans = Glen Davis

# What percentage of players have a BMI over 25, which is considered "overweight"?
overweight = nba$BMI >= "25"
overweight
fat = sum(overweight == TRUE)
fat
round(100*fat/505,2)
# Ans = 45.35

# Which position has the highest average height?
heighest_average_height = nba %>% select(Pos, Height)
heighest_average_height
mean_center = with(heighest_average_height, mean(Height[Pos == "C"]))
mean_center #83.18478
mean_forward = with(heighest_average_height, mean(Height[Pos == "F"]))
mean_forward #80.57346
mean_guard = with(heighest_average_height, mean(Height[Pos == "G"]))
mean_guard #75.61386
# Ans = Center

# Which position has the highest average BMI?
heighest_average_BMI = nba %>% select(Pos, BMI)
heighest_average_BMI
mean_center_BMI = with(heighestt_average_BMI, mean(BMI[Pos == "C"]))
mean_center_BMI #25.53904
mean_forward_BMI = with(heighest_average_BMI, mean(BMI[Pos == "F"]))
mean_forward_BMI #24.96087
mean_guard_BMI = with(heighest_average_BMI, mean(BMI[Pos == "G"]))
mean_guard_BMI #24.1548
# Ans = Center

# What percentage of players are 6 feet or taller?
tall = nba$Height >= 72
tall
t = sum(tall == TRUE)
t
round(100*t/505,2)
# Ans = 98.81

# What is the percentage of players whose weight is greater or equal 
# to 100kg?
nba$kgweight = (nba$Weight)/2.20462442
nba$kgweight
kg = nba$kgweight >= 100
kg
k = sum(kg == TRUE)
k
round(100*k/505,2)
# Ans = 48.51

##### Exercise 3. Flights data #####

library(nycflights13)

glimpse(airlines)
glimpse(airports)
glimpse(planes)
glimpse(weather)
glimpse(flights)

# What is the destination of the longest flight 
# (in terms of distance, not time)? 
# [Use the name in the airports database]
?flights
?airports
flights %>% select(origin, dest, flight, distance) %>%
  arrange(desc(distance))

airports %>% select(faa, name) %>%
  filter(name == "Honolulu Intl")
# Ans = Honolulu Intl

# Which airline has the highest average distance flights? 
# [Use the airline name in the airlines database]
flights %>% select(carrier, distance) %>%
  arrange(desc(distance))

airlines %>% select(carrier, name) %>%
  filter(carrier == "HA")
# Ans = Hawaiian Airlines Inc.

# Which day had the highest average delays? (in mm/dd/yyyy)
flights %>% group_by(year, month, day) %>% 
  summarize(avgDelay = mean(arr_delay, na.rm = T)) %>% 
  arrange(desc(avgDelay))
# Ans = 03/08/2013

# Which airport in the airports database has the highest altitude? 
# [Use the full name in the airports database]
?airports
airports %>% select(name, alt) %>%
  arrange(desc(alt))
# Ans = Telluride

# Which month had the highest average precipitation?
?weather

# January
january_weather = weather %>% select(year, month, day, hour, precip) %>%
  filter(month == "1") %>%
  arrange(desc(precip))
january_weather
sum(january_weather$precip)
# Ans = 8.5

# February
february_weather = weather %>% select(year, month, day, hour, precip) %>%
  filter(month == "2") %>%
  arrange(desc(precip))
february_weather
sum(february_weather$precip)
# Ans = 9.72

# March
march_weather = weather %>% select(year, month, day, hour, precip) %>%
  filter(month == "3") %>%
  arrange(desc(precip))
march_weather
sum(march_weather$precip)
# Ans = 7.66

# April
april_weather = weather %>% select(year, month, day, hour, precip) %>%
  filter(month == "4") %>%
  arrange(desc(precip))
april_weather
sum(april_weather$precip)
# Ans = 4.4

# May
may_weather = weather %>% select(year, month, day, hour, precip) %>%
  filter(month == "5") %>%
  arrange(desc(precip))
may_weather
sum(may_weather$precip)
# Ans = 13.71

# June
june_weather = weather %>% select(year, month, day, hour, precip) %>%
  filter(month == "6") %>%
  arrange(desc(precip))
june_weather
sum(june_weather$precip)
# Ans = 24.84

# July
july_weather = weather %>% select(year, month, day, hour, precip) %>%
  filter(month == "7") %>%
  arrange(desc(precip))
july_weather
sum(july_weather$precip)
# Ans = 8.8

# August
august_weather = weather %>% select(year, month, day, hour, precip) %>%
  filter(month == "8") %>%
  arrange(desc(precip))
august_weather
sum(august_weather$precip)
# Ans = 9.27

# September
september_weather = weather %>% select(year, month, day, hour, precip) %>%
  filter(month == "9") %>%
  arrange(desc(precip))
september_weather
sum(september_weather$precip)
# Ans = 6.75

# October
october_weather = weather %>% select(year, month, day, hour, precip) %>%
  filter(month == "10") %>%
  arrange(desc(precip))
october_weather
sum(october_weather$precip)
# Ans = 1.25

# November
november_weather = weather %>% select(year, month, day, hour, precip) %>%
  filter(month == "11") %>%
  arrange(desc(precip))
november_weather
sum(november_weather$precip)
# Ans = 8.3

# December
december_weather = weather %>% select(year, month, day, hour, precip) %>%
  filter(month == "12") %>%
  arrange(desc(precip))
december_weather
sum(december_weather$precip)
# Ans = 13.51

# Month with Highest Average Precipitation: June with 24.84in
