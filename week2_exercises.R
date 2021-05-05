## Task 0: Import your data ###

## Load the necessary libraries ################################################

library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times

## Import the downloaded csv ##################################################

wildschwein_BE <- read_delim("wildschwein_BE_2056.csv",",") # adjust path

wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE)



## Task 1: Getting an overview ###############################################
# Questions :

# How many individuals were tracked?

str(wildschwein_BE)  
table(wildschwein_BE$TierName)

# Rosa, Ruth und Sabi, # Sabi hat die meisten Zeitstempel, Ruth am wenigsten

#tiere untescheiden
wildschwein_BE <-wildschwein_BE%>%
  group_by(wildschwein_BE,TierID)%>% # nach Tieren unterscheiden
  mutate(wildschwein_BE,timelag = as.numeric(difftime(lead(DatetimeUTC),DatetimeUTC, units(secs))))
wildschwein_BE


# For how long were the individual tracked? Are there gaps?
wildschwein_BE %>%
  summarise(wildschwein_BE, min= min(DatetimeUTC))%>%
  summarise(wildschwein_BE, max= max(DatetimeUTC))
#2014-08-22 21:00:12 - 2015-07-27 11:00:14
# könnte man noch unterteilen nach TierID


# Were all individuals tracked concurrently or sequentially?

table(is.na(wildschwein_BE$timelag)) # keine fehlenden Daten, die drei NA ist der lead&lag Funktion geschuldet 

# What is the temporal sampling interval between the locations?Number of individuals?
wildschwein_BE %>%
  group_by(TierID) %>%
  summarise(wildschwein_BE,mean = mean(timelag, na.rm = T), max = max(timelag, na.rm = T), min = min(timelag, na.rm=T))
# max= 60367, min= 12, mean=1408


wildschwein_BE$timelag
# die Tiere wurden nacheinander getrackt
View(table(wildschwein_BE$DatetimeUTC, wildschwein_BE$TierID))
# die Tiere wurden in überlappenden Zeitfenstern getrackt

wildschwein_BE%>%
  group_by(TierID)%>%
  summarise(median(timelag,na.rm=TRUE))
# What is the temporal sampling interval between the location
# rund 900 sekunden, zwischen 5 Uhr morgens und 7 Uhr abends nur alle drei stunden ein standort.
View(wildschwein_BE)


# Visualtisation: Trackingdauer. They have been tracked concurently
install.packages("mapproj")
ggplot(wildschwein_BE, aes(DatetimeUTC, TierID, colour = TierID)) +
  geom_line() +
  theme(legend.position = "none")

# zeigt auf, dass Tier 018A zeitgleich mit 016A begonnen hat zu tracken, jedoch 018A länger gtrackt wurde. 
# 002A wurde am längsten getrackt. startete früher und endet zeitgleich mit 018A.



#Visualisation: sampling intervall


ggplot(data=wildschwein_BE)+
  geom_histogram(mapping=aes(x=timelag))+
  scale_y_log10()






 # Visualisation 3
ggplot(wildschwein_BE, aes(DatetimeUTC, timelag, colour= TierID)) +
  geom_line() +
  theme(legend.position = "none")

# tada die visualierung zeigt noch ganz andere Dinge auf :-)
# die Intervalle sind am anfang abgesehen von wenigen Aussreissen recht konsitent. Dann werden die Intevalle sehr viel kürzer usw....
# keep in mind: immer visualisieren!!!





### Task 2: Deriving movement parameters I: Speed  

# Euclidean distance als Spalte einfügen
wildschwein_BE <- wildschwein_BE %>% 
  group_by(TierID) %>%
  mutate(steplenght = sqrt((E -lead(E,1))^2 + (N - lead(N,))^2))




#Berechnung Speed aus Euclidean Speed :steplength /timelag 
#Überlegung: die steplengh ist in koordinate, timelag in sekunden. 

library(purrr)
map(3,6,9, function(x{}))



wildschwein_BE <- wildschwein_BE %>%
  mutate(speed=steplenght/timelag)



# Task 3: Cross-scale movement analysis
caro <- read_delim("caro60.csv",",")
caro <- st_as_sf(caro, coords = c("E", "N"), crs = 2056, remove = FALSE)


nrow(caro)  # 200

# Sequenzierungen erstellen (3,6,9)

?seq
?slice

#caro_3
caro_3<-slice(caro,seq(1,200,3))
nrow(caro_3)   #67

#caro_6
caro_6<-slice(caro,seq(1,200,6))
nrow(caro_6)   # 34

#caro_9
caro_9 <- slice(caro, seq(1,200,9))
nrow(caro_9)    # 23


# steplength und speed berechnen für caro_3, caro_6, caro_9
caro <- caro %>%
  mutate(timelag=as.integer(difftime(lead(DatetimeUTC), DatetimeUTC,units="secs"))) %>%
  mutate(steplenght = sqrt((E -lead(E,1))^2 + (N - lead(N,))^2)) %>%
  mutate(speed=steplenght/timelag)


caro_3<- caro_3 %>%
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC, units(secs)))) %>%
  mutate(steplenght = sqrt((E -lead(E,1))^2 + (N - lead(N,))^2)) %>%
  mutate(speed=steplenght/timelag)


caro_6<- caro_6 %>%
  mutate(timelag = as.numeric(difftime(lead(DatetimeUTC),DatetimeUTC, units(secs)))) %>%
  mutate(steplenght = sqrt((E -lead(E,1))^2 + (N - lead(N,))^2)) %>%
  mutate(speed=steplenght/timelag)

caro_6


# Visualisierung des Speeds in Vergleich mit der Originaldaten mit geom_path

ggplot(data=caro,mapping=aes(x=E, y=N))+
  geom_path(aes(color= "1 minute"))+
  geom_path(aes(color))




