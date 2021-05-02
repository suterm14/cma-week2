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
# Questions  
# individuals Number?
str(wildschwein_BE)  
table(wildschwein_BE$TierName)

# Rosa, Ruth und Sabi
# Sabi hat die meisten Zeitstempel, Ruth am wenigsten

#tiere untescheiden
wildschwein_BE <- group_by(wildschwein_BE,TierID)
wildschwein_BE<- mutate(wildschwein_BE,timelag = as.numeric(difftime(lead(DatetimeUTC),DatetimeUTC, units(secs))))
wildschwein_BE


# Wie lange wurden die drei Rehe Rosa, Ruth und Sabi getrackt?
wildschwein_BE %>%
  summarise(wildschwein_BE, min= min(DatetimeUTC))%>%
  summarise(wildschwein_BE, max= max(DatetimeUTC))
#2014-08-22 21:00:12 - 2015-07-27 11:00:14
#2014-08-22 21:00:12 - 2015-07-27 11:00:14


# gitb es Gaps / Unterbrüche?
# wie wurde getrackt? concuurrently oder sequenziell?
table(is.na(wildschwein_BE$timelag)) # keine fehlenden Daten, die drei NA ist der lead&lag Funktion geschuldet 

wildschwein_BE %>%
  group_by(TierID) %>%
  summarise(wildschwein_BE,mean = mean(timelag, na.rm = T), max = max(timelag, na.rm = T), min = min(timelag, na.rm=T))
# max= 60367, min= 12, mean=1408


wildschwein_BE$timelag
# die Tiere wurden nacheinander getrackt
View(table(wildschwein_BE$DatetimeUTC, wildschwein_BE$TierID))
# die Tiere wurden in überlappenden Zeitfenstern getrackt


# What is the temporal sampling interval between the location
# rund 900 sekunden, zwischen 5 Uhr morgens und 7 Uhr abends nur alle drei stunden ein standort.
View(wildschwein_BE)


# Visualtisation: Trackingdauer
install.packages("mapproj")
ggplot(wildschwein_BE, aes(DatetimeUTC, TierID, colour = TierID)) +
  geom_line() +
  theme(legend.position = "none")

# zeigt auf, dass Tier 018A zeitgleich mit 016A begonnen hat zu tracken, jedoch 018A länger gtrackt wurde. 
# 002A wurde am längsten getrackt. startete früher und endet zeitgleich mit 018A.

#Abbildung zwei verstehe ich nicht


 # Abbildung 3
ggplot(wildschwein_BE, aes(DatetimeUTC, timelag, colour= TierID)) +
  geom_line() +
  theme(legend.position = "none")

# tada die visualierung zeigt noch ganz andere Dinge auf :-)
# die Intervall sind am anfang abgesehen von wenigen Aussreissen recht konsitent. Dann werden die Intevalle sehr viel kürzer usw....
# keep in mind: immer visualisieren!!!




### Task 2: Deriving movement



