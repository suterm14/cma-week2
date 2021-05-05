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
#  Rosa  Ruth  Sabi 
#14364 14136 22746 


#Tiere unterscheiden

wildschwein_BE <- group_by(wildschwein_BE,TierID)
wildschwein_BE<- mutate(wildschwein_BE,timelag = as.numeric(difftime(lead(DatetimeUTC),DatetimeUTC, units(secs))))

#piping
wildschwein_BE <- wildschwein_BE%>%
  group_by(TierID)%>% # nach Tieren unterscheiden
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC, units(secs))))
wildschwein_BE



# For how long were the individual tracked? Are there gaps?

wildschwein_BE %>%
  summarise(wildschwein_BE, min= min(DatetimeUTC))%>%
  summarise(wildschwein_BE, max= max(DatetimeUTC))
#2014-08-22 21:00:12 - 2015-07-27 11:00:14
# könnte man noch unterteilen nach TierID




# Were all individuals tracked concurrently or sequentially?

table(is.na(wildschwein_BE$timelag)) # keine fehlenden Daten, die drei NA ist der lead&lag Funktion geschuldet 
View(table(wildschwein_BE$DatetimeUTC, wildschwein_BE$TierID))
# die Tiere wurden in überlappenden Zeitfenstern getrackt




# What is the temporal sampling interval between the locations?

wildschwein_BE %>%
  group_by(TierID) %>%
  summarise(wildschwein_BE,mean = mean(timelag, na.rm = T), max = max(timelag, na.rm = T), min = min(timelag, na.rm=T))
# max= 60367, min= 12, mean=1408


wildschwein_BE%>%
  group_by(TierID)%>%
  summarise(median(timelag,na.rm=TRUE))   # samling intervall

# rund 900 sekunden, zwischen 5 Uhr morgens und 7 Uhr abends nur alle drei stunden ein standort.




# Visualtisation: Trackingdauer


install.packages("mapproj")
ggplot(wildschwein_BE, aes(DatetimeUTC, TierID, colour = TierID)) +
  geom_line() +
  theme(legend.position = "none")

#They have been tracked concurently
# zeigt auf, dass Tier 018A zeitgleich mit 016A begonnen hat zu tracken, jedoch 018A länger gtrackt wurde. 
# 002A wurde am längsten getrackt. startete früher und endet zeitgleich mit 018A.




# Visualisation: Sampling intervall

ggplot(data=wildschwein_BE)+
  geom_histogram(mapping=aes(x=timelag),binwidth = 10)+
  scale_y_log10()+
  xlim(0,15000)
  


# Visualisation 3
ggplot(wildschwein_BE, aes(DatetimeUTC, timelag, colour= TierID)) +
  geom_line()+
  theme(legend.position = "right")+
  ylim(0,20000)
  
# sieht noch nicht so rosig aus. wie kann ich die x-Achse skalieren bei Zeitdaten? xlim()?

  
 





### Task 2: Deriving movement parameters I: Speed  


# Euclidean distance als Spalte einfügen

wildschwein_BE <- wildschwein_BE %>% 
  group_by(TierID) %>%
  mutate(steplenght = sqrt((E -lead(E,1))^2 + (N - lead(N,))^2))


#Berechnung Speed aus Euclidean Speed :steplength /timelag . 

wildschwein_BE <- wildschwein_BE %>%
  mutate(speed=steplenght/timelag)

View(wildschwein_BE)   # speed [m/s]








# Task 3: Cross-scale movement analysis
# irgendwann mal noch library(purrr) ansehen

#D Daten einlesen
caro <- read_delim("caro60.csv",",")
caro <- st_as_sf(caro, coords = c("E", "N"), crs = 2056, remove = FALSE)

#Anzahl Reihen
nrow(caro)  # 200


# Sequenzierungen erstellen (3,6,9)

?seq
?slice

caro_3 <-slice(caro,seq(1,200,3))
caro_6 <-slice(caro,seq(1,200,6))
caro_9 <- slice(caro, seq(1,200,9))

nrow(caro_3)   #67
nrow(caro_6)   # 34
nrow(caro_9)    # 23



# steplength und speed berechnen für caro_3, caro_6, caro_9
caro <- caro %>%
  mutate(timelag=as.integer(difftime(lead(DatetimeUTC), DatetimeUTC,units="secs"))) %>%
  mutate(steplenght = sqrt((E -lead(E,1))^2 + (N - lead(N,))^2)) %>%
  mutate(speed=steplenght/timelag)

View(caro)

caro_3<- caro_3 %>%
  mutate(timelag=as.integer(difftime(lead(DatetimeUTC), DatetimeUTC,units="secs"))) %>%
  mutate(steplenght = sqrt((E -lead(E,1))^2 + (N - lead(N,))^2)) %>%
  mutate(speed=steplenght/timelag)

View(caro_3)


caro_6<- caro_6 %>%
  mutate(timelag=as.integer(difftime(lead(DatetimeUTC), DatetimeUTC,units="secs"))) %>%
  mutate(steplenght = sqrt((E -lead(E,1))^2 + (N - lead(N,))^2)) %>%
  mutate(speed=steplenght/timelag)


caro_9<- caro_9 %>%
  mutate(timelag=as.integer(difftime(lead(DatetimeUTC), DatetimeUTC,units="secs"))) %>%
  mutate(steplenght = sqrt((E -lead(E,1))^2 + (N - lead(N,))^2)) %>%
  mutate(speed=steplenght/timelag)



# Visualisierung des Speeds in Vergleich mit der Originaldaten mit geom_path
library(mapproj)
ggplot(data=caro,mapping=aes(x=E, y=N))+
  geom_path(data=caro,mapping=aes(col= "1 minute"))+    # geom_path, damit es auch zeitlich nacheinander visualisiert wird
  geom_path(data=caro_3, mapping=aes(col="3 minute"))+   
  geom_point(ata=caro,mapping=aes(col= "1 minute"))+    #geom_point um die jeweiligen standorte zur Zeit des Zeitstempes anzuzeigen
  geom_point(data=caro_3, mapping=aes(col="3 minute"))+
  labs(col="Trajectory", title = "Comparing original - with 3 minute-resampled data")+
  theme(title=element_text(size=12))
  
  
  
ggplot(data=caro,mapping=aes(x=E, y=N))+
  geom_path(data=caro,mapping=aes(col= "1 minute"))+    # geom_path, damit es auch zeitlich nacheinander visualisiert wird
  geom_path(data=caro_6, mapping=aes(col="6 minute"))+   
  geom_point(ata=caro,mapping=aes(col= "1 minute"))+    #geom_point um die jeweiligen standorte zur Zeit des Zeitstempes anzuzeigen
  geom_point(data=caro_6, mapping=aes(col="6 minute"))+
  labs(col="Trajectory", title = "Comparing original - with 6 minute-resampled data")+
  theme(title=element_text(size=12)) 



ggplot(data=caro,mapping=aes(x=E, y=N))+
  geom_path(data=caro,mapping=aes(col= "1 minute"))+    # geom_path, damit es auch zeitlich nacheinander visualisiert wird
  geom_path(data=caro_9, mapping=aes(col="9 minute"))+   
  geom_point(ata=caro,mapping=aes(col= "1 minute"))+    #geom_point um die jeweiligen standorte zur Zeit des Zeitstempes anzuzeigen
  geom_point(data=caro_9, mapping=aes(col="9 minute"))+
  labs(col="Trajectory", title = "Comparing original - with 9 minute-resampled data")+
  theme(title=element_text(size=12)) 



ggplot(data=caro, mapping=aes(x=DatetimeUTC,y=speed))+
  geom_line(data=caro, mapping=aes(col="1 minute"))+
  geom_line(data=caro_3, mapping=aes(col="3 minute"))+
  geom_line(data=caro_6, mapping=aes(col="6 minute"))+
  geom_line(data=caro_9, mapping=aes(col="9 minute"))+
  labs(col= "trajectories", title = ("Comparing derived speed at different samling intervals"))+
  ylab("Speed (m/s)")+
  xlab("Time")







### Task 4: Deriving movement parameters II: Rolling window functions ###
# smoothen the derived parameters using a moving window function

# Package laden
library(zoo)

?zoo   #s the creator for an S3 class of indexed totally ordered observations which includes irregular time series.


example <- rnorm(10)
rollmean(example,k = 3,fill = NA,align = "left")
##  [1]  0.93634335  0.31709038  0.02370048  0.67869801  0.73369105  0.50401344
##  [7] -0.56144365 -0.56902598          NA          NA
rollmean(example,k = 4,fill = NA,align = "left")
##  [1]  0.6775521  0.2045005  0.5848215  0.5255629  0.3446928  0.1459635
##  [7] -0.4102301         NA         NA         NA
  

rollmean(caro$speed, k =2, fill=NA, align = "left")

caro_roll <- caro
caro_roll$k1 <- rollmean(caro_roll$speed,k =1, fill=NA, align = "left")
caro_roll$k2 <- rollmean(caro_roll$speed,k =2, fill=NA, align = "left")
caro_roll$k4 <- rollmean(caro_roll$speed,k =4, fill=NA, align = "left")
caro_roll$k8<- rollmean(caro_roll$speed,k =8, fill=NA, align = "left")
caro_roll$k16 <- rollmean(caro_roll$speed,k =16, fill=NA, align = "left")



ggplot(data= caro_roll, mapping= aes(x=DatetimeUTC, y=speed))+
        geom_line(data= caro_roll, mapping= aes(y=k1, col="k1"))+   # mit x=k1 ergibt das nur eine gerade Linie. wieso?
        geom_line(data= caro_roll, mapping= aes(y=k2, col="k2"))+
        geom_line(data= caro_roll, mapping= aes(y=k4, col="k4"))+
        geom_line(data= caro_roll, mapping= aes(y=k8, col="k8"))+
        geom_line(data= caro_roll, mapping= aes(y=k16, col="k16"))          
