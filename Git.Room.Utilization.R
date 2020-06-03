#############################################################################
#
#   Room Utilization for University
#   05/18/2019
#   Hunter Martin
#
#############################################################################
library(lubridate)
library(tidyr)
library(readxl)
library(tidyverse)
library(dplyr)
library(reshape)


# removed for privacy reasons
setwd("PATH/TO/DATA")

Room.tech <- read_excel("PATH/TO/DATA",sheet = "Inventory")

Room.tech <- Room.tech %>%
  select(Room, `Room type`,`Room level`)

Room.tech$Room<-gsub("\\(.*","",Room.tech$Room)


Room.tech$Room <-ifelse(Room.tech$Room =="CGC 031", "CGC 31",Room.tech$Room) 
Room.tech$Room <-ifelse(Room.tech$Room =="CGC 040", "CGC 40",Room.tech$Room) 
Room.tech$Room <-ifelse(Room.tech$Room =="CGC 045", "CGC 45",Room.tech$Room) 
Room.tech$Room <-ifelse(Room.tech$Room =="DSB 204D", "DSB 204",Room.tech$Room) 
Room.tech <- Room.tech[-c(92),]
Room.tech$Room <-ifelse(Room.tech$Room =="DSB 206A", "DSB 206",Room.tech$Room) 
Room.tech$Room <-ifelse(Room.tech$Room =="NH 040", "NH 40",Room.tech$Room) 
Room.tech$Room <-ifelse(Room.tech$Room =="CGC 267G?", "CGC 267",Room.tech$Room)
Room.tech$Room <-ifelse(Room.tech$Room =="CGC 267G", "CGC 267",Room.tech$Room) 
Room.tech$Room <-ifelse(Room.tech$Room =="RCC 418A", "RCC 418",Room.tech$Room)


rooms<- read.csv("ITSBAT - Course Section Locations.csv", header = TRUE, na.strings=c("","NA"))
colSums(is.na(rooms))

index1<-which(is.na(rooms$CSM..START..TIME))
rooms<-rooms[-index1,]

colSums(is.na(rooms))

#After deleting rows with missing times, delete rows with missing building names and room numbers 

index2<-which(is.na(rooms$CSM..BLDG))
rooms<-rooms[-index2,]

#If the BR PK field is empty, that means the course did not 
#have a set location on campus (could be an independent study 
#course for instance) or was taught off campus.
#Therefore, rows removed  with empty BR PK
colSums(is.na(rooms))

#Ender reccomended to remove the sched capacity column

rooms$Sched.Capacity <- NULL

#Ender reccommended to remove courses under "Section Name" like BUSN-XXXX-1 since 
#they were never actually used 

rooms[-grep("XXXX",rooms$Section.Name),]

unique(rooms$CSM..FREQUENCY)
#All are labled W, therefore no use for this column
rooms$CSM..FREQUENCY <- NULL

#We are not worried about meeting ID's
rooms$COURSE..SEC..MEETING..ID<- NULL
rooms$BR.PK<- NULL

unique(rooms$Current.Status)

# Under Current Status, remove courses which were cancelled or pending

index3<- which(rooms$Current.Status=="C" | rooms$Current.Status=="P")
rooms<-rooms[-index3,]
unique(rooms$Current.Status)
rooms[rooms$Current.Status=="C"]
#Now that all data is Active, remove the current status column
rooms$Current.Status <- NULL

#Unite Building and Room columns so that the strings match on the spaced format for the 
#Room technology Data

Bldg.Rm<-unite(rooms,'Building.Room',CSM..BLDG,CSM..ROOM, sep = " ")

rooms<-cbind(rooms,Bldg.Rm$Building.Room, drop = FALSE)
#Rename for ease of use
colnames(rooms)[colnames(rooms)=="Bldg.Rm$Building.Room"]<- "Room"

#Join Room.tech to rooms
#Result is not joining on Room with exact match
rooms<-left_join(rooms,Room.tech, by="Room", drop = TRUE)
rooms$drop<- NULL

#CSI 437 is Classroom, level = L5


#Fill in levels on the ones that didn't match

#CSI 437
rooms$`Room type`[rooms$Room == "CSI 437"]<- 'Classroom'
rooms$`Room level`[rooms$Room == "CSI 437"]<- 'L5' 
#COAT 237
rooms$`Room type`[rooms$Room == "COAT 237"]<- 'Conference'
rooms$`Room level`[rooms$Room == "CSI 437"]<- 'L3' 

which(is.na(rooms$`Room type`))

#Remove rooms with no given tech values

rooms<- rooms[!is.na(rooms$`Room type`), ]
which(is.na(rooms$`Room type`))

rooms<- rooms[!is.na(rooms$`Room level`), ]
which(is.na(rooms$`Room level`))
#The room tech data is cleaned.

#Create Day field showing Each Course and what day it falls on
#Melt did not work
#rooms<-melt(rooms, id=c("CSM..MONDAY","CSM..TUESDAY","CSM..WEDNESDAY","CSM..THURSDAY","CSM..FRIDAY"))

dayc<-vector()

for(i in 1:nrow(rooms))
{
  d<-""
  if (!is.na(rooms[i,5]=='Y'))
{
  d<-paste(d,"Monday")
}
  if (!is.na(rooms[i,6]=='Y'))
{
  d<-paste(d," Tuesday")
}
  if (!is.na(rooms[i,7]=='Y'))
{
  d<-paste(d," Wednesday")
}
  if (!is.na(rooms[i,8]=='Y'))
{
  d<-paste(d," Thursday")
}
  if (!is.na(rooms[i,9]=='Y'))
{
  d<-paste(d," Friday")
}
dayc<-append(dayc, d)
}


rooms$day=dayc

rooms<- rooms %>% 
  mutate(day = strsplit(as.character(day), " "), by=day) %>% 
  unnest(day)

#Works, now remove empty day columns

rooms<-rooms[!rooms$day=="",]

#Create Duration field showing minutes elapsed between end and start times

rooms$CSM..START..TIME<- as.POSIXct(rooms$CSM..START..TIME, format = "%I:%M:%S %p")
rooms$CSM..END..TIME<- as.POSIXct(rooms$CSM..END..TIME,format = "%I:%M:%S %p")
#Strip Incorrect Dates
rooms$CSM..START..TIME<- strftime(rooms$CSM..START..TIME, format = "%H:%M:%S")
rooms$CSM..END..TIME<- strftime(rooms$CSM..END..TIME, format = "%H:%M:%S")
typeof(rooms$CSM..START..TIME)
#Bind Duration to rooms
Duration<- as.POSIXct(rooms$CSM..END..TIME,format = "%H:%M:%S") - as.POSIXct(rooms$CSM..START..TIME, format = "%H:%M:%S")
rooms<- cbind(rooms,Duration)

unique(Duration)

#Add column with total minutes possible in a day (885)
MinutesInDay<- rep(885,length(19854))
rooms<-cbind(rooms,MinutesInDay)


write.csv(rooms,file="clenaed course selection data.csv")

rooms$CSM..END..TIME[rev(order(rooms$CSM..END..TIME))]

#Courses run from 07:00:00 to 21:45:00
#Total time availible is 14 hours and 45 minutes, or 885 min
