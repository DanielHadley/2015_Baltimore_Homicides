#### Created by DH  ####

setwd("C:/Users/dhadley/Documents/GitHub/2015_Baltimore_Homicides/")


library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggmap)
library(lubridate)


d <- read.csv("./Baltimore.csv")


#### This is how I retreived the data ####
# dBRaw <- read.csv(url("https://data.baltimorecity.gov/resource/wsfq-mvij.csv?$limit=20000&description=HOMICIDE"))
# 
# write.csv(dBRaw, "./Baltimore.csv")



#### Clean data ####

# dates
today <- Sys.Date()
yesterday <- today - 1
sixtyDaysAgo <- today - 60
YearAgo <- today - 365
TwoYearsAgo <- YearAgo - 365
ThreeYearsAgo <- TwoYearsAgo - 365
FourYearsAgo <- ThreeYearsAgo - 365


# Date
d$Date <- as.Date(d$CrimeDate,"%m/%d/%Y") # Tell R it's a date
d$Month <- format(d$Date, format='%m') # Break it into month, day, year...
d$Day <- format(d$Date, format='%d')
d$Year <- format(d$Date, format='%Y')
d$Month <- as.numeric(as.character(d$Month)) # Transform month to numeric for ifelse
# There are more nuanced ways of dividing seasons, but I prefer 4 even periods:
d$Season <- ifelse((d$Month >= 3) & (d$Month <= 5), "Spring", 
                   ifelse((d$Month >= 6) & (d$Month <= 8), "Summer",
                          ifelse((d$Month >= 9) & (d$Month <= 11), "Fall", "Winter")))

d$MonthYear <-  format(d$Date, "%y %m")

d$MonthTwo <- as.Date(cut(d$Date,
                          breaks = "month"))

d$Date <- as.Date(d$Date, "%m/%d/%Y")
d$Year <- format(d$Date, '%Y')
d$Year.Month <- format(d$Date, '%Y-%m')
d$Month <- format(d$Date, '%m')
d$YearDay <- yday(d$Date)

d$DaysAgo <- difftime(d$Date, today, units = "days")



d <- d %>%
  separate(Location.1, c("y", "x"), ",", extra = "merge")

d$x = gsub("\\)", "", d$x)
d$y = gsub("\\(", "", d$y)
d$x = as.numeric(d$x) 
d$y = as.numeric(d$y)


####  Visualize ####

lime_green = "#2ecc71"
soft_blue = "#3498db"
pinkish_red = "#e74c3c"
purple = "#9b59b6"
teele = "#1abc9c"
nice_blue = "#2980b9"

my.theme <- 
  theme(#plot.background = element_rect(fill="white"), # Remove background
    panel.grid.major = element_blank(), # Remove gridlines
    # panel.grid.minor = element_blank(), # Remove more gridlines
    # panel.border = element_blank(), # Remove border
    panel.background = element_blank(), # Remove more background
    axis.ticks = element_blank(), # Remove axis ticks
    axis.text=element_text(size=6), # Enlarge axis text font
    axis.title=element_text(size=8), # Enlarge axis title font
    plot.title=element_text(size=12) # Enlarge, left-align title
    ,axis.text.x = element_text(angle=60, hjust = 1) # Uncomment if X-axis unreadable 
  )

my_color = nice_blue



crimeTypeData <- d
crimeType <- "Homicides"


#### Time Series ####
days <- crimeTypeData %>%
  group_by(Date) %>%
  summarise(Events = n())

allDays <- seq.Date(from = d$Date[nrow(d)], to=d$Date[1], b='days')
allDays <- allDays  %>%  as.data.frame() 
colnames(allDays)[1] = "Date"

# After this we will have a df with every date and how many work orders
ts = merge(days, allDays, by='Date', all=TRUE)
ts[is.na(ts)] <- 0

remove(allDays, days)

ggplot(ts, aes(x=Date, y=Events)) + 
  geom_line(colour=my_color, size = .5) + 
  my.theme + ggtitle(paste(crimeType, "Reports Over Time")) + xlab("Time") +
  ylab("Daily Reports") + 
  scale_y_continuous(labels = comma)

ggsave(paste("./plots/OneOff/",crimeType, "_DailyTimeSeries.png", sep=""), dpi=250, width=5, height=3)


# Monthly time series 
tsm <- ts %>%
  mutate(Year.Month = format(Date, '%Y-%m')) %>%
  group_by(Year.Month) %>%
  summarise(Events = sum(Events)) %>%
  mutate(Year.Month = as.Date(paste(Year.Month,1,sep="-"),"%Y-%m-%d"))

ggplot(tsm, aes(x=Year.Month, y=Events, group = 1)) + 
  geom_line(colour=my_color, size = .5) + 
  my.theme + ggtitle(paste(crimeType, "Reports Over Time")) + xlab("Time") +
  ylab("Monthly Reports") + 
  scale_y_continuous(labels = comma) + scale_x_date(labels=date_format("%Y"))

ggsave(paste("./plots/OneOff/",crimeType, "_MonthlyTimeSeries.png", sep=""), dpi=250, width=5, height=3)

ggplot(tsm, aes(x=Year.Month, y=Events, group = 1)) + 
  geom_bar(stat='identity', fill=my_color) + 
  my.theme + ggtitle(paste(crimeType, "Reports Over Time")) + xlab("Time") +
  ylab("Monthly Reports") + 
  scale_y_continuous(labels = comma) + scale_x_date(labels=date_format("%Y"))

ggsave(paste("./plots/OneOff/",crimeType, "_MonthlyTimeSeriesBar.png", sep=""), dpi=250, width=5, height=3)


# Recent monthly time series
tsrm <- tsm %>%
  filter(Year.Month > YearAgo)

ggplot(tsrm, aes(x=Year.Month, y=Events, group = 1)) + 
  geom_bar(stat='identity', fill=my_color) + 
  my.theme + ggtitle(paste(crimeType, ": Last 12 Months")) + xlab("Month") +
  ylab("Monthly Reports") + 
  scale_y_continuous(labels = comma) + scale_x_date(labels=date_format("%b %Y"))

ggsave(paste("./plots/OneOff/",crimeType, "_MonthlyTimeSeriesBarRecent.png", sep=""), dpi=250, width=5, height=3)


# Very recent daily time series
tsr <- ts %>%
  filter(Date > sixtyDaysAgo)

ggplot(tsr, aes(x=Date, y=Events)) + 
  geom_line(colour=my_color, size = .5) + 
  my.theme + ggtitle(paste(crimeType, ": Last 60 Days")) + xlab("Day") +
  ylab("Daily Reports") + 
  scale_y_continuous(labels = comma)

ggsave(paste("./plots/OneOff/",crimeType, "_VeryRecentDailyTimeSeries.png", sep=""), dpi=250, width=5, height=3)

ggplot(tsr, aes(x=Date, y=Events)) + 
  geom_bar(stat='identity', fill=my_color) + 
  my.theme + ggtitle(paste(crimeType, ": Last 60 Days")) + xlab("Day") +
  ylab("Daily Reports") + 
  scale_y_continuous(labels = comma)

ggsave(paste("./plots/OneOff/",crimeType, "_VeryRecentDailyTimeSeriesBar.png", sep=""), dpi=250, width=5, height=3)


### Year to Date Yearly Comparison
JustYtD <- crimeTypeData %>%
  filter(YearDay <= yday(today))

AnnualYtD <- JustYtD %>%
  group_by(Year) %>%
  summarise(Events = n()) 

ggplot(AnnualYtD, aes(x=Year, y=Events)) + 
  geom_bar(stat='identity', fill=my_color) + 
  my.theme + ggtitle(paste(crimeType, ": Year to Date")) + xlab("Year") +
  ylab("YtD Reports") + 
  scale_y_continuous(labels = comma)

ggsave(paste("./plots/OneOff/",crimeType, "_YeartoDateBar.png", sep=""), dpi=250, width=5, height=3)


# More recent ytd
RecentYtD <- JustYtD %>%
  group_by(Year) %>%
  summarise(Events = n()) %>%
  filter(Year > 2010)

ggplot(RecentYtD, aes(x=Year, y=Events)) + 
  geom_bar(stat='identity', fill=my_color) + 
  my.theme + ggtitle(paste(crimeType, ": Year to Date")) + xlab("Year") +
  ylab("YtD Reports") + 
  scale_y_continuous(labels = comma)

ggsave(paste("./plots/OneOff/",crimeType, "_YeartoDateBarRecent.png", sep=""), dpi=250, width=5, height=3)


# Trailing 365 - a better comparison than YTD when it's early in the year

crimeTypeData$period <- 
  ifelse((crimeTypeData$Date >= YearAgo), "TrailingYear",
         ifelse((crimeTypeData$Date >= TwoYearsAgo) & (crimeTypeData$Date < YearAgo), "PrevPer1",
                ifelse((crimeTypeData$Date >= ThreeYearsAgo) & (crimeTypeData$Date < TwoYearsAgo), "PrevPer2",
                       ifelse((crimeTypeData$Date >= FourYearsAgo) & (crimeTypeData$Date < ThreeYearsAgo), "PrevPer3",
                              "LongAgo"))))

TrailingYear <- crimeTypeData %>%
  group_by(period) %>%
  summarise(Events = n()) %>%
  filter(period != "LongAgo") %>%
  mutate(period = factor(period, levels=c("PrevPer3", "PrevPer2", "PrevPer1", "TrailingYear")))

ggplot(TrailingYear, aes(x=period, y=Events)) + 
  geom_bar(stat='identity', fill=my_color) + 
  my.theme + ggtitle(paste(crimeType, ": Trailing 365 Days")) + xlab("Period") +
  ylab("Reports / 365 Days") + 
  scale_y_continuous(labels = comma)

ggsave(paste("./plots/OneOff/",crimeType, "_Trailing365Days.png", sep=""), dpi=250, width=5, height=3)




##### Descriptive stats ouput to a readme.txt ####

# Difference in YtD
thisYear <- nrow(AnnualYtD)
PerChange <- (AnnualYtD$Events[thisYear] - AnnualYtD$Events[thisYear - 1]) / AnnualYtD$Events[thisYear - 1]
GrowthOrDecline <- ifelse(PerChange > 0, "are up by", "are down by")

# Difference in Trailing 365
PerChangeLastYear <- (TrailingYear$Events[nrow(TrailingYear)] - TrailingYear$Event[(nrow(TrailingYear)) - 1]) / TrailingYear$Event[nrow(TrailingYear) - 1]
GrowthOrDeclineLastYear <- ifelse(PerChangeLastYear > 0, "are up by", "are down by")


# start writing out
# This makes the .txt report
sink(paste("./plots/OneOff/",crimeType, "_ReadMe.txt", sep=""))

cat(sprintf("Year to Date there have been %s Reports for %s. Last year during the same time frame there were %s, which means Reports for this work order %s %s percent \n", AnnualYtD$Events[thisYear], crimeType, AnnualYtD$Events[thisYear-1], GrowthOrDecline, round((PerChange * 100))))

cat("---\n")

cat("Trailing 365 - a better comparison than YTD when it's early in the year:\n")

cat(sprintf("During the last 365 days there have been %s Reports for %s. During the previous 365-day time frame there were %s, which means Reports for this work order %s %s percent \n", TrailingYear$Events[nrow(TrailingYear)], crimeType, TrailingYear$Event[(nrow(TrailingYear)) - 1], GrowthOrDeclineLastYear, round((PerChangeLastYear * 100))))


# Stop writing to the file
sink()




#############################################################
#### Maps ####

workOrderDataRecent <- filter(workOrderData, DaysAgo >= -30)

addresses <- paste(workOrderDataRecent$Location, "Somerville", "MA", sep=", ")
locs <- geocode(addresses)
locs2 <- subset(locs, lat != 42.3875968 ) # Takes out the weird ones Google couldn't pin
# I map locs2 because when Google can't find something, it usually puts it int the center of the map
# This throws off the heat maps


# Dot map 
map.center <- geocode("Central Rd, Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 14)
SHmap + geom_point(
  aes(x=locs2$lon, y=locs2$lat),size = 3, alpha = .7, bins = 26, color="red", 
  data = locs2) +
  ggtitle(paste(workOrder, "Calls Since", sixtyDaysAgo))

ggsave(paste("./plots/OneOff/",workOrder, "_map.png", sep=""), dpi=250, width=6, height=5)


# Dot map 
map.center <- geocode("East Somerville, Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 15)
SHmap + geom_point(
  aes(x=locs2$lon, y=locs2$lat),size = 3, alpha = .7, bins = 26, color="red", 
  data = locs2) +
  ggtitle(paste(workOrder, "Calls Since", sixtyDaysAgo))

ggsave(paste("./plots/OneOff/",workOrder, "_map_East.png", sep=""), dpi=250, width=6, height=5)


# Dot map 
map.center <- geocode("West Somerville, Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 15)
SHmap + geom_point(
  aes(x=locs2$lon, y=locs2$lat),size = 3, alpha = .7, bins = 26, color="red", 
  data = locs2) +
  ggtitle(paste(workOrder, "Calls Since", sixtyDaysAgo))

ggsave(paste("./plots/OneOff/",workOrder, "_map_West.png", sep=""), dpi=250, width=6, height=5)


# Dot map 
map.center <- geocode("Central Rd, Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 15)
SHmap + geom_point(
  aes(x=locs2$lon, y=locs2$lat),size = 3, alpha = .7, bins = 26, color="red", 
  data = locs2) +
  ggtitle(paste(workOrder, "Calls Since", sixtyDaysAgo))

ggsave(paste("./plots/OneOff/",workOrder, "_map_Central.png", sep=""), dpi=250, width=6, height=5)


# A for loop that will create a dot map for every neighborhood you specify
neighborhoodList <- c("Assembly Square", "Ball Square", "Davis Square", "East Somerville", "Gilman Square", "Magoun Square", "Porter Square", "Prospect Hill", "Spring Hill", "Teele Square", "Ten Hills", "Union Square", "Winter Hill")

for (n in 1:(length(neighborhoodList))) {
  map <- get_map(location = paste(neighborhoodList[n], "Somerville, MA", sep=", "), zoom=16, maptype="roadmap", color = "bw")
  ggmap(map) +
    geom_point(data=locs2,size=4, color = "red", alpha = .5,
               aes(x=lon,y=lat)) +
    labs(x="",y="") +
    theme(axis.text=element_blank(),axis.ticks=element_blank()) +
    ggtitle(paste(workOrder, neighborhoodList[n]))
  
  ggsave(paste("./plots/OneOff/",workOrder, "_NB_map_",neighborhoodList[n], ".png", sep=""), dpi=250, width=6, height=5)
  
}


# More traditional heat map
map.center <- geocode("Central Rd, Somerville, MA")
map.center <- c(lon=map.center$lon, lat=map.center$lat)
somerville.map = get_map(location = map.center, zoom = 13, maptype="roadmap",color = "bw")
ggmap(somerville.map, extent = "panel", maprange=FALSE) %+% locs2 + aes(x = locs2$lon, y = locs2$lat) +
  # geom_density2d(data = locs2, aes(x = lon, y = lat)) + # uncomment for contour lines
  stat_density2d(data = locs2, aes(x = lon, y = lat,  fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12)) +
  ggtitle(paste(workOrder, "Calls Since", sixtyDaysAgo))


ggsave(paste("./plots/OneOff/",workOrder, "_map_Heat1.png", sep=""), dpi=250, width=6, height=5)


# More traditional heat map
map.center <- geocode("Central Rd, Somerville, MA")
map.center <- c(lon=map.center$lon, lat=map.center$lat)
somerville.map = get_map(location = map.center, zoom = 14, maptype="roadmap",color = "bw")
ggmap(somerville.map, extent = "panel", maprange=FALSE) %+% locs2 + aes(x = locs2$lon, y = locs2$lat) +
  # geom_density2d(data = locs2, aes(x = lon, y = lat)) + # uncomment for contour lines
  stat_density2d(data = locs2, aes(x = lon, y = lat,  fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12)) +
  ggtitle(paste(workOrder, "Calls Since", sixtyDaysAgo))


ggsave(paste("./plots/OneOff/",workOrder, "_map_Heat2.png", sep=""), dpi=250, width=6, height=5)


