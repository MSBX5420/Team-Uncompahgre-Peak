#import data, get rid of #-comments
df = read.csv("waqi-covid19-airqualitydata-2020.csv", comment.char="#")

#load libraries,
library(psych)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(gganimate)
library(maps)
library(tibble)
library(lubridate)

#look at unique values of Specie column to determine which one(s) to use for this project
unique(df$Specie)

#Subset df$Specie to "no2" value, drop all other levels
df_no2 <- subset(df, Specie=="no2")
df_no2 <- droplevels(df_no2)
levels(df_no2$Specie)

#Order df_no2 by date 
df_no2[order(as.Date(df_no2$Date, format="%Y-%m-%d")),]

#Subset df_no2 by relevant citiies
df_chicago = df_no2[df_no2$City=="Chicago",]
df_losangeles = df_no2[df_no2$City=="Los Angeles",]
df_manhattan = df_no2[df_no2$City=="Manhattan",]
df_sanfrancisco = df_no2[df_no2$City=="San Francisco",]
df_seattle = df_no2[df_no2$City=="Seattle",]
df_washingtondc = df_no2[df_no2$City=="Washington D.C.",]

#rbind all data sets
us_no2_2020 <- rbind(df_chicago, df_losangeles, df_manhattan, df_sanfrancisco, df_seattle, df_washingtondc)

#Create new column to enter shelter-in-place-dates for each city, enter 1 (binary value) for shelter-in-place date
us_no2_2020$shelter_in_place <- ifelse(us_no2_2020$Date =="2020-03-20" & us_no2_2020$City=="Chicago", 1, 
                                       ifelse(us_no2_2020$Date=="2020-03-20" & us_no2_2020$City == "Los Angeles", 1,
                                              ifelse(us_no2_2020$Date=="2020-03-20" & us_no2_2020$City == "Manhattan", 1,
                                                     ifelse(us_no2_2020$Date=="2020-03-18" & us_no2_2020$City == "San Francisco", 1,
                                                            ifelse(us_no2_2020$Date=="2020-03-23" & us_no2_2020$City == "Seattle", 1,
                                                                   ifelse(us_no2_2020$Date=="2020-04-01" & us_no2_2020$City == "Washington D.C.", 1, 0))))))
                                                            
#Create longitude column "lon"
us_no2_2020$lon <- ifelse(us_no2_2020$City=="Chicago", -87.629799,
                          ifelse(us_no2_2020$City=="Los Angeles", -118.243683,
                                 ifelse(us_no2_2020$City=="Manhattan", -73.971252,
                                        ifelse(us_no2_2020$City=="San Francisco", -122.418433,
                                               ifelse(us_no2_2020$City=="Seattle", -122.330417,
                                                      ifelse(us_no2_2020$City=="Washington D.C.", -77.031281, 0))))))

#Create latitude column "lat"
us_no2_2020$lat <- ifelse(us_no2_2020$City=="Chicago", 41.878113,
                          ifelse(us_no2_2020$City=="Los Angeles", 34.052235,
                                 ifelse(us_no2_2020$City=="Manhattan", 40.783058,
                                        ifelse(us_no2_2020$City=="San Francisco", 37.779379,
                                               ifelse(us_no2_2020$City=="Seattle", 47.603363,
                                                      ifelse(us_no2_2020$City=="Washington D.C.", 38.895438, 0))))))
                          

#find Max/Min of "median" variable
summary(us_no2_2020)

usa <- ggplot() +
  borders("usa", colour = "gray85", fill = "gray80") +
  theme_map() 

usmap <- usa +
  geom_point(data = us_no2_2020, aes(x = lon,y = lat, size = median),
             colour = 'red1', alpha = .5) +
  scale_size_continuous(range = c(1, 8), 
                        breaks = c(10, 20, 30, 40)) +
  labs(size = 'median')

ghost_points_ini <- tibble(
  created_at = as.Date('2019-12-30'),
  median = 0, lon = 0, lat = 0)

ghost_points_fin <- tibble(
  created_at = seq(as.Date('2020-04-13'),
                   as.Date('2020-04-26'),
                   by = 'days'),
  median = 0, lon = 0, lat = 0)

usmap <- usa +
  geom_point(aes(x = lon, y = lat, size = median, frame = created_at, cumulative = TRUE),
             data = us_no2_2020, colour = 'red1', alpha = .5) +
  geom_point(aes(x = lon, y = lat, size = median,frame = created_at, cumulative = TRUE),
             data = ghost_points_ini, alpha = 0) +
  geom_point(aes(x = lon, y = lat, size = median,frame = created_at, cumulative = TRUE),
             data = ghost_points_fin, alpha = 0) +
  scale_size_continuous(range = c(1, 8), breaks = c(10,20,30,40)) +
  labs(size = 'median') 

ani.options(interval = 0.5)
gganimate(usmap)

