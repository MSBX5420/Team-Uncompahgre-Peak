#import data, get rid of #-comments
df <- read.csv("C:/Users/manth/Google Drive/MS BUAN/03 Spring session 2020/Unstructured and Distributed Data Modeling MSBX 5420/Group_Project_Unstructured_Distributed_Data/waqi-covid19-airqualitydata-2020.csv", comment.char="#")
df$Date=as.Date(df$Date)
str(df)
df$weekday=weekdays(df$Date)

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
tail(us_no2_2020)

usa <- ggplot()+
  borders("usa", colour = "gray85", fill = "gray80") +
  theme_map() 

usmap <- usa +
  geom_point(data = us_no2_2020, 
             aes(x = lon,y = lat, 
                 size = median, 
                 colour=median), alpha = .5) +
  scale_size_continuous(range = c(1, 8), 
                        breaks = c(10, 20, 30, 40)) +
  labs(size = 'Median NO2 Concentration')+ 
  theme(legend.position="bottom") +
  scale_color_gradient(low="yellow", high="red")


### Working Plot Animation Script

anim <- usmap + 
  transition_states(Date,
                    transition_length = 2,
                    state_length = 1)
anim+transition_time(Date) +
  labs(title = "Date: {frame_time}")
file_renderer(dir = ".", prefix = "gganim_plot", overwrite = FALSE)


#anim + ggtitle('Now showing {closest_state}',subtitle = 'Frame {frame} of {nframes}')

#anim



#### Pre-existing attempt at Plot Animation

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


#Create new column to enter shelter-in-place-dates for each city, enter 1 (binary value) for shelter-in-place date
us_no2_2020$shelter_in_place <- ifelse(us_no2_2020$Date>"2020-03-20" & us_no2_2020$City=="Chicago", 1, 
                                       ifelse(us_no2_2020$Date>"2020-03-20" & us_no2_2020$City == "Los Angeles", 1,
                                              ifelse(us_no2_2020$Date>"2020-03-20" & us_no2_2020$City == "Manhattan", 1,
                                                     ifelse(us_no2_2020$Date>"2020-03-18" & us_no2_2020$City == "San Francisco", 1,
                                                            ifelse(us_no2_2020$Date>"2020-03-23" & us_no2_2020$City == "Seattle", 1,
                                                                   ifelse(us_no2_2020$Date>"2020-04-01" & us_no2_2020$City == "Washington D.C.", 1, 0))))))

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
tail(us_no2_2020)

usa <- ggplot()+
  borders("usa", colour = "gray85", fill = "gray80") +
  theme_map() 

usmap <- usa +
  geom_point(data = us_no2_2020, 
             aes(x = lon,y = lat, 
                 size = median, 
                 colour=median), alpha = .5) +
  scale_size_continuous(range = c(1, 8), 
                        breaks = c(10, 20, 30, 40)) +
  labs(size = 'Median NO2 Concentration')+ 
  theme(legend.position="bottom") +
  scale_color_gradient(low="yellow", high="red")



setwd("C:/Users/manth/Google Drive/MS BUAN/
03 Spring session 2020/Unstructured and Distributed Data Modeling MSBX 5420/
      Group_Project_Unstructured_Distributed_Data/Analysis results and charts")

anim <- usmap + 
  transition_states(Date,
                    transition_length = 2,
                    state_length = 1)
anim+transition_time(Date) +
  labs(title = "Date: {frame_time}")
file_renderer(dir = ".", prefix = "gganim_plot", overwrite = FALSE)

us_no2_under_shelter_in_place=us_no2_2020[us_no2_2020$shelter_in_place==1,]
us_no2_pre_shelter=us_no2_2020[us_no2_2020$shelter_in_place==0,]

boxplot(us_no2_pre_shelter$median,us_no2_under_shelter_in_place$median)
t.test(us_no2_pre_shelter$median,us_no2_under_shelter_in_place$median)

agg_by_day=aggregate(us_no2_2020$median, by=list(us_no2_2020$Date),FUN=median)
names(agg_by_day)=c('day','median_no2')

pre_no2=agg_by_day$x[agg_by_day$x<"2020-03-20"]
post_no2=agg_by_day$x[agg_by_day$x>"2020-03-20"]

pre_no2_mean=mean(agg_by_day$x[agg_by_day$x<"2020-03-20"])
post_no2_mean=mean(agg_by_day$x[agg_by_day$x>"2020-03-20"])


plot(agg_by_day,type='l',
     main="Median Daily Nitrogen Dioxide Concentrations
     for Chicago, LA, NYC, SF, Seatle and DC
     With Stay at Home Order Issue Dates",
     xlab='Date',
     y="Concentation in ppb")

     
abline(v=as.Date("2020-03-18"),col="blue")
text(x=as.Date("2020-03-18")-2,y=14,"SF - March 23rd", col="blue",srt=90,cex=0.6)

abline(v=as.Date("2020-03-20"),col="red")
text(x=as.Date("2020-03-20")+1.5,y=14,"Chicago, LA, NYC", col="red",srt=90,cex=0.6)

abline(v=as.Date("2020-03-23"),col="green")
text(x=as.Date("2020-03-23")+1.5,y=14,"Seattle", col="green",srt=90,cex=0.6)

abline(v=as.Date("2020-04-01"),col="purple")
text(x=as.Date("2020-04-01")+1.5,y=14,"DC - April 1st", col="purple",srt=90,cex=0.6)



abline(h=pre_no2_mean,col='red',lwd=2)
text(x=as.Date("2020-03-01"),y=pre_no2_mean+1,col="red")

abline(h=post_no2_mean,col="green",lwd=2)

boxplot(pre_no2,post_no2, main= "Boxplot of NO2 Concentration Measurments
Before and After March 20th 2020", names=c("Pre March 20th","Post March 20th"),
col="lightblue",
sub= "Values are the daily median NO2 concentration in ppb from 
        stations in Chicago, LA, NYC, SF, Seatle and DC. 
        Date range is Jan 1st 2020 - April 26th 2020")

sd(pre_no2)
sd(post_no2)
t.test(pre_no2,post_no2)

