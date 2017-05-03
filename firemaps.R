install.packages("leaflet")
install.packages("ggplot2")
install.packages("plotly")
install.packages("data.table")
install.packages("ggmap")
install.packages("rworldmap")
require(plotly)
require(ggplot2)
library(ggmap)
library(maps)
library(leaflet)
require(data.table)

#read data into a variable
setwd("C:/Users/miracle/Desktop/Nasa Space Apps")
firedata <- read.csv("fire_nrt_V1_10660.csv")
firenewdata <- firedata[sample(nrow(firedata),10000),]
firesplit <- firenewdata[1:1000,]

#getting country of fires
countries <- map.where(database="world",firenewdata$latitude,firenewdata$longitude)
countries <- data.frame(countries)
firedatwith_countries = cbind(firenewdata,countries)
firedatwith_countries <- na.omit(firedatwith_countries)

#Plotting the last 7 day fires on World map
leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=firedatwith_countries$longitude, lat=firedatwith_countries$latitude, popup="fire")

#group the fires based on date to get how many fires happened on each day
firescount <- data.frame(table(firedatwith_countries$acq_date))
names(firescount) <- c("day","number")

#plot graph for number of fires
plot_ly(firescount, x = ~day, y= ~number,  type = 'bar')

#group the fires based on countries
grouped_countries <- data.frame(table(firedatwith_countries$countries))
names(grouped_countries) <- c("country","count")

#fires with countries
plot_ly(grouped_countries, x = ~country, y= ~count,  type = 'bar')

#day and night fires
grouped_daynight <- data.frame(table(firenewdata$daynight))
names(grouped_daynight) <- c("day_night","count")

#plota pie chart for fires in day or night
plot_ly(grouped_daynight, labels = ~day_night,values = ~count, type="pie")

#top fire
max_fire <- subset(firedatwith_countries,bright_ti4==max(firedatwith_countries$bright_ti4))
ten_max_fire <- head(max_fire,10)
leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=ten_max_fire$longitude, lat=ten_max_fire$latitude, popup="fire")

#take last 10 fires count
last_three_fires <- tail(firescount,3)

#predict
model <- glm(formula = number ~ day, data = last_three_fires, family = "gaussian")
p <- as.Date(c("2017-05-01","2017-05-02","2017-05-03"))
days <- data.frame(p)
names(days) <- "day"
predicted <- predict(model,days)
predicted_fires <- data.frame(days,predicted)
