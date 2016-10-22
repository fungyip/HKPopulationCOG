library(ggmap)
library(dplyr)
library(xlsx)

#getwd()

####2.2 Data Manipulation####
#Hong Kong Census data 2001
data_2011<-read.xlsx("./dataIn/table_2011.xlsx", sheetName = "Sheet1", startRow = 4, header=TRUE, encoding = "UTF-8", stringsAsFactors=FALSE) 
data_2001<-read.xlsx("./dataIn/table_2001.xlsx", sheetName = "Sheet1", startRow = 4, header=TRUE, encoding = "UTF-8", stringsAsFactors=FALSE)

str(data_2011)
head(data_2011)
names(data_2011)
names(data_2011)[1]<-"Area"

data_2011<- data_2011 %>% select(Area,Population)

data_2011<-data_2011[1:412,c(1:2)]

# geocoding
# get the address list, and append "Hong Kong" to the end to increase accuracy
# (change or remove this if your address already include a country etc.)
addresses = data_2011$Area
addresses = paste0(addresses, ", Hong Kong")

x<-geocode(addresses)
data_2011<-cbind(data_2011,x)
data_2011 %>% filter(!complete.cases(.))
data_2011 <-na.omit(data_2011)

write.xlsx(data_2011,"./DataOut/COG_2011.xlsx")


#Hong Kong Census data 2001
str(data_2001)

head(data_2001)

names(data_2001)

names(data_2001)[1]<-"Area"

data_2001<- data_2001 %>% select(Area,Population)

data_2001<-data_2001[1:389,c(1:2)]


# get the address list, and append "Hong Kong" to the end to increase accuracy
# (change or remove this if your address already include a country etc.)
addresses_y = data_2001$Area
addresses_y = paste0(addresses_y, ", Hong Kong")

y<-geocode(addresses_y)
data_2001<-cbind(data_2001,y)
data_2001 <-na.omit(data_2001)

write.xlsx(data_2001,"./DataOut/COG_2001.xlsx")


data_2011_a<-data_2011 %>% mutate(lon_weight =lon*Population, lat_weight=lat*Population)
data_2001_a<-data_2001 %>% mutate(lon_weight =lon*Population, lat_weight=lat*Population)

write.xlsx(data_2011_a,"./DataOut/COG_2011_a.xlsx")
write.xlsx(data_2001_a,"./DataOut/COG_2001_a.xlsx")


####2.3 Analysis Results - Hong Kong Population Center of Gravity####
sum_2011<-colSums(data_2011_a[,-1])
sum_2001<-colSums(data_2001_a[,-1])

centerofgravity_2011_lon<- sum_2011[4:4]/sum_2011[1:1]
centerofgravity_2011_lat<- sum_2011[5:5]/sum_2011[1:1]
centerofgravity_2001_lon<- sum_2001[4:4]/sum_2001[1:1]
centerofgravity_2001_lat<- sum_2001[5:5]/sum_2001[1:1]



####2.4 Points-in-Polygon - Where is the HK Population COG in terms of District and Area?####
# install.packages("rgdal")
require(sp)
require(rgdal)
require(maps)
library(xlsx)
library(dplyr)

POI <- c (centerofgravity_2011_lon, centerofgravity_2011_lat)
POI<-as.data.frame(POI)
POI<-t(POI)
str(POI)
POI<-POI[,1:2]
POI<-as.numeric(POI)
names(POI)[1]<-"lon"
names(POI)[2]<-"lat"
View(POI)
POI<-data.frame(POI)
POI
POI<-t(POI)
sapply(POI,typeof)
write.csv(POI,"./DataOut/poi.csv")
#View(POI)
#!is.na(POI_1)
POI<-read.csv("./DataOut/poi.csv")

#Import Shape file
coordinates(POI) <- c("lon", "lat")
hk.map <- readOGR("./DataIn/DC_2015_poly Shapefile/GIH3_DC_2015_POLY.shp",layer="GIH3_DC_2015_POLY")
hk.map_sp <- spTransform(hk.map, CRS("+proj=longlat +datum=WGS84"))
#hk.map_for <- fortify(hk.map_sp)

#coordinates(POI) <- c("lon", "lat")
proj4string(POI) <- proj4string(hk.map_sp)

POI$Area <- over(POI,hk.map_sp)$ENAME
POI$District <- over(POI,hk.map_sp)$DISTRICT_E

POI$Area
POI$District


#####2.5 Line Distance - Distance between HK Population COG in 2011 and 2001####
#install.packages("McSpatial")
library(McSpatial)
dis_mile<-geodistance(centerofgravity_2011_lon, centerofgravity_2011_lat,centerofgravity_2001_lon,centerofgravity_2001_lat)
dis_mile$dist

dis_km<-dis_mile$dist*1.60934
dis_km


####3. Data visualization - Hong Kong Population Center of Gravity####
# install.packages("rgeos")
library(rgeos)
library(sp)
library(rgdal)
library(ggplot2)

#1.Create HK Map
#library(foreign)
library(ggmap)
library(RgoogleMaps)

CenterOfMap <- geocode("22.3664714,114.1202076")

HK_OSM <- get_map(c(lon=CenterOfMap$lon, lat=CenterOfMap$lat),zoom = 10, maptype = "terrain", source = "osm")
HK_OSM <- ggmap(HK_OSM)
HK_OSM

#2.Import Shape File
hk.map <- readOGR("./DataIn/DC_2015_poly Shapefile/GIH3_DC_2015_POLY.shp",layer="GIH3_DC_2015_POLY")
hk.map_sp <- spTransform(hk.map, CRS("+proj=longlat +datum=WGS84"))
hk.map_for <- fortify(hk.map_sp)

library(RColorBrewer)
colors <- brewer.pal(9, "BuGn")

HK_OSM_a<-HK_OSM +
  
  geom_polygon(aes(x = long,
                   y = lat,
                   group = group),
               data = hk.map_for,
               color = "black",
               fill = "purple",
               alpha = 0.1) +
  labs(x = "Longitude",
       y = "Latitude")

# Plot  point of interest - COG
HK_OSM_final<-HK_OSM_a +
  geom_point(aes(x = centerofgravity_2011_lon,
                 y = centerofgravity_2011_lat),
             #data = POI,
             color = "black",
             size = 2)

HK_OSM_final_a<-HK_OSM_final +
  geom_point(aes(x = centerofgravity_2001_lon,
                 y = centerofgravity_2001_lat),
             #data = POI,
             color = "red",
             size = 2)

HK_OSM_final_a

ggsave(filename="./DataOut/hk_COG.png",plot=HK_OSM_final_a,scale=4)