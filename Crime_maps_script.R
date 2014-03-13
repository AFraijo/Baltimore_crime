#Load data...
#Already done, should write something to pull from SQLite though...
library('RSQLite')
library('sqldf')
library('plyr')
library('stringr')
library('rgdal')
library('foreign')
library('maptools')


Balt_crime <- dbConnect(SQLite(), dbname="/home/jaunte/Baltimore.sqlite")
dbListTables(Balt_crime)
Narco <- dbGetQuery(Balt_crime, "Select * from Arrests where incidentOffense='87-Narcotics'", row.names=FALSE)


#Strip geocode from Location.1
#Regex expression
geo <-function(String){
  Loc <- str_extract_all(as.character(String), "\\([^()]+\\)")[[1]]
  Loc <- substring(Loc, 2, nchar(Loc)-1)
  return(Loc)}

#Use plyr to create new Geocode Variables
Narco$Geocode <- laply(Narco$Location_1, geo)
position <- str_split(Narco$Geocode, ', ')
position <- ldply(position[Narco$Location_1 != ''])
Narco$lat<- as.numeric(position[,1])
Narco$long <- as.numeric(position[,2])
Narco <- Narco[Narco$lat != 0,]
Narco <- Narco[Narco$long != 0,]
Narco <- Narco[Narco$lat > 39.35, ]
Narco <- Narco[Narco$incidentLocation != "", ]
Narco <- Narco[Narco$arrestLocation != "", ]

#Maps
#according to the .prj file these are the correct variables for a 
#lambert conformal conic (2SP for proj4)
projection <- " +proj=lcc +lat_1=38.3 +lat_2=39.45 +lat_0=37.66666666666666 +lon_0=-77
+x_0=399999.9999999999 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80
+towgs84=0,0,0"

#read shape files in
Balt_CLine <- readShapeLines("~/Documents/Baltimore/City/Baltcity_20Line/baltcity_line", proj4string=CRS(projection))
Balt_NBHD <- readShapePoly("~/Documents/Baltimore/City/Neighborhood_202010/nhood_2010", proj4string=CRS(projection))
City_line <- spTransform(Balt_CLine,CRS("+proj=longlat +datum=WGS84"))
NBHD <- spTransform(Balt_NBHD,CRS("+proj=longlat +datum=WGS84"))
#Balt_df <- fortify(Balt_CLine)
#Balt_ND_df <- fortify(Balt_NBHD)
library('ggplot2')
library('ggmap')
#bound_plot <- ggplot(data=Balt_df, 
#                     aes(x=long, y=lat, group=group)) +
#  geom_polygon(color='gray', fill='lightblue')

bound_plot <- ggplot(data=City_line, 
                     aes(x=long, y=lat, group=group)) +
  geom_polygon(color='gray', fill='lightblue') + coord_equal() + theme_nothing()

nbhds_plot <- bound_plot + geom_polygon(data=NBHD,color='light gray') 
nbhds_plot

#Could also use readOGR instead of specifying the projection

#code latitude and longitude into city grid
#with help from 
#http://www.obscureanalytics.com/2012/12/07/visualizing-baltimore-with-r-and-ggplot2-crime-data/
#latlng_spdf <- SpatialPoints(Narco[,c('lat','long')],proj4string=CRS("+proj=longlat +datum=WGS84"))
#latlng_spdf <-  spTransform(latlng_spdf,CRS(projection))
#latlng_spdf_coords <- coordinates(latlng_spdf)
#Narco$lat <- latlng_spdf_coords[,1]
#Narco$long <- latlng_spdf_coords[,2]

narco_plot<-nbhds_plot + geom_point(data=subset(Narco,select=lat:long),aes(group=1), 
                      shape='x', alpha='.8', colour = "red", guide=F)

narco_plot
