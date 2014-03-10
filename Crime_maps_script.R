#Load data...
#Already done, should write something to pull from SQLite though...
library('RSQLite', 'plyr', 'stringr')
library('rgdal')
#Strip geocode from Location.1
#Regex expression
geo <-function(String){
  Loc <- str_extract_all(as.character(String), "\\([^()]+\\)")[[1]]
  Loc <- substring(Loc, 2, nchar(Loc)-1)
  return(Loc)}

#Use plyr to create new Geocode Variables
Narco$Geocode <- laply(Narco$Location_1, geo)
position <- str_split(Narco$Geocode, ', ')
position <- ldply(position[Narco$Geocode != ''])
Narco$Lat <- as.numeric(position[,1])
Narco$Long <- as.numeric(position[,2])


#Maps
#according to the .prj file these are the correct variables for a 
#lambert conformal conic (2SP for proj4)
projection <- "+proj=lcc +lat_1=38.3 +lat2=39.45 +lat0=37.66666666666666 +lon0=-77.0 +x_0=1312333.333333333 +y_0=0.0"

#read shape files in
Balt_CLine <- readShapeLines("~/Documents/Baltimore/City/Baltcity_20Line/baltcity_line", proj4string=CRS(projection))
Balt_NBHD <- readShapePoly("~/Documents/Baltimore/City/Neighborhood_202010/nhood_2010", proj4string=CRS(projection))
library('ggplot2')
library('ggmap')
bound_plot <- ggplot(data=Balt_CLine, 
                     aes(x=long, y=lat, group=group)) +
  geom_polygon(color='gray', fill='lightblue') + 
  coord_equal() +  theme_nothing()

nbhds_plot <- bound_plot + geom_polygon(data=Balt_NBHD,color='light gray')
nbhds_plot

#Could also use readOGR instead of specifying the projection

#code latitude and longitude into city grid
#with help from 
#http://www.obscureanalytics.com/2012/12/07/visualizing-baltimore-with-r-and-ggplot2-crime-data/
latlng_spdf <- SpatialPoints(Narco[,c('Lat','Long')],proj4string=CRS("+proj=longlat +datum=WGS84"))
origProj <- Balt_NBHD@proj4string
latlng_spdf <-  spTransform(latlng_spdf,origProj)
latlng_spdf_coords <- coordinates(latlng_spdf)