#Load data...
#Already done, should write something to pull from SQLite though...
library('RSQLite', 'plyr', 'stringr')
#Strip geocode from Location.1
#Regex expression
geo <-function(String){
  Loc <- str_extract_all(as.character(String), "\\([^()]+\\)")[[1]]
  Loc <- substring(Loc, 2, nchar(Loc)-1)
  return(Loc)}

#Use plyr to create new Geocode Variable
Narco$Geocode <- laply(Narco$Location.1, geo)
