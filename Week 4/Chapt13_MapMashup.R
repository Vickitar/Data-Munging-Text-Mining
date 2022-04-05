install.packages("mapproj")
install.packages("ggmap")
install.packages("ggplot2")
library(maps)
library(ggmap)
library(mapproj)
readCensus <-function(){
  urlToRead <- "https://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv"
  testFrame <- read.csv(url(urlToRead))
  #removing rows and columns: remove header rows using minus
  testFrame <-testFrame[-1:-8,]
  #keep the first five columns
  testFrame <- testFrame[,1:5]
  #rename the first column
  testFrame$stateName<-testFrame[,1]
  testFrame <-testFrame[,-1]    
  #remove the 'dot' from the state name
  testFrame$stateName <-gsub("\\.","",testFrame$stateName)
  #convert the columns to actual numbers and rename columns
  testFrame$april10census <-as.numeric(gsub(",","",testFrame$X))
  testFrame$april10base <-as.numeric(gsub(",","",testFrame$X.1))
  testFrame$july10pop <-as.numeric(gsub(",","",testFrame$X.2))
  testFrame$july11pop <-as.numeric(gsub(",","",testFrame$X.3))
  testFrame <-testFrame[,-1:-4]
  #remove the old rownames, which are now confusing
  rownames(testFrame) <-NULL
  testFrame <-testFrame[c(-52, -54:-58),]
  return(testFrame)
  
}

dfStates <-readCensus()


library(ggplot2)

us <- map_data("state")

dummyDF <- data.frame(state.name,stringsAsFactors = FALSE)
dummyDF$state <- tolower(dummyDF$state.name)

head(dummyDF)

map.simple <- ggplot(dummyDF, aes(map_id = state))
map.simple <- map.simple + geom_map(map = us, fill = "white", color="black")
map.simple <- map.simple + expand_limits(x = us$long, y= us$lat)
map.simple <- map.simple + coord_map() + ggtitle("Basic map of Continental USA")
map.simple


dfStates <- readCensus()

dfStates$state <- tolower(dfStates$stateName)


map.popColor <- ggplot(dfStates,aes(map_id=state))
map.popColor <- map.popColor + geom_map(map = us, aes(fill=july11pop))
map.popColor <- map.popColor + expand_limits(x = us$long, y = us$lat)
map.popColor <- map.popColor + coord_map() + ggtitle("State Population")
map.popColor
 

map.simple + geom_point(aes(x=-100, y = 30))

library(ggmap)
api <- "AIzaSyAxlP-tNYSZlWaMI9she8PPHXynJtSqw7s"
register_google(key=api)

latlon <- geocode("syracuse, ny")
latlon

map.popColor + geom_point(aes(x = latlon$lon, y = latlon$lat), color="darkred", size = 3)

df.latlon <- data.frame(latlon)
latlon <- geocode("colorado")
df.latlon[2,] <- latlon
df.latlon[3,] <- geocode("denver, colorado")

#map.simple + geom_point(data=df.latlon, aes(x=lon, y = lat))

df.latlon$state <- "?"

map.popColor + geom_point(data=df.latlon, aes(x=lon, y = lat), alpha = 5, color="darkred", size = 4)

#####################################################################################################

urlFile <- "https://raw.githubusercontent.com/GovLab/OpenData500/master/static/files/us/us_companies_all.csv"
od.companies <- read.csv(url(urlFile))


od.companies <- od.companies[od.companies$city !="",]

od.companies$state <- as.character(od.companies$state)
od.companies <- od.companies[od.companies$state != "DC",]
od.companies$state <- ifelse(od.companies$state == "KA", "KS", od.companies$state)

od.companies$cityState <- paste(od.companies$city,od.companies$state)

od.companies$geoCode <- geocode(od.companies$cityState)

map.simple + geom_point(data=od.companies,aes(x=geoCode$lon, y = geoCode$lat,), shape=2)

#bad <- od.companies[od.companies$geoCode$lon > 3, ]
#od.companies <- od.companies[od.companies$geoCode$lon < 0, ]
# we could get this code to work so we found which location was causing the error.
od.companies <- od.companies[od.companies$state != "PR",]
map.simple + geom_point(data=od.companies, aes(x= geoCode$lon, y=geoCode$lat), shape=1)

library(RColorBrewer)
#install.packages("RColorBrewer")
od.companies$sizes <- factor(od.companies$full_time_employees,levels = c("1-10", "11-50", "51-200", "201-500", "501-1,000", "1,001-5,000","5,001-10,000", "10,001+"))

numSizes <- length(levels(od.companies$sizes))
myColors <- brewer.pal(numSizes,"Reds")
names(myColors) <- levels(od.companies$sizes)
myColors[1:3]

map.popColor + geom_point(data=od.companies, aes(x=geoCode$lon, y= geoCode$lat, color=sizes)) + scale_color_manual(name = "sizeOfCompany", values = myColors)
+ ggtitle("Open Data Company Analysis")























