install.packages("mapproj")
install.packages("ggmap")
install.packages("ggplot2")
library(maps)
library(ggmap)

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

latlong <- geocode("syracuse, ny")
source: 