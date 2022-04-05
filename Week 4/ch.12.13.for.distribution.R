# -------------- Chapter 12: Pictures vs Numbers ---------------
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

hist(dfStates$july11pop)
barplot(dfStates$july11pop, names.arg = dfStates$stateName, las=2)

# EnsurePackage<-function(x) {
#   x <- as.character(x)
#   
#   if (!require(x,character.only=TRUE)) {
#     install.packages(pkgs=x,repos="http://cran.r-project.org")
#     require(x,character.only=TRUE)
#   }
# }
# EnsurePackage("ggplot2")

g <- ggplot(dfStates, aes(x=july11pop)) 
g <- g + geom_histogram(binwidth=5000000, color="black", fill="white")
g <- g + ggtitle("states population histogram")
g



ggplot(dfStates,aes(x=factor(0),july11pop)) + geom_boxplot()

dfStates$popChange <- dfStates$july11pop - dfStates$july10pop
dfStates$increasePop <- ifelse(dfStates$popChange > 0, "positive", "negative")

g <- ggplot(dfStates,aes(x=factor(increasePop), july11pop))
g <- g +geom_boxplot() + coord_flip() 
g <- g + ggtitle('Population grouped by positive or negative change')
g



g <- ggplot(dfStates,aes(x=reorder(stateName, july11pop), y=july11pop, group=1))  
g <- g + geom_line()
g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g

g <- ggplot(dfStates,aes(x=reorder(stateName, july11pop), y=july11pop, group=1))  
g <- g + geom_col()
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g


dfStates$percentChange <- dfStates$popChange/dfStates$july10pop * 100
g <- ggplot(dfStates,aes(x=reorder(stateName, july11pop), y=july11pop,fill=percentChange)) 
g <- g + geom_col() 
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g

g <- ggplot(dfStates, aes(x=popChange, y=percentChange))  
g <- g + geom_point(aes(size=july11pop, color=july11pop))
g


g  + geom_text(aes(label=stateName), size=4)


g + geom_text(aes(label=stateName),size=4, hjust=1, vjust=-1)


minPerChange <- 1
minPopChange <- 100000
dfStates$keystate <- dfStates$popChange > minPopChange & dfStates$percentChange> minPerChange

minLabel <- format(min(dfStates$july11pop), big.mark=",", trim=TRUE)
maxLabel <- format(max(dfStates$july11pop), big.mark=",", trim=TRUE)
medianLabel <- format(median(dfStates$july11pop), big.mark=",", trim=TRUE)

g <- ggplot(dfStates, aes(x=popChange, y=percentChange))  
g <- g + geom_point(aes(size=july11pop, color=july11pop, shape=keystate))
g <- g + geom_text(data=dfStates[dfStates$popChange > minPopChange & dfStates$percentChange> minPerChange,], aes(label=stateName, hjust=1, vjust=-1))
g + scale_color_continuous(name="Pop",                                    
                           breaks = with(dfStates, c(min(july11pop), median(july11pop), max(july11pop))), 
                           labels = c(minLabel, medianLabel, maxLabel),         
                           low = "white",                                    
                           high = "black")    

####################################################################
# ------------------ Chapter 13: Map Mash-Up -------------------
######################################################################
#install.packages("mapproj")
require("mapproj")
us <- map_data("state")
head(us)
dummyDF <- data.frame(state.name, stringsAsFactors=FALSE)
dummyDF$state <- tolower(dummyDF$state.name)
head(dummyDF)
map.simple <- ggplot(dummyDF, aes(map_id = state))  
map.simple <- map.simple+  geom_map(map = us, fill="white", color="black") 
map.simple <- map.simple + expand_limits(x = us$long, y = us$lat)
map.simple <- map.simple + coord_map() +  ggtitle("basic map of continental USA")
map.simple



dfStates <- readCensus()
# make sure everything is lowercase
dfStates$state <- tolower(dfStates$stateName)

map.popColor <- ggplot(dfStates, aes(map_id = state))  
map.popColor <- map.popColor + geom_map(map = us, aes(fill=july11pop)) 
map.popColor <- map.popColor + expand_limits(x = us$long, y = us$lat)
map.popColor <- map.popColor+ coord_map() + ggtitle("state population")
map.popColor

map.simple + geom_point(aes(x = -100, y = 30))
# follow this direction to receive API keys (https://developers.google.com/maps/documentation/embed/get-api-key)
#you need to register with a credit card. You will not be charged for this though. 

library(ggmap)
#api <-"AIzaSyDZE1x8sV7lw5mZYdr58BtMB3gNomAsJJ4"
#register_google(key=api)

latlon <- geocode("tampa, fl")
latlon
map.popColor + geom_point(aes(x = latlon$lon, y = latlon$lat), color="darkred", size = 5)


df.latlon <- data.frame(latlon)
latlon <- geocode("colorado")
df.latlon[2,] <- latlon
df.latlon[3,] <- geocode("denver, colorado")
#map.simple + geom_point(data=df.latlon, aes(x = lon, y = lat))

df.latlon$state <- "?"
map.popColor + geom_point(data=df.latlon,aes(x = lon, y = lat),  alpha = .5, color="darkred", size = 10)

#######################################################################################################
urlFile <- "http://www.opendata500.com/us/download/us_companies.csv"
od.companies <- read.csv(url(urlFile))
str(od.companies)
od.companies <- od.companies[od.companies$city != "",] 
od.companies$state <- as.character(od.companies$state)
od.companies <- od.companies[od.companies$state != "DC",]
od.companies$state <- ifelse(od.companies$state == "KA", "KS", od.companies$state)

od.companies$cityState <- paste(od.companies$city, od.companies$state)
#od.companies$geoCode <- geocode(od.companies$cityState)
storedGeoCode <-geocode(od.companies$cityState)
od.companies$geoCode <-storedGeoCode
#map.simple + geom_point(data=od.companies,aes(x = geoCode$lon, y = geoCode$lat), shape=1)

bad <- od.companies[od.companies$geoCode$lon > 0, ]
bad$cityState

od.companies <- od.companies[od.companies$geoCode$lon < 0, ]
map.simple + geom_point(data=od.companies, aes(x = geoCode$lon, y = geoCode$lat), shape=1)


library(RColorBrewer)
od.companies$sizes <- factor(od.companies$full_time_employees,levels = 
                               c("1-10", "11-50", "51-200", "201-500", "501-1,000", 
                                 "1,001-5,000", "5,001-10,000", "10,001+"))
numSizes <- length(levels(od.companies$sizes))
myColors <- brewer.pal(numSizes,"Reds")
names(myColors) <- levels(od.companies$sizes)
myColors[1:3]

map.popColor + 
  geom_point(data=od.companies, aes(x = geoCode$lon, y = geoCode$lat, color=sizes)) + 
  scale_colour_manual(name =  "sizeOfCompany", values = myColors) +
  ggtitle("Open Data Company Analysis")

save.image("map.mashup.RData")
