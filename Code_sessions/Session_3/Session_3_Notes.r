library(stringr)
library(leaflet)
library(dplyr)

names(providers)[c(1:5)] #Shows first 5 in provider list

filter = str_detect(names(providers), "Esri") #Creates filter
names(providers)[filter] #Filters Esri maps

leaflet() %>%
  addProviderTiles("Esri")

#Enter longitude and latitude
long = -76.948270
lat = 38.983640
# Creat map
leaflet() %>% 
  addProviderTiles("CartoDB") %>% 
  addMarkers(lng = long, lat = lat, popup = 'SPP')

url = "https://assets.datacamp.com/production/repositories/1942/datasets/18a000cf70d2fe999c6a6f2b28a7dc9813730e74/ipeds.csv"
ipeds = read.csv(url)
ipeds %>% head(3)

map = leaflet() %>% 
addProviderTiles("CartoDB") %>% 
addCircleMarkers(data=ipeds,
                   radius = 2, color = "red", popup=~name)
map

map2 <- map %>% 
  clearMarkers() %>% 
  clearBounds()
map2 %>% 
  addCircleMarkers(data = ipeds, radius = 2, 
                   popup = ~paste0(name, "<br/>","<br/>", sector_label))


# Make a color palette called pal for the values of `sector_label` using `colorFactor()`  
# Colors should be: "red", "blue", and "#9b4a11" for "Public", "Private", and "For-Profit" colleges, respectively
pal <- colorFactor(palette = c("red", "blue", "#9b4a11"), 
                   levels = c("Public", "Private", "For-Profit"))

# Add circle markers that color colleges using pal() and the values of sector_label
map2 <- 
    map %>% 
        addCircleMarkers(data = ipeds, radius = 2, 
                         color = ~pal(sector_label), 
                         label = ~paste0(name, " (", sector_label, ")"))%>% 
    addLegend(pal = pal, 
              values = c("Public", "Private", "For-Profit"))

# Print map2
map2

library(leaflet.extras)
library(htmltools)

# Create data frame called public with only public colleges
public <- ipeds %>% filter(sector_label == "Public")  
profit <- ipeds %>%filter(sector_label == "For-Profit")  
private <- ipeds %>% filter(sector_label == "Private")  

m4 <- leaflet() %>% 

#Adding three sep maps part 1
        addTiles(group = "OSM") %>% 
        addProviderTiles("CartoDB", group = "Carto") %>% 
        addProviderTiles("Esri", group = "Esri")

m4  = m4 %>% addCircleMarkers(data = public, radius = 2, label = ~htmlEscape(name),
                         color = ~pal(sector_label), group = "Public") %>% #Public

        addCircleMarkers(data = private, radius = 2, label = ~htmlEscape(name),
                           color = ~pal(sector_label), group = "Private")  %>% #Private

        addCircleMarkers(data = profit, radius = 2, label = ~htmlEscape(name),
                         color = ~pal(sector_label), group = "For-Profit") #For profit

m4 = m4 %>% addLayersControl(baseGroups = c("OSM", "Carto", "Esri"), 
                         overlayGroups = c("Public", "Private", "For-Profit")) %>% 
        setView(lat = 39.8282, lng = -98.5795, zoom = 4) 
m4

# Make each sector of colleges searchable 
m4_search <- m4  %>% 
        addSearchFeatures(
            targetGroups = c("Public", "Private", "For-Profit"), 
            # Set the search zoom level to 18
            options = searchFeaturesOptions(zoom = 18)) 

# Try searching the map for a college
m4_search

ipeds %>% 
    leaflet() %>% 
        addTiles() %>% 
        # Sanitize any html in our labels
        addCircleMarkers(radius = 2, label = ~htmlEscape(name),
                         # Color code colleges by sector using the `pal` color palette
                         color = ~pal(sector_label),
                         # Cluster all colleges using `clusterOptions`
                         clusterOptions = markerClusterOptions()) 

suppressMessages(library(ggmap))
library(maps)
library(mapproj)

#THIS WILL NOT WORK UNLESS YOU INPUT AN API KEY!!!!!
api_key = 'YOURAPIKEY'
register_google(api_key)

#Coordinates
spp = c(long = -76.948270, lat = 38.983640)
# Get map
map = get_map(spp, zoom = 15, scale = 1)
# Plot map
ggmap(map)

dfworldmap = map_data("world")
dfworldmap %>%  head(2)

dfworldmap %>% filter(region=='Indonesia') %>% head(2)

us = map_data("state")
us %>% head(3)

data("USArrests")
arrest = USArrests %>% #create new df
  add_rownames("region") %>% # add a new var region
  #format data to work
  mutate(region=tolower(region)) #make it all lowercase

head(arrest) #check that we have our new var!

g = ggplot()
g = g + geom_map(data=us, map=us,
                    aes(x=long, y=lat, map_id=region),
                    fill="#ffffff", color="#ffffff", size=0.15)
g = g + geom_map(data=arrest, map=us,
                    aes(fill=Murder, map_id=region),
                    color="#ffffff", size=0.15)
g

library(lubridate) # date operations
library(gridExtra)

head(mtcars, 5)

ggplot(mtcars, aes(x = mpg, y = wt)) + 
geom_point()

ggplot(mtcars, aes(x = mpg, y = wt)) + 
geom_point()+
ggtitle('Look mom I can scatter plot')+ theme(plot.title = element_text(hjust = 0.5)) #title

ggplot(mtcars, aes(x = mpg, y = wt)) + 
geom_point(alpha=.5, color='red')


url = 'https://raw.githubusercontent.com/corybaird/PLCY_610_public/master/Reference_materials/Tutorials_R_Stata_Python/R/W3_ggplot/global_covid.csv'
df = read.csv(url)
head(df,3)

ggplot(df%>%filter(confirm>10000), aes(x=name, y=confirm)) + 
geom_bar(stat="identity", width=.5, fill='red',color='blue')+  
theme(axis.text.x = element_text(angle=65, vjust=0.6))

ggplot(df%>%arrange(desc(confirm))%>%filter(confirm>10000), aes(x=reorder(name, confirm), y=confirm)) + 
geom_bar(stat="identity", fill='blue')+  
theme(axis.text.x = element_text(angle=65, vjust=0.6))

url = 'https://raw.githubusercontent.com/corybaird/PLCY_610_public/master/Reference_materials/Tutorials_R_Stata_Python/R/W3_ggplot/Covid_TS_global.csv'
df = read.csv(url)
df$date = as.Date(df$date)
head(df, 2)


japan = df %>% filter(country=='Japan')
head(japan,2)

japan %>% 
ggplot(aes(x=date, y=confirmed)) + geom_line(alpha=0.5)

df %>% filter(country=='Italy'| country=="US") %>% 
ggplot(aes(x=date, y=confirmed)) + geom_line(aes(color=country))+ 
theme(axis.title.x=element_blank())

plot1 = df %>% filter(confirmed>1000)  %>%  filter(country=='Italy'| country=="US") %>% 
ggplot(aes(x=date, y=confirmed)) + geom_line(aes(color=country))+ 
theme(axis.title.x=element_blank())+
xlab('') + ylab('Count') + labs(title='Current Confirmed Cases') + theme(axis.text.x=element_text(angle=45, hjust=1))

plot2 = df %>% filter(deaths>100) %>%filter(country=='Italy'| country=="US") %>% 
ggplot(aes(x=date, y=deaths)) + geom_line(aes(color=country))+ 
theme(axis.title.x=element_blank())+
xlab('') + ylab('Count') + labs(title='Current Deaths') + theme(axis.text.x=element_text(angle=45, hjust=1))

grid.arrange(plot1, plot2, ncol=2)


