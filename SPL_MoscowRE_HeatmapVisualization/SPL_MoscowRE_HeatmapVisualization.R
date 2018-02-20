#Name of Quantlet:  SPL_HeatmapVisualization
#Published in:      Statistical Programming Languages
#Description:       This quantlet visualizes the distribution of different prices per square meter for the districts of Moscow.
#Keywords:          heatmap, moscow, housing prices, visualization, price distribution, ggplot2, ggmap
#Author:            David Berscheid
# Submitted:        Fr, Aug 18 2017

rm(list=ls())

#Reference: This code is partly based on https://www.kaggle.com/c/sberbank-russian-housing-market/discussion/32201


## --------------------------Packages & Libraries -------------------------- ##
libraries = c("data.table", "stringr","DT","sp","rgdal","RColorBrewer","ggplot2", "ggthemes","ggmap", "grDevices","dplyr")
lapply(libraries,function(x)if(!(x %in% installed.packages())){install.packages(x)})
lapply(libraries,require,quietly=TRUE,character.only=TRUE)

#problems loading the packages if done differently
if(!require('rgeos')) install.packages('rgeos', type="source"); library(rgeos)
if(!require('rgdal')) install.packages('rgdal', type="source"); library(rgdal)

## --------------------------Data Import-------------------------- ##
#read in data sets & shapefile
train = read.csv("train.csv", sep = ",", header = TRUE)
shapefile = readOGR(dsn = "administrative-divisions-of-moscow", layer = "moscow_adm")
          
## --------------------------Preprocessing--------------------------- ##
#define longitude & latitude based on polygons
position = shapefile@bbox
#define the centers
center = coordinates(shapefile)
shapefileMoscow = shapefile
shapefile$long_c = center[,1]
shapefile$lat_c = center[,2]
          
#data preparation
trainAdj = train %>%
  # to avoid division by 0
  filter(full_sq > 0) %>% 
  #calculate price per square meter                                                                                
  mutate(pricePerSq = price_doc/full_sq) %>%   
    #group the data set by districts
    group_by(sub_area) %>%
      #calculate mean for one district
      summarise(meanPricePerSq = mean(pricePerSq))

#add variable of interest to shapefile
shapefileMoscow = merge(shapefileMoscow, trainAdj, by.x ="RAION", by.y= "sub_area") 


## --------------------------Define Settings--------------------------- ##
#set parameters for heatmap
#heatcolors from green to red (from cheap to expensive)
difColors = 30
heatColors = colorRampPalette(c("#FFFFFF","#FFFF00", "#FF0000"))(difColors)
#get satellite map of moscow
moscowMap = get_map(location = "moscow", zoom = 6,
                maptype = "satellite",
                color = "color") 
#define a grid for the heatmap
grid = list("grid.raster", moscowMap,
             x = mean(position["x",]),
             y = mean(position["y",]),
             width = position["x",2]-position["x",1],
             height = position["y",2]-position["y",1],
             default.units = "native", first = TRUE)

## --------------------------Plot Heatmap--------------------------- ##
#plot the heatmap          
spplot(shapefileMoscow, c("meanPricePerSq"), sp.layout = grid, main = "House Price Distribution by Moscow's Districts", col.regions = heatColors,
       sub = "", cuts = difColors-1, col = "black", lwd = 1)


