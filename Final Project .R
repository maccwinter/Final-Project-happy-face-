#Final Project 


#A project-specific ‘R’ project + Git Hub repository (see name of project) ----
#evidence of version control ... (Show GitHub repository) ----

library(readxl)

#read in data (see below) ----
data <- read_xlsx('SFBVEG2013.xlsx')


#indexing ----
names(data)
species <- unique(data$Species)

#subsetting ----

jc1 <- subset(x = data, data$Species == 'Jaumea carnosa',
              select = c('Canopy Height', 'Density'))
jc1
jc2 <- subset(x=data, data$Species == 'Jaumea carnosa', 
              select = c('Canopy Height', '% Cover'))
jc2


#ordering and summarizing ---- 
library(tidyverse)

names(data)

data$`% Cover` <- as.numeric(data$`% Cover`)

sp.cover <- data %>% group_by(Species) %>% 
  summarise(avg.Cover = mean(`% Cover`, na.rm=T), Cover.stdev = sd(`% Cover`, na.rm = T))
sp.cover

sp.Density <- data %>% group_by(Species) %>% 
  summarise(avg.Density = mean(`Density`, na.rm=T), Density.stdev = sd(`Density`, na.rm = T))
sp.Density

sp.CD <- merge(x=sp.cover, y = sp.Density, by = 'Species' )
sp.CD
sp.CD[3,] 
sp.CD <- sp.CD[-3,]
sp.CD

#use if else and for loop ---- 

coverabundance <- function(x){
  cover <- x[,2]
  
if(cover > 80){ abundance <- 'abundant' }
  else if(cover < 10){abundance <- 'medium'}
  else{abundance <- 'low'}
  return(abundance)
}
sp.CD$abundance <- NA
for(i in 1:nrow(sp.CD)){sp.CD[i,]$abundance <- coverabundance(x = sp.CD[i,])}
sp.CD

#reshape data with melt ---- 

library(reshape2)

sp.AB <- melt(data=sp.CD, id.vars = c('abundance'), measure.vars = 'Species')
sp.AB

#for loop again ----- 
realdeal <- function(x){
  plant <- x[,13]
  if(plant == 'Salicornia pacifica'){abundance <- 'abundant'}
  else if(plant == 'Cuscuta salina'){abundance <- 'medium'}
  else if(plant =='Limonium californicum'){abundance <- 'medium'}
  else{abundance <- 'low'}
  return(abundance)
}

data$abundance <- NA
for(i in 1:nrow(data)){data[i,]$abundance <- realdeal(x = data[i,])}
unique(data$abundance)

#Oh dang, I could have done that with ddply. lol.  

#Use ddply ----- 

library(plyr)
data1 <- ddply(.data = data, .variables = 'Species', function(x){
  p = unique(x$Species)
  abundancy <- function(y){
    if(y == 'Salicornia pacifica') q <- 'abundant'
    else if(y == 'Cuscuta salina') q <- 'medium'
    else if(y=='Limonium californicum') q <- 'medium'
    else q <- 'low'
  }
  x$abundance <- abundancy(y=p)
  return(x)
}, .inform=T, .progress = "text")
names(data1)

#export data set ---- 

save(data1, file ="SFBVEG2013.Rdata")

library(ggplot2)

#create histogram ---- 

ggplot(data = data1, aes(x = `% Cover`)) + 
  geom_histogram(binwidth = 5) +
  facet_wrap(.~abundance) 

names(data1)


#ggplot with 2 geoms 

ggplot(data=data1, aes(abundance, `% Cover`)) +
  geom_bar() 
  
ggplot(data1, aes(`% Cover`, Density, color = abundance)) +
  geom_point() +
  
  ggplot(data1) +
  geom_bar(aes(x=abundance, y=`% Cover`, fill = Density), position="stack", stat="identity") 
 
#ggplot and exporting and saving figures ---- 
  
ddply(.data=data1, .variables = 'TransectID', function(x){
  name=unique(x$TransectID)
pl <- ggplot(data1, aes(abundance,`% Cover` , color = Density)) +
  geom_point() + 
  scale_color_continuous(type='viridis') + 
  geom_count() +
  theme_linedraw() +
ggtitle(name)
ggsave(filename= paste0(name,'.tiff'),plot=pl, width =4, height=3, units='in',
       dpi=600, compression = 'lzw') }, .progress ='text')

#create map of sampling locations ---- 
  
library(tidyverse)
install.packages('ggmap')
install.packages("osmdata")
library(ggmap)
library(osmdata)

names(data1)

data1$Lat <- as.numeric(data1$Lat, na.rm =T)
data1$Long <- as.numeric(data1$Long, na.rm =T)

bb <- c(left=min(data1$Long - 0.007, na.rm = T), bottom = min(data1$Lat - 0.007, na.rm = T),
        right = max(data1$Long + 0.007, na.rm = T), top = max(data1$Lat + 0.007, na.rm = T))
bb


map <- get_stamenmap(bbox = bb, zoom = 15, map = 'terrain-background')

ggmap(map) +
  geom_point(data = data1, aes(x=Long, y = Lat))





