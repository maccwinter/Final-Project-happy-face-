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

jc1 <- subset(x = data, data$TransectID == 'Jaumea carnosa', select = c('Canopy Height', 'Density'))
jc1
jc2 <- subset(x=data, data$Species == 'Jaumea carnosa', select = c('Canopy Height', '% Cover'))
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





