#Final Project 


#A project-specific ‘R’ project + Git Hub repository (see name of project) ----
#evidence of version control ... (Show GitHub repository) ----

library(readxl)

#read in data (see below) ----
data <- read_xlsx('SFBVEG2013.xlsx')


#indexing ----
names(data)
species <- unique(data$Species)
named <- species[3:9]
#subsetting ----

jc1 <- subset(x = data, data$TransectID == 'Jaumea carnosa', select = c('Height', 'Distance', 'Density'))
jc1
jc2 <- subset(x=data, data$Species == 'Jaumea carnosa', select = c('Canopy Height', '% Cover'))
jc2

#application of appropriate data storage structure? ---- 
jc1 <- as.data.frame(jc1)
jc2 <- as.data.frame(jc2)

#merging data frames---- 

jc1.0 <- merge(x=jc1, y =jc2)
jc1.0

jc1.0[,c(1,4)] <- NULL
head(jc1.0)

#ordering and summarizing ---- 
library(tidyverse)

names(data)


data$Distance
data$`% Cover` <- as.numeric(data$`% Cover`)

sp.info <- data %>% group_by(Species) %>% 
  summarise(avg.Cover = mean(`% Cover`, na.rm=T), Cover.stdev = sd(`% Cover`))
sp.info











