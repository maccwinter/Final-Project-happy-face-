#Final Project 


#A project-specific ‘R’ project + Git Hub repository (see name of project)
#evidence of version control ... (Show GitHub repository) 

library(readxl)

#read in data (see below)
data <- read_xlsx('SFBVEG2013.xlsx')


#indexing 
names(data)
species <- unique(data$Species)
named <- species[3:9]
#subsetting

jc <- subset(x = data, data$Species == 'Jaumea carnosa', select = c('Height', 'Distance', 'Density'))
jc





