library(ggplot2)
library(grid)
library(gridExtra)
library(tidyr)
library(dplyr)
library(viridis)
library(oce)
library(vcd)
library(vcdExtra)
library(extracat)

## Set file path
path = "/Users/cynthiaclement/Desktop"


################################# Looking at US Alliances #################################
dyad_alliance = read.csv(paste(path, "version4.1_csv/alliance_v4.1_by_dyad.csv", sep= "/"))
us_alliances <- dyad_alliance[dyad_alliance$ccode1 == 2,] 
alliances <- us_alliances[,c(2:5, 8, 11)]
alliances<- gather(alliances, st_end, year, 	dyad_st_year:dyad_end_year)
ggplot(alliances, aes(x=year, y = state_name2, color = st_end)) + geom_point() 

