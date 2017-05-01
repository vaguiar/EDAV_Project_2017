## Cynthia Clement: Final Project 
## EDAV 
##############################################


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

## define year range to look at 
year_min = 1910
year_usdev_max = 2010 

member_alliances = read.csv(paste(path, "version4.1_csv/alliance_v4.1_by_member.csv", sep= "/"))
NMC = read.csv(paste(path, "NMC/NMC_v4_0.csv", sep= "/"))



## NMC Overall 

#The Composite Index of National Capability (CINC) is a statistical measure of national
#It uses an average of percentages of world totals in six different components. 
#The components represent demographic, economic, and military strength.

#TPR = total population of country ratio
#UPR = urban population of country ratio
#ISPR = iron and steel production of country ratio
#ECR = primary energy consumption ratio
#MER = military expenditure ratio
#MPR = military personnel ratio



# "-9" is missing values => re set them as 0

NMC$cinc[NMC$cinc == -9] <- 0
NMC$irst[NMC$irst == -9] <- NA
NMC$milex[NMC$milex == -9] <- NA
NMC$milper[NMC$milper== -9] <- NA
NMC$pec[NMC$pec == -9] <- NA
NMC$tpop[NMC$tpop == -9] <- NA
NMC$upop[NMC$upop == -9] <- NA




ggplot(NMC, aes(year , cinc, color = stateabb)) + geom_line()

a<- ggplot(NMC[NMC$ccode == 2,], aes(year , cinc, color = stateabb)) + geom_line()
b<- ggplot(NMC[NMC$ccode == 2,], aes(year , irst, color = stateabb)) + geom_line()
c<- ggplot(NMC[NMC$ccode == 2,], aes(year , milex, color = stateabb)) + geom_line()
d<- ggplot(NMC[NMC$ccode == 2,], aes(year , milper, color = stateabb)) + geom_line()
e<- ggplot(NMC[NMC$ccode == 2,], aes(year , pec, color = stateabb)) + geom_line()
f<- ggplot(NMC[NMC$ccode == 2,], aes(year , tpop, color = stateabb)) + geom_line()
g<- ggplot(NMC[NMC$ccode == 2,], aes(year , upop, color = stateabb)) + geom_line()

grid.arrange(a,b,c,d,e,f,g, nrow = 4)

## Current world power 

countries <- c("United States of America", "United Kingdom", "France", "Russia","Germany", "Italy", "Japan", "China")
country_code <- member_alliances$ccode[match(countries, member_alliances$state_name)]

nmc_c  <- c("")
for(code in country_code){
  nmc_c <- rbind(nmc_c, NMC[NMC$ccode == as.numeric(code),])
}

nmc_c <- nmc_c[c(2:nrow(nmc_c)),]
nmc_c$irst <- as.numeric(nmc_c$irst)
nmc_c$milex <- as.numeric(nmc_c$milex)
nmc_c$milper <- as.numeric(nmc_c$milper)
nmc_c$pec <- as.numeric(nmc_c$pec)
nmc_c$tpop <- as.numeric(nmc_c$tpop)
nmc_c$upop <- as.numeric(nmc_c$upop)
nmc_c$cinc <- as.numeric(nmc_c$cinc)
nmc_c$year <- as.numeric(nmc_c$year)    


## create individual plots of the 6 factors in CINC

irst <- ggplot(nmc_c[nmc_c$year > 1930,], aes(year , irst, color = stateabb, group = stateabb)) + geom_point(alpha = 1/100) + geom_line()  + theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ggtitle("Iron and Steel Production")
ex <- ggplot(nmc_c[nmc_c$year > 1930,], aes(year , milex, color = stateabb, group = stateabb)) + geom_point(alpha = 1/100) + geom_line()  + theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ggtitle("Military Expenditures")
per <- ggplot(nmc_c[nmc_c$year > 1930,], aes(year , milper, color = stateabb, group = stateabb)) + geom_point(alpha = 1/100) + geom_line()  + theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ggtitle("Military Personell")
pec <- ggplot(nmc_c[nmc_c$year > 1930,], aes(year , pec, color = stateabb, group = stateabb)) + geom_point(alpha = 1/100) + geom_line()  + theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ggtitle("Primary Energy Consumption Ratio")
tpop <- ggplot(nmc_c[nmc_c$year > 1930,], aes(year , tpop, color = stateabb, group = stateabb)) + geom_point(alpha = 1/100) + geom_line()  + theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ggtitle("Total Population")
upop <- ggplot(nmc_c[nmc_c$year > 1930,], aes(year , upop, color = stateabb, group = stateabb)) + geom_point(alpha = 1/100) + geom_line()  + theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ggtitle("Urban Population")
grid.arrange(irst, ex, per, pec, tpop, upop , nrow = 3)


## create plots of CINC and mark the different conflicts we are looking at 
d=data.frame(x1=c(1914,1939, 1947, 1950, 1955, 2001), x2=c(1918, 1945, 1991, 1953, 1975, 2010), y1=c(0,0,0,0,0,0), y2=c(.4,.4,.4,.4,.4,.4), Conflict=c("WWI", "WWII", "Cold War", "Korean War", "Vietnam War", "Afghanistan War"), r=c(1,2,3,4,5,6))
nmc_yr<- nmc_c[nmc_c$year > 1900,]
ggplot() + 
  scale_x_continuous(name="x") + 
  scale_y_continuous(name="y") +
  geom_rect(data=d, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=Conflict),alpha=0.15) +
  geom_line(data = nmc_yr, aes(x = year, y = cinc, color = stateabb, group = stateabb))


## Heatmap of CNIC 

ggplot(nmc_c[nmc_c$year > 1900,], aes(stateabb, year, fill = cinc)) + geom_tile()+
  scale_fill_viridis() + ggtitle("Heatmap of cinc") + theme(axis.text.x = element_text(angle = 60, hjust = 1),plot.title = element_text(hjust = 0.5))


##PCP PLot 
library(GGally)
ct <- ggparcoord(nmc_c[nmc_c$year>1930,], columns = 4:10, scale = "uniminmax", alphaLines = .2, groupColumn = "stateabb")
yr <- ggparcoord(nmc_c[nmc_c$year>1930,], columns = 4:10, scale = "uniminmax", alphaLines = .2, groupColumn = "year")
grid.arrange(ct, yr, nrow = 2)



##WWI ---------------------- 1914-1918  

### In the aftermath of the war, four empires disappeared: the German, Austro-Hungarian, Ottoman, and Russian. 
### Numerous nations regained their former independence, and new ones were created. 
### Four dynasties, together with their ancillary aristocracies, all fell as a result of the war: the Romanovs, the Hohenzollerns, the Habsburgs, and the Ottomans. 
### Belgium and Serbia were badly damaged, as was France, with 1.4 million soldiers dead,[183] not counting other casualties. 
### Germany and Russia were similarly affected

allied <- c("United States of America", "United Kingdom", "Russia", "Japan", "Italy")
allied_ccode <- member_alliances$ccode[match(allied, member_alliances$state_name)]

central <- c("Germany", "Turkey", "Austria-Hungary", "Romania", "Bulgaria")
central_ccode <- member_alliances$ccode[match(central, member_alliances$state_name)]


WWI_yrMax = 1928
WWI_yrMin = 1904

WWI <- NMC

alliedP_nmc <- c("")
for(code in allied_ccode){
  alliedP_nmc <- rbind(alliedP_nmc, WWI[WWI$ccode == as.numeric(code),])
}
alliedP_nmc  <- alliedP_nmc [c(2:nrow(alliedP_nmc )),]
alliedP_nmc$side = "Allied Powers"

centralP_nmc <- c("")
for(code in central_ccode){
  centralP_nmc <- rbind(centralP_nmc, NMC[NMC$ccode == as.numeric(code),])
}
centralP_nmc <- centralP_nmc[c(2:nrow(centralP_nmc)),]
centralP_nmc$side = "Central Powers"


WWI <-rbind(alliedP_nmc,centralP_nmc)
WWI$milex <- as.numeric(WWI$milex)
WWI$milper <- as.numeric(WWI$milper)
WWI$pec <- as.numeric(WWI$pec)
WWI$tpop <- as.numeric(WWI$tpop)
WWI$upop <- as.numeric(WWI$upop)
WWI$cinc <- as.numeric(WWI$cinc)
WWI$year <- as.numeric(WWI$year)
WWI$irst <- as.numeric(WWI$irst)


WWI <- WWI[WWI$year >= WWI_yrMin, ]
WWI <- WWI[WWI$year <= WWI_yrMax, ]



d=data.frame(x1=c(1939), x2=c(1918, 1945, 1991, 1953, 1975, 2010), y1=c(0,0,0,0,0,0), y2=c(.4,.4,.4,.4,.4,.4), Conflict=c("WWI", "WWII", "Cold War", "Korean War", "Vietnam War", "Afghanistan War"), r=c(1,2,3,4,5,6))
ggplot() + 
  geom_rect(data=d, mapping=aes(xmin=1914, xmax=1918, ymin=0, ymax=.4),alpha=0.1, fill = "salmon") +
  geom_line(data = WWI, aes(x = year, y = cinc, color = stateabb, group = stateabb)) + facet_wrap(~side)


ggplot() + 
  geom_rect(data=d, mapping=aes(xmin=1914, xmax=1918, ymin=0, ymax=max(WWI$irst)),alpha=0.1, fill = "salmon") +
  geom_line(data = WWI, aes(x = year, y = irst, color = stateabb, group = stateabb)) + facet_wrap(~side)

ggplot() + 
  geom_rect(data=d, mapping=aes(xmin=1914, xmax=1918, ymin=0, ymax= max(WW$milex)),alpha=0.1, fill = "salmon") +
  geom_line(data = WWI, aes(x = year, y = milex, color = stateabb, group = stateabb)) + facet_wrap(~side)

ggplot() + 
  geom_rect(data=d, mapping=aes(xmin=1914, xmax=1918, ymin=0, ymax=max(WWI$milper)),alpha=0.1, fill = "salmon") +
  geom_line(data = WWI, aes(x = year, y = milper, color = stateabb, group = stateabb)) + facet_wrap(~side)

ggplot() + 
  geom_rect(data=d, mapping=aes(xmin=1914, xmax=1918, ymin=0, ymax=max(WWI$pec)),alpha=0.1, fill = "salmon") +
  geom_line(data = WWI, aes(x = year, y = pec, color = stateabb, group = stateabb)) + facet_wrap(~side)


ggplot() + 
  geom_rect(data=d, mapping=aes(xmin=1914, xmax=1918, ymin=0, ymax=max(WWI$tpop)),alpha=0.1, fill = "salmon") +
  geom_line(data = WWI, aes(x = year, y = tpop, color = stateabb, group = stateabb)) + facet_wrap(~side)

ggplot() + 
  geom_rect(data=d, mapping=aes(xmin=1914, xmax=1918, ymin=0, ymax=max(WWI$upop)),alpha=0.1, fill = "salmon") +
  geom_line(data = WWI, aes(x = year, y = upop, color = stateabb, group = stateabb)) + facet_wrap(~side)




##WWII  ----------------------- 1939-1945

allies <- c("United States of America", "United Kingdom", "France", "Russia", "Australia", "Belgium", "Brazil", "Canada", "China", "Denmark", "Greece", "Netherlands", "New Zealand", "Norway", "Poland", "South Africa", "Yugoslavia")
allies_ccode <- member_alliances$ccode[match(allies, member_alliances$state_name)]

axis <- c("Germany", "Italy", "Japan", "Hungary", "Romania", "Bulgaria")
axis_ccode <- member_alliances$ccode[match(axis, member_alliances$state_name)]

allies_nmc <- c("")
for(code in allies_ccode){
  allies_nmc <- rbind(allies_nmc, NMC[NMC$ccode == as.numeric(code),])
}
allies_nmc  <- allies_nmc [c(2:nrow(allies_nmc )),]
allies_nmc$side = "Allied Powers"
axis_nmc <- c("")
for(code in axis_ccode){
  axis_nmc <- rbind(axis_nmc, NMC[NMC$ccode == as.numeric(code),])
}
axis_nmc <- axis_nmc[c(2:nrow(axis_nmc)),]
axis_nmc$side = "Axis Powers"

WWII <-rbind(allies_nmc,axis_nmc)
WWII$milex <- as.numeric(WWII$milex)
WWII$milper <- as.numeric(WWII$milper)
WWII$pec <- as.numeric(WWII$pec)
WWII$tpop <- as.numeric(WWII$tpop)
WWII$upop <- as.numeric(WWII$upop)
WWII$cinc <- as.numeric(WWII$cinc)
WWII$year <- as.numeric(WWII$year)

WWII <- WWII[WWII$year < 1950,]
WWII <- WWII[WWII$year > 1934,]


## create plots of CINC and mark the different conflicts we are looking at year 
d=data.frame(x1=c(1939), x2=c(1918, 1945, 1991, 1953, 1975, 2010), y1=c(0,0,0,0,0,0), y2=c(.4,.4,.4,.4,.4,.4), Conflict=c("WWI", "WWII", "Cold War", "Korean War", "Vietnam War", "Afghanistan War"), r=c(1,2,3,4,5,6))
ggplot() + 
  scale_x_continuous(name="x") + 
  scale_y_continuous(name="y") +
  geom_rect(data=d, mapping=aes(xmin=1939, xmax=1945, ymin=0, ymax=.4),alpha=0.1, fill = "salmon") +
  geom_line(data = WWII, aes(x = year, y = cinc, color = stateabb, group = stateabb)) + facet_wrap(~side)


WWII_g <- gather(WWII, factor, value, irst:upop )
WWII_g$value <- as.numeric(WWII_g$value)
ggplot() + 
  scale_x_continuous(name="x") + 
  scale_y_continuous(name="y") +
  geom_rect(data=d, mapping=aes(xmin=1939, xmax=1945, ymin=0, ymax=.4),alpha=0.1, fill = "salmon") +
  geom_line(data = WWII_g, aes(x = year, y = value, color = stateabb, group = stateabb)) + facet_wrap(~factor)



##Cold War -------------------- 1947-1991  the m
##Korean War ------------------ 1950-1953  
##Vietnam War ----------------- 1955-1975  
##War in Afghanistan ---------- 2001-2010  








