
############ Data import and cleaning

## Importing data
install.packages("foreign")
library(foreign)
tzfac <- read.spss("TZFC71FLSR.SAV", to.data.frame = T)

## Variable names
names(tzfac)
dim(tzfac)
# col: 1200  rows: 1670

## Facility type distribution
table(tzfac$V007)

## Connection to central grid, Reliability of central grid 
table(tzfac$V120A)

## Connection to central grid by fac type, Reliability of central grid 
table(tzfac$V007,tzfac$V120A)

  ## Bar diagram connection to central grid by fac type, Reliability of central grid 
  prop.table(table(tzfac$V007,tzfac$V120A), 2)
  install.packages("ggplot2")
  library(ggplot2)
  ggplot(test, aes(x = V120A)) + geom_bar(aes(fill = V007)) + 
  theme(axis.text.x=element_text(angle = -45, hjust = 0))
  x11()
    
## Back-up with fuel or charged battery
table(tzfac$V120A, tzfac$V120)

  ## alternative calculation
  table(tzfac$V120[tzfac$V120A!="Not connected"])
  table(test$V120[test$V120A =="Not connected"])

## other energy source

## CS  
table(tzfac$V121D)
#no values
  
## No other source
table(tzfac$V121A == "No" & tzfac$V121B == "No" & tzfac$V121C == "No",tzfac$V120A)
## Fuel-operated generator (only) 
table(tzfac$V121A == "Yes" & tzfac$V121B == "No" & tzfac$V121C == "No",tzfac$V120A)
## battery-operated generator (only) 
table(tzfac$V121A == "No" & tzfac$V121B == "Yes" & tzfac$V121C == "No",tzfac$V120A)
## Solar system (only)
table(tzfac$V121A == "No" & tzfac$V121B == "No" & tzfac$V121C == "Yes",tzfac$V120A)
## Fuel and battery
table(tzfac$V121A == "Yes" & tzfac$V121B == "Yes" & tzfac$V121C == "No", tzfac$V120A)
## Fuel and solar
table(tzfac$V121A == "Yes" & tzfac$V121B == "No" & tzfac$V121C == "Yes", tzfac$V120A)
## Battery and solar
table(tzfac$V121A == "No" & tzfac$V121B == "Yes" & tzfac$V121C == "Yes",tzfac$V120A)
## Fuel, battery and solar
table(tzfac$V121A == "Yes" & tzfac$V121B == "Yes" & tzfac$V121C == "Yes", tzfac$V120A)

# 25 with no energy access -> correct?

## Back-up source by functionality

## Fuel-operated generator (only) 
table(tzfac$V121A == "Yes" & tzfac$V121B == "No" & tzfac$V121C == "No",tzfac$V120)
## battery-operated generator (only) 
table(tzfac$V121A == "No" & tzfac$V121B == "Yes" & tzfac$V121C == "No",tzfac$V120)
## Solar system (only)
table(tzfac$V121A == "No" & tzfac$V121B == "No" & tzfac$V121C == "Yes",tzfac$V120)
## Fuel and battery
table(tzfac$V121A == "Yes" & tzfac$V121B == "Yes" & tzfac$V121C == "No", tzfac$V120)
## Fuel and solar
table(tzfac$V121A == "Yes" & tzfac$V121B == "No" & tzfac$V121C == "Yes", tzfac$V120)
## Battery and solar
table(tzfac$V121A == "No" & tzfac$V121B == "Yes" & tzfac$V121C == "Yes",tzfac$V120)
## Fuel, battery and solar
table(tzfac$V121A == "Yes" & tzfac$V121B == "Yes" & tzfac$V121C == "Yes", tzfac$V120)







# Other countries

ghfac <- read.spss("GHFC4IFLSP.SAV", to.data.frame = T)
dim(ghfac)
# 428 937


kefac <- read.spss("KEFC6AFLSR.SAV", to.data.frame = T)
dim(kefac)
# 695 3798


rwfac <- read.spss("RWFC5JFLSR.SAV", to.data.frame = T)
dim(rwfac)
# 538 2520


mwfac <- read.spss("MWFC6JFLSR.SAV", to.data.frame = T)
dim(mwfac)
# 1060 1627


snfac <- read.spss("SNFC71FLSP.SAV", to.data.frame = T)
dim(snfac)
# 452 1743


egfac <- read.spss("EGFC5IFLSP.SAV", to.data.frame = T)
dim(egfac)
# 659 1197


