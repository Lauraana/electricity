
############ Data import and cleaning

setwd("C:/Users/Laura/Documents/electricity")

############################################ TANZANIA
  table(tzfac$V120)
  
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
  
  ## Missing values
  table(tzfac$V007, is.na(tzfac$V120A))
  # 7 other hospital (private), 1 health center, 1 clinic, 3 dispensaries
  
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
  
  ## other energy source by facility
  
  ## No other source
  table(tzfac$V121A == "No" & tzfac$V121B == "No" & tzfac$V121C == "No",tzfac$V120A, tzfac$V007)
  ## Fuel-operated generator (only) 
  table(tzfac$V121A == "Yes" & tzfac$V121B == "No" & tzfac$V121C == "No",tzfac$V120A, tzfac$V007)
  ## battery-operated generator (only) 
  table(tzfac$V121A == "No" & tzfac$V121B == "Yes" & tzfac$V121C == "No",tzfac$V120A, tzfac$V007)
  ## Solar system (only)
  table(tzfac$V121A == "No" & tzfac$V121B == "No" & tzfac$V121C == "Yes",tzfac$V120A, tzfac$V007)
  ## Fuel and battery
  table(tzfac$V121A == "Yes" & tzfac$V121B == "Yes" & tzfac$V121C == "No", tzfac$V120A, tzfac$V007)
  ## Fuel and solar
  table(tzfac$V121A == "Yes" & tzfac$V121B == "No" & tzfac$V121C == "Yes", tzfac$V120A, tzfac$V007)
  ## Battery and solar
  table(tzfac$V121A == "No" & tzfac$V121B == "Yes" & tzfac$V121C == "Yes",tzfac$V120A, tzfac$V007)
  ## Fuel, battery and solar
  table(tzfac$V121A == "Yes" & tzfac$V121B == "Yes" & tzfac$V121C == "Yes", tzfac$V120A, tzfac$V007)
  
  
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
  
  # Variables
  
  # connected (without don´t know)
  connected <- (tzfac$V120A !=  "Don't know if connected")
  table(connected)
  
  # solar(only)
  solar <- as.character(tzfac$V121A == "No" & tzfac$V121B == "No" & tzfac$V121C == "Yes")
  table(solar)
  
  # Categories reliability 
  
  # 1: Reliable
  reliable <- as.numeric(tzfac$V120A == "Connected, always available") 
  table(reliable)

  # 2: Sometimes interrupted but Functional back-up
  interfunctback <- as.numeric(tzfac$V120A == "Connected, sometimes interrupted" & tzfac$V120 == "Reported functional with fuel")
  table(interfunctback)
  
  # 3: Sometimes interrupted, no functional back-up
  internofunctback <- as.numeric(tzfac$V120A == "Connected, sometimes interrupted" & tzfac$V120 != "Reported functional with fuel")
  table(internofunctback)
  
  # 4: Not connected functional back-up
  notconnectedfuncback <- as.numeric(tzfac$V120A =="Not connected" & tzfac$V120 == "Reported functional with fuel")
  table(notconnectedfuncback)
  
  # 5: Not connected no functional back-up (without only solar)
  notconnectednofuncback <- as.numeric(tzfac$V120A =="Not connected" & tzfac$V120 == "No backup generator" & solar == "FALSE")
  table(notconnectednofuncback)
  
  # 6: Only solar
  onlysolar <- as.numeric(tzfac$V120A =="Not connected" & solar == "TRUE")
  table(onlysolar)



  table(tzfac$V177B, reliable)

###################################################MALAWI
setwd("C:/Users/Laura/Documents/electricity")

install.packages("foreign")
library(foreign)
mwfac <- read.spss("MWFC6JFLSR.SAV", to.data.frame = T)

## Variable names
names(mwfac)
dim(mwfac)
# col: 1060   rows: 1627

## Facility type distribution
table(mwfac$V007)

## Missing values
table(mwfac$V007, is.na(mwfac$V120A))
# 1 Central hospital, 2 other hospital, 11 health centre, 1 maternity, 8 dispensary, 52 clinic, 8 health post

## Connection to central grid, Reliability of central grid 
table(mwfac$V120A)

## Connection to central grid by fac type, Reliability of central grid 
table(mwfac$V007,mwfac$V120A)

## Bar diagram connection to central grid by fac type, Reliability of central grid 
prop.table(table(mwfac$V007,mwfac$V120A), 2)
install.packages("ggplot2")
library(ggplot2)
ggplot(test, aes(x = V120A)) + geom_bar(aes(fill = V007)) + 
  theme(axis.text.x=element_text(angle = -45, hjust = 0))
x11()

## Back-up with fuel or charged battery 
table(mwfac$V120A, mwfac$V120)

## alternative calculation
table(mwfac$V120[mwfac$V120A!="Not connected"])
table(test$V120[test$V120A =="Not connected"])

## other energy source

## CS  
table(mwfac$V121D)
#no values

## No other source
table(mwfac$V121A == "No" & mwfac$V121B == "No" & mwfac$V121C == "No" & mwfac$V121D =="No",mwfac$V120A)
## Fuel-operated generator (only) 
table(mwfac$V121A == "Yes" & mwfac$V121B == "No" & mwfac$V121C == "No" & mwfac$V121D =="No",mwfac$V120A)
## battery-operated generator (only) 
table(mwfac$V121A == "No" & mwfac$V121B == "Yes" & mwfac$V121C == "No" & mwfac$V121D =="No",mwfac$V120A)
## Solar system (only)
table(mwfac$V121A == "No" & mwfac$V121B == "No" & mwfac$V121C == "Yes" & mwfac$V121D =="No",mwfac$V120A)
## CS (only)
table(mwfac$V121A == "No" & mwfac$V121B == "No" & mwfac$V121C == "No" & mwfac$V121D =="Yes",mwfac$V120A)
## Fuel and battery
table(mwfac$V121A == "Yes" & mwfac$V121B == "Yes" & mwfac$V121C == "No" & mwfac$V121D =="No", mwfac$V120A)
## Fuel and solar
table(mwfac$V121A == "Yes" & mwfac$V121B == "No" & mwfac$V121C == "Yes" & mwfac$V121D =="No", mwfac$V120A)
## Fuel and CS
table(mwfac$V121A == "Yes" & mwfac$V121B == "No" & mwfac$V121C == "No" & mwfac$V121D =="Yes", mwfac$V120A)
## Battery and solar
table(mwfac$V121A == "No" & mwfac$V121B == "Yes" & mwfac$V121C == "Yes" & mwfac$V121D =="No",mwfac$V120A)
## Battery and CS
table(mwfac$V121A == "No" & mwfac$V121B == "Yes" & mwfac$V121C == "No" & mwfac$V121D =="Yes",mwfac$V120A)
## solar and CS
table(mwfac$V121A == "No" & mwfac$V121B == "No" & mwfac$V121C == "Yes" & mwfac$V121D =="Yes",mwfac$V120A)
## Fuel, battery and solar
table(mwfac$V121A == "Yes" & mwfac$V121B == "Yes" & mwfac$V121C == "Yes" & mwfac$V121D =="No", mwfac$V120A)
## Fuel, battery and CS
table(mwfac$V121A == "Yes" & mwfac$V121B == "Yes" & mwfac$V121C == "No" & mwfac$V121D =="Yes", mwfac$V120A)
## Fuel, solar and CS
table(mwfac$V121A == "Yes" & mwfac$V121B == "No" & mwfac$V121C == "Yes" & mwfac$V121D =="Yes", mwfac$V120A)
## Battery, solar and CS
table(mwfac$V121A == "No" & mwfac$V121B == "Yes" & mwfac$V121C == "Yes" & mwfac$V121D =="Yes",mwfac$V120A)
## Fuel, battery, solar and CS
table(mwfac$V121A == "Yes" & mwfac$V121B == "Yes" & mwfac$V121C == "Yes" & mwfac$V121D =="Yes", mwfac$V120A)


## other energy source by facility

## No other source
table(mwfac$V121A == "No" & mwfac$V121B == "No" & mwfac$V121C == "No" & mwfac$V121D =="No" & mwfac$V120 !="Reported, don't know if functional", mwfac$V120A, mwfac$V007)
## Fuel-operated generator (only) 
table(mwfac$V121A == "Yes" & mwfac$V121B == "No" & mwfac$V121C == "No" & mwfac$V121D =="No" & mwfac$V120 !="Reported, don't know if functional", mwfac$V120A, mwfac$V007)
## battery-operated generator (only) 
table(mwfac$V121A == "No" & mwfac$V121B == "Yes" & mwfac$V121C == "No" & mwfac$V121D =="No" & mwfac$V120 !="Reported, don't know if functional", mwfac$V120A, mwfac$V007)
## Solar system (only)
table(mwfac$V121A == "No" & mwfac$V121B == "No" & mwfac$V121C == "Yes" & mwfac$V121D =="No" & mwfac$V120 !="Reported, don't know if functional", mwfac$V120A, mwfac$V007)
## CS (only)
table(mwfac$V121A == "No" & mwfac$V121B == "No" & mwfac$V121C == "No" & mwfac$V121D =="Yes" & mwfac$V120 !="Reported, don't know if functional", mwfac$V120A, mwfac$V007)
## Fuel and battery
table(mwfac$V121A == "Yes" & mwfac$V121B == "Yes" & mwfac$V121C == "No" & mwfac$V121D =="No" & mwfac$V120 !="Reported, don't know if functional", mwfac$V120A, mwfac$V007)
## Fuel and solar
table(mwfac$V121A == "Yes" & mwfac$V121B == "No" & mwfac$V121C == "Yes" & mwfac$V121D =="No" & mwfac$V120 !="Reported, don't know if functional", mwfac$V120A, mwfac$V007)
## solar and CS
table(mwfac$V121A == "No" & mwfac$V121B == "No" & mwfac$V121C == "Yes" & mwfac$V121D =="Yes" & mwfac$V120 !="Reported, don't know if functional", mwfac$V120A, mwfac$V007)

## Back-up source by functionality

## Fuel-operated generator (only) 
table(mwfac$V121A == "Yes" & mwfac$V121B == "No" & mwfac$V121C == "No" & mwfac$V121D =="No",mwfac$V120)
## battery-operated generator (only) 
table(mwfac$V121A == "No" & mwfac$V121B == "Yes" & mwfac$V121C == "No" & mwfac$V121D =="No",mwfac$V120)
## Solar system (only)
table(mwfac$V121A == "No" & mwfac$V121B == "No" & mwfac$V121C == "Yes" & mwfac$V121D =="No",mwfac$V120)
## CS (only)
table(mwfac$V121A == "No" & mwfac$V121B == "No" & mwfac$V121C == "No" & mwfac$V121D =="Yes",mwfac$V120)
## Fuel and battery
table(mwfac$V121A == "Yes" & mwfac$V121B == "Yes" & mwfac$V121C == "No" & mwfac$V121D =="No", mwfac$V120)
## Fuel and solar
table(mwfac$V121A == "Yes" & mwfac$V121B == "No" & mwfac$V121C == "Yes" & mwfac$V121D =="No", mwfac$V120)
## solar and CS
table(mwfac$V121A == "No" & mwfac$V121B == "No" & mwfac$V121C == "Yes" & mwfac$V121D =="Yes",mwfac$V120)
## Fuel, battery and solar
table(mwfac$V121A == "Yes" & mwfac$V121B == "Yes" & mwfac$V121C == "Yes" & mwfac$V121D =="No", mwfac$V120)
## Fuel, battery and CS
table(mwfac$V121A == "Yes" & mwfac$V121B == "Yes" & mwfac$V121C == "No" & mwfac$V121D =="Yes", mwfac$V120)
## Fuel, solar and CS
table(mwfac$V121A == "Yes" & mwfac$V121B == "No" & mwfac$V121C == "Yes" & mwfac$V121D =="Yes", mwfac$V120)
## Battery, solar and CS
table(mwfac$V121A == "No" & mwfac$V121B == "Yes" & mwfac$V121C == "Yes" & mwfac$V121D =="Yes",mwfac$V120)
## Fuel, battery, solar and CS
table(mwfac$V121A == "Yes" & mwfac$V121B == "Yes" & mwfac$V121C == "Yes" & mwfac$V121D =="Yes", mwfac$V120)

# Variables

# connected (without don´t know)
connected <- (mwfac$V120A !=  "Don't know if connected")
table(connected)

# solar(only)
solar <- as.character(mwfac$V121A == "No" & mwfac$V121B == "No" & mwfac$V121C == "Yes" & mwfac$V121D =="No")
table(solar)

# Categories 

# 1: Reliable
reliable <- as.numeric(mwfac$V120A == "Connected, always available") 
table(reliable)

# 2: Sometimes interrupted but Functional back-up
interfunctback <- as.numeric(mwfac$V120A == "Connected, sometimes interrupted" & mwfac$V120 == "Reported functional with fuel")
table(interfunctback)

# 3: Sometimes interrupted, no functional back-up
internofunctback <- as.numeric(mwfac$V120A == "Connected, sometimes interrupted" & mwfac$V120 != "Reported functional with fuel" & mwfac$V120 != "Reported, don't know if functional")
table(internofunctback)

# 4: Not connected functional back-up
notconnectedfuncback <- as.numeric(mwfac$V120A =="Not connected" & mwfac$V120 == "Reported functional with fuel")
table(notconnectedfuncback)

# 5: Not connected no functional back-up (without only solar)
notconnectednofuncback <- as.numeric(mwfac$V120A =="Not connected" & (mwfac$V120 == "No backup generator" | mwfac$V120 == "Reported functional, no fuel, DK fuel" | mwfac$V120 == "Reported not functional") & solar == "FALSE")
table(notconnectednofuncback)

# 6: Only solar
onlysolar <- as.numeric(mwfac$V120A =="Not connected" & solar == "TRUE")
table(onlysolar)

# Categories by type

# 1: reliable
table(reliable, mwfac$V007)

# 2: Sometimes interrupted but Functional back-up
table(interfunctback, mwfac$V007)

# 3: Sometimes interrupted, no functional back-up
table(internofunctback, mwfac$V007)

# 4: Not connected functional back-up
table(notconnectedfuncback, mwfac$V007)

# 5: Not connected no functional back-up (without only solar)
table(notconnectednofuncback, mwfac$V007)

# 6: Only solar
table(onlysolar, mwfac$V007)

# category
install.packages("dplyr")
library(dplyr)

categories <- data.frame(reliable, interfunctback, internofunctback, notconnectedfuncback, notconnectednofuncback, onlysolar)
rowwise(categories) 
mutate(categories, category = ifelse(reliable == 1, 1,
                                   ifelse(interfunctback == 1, 2,
                                          ifelse(internofunctback == 1, 3,
                                                 ifelse(notconnectedfuncback == 1, 4,
                                                        ifelse(notconnectednofuncback == 1, 5,
                                                               ifelse(onlysolar == 1, 6, 0)))))))



# No of visits
table(mwfac$V010C)

# Zone
table(mwfac$SFZONE)

# Services PMTCT
table(mwfac$V1260)

# Blood typing
table(mwfac$V034A)

# Blood transfusion 
table(mwfac$V034B)

# Inpatient by factype
table(mwfac$V007,mwfac$V010B)

# 24 hours by factype
table(mwfac$V101A, mwfac$V007)
table(mwfac$V502, mwfac$V007)  #yes, no

# No of client outpatient visits
table(mwfac$V134, mwfac$V007)

# No of live discharges
table(mwfac$V135, mwfac$V007)

# Autoclave
table(mwfac$V177B, mwfac$V007)

# Correlations
cor(onlysolar[mwfac$V007], mwfac$V134[mwfac$V007], use = "complete.obs")
cor(onlysolar, mwfac$V134, use = "complete.obs")

cor(reliable[mwfac$V007], mwfac$V135[mwfac$V007], use = "complete.obs")




###################################################SENEGAL

install.packages("foreign")
library(foreign)
snfac <- read.spss("SNFC71FLSP.SAV", to.data.frame = T)

## Variable names
names(snfac)
dim(snfac)
# col: 452   rows: 1743

## Facility type distribution
table(snfac$"FACTYPE")

## Missing values
table(snfac$"FACTYPE", is.na(snfac$Q340))
# no missing values

## Connection to central grid, Reliability of central grid 
table(snfac$Q340)
table(snfac$Q340, snfac$Q341)

## Connection to central grid by fac type, Reliability of central grid 
table(snfac$"FACTYPE", snfac$Q340)
table(snfac$"FACTYPE", snfac$Q340, snfac$Q341)

## Back-up with fuel or charged battery 
table(snfac$Q340 == "No" & snfac$Q342 == "No other source")
table(snfac$Q340 == "No" & snfac$Q342 == "Yes" & snfac$Q345 == "Yes" & snfac$Q346 == "Yes")
table(snfac$Q340 == "No" & snfac$Q342 == "Yes" & snfac$Q345 == "Yes" & snfac$Q346 != "Yes")
table(snfac$Q340 == "No" & snfac$Q342 == "Yes" & snfac$Q345 == "No")

table(snfac$Q340 == "Yes" &  snfac$Q341 == "Always available" &  snfac$Q342 == "No other source")
table(snfac$Q340 == "Yes" &  snfac$Q341 == "Always available" &  snfac$Q342 == "Yes" & snfac$Q345 == "Yes" & snfac$Q346 == "Yes")
table(snfac$Q340 == "Yes" &  snfac$Q341 == "Always available" &  snfac$Q342 == "Yes" & snfac$Q345 == "Yes" & snfac$Q346 == "No")
table(snfac$Q340 == "Yes" &  snfac$Q341 == "Always available" &  snfac$Q342 == "Yes" & snfac$Q345 == "No")

table(snfac$Q340 == "Yes" &  snfac$Q341 == "Sometimes interrupted" &  snfac$Q342 == "No other source")
table(snfac$Q340 == "Yes" &  snfac$Q341 == "Sometimes interrupted" &  snfac$Q342 == "Yes" & snfac$Q345 == "Yes" & snfac$Q346 == "Yes")
table(snfac$Q340 == "Yes" &  snfac$Q341 == "Sometimes interrupted" & snfac$Q342 == "Yes" & snfac$Q345 == "Yes" & snfac$Q346 == "No")
table(snfac$Q340 == "Yes" &  snfac$Q341 == "Sometimes interrupted" &  snfac$Q342 == "Yes" & snfac$Q345 == "No")

table(snfac$Q340 == "Yes" &  snfac$Q341 == "Don't know" &  snfac$Q342 == "No other source")
table(snfac$Q340 == "Yes" &  snfac$Q341 == "Don't know" &  snfac$Q342 == "Yes" & snfac$Q345 == "Yes" & snfac$Q346 == "Yes")
table(snfac$Q340 == "Yes" &  snfac$Q341 == "Don't know" & snfac$Q342 == "Yes" & snfac$Q345 == "Yes" & snfac$Q346 == "No")
table(snfac$Q340 == "Yes" &  snfac$Q341 == "Don't know" &  snfac$Q342 == "Yes" & snfac$Q345 == "No")

table(snfac$Q340)
table(snfac$Q341)
table(snfac$Q342)
table(snfac$Q343)
table(snfac$Q345)
table(snfac$Q346)

## Other energy source
# A: fuel; B: battery; c: solar
## Not connected
table(snfac$Q340 == "No", snfac$Q343)
## Connected, always available
table(snfac$Q340 == "Yes" & snfac$Q341 == "Always available", snfac$Q343)
## Connected, sometimes interrupted
table(snfac$Q340 == "Yes" & snfac$Q341 == "Sometimes interrupted", snfac$Q343)
## Connected, DK if interrupted in last 7 days
table(snfac$Q340, snfac$Q341, snfac$Q343)

## other energy source by facility
## Not connected
table(snfac$"FACTYPE", snfac$Q340 == "No", snfac$Q343)
## Connected, always available
table(snfac$"FACTYPE", snfac$Q340 == "Yes" & snfac$Q341 == "Always available", snfac$Q343)
## Connected, sometimes interrupted
table(snfac$"FACTYPE", snfac$Q340 == "Yes" & snfac$Q341 == "Sometimes interrupted", snfac$Q343)
## Connected, DK if interrupted in last 7 days
table(snfac$"FACTYPE", snfac$Q340, snfac$Q341, snfac$Q343)

## Back-up source by functionality

table(snfac$Q342 == "Yes", snfac$Q343,  snfac$Q345 == "Yes", snfac$Q346 == "Yes")
table(snfac$Q342 == "Yes", snfac$Q343,  snfac$Q345 == "Yes", snfac$Q346 == "No")
table(snfac$Q342 == "Yes", snfac$Q343,  snfac$Q345 == "No", snfac$Q346 == "Yes")
table(snfac$Q342 == "Yes", snfac$Q343,  snfac$Q345 == "No")
table(snfac$Q342 == "Yes", snfac$Q343,  snfac$Q345 == "DonÂ´t know")


#################################################Haiti

install.packages("foreign")
library(foreign)
htfac <- read.spss("HTFC6AFLSR.SAV", to.data.frame = T)

## Variable names
names(htfac)
dim(htfac)
# col: 907  rows: 1605

## Facility type distribution
table(htfac$V007)

## Connection to central grid, Reliability of central grid 
table(htfac$V120A)

## Missing values
table(htfac$V007, is.na(htfac$V120A))
# 1 Health center without LIT, 1 dispensary

## Connection to central grid by fac type, Reliability of central grid 
table(htfac$V007,htfac$V120A)

## Bar diagram connection to central grid by fac type, Reliability of central grid 
prop.table(table(htfac$V007,htfac$V120A), 2)
install.packages("ggplot2")
library(ggplot2)
ggplot(test, aes(x = V120A)) + geom_bar(aes(fill = V007)) + 
  theme(axis.text.x=element_text(angle = -45, hjust = 0))
x11()

## Back-up with fuel or charged battery
table(htfac$V120A, htfac$V120)

## alternative calculation
table(htfac$V120[htfac$V120A!="Not connected"])
table(test$V120[test$V120A =="Not connected"])

## other energy source

## CS  
table(htfac$V121D)
# 289 ??

## No other source
table(htfac$V121A == "No" & htfac$V121B == "No" & htfac$V121C == "No",htfac$V120A)
## Fuel-operated generator (only) 
table(htfac$V121A == "Yes" & htfac$V121B == "No" & htfac$V121C == "No",htfac$V120A)
## battery-operated generator (only) 
table(htfac$V121A == "No" & htfac$V121B == "Yes" & htfac$V121C == "No",htfac$V120A)
## Solar system (only)
table(htfac$V121A == "No" & htfac$V121B == "No" & htfac$V121C == "Yes",htfac$V120A)
## Fuel and battery
table(htfac$V121A == "Yes" & htfac$V121B == "Yes" & htfac$V121C == "No", htfac$V120A)
## Fuel and solar
table(htfac$V121A == "Yes" & htfac$V121B == "No" & htfac$V121C == "Yes", htfac$V120A)
## Battery and solar
table(htfac$V121A == "No" & htfac$V121B == "Yes" & htfac$V121C == "Yes",htfac$V120A)
## Fuel, battery and solar
table(htfac$V121A == "Yes" & htfac$V121B == "Yes" & htfac$V121C == "Yes", htfac$V120A)


## other energy source by facility

## No other source
table(htfac$V121A == "No" & htfac$V121B == "No" & htfac$V121C == "No",htfac$V120A, htfac$V007)
## Fuel-operated generator (only) 
table(htfac$V121A == "Yes" & htfac$V121B == "No" & htfac$V121C == "No",htfac$V120A, htfac$V007)
## battery-operated generator (only) 
table(htfac$V121A == "No" & htfac$V121B == "Yes" & htfac$V121C == "No",htfac$V120A, htfac$V007)
## Solar system (only)
table(htfac$V121A == "No" & htfac$V121B == "No" & htfac$V121C == "Yes",htfac$V120A, htfac$V007)
## Fuel and battery
table(htfac$V121A == "Yes" & htfac$V121B == "Yes" & htfac$V121C == "No", htfac$V120A, htfac$V007)
## Fuel and solar
table(htfac$V121A == "Yes" & htfac$V121B == "No" & htfac$V121C == "Yes", htfac$V120A, htfac$V007)
## Battery and solar
table(htfac$V121A == "No" & htfac$V121B == "Yes" & htfac$V121C == "Yes",htfac$V120A, htfac$V007)
## Fuel, battery and solar
table(htfac$V121A == "Yes" & htfac$V121B == "Yes" & htfac$V121C == "Yes", htfac$V120A, htfac$V007)


## Back-up source by functionality

## Fuel-operated generator (only) 
table(htfac$V121A == "Yes" & htfac$V121B == "No" & htfac$V121C == "No",htfac$V120)
## battery-operated generator (only) 
table(htfac$V121A == "No" & htfac$V121B == "Yes" & htfac$V121C == "No",htfac$V120)
## Solar system (only)
table(htfac$V121A == "No" & htfac$V121B == "No" & htfac$V121C == "Yes",htfac$V120)
## Fuel and battery
table(htfac$V121A == "Yes" & htfac$V121B == "Yes" & htfac$V121C == "No", htfac$V120)
## Fuel and solar
table(htfac$V121A == "Yes" & htfac$V121B == "No" & htfac$V121C == "Yes", htfac$V120)
## Battery and solar
table(htfac$V121A == "No" & htfac$V121B == "Yes" & htfac$V121C == "Yes",htfac$V120)
## Fuel, battery and solar
table(htfac$V121A == "Yes" & htfac$V121B == "Yes" & htfac$V121C == "Yes", htfac$V120)








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

## Malawi
mwfac <- read.spss("MWFC6JFLSR.SAV", to.data.frame = T)
dim(mwfac)
# 1060 1627

## Senegal
snfac <- read.spss("SNFC71FLSP.SAV", to.data.frame = T)
dim(snfac)
# 452 1743


egfac <- read.spss("EGFC5IFLSP.SAV", to.data.frame = T)
dim(egfac)
# 659 1197


