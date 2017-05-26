
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
mwfac1 <- read.spss("MWFC6KFLSP.SAV", to.data.frame = T) #JAKE?


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
energycatframe <- data.frame(reliable, interfunctback, internofunctback, notconnectedfuncback, notconnectednofuncback, onlysolar)
table(energycatframe)

energycat$category[energycat$reliable == 1] <- "c1"
energycat$category[energycat$interfunctback == 1] <- "c2"
energycat$category[energycat$internofunctback == 1] <- "c3"
energycat$category[energycat$notconnectedfuncback == 1] <- "c4"
energycat$category[energycat$notconnectednofuncback == 1] <- "c5"
energycat$category[energycat$onlysolar == 1] <- "c6"

energycat$category <- factor(energycat$category)

table(energycat)

# Categories Fac. types: WHO

    CentralHospital <- as.numeric(mwfac$V007 == "Central Hospital")
    Referrallevel <- as.numeric(mwfac$V007 == "District Hospital")
    Firstlevel <- as.numeric(mwfac$V007 == "Rural/Community Hospital")
    Communitylevel <- as.numeric(mwfac$V007 == "Other Hospital" | mwfac$V007 == "Health Centre" | mwfac$V007 == "Maternity" | mwfac$V007 == "Dispensary" | mwfac$V007 == "Clinic" | mwfac$V007 == "Health Post")
    
    facilitycatwho <- data.frame(CentralHospital, Referrallevel, Firstlevel, Communitylevel)
    
    facilitycatwho$category[facilitycatwho$CentralHospital == 1] <- "Central"
    facilitycatwho$category[facilitycatwho$Referrallevel == 1] <- "Referral"
    facilitycatwho$category[facilitycatwho$Firstlevel == 1] <- "First"
    facilitycatwho$category[facilitycatwho$Communitylevel == 1] <- "Community"
    
    facilitycatwho$category <- factor(facilitycatwho$category)
    table(facilitycatwho$category)



# Categories Fac. types: Original

    CentralHospOr <- as.numeric(mwfac$V007 == "Central Hospital")
    DistrictHospOr <- as.numeric(mwfac$V007 == "District Hospital")
    RuralCommunityHospOr <- as.numeric(mwfac$V007 == "Rural/Community Hospital")
    OtherHospOr <- as.numeric(mwfac$V007 == "Other Hospital")
    HealthCentreOr <- as.numeric(mwfac$V007 == "Health Centre")
    MaternityOr <- as.numeric(mwfac$V007 == "Maternity")
    DispensaryOr <- as.numeric(mwfac$V007 == "Dispensary")
    ClinicOr <- as.numeric(mwfac$V007 == "Clinic")
    HealthPostOr <- as.numeric(mwfac$V007 == "Health Post")

    facilitycatOr <- data.frame(CentralHospOr, DistrictHospOr, RuralCommunityHospOr, OtherHospOr,  HealthCentreOr, MaternityOr, DispensaryOr, HealthPostOr)

# ENERGY USES INTERAGENCY LIST

# Blood typing
table(mwfac$V034A)
# Anti-A
table(mwfac$V860A)
# Anti-B
table(mwfac$V860B)
# Anti-D
table(mwfac$V860D)

# Blood transfusion 
bloodtransf <- as.numeric(mwfac$V034B=="Yes, completed" | mwfac$V034B=="Not returned from field")
table(bloodtransf)
 
table(bloodtransf, energycat$category, facilitycatwho$category)
table(bloodtransf, facilitycatwho$category)

# Glucometer (Batteries)
table(mwfac$V850B)
# Unexpired
table(mwfac$V850B1)

# HIV testing and counseling
table(mwfac$V042)

# HIV care and support services 
table(mwfac$V046)
table(mwfac$V1515)

# HIV RDT
table(mwfac$VT807)

# Lab test
table(mwfac$VT808)

# Syphillis RDT
table(mwfac$V840E1)
    # Any other test (?) 
    table(mwfac$V840E2)
    # Serology
    table(mwfac$VT829)  
    # TPHA
    table(mwfac$V847E)
    
# Scale (unclear if electronic) - Basic equipment!
    #Adult
    table(mwfac$V166A)
    # Child
    table(mwfac$V166B)
    # Infant
    table(mwfac$V166C)
 
# Light
    # General OPD area
    table(mwfac$V166I)
    # Other equipm: Examination light
    table(mwfac$V333E)
    table(mwfac$V533B) # Difference?
    # ANC
    table(mwfac$V433I)
    # NC diseases
    table(mwfac$V1608I) 
    
    # Light categories: observed functioning vs. observed not functioning/dk if functioning
    # General OPD area
    
    
    # Light categories: functioning vs. not functioning/dk if functioning (incl. observed and reported)
    # General OPD area
    
    notav <- as.numeric(mwfac$V166I == "Not available")
    obsfunc <- as.numeric(mwfac$V166I == "Observed, functioning")
    obsnotfunc <- as.numeric(mwfac$V166I == "Observed, not/DK if functioning")
    repfunc <- as.numeric(mwfac$V166I == "Reported functioning") 
    repnotfunc <- as.numeric(mwfac$V166I == "Reported, not/DK  if functioning")
    
    lightcatframe <- data.frame(notav, obsfunc, obsnotfunc, repfunc, repnotfunc)
    
    lightcat$category[lightcat$obsfunc == 1 | lightcat$repfunc == 1 ] <- "func"
    lightcat$category[lightcat$obsnotfunc == 1 | lightcat$repnotfunc == 1 | lightcat$notav == 1] <- "notfuncorav"
    
    lightcat$category <- factor(lightcat$category)
    table(lightcat$category)
        
    enlight <- data.frame(lightcat$category, energycat$category)
    ctablelight <- table(enlight)
    ctablelight
    
## ????
    df <- data.frame(energycatframe, facilitycatOr)         
    ma <- data.matrix(df, rownames.force = NA)

    
    mytable=xtabs(~class + ma + lightcatframe, data=mwfac)
                    energycatframe + facilitycatOr + lightcatframe, data=mwfac)
    ftable(mytable)
    freqdata=data.frame(mytable)
    
    # fullmodel=glm(Freq~SITE*SEX*MORTALITY,family=poisson,data=freqdata)
    
    
    
    
  
# Microscope
    # Used, observed
    table(mwfac$V841A1)
    # Working order
    table(mwfac$V842A1)
    
# Ultrasound
    # Performs diagnostic X-rays, ultrasound or computerized tomography  
    table(mwfac$V863)
    # Ultrasound available
    table(mwfac$V864C)
    # Working order ultrasound
    table(mwfac$V865C)
    
# Thermometer (battery)
    # General OPD
    table(mwfac$V166E) 
    # Sick child exam
    table(mwfac$V265D)
    # Other equipment
    table(mwfac$V533I)

# Automatic Timer
    # Child sick
    table(mwfac$V265E)
    
# X-Ray
    # Performs diagnostic X-rays, ultrasound or computerized tomography  
    table(mwfac$V863)
    # Digital, not requiring film
    table(mwfac$V864F)
    # Available
    table(mwfac$V864A)
    # Unexpired film
    table(mwfac$V864B)
    # Working order: not requiring film
    table(mwfac$V865F)
    # Working order (requiring film)
    table(mwfac$V865A)
    
# Pulse oxymeter
    # General OPD, available and functioning 
    table(mwfac$V166N)

# Suction (ENERGY?)
    # Newborn with catheter
        # available
        table(mwfac$V536F)
        # routinely used
        table(mwfac$V507A)

        
# Newborn Incubator
        table(mwfac$V536B)
        
# Oxygen concentrator: general OPD
        table(mwfac$V166O)
        # Country specific
        table(mwfac$V533K)
      
          table(mwfac$V536C)
     
# Electric autoclave
        # Available, functioning
        table(mwfac$V177B, mwfac$V007)
        
# Non-electric autoclave
        # Available, functioning
        table(mwfac$V177C)

# Other processing technologies: 177A, 176A... !!!
        table(mwfac$V173)
        
        
# Autoclave for FP (family planning)
        table(mwfac$V343A)  
        table(mwfac$V193WA)
        
# Method of processing for reuse ???
        table(mwfac$V175A.1)
        table(mwfac$V175A.2)
        table(mwfac$V175A.3)
        table(mwfac$V175A.4)
        table(mwfac$V175A.5)
        table(mwfac$V175A.6)
        table(mwfac$V175A.7)
        table(mwfac$V175A.8)
          # Duration of correct usage could be included
        
# Microscope
        # Functional (yes, no)
        table(mwfac$VT801)
        # Light-Microscope av. and used
        table(mwfac$V841A1)
          # Working order
          table(mwfac$
        # Electron Microscope av. and used
        table(mwfac$V841A2)
          # Working order
          table(mwfac$V842A2)   
        # Malaria Acridine Orange (AO) Microscope, av. and used 
          table(mwfac$V852D)
      
# Centrifuge
        # Av., used 
          table(mwfac$V841G) 
        # Working order
          table(mwfac$V842G)
      
# Colorimeter
          # av., used
          table(mwfac$V845C)
          # Working order
          table(mwfac$V846C)
          
# ELISA/EIA Test items av.
          # Scanner, av. used
          table(mwfac$V843A)
            # Working order scanner/reader
            table(mwfac$V844A)
          # Washer for ELISA  
          table(mwfac$V843H)
            # Working order
            table(mwfac$V844H)
           
# Hematology analyzer
            # av, used
            table(mwfac$V845A)
              # working order
            table(mwfac$V846A)
          
# Hemocue
           # av., used
            table(mwfac$V845B)
           # working order
            table(mwfac$V846B)
            
# Incubator for HIV testing ?? = EIA/ELISA?
            table(mwfac$VT805)
            
# Laminar Airflow cabinet (biosafety hood)
          # Av., used
          table(mwfac$V859H) 
          
# (Micro)nebulizor (general OPD)
          table(mwfac$V166L) 
          
          
# Rotator, Shaker
          table(mwfac$VT806)

# Syphillis rotator/shaker
          # av., used
          table(mwfac$V847B)
          # working order
          table(mwfac$V848B)
        
# Refrigerator 
          # for storing blood
          table(mwfac$V876A)
            # Temperature
            table(mwfac$V876B)
          # lab area: refrigerator observed: used, observed
            table(mwfac$V841B) 
            # Working order
            table(mwfac$V842B)
          # Vaccine storage area
              # av, used
             table(mwfac$LV221)
             # Temperature
             table(mwfac$LV222A)
             
# Computer
        table(mwfac$V128)
          # Referral system on Computer or paper     
          table(mwfac$V1107C)
          
# Automatic timer
          table(mwfac$V177F)
          
# rtn newborn bath within hours/min
          table(mwfac$V507C)
          
# Cytometer
        # av., used
        table(mwfac$V843B)
        # working order
        table(mwfac$V844B)
     
# schedule on 24 hours           
  table(mwfac$V503, mwfac$V007)          

# ? CATEGORY: Heat source for non-electric equipment: Stove or cooker 
    table(mwfac$V177E)
    
# ANC care: yes, no
    table(mwfac$V014A, energycat$category, mwfac$V007)
    table(mwfac$V014A, facilitycatwho$category)
    table(mwfac$V014A, mwfac$V007)

# Any FP: yes, no        
    table(mwfac$VT301, mwfac$V007)  

# Fac. offers minor surgeries
    table(mwfac$V049, mwfac$V007)
       
# Normal delivery
    table(mwfac$V015A, mwfac$V007)

# Caesarian delivery    
    table(mwfac$V015C, mwfac$V007)
    
# Routine inpatient care or overnight
    table(mwfac$V142, mwfac$V007)

# Landline telephone    
    table(mwfac$V127A)
    
# Cellphone
    table(mwfac$V127B)

# Shortwave radio
  table(mwfac$V127C)

# Communiation equipment
  table(mwfac$V127)

# Access to email, internet at least 2h/day  
  table(mwfac$V129)          

# Most commonly used source of water
  table(mwfac$V123)

  # Running water OPD  
  table(mwfac$V168A1)
  
  # Running water Lab
  table(mwfac$V866A1)

# System to compile health services data in place (not specified if digital)    
  table(mwfac$V033)
        
# Zone
table(mwfac$SFZONE)

# Services PMTCT
table(mwfac$V1260)

# Inpatient by factype
table(mwfac$V007,mwfac$V010B)

# 24 hours by factype
table(mwfac$V101A, mwfac$V007)
table(mwfac$V502, mwfac$V007)  #yes, no

# No of client outpatient visits
table(mwfac$V134, mwfac$V007)

# No of live discharges
table(mwfac$V135, mwfac$V007)

# Medical Waste disposal technology
  table(mwfac$V186A)
  
# Sharps disposal technology
  table(mwfac$V184A)

  # Medical waste: incinerator functional
  table(mwfac$V189)  
  
# Sharps Waste site    
  table(mwfac$V185)

# Medical waste site
  table(mwfac$V187)

# Sanitation technology    
  table(mwfac$V153A)

# Medication room well ventilated
  table(mwfac$V928D)

# Place where medical equipment is processed for re-use (FP)    
  table(mwfac$V342)
  # Autoclave used (FP) [others also av.]
  table(mwfac$V343A)
 
## STAFF   
mwprov <- read.spss("MWPV6IFLSR.SAV", to.data.frame = T) #JAKE?
dim(mwprov)  
  
  # Year started working in fac.
  table(mwprov$W106)
  
  # Received vaccination as part of service
  table(mwprov$W170)

  # Provides delivery
  table(mwprov$W121F)
  
  # No of deliveries last 6 months
  table(mwprov$W135)
  
  # average working Hours per week 
  table(mwprov$W108)
  
  # Non-monetary incentives (any)
  table(mwprov$W117) 
  
  # What provider likes to see improved
  # 1st
  table(mwprov$W172A)
  # 2nd
  table(mwprov$W172B)
  # 3rd
  table(mwprov$W172C)
  
# SICK CHILD OBSERVATION  
  
# Child brought to fac. before because of same sickness  
  table(mwprov$C257) 
  
# Correlations
cor(onlysolar[mwfac$V007], mwfac$V134[mwfac$V007], use = "complete.obs")
cor(onlysolar, mwfac$V134, use = "complete.obs")

cor(reliable[mwfac$V007], mwfac$V135[mwfac$V007], use = "complete.obs")


table(mwfac$LV224)

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
table(snfac$Q342 == "Yes", snfac$Q343,  snfac$Q345 == "Don´t know")


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





