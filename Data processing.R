
# Variables:

# Energy Categories 
      # connected (without don´t know)
        connected <- (mwfac$V120A !=  "Don't know if connected")
        table(connected)

      # solar(only)
        solar <- as.character(mwfac$V121A == "No" & mwfac$V121B == "No" & mwfac$V121C == "Yes" & mwfac$V121D =="No")
        table(solar, mwfac$V120A)

        # 1: Reliable,  back-up
        mwfac$encate3[mwfac$V120A == "Connected, always available" & mwfac$V120 != "No backup generator"] <- "c1" 

        # 2: Sometimes interrupted, back-up
        mwfac$encate3[mwfac$V120A == "Connected, sometimes interrupted" & mwfac$V120 != "No backup generator"] <- "c2"


        # 3: Reliable, no back-up
        mwfac$encate3[mwfac$V120A == "Connected, always available" & mwfac$V120 == "No backup generator"] <- "c3"


        # 4: Sometimes interrupted, no back-up
        mwfac$encate3[mwfac$V120A == "Connected, sometimes interrupted" & mwfac$V120 == "No backup generator"] <- "c4"

        # 5: Not connected, off-grid
        mwfac$encate3[mwfac$V120A =="Not connected" & solar == "TRUE" | mwfac$V120A =="Not connected" & (mwfac$V120 == "Reported functional with fuel" | mwfac$V120 == "Reported functional, no fuel, DK fuel" | mwfac$V120 == "Reported not functional" | mwfac$V120 == "Reported, don't know if functional")] <- "c5"

        # 6: Not connected to electricity  
        mwfac$encate3[mwfac$V120A =="Not connected" & mwfac$V120 == "No backup generator" & solar == "FALSE"] <- "c6"

# Facility levels
        mwfac$facilitycategory[mwfac$V007 == "District Hospital" | mwfac$V007 == "Central Hospital"] <- "Referral"
        # First
        mwfac$facilitycategory[mwfac$V007 == "Rural/Community Hospital" | mwfac$V007 == "Other Hospital" | mwfac$V007 == "Health Centre"] <- "First"
        # Community
        mwfac$facilitycategory[mwfac$V007 == "Maternity" | mwfac$V007 == "Dispensary" | mwfac$V007 == "Clinic" | mwfac$V007 == "Health Post"] <- "Community"


# Refrigeration
      refap <- as.numeric(mwfac$LV222A == "Between +2 and +8 degrees")  
      refinap <- as.numeric(mwfac$LV222A == "Above +8 degrees" | mwfac$LV222A == "Below +2 degrees")
      reftnf <- as.numeric(mwfac$LV222A == "Thermometer not functional")
      vaccns <- as.numeric(mwfac$LV222A == "Vaccine refrigerator not seen")
      
      refrvac1 <- data.frame(refap, refinap, reftnf, vaccns)
      
      # 3 categories
      refrvac1$refr2[refrvac1$refap == 1] <- "appro"
      refrvac1$refr2[refrvac1$refinap == 1] <- "inappr"
      refrvac1$refr2[refrvac1$reftnf == 1 | refrvac1$vaccns == 1] <- "unknna"
      
      refrvac1$INV_ID <- mwfac$INV_ID
      mwfac <- merge(refrvac1, mwfac, by="INV_ID")
# Lighting
      notav <- as.numeric(mwfac$V166I == "Not available" & mwfac$V010B == "Outpatient")
      obsfunc <- as.numeric(mwfac$V166I == "Observed, functioning" & mwfac$V010B == "Outpatient")
      obsnotfunc <- as.numeric(mwfac$V166I == "Observed, not/DK if functioning" & mwfac$V010B == "Outpatient")
      repfunc <- as.numeric(mwfac$V166I == "Reported functioning" & mwfac$V010B == "Outpatient") 
      repnotfunc <- as.numeric(mwfac$V166I == "Reported, not/DK  if functioning" & mwfac$V010B == "Outpatient")
      
      lightcatframe <- data.frame(notav, obsfunc, obsnotfunc, repfunc, repnotfunc)
      
      lightcatframe$licatopdfunc[lightcatframe$obsfunc == 1 | lightcatframe$repfunc == 1 ] <- 1
      lightcatframe$licatopdfunc[lightcatframe$obsnotfunc == 1 | lightcatframe$repnotfunc == 1 | lightcatframe$notav == 1] <- 0
      
      lightcatframe$INV_ID <- mwfac$INV_ID
      mwfac <- merge(lightcatframe, mwfac, by="INV_ID")

# 24h staff
      mwfac$hours24overall[mwfac$V101A != "No 24 hour staff"] <- 1
      mwfac$hours24overall[mwfac$V101A == "No 24 hour staff"] <- 0
################################################################################################################
# A RECODE AS NUMERIC
# Total staff number
mwfac$V102T

# Total outpatient visits in the last full month
mwfac$V134


# B MULTINOMIAL REGRESSION
# Vaccine refrigeration
      table(mwfac$refr2, mwfac$encate3)
      
      regframerefr2 <- data.frame(mwfac$refr2, mwfac$facilitycategory, mwfac$encate3, mwfac$V008, mwfac$V003, mwfac$V001)
      
      # delete missings
      sapply(regframerefr2, function(x) sum(is.na(x))) # n.a. ref=432; encate3=88 ("don´t know if connected")
      sapply(regframerefr2, function(x) length(unique(x)))
      
      regframerefr2 <- na.omit(regframerefr2)
      
      install.packages("VGAM")
      library(VGAM)
      # set reference caegory
      regframerefr2$mwfac.refr2 <- relevel(regframerefr2$mwfac.refr2, "inappr")
      
      # Model
      vglmFitMN <- vglm(mwfac.refr2 ~ mwfac.facilitycategory + mwfac.encate3 + mwfac.V008 + mwfac.V003 + mwfac.V001, family=multinomial, data=regframerefr2)

      
      # Diagnostics
      
      # Independence of Irrelevant Alternatives (IIA) assumption
        # SubModel
      modelrefrvaclogsub <- mlogit(mwfac.refr2 ~ 0 | mwfac.facilitycategory + mwfac.encate3 + mwfac.V008 + mwfac.V003 + mwfac.V001, dfMNL, alt.subset=c("unknna","inappr"))
        # Hausman Test
      hmftest(modelrefrvac2,modelrefrvaclogsub)
      # SIGNIFICANT: IAA rejected [p.6: https://www3.nd.edu/~rwilliam/stats3/Mlogit2.pdf]

      # Log likelihoods for full model and 0-model
      vglm0 <- vglm(mwfac.refr2 ~ 1, family=multinomial(refLevel = 1), data=regframerefr2)
      LLf   <- logLik(modelrefrvac2)
      LL0   <- logLik(vglm0)
      
      # Log likelihood test full model against 0-model
      VGAM::lrtest(vglmFitMN, vglm0)

      # McFadden pseudo R
      as.vector(1 - (LLf / LL0))
      # 0.051
      
      # Cox&Snell
      as.vector(1 - exp((2/625) * (LL0 - LLf)))
      # 0.072
      
      # Nagelkerke
      as.vector((1 - exp((2/625) * (LL0 - LLf))) / (1 - exp(LL0)^(2/625)))
      # 0.093
             
#### Inpatient visits/overnight observation
     # ALL FACILITIES        
                       regframeoutpovin <- data.frame(mwfac$V142,
                                          mwfac$encate3, 
                                          mwfac$licatopdfunc, 
                                          mwfac$V008, mwfac$V001, mwfac$V003, 
                                          mwfac$hours24overall, 
                                          mwfac$facilitycategory)[mwfac$V010B == "Outpatient" 
                                                                  & mwfac$V015A == "Yes, completed",] 
                                                                  
          # Missing values
          sapply(regframeoutpovin, function(x) sum(is.na(x))) # n.a. inpa = 82
          sapply(regframeoutpovin, function(x) length(unique(x)))
          
          regframeoutpovin <- na.omit(regframeoutpovin)
          
          #Long format of data
          modelframoutpovin <- mlogit.data(regframeoutpovin, choice="mwfac.V142", shape="wide", varying=NULL)
          
          # model a
          library("mlogit")
          modeloutpovina <- mlogit(mwfac.V142 ~ 0 | mwfac.encate3 + mwfac.licatopdfunc + mwfac.V008 + mwfac.V001 + mwfac.V003 + mwfac.hours24overall + mwfac.facilitycategory, modelframoutpovin1)
          summary(modeloutpovina)
          
                 # Error: Error in solve.default(H, g[!fixed]) : system is computationally singular: reciprocal condition number = 2.6772e-17
                 
          # model b
          library("nnet") 
          modeloutpovinb <- multinom(mwfac.V142 ~ ., data = regframeoutpovin)
          summary(modeloutpovinb)
          
          # model c
          library("VGAM")
          modeloutpovinc <- vglm(mwfac.V142 ~ mwfac.encate3 + mwfac.licatopdfunc + mwfac.V008 + mwfac.V001 + mwfac.V003 + mwfac.hours24overall + mwfac.facilitycategory, family=multinomial, data=regframeoutpovin)
          summary(modeloutpovinc)
          
          # Different results??
          
          # Diagnostics
          # Independence of Irrelevant Alternatives (IIA) assumption
          # SubModel
          modeloutpovinasub <- mlogit(mwfac.V142 ~ 0 | mwfac.encate3 + mwfac.licatopdfunc + mwfac.V008 + mwfac.V001 + mwfac.V003 + mwfac.hours24overall + mwfac.facilitycategory, modelframoutpovin, alt.subset=c("No","Yes"))
          # Hausman Test
          hmftest(modeloutpovina,modeloutpovinasub)
          # SIGNIFICANT: IIA rejected
          
          # Log likelihoods for full model and 0-model
          install.packages("VGAM")
          library(VGAM)
          vglm0 <- vglm(mwfac.V142 ~ 1, family=multinomial(refLevel = 1), data=regframeoutpovin)
          LLf   <- logLik(modeloutpovinc)
          LL0   <- logLik(vglm0)
          
          # Log likelihood test full model against 0-model
          # Full model with VGAM package
          vglmFitMN <- vglm(mwfac.V142 ~., family=multinomial, data=regframeoutpovin)
          VGAM::lrtest(vglmFitMN, vglm0)
          # p=0 (same as in modela)
          
          # McFadden pseudo R
          as.vector(1 - (LLf / LL0))
          # 0.2
          
          # Cox&Snell
          as.vector(1 - exp((2/625) * (LL0 - LLf)))
          # 0.27
          
          # Nagelkerke
          as.vector((1 - exp((2/625) * (LL0 - LLf))) / (1 - exp(LL0)^(2/625)))
          # 0.34

          library(stargazer) 
          stargazer(modeloutpovina, type = "html",single.row = TRUE, out="outputoutp2.htm")
          
          library(stargazer) 
          stargazer(modeloutpovinb, type = "html", single.row = TRUE, out="outputoutp3.htm")
          
          library(stargazer) 
          stargazer(modeloutpovinc, type = "html", single.row = TRUE, out="outputoutp4.htm")
          
#### FIRST AND REFERRAL LEVEL
          regframeoutpovin1 <- data.frame(mwfac$V142,
                                          mwfac$encate3, 
                                          mwfac$licatopdfunc, 
                                          mwfac$V008, mwfac$V001, mwfac$V003, 
                                          mwfac$hours24overall, mwfac$facilitycategory)[mwfac$V010B == "Outpatient" 
                                                                                        & mwfac$V015A == "Yes, completed"
                                                                                        & mwfac$facilitycategory != "Community",] 
                                                                                                       
          
          # Missing values
          sapply(regframeoutpovin1, function(x) sum(is.na(x))) # n.a. inpa = 14
          sapply(regframeoutpovin1, function(x) length(unique(x)))
          
          regframeoutpovin1 <- na.omit(regframeoutpovin1)
          
          # set first level as reference category
          # regframeoutpovin1$mwfac.facilitycategory <- relevel(regframeoutpovin1$mwfac.facilitycategory, "First")
          
          
          #Long format of data
          modelframoutpovin1 <- mlogit.data(regframeoutpovin1, choice="mwfac.V142", shape="wide", varying=NULL)
          
          # model a
          library("nnet") 
          modeloutpovin1a <- mlogit(mwfac.V142 ~ 0 | mwfac.encate3 + mwfac.licatopdfunc + mwfac.V008 + mwfac.V001 + mwfac.V003 + mwfac.hours24overall + mwfac.facilitycategory, modelframoutpovin1)
          summary(modeloutpovin1a)
          
          
          # model b
          library("nnet") 
          modeloutpovin1b <- multinom(mwfac.V142 ~ ., data = regframeoutpovin1)
          summary(modeloutpovin1b)
          
          # model c
          library("VGAM")
          modeloutpovin1c <- vglm(mwfac.V142 ~ mwfac.encate3 + mwfac.licatopdfunc + mwfac.V008 + mwfac.V001 + mwfac.V003 + mwfac.hours24overall + mwfac.facilitycategory, family=multinomial, data=regframeoutpovin1)
          summary(modeloutpovin1c)
          
          # Completely different outputs
                 
          # Diagnostics
          # Independence of Irrelevant Alternatives (IIA) assumption
          # SubModel
          modeloutpovin1sub <- mlogit(mwfac.V142 ~ 0 | mwfac.encate3 + mwfac.licatopdfunc + mwfac.V008 + mwfac.V001 + mwfac.V003 + mwfac.hours24overall + mwfac.facilitycategory, modelframoutpovin1, alt.subset=c("No","Yes"))
          # Hausman Test
          hmftest(modeloutpovin1a,modeloutpovin1sub)
          # SIGNIFICANT: IIA rejected
          
          # Log likelihoods for full model and 0-model
          install.packages("VGAM")
          library(VGAM)
          vglm0 <- vglm(mwfac.V142 ~ 1, family=multinomial(refLevel = 1), data=regframeoutpovin1)
          LLf   <- logLik(modeloutpovin1c)
          LL0   <- logLik(vglm0)
          
          # Log likelihood test full model against 0-model
          # Full model with VGAM package
          vglmFitMN <- vglm(mwfac.V142 ~., family=multinomial, data=regframeoutpovin1)
          VGAM::lrtest(vglmFitMN, vglm0)
          # p=0 (different to model1a)
          
          # McFadden pseudo R
          as.vector(1 - (LLf / LL0))
          # 0.17
          
          # Cox&Snell
          as.vector(1 - exp((2/625) * (LL0 - LLf)))
          # 0.22
          
          # Nagelkerke
          as.vector((1 - exp((2/625) * (LL0 - LLf))) / (1 - exp(LL0)^(2/625)))
          # 0.28

          library(stargazer) 
          stargazer(modeloutpovin1a, type = "html",single.row = TRUE, out="outputoutp2.htm")
          
          library(stargazer) 
          stargazer(modeloutpovin1b, type = "html", single.row = TRUE, out="outputoutp3.htm")
          
          library(stargazer) 
          stargazer(modeloutpovin1c, type = "html", single.row = TRUE, out="outputoutp4.htm")  
  
  #### COMMUNITY & FIRST
                 
                          
          regframeoutpovin2 <- data.frame(mwfac$V142,
                                          mwfac$encate3, 
                                          mwfac$licatopdfunc, 
                                          mwfac$V008, mwfac$V001, mwfac$V003, 
                                          mwfac$hours24overall, mwfac$facilitycategory)[mwfac$V010B == "Outpatient" 
                                                                                        & mwfac$V015A == "Yes, completed" 
                                                                                        & mwfac$facilitycategory != "Referral",]
          
          # Missing values
          sapply(regframeoutpovin2, function(x) sum(is.na(x))) # n.a. inpa = 81
          sapply(regframeoutpovin2, function(x) length(unique(x)))
          
          regframeoutpovin2 <- na.omit(regframeoutpovin2)
          
          #Long format of data
          modelframeoutpovin2 <- mlogit.data(regframeoutpovin2, choice="mwfac.V142", shape="wide", varying=NULL)
          
          # model a
          modeloutpovin2a <- mlogit(mwfac.V142 ~ 0 | mwfac.encate3 + mwfac.licatopdfunc + mwfac.V008 + mwfac.V001 + mwfac.V003 + mwfac.hours24overall + mwfac.facilitycategory, modelframeoutpovin2)         
          # Doesn´t work ?
                 # Error in solve.default(H, g[!fixed]) : Lapack routine dgesv: system is exactly singular: U[35,35] = 0
          
          # model b
          library("nnet") 
          modeloutpovin2b <- multinom(mwfac.V142 ~ ., data = regframeoutpovin2)
          summary(modeloutpovin2b)
          
          # model c
          library("VGAM")
          modeloutpovin2c <- vglm(mwfac.V142 ~ mwfac.encate3 + mwfac.licatopdfunc + mwfac.V008 + mwfac.V001 + mwfac.V003 + mwfac.hours24overall + mwfac.facilitycategory, family=multinomial, data=regframeoutpovin2)
          summary(modeloutpovin2c)
          
          # Completely different coefficients
          
          # Diagnostics
          # Independence of Irrelevant Alternatives (IIA) assumption
          # SubModel
          modeloutpovin2sub <- mlogit(mwfac.V142 ~ 0 | mwfac.encate3 + mwfac.licatopdfunc + mwfac.V008 + mwfac.V001 + mwfac.V003 + mwfac.hours24overall + mwfac.facilitycategory, modelframeoutpovin2, alt.subset=c("No","Yes"))
          # Hausman Test
          hmftest(modeloutpovin2a,modeloutpovin2sub)
          # Doesn´t work?
          
          # Log likelihoods for full model and 0-model
          install.packages("VGAM")
          library(VGAM)
          vglm0 <- vglm(mwfac.V142 ~ 1, family=multinomial(refLevel = 1), data=regframeoutpovin2)
          LLf   <- logLik(modeloutpovin2c)
          LL0   <- logLik(vglm0)
          
          # Log likelihood test full model against 0-model
          # Full model with VGAM package
          vglmFitMN <- vglm(mwfac.V142 ~., family=multinomial, data=regframeoutpovin2)
          VGAM::lrtest(vglmFitMN, vglm0)
          # p=0 
          
          # McFadden pseudo R
          as.vector(1 - (LLf / LL0))
          # 0.16
          
          # Cox&Snell
          as.vector(1 - exp((2/625) * (LL0 - LLf)))
          # 0.21
          
          # Nagelkerke
          as.vector((1 - exp((2/625) * (LL0 - LLf))) / (1 - exp(LL0)^(2/625)))
          # 0.27
          
          library(stargazer) 
          stargazer(modeloutpovin2a, type = "html",single.row = TRUE, out="outputoutp2.htm")
          
          library(stargazer) 
          stargazer(modeloutpovin2b, type = "html", single.row = TRUE, out="outputoutp3.htm")
          
          library(stargazer) 
          stargazer(modeloutpovin2c, type = "html", single.row = TRUE, out="outputoutp4.htm")      

                 
 
                 
                 
                 
                 
