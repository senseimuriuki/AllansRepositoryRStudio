library(dplyr)
library(fixest)
library(jtools)
library(huxtable)
library(car)
library(stargazer)
#reverse 2003
#time variable: 
help("seq_along")
View(rawdata_allan)
### Data Cleanup for Regression Estimation
rawdata_allan <- as_tibble(rawdata_allan)

rawdata_allan <- rawdata_allan %>% mutate(beforetreat = year < 2003,
                                          treat = areaid == "ontario",
                                          nontreat = areaid != "ontario",
                                          afterycja = year > 2009,
                                          ycja = afterycja*treat,
                                          pcratio1 = lag(pcratio),
                                          trend1 = seq_along(crimesevind),
                                          trend2 = seq_along(assault2),
                                          trend3 = seq_along(assault1),
                                          trend4 = seq_along(utterthreat),
                                          trend5 = seq_along(shoplifting),
                                          trend6 = seq_along(adminjust),
                                          Treatedslope2 = beforetreat*treat*trend2,
                                          Treatedint = beforetreat*treat,
                                          Treatedslope1 = beforetreat*treat*trend1,
                                          Treatedslope3 = beforetreat*treat*trend3,
                                          Treatedslope4 = beforetreat*treat*trend4,
                                          Treatedslope5 = beforetreat*treat*trend5,
                                          Treatedslope6 = beforetreat*treat*trend6
                                          )

rawdata_allan2 <- rawdata_allan[!(rawdata_allan$areaid == "canada"),]
View(rawdata_allan2)
### this removes the canada measure from rawdata_allan ###
### Test treated slope and treated int separately. Slope says everyone has a trend, int asks if there was a different trend 
### verify trend 
### if not using trend, put beforetreat inside 
summary(rawdata_allan)
df <- as.data.frame(rawdata_allan)
stargazer(df, type = "text")
type()
## Treated 2 # 

### Crime where the trends were relatively similar across time period ####

#CrimeSevInd
DiD2.2 <-  feols(crimesevind~  trend1*treat + trend1 + Treatedslope1 + beforetreat +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan2)
export_summs(DiD2.2, stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))

DiD2.3 <-  feols(crimesevind~ trend1*treat + trend1 + Treatedint +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan2)

export_summs(DiD2.3, stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))

#Assault1
DiD2.4 <-  feols(assault1~ trend3*treat + trend3 + Treatedslope3 + beforetreat +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan2)

export_summs(DiD2.4, stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))

DiD2.5 <-  feols(assault1~ trend3*treat + trend3 + Treatedint +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan2)

export_summs(DiD2.5, stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))

#Utterthreat
DiD2.6 <-  feols(utterthreat~ trend4*treat + trend4 + Treatedslope4 + beforetreat +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan2)

export_summs(DiD2.6, stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
summar
DiD2.7 <-  feols(utterthreat~ trend4*treat + trend4 + Treatedint +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan2)

export_summs(DiD2.7, stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))


### Crime where the trends were  different  across time period ####


#Assault2

DiD2.8 <-  feols(assault2~ trend1*treat + trend1 + Treatedslope1 + beforetreat +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan2)

export_summs(DiD2.8, stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))

DiD2.9 <-  feols(assault2~ trend1*treat + trend1 + Treatedint +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan2)
##trend1*treat is significant, fails parallel trends
export_summs(DiD2.9, stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))


# Shoplifting 

DiD2.10 <-  feols(shoplifting~ trend5*treat + trend5 + Treatedslope5 + beforetreat +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan2)

export_summs(DiD2.10, stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
###the effect is now negative but statistically insignificant. This may be due to sharp decline in crime in post 2018. Look into why this was 

DiD2.11 <-  feols(shoplifting~ trend5*treat + trend5 + Treatedint +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan2)

export_summs(DiD2.11, stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))

rawdata_allan3 <- rawdata_allan2[(rawdata_allan2$year < 2017),]

DiD2.14 <-  feols(shoplifting~ trend5 + Treatedslope5 + beforetreat +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan3)

export_summs(DiD2.14, stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))

DiD2.15 <-  feols(shoplifting~ trend5 + Treatedint +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan3)

export_summs(DiD2.15, stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))


# AdminJustice

DiD2.12 <-  feols(adminjust~ trend6*treat + trend6 + Treatedslope6 + beforetreat +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan2)

export_summs(DiD2.12, stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))

DiD2.13 <-  feols(adminjust~ trend6*treat + trend6 + Treatedint +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan2)

export_summs(DiD2.13, stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))


###Results Output Code###
install.packages("kableExtra")
library(modelsummary)
library(kableExtra)
library(gt)

url <- 'https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Guerry.csv'
dat <- read.csv(url)


#CrimeSevInd
models <- list(
  "Crime Severity Index - Kinked Slope" = feols(crimesevind~ trend1 + Treatedslope1 + beforetreat +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan2),
  "Crime Severity Index - Leveled Slpoe" = feols(crimesevind~ trend1 + Treatedint +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan2)
)
modelsummary(models, stars = TRUE)


#Assault1
models2 <- list(
  "Assault 1 - Kinked Slope" = feols(assault1~ trend3 + Treatedslope3 + beforetreat +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan2),
  "Assault 1 - Leveled Slpoe" = feols(assault1~ trend3 + Treatedint +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan2)
)
modelsummary(models2, stars = TRUE)


#Utterthreat
models3 <- list(
  "Utter Threat - Kinked Slope" =  feols(utterthreat~ trend4*treat + trend4 + Treatedslope4 + beforetreat +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan2),
  "Utter Threat - Leveled Slpoe" = feols(utterthreat~ trend4*treat + trend4 + Treatedint +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan2)
)
modelsummary(models3, stars = TRUE)

#Adminjust
models4 <- list(
  "Adminjust - Kinked Slope" =  feols(adminjust~ trend6*treat + trend6 + Treatedslope6 + beforetreat +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan2),
  "Adminjust - Leveled Slpoe" = feols(adminjust~ trend6*treat + trend6 + Treatedint +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan2)
)
modelsummary(models4, stars = TRUE)








### DND Check ###


#CrimeSevInd
check1 <- list(
  "Crime Severity Index - Kinked Slope" = feols(crimesevind~ trend1*treat + Treatedslope1 + beforetreat +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan2)
  
)
modelsummary(check1, stars = TRUE)

#Assault1
check2 <- list (
  "Assault1" = feols(assault1~ trend3*treat + trend3 + Treatedslope3 + beforetreat +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan2)
)

modelsummary(check2, stars = TRUE)


#Assault2
check3 <- list (
  "Assault2" = feols(assault2~ trend1*treat + trend1 + Treatedint +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan2)
)

modelsummary(check3, stars = TRUE)


#utterthreat
check4 <- list (
  "Utter Threats" = feols(utterthreat~ trend4*treat + trend4 + Treatedslope4 + beforetreat +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan2),
  
)

modelsummary(check4, stars = TRUE)


#shoplifting 
check5 <- list (
  "Shoplifting" = feols(shoplifting~ trend5*treat + trend5 + Treatedslope5 + beforetreat +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan2)
)

modelsummary(check5, stars = TRUE)

#adminjust
check6 <- list (
  "Shoplifting" = feols(adminjust~ trend6*treat + trend6 + Treatedslope6 + beforetreat +  treat + uerate + pcratio1 + lowincome + afterycja + educhs, data = rawdata_allan2)
)

modelsummary(check6, stars = TRUE)




             