# load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# set working directory
setwd("~/vw7.6ncnovo/cormas2020/Models/WitUses/analyses")
#setwd("C:/vw7.6nc/cormas2020/Models/WitUses/analyses")

#load functions to interact with cormas
source("cormas-func.R")

#Open model
r <- openModel("WitUses", parcelFile="WitUses.pcl")

# Set the basic init and step method
r <- setInit("init") # Initialization choice
r <- setStep("step:") # Scenario choice

resPlan <- NULL
resPlanBig <- NULL

rTMValues <- c(0.1,0.5,1)
rTSValues <- c(0.1,0.5,1)

simuDuration <- 20

expPlanProgress <- txtProgressBar(min = 1,
                               max = length(rTMValues) * length(rTMValues),
                               style = 3)

simuNb <- 0
## Set the value of attribute
for (ressTappingMobileValue in rTMValues) {
for (ressTappingSettledValue in rTSValues) {
  simuNb <- simuNb + 1
  setTxtProgressBar(expPlanProgress, simuNb)
  #choose the probe to activate during the simulation
  r<- activateProbe("satisfiedUsers", "Observer")

  r <- setNumericAttributeValue("ressTappingFromR",
                                "MobileUse",
                                ressTappingMobileValue)
  r <- setNumericAttributeValue("ressTappingFromR",
                              "SettledUse",
                              ressTappingSettledValue)
####### Initialize the Cormas model #######
r <- initSimu()

####### Run Simulation #######
R <- runSimu(simuDuration)

### Get results ####
res <- getNumericProbe("satisfiedUsers", "Observer")
resPlan <- rbind(resPlan, c(ressTappingMobileValue, 
                            ressTappingSettledValue,
                            res$satisfiedUsers[21]))
res$ressTappingMobile <- ressTappingMobileValue
res$ressTappingSettled <- ressTappingSettledValue
resPlanBig <- rbind(resPlanBig, res)
}}

resPlanBig %>% 
  tbl_df()

resPlanBig %>% 
  ggplot() +
  geom_line(aes(y=satisfiedUsers, x=t)) +
  facet_grid(ressTappingMobile ~ ressTappingSettled)

