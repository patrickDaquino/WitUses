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
## Set the value of attribute
for (  ressTappingMobileValue in c(0.1,0.5,1)) {
for (  ressTappingSettledValue in c(0.1,0.5,1)) {
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
R <- runSimu(20)

### Get results ####
res <- getNumericProbe("satisfiedUsers", "Observer")
resPlan <- rbind(resPlan, c(ressTappingMobileValue, 
                            ressTappingSettledValue,
                            res$satisfiedUsers[21]))
res$ressTappingMobile <- ressTappingMobileValue
res$ressTappingSettled <- ressTappingSettledValue
resPlanBig <- rbind(resPlanBig, res)
}}

