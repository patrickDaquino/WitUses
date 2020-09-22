# load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# set working directory
#setwd("~/vw7.6ncnovo/cormas2020/Models/WitUses/analyses")
setwd("C:/vw7.6nc/cormas2020/Models/WitUses/analyses")

#load functions to interact with cormas
source("cormas-func.R")

#Open model
r <- openModel("WitUses", parcelFile="WitUses.pcl")

# Set the basic init and step method
r <- setInit("init") # Initialization choice
r <- setStep("step:") # Scenario choice
#choose the probe to activate during the simulation
r<- activateProbe("sumCapitalOfUsers", "WitUses")

resPlan <- c()
## Set the value of attribute
for (ressCoefValue in (1:2)/10) {
r <- setNumericAttributeValue("ressCoef",
                              "Calibration class",
                              ressCoefValue)
####### Initialize the Cormas model #######
r <- initSimu()

####### Run Simulation #######
runSimu(20)

### Get results ####
res <- getNumericProbe("sumCapitalOfUsers", "WitUses")
resPlan <- c(resPlan, res[20])
}
