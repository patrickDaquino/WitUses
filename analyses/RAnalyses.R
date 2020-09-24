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

resPlan <- NULL
resPlanBig <- NULL

rTMValues <- c(0.1, 1)
rTSValues <- c(0.1 ,1)

simuDuration <- 20
nbReplication <- 5

expPlanProgress <- txtProgressBar(min = 1,
                               max = length(rTMValues) * length(rTMValues) * nbReplication,
                               style = 3)

simuNb <- 0

## Set the value of attribute
for (ressTappingMobileValue in rTMValues) {
for (ressTappingSettledValue in rTSValues) {
  for (replicate in 1:nbReplication) {
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
    runSimu(2)
  ### Get results ####
  res <- getNumericProbe("satisfiedUsers", "Observer")
  lastNbSatisfiedUsers <- getAttributesOfEntities("satisfiedUsers",
                                                  "Observer") %>%
    pull("satisfiedUsers")
  
  getAttributesOfEntities("capital", "User")
  
  resPlan <- rbind(resPlan, c(ressTappingMobileValue, 
                            ressTappingSettledValue,
                            simuNb,
                            lastNbSatisfiedUsers))
  
  res$ressTappingMobile <- ressTappingMobileValue
  res$ressTappingSettled <- ressTappingSettledValue
  res$simuNb <- simuNb
  resPlanBig <- rbind(resPlanBig, res)
  }
}}

colnames(resPlan) <- c("ressTappingMobile",
                       "ressTappingSettled",
                       "simuNb",
                       "satisfiedUsers")

resPlanBig %>% 
  tbl_df()

resPlanBig %>% 
  mutate(repNb = as.factor(simuNb %/% (length(rTMValues) * length(rTMValues)))) %>%
  ggplot() +
  geom_line(aes(y=satisfiedUsers, 
                x=t, 
                color = repNb, 
                group = simuNb)) +
  facet_grid(ressTappingMobile ~ ressTappingSettled, 
             labeller = label_both)

resPlanBig %>% 
  tbl_df() %>% 
  group_by(ressTappingMobile,
           ressTappingSettled,
           t) %>%
  summarise(expectedSatisfiedUsers = mean(satisfiedUsers),
            sdSatisfiedUsers = sd(satisfiedUsers)) %>%
  ggplot() +
  geom_line(aes(y=expectedSatisfiedUsers, 
                x=t)) +
  facet_grid(ressTappingMobile ~ ressTappingSettled, 
             labeller = label_both)

resPlan %>%
  as.data.frame() %>%
  ggplot() +
  geom_boxplot(aes(y = satisfiedUsers)) +
  facet_grid(ressTappingMobile ~ ressTappingSettled, 
             labeller = label_both)
  
resPlan %>%
  as.data.frame() %>% 
  group_by(ressTappingMobile,
           ressTappingSettled) %>%
  summarise(expectedSatisfiedUsers = mean(satisfiedUsers)) %>%
  ggplot() +
  geom_tile(aes(fill = expectedSatisfiedUsers, 
                y = ressTappingMobile, 
                x = ressTappingSettled))

write.table(x=resPlanBig,
            file = "simulation-results.csv",
            sep = ";",
            dec = ".",
            row.names = F)

