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

# Defining a simple simulateModel function
# Supose that the modele is loaded and init and step method are choosen
simulateWitUsesModel <- function(rTM, rTS, rTP, duration) {
  # Set attributes values
  r <- setNumericAttributeValue("ressTappingFromR",
                                "MobileUse",
                                rTM)
  r <- setNumericAttributeValue("ressTappingFromR",
                                "SettledUse",
                                rTS)
  r <- setNumericAttributeValue("ressTappingFromR",
                                "PickUse",
                                rTP)
  ####### Initialize the Cormas model #######
  r <- initSimu()
  
  ####### Run Simulation #######
  R <- runSimu(duration)
  
  ### Get results ####
  lastNbSatisfiedUsers <- getAttributesOfEntities("satisfiedUsers",
                                                  "Observer") %>%
    pull("satisfiedUsers")
  return(lastNbSatisfiedUsers)
}

# Defining a fitness function
nbReplication = 5
fitness <- function(genes, 
                    nbRep = nbReplication, 
                    simDuration = 50) {
  sUs <- NULL
  for (replicate in 1:nbRep) {
    sUs <- c(sUs, simulateWitUsesModel(genes$rTM, genes$rTS, genes$rTP, simDuration))
  }
  return(c(mean(sUs) , sd(sUs)))
}

resPlan <- NULL
resPlanBig <- NULL

#Definition de pla population initiale
popSize <- 10
pop <- data.frame(rTM = runif(popSize), # (Choose randomly a nb in ]0:1[)
                  rTS  = runif(popSize), 
                  rTP = runif(popSize),
                  fitnessA = NA,
                  fitnessB = NA) %>%
  tbl_df()

# decompte de l'exectution du plan d'expérience

numberOfGenerations <- 20

expPlanProgress <- txtProgressBar(min = 1,
                               max = numberOfGenerations,
                               style = 3)

# exectution de l'algo génétique
allPops <- pop
allPops$generation <- 0

for (generation in 1:numberOfGenerations){
  setTxtProgressBar(expPlanProgress, generation)
  # Compute fitness for each individual
  for (i in 1:length(pop)) {
    fit <- fitness(genes = pop[i,])
    pop$fitnessA[i] <- fit[1]
    pop$fitnessB[i] <- fit[2]
  }
  # Natural selection (les solutions dominées meurent)
  bestA <- max(pop$fitnessA)
  bestB <- min(pop$fitnessB)
  newPop <- pop %>% 
     mutate(alive = (fitnessA > bestA - 0.1) & 
            (fitnessB < bestB + 0.1)) %>%
    filter(alive) %>%
    select(- fitnessA, - fitnessB)

  # reproduction
  child <- data.frame(rTM = NA,
                      rTS  = NA, 
                      rTP = NA)
  while(dim(newPop)[1] < popSize ) {
    parents <- pop %>% sample_n(2)
    child$rTM <- parents$rTM[1]
    child$rTS <- parents$rTS[2]
    child$rTP <- parents$rTP[sample(c(1,2),1)]
    newPop <- newPop %>%
      union(child)
  }
  
  # Mutation
  newPop <- newPop %>% 
    select(starts_with("rT")) %>%
    rowwise() %>%
    mutate_all(~rnorm(1,.,sd = 0.1)) %>%
    mutate(fitnessA = NA, 
           fitnessB = NA)
  
  #Update Population
  pop$generation <- generation
  allPops <- allPops %>%
    union(pop)
  pop <- newPop

  #Plot Advances
allPops %>% 
 	gather("indicator", "value", -generation) %>%
	mutate(generation = as.factor(generation)) %>%
	ggplot() +
	geom_boxplot(aes(x = generation,
	 y = value, 
	color = indicator))
}

 
# Notes from the file 
  # Natural selection: pop$fitnessA <- maxmean(sUs), pop$fitnessB <-minsd(sUs)
  # Mutations : rTMValues <- c(0,1) rTSValues <- c(0,1) rTPValues <- c(0,1)
  


