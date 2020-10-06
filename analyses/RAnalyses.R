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

# Defining a simple simulateModel function
# Supose that the modele is loaded and init and step method are choosen
simulateWitUsesModel <- function(rTM, rTS, rTP, duration) {
  
  # Set the basic init and step method
  r <- setInit("init") # Initialization choice
  r <- setStep("step:") # Scenario choice
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
nbReplication = 8
fitness <- function(genes, 
                    nbRep = nbReplication, 
                    simDuration = 150) {
  sUs <- NULL
  for (replicate in 1:nbRep) {
    sUs <- c(sUs, simulateWitUsesModel(genes$rTM, 
                                       genes$rTS,
                                       genes$rTP, 
                                       simDuration))
  }
  return(c(mean(sUs) , sd(sUs)))
}

resPlan <- NULL
resPlanBig <- NULL

#Definition de pla population initiale
popSize <- 20
pop <- data.frame(rTM = runif(popSize, min = 0.3, max = 0.5), # (Choose randomly a nb in ]0:1[)
                  rTS  = runif(popSize, min = 0.3, max = 0.6), 
                  rTP = runif(popSize, min = 0.2, max = 0.4),
                  fitnessA = NA,
                  fitnessB = NA) %>%
  tbl_df()

# decompte de l'exectution du plan d'expérience

numberOfGenerations <- 10

expPlanProgress <- txtProgressBar(min = 1,
                               max = numberOfGenerations,
                               style = 3)

# exectution de l'algo génétique
allPops <- NULL

for (generation in 1:numberOfGenerations){
  setTxtProgressBar(expPlanProgress, generation)
  # Compute fitness for each individual
  for (i in seq(dim(pop)[1])) {
    fit <- fitness(genes = pop[i,])
    pop$fitnessA[i] <- fit[1]
    pop$fitnessB[i] <- fit[2]
  }
  # Natural selection (les solutions dominées meurent)
  bestA <- max(pop$fitnessA, na.rm = T)
  bestB <- min(pop$fitnessB, na.rm = T)
  sdA <- sd(pop$fitnessA, na.rm = T)
  sdB <- sd(pop$fitnessB, na.rm = T)
  newPop <- pop %>% 
     mutate(alive = (fitnessA > (bestA - sdA)) & 
            (fitnessB < (bestB + sdB))) %>%
    filter(alive) %>%
    select(- fitnessA, - fitnessB, -alive)

  # Reproduction
  while(dim(newPop)[1] < popSize ) {
    child <- data.frame(rTM = NA,
                        rTS  = NA, 
                        rTP = NA)
    protoParents <- pop %>% 
      mutate(fitLevel = (fitnessA - fitnessB))
    popFitLevel <- sum(protoParents$fitLevel, na.rm = T)
    parents <- protoParents %>% 
      mutate(proba = fitLevel / popFitLevel) %>%
      sample_n(2, prob =proba, replace = T)
    child$rTM <- parents$rTM[1]
    child$rTS <- parents$rTS[2]
    child$rTP <- parents$rTP[sample(c(1,2),1)]
    
    # Child Mutation
    child <- child %>% 
      select(starts_with("rT")) %>%
      rowwise() %>%
      mutate_all(~rnorm(1,.,sd = 0.01)) %>%
      mutate(rTM = max(rTM,0)) %>%
      mutate(rTS = max(rTS,0)) %>%
      mutate(rTP = max(rTP,0)) %>%
      mutate(fitnessA = NA, 
             fitnessB = NA)
    
    newPop <- newPop %>%
      bind_rows(child)
  }
  
  # Update Population
  pop$generation <- generation
  if (is.null(allPops)) {
    allPops <- pop
  } else {
  allPops <- allPops %>%
    union(pop)
  }

  # Plot Advances
allPops %>% 
  select(generation, fitnessA, fitnessB) %>%
 	gather("indicator", "value", -generation) %>%
	mutate(generation = as.factor(generation)) %>%
	ggplot() +
	geom_boxplot(aes(x = generation,
	 y = value, 
	color = indicator)) +
  ggsave("suivi-algo.pdf")
  
  # Update actual population
  pop <- newPop
}

  
  
    


