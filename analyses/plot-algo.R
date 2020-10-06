metapop <- read.table("res-algo.csv",
                    header = T,
                    sep="\t",
                    dec= ".") %>%
  tbl_df()

  metapop %>% 
    mutate(fit = fitnessA - fitnessB) %>%
    select(-fitnessA, -fitnessB) %>%
  #gather("gene","allele", - generation, -fit) %>%
  ggplot() +
  geom_point(aes( x = rTM,
                   y = rTS, 
                   color = generation),
             position = position_jitter(width = 0.03)) 
  
  metapop %>% 
    mutate(fit = fitnessA - fitnessB) %>%
    select(-fitnessA, -fitnessB) %>%
    mutate(generation = as.factor(generation)) %>%
    #gather("gene","allele", - generation, -fit) %>%
    ggplot() +
    geom_density_2d(aes( x = rTM,
                    y = rTP, 
                    #color = generation
                    ),
               #position = position_jitter(width = 0.03)
               ) +
    facet_wrap(~generation)
  
  