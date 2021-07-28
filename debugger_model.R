#########################
##### Stubborn Model ####
#########################


#### Packages ####
library(tidyverse)
library(patchwork)
theme_set(theme_classic())

#### Parameters ####
num_traits <- 4
num_rounds <- 2 
num_turns <- 100
N <- 100
starting_error_sd <- 0.01
sd_exploration <- 0.01


#### Set-Up ####

# Keep record 
# Create a matrix first - cols number of traits 
traits_df <- matrix(NA, 
                    nrow = num_rounds * num_turns, 
                    ncol = 2 + num_traits)
# Change the column names
colnames(traits_df) <- c("run", 
                         "generation", 
                         map_chr(.x = 1:num_traits, ~ paste0("trait", .x)))

# Turn into a tibble for ease of manipulation
traits_df <- as_tibble(traits_df)

# Populate the dataframe
traits_df <- traits_df %>% 
  mutate(run = as.factor(rep(1:num_rounds, each = num_turns)),
         generation = rep(1:num_turns, num_rounds)) %>% 
  mutate(across(.cols = 3:ncol(.), as.numeric))


# Output for the model
output <- matrix(NA, 
                 nrow = num_rounds * num_turns, 
                 ncol = 5 + num_traits)

# Name the columns 
colnames(output) <- c("run", 
                      "generation", 
                      "avg_soc_learning_prob",
                      "avg_ind_learning_prob", 
                      "avg_fitness",
                      map_chr(.x = 1:num_traits, ~ paste0("mean_trait", .x)))
output <- as_tibble(output)
# Populate the dataframe
output <- output %>% 
  mutate(run = as.factor(rep(1:num_rounds, each = num_turns)),
         generation = rep(1:num_turns, num_rounds)) %>% 
  mutate(across(.cols = 3:ncol(.), as.numeric))


#### The Model ####
for (j in 1:num_rounds) {
  # Probabilities that each trait changes
  pr_change <- rbeta(n = num_traits, 
                     0.5, 
                     5)
  
  # Draw initial values for the traits 
  traits_init <- runif(n = num_traits,
                       min = 0,
                       max = 1)
  traits_df[traits_df$run==j&traits_df$generation==1,3:ncol(traits_df)] <- as.list(traits_init)
  # Dataframe for population
  population <- matrix(NA, 
                       nrow = N, 
                       ncol = 3 + num_traits)
  # Change the column names
  colnames(population) <- c("prob_soc_learns", 
                            "prob_ind_learns",
                            "fitness", 
                            map_chr(.x = 1:num_traits, ~ paste0("trait", .x)))
  population <- as_tibble(population)
  
  population <- population %>% 
    mutate(prob_soc_learns = rbeta(N, 2, 5),
           prob_ind_learns = rbeta(N, 0.8, 5),
           across(.cols = 2:ncol(.), as.numeric))
  
  # List of starting values (with slight error)
  list_starting_values <- map(.x = 1:num_traits,
                              ~ rnorm(mean = 0,
                                      sd = starting_error_sd,
                                      n = N) + rep(traits_init[.x], N))
  # Assign starting values 
  for (s in 1:num_traits) {
    population[,s+3] <- list_starting_values[[s]]
  }

  for (t in 1:num_turns) {
    #### One turn ####
    
    # 1. Change in traits? 
    # Do the traits change? 
    traits_change <- rbinom(n = 4, 
                            size = 1, 
                            prob = pr_change)
    
    if(t == 1) { 
      traits_df[traits_df$run==j&traits_df$generation==t,c(which(traits_change==1)+2)] <- as.list(runif(sum(traits_change)))
    } else { 
      traits_df[traits_df$run==j&traits_df$generation==t,3:ncol(traits_df)] <- traits_df[traits_df$run==j&traits_df$generation==t-1,3:ncol(traits_df)]
      traits_df[traits_df$run==j&traits_df$generation==t,c(which(traits_change==1)+2)] <- as.list(runif(sum(traits_change)))
    }
    
    # 2. Draw trait & learning 
    # One trait gets drawn 
    current_trait <- sample(1:4, 
                            size = 1)
    # How many social learners?
    learns <- rbinom(n = N, 
                     size = 1, 
                     prob = population$prob_soc_learns)
    # Update these 
    population[which(learns == 1),current_trait+3] <- sample(population[,current_trait+3][[1]],
                                                             sum(learns))
    
    # Now see who explores individually 
    explorers <- rbinom(n = N, 
                        size = 1, 
                        prob = population$prob_ind_learns)
    
    # Explorers get knowledge of the trait with some error
    population[which(explorers == 1),current_trait+3] <- rnorm(n = sum(explorers), 
                                                               mean = traits_df[traits_df$run==j&traits_df$generation==t,current_trait+2][[1]], 
                                                               sd = sd_exploration)
    # Record the means 
    for (m in 1:num_traits) {
      output[output$generation==t & 
               output$run==j,m+5] <- mean(population[[m+3]])
    }
    
    # 3. Calculate fitness 
    
    # Get difference between all traits and responses  
    # First get a matrix of the current values of the traits 
    # Same number of rows as agents 
    traits_mat <- matrix(unlist(rep(traits_df[traits_df$run==j&traits_df$generation==t,
                                              3:(2+num_traits)][,1:num_traits], N)), 
                         nrow = N, 
                         ncol = num_traits, 
                         byrow = T)
    
    # Now the fitness will be:
    # 1- the sum of absolute distances / M
    population$fitness <- 1-(rowSums(abs(population[4:ncol(population)] - traits_mat))/num_traits)

    
    # if fitnesses below 0 then 0
    
    population[population$fitness<=0,'fitness'] <- 0
    
    # If all fitnesses are 0, everyone dies
    if (sum(population$fitness)<= 0) {
      print("Everyone died!")
      break
    }
    
    # 4. Store population characteristics 
    output[output$generation==t & 
             output$run==j,]$avg_fitness <- mean(population$fitness, na.rm = T)
    output[output$generation==t & 
             output$run==j,]$avg_soc_learning_prob <- mean(population$prob_soc_learns)
    output[output$generation==t & 
             output$run==j,]$avg_ind_learning_prob <- mean(population$prob_ind_learns)
    
    
    # 5. Reproduction
    previous_population <- population
    population$fitness <- as.numeric(NA)
    
    if(t %% 1 == 0) {
    # Which ones survive?
    survivors <- rbinom(n = N, 
                        size = 1, 
                        prob = previous_population$fitness)
    # They reproduce 
    population[which(survivors==1),] <- previous_population[which(survivors==1),]
    
    # The rest get values re-initialized 
    population[which(survivors==0),"prob_soc_learns"] <- rbeta(sum(survivors==0), 2, 5)
    population[which(survivors==0), "prob_ind_learns"] <- rbeta(sum(survivors==0), 0.8, 5)
    for (k in 1:num_traits) {
      population[which(survivors==0),3+k] <- rnorm(n = sum(survivors==0),
                                                   mean = output[output$run==1 & 
                                                                   output$generation==1,5+k][[1]],
                                                   sd = starting_error_sd)
    }
    }
  }
  
}
