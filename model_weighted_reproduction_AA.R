library(tidyverse)
library(patchwork)
theme_set(theme_classic())


stubborn_model <- function(num_traits = 4,
                           num_turns = 200,
                           num_rounds = 2,
                           N = 100,
                           sd_exploration = 0.01,
                           starting_error_sd = 0.1) { 
  #### Set-Up ####
  
  # Keep record 
  traits_df <- tibble(run = as_factor(rep(1:num_rounds, each = num_turns)),
                      generation = rep(1:num_turns, num_rounds))
  
  columns_for_traits <- paste("trait", 1:num_traits, sep = "")
  
  traits_df[, columns_for_traits] = as.numeric(NA)

  # Output for the model
  output <- tibble(run = as_factor(rep(1:num_rounds, each = num_turns)),
                      generation = rep(1:num_turns, num_rounds), 
                      avg_soc_learning_prob = as.numeric(NA),
                      avg_ind_learning_prob = as.numeric(NA), 
                      avg_fitness = as.numeric(NA))
  
  columns_for_traits <- paste("mean_trait", 1:num_traits, sep = "")
  
  output[, columns_for_traits] = as.numeric(NA)
  
  #### The Model ####
  for (j in 1:num_rounds) {
    # Probabilities that each trait changes
    pr_change <- rbeta(n = num_traits, 
                       0.1, 
                       5)
    
    # TEST HERE!!!
    pr_change = rep(0, num_traits)
    # 
    
    # Draw initial values for the traits 
    traits_init <- runif(n = num_traits,
                         min = 0,
                         max = 1)
    traits_df[traits_df$run==j&traits_df$generation==1,3:ncol(traits_df)] <- as.list(traits_init)
    
    # Population 
    population <- tibble(prob_soc_learns = rbeta(N, 2, 5), 
                            prob_ind_learns = rbeta(N, 0.8, 5),
                            fitness = as.numeric(NA))
    
    columns_for_traits <- paste("trait", 1:num_traits, sep = "")
    
    population[, columns_for_traits] = as.numeric(NA)
    
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
      traits_change <- rbinom(n = num_traits, 
                              size = 1, 
                              prob = pr_change)
      
      # TEST HERE!
      # traits_change = rep(0,4)
      
      if(t == 1) { 
        traits_df[traits_df$run==j&traits_df$generation==t,c(which(traits_change==1)+2)] <- as.list(runif(sum(traits_change)))
      } else { 
        traits_df[traits_df$run==j&traits_df$generation==t,3:ncol(traits_df)] <- traits_df[traits_df$run==j&traits_df$generation==t-1,3:ncol(traits_df)]
        traits_df[traits_df$run==j&traits_df$generation==t,c(which(traits_change==1)+2)] <- as.list(runif(sum(traits_change)))
      }
      
      # 2. Draw trait & learning 
      # One trait gets drawn 
      current_trait <- sample(1:num_traits, 
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
      # Get difference between chosen trait and responses  
      population$fitness <- 1-abs(population[,3+current_trait][[1]] - traits_df[traits_df$run==j&traits_df$generation==t,
                                                                                2+current_trait][[1]])
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
      
      # if(t %% 1 == 0) {
      #   std_fitness <- population$fitness/sum(population$fitness)
      #   
      #   new_children <- sample(1:100, 
      #                          100, 
      #                          replace = T, 
      #                          prob = std_fitness)
      #   
      #   previous_population <- population
      #   
      #   for (i in 1:100) {
      #     population[i,] <- previous_population[new_children[i],]
      #   }
      #   
      #   population$fitness <- as.numeric(NA)
      #   
      # }
    }
    
  }
  
  return(list(output, 
              traits_df, 
              pr_change))
  
}

results <- stubborn_model(num_rounds = 4, 
                          num_turns = 500, num_traits = 10, sd_exploration = 0)

pop_df <- results[[1]]

pop_df %>% 
  ggplot(aes(x = generation, 
             y = avg_fitness)) +
  geom_line() + 
  facet_wrap(~run) +
  labs(x = 'Generation', 
       y = "Mean Fitness", 
       title = "Fitness across generations")


