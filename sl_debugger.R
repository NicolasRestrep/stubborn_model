library(tidyverse)
library(patchwork)
theme_set(theme_minimal())
#### Parameter Block ###
num_traits = 50
num_turns = 200
num_rounds = 4
N = 100
sd_exploration = 0.05
starting_error_sd = 0.0
alpha_pr_change = 0.0
prob_learning  = 1
prob_ind = 1 
genetic = F
beta_pr_change = 1
starting_subset = 2
transform_exp = 0.05
p_exp = 0.05

#### Helper function #### 
distance_transformation <- function(x) {
  
  distances <- abs(as.numeric(population[x, 5:ncol(population)]) - as.numeric(traits_df[traits_df$run ==
                                                                                          j & traits_df$generation == t, 
                                                                                        3:ncol(traits_df)]))
  distance_transformed <- 1 - (distances + distances^(transform_exp))
  fitness <- ifelse(distance_transformed == 0, 1, distance_transformed)
  return(sum(fitness, na.rm = T)/num_traits)
}

### The Model ####
# Keep record
traits_df <- matrix(NA,
                    nrow = num_rounds * num_turns,
                    ncol = 2 + num_traits)

# Change the column names
colnames(traits_df) <- c("run",
                         "generation",
                         map_chr(.x = 1:num_traits, ~ paste0("trait", .x)))

# Turn into a tibble for ease of manipulation
traits_df <- data.frame(traits_df)

# Populate the dataframe
traits_df <- traits_df %>%
  mutate(run = as.factor(rep(1:num_rounds, each = num_turns)),
         generation = rep(1:num_turns, num_rounds)) %>%
  mutate(across(.cols = 3:ncol(.), as.numeric))


# Output for the model
output <- matrix(NA,
                 nrow = num_rounds * num_turns,
                 ncol = 6 + num_traits)

# Name the columns
colnames(output) <- c(
  "run",
  "generation",
  "strategy",
  "prob_learning",
  "ind_learning",
  "avg_fitness",
  map_chr(.x = 1:num_traits, ~ paste0("mean_trait", .x))
)
output <- data.frame(output)
# Populate the dataframe
output <- output %>%
  mutate(run = as.factor(rep(1:num_rounds, each = num_turns)),
         generation = rep(1:num_turns, num_rounds)) %>%
  mutate(across(.cols = 4:ncol(.), as.numeric))

# Data frame to keep track of pr_change
probs_df <- matrix(data = NA,
                   ncol = num_traits + 1,
                   nrow = num_rounds)
colnames(probs_df) <- c("round",
                        paste0("trait", c(1:num_traits)))
probs_df <- data.frame(probs_df)


for (j in 1:num_rounds) {
  print(
    paste0(
      "working on: ",
      "trait change: ",
      alpha_pr_change,
      "learning: ",
      prob_learning,
      "individual: ",
      prob_ind,
      "round: ",
      j
    )
  ) 
  # Probabilities that each trait changes
  pr_change <- rbeta(n = num_traits,
                     shape1 = alpha_pr_change,
                     shape2 = beta_pr_change)
  probs_df[j, ] <- c(j, pr_change)
  
  # Draw initial values for the traits
  traits_init <- runif(n = num_traits,
                       min = 0,
                       max = 1)
  traits_df[traits_df$run == j &
              traits_df$generation == 1, 3:ncol(traits_df)] <- c(traits_init)
  # Dataframe for population
  population <- matrix(NA,
                       nrow = N,
                       ncol = 4 + num_traits)
  # Change the column names
  colnames(population) <- c(
    "strategy",
    "prob_learns",
    "prob_ind_explore",
    "fitness",
    map_chr(.x = 1:num_traits, ~ paste0("trait", .x))
  )
  population <- data.frame(population)
  
  population <- population %>%
    mutate(
      prob_learns = prob_learning,
      prob_ind_explore = prob_ind,
      strategy = sample(size = N, c("stay", "explore"), prob = c(1-p_exp, p_exp), replace = T),
      across(.cols = 2:ncol(.), as.numeric)
    )
  
  # Here we start to change from our previous model
  # Starting values are only a fraction 
  # This loop does it; every agent starts with half 
  for (s in 1:N) {
    
    traits_selected <- sample(1:num_traits, starting_subset)
    starting_values <- rep(NA_real_, num_traits)
    starting_values[traits_selected] <-  rnorm(mean = 0,
                                               sd = starting_error_sd,
                                               n = starting_subset) + traits_init[traits_selected]
    population[s, 5:(num_traits+4)] <- starting_values
    
  } 
  
  for (t in 1:num_turns) {
    #### One turn ####
    
    # 1. Change in traits?
    # Do the traits change?
    traits_change <- rbinom(n = num_traits,
                            size = 1,
                            prob = pr_change)
    
    
    if (t == 1) {
      traits_df[traits_df$run == j &
                  traits_df$generation == t, c(which(traits_change == 1) + 2)] <-
        c(runif(sum(traits_change)))
    } else {
      traits_df[traits_df$run == j &
                  traits_df$generation == t, 3:ncol(traits_df)] <-
        traits_df[traits_df$run == j &
                    traits_df$generation == t - 1, 3:ncol(traits_df)]
      traits_df[traits_df$run == j &
                  traits_df$generation == t, c(which(traits_change == 1) + 2)] <-
        c(runif(sum(traits_change)))
    }
    
    # 2. Learning 
    # How many learners?
    explorers <- which(population$strategy=="explore")
    
    learns <- rbinom(n = length(explorers),
                     size = 1,
                     prob = population$prob_learns[explorers])
    
    # from these who explores individually?
    individual_explorers <- rbinom(
      n = length(explorers[which(learns == 1)]),
      size = 1,
      prob = population[explorers[which(learns == 1)], ]$prob_ind
    )
    
    st <- Sys.time()
    # Update individual explorers
    if (sum(individual_explorers) > 0) {
      for (x in 1:sum(individual_explorers)) {
        learning_agent <- explorers[which(learns ==1)][which(individual_explorers == 1)][x]
        what_trait <- ifelse(is_empty(which(is.na(population[learning_agent,5:ncol(population)]))), 
                             sample(c(1:num_traits), 1), 
                             sample(which(is.na(population[learning_agent,5:ncol(population)])), 1))
        population[learning_agent,4+what_trait] <- rnorm(n = 1,
                                                         mean = traits_df[traits_df$run == j &
                                                                            traits_df$generation == t, 
                                                                          what_trait + 2],
                                                         sd = sd_exploration)
        
      }
    }
    et <- Sys.time()
    
    print(paste("explorers' loop", round(et-st, 4), sep = "-"))
    # Update non-explorers 
    # Count how many there are
    non_explorers <-  sum(learns) - sum(individual_explorers)
    
    st <- Sys.time()
    if (sum(non_explorers) > 0) {
      for (z in 1:non_explorers) {
        learning_nex <- explorers[which(learns ==1)][which(individual_explorers == 0)][z]
        demonstrator <- sample(c(1:N)[-learning_nex], 1)
        what_trait <- ifelse(is_empty(which(is.na(population[learning_nex,5:ncol(population)]))), 
                             sample(c(1:num_traits), 1), 
                             sample(which(is.na(population[learning_nex,5:ncol(population)])), 1))
        population[learning_nex,
                   what_trait + 3]  <-
          ifelse(is.na(population[demonstrator, what_trait + 3]), 
                 population[learning_nex,
                            what_trait + 3], 
                 population[demonstrator, what_trait + 3])
        
      }
    }
    et <- Sys.time()
    
    print(paste("learners' loop", round(et-st, 4), sep = "-"))
    
    # Record the means
    for (m in 1:num_traits) {
      output[output$generation == t &
               output$run == j, m + 5] <- mean(population[[m + 4]], na.rm = T)
    }
    
    # 3. Calculate fitness
    # Get difference between chosen trait and responses
    st <- Sys.time()
    fs <- map_dbl(c(1:N), 
                  distance_transformation)
    population$fitness <- fs
    et <- Sys.time()
    
    print(paste("fitness' loop", round(et-st, 4), sep = "-"))
    
    # 4. Agents copy each other's strategies 
    st <- Sys.time()
    demonstrators <- sample(c(1:N), N, replace = T)
    
    population$strategy <- if_else(
      population$fitness < population[demonstrators,]$fitness, 
      population[demonstrators,]$strategy, 
      population$strategy
    )
    et <- Sys.time()
    
    print(paste("strategy loop", round(et-st, 4), sep = "-"))
    # Record outputs 
    output[output$generation == t &
             output$run == j, ]$avg_fitness <-
      mean(population$fitness, na.rm = T)
    output[output$generation == t &
             output$run == j, ]$prob_learning <-
      mean(population$prob_learns)
    output[output$generation == t &
             output$run == j, ]$ind_learning <-
      mean(population$prob_ind_explore)
    output[output$generation == t &
             output$run == j, ]$strategy <- 
      sum(population$strategy=="explore")/N
  }
}



#### Plotting ####
prb_plot <- output %>% 
  ggplot(aes(x = generation, 
             y = strategy, 
             color = run)) + 
  geom_line()

fit_plot <- 
  output %>% 
  ggplot(aes(x = generation, 
             y = avg_fitness, 
             color= run)) + 
  geom_line() + 
  labs(title = "High individual learning; environment changes")

(fit_plot + prb_plot) + 
  plot_layout(guides = "collect") + 
  plot_annotation(title = "Copy pi if fitness is higher and pi is higher") & 
  theme(legend.position = "top")    