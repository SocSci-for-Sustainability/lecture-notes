


# Initialize the ABM and agents
abm <- 
  make_abm(graph = igraph::make_empty_graph(10)) |>
  initialize_agents(initial_prevalence = 0.2)

# Inspect the initialization to ensure 2 of 10 are A-doers
plot_network_adoption(
  abm, layout = igraph::in_circle(), 
  plot_mod = \(p) p + ggtitle("Adoption at t = 0")
)


conformity_dynamics <- make_model_dynamics(
  
  # Partner selection is not necessary in conformity
  partner_selection = \(focal_agent, model) {},
  
  # Conformity interaction is not partner-based, so use dummy arg
  interaction = \(focal_agent, ., model) {
    
    # Observe behaviors of randomly-chosen demonstrators
    behaviors_sample <- sample(model$get_parameter("agent_behaviors"), 
                               model$get_parameter("n_demonstrators"))
    
    # Count how many of each behavior is present
    behaviors_table <- table(behaviors_sample)
    
    # Sample behavior to copy, weighted by frequency; names are behaviors
    next_behavior <- sample(names(behaviors_table), size = 1, prob = behaviors_table)
    
    # Only need to set next behavior, payoffs irrelevant w/ conformity
    focal_agent$set_next_behavior(next_behavior)
  },
  # Use the learning model stepper that makes "next" behavior/payoff "current"
  model_step = \(model) {
    learning_model_step(model)
    
    model$set_parameter("agent_behaviors", 
                        purrr::map_chr(model$agents, \(a) a$get_behavior()))
  
  }
)


make_conformity_abm <- function(n_agents = 10, n_demonstrators = 5) {
  abm <- 
    make_abm(graph = igraph::make_empty_graph(n_agents), 
             model_dynamics = conformity_dynamics,
             n_demonstrators = n_demonstrators
    ) |>
    initialize_agents(initial_prevalence = 0.2)
  
  abm$set_parameter("agent_behaviors", 
                    purrr::map_chr(abm$agents, \(a) a$get_behavior()))

  return (abm)
}


single_conformity_trial <- function(n_agents = 10, n_demonstrators = 10) {
  
  abm <- make_conformity_abm(n_agents, n_demonstrators)
  
  # Stop at fixation by default
  trial <- run_trial(abm)
  
  return (trial)
}

# 
# tr1 <- single_conformity_trial()
