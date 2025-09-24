##
# posts/free-agents-adapt-better/model.R
#
# Modeling code to accompany week of 9/16 lecture "Free Agents Adapt Better than
# Conformist Puppets".
# 
# Author: Matt Turner <matt@mat.phd>
# Date: 2025-09-18
#


#-------- CONFORMITY ------------

#' Create a conformity learning instance of ModelDynamics.
#' 
#' Returns socmod::ModelDynamics
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
                        purrr::map_chr(model$agents, 
                                       \(a) a$get_behavior()))
  }
)


#' Create an AgentBasedModel with conformity learning model dynamics.
#' 
#' @returns socmod::AgentBasedModel
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


#' Create a single conformity ABM then run a simulation trial on it.
#' 
#' @returns socmod::Trial
single_conformity_trial <- function(n_agents = 10, 
                                    n_demonstrators = 10,
                                    stop = socmod::fixated) {
  
  abm <- make_conformity_abm(n_agents, n_demonstrators)
  
  trial <- run_trial(abm, stop = stop)
  
  return (trial)
}




#-------- SUCCESS BIAS ------------

# TODO modify conformity_dynamics below for success

#' Create a conformity learning instance of ModelDynamics.
#' 
#' Returns socmod::ModelDynamics
vicarity_dynamics <- make_model_dynamics(
  
  # Partner selection is adaptive; learners prefer more successful individuals
  partner_selection = \(focal_agent, model) {
    
    # Select n_demonstrators potential teachers
    M <- model$get_parameter("n_demonstrators")
    prospective_teacher_ids <- sample.int(n = N, size = M, replace = FALSE) 
    # Extract prospective teacher fitnesses
    prospective_fitnesses <- 
      purrr::map_dbl(prospective_teacher_ids, 
                     ~ model$get_agent(.x)$get_fitness())
      
    partner_idx <- sample(prospective_teacher_ids, size = 1, 
                          prob = prospective_fitnesses)
    
    partner <- model$get_agent(partner_idx)
    
    return (partner)
  },
  
  # Conformity interaction is not partner-based, so use dummy arg
  interaction = \(focal_agent, ., model) {
    
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


#' Create an AgentBasedModel with vicarity (success-biased) learning model dynamics.
#' 
#' @returns socmod::AgentBasedModel
make_vicarity_abm <- function(n_agents = 10, n_demonstrators = 5) {
  
  abm <- 
    make_abm(graph = igraph::make_empty_graph(n_agents), 
             model_dynamics = vicarity_dynamics,
             n_demonstrators = n_demonstrators
    ) |>
    initialize_agents(initial_prevalence = 0.2)
  
  abm$set_parameter("agent_behaviors", 
                    purrr::map_chr(abm$agents, \(a) a$get_behavior()))
  
  return (abm)
}