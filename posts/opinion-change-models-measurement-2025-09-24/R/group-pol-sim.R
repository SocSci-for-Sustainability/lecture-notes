source("posts/opinion-change-models-measurement-2025-09-24/R/model-abm.R")


library(purrr)

make_opinion_abm <- function(n_agents = 10, graph = NULL, agents = NULL, alpha = 1.0) {
  
  if (is.null(graph) & is.null(agents)) {
    graph <- igraph::make_empty_graph(n_agents)
  }
  
  if (is.null(agents)) {
    agents <- map(1:n_agents, 
                  \(i) OpinionAgent$new(id = i, )
  }
  
  abm <- 
    socmod::make_abm(graph = graph, 
                     agents = agents,
                     model_dynamics = opinion_dynamics)
  
  return(abm)
}

#-------- SCRATCH -----------
n_agents <- make_opinion_abm(n_agents = 10, graph = NULL, agents = NULL)

