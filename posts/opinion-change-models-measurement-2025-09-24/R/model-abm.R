OpinionAgent <- R6::R6Class(
  classname = "OpinionAgent",
  inherit = socmod::Agent,
  public = list(
    next_opinions = NULL,
    opinions = NULL,
    step_opinions = function() {
      if (is.null(self$next_opinions)) {
        stop("next_opinions is NULL; cannot step opinions")
      }
      
      self$opinions <- self$next_opinions
      self$next_opinions <- NULL
      
      return(invisible(self))
    },
    initialize = function(cultural_complexity = 2, 
                          bounded = TRUE, 
                          init_opinions = NULL) {
      
      # Initialize agent with init_opinions if given
      if (!is.null(init_opinions)) {
        
        if (length(init_opinions) != cultural_complexity) {
          stop("Length of init_opinions must match cultural_complexity")
        }
        
        self$opinions <- init_opinions
        
        return(invisible(self))
      }
      
      # Otherwise initialize agents randomly for convenience 
      if (bounded) {
        self$opinions <- runif(cultural_complexity, min = -1, max = 1)
      } else {
        self$opinions <- rnorm(cultural_complexity, mean = 0, sd = 1)
      }
      
      return(invisible(self))
    }
  ),
  private = list(
    
  )
)

turner_2020 <- function(focal_agent, partner, model) {
  # 
}



# Define opinion learning model dynamics
opinion_dyanmics <- make_model_dynamics(
  partner_selection = function(focal_agent, model) {
    
  },
  interaction = turner_2020,
  model_step = \(model) { purrr::walk(model$agents, ~ .x$step_opinions()) }
)