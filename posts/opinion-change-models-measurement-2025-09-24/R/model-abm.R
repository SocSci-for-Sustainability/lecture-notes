##
# 
# Lecture Notes Code 
# Date: 2025-09-27
# Author: Matt Turner <matt@mat.phd>
#
# Agent class and social influence for simulating opinion dynamics. 
#

# -------------------------------
# Stubbornness function
# -------------------------------
library(socmod)

stubbornness <- function(o, alpha) {
  val <- 1.0 / (1.0 + abs(o)^alpha)
  val[val < 0.0] <- 0.0  # numerical guard
  val
}


#' Opinion agent extends base socmod::Agent to add opinions and stubbornness
OpinionAgent <- R6::R6Class(
  classname = "OpinionAgent",
  inherit = socmod::Agent,
  
  public = list(
    # these are new fields in OpinionAgent that Agent doesn't have
    next_opinions = NULL,
    opinions = NULL,
    stubbornness = NULL,
    alpha = 1.0,  # 
    
    # make opinion stepping internal, unlike current social learning dynamics
    step_opinions = function() {
      if (is.null(self$next_opinions)) {
        stop("next_opinions is NULL; cannot step opinions")
      }
      
      # commit new opinions
      self$opinions <- self$next_opinions
      self$next_opinions <- NULL
      
      # return this OpinionAgent silently for chaining
      return(invisible(self))
    },
    
    # init for OpinionAgent$new constructor—`id` and `...` go to Agent$new
    initialize = function(id, cultural_complexity = 2, 
                          bounded = FALSE, max_op_mag = 1.0,
                          init_op_mean = 0.0, init_op_sd = 1.0,
                          init_opinions = NULL, alpha = 1.0, ...) {
      
      # base Agent class init for id (req'd); opt'l are name, behavior, fitness
      super$initialize(id = id, ...)
      
      # use initial opinions if user provides
      if (!is.null(init_opinions)) {
        self$opinions <- init_opinions
        cultural_complexity <- length(self$opinions)
        
      # otherwise create random opinions...
      } else {
        # ...use a uniform distro over -1 to 1 if bounded...
        if (bounded) {
          self$opinions <- runif(cultural_complexity, 
                                 min = -max_op_mag, 
                                 max = max_op_mag)
        # ...or a normal distribution if not.
        } else {
          self$opinions <- rnorm(cultural_complexity, 
                                 mean = init_op_mean, 
                                 sd = init_op_sd)
        }
      }
      
      self$alpha <- alpha
      
      # init stubbornness vector, one entry for each opinion in opinion vector
      self$stubbornness <- stubbornness(self$opinions, self$alpha)
      
      return(invisible(self))
    }
  ),
  private = list()
)



#' Bivalent social influence
#' 
#' negative = anti-consensus/repulsion
#' positive = consensus
#' zero = ignore
#'
#' @param focal_agent 
#' @param partner 
#' @param model 
#'
#' @returns
#' @export
social_influence <- function(focal_agent, partner, model) {
  
  dij <- mean(abs(focal_agent$opinions - partner$opinions))
  wij <- 1.0 - dij
  
  delta_ok <- 0.5 * wij * (partner$opinions - focal_agent$opinions)
  
  alpha <- model$get_parameter("alpha")
  stubbornness <- s_latent(focal_agent$opinions, alpha)
  
  focal_agent$stubbornness <- stubbornness
  focal_agent$next_opinions <- focal_agent$opinions + delta_ok * stubbornness
  
  return(invisible(focal_agent))
}


# select 
well_mixed_selection <- function(focal, model) { 
  if (model$self_influence) {
    agents_to_sample <- model$agents
  } else {
    agents_to_sample <- 
  }
  
  partner <- sample(agents_to_sample, 1)
  
  return(partner)
}

# Basic model dynamics for the no-
opinion_dynamics <- make_model_dynamics(
  partner_selection = well_mixed_selection,
  interaction = social_influence,
  
)


#--------- ad hoc unit tests, i.e., sanity checks ---------


# --- Stubbornness function tests ---
stopifnot(all.equal(s_latent(0, 1), 1))     # at 0 → 1
stopifnot(all.equal(s_latent(1, 1), 0.5))     # at ±1 → 0
stopifnot(s_latent(10, 1) < 0.1)            # large opinions ≈ 0

# --- Opinion update tests ---

# identical agents → no change
a <- OpinionAgent$new(id = 1, name = "yo", init_opinions = c(0))
b <- OpinionAgent$new(id = 2, name = "hey", init_opinions = c(0))
graph <- igraph::make_empty_graph(n = 2, directed = FALSE)
m <- socmod::make_abm(agents = c(a, b), alpha = 1.0)

social_influence(a, b, m)
stopifnot(all.equal(a$next_opinions, c(0)))

# similar opinions → small attraction
a <- OpinionAgent$new(id = 1, init_opinions = c(0.2))
b <- OpinionAgent$new(id = 2, init_opinions = c(0.4))
social_influence(a, b, m)
stopifnot(a$next_opinions > 0.2 & a$next_opinions < 0.4)

# distant opinions → repulsion
a <- OpinionAgent$new(id = 1, init_opinions = c(-0.9))
b <- OpinionAgent$new(id = 2, init_opinions = c(0.9))
social_influence(a, b, m)
stopifnot(abs(a$next_opinions) > abs(a$opinions))  # moves farther out

# 3D opinions → updates each dimension
a <- OpinionAgent$new(id = 1, init_opinions = c(-0.5, 0.0, 0.8))
b <- OpinionAgent$new(id = 2, init_opinions = c(0.5, -0.2, 0.9))
social_influence(a, b, m)
stopifnot(length(a$next_opinions) == 3)                # dimension preserved
stopifnot(length(a$stubbornness) == 3)                 # stubbornness matches opinions
stopifnot(!all(a$next_opinions == a$opinions))         # opinions updated

# --- Manual calculation check for the 3D case ---
opinions_a <- c(-0.5, 0.0, 0.8)
opinions_b <- c( 0.5, -0.2, 0.9)

# Step 1: social distanceat
dij <- mean(abs(opinions_a - opinions_b))  # (| -0.5 - 0.5 | + | 0 - (-0.2) | + | 0.8 - 0.9 |) / 3
# = (1.0 + 0.2 + 0.1) / 3 = 1.3 / 3 ≈ 0.4333

# Step 2: influence weight
wij <- 1 - dij  # ≈ 0.5667

# Step 3: raw delta
delta_ok <- 0.5 * wij * (opinions_b - opinions_a)
# = 0.2833 * (1.0, -0.2, 0.1)
# = (0.2833, -0.0567, 0.0283)

# Step 4: stubbornness
stubb <- s_latent(opinions_a, alpha = 1)
# = ( (1 - | -0.5 |^2) / (1 + | -0.5 |), 
#     (1 - | 0 |^2) / (1 + | 0 |), 
#     (1 - | 0.8 |^2) / (1 + | 0.8 |) )
# = ( (1 - 0.25)/(1 + 0.5), (1 - 0)/(1 + 0), (1 - 0.64)/(1 + 0.8) )
# = (0.75/1.5, 1, 0.36/1.8)
# = (0.5, 1.0, 0.2)

# Step 5: apply update
expected <- opinions_a + delta_ok * stubb
# = (-0.5, 0.0, 0.8) + (0.2833*0.5, -0.0567*1.0, 0.0283*0.2)
# = (-0.5 + 0.1417, 0.0 - 0.0567, 0.8 + 0.0057)
# ≈ (-0.3583, -0.0567, 0.8057)

stopifnot(all.equal(as.numeric(a$next_opinions), expected, tolerance = 1e-6))
