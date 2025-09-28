#---- AGENTS INIT FROM TIBBLE
agents_from_tbl <- function(tbl, Class) {
  stopifnot(is.data.frame(tbl))
  stopifnot(inherits(Class, "R6ClassGenerator"))
  
  purrr::pmap(tbl, function(...) {
    Class$new(...)
  })
}


df <- tibble::tibble(
  id = 1:3,
  init_opinions = list(c(0.2), c(-50), c(0.9)),
  alpha = c(1.0, 2.0, 1.5)
)

agents <- agents_from_tbl(df, OpinionAgent)
print(agents)
# check
stopifnot(agents[[1]]$opinions == c(0.2))
stopifnot(agents[[2]]$opinions == c(-50.0))
stopifnot(agents[[3]]$opinions == c(0.9))

#------- TIBBLE PRINTOUT OF A LIST OF AGENTS -----------
agents_to_tbl <- function(.agents, ..., .fields = NULL) {
  # capture bare names
  fields <- rlang::ensyms(...)
  
  # if none were provided, use .fields (character vector)
  if (length(fields) == 0 && !is.null(.fields)) {
    fields <- rlang::syms(.fields)
  }
  
  # guard: require something
  if (length(fields) == 0) {
    stop("You must specify fields via ... or .fields", call. = FALSE)
  }
  
  purrr::map_dfr(.agents, function(a) {
    vals <- purrr::map(fields, ~ a[[rlang::as_string(.x)]])
    names(vals) <- purrr::map_chr(fields, rlang::as_string)
    tibble::tibble(!!!vals)
  })
}


print(agents_to_tbl(agents, name, opinions))
