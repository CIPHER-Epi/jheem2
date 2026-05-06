#' @title Create Custom Likelihood Instructions
#' @description
#' Create instructions for a likelihood that computes using a custom function.
#' @inheritParams create.basic.likelihood.instructions
#' @param compute.function A function that takes only arguments "sim", "log", and "weights", with optional "data" and "debug" arguments.
#' @details All weights used in the compute function should arrive in its "weights" argument in two ways:
#' From the "weights" argument of the likelihood instructions (which apply to these instructions only),
#' and additional weights passed top-down from joint likelihoods that will include this likelihood.
#' So, no weights should be passed in the "data" function, for example, to maintain consistency and accuracy.
#' @export
create.custom.likelihood.instructions <- function(name,
                                                  compute.function,
                                                  get.data.function = NULL,
                                                  weights = NULL,
                                                  verbose = F) {
    JHEEM.CUSTOM.LIKELIHOOD.INSTRUCTIONS$new(name = name,
                                             compute.function = compute.function,
                                             get.data.function = get.data.function,
                                             weights = weights,
                                             verbose = verbose)
}

JHEEM.CUSTOM.LIKELIHOOD.INSTRUCTIONS <- R6::R6Class(
    "jheem.custom.likelihood.instructions",
    inherit = JHEEM.LIKELIHOOD.INSTRUCTIONS,
    public = list(
        initialize = function(name,
                              compute.function,
                              get.data.function,
                              weights = NULL,
                              verbose = verbose) {
            error.prefix = "Error initializing 'jheem.custom.likelihood.instructions': "

            # *name* is a single non-NA, non-empty character vector
            if (!is.character(name) || length(name) > 1 || is.null(name) || is.na(name)) {
                stop(paste0(error.prefix, "'name' must be a single non-NA, non-empty character vector"))
            }
            
            # *compute.function* is a function taking only args "sim", "log", and "weights" and optionally "data" and "debug"
            if (!is.function(compute.function))
                stop(paste0(error.prefix, "'compute.function' must be a function"))
            if (!(all(c("sim", "log", "weights") %in% names(formals(compute.function)))) ||
                !all(names(formals(compute.function)) %in% c("sim", "log", "weights", "data", "debug")))
                stop(paste0(error.prefix, "'compute.function' must be a function taking only arguments ('sim', 'log', and 'weights')  along with optional 'data' and 'debug' arguments"))

            # *get.data.function* is a function taking only args "version" and "location"
            if (is.null(get.data.function))
            {}
            else if (!is.function(get.data.function) || !setequal(names(formals(get.data.function)), c("version", "location")))
                stop(paste0(error.prefix, "'get.data.function' must be a function taking only arguments 'version' and 'location'"))
            
            private$i.name = name
            private$i.compute.function = compute.function
            private$i.get.data.function = get.data.function
            private$i.weights = weights
        },
        instantiate.likelihood = function(version,
                                          location,
                                          additional.weights=1,
                                          verbose=F) {
            JHEEM.CUSTOM.LIKELIHOOD$new(instructions = self,
                                        version=version,
                                        location=location,
                                        verbose=verbose,
                                        additional.weights=additional.weights)
        },
        equals = function(other) {}
    ),
    active = list(
        compute.function = function(value) {
            if (missing(value)) {
                private$i.compute.function
            } else {
                stop("Cannot modify a jheem.likelihood.instruction's 'compute.function' - it is read-only")
            }
        },
        get.data.function = function(value) {
            if (missing(value)) {
                private$i.get.data.function
            } else {
                stop("Cannot modify a jheem.likelihood.instruction's 'get.data.function' - it is read-only")
            }
        },
        weights = function(value) {
            if (missing(value)) {
                private$i.weights
            } else {
                stop("Cannot modify a jheem.likelihood.instruction's 'weights' - it is read-only")
            }
        },
        name = function(value) {
            if (missing(value)) {
                private$i.name
            } else {
                stop("Cannot modify a jheem.likelihood.instruction's 'name' - it is read-only")
            }
        }
    ),
    private = list(
        i.compute.function = NULL,
        i.get.data.function = NULL,
        i.name = NULL,
        i.weights = NULL
    )
)

JHEEM.CUSTOM.LIKELIHOOD <- R6::R6Class(
    "jheem.custom.likelihood",
    inherit = JHEEM.LIKELIHOOD,
    portable = F, # necessary??
    public = list(
        ### now takes a 'get.data' function takes 'version' and 'location'
        initialize = function(instructions,
                              version,
                              location,
                              additional.weights,
                              verbose) {
            
            # Purposely SKIP the super$initialize for likelihoods (STILL TRUE? CONSIDER REVISING THIS BECAUSE WE USE VERSION/LOCATION NOW)
            
            private$i.name <- instructions$name
            private$i.compute.function <- instructions$compute.function
            private$i.check.consistency.flag <- T
            
            # We will take the "additional.weights", which are a weights object, and pull out its total weight.
            # This is to simplify the code for users later. But no dimension values can be used with these weights as a result.
            # browser()
            if (is(additional.weights[[1]], "jheem.likelihood.weights")) {
                if (is.null(instructions$weights))
                    private$i.weights <- additional.weights[[1]]$total.weight
                else
                    private$i.weights <- instructions$weights * additional.weights[[1]]$total.weight  
            }
                
            else {
                if (is.null(instructions$weights))
                    private$i.weights <- additional.weights[[1]]
                else
                    private$i.weights <- instructions$weights * additional.weights[[1]]
                
            }
            if (!is.numeric(private$i.weights) || length(private$i.weights)!=1)
                stop("Error: custom likelihood didn't have a single numeric weight; contact Andrew")
            
            private$i.compute.function.takes.data = any(names(formals(private$i.compute.function)) == 'data')
            
            if (is.null(instructions$get.data.function))
                private$i.data = NULL
            else
            {
                # private$i.data <- instructions$get.data.function(version, location)
               tryCatch({private$i.data <- instructions$get.data.function(version, location)},
                        error=function(e) {stop(paste0("Error instantiating likelihood '", private$i.name, "': error in 'get.data.function'"))})
            }
        },
        check = function() {browser()}
    ),
    private = list(
        i.compute.function = NULL,
        i.compute.function.takes.data = NULL,
        i.data = NULL,
        
        do.compute = function(sim, log, use.optimized.get, check.consistency, debug) {
            if (private$i.compute.function.takes.data)
                likelihood = private$i.compute.function(sim=sim, data=private$i.data, log=log, weights=private$i.weights)
            else
                likelihood = private$i.compute.function(sim=sim, log=log, weights=private$i.weights)
            
            error.prefix = paste0("Error computing custom likelihood '", self$name, "': ")
            
            if (!is.numeric(likelihood) || length(likelihood)!=1 || is.na(likelihood))
            {
                .GlobalEnv$errored.likelihood = self
                .GlobalEnv$errored.sim = sim
                stop(paste0(error.prefix, "the likelihood value returned from the 'compute.function' for '", self$name,
                            "' must be a single numeric value. The simulation and likelihood which produced this error have been stored in the global environment as 'errored.sim' and 'errored.likelihood'"))
            }
            # error check: non-negative unless log
            if (!log && likelihood<0)
                stop(paste0(error.prefix, "the likelihood value returned from the 'compute.function' must be non-negative if 'log' is FALSE"))
            if (debug) browser()
            likelihood
        }
    )
)