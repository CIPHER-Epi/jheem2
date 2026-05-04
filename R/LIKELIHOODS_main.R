LIKELIHOOD.CLASS.GENERATORS <- new.env()

## --------------------------##
## -- THE PUBLIC INTERFACE --##
## --------------------------##


#' @title Instantiate Likelihood
#' @inheritParams jheem2-likelihood-params
#' @param data.manager A jheem.data.manager object. May be NULL for Bernoulli likelihood instructions.
#' @param throw.error.if.no.data Should an error be thrown if no relevant data is found?
#' @param verbose Should detailed messages be printed out as we go
#' @export
instantiate.likelihood <- function(instructions,
                                   version,
                                   location,
                                   sub.version = NULL,
                                   data.manager = get.default.data.manager(), # pass this to the child classes; Bernoullis don't need this or next arg
                                   verbose = F,
                                   throw.error.if.no.data = F,
                                   error.prefix = "Error instantiating likelihood from instructions: ") {
    if (!R6::is.R6(instructions) || !is(instructions, "jheem.likelihood.instructions")) {
        stop(paste0(error.prefix, "'instructions' must be a 'jheem.likelihood.instructions' object"))
    }
    instructions$instantiate.likelihood(
        version = version,
        sub.version = sub.version,
        location = location,
        data.manager = data.manager,
        verbose = verbose,
        throw.error.if.no.data = throw.error.if.no.data,
        error.prefix = error.prefix
    )
}

#' @title Compute Likelihood
#' @inheritParams jheem2-likelihood-params
#' @param use.optimized.get Should the optimized sim$get be used? The optimized get only works on simsets of the same internal structure as that defined in the specification at the time this likelihood was instantiated, so use it for the MCMC but leave it FALSE for analysis.
#' @param check.consistency - Whether to spend time checking to make sure everything is internally consistent. Setting to F is faster, but may generate weird error messages if there are bugs
#' @export
compute.likelihood <- function(likelihood,
                               sim,
                               log = T,
                               use.optimized.get = F,
                               check.consistency = T,
                               debug = F) {
    error.prefix <- "Error computing likelihood: "
    if (!R6::is.R6(likelihood) || !is(likelihood, "jheem.likelihood")) {
        stop(paste0(error.prefix, "'likelihood' must be a 'jheem.likelihood' object"))
    }
    likelihood$compute(
        sim = sim,
        log = log,
        use.optimized.get = use.optimized.get,
        check.consistency = check.consistency,
        error.prefix = error.prefix,
        debug = debug
    )
}

#' @title Compute Joint Likelihood Piecewise
#' @inheritParams compute.likelihood
#' @details For likelihoods that are not joint likelihoods, this is the same as \code{\link{compute.likelihood()}}
#' @export
compute.likelihood.piecewise <- function(likelihood,
                                         sim,
                                         log = T,
                                         use.optimized.get = F,
                                         check.consistency = T,
                                         debug = F) {
    error.prefix <- "Error computing likelihood: "
    if (!R6::is.R6(likelihood) || !is(likelihood, "jheem.likelihood")) {
        stop(paste0(error.prefix, "'likelihood' must be a 'jheem.likelihood' object"))
    }
    likelihood$compute(
        sim = sim,
        log = log,
        use.optimized.get = use.optimized.get,
        check.consistency = check.consistency,
        debug = debug
    )
}

#' @title Compare Two Simsets
#' @description Compare how two simsets score on a likelihood.
#' @inheritParams jheem2-likelihood-params
#' @param sim1,sim2 'jheem.simulation.set' objects to compare
#' @param piecewise Should they be compared on each likelihood individually, if a joint likelihood is supplied?
#' @value The likelihood of sim2 divided by that of sim1 (if log is FALSE), or the 
#' log likelihood of sim2 minus that of sim1 (if log is TRUE).
#' @export
compare.sims <- function(likelihood, sim1, sim2, piecewise = T, log = F) {
    error.prefix <- "Error computing likelihood: "
    if (!R6::is.R6(likelihood) || !is(likelihood, "jheem.likelihood")) {
        stop(paste0(error.prefix, "'likelihood' must be a 'jheem.likelihood' object"))
    }
    likelihood$compare.sims(
        sim1 = sim1,
        sim2 = sim2,
        piecewise = piecewise,
        log = log,
        error.prefix = error.prefix
    )
}

## ---------------##
## -- CONSTANTS --##
## ---------------##


JHEEM.LIKELIHOOD.CODE.ITERATION <- "2.0"

## ----------------------------------##
## -- THE LIKELIHOOD MANAGER CLASS --##
##    (and related functionality)   ##
## ----------------------------------##

LIKELIHOOD.INSTRUCTIONS.MANAGER <- new.env()

## -----------------------------------------##
## -- THE LIKELIHOOD *INSTRUCTIONS* CLASS --##
## -----------------------------------------##

#'
do.instantiate.likelihood <- function(instructions,
                                      version,
                                      sub.version,
                                      location,
                                      data.manager,
                                      additional.weights,
                                      throw.error.if.no.data,
                                      verbose,
                                      error.prefix="") {
    # *error.prefix* is a single non-NA, non-empty character vector
    if (!is.character(error.prefix) || length(error.prefix) > 1 || is.na(error.prefix)) {
        stop(paste0(error.prefix, "'error.prefix' must be a single non-NA, non-empty character vector"))
    }
    
    # Forcibly append the outcome name to the error message, because joints have to declare a different one earlier
    error.prefix <- paste0(error.prefix, "Error initializing likelihood for '", instructions$name, "': ")
    
    likelihood.class.generator = LIKELIHOOD.CLASS.GENERATORS[[class(instructions)[1]]]
    if (is.null(likelihood.class.generator))
        stop(paste0(error.prefix, "' Please source your '", class(instructions)[1], "' for this likelihood again."))

    # *version* and *location* -- validated by 'jheem.likelihood' parent class 'jheem.entity'
    
    # *data.manager*  -- validated at the sub-class level if needed (maybe I'll REQUIRE NULL for Bernoulli at some point)
    
    # *throw.error.if.no.data* is a single boolean
    if (!is.logical(throw.error.if.no.data) || length(throw.error.if.no.data) > 1 || is.null(throw.error.if.no.data) || is.na(throw.error.if.no.data)) {
        stop(paste0(error.prefix, "'throw.error.if.no.data' must be a single logical value"))
    }
    
    # # Check that likelihood instructions are registered
    # if (is.null(private$i.code))
    #     stop(paste0(error.prefix, "likelihood instructions must be registered before instantiating a likelihood with them"))
    
    # All outcomes must be registered for the specification. *denominator.outcome.for.sim* will be checked at the subclass level when relevant
    simulation.metadata <- get.simulation.metadata(version = version,
                                                   location = location)
    if (!(instructions$outcome.for.sim %in% simulation.metadata$outcomes)) {
        stop(paste0(error.prefix, "likelihood instructions' 'outcome.for.sim' must be a registered outcome in the specification"))
    }
    
    likelihood.class.generator$new(
        instructions = instructions,
        version = version,
        sub.version = sub.version,
        location = location,
        data.manager = data.manager,
        additional.weights = additional.weights,
        verbose = verbose,
        throw.error.if.no.data = throw.error.if.no.data,
        error.prefix = error.prefix
    )
}


# An abstract class
# Needs to implement the following methods:
# - equals
# - will probably need to override the constructor BUT needs to call the superclass constructor

JHEEM.LIKELIHOOD.INSTRUCTIONS <- R6::R6Class(
    "jheem.likelihood.instructions",
    public = list(
        initialize = function(outcome.for.sim,
                              dimensions,
                              levels.of.stratification,
                              weights,
                              likelihood.class.generator,
                              name,
                              error.prefix = "Error initializing 'jheem.likelihood.instructions': ") {
            # *error.prefix* is a single non-NA, non-empty character vector
            if (!is.character(error.prefix) || length(error.prefix) > 1 || is.na(error.prefix)) {
                stop(paste0("Error initializing 'jheem.likelihood.instructions': ", "'error.prefix' must be a single non-NA, non-empty character vector"))
            }

            # *outcome.for.sim* is a single character vector
            if (!is.character(outcome.for.sim) || length(outcome.for.sim) > 1 || is.na(outcome.for.sim)) {
                stop(paste0(error.prefix, "'outcome.for.sim' must be a character vector of length 1"))
            }
            private$i.outcome.for.sim <- outcome.for.sim
            
            # *name* is a single character vector
            if (!is.character(name) || length(name) > 1 ||is.na(name)) {
                stop(paste0(error.prefix, "'name' must be a character vector of length 1"))
            }
            private$i.name = name

            # *dimensions* is a character vector with no NAs or duplicates, post conversion if NULL
            if (is.null(dimensions)) dimensions <- character(0)
            if (!is.character(dimensions) || any(is.na(dimensions)) || any(duplicated(dimensions))) {
                stop(paste0(error.prefix, "'dimensions' must be NULL or a character vector containing no NAs or duplicates"))
            }
            private$i.dimensions <- dimensions

            # *levels.of.stratification* is NULL or a numeric vector with no NAs or duplicates
            if (is.null(levels.of.stratification)) levels.of.stratification <- 0
            if (!is.numeric(levels.of.stratification) || any(is.na(levels.of.stratification)) || any(duplicated(levels.of.stratification)) || any(sapply(levels.of.stratification, function(x) {
                x < 0
            }))) {
                stop(paste0(error.prefix, "'levels.of.stratification' must be NULL or an integer vector containing no NAs, duplicates, or nonnegative numbers"))
            }
            private$i.stratifications <- private$generate.stratifications(
                dimensions = dimensions,
                levels.of.stratification = levels.of.stratification
            )

            # *weights* is NULL, a single numeric value, or a list containing only 1. jheem.likelihood.weights' objects whose dimensions, if any, are named in 'dimensions' (or are 'year') and 2. numeric vectors with nonnegative, non-NA values
            if (!is.null(weights) && (!is.numeric(weights) || length(weights) != 1 || is.na(weights)) && (!is.list(weights) || (any(sapply(weights, function(x) {
                !(is(x, "jheem.likelihood.weights") && all(names(x$dimension.values) %in% c(dimensions, "year"))) && (!is.numeric(x) || any(is.na(x)) || any(sapply(x, function(value) {
                    value < 0
                })))
            }))))) {
                stop(paste0(error.prefix, "'weights' must be NULL, a single numeric value, or a list containing only 1. jheem.likelihood.weights' objects whose dimensions, if any, are named in 'dimensions' (or are 'year') and 2. numeric vectors with nonnegative, non-NA values"))
            }
            private$i.weights <- private$generate.weights.from.weights.list(weights)

            # *likelihood.class.generator* is an R6ClassGenerator object.
            if (!R6::is.R6Class(likelihood.class.generator)) {
                stop(paste0(error.prefix, "'likelihood.class.generator' must be an R6ClassGenerator object"))
            }

            # *likelihood.class.generator* must have class with classname 'jheem.likelihood' somewhere in its chain of inheritance
            current.class <- likelihood.class.generator
            while (is.null(current.class) || current.class$classname != "jheem.likelihood") {
                if (is.null(current.class)) {
                    stop(paste0(error.prefix, "'likelihood.class.generator' must have class 'jheem.likelihood' in its chain of inheritance"))
                }
                current.class <- current.class$get_inherit()
            }
            # Add this class generator to the environment
            LIKELIHOOD.CLASS.GENERATORS[[class(self)[1]]] = likelihood.class.generator
        },
        instantiate.likelihood = function(version,
                                          location,
                                          sub.version = NULL,
                                          data.manager = get.default.data.manager(),
                                          throw.error.if.no.data = F,
                                          verbose = F,
                                          error.prefix = "") {
            
            do.instantiate.likelihood(instructions = self,
                                      version = version,
                                      sub.version = sub.version,
                                      location=location,
                                      data.manager = data.manager,
                                      additional.weights=NULL,
                                      throw.error.if.no.data = throw.error.if.no.data,
                                      verbose = verbose,
                                      error.prefix = error.prefix)
            
            
        },
        register = function(code,
                            description,
                            error.prefix = "Error registering likelihood instructions: ") {
            # *error.prefix* is a single non-NA, non-empty character vector
            if (!is.character(error.prefix) || length(error.prefix) > 1 || is.null(error.prefix) || is.na(error.prefix)) {
                stop(paste0(error.prefix, "'error.prefix' must be a single non-NA, non-empty character vector"))
            }

            # *code* is no more than 5 characters and contains only numbers, letters, periods, and dashes
            if (!is.character(code) || length(code) > 1 || is.null(code) || is.na(code) || nchar(code) > 5 || string.contains.invalid.characters(code, NUMBERS.LETTERS.DASH.PERIOD)) {
                stop(paste0(error.prefix, "code must be no more than 5 characters and contain only numbers, letters, periods, and dashes"))
            }

            # *description* is a single, non-NA character vector
            if (!is.character(description) || length(description) > 1 || is.null(description) || is.na(description)) {
                stop(paste0(error.prefix, "'description' must be a single non-NA, non-empty character vector"))
            }
            private$i.description <- description

            for (registered.code.name in names(LIKELIHOOD.INSTRUCTIONS.MANAGER)) {
                if (code == registered.code.name) {
                    if (!self$equals(get.likelihood.instructions(code, error.prefix))) {
                        stop(paste0(error.prefix, "other instructions are already registered with the code '", code, "'"))
                    }
                } else {
                    if (self$equals(get.likelihood.instructions(registered.code.name, error.prefix))) {
                        stop(paste0(error.prefix, "identical instructions are already registered under the code '", registered.code.name, "'"))
                    }
                }
            }
            private$i.code <- code
            LIKELIHOOD.INSTRUCTIONS.MANAGER[[private$i.code]] <- self
        },
        equals = function(other) {
            stop("The 'equals' method must be implemented at the sub-class level")
        },
        check = function() browser()
    ),
    active = list(
        name = function(value) {
            if (missing(value)) {
                private$i.name
            } else {
                stop("Cannot modify a jheem.likelihood.instruction's 'name' - it is read-only")
            }
        },
        code = function(value) {
            if (missing(value)) {
                private$i.code
            } else {
                stop("Cannot modify a jheem.likelihood.instruction's 'code' - it is read-only")
            }
        },
        description = function(value) {
            if (missing(value)) {
                private$i.description
            } else {
                stop("Cannot modify a jheem.likelihood.instruction's 'description' - they are read-only")
            }
        },
        outcome.for.sim = function(value) {
            if (missing(value)) {
                private$i.outcome.for.sim
            } else {
                stop("Cannot modify a jheem.likelihood.instruction's 'outcome.for.sim' - it is read-only")
            }
        },
        outcomes = function(value) {
            if (missing(value)) {
                private$i.outcome.for.sim
            } else {
                stop("Cannot modify a jheem.likelihood.instruction's 'outcomes' - it is read-only")
            }
        },
        dimensions = function(value) {
            if (missing(value)) {
                private$i.dimensions
            } else {
                stop("Cannot modify a jheem.likelihood.instruction's 'dimensions' - they are read-only")
            }
        },
        stratifications = function(value) {
            if (missing(value)) {
                private$i.stratifications
            } else {
                stop("Cannot modify a jheem.likelihood.instruction's 'stratifications' - it is read-only")
            }
        },
        weights = function(value) {
            if (missing(value)) {
                private$i.weights
            } else {
                stop("Cannot modify a jheem.likelihood.instruction's 'weights' - they are read-only")
            }
        }
    ),
    private = list(
        i.name = NULL,
        i.description = NULL,
        i.code = NULL,
        i.outcome.for.sim = NULL,
        i.dimensions = NULL,
        i.stratifications = NULL,
        i.weights = NULL,
        i.likelihood.class.generator = NULL,
        generate.stratifications = function(dimensions, levels.of.stratification) {
            levels.of.stratification <- as.integer(levels.of.stratification)
            output.stratifications <- list()
            if (is.null(levels.of.stratification)) levels.of.stratification <- 0
            for (level in sort(levels.of.stratification)) {
                if (level == 0) {
                    output.stratifications <- c(output.stratifications, "")
                } # where '' will have to be replaced with NULL during the pull. There is no other way that I've found to append something blank to the list.
                else {
                    output.stratifications <- c(output.stratifications, combn(sort(dimensions), level, simplify = F))
                }
            }
            output.stratifications
        },
        generate.weights.from.weights.list = function(weights) {
            if (is.numeric(weights)) {
                return(list(create.likelihood.weights(weights)))
            }
            output.weights <- list()
            total.recursive.weight <- 1
            for (w in weights) {
                if (is.numeric(w)) {
                    total.recursive.weight <- total.recursive.weight * prod(w)
                } else if (is(w, "jheem.likelihood.weights")) {
                    if (length(w$dimension.values) == 0 && w$is.recursive) {
                        total.recursive.weight <- total.recursive.weight * w$total.weight
                    } else {
                        output.weights <- c(output.weights, list(w))
                    }
                }
            }
            output.weights <- c(output.weights, list(create.likelihood.weights(total.weight = total.recursive.weight)))
        },
        stratification.lists.equal = function(strat.list.1, strat.list.2) {
            if (length(strat.list.1) != length(strat.list.2)) {
                F
            } else {
                all(sapply(1:length(strat.list.1), function(i) {
                    setequal(strat.list.1[[i]], strat.list.2[[i]])
                }))
            }
        }
    )
)


## --------------------------##
## -- THE LIKELIHOOD CLASS --##
## --------------------------##

# An abstract class
# Subclasses need to implement the following methods:
# - do.compute
JHEEM.LIKELIHOOD <- R6::R6Class(
    "jheem.likelihood",
    inherit = JHEEM.ENTITY,
    portable = F,
    public = list(
        initialize = function(instructions,
                              version,
                              sub.version,
                              location,
                              verbose,
                              error.prefix,
                              additional.weights=list()) {
            # Validate instructions
            # (the superclass constructor will validate version and location)

            # *error.prefix* is a single non-NA, non-empty character vector
            if (!is.character(error.prefix) || length(error.prefix) > 1 || is.null(error.prefix) || is.na(error.prefix)) {
                stop(paste0(error.prefix, "'error.prefix' must be a single non-NA, non-empty character vector"))
            }

            if (!is(instructions, "jheem.likelihood.instructions")) {
                stop(error.prefix, "'instructions' must be a 'jheem.likelihood.instructions'")
            }

            # Call the superclass (jheem.entity) constructor
            super$initialize(
                version = version,
                sub.version = sub.version,
                location = location,
                type = "likelihood",
                error.prefix = error.prefix
            )
            
            ## VALIDATE weights
            
            private$i.name <- instructions$name
            private$i.outcome.for.sim <- instructions$outcome.for.sim
            private$i.stratifications <- instructions$stratifications
            private$i.weights <- c(instructions$weights, additional.weights)

            # Set check.consistency flag
            private$i.check.consistency.flag <- T
        },
        compute = function(sim, log = T, use.optimized.get = F, check.consistency = private$i.check.consistency.flag, error.prefix = "Error computing likelihood: ", debug = F)
            {
            # VALIDATION PURPOSELY SKIPPED FOR TIME SAVING. ENSURE SIM IS A SIMULATION!
            if (!is.logical(check.consistency) || length(check.consistency) != 1 || is.na(check.consistency)) {
                stop(paste0(error.prefix, "'check.consistency' must be TRUE or FALSE"))
            }
            if (!is.character(error.prefix) || length(error.prefix) != 1 || is.na(error.prefix)) {
                stop(paste0(error.prefix, "'error.prefix' must be a single character value"))
            }
            if (check.consistency) {
                if (!is(sim, "jheem.simulation.set")) {
                    stop(paste0(error.prefix, "'sim' must be a 'jheem.simulation.set' object"))
                }
                if (!is.logical(log) || length(log) != 1 || is.na(log)) {
                    stop(paste0(error.prefix, "'log' must be TRUE or FALSE"))
                }
                if (!is.logical(use.optimized.get) || length(use.optimized.get) != 1 || is.na(use.optimized.get)) {
                    stop(paste0(error.prefix, "'use.optimized.get' must be TRUE or FALSE"))
                }
                if (sim$n.sim != 1) {
                    stop(paste0(error.prefix, "'sim' must have only one simulation in it"))
                }
                if (!is.logical(debug) || length(debug) != 1 || is.na(debug)) {
                    stop(paste0(error.prefix, "'debug' must be TRUE or FALSE"))
                }
                # Make sure that the sim has the years needed for the likelihood
                if (!all(private$i.years %in% sim$from.year:sim$to.year)) {
                    stop(paste0(error.prefix, "simulation does not have all needed years"))
                }
            }

            if (sim$is.degenerate) {
                if (log) {
                    return(-Inf)
                } else {
                    return(0)
                }
            }

            # Call the subclass function
            result <- private$do.compute(sim, log = log, use.optimized.get = use.optimized.get, check.consistency = check.consistency, debug = debug)

            # Flip check.consistency flag
            private$i.check.consistency.flag <- F

            return(result)
        },

        # A function-factory
        get.compute.function = function(default.log = T, default.use.optimized.get = F, check.consistency = T, debug = F) {
            function(sim, log = default.log, use.optimized.get = default.use.optimized.get) {
                self$compute(sim, log = log, use.optimized.get = F, check.consistency = check.consistency, debug = debug)
            }
        },

        # Override at the sub-class level, just for joint likelihood
        compute.piecewise = function(sim, log = T, use.optimized.get = F, check.consistency = T, error.prefix = "Error computing liklihood: ", debug = F) {
            self$compute(sim = sim, log = log, use.optimized.get = use.optimized.get, check.consistency = check.consistency, error.prefix = error.prefix, debug = debug)
        },

        # compare sims
        compare.sims = function(sim1, sim2, piecewise = T, log = F, error.prefix = "Error comparing sims: ") {
            if (!R6::is.R6(sim1) || !is(sim1, "jheem.simulation.set")) {
                stop(paste0(error.prefix, "'sim1' must be a 'jheem.simulation.set' object"))
            }
            if (!R6::is.R6(sim2) || !is(sim2, "jheem.simulation.set")) {
                stop(paste0(error.prefix, "'sim2' must be a 'jheem.simulation.set' object"))
            }
            if (!is.logical(piecewise) || length(piecewise) != 1 || is.na(piecewise)) {
                stop(paste0(error.prefix, "'piecewise' must be TRUE or FALSE"))
            }
            if (!is.logical(log) || length(log) != 1 || is.na(log)) {
                stop(paste0(error.prefix, "'log' must be TRUE or FALSE"))
            }
            if (!is.character(error.prefix) || length(error.prefix) != 1 || is.na(error.prefix)) {
                stop(paste0(error.prefix, "'error.prefix' must be a single character value"))
            }

            if (piecewise) {
                diff <- self$compute.piecewise(sim2, log=T) - self$compute.piecewise(sim1, log=T)
            } else {
                diff <- self$compute(sim2, log=T) - self$compute(sim1, log=T)
            }

            if (log) {
                diff
            } else {
                exp(diff)
            }
        },
        get.outcome.location.mapping = function() {
            return(NULL) # for now we'll just return NULL to make code work

            stop("The 'get.outcome.location.mapping' function must be overridden at the subclass-level for a jheem.likelihood")

            # @Andrew to fill in for subclasses
            # use the code in JHEEM_outcome_location_mapping
            # create.outcome.location.mapping for single-outcome likelihoods
            # join.outcome.location.mappings for joint likelihoods
        }
    ),
    active = list(
        name = function(value) {
            if (missing(value)) {
                private$i.name
            } else {
                stop("Cannot modify a jheem.likelihood.instruction's 'name' - it is read-only")
            }
        },
        outcome.for.sim = function(value) {
            if (missing(value)) {
                private$i.outcome.for.sim
            } else {
                stop("Cannot modify a jheem.likelihood.instruction's 'outcome.for.sim' - it is read-only")
            }
        }
    ),
    private = list(
        i.name = NULL,
        i.check.consistency.flag = NULL,
        i.years = NULL,
        i.outcome.for.sim = NULL,
        i.stratifications = NULL,
        i.weights = NULL,
        do.compute = function(sim, log, use.optimized.get = F, check.consistency, debug) {
            stop("The 'do.compute' function must be overridden at the subclass-level for a jheem.likelihood")
        },
        get.current.code.iteration = function() {
            JHEEM.LIKELIHOOD.CODE.ITERATION
        },
        get.likelihood.years = function(from.year,
                                        to.year,
                                        omit.years,
                                        data.manager,
                                        outcome.for.data,
                                        exclude.ontology.names = NULL) {
            if (from.year == -Inf || to.year == Inf) {
                data.manager.bounds <- data.manager$get.year.bounds.for.outcome(outcome.for.data, exclude.ontology.names = exclude.ontology.names)
                if (from.year == -Inf) {
                    from.year <- data.manager.bounds[["earliest.year"]]
                }
                if (to.year == Inf) {
                    to.year <- data.manager.bounds[["latest.year"]]
                }
            }
            setdiff(from.year:to.year, omit.years)
        }
    )
)

## ----------------------------------##
## -- THE LIKELIHOOD WEIGHTS CLASS --##
## ----------------------------------##

#' @title Create Weights to Be Applied in Calculating a JHEEM Likelihood
#'
#' @param total.weight The weight that is applied to all observations in the
#' @param dimension.values
#' @param is.recursive Should this weight apply to deeper levels of stratification? For example, if F and dimension.values is the default empty list, this weight would only apply to data points that are totals.
#'
#' @export
create.likelihood.weights <- function(total.weight = 1,
                                      dimension.values = list(),
                                      is.recursive=T) {
    JHEEM.LIKELIHOOD.WEIGHTS$new(
        total.weight = total.weight,
        dimension.values = dimension.values,
        is.recursive = is.recursive
    )
}

JHEEM.LIKELIHOOD.WEIGHTS <- R6::R6Class(
    "jheem.likelihood.weights",
    portable = F,
    public = list(
        initialize = function(total.weight,
                              dimension.values,
                              is.recursive) {
            error.prefix <- "Error creating likelihood weights: "
            
            # *total.weight* is a single numeric value that is not NA and > 0
            if (!is.numeric(total.weight) || length(total.weight) > 1 || is.null(total.weight) || is.na(total.weight) || !(total.weight > 0)) {
                stop(paste0(error.prefix, "'total.weight' must be a single non-NA, positive numeric value"))
            }
            private$i.total.weight <- total.weight
            
            # *dimension.values* is either (a) an empty list or (b) a named list with no duplicate names
            # we want to reorder the list to be in alphabetical order by dimension
            if (!is.list(dimension.values) || (length(dimension.values) > 0 && is.null(names(dimension.values))) || any(duplicated(names(dimension.values)))) {
                stop(paste0(error.prefix, "'dimension.values' must be an empty list or a named list with no duplicate names"))
            }
            private$i.dimension.values <- dimension.values[sort(names(dimension.values))]
            
            if (!is.logical(is.recursive) || length(is.recursive)!=1 || is.na(is.recursive))
                stop(paste0(error.prefix, "'is.recursive' must be T or F"))
            private$i.is.recursive = is.recursive
        },
        equals = function(other) {
            if (!is(other, "jheem.likelihood.weights")) {
                stop(paste0(error.prefix, "'other' must be a 'jheem.likelihood.weights' object"))
            }
            if (self$total.weight == other$total.weight) {
                if (setequal(names(self$dimension.values), names(other$dimension.values))) {
                    all(sapply(names(self$dimension.values), function(d) {
                        setequal(self$dimension.values[[d]], other$dimension.values[[d]])
                    }))
                } else {
                    F
                }
            } else {
                F
            }
        }
    ),
    active = list(
        total.weight = function(value) {
            if (missing(value)) {
                private$i.total.weight
            } else {
                stop("Cannot modify a jheem.likelihood.weights' 'total.weight' - it is read-only")
            }
        },
        dimension.values = function(value) {
            if (missing(value)) {
                private$i.dimension.values
            } else {
                stop("Cannot modify a jheem.likelihood.weights' 'dimension.values' - it is read-only")
            }
        },
        is.recursive = function(value) {
            if (missing(value)) {
                private$i.is.recursive
            } else {
                stop("Cannot modify a jheem.likelihood.weights' 'is.recursive' - it is read-only")
            }
        }
    ),
    private = list(
        i.total.weight = NULL,
        i.dimension.values = NULL,
        i.is.recursive = NULL
    )
)
