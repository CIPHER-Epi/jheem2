#' @title Make a single (joint) set of likelihood instructions out of two or more likelihood instructions objects
#'
#' @param ... One or more jheem.likelihood.instructions objects or lists which contain only jheem.likelihood.instructions objects #OR it could just be codes
#' @param additional.weights A single numeric value that applies weight to all instructions.
#' @export
join.likelihood.instructions <- function(..., additional.weights=NULL) {
    # Each argument is either an instructions object or a code.
    sub.instructions <- lapply(list(...), function(x) {
        if (is(x, "jheem.likelihood.instructions")) {
            x
        } else {
            get.likelihood.instructions(code = x)
        }
    })

    JHEEM.JOINT.LIKELIHOOD.INSTRUCTIONS$new(sub.instructions,
                                            additional.weights = additional.weights)
}


JHEEM.JOINT.LIKELIHOOD.INSTRUCTIONS <- R6::R6Class(
    "jheem.joint.likelihood.instructions",
    inherit = JHEEM.LIKELIHOOD.INSTRUCTIONS,
    public = list(
        initialize = function(sub.instructions,
                              additional.weights=NULL) {
            # Validate sub-instructions
            # Make sure to flatten out list of sub-instructions
            
            error.prefix <- "Error joining likelihood instructions: "
            
            LIKELIHOOD.CLASS.GENERATORS[[class(self)[1]]] = JHEEM.JOINT.LIKELIHOOD.INSTRUCTIONS
            
            # *additional.weights* is NULL or a single numeric value
            if (!is.null(additional.weights) && (!is.numeric(additional.weights) || length(additional.weights) != 1 || is.na(additional.weights))) {
                stop(paste0(error.prefix, "'additional.weights' must be NULL or a single numeric value"))
            }
            
            # We'll extend this list so that it has one weights object (or number "1") per sub instruction, flattened if joint.
            # Then, we'll generate a proper weight combining the "additional.weights" passed in so that it applies recursively when joints are nested.
            private$i.additional.weights <- list()

            for (sub.instr in sub.instructions) {
                private$i.name <- paste0(private$i.name, sub.instr$name, sep = "__")
                if (is(sub.instr, "jheem.joint.likelihood.instructions")) {
                    private$i.sub.instructions <- c(private$i.sub.instructions, sub.instr$sub.instructions)
                    private$i.additional.weights <- c(private$i.additional.weights, unlist(sub.instr$additional.weights, recursive = F))
                } else {
                    private$i.sub.instructions <- c(private$i.sub.instructions, sub.instr)
                    private$i.additional.weights <- c(private$i.additional.weights, create.likelihood.weights(1))
                }
            }
            private$i.name <- trimws(private$i.name, "both", "[__]")
            names(private$i.sub.instructions) <- strsplit(private$i.name, "__")[[1]]
            # browser()
            private$i.additional.weights <- lapply(private$i.additional.weights, function(w) {
                private$generate.weights.from.weights.list(list(additional.weights, w))
            })
            names(private$i.additional.weights) <- names(private$i.sub.instructions)
        },
        equals = function(other) {
            if (!is.null(self$code) && !is.null(other$code)) {
                self$code == other$code
            } else {
                if (is(other, "jheem.joint.likelihood.instructions")) {
                    all(sapply(self$sub.instructions, function(x) {
                        any(sapply(other$sub.instructions, function(y) {
                            x$equals(y)
                        }))
                    }))
                } else {
                    F
                }
            }
        },
        instantiate.likelihood = function(version,
                                          location,
                                          sub.version = NULL,
                                          data.manager = get.default.data.manager(),
                                          throw.error.if.no.data = F,
                                          verbose = F,
                                          error.prefix = "") {
            # Validate location
            # Check that these instructions are registered?
            # Make sure all outcomes for the instructions are registered to the specification of the version
            JHEEM.JOINT.LIKELIHOOD$new(instructions=self,
                                       version=version,
                                       sub.version=sub.version,
                                       location=location,
                                       data.manager=data.manager,
                                       throw.error.if.no.data = throw.error.if.no.data,
                                       verbose = verbose,
                                       error.prefix=error.prefix)
        }
    ),
    active = list(
        sub.instructions = function(value) {
            if (missing(value)) {
                private$i.sub.instructions
            } else {
                stop("Cannot modify a jheem.likelihood.instruction's 'sub.instructions' - they are read-only")
            }
        },
        details = function(value) {
            if (missing(value)) {
                details <- NULL
                for (instr in private$i.sub.instructions) {
                    details <- rbind(details, instr$details)
                }
            } else {
                stop("Cannot modify a jheem.likelihood.instruction's 'details' - they are read-only")
            }
        },
        name = function(value) {
            if (missing(value)) {
                private$i.name
            } else {
                stop("Cannot modify a jheem.joint.likelihood.instruction's 'outcomes' - they are read-only")
            }
        },
        additional.weights = function(value) {
            if (missing(value)) {
                private$i.additional.weights
            } else {
                stop("Cannot modify a jheem.joint.likelihood.instruction's 'additional.weights' - they are read-only")
            }
        }
    ),
    private = list(
        i.sub.instructions = NULL,
        i.name = NULL,
        i.additional.weights = NULL
    )
)


JHEEM.JOINT.LIKELIHOOD <- R6::R6Class(
    "jheem.joint.likelihood",
    inherit = JHEEM.LIKELIHOOD,
    portable = F,
    public = list(
        initialize = function(instructions,
                              version,
                              sub.version,
                              location,
                              data.manager,
                              throw.error.if.no.data,
                              verbose,
                              error.prefix) {
            super$initialize(instructions = instructions,
                             version = version,
                             sub.version = sub.version,
                             location = location,
                             verbose = verbose,
                             error.prefix = error.prefix)

            private$i.sub.likelihoods <- lapply(seq_along(instructions$sub.instructions), function(i) {
                
                instr <- instructions$sub.instructions[[i]]
                additional.weights <- instructions$additional.weights[[i]]
                
                if (verbose)
                    print(paste0("Instantiating sub '",
                                 class(instr)[1], "' for '",
                                 instr$name, "'..."))
                
                # Need to pass additional weights to custom likelihoods, too!! (4/29/2026)
                if (is(instr, 'jheem.custom.likelihood.instructions'))
                    instr$instantiate.likelihood(version = version,
                                                 location = location,
                                                 additional.weights = additional.weights,
                                                 verbose = verbose)
                else if (is(instr,'jheem.ifelse.likelihood.instructions'))
                    do.ifelse.instantiate.likelihood(instructions = instr,
                                                     version = version,
                                                     sub.version = sub.version,
                                                     location = location,
                                                     data.manager=data.manager,
                                                     additional.weights = additional.weights,
                                                     throw.error.if.no.data=throw.error.if.no.data,
                                                     verbose = verbose,
                                                     error.prefix=error.prefix)
                else
                    do.instantiate.likelihood(instructions = instr,
                                              version = version,
                                              sub.version = sub.version,
                                              location = location,
                                              data.manager=data.manager,
                                              additional.weights = additional.weights,
                                              throw.error.if.no.data=throw.error.if.no.data,
                                              verbose = verbose,
                                              error.prefix=error.prefix)
            })
            
            names(private$i.sub.likelihoods) <- sapply(private$i.sub.likelihoods, function(lik) {
                lik$name # was outcomes
            })
            private$i.sub.likelihoods
        },
        get.outcome.location.mapping = function() {
            sub.lik.mappings <- lapply(private$i.sub.likelihoods, function(sub.lik) {
                sub.lik$get.outcome.location.mapping()
            })
            join.outcome.location.mappings(sub.lik.mappings)
        },
        compute.piecewise = function(sim, log = T, use.optimized.get = F, check.consistency = T, error.prefix = "Error computing likelihood: ", debug = F) {
            # why do I need to use return here?
            return(sub.values = sapply(private$i.sub.likelihoods, function(like) {
                like$compute(sim, log = log, use.optimized.get = use.optimized.get, check.consistency = check.consistency, error.prefix = error.prefix, debug = debug)
            }))
        },
        check = function() {
            browser()
        }
    ),
    active = list(
        sub.likelihoods = function(value) {
            if (missing(value)) {
                private$i.sub.likelihoods
            } else {
                stop("Cannot modify a jheem.likelihood.instruction's 'sub.likelihoods' - they are read-only")
            }
        }
    ),
    private = list(
        i.sub.likelihoods = NULL,
        do.compute = function(sim, log, use.optimized.get, check.consistency, debug) {
            sub.values <- sapply(private$i.sub.likelihoods, function(like) {
                like$compute(sim, log = log, use.optimized.get = use.optimized.get, check.consistency = check.consistency, error.prefix = "Error computing likelihood: ", debug)
            })
            # print(sub.values)
            if (log) {
                sum(sub.values)
            } else {
                prod(sub.values)
            }
        }
    )
)
