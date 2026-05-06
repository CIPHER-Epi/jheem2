JHEEM.SIMULATION.CODE.ITERATION = '2.0'

##----------------------##
##----------------------##
##-- PUBLIC INTERFACE --##
##----------------------##
##----------------------##

#'@title Get Simulation Data
#'
#'@param outcomes A character vector with one or more outcomes for which to pull data. Must be a subset of sim$outcomes
#'@param keep.dimensions Which dimensions should be retained in the resulting array
#'@param dimension.values A set of dimension values according to which to subset the data (will be combined with ...)
#'@param ... The parameters in ... are interpreted as additional dimension values according to which to subset the data. Each must be named, and either a character, integer, or logical vector
#'@param check.consistency Whether to check the consistency of arguments. Setting to FALSE will yield faster performance but unintelligible error messages
#'@param drop.single.outcome.dimensions When set to FALSE, the resulting array will have 'outcome' as its last dimension regardless of how many outcomes we are getting data for. If TRUE, the returned array will omit that dimension when only one outcome is requested
#'@param error.prefix A character value to prepend to any errors generating in getting these data
#'
#'@details Returns an array whose dimensions are keep.dimensions - plus (if there is more than one outcome or drop.single.outcome.dimensions==FALSE) an 'outcome' dimension at the end
#'
#'@export
get.simset.data <- function(simset,
                            outcomes,
                            output = c('value', 'numerator', 'denominator')[[1]],
                            keep.dimensions=NULL,
                            dimension.values = list(),
                            ...,
                            check.consistency = T,
                            drop.single.outcome.dimension = T,
                            error.prefix = "Error getting simulation results: ")
{
    if (!is(simset, "R6") || !is(simset, "jheem.simulation.set"))
        stop("simset must be an R6 object of class 'jheem.simulation.set'") 
    
    simset$get(outcomes = outcomes,
               keep.dimensions = keep.dimensions,
               dimension.values = dimension.values,
               ...,
               check.consistency = check.consistency,
               drop.single.outcome.dimension = drop.single.outcome.dimension,
               error.prefix = error.prefix)
}

#'@title Get a Simulation Metadata Object
#'
#'@param version The version of the model specification (must have been previously registered) for which to get metadata
#'@param location A single character value representing the location for the metadata
#'@param error.prefix A string to prepend to any errors generated in getting the metadata object
#'@param from.year,to.year The years which a corresponding simulation will have data for
#'@param jheem.kernel Optional: the jheem.kernel on which to base the simulation metadata
#'@param update.labels If jheem.kernel is not NULL, whether to get updated labels from the latest specification (vs the specification as it was when the kernel was created)
#'
#'@details A simulation.metadata object contains metadata, such as the dim.names to which its contents will conform
#'
#'@export
get.simulation.metadata <- function(version, 
                                    location,
                                    from.year = NULL, 
                                    to.year = NULL,
                                    n.sim = 1,
                                    sub.version = NULL,
                                    jheem.kernel = NULL,
                                    update.labels = T,
                                    error.prefix = paste0("Error deriving the simulation-metadata for '", version, "' and location '", location, "': "))
{
    if (is.null(jheem.kernel))
    {
        jheem.kernel.or.specification = get.compiled.specification.for.version(version)
        specification.metadata = do.get.specification.metadata(jheem.kernel.or.specification, location, error.prefix = error.prefix)   
    }
    else
    {
        jheem.kernel.or.specification = jheem.kernel
        specification.metadata = jheem.kernel$specification.metadata
    }
    
    SIMULATION.METADATA$new(version = version,
                            location = location,
                            sub.version = sub.version,
                            metadata =  make.simulation.metadata.field(jheem.kernel.or.specification = jheem.kernel.or.specification,
                                                                       specification.metadata = specification.metadata,
                                                                       sub.version = sub.version,
                                                                       outcome.location.mapping = NULL,
                                                                       from.year = from.year,
                                                                       to.year = to.year,
                                                                       n.sim = n.sim,
                                                                       update.labels = update.labels,
                                                                       error.prefix = error.prefix),
                            type = "Simulation Metadata",
                            error.prefix = error.prefix)
}

#'@title Rerun a Simulation Set
#'
#'@inheritParams create.jheem.engine
#'@param simset The JHEEM simulation set to re-run
#'@param verbose Whether to print updates as simulations are run
#'@param sub.version The sub-version to run
#'@param from.year,to.year The years to keep in the simulation object
#'
#'@details Helpful if the specification has changed
#'
#'@export
rerun.simulations <- function(simset,
                              max.run.time.seconds=Inf,
                              sub.version = simset$sub.version,
                              from.year = simset$from.year,
                              to.year = simset$to.year,
                              verbose=T)
{
    if (!is(simset, 'jheem.simulation.set'))
        stop("Cannot rerun.simulations: simset must be an object of class 'jheem.simulation.set'")
    
    specification = get.compiled.specification.for.version(simset$version)

    if (verbose)
        cat("Creating engine to re-run simulations...")
    
    jheem.kernel = create.jheem.kernel(version = simset$version,
                                       location = simset$location)
    
    engine = do.create.jheem.engine(jheem.kernel = jheem.kernel,
                                    sub.version = sub.version,
                                    start.year = specification$start.year,
                                    end.year = to.year,
                                    max.run.time.seconds = simset$max.run.time.seconds,
                                    prior.simulation.set = NULL,
                                    keep.from.year = from.year,
                                    keep.to.year = to.year,
                                    intervention.code = simset$intervention.code,
                                    calibration.code = simset$calibration.code,
                                    solver.metadata = simset$solver.metadata,
                                    finalize = T,
                                    error.prefix = "Error re-running simulations: ")
    if (verbose)
        cat("done\n")
    
    update.every = ceiling(simset$n.sim/10)
    new.sims = lapply(1:simset$n.sim, function(i){
        
        if (verbose && ((i-1) %% update.every)==0)
        {
            update.until = min(simset$n.sim, i+update.every-1)
            cat("Re-running ",
                ifelse(update.until==i, "simulation", "simulations"),
                " ", i,
                ifelse(update.every==i, "", 
                       paste0(" to ", update.until)),
                "...", sep='')
        }
        
        sim = engine$run(parameters = simset$parameters[,i])
        
        if (verbose && (i%%update.every==0 || i==simset$n.sim))
            cat("done\n")
        
        sim
    })
    
    do.join.simulation.sets(new.sims, 
                            simulation.chain = simset$simulation.chain, 
                            finalize = T,
                            run.metadata = simset$run.metadata)
}

#'@title Run a Simulations from a Set of Parameters
#'
#'@param parameters The parameters to run simulations for. Can be either (a) a matrix with parameters on the rows, and one column for each set of parameters/simulation to run, with named rows or (b) a named numeric vector of parameters for a single simulation
#'@param calibration.code If parameters were derived from a previous calibration, the calibration code that should be attached to the resulting simulation set
#'@inheritParams create.jheem.engine
#'
#'@value An object of class jheem.simulation.set
#'
#'@export
run.simulations.from.parameters <- function(version,
                                            location,
                                            parameters,
                                            end.year,
                                            sub.version = NULL,
                                            max.run.time.seconds = Inf,
                                            calibration.code = NULL,
                                            solver.metadata = create.solver.metadata(),
                                            keep.from.year = start.year,
                                            keep.to.year = end.year,
                                            verbose = F)
{
    specification = get.compiled.specification.for.version(version)
    
    if (verbose)
        cat("Creating engine to run simulations...")
    
    if (!is.numeric(parameters) || any(is.na(parameters)))
        stop("'parameters' must be a numeric vector or matrix with no NA values")
    
    if (is.null(dim(parameters)))
    {
        if (is.null(dimnames(parameters))[[1]])
            stop("If 'parameters' is a matrix, it must have dimnames for the rows set")
    }
    else if (length(dim(parameters))==0)
    {
        if (is.null(names(parameters)))
            stop("If 'parameters' is a vector, it must be NAMED")
        
        param.names = names(parameters)
        dim(parameters) = c(length(parameters), 1)
        dimnames(parameters) = list(param.names, NULL)
    }
    else
        stop("'parameters' must be either a (numeric) vector or matrix")
    
    jheem.kernel = create.jheem.kernel(version = version,
                                       location = location)
    
    engine = do.create.jheem.engine(jheem.kernel = jheem.kernel,
                                    sub.version = sub.version,
                                    start.year = specification$start.year,
                                    end.year = end.year,
                                    max.run.time.seconds = max.run.time.seconds,
                                    prior.simulation.set = NULL,
                                    keep.from.year = keep.from.year,
                                    keep.to.year = keep.to.year,
                                    intervention.code = NULL,
                                    calibration.code = calibration.code,
                                    solver.metadata = solver.metadata,
                                    finalize = T,
                                    error.prefix = "Error re-running simulations: ")
    
    if (verbose)
        cat("done\n")    
    
    update.every = ceiling(simset$n.sim/10)
    n.sim = dim(parameters)[1]
    new.sims = lapply(1:n.sim, function(i){
        
        if (verbose && ((i-1) %% update.every)==0)
        {
            update.until = min(n.sim, i+update.every-1)
            cat("Re-running ",
                ifelse(update.until==i, "simulation", "simulations"),
                " ", i,
                ifelse(update.every==i, "", 
                       paste0(" to ", update.until)),
                "...", sep='')
        }
        
        sim = engine$run(parameters = parameters[,i])
        
        if (verbose && (i%%update.every==0 || i==n.sim))
            cat("done\n")
        
        sim
    })
    
    do.join.simulation.sets(new.sims)
    
}

#'@title Get MCMC Mixing Statistic
#'@description For one chain, the mixing statistic is the variance in the first
#' quarter of simulations divided by the variance in the last quarter of simulations.
#'
#'@param jheem2-simset-params
#'@param match.names (Optional) A regex on parameter names for which to compute the mixing statistic. If NULL, all parameters are used.
#'@param chains (Optional) Which chains to use, defaulting to all chains.
#'@param sort (Optional) Whether to sort the output in decreasing order by mixing statistic value, defaulting to TRUE.
#'@value A vector indexed by parameter
#'@export
get.mcmc.mixing.statistic = function(simset,
                                     match.names=NULL,
                                     chains = simset$unique.chains, # which chains to use
                                     sort = T)
{
    error.prefix = "Cannot get.mcmc.mixing.statistic: "
    if (!is(simset, 'jheem.simulation.set'))
        stop(paste0(error.prefix, "'simset' must be an object of class 'jheem.simulation.set'"))
    
    simset$get.mcmc.mixing.statistic(match.names=match.names,
                                     chains=chains,
                                     sort=sort)
}

#'@title Subset a Simulation Set
#'@inheritParams jheem2-simset-params
#'@param simulation.indices An integer or logical vector specifying which simulations to keep.
#'@export
subset.simset = function(simset,
                         simulation.indices)
{
    error.prefix = "Error subsetting jheem.simulation.set: "
    if (!is(simset, 'jheem.simulation.set'))
        stop(paste0(error.prefix, "'simset' must be an object of class 'jheem.simulation.set'"))
    
    simset$subset(simulation.indices)
}

#'@title Thin A Simulation Set
#'@description Subset a simulation set either by keeping every Nth simulation
#'or keeping a fraction of all simulations. One of 'n' or 'keep' can be specified,
#'with the other left as NULL.
#'@inheritParams jheem2-simset-params
#'@param n Every Nth simulation will be kept, counting backwards from the last sim (ie., if n=3, the last simulation and the fourth-to-last simulation will be kept, etc.).
#'@param allow.expand Whether to duplicate simulations if keep > simset$n.sim
#'@param k Either a number of simulations to keep or a fraction to keep (between 0 and 1). Simulations will be drawn uniformly from the set.
#'@details Decimal values above 1 are rounded down to the nearest whole number.
#'@export
thin.simset = function(simset,
                       n=NULL,
                       keep=NULL,
                       allow.expand=F)
{
    error.prefix = "Error thinning simulation.set: "
    if (!is(simset, 'jheem.simulation.set'))
        stop(paste0(error.prefix, "'simset' must be an object of class 'jheem.simulation.set'"))
    simset$thin(n=n,
                keep=keep,
                allow.expand=allow.expand)
}

#'@title Burn Simulations From a Simulation Set
#'@description Subset a simulation set by trimming off a portion of earlier simulations.
#'@inheritParams jheem2-simset-params
#'@param keep Either a number of simulations to keep or a fraction to keep (between 0 and 1). The latest simulations will be kept.
#'@details Decimal values above 1 are rounded down to the nearest whole number.
#'@export
burn.simset = function(simset,
                       keep=NULL)
{
    error.prefix = "Error burning sims from simulation.set: "
    if (!is(simset, 'jheem.simulation.set'))
        stop(paste0(error.prefix, "'simset' must be an object of class 'jheem.simulation.set'"))
    simset$burn(keep=keep)
}

#'@title Get Simulation Set Paramater Values
#'@inheritParams jheem2-simset-params
#'@param simulation.indices An integer or logical vector specifying which simulations to show in output. Defaults to the last simulation.
#'@param drop If the default TRUE, then an output with only one simulation will be coerced to a vector.
#'@value A vector of parameter values for a simulation or an array indexed by parameter name and simulation.
#'@export
get.simset.params = function(simset,
                             match.names=NULL,
                             simulation.indices=simset$n.sim,
                             drop=T)
{
    error.prefix = "Error getting params from simulation.set: "
    if (!is(simset, 'jheem.simulation.set'))
        stop(paste0(error.prefix, "'simset' must be an object of class 'jheem.simulation.set'"))
    simset$get.params(match.names=match.names,
                      simulation.indices=simulation.indices,
                      drop=drop)
}

#'@title Create Traceplot of MCMC
#'@inheritParams jheem2-simset-params
#'@param burn How many iterations to burn and thus ignore on the plot, defaulting to 0.
#'@va;ie A ggplot of parameter value vs. iteration, faceted by parameter and colored by chain.
#'@export
mcmc.traceplot = function(simset,
                          match.names,
                          chains = simset$unique.chains,
                          burn = 0)
{
    error.prefix = "Error creating traceplot from simulation.set: "
    if (!is(simset, 'jheem.simulation.set'))
        stop(paste0(error.prefix, "'simset' must be an object of class 'jheem.simulation.set'"))
    simset$traceplot(match.names=match.names,
                     chains=chains,
                     burn=burn)
}

##-----------------------------------------------------------##
##-----------------------------------------------------------##
##-- INTERNAL (to the package) WRAPPERS OF THE CONSTRUCTOR --##
##-----------------------------------------------------------##
##-----------------------------------------------------------##

SINGLE.SIMULATION.MAKER = R6::R6Class(
    'single.simulation.maker',
    
    public = list(
        
        initialize = function(jheem.kernel,
                              sub.version,
                              from.year,
                              to.year,
                              solver.metadata,
                              intervention.code,
                              calibration.code,
                              outcome.location.mapping,
                              error.prefix)
        {
            private$i.jheem.kernel = jheem.kernel
            
            private$i.metadata = make.simulation.metadata.field(jheem.kernel.or.specification = jheem.kernel,
                                                                specification.metadata = jheem.kernel$specification.metadata,
                                                                sub.version = sub.version,
                                                                outcome.location.mapping = outcome.location.mapping,
                                                                from.year = from.year,
                                                                to.year = to.year,
                                                                n.sim = 1,
                                                                solver.metadata = solver.metadata,
                                                                intervention.code = intervention.code,
                                                                calibration.code = calibration.code,
                                                                update.labels = FALSE,
                                                                error.prefix = error.prefix)
        },
        
        make.simulation = function(outcome.numerators, # now must have sim dimension
                                   outcome.denominators, # now must have sim dimension
                                   parameters,
                                   run.metadata,
                                   is.degenerate,
                                   simulation.chain,
                                   finalize,
                                   error.prefix)
        {
            outcome.numerators.with.sim.dimension = lapply(outcome.numerators, function(arr) {
                new.dimnames = c(dimnames(arr), sim=1)
                #array(arr, dim=sapply(new.dimnames, length), new.dimnames)
                
                dim(arr) = sapply(new.dimnames, length)
                dimnames(arr) = new.dimnames
                arr
            })
            outcome.denominators.with.sim.dimension = lapply(outcome.denominators, function(arr) {
                if (is.null(arr)) return(arr)
                new.dimnames = c(dimnames(arr), sim=1)
                #array(arr, dim=sapply(new.dimnames, length), new.dimnames)
                
                dim(arr) = sapply(new.dimnames, length)
                dimnames(arr) = new.dimnames
                arr
            })
            parameters = matrix(parameters, ncol=1, dimnames=list(parameter=names(parameters), sim=NULL)) # used to say "parameters.indexed.by.sim = list('1'=parameters)" but I don't think I need the character number there. When simsets are joined, the index is meaningless anyways

            do.create.simulation.set.from.metadata(metadata = private$i.metadata,
                                                   jheem.kernel = private$i.jheem.kernel,
                                                   outcome.numerators = outcome.numerators.with.sim.dimension,
                                                   outcome.denominators = outcome.denominators.with.sim.dimension,
                                                   parameters = parameters,
                                                   simulation.chain = simulation.chain,
                                                   run.metadata = run.metadata,
                                                   is.degenerate = is.degenerate,
                                                   finalize = finalize,
                                                   error.prefix = error.prefix)
        }
    ),
    
    active = list(
        
        metadata = function(value)
        {
            if (missing(value))
                private$i.metadata
            else
                stop("Cannot modify a single.simulation.maker's metadata")
        }
    ),
    
    private = list(
        i.jheem.kernel = NULL,
        i.metadata = NULL
    )
)


derive.degenerate.simulation <- function(sim,
                                         from.year = sim$from.year,
                                         to.year = sim$to.year,
                                         intervention.code = sim$intervention.code,
                                         parameters = sim$parameters,
                                         run.metadata = sim$run.metadata,
                                         simulation.chain = sim$simulation.chain[1],
                                         error.prefix = 'Error deriving degenerate simulation')
{
    maker = SINGLE.SIMULATION.MAKER$new(jheem.kernel = sim$jheem.kernel,
                                        sub.version = sim$sub.version,
                                        from.year = from.year,
                                        to.year = to.year,
                                        solver.metadata = sim$solver.metadata,
                                        intervention.code = intervention.code,
                                        calibration.code = sim$calibration.code,
                                        outcome.location.mapping = sim$outcome.location.mapping,
                                        error.prefix = error.prefix)
    
    outcome.numerators = outcome.denominators = lapply(maker$metadata$outcome.ontologies, function(ont){
        array(as.numeric(NA),
              dim = sapply(ont, length), 
              dimnames = ont)
    })
    
    outcome.denominators[sapply(maker$metadata$outcome.metadata[names(outcome.denominators)], function(metadata){
        metadata$scale=='non.negative.number' || metadata$scale=='number'
    })] = NULL
    
    maker$make.simulation(outcome.numerators = outcome.numerators,
                          outcome.denominators = outcome.denominators,
                          parameters = parameters,
                          run.metadata,
                          is.degenerate = T,
                          simulation.chain = simulation.chain,
                          finalize = T,
                          error.prefix = error.prefix)
}

#'@title Join multiple simulation sets into a single set
#'
#'@param ... One or more jheem.simulation.set objects or lists that contain only jheem.simulation.set objects
#'
#'@return A jheem.simulation.set object
#'
#'@export
join.simulation.sets <- function(...)
{
    args = list(...)
    simset.list = list()
    
    for (one.arg in args)
    {
        if (is(one.arg, 'jheem.simulation.set'))
            one.arg = list(one.arg)
        
        if (!is.list(one.arg))
            stop("Cannot join.simulation.sets() - the arguments must be either jheem.simulation.set objects or lists that contain only jheem.simulation.set objects")
        
        if (any(!sapply(one.arg, is, 'jheem.simulation.set')))
            stop("Cannot join.simulation.sets() - the arguments must be either jheem.simulation.set objects or lists that contain only jheem.simulation.set objects")
        
        simset.list = c(simset.list, one.arg)
    }
    
    do.call(do.join.simulation.sets, simset.list)
}

# INTERNAL NOTE: NB - we have analogous functionality in assemble.simulations.from.calibration()
#                in CALIBRATION_main.R. If we ever edit this, we need to edit that in parallel too
#                (It is there for memory efficiency)
do.join.simulation.sets <- function(..., 
                                 simulation.chain=NULL,
                                 finalize=T, 
                                 run.metadata=NULL)
{
    # Validate
    # each argument must be either a simset or a list of simsets
    # browser()
    error.prefix = "Error joining simulation sets: "
    simset.list = unlist(list(...), recursive = F)
    
    if (any(sapply(simset.list, function(element) {!R6::is.R6(element) || !is(element, 'jheem.simulation.set')})))
        stop(paste0(error.prefix, "arguments must all be either 'jheem.simulation.set' objects or lists containing only 'jheem.simulation.set' objects"))
    
    # all simsets should have the same metadata. This will require implementing an equals() method in simulation metadata class
    
    new.n.sim = sum(sapply(simset.list, function(simset) {simset$n.sim}))

    sample.simset = simset.list[[1]]
    outcomes = sample.simset$outcomes
    
    outcome.dimnames = lapply(sample.simset$outcome.ontologies, function(outcome.ontology) {
        dim.names = outcome.ontology
        dim.names[['sim']] = 1:new.n.sim
        dim.names
    })
    
    combined.outcome.numerators = lapply(outcomes, function(outcome) {
        data.vec = lapply(simset.list, function(simset) {simset$data$outcome.numerators[[outcome]]})
        if (any(sapply(data.vec, is.null))) return(NULL)
        array(unlist(data.vec), sapply(outcome.dimnames[[outcome]], length), outcome.dimnames[[outcome]])
    })
    names(combined.outcome.numerators) = outcomes
    
    combined.outcome.denominators = lapply(outcomes, function(outcome) {
        data.vec = lapply(simset.list, function(simset) {simset$data$outcome.denominators[[outcome]]})
        if (any(sapply(data.vec, is.null))) return(NULL)
        array(unlist(data.vec), sapply(outcome.dimnames[[outcome]], length), outcome.dimnames[[outcome]])
    })
    names(combined.outcome.denominators) = outcomes
    
    combined.is.degenerate = unlist(sapply(simset.list, function(sim){
        sim$is.degenerate
    }))

    #combined.parameters = unlist(lapply(simset.list, function(simset) {simset$data$parameters}), recursive=F)
    
    combined.parameters = unlist(lapply(simset.list, function(simset){simset$parameters}))
    
    dim(combined.parameters) = c(parameter = dim(simset.list[[1]]$parameters)[1],
                                 sim = new.n.sim)
    dimnames(combined.parameters) = list(parameter = dimnames(simset.list[[1]]$parameters)[[1]],
                                         sim = NULL)
    
    if (is.null(run.metadata))
        run.metadata = join.run.metadata(lapply(simset.list, function(sim){sim$run.metadata}))
    
    intervention.code = sample.simset$intervention.code
    
    if (is.null(simulation.chain))
    {
        simulation.chain = as.integer(unlist(lapply(simset.list, function(sim){sim$simulation.chain})))
    }
    else
    {
        if (!is.numeric(simulation.chain) || any(is.na(simulation.chain)) || any(round(simulation.chain)!=simulation.chain))
            stop(paste0(error.prefix, "If it is not NULL, 'simulation.chain' must be an integer vector with no NA values"))
        
        if (length(simulation.chain)!=new.n.sim)
            stop(paste0(error.prefix, 
                        "If it is not NULL, 'simulation.chain' must have the same length as the number of joined simulations (",
                        new.n.sim, ")"))
    }
    
    do.create.simulation.set(jheem.kernel = sample.simset$jheem.kernel,
                             sub.version = sample.simset$sub.version,
                             outcome.numerators = combined.outcome.numerators,
                             outcome.denominators = combined.outcome.denominators,
                             parameters = combined.parameters,
                             simulation.chain = simulation.chain,
                             from.year = sample.simset$from.year,
                             to.year = sample.simset$to.year,
                             n.sim = new.n.sim,
                             intervention.code = intervention.code,
                             calibration.code = sample.simset$calibration.code,
                             outcome.location.mapping = sample.simset$outcome.location.mapping,
                             solver.metadata = sample.simset$solver.metadata,
                             run.metadata = run.metadata,
                             finalize = finalize,
                             is.degenerate = combined.is.degenerate)
}

'[.jheem.simulation.set' <- function(obj, x) {
    rv = obj$subset(x)
}

##-----------------------##
##-----------------------##
##-- CLASS DEFINITIONS --##
##-----------------------##
##-----------------------##


make.simulation.metadata.field <- function(jheem.kernel.or.specification,
                                           specification.metadata,
                                           sub.version,
                                           outcome.location.mapping,
                                           from.year = NULL,
                                           to.year = NULL,
                                           n.sim = 1,
                                           solver.metadata = NULL,
                                           intervention.code = NULL,
                                           calibration.code = NULL,
                                           type = "Simulation Metadata",
                                           years.can.be.missing = T,
                                           update.labels = T,
                                           error.prefix)
{
    # Validate from.year, to.year
    if (!is.logical(years.can.be.missing) || length(years.can.be.missing)!=1 || is.na(years.can.be.missing))
        stop(paste0(error.prefix, "'years.can.be.missing' must be a single, non-NA logical value"))
    
    if (is.null(from.year))
    {
        if (!years.can.be.missing)
            stop(paste0(error.prefix, "'from.year' and 'to.year' must both be specified (ie, non-NULL)"))
        
        if (!is.null(to.year))
            stop(paste0(error.prefix, "'to.year' cannot be set if 'from.year' is NULL"))
    }
    else
    {
        if (is.null(to.year))
        {
            if (years.can.be.missing)
                stop(paste0(error.prefix, "'from.year' cannot be set if 'to.year' is NULL"))
            else
                stop(paste0(error.prefix, "'from.year' and 'to.year' must both be specified (ie, non-NULL)"))
        }
        
        if(!is.numeric(from.year) || length(from.year)!=1 || is.na(from.year))
            stop(paste0(error.prefix, "'from.year' must be a single, non-NA numeric value"))
        
        if(!is.numeric(to.year) || length(to.year)!=1 || is.na(to.year))
            stop(paste0(error.prefix, "'to.year' must be a single, non-NA numeric value"))
        
        if (from.year > to.year)
            stop(paste0(error.prefix, "'from.year' (", from.year, ") must be BEFORE 'to.year' (", to.year, ")"))
    }
    
    metadata = list(from.year = from.year,
                              to.year = to.year)
    
    # n.sim must be an integer greater than 0.
    if (!is.numeric(n.sim) || length(n.sim)!=1 || is.na(n.sim) || n.sim%%1!=0)
        stop(paste0(error.prefix, "'n.sim' must be a whole number greater than zero"))
    metadata$n.sim = n.sim
    
    # Pull outcome ontologies and metadata from the specification
    
    metadata$outcome.ontologies = list()
    metadata$outcome.metadata = list()
    
    for (outcome.name in jheem.kernel.or.specification$get.outcome.names.for.sub.version(sub.version))
    {
        if (is(jheem.kernel.or.specification, 'jheem.kernel'))
            outcome = jheem.kernel.or.specification$get.outcome.kernel(outcome.name)
        else
            outcome = jheem.kernel.or.specification$get.outcome(outcome.name)
        
        if (!is.null(sub.version) || outcome$save)
        {
            sub.version.outcome.details = NULL
            if (!is.null(sub.version))
                sub.version.outcome.details = jheem.kernel.or.specification$outcome.sub.version.details[[sub.version]][[outcome.name]]
            
            if (outcome$is.cumulative && !is.null(metadata$from.year) && !is.null(metadata$to.year))
            {
                from.year = max(metadata$from.year, outcome$from.year)
                to.year = min(metadata$to.year, outcome$to.year)
                
                if (!is.null(sub.version))
                {
                    from.year = max(from.year, sub.version.outcome.details$from.year)
                    to.year = min(to.year, sub.version.outcome.details$to.year)
                }
                
                if (to.year>=from.year)
                    years.for.ont = as.character(from.year:to.year)
                else
                    years.for.ont = character()
            }
            else if (outcome$is.intrinsic && !is.null(metadata$from.year) && !is.null(metadata$to.year))
            {
                from.year = max(metadata$from.year, outcome$from.year)
                to.year = min(metadata$to.year+1, outcome$to.year)
                
                if (!is.null(sub.version))
                {
                    from.year = max(from.year, sub.version.outcome.details$from.year)
                    to.year = min(to.year, sub.version.outcome.details$to.year)
                }
                
                if (to.year>=from.year)
                    years.for.ont = as.character(from.year:to.year)
                else
                    years.for.ont = character()
            }
            else
                years.for.ont = NULL
            
            if (is.null(sub.version))
            {
                if (is(jheem.kernel.or.specification, 'jheem.kernel'))
                    base.ont = outcome$ontology
                else
                    base.ont = specification.metadata$apply.aliases(outcome$ontology, error.prefix=error.prefix)
            }
            else
            {
        tryCatch({
                base.ont = as.ontology(sub.version.outcome.details$dim.names)
        }, error = function(e){
            browser()
        })
            }
            
            base.ont = specification.metadata$apply.aliases(base.ont)
            ont = c(ontology(year=years.for.ont, incomplete.dimensions = 'year'), base.ont)
            metadata$outcome.ontologies[[outcome$name]] = ont
            
            metadata$outcome.metadata[[outcome$name]] = MODEL.OUTCOME.METADATA$new(outcome.metadata = outcome$metadata,
                                                                                             is.cumulative = outcome$is.cumulative,
                                                                                             is.intrinsic = outcome$is.intrinsic,
                                                                                             corresponding.observed.outcome = outcome$corresponding.data.outcome)
        }
    }
    
    # Pull in the outcome location mapping
    if (is.null(outcome.location.mapping))
        metadata$outcome.location.mapping = outcome.location.mapping = NULL
    else if (!is(outcome.location.mapping, 'outcome.location.mapping'))
        stop(paste0(error.prefix, "'outcome.location.mapping' must be an object of class 'outcome.location.mapping'"))
    else
        metadata$outcome.location.mapping = outcome.location.mapping
    
    
    # Validate solver.metadata
    if (!is.null(solver.metadata) && !is(solver.metadata, 'solver.metadata'))
        stop(paste0(error.prefix, "'solver.metadata' must be an object of class 'solver.metadata'"))
    
    # Validate intervention.code
    if (!is.null(intervention.code))
    {
        if (!is.character(intervention.code) || length(intervention.code)!=1 || is.na(intervention.code))
            stop(paste0(error.prefix, "'intervention.code' must be a single, non-NA character value"))
    }
    
    # Validate calibration.code
    if (!is.null(calibration.code))
    {
        if (!is.character(calibration.code) || length(calibration.code)!=1 || is.na(calibration.code))
            stop(paste0(error.prefix, "'calibration.code' must be a single, non-NA character value"))
    }

    metadata$solver.metadata = solver.metadata        
    metadata$intervention.code = intervention.code
    metadata$calibration.code = calibration.code
    metadata$sub.version = sub.version
    
    if (update.labels && 
        is(jheem.kernel.or.specification, 'jheem.kernel') && 
        is.specification.registered.for.version(jheem.kernel.or.specification$version))
    {
        spec = get.compiled.specification.for.version(jheem.kernel.or.specification$version)    
        metadata$labels = spec$labels    
    }
    else
        metadata$labels = jheem.kernel.or.specification$labels
        
    
    metadata
}


SIMULATION.METADATA = R6::R6Class(
    'simulation.metadata',
    inherit = JHEEM.ENTITY,
    portable = F,
    
    public = list(
        
        initialize = function(version,
                              location,
                              sub.version,
                              metadata,
                              type = "Simulation Metadata",
                              error.prefix)
        {
            # if (!is(jheem.kernel.or.specification, 'jheem.kernel') && 
            #     !is(jheem.kernel.or.specification, 'jheem.compiled.specification'))
            #     stop(paste0(error.prefix, "'jheem.kernel' must be an object of class jheem.kernel or jheem.compiled.specification"))
            
            #-- Call the superclass constructor --#
            super$initialize(version = version,
                             sub.version = sub.version,
                             location = location,
                             type = type,
                             error.prefix = error.prefix)
            
            private$i.metadata = metadata
        },
        
        # Returns the dimnames that the results of a call to simulation$get will have
        # It's arguments mirror simulation$get
        get.dim.names = function(outcomes,
                                 keep.dimensions=NULL, # will always include sim
                                 dimension.values = list(),
                                 ...,
                                 check.consistency = T,
                                 drop.single.outcome.dimension = T,
                                 drop.single.sim.dimension = F,
                                 error.prefix = "Error getting dimnames of simulation results: ")
        {
            dimension.values = private$process.dimension.values(dimension.values, ..., check.consistency=check.consistency, error.prefix=error.prefix)
            
            # Validate outcomes
            if (!is.character(outcomes) || length(outcomes)==0 || any(is.na(outcomes)))
                stop(paste0(error.prefix, "'outcomes' must be a non-empty character vector with no NA values"))
            invalid.outcomes = setdiff(outcomes, self$outcomes)
            if (length(invalid.outcomes)>0)
                stop(paste0(error.prefix, "Invalid ",
                            ifelse(length(invalid.outcomes)==1, 'outcome: ', 'outcomes: '),
                            collapse.with.and("'", invalid.outcomes, "'"),
                            ifelse(length(invalid.outcomes)==1, ' is', ' are'),
                            " not defined in the '", self$version, "' model specification"))
            
            
            # Pull the ontologies
            ontologies = lapply(outcomes, function(outcome){
                
                ont = self$outcome.ontologies[[outcome]]
                if (is.null(keep.dimensions))
                    keep.dimensions <<- intersect(names(ont),
                                                  union(names(ont)[!is.complete(ont)],
                                                        names(dimension.values)[sapply(dimension.values, length)>1]))
                if (check.consistency)
                {
                    # Make sure keep dimensions work
                    invalid.keep.dimensions = setdiff(keep.dimensions, names(ont))
                    if (length(invalid.keep.dimensions)>0)
                        stop(paste0(error.prefix, "For the '", outcome, 
                                    "' outcome, ",
                                    ifelse(length(invalid.keep.dimensions)==1, "dimension ", "dimensions "),
                                    collapse.with.and("'", invalid.keep.dimensions, "'"),
                                    ifelse(length(invalid.keep.dimensions)==1, " is requested as a keep.dimension, but is", " are requested as keep.dimensions, but are"),
                                    " not present in the ontology"))
                    
                    # Make sure dimension.values dimensions work
                    invalid.dimension.value.dimensions = setdiff(names(dimension.values), names(ont))
                    if (length(invalid.dimension.value.dimensions)>0)
                        stop(paste0(error.prefix, "For the '", outcome, 
                                    "' outcome, ",
                                    ifelse(length(invalid.dimension.value.dimensions)==1, "dimension ", "dimensions "),
                                    collapse.with.and("'", invalid.dimension.value.dimensions, "'"),
                                    ifelse(length(invalid.dimension.value.dimensions)==1, " is specified in dimension.values, but is", " are specified in dimension.values, but are"),
                                    " not present in the ontology"))
                    
                    # If any of the ontologies dimensions are NULL, they must have dimension.values set
                    null.dimensions.in.ontology = names(ont)[sapply(ont, is.null)]
                    missing.dimension.values = setdiff(null.dimensions.in.ontology, names(dimension.values))
                    if (length(missing.dimension.values)>0)
                        stop(paste0(error.prefix,
                                    "For the '", outcome, "' outcome, '",
                                    ifelse(length(missing.dimension.values)==1, "dimension ", "dimensions "),
                                    collapse.with.and("'", missing.dimension.values, "'"),
                                    " must have dimension.values specified (",
                                    ifelse(length(missing.dimension.values)==1, "it is", "they are"),
                                    " NULL in the ontology and must be specified in the get() call)"
                        ))
                    
                    
                    dimension.values = resolve.ontology.dimension.values(ont = ont,
                                                                         dimension.values = dimension.values,
                                                                         error.prefix = error.prefix)
                    ont[names(dimension.values)] = dimension.values
                    
                    
                    # The below should be rendered unnecessary by the resolve.ontology.dimension.values above
                    # HOWEVER - do we need to check that years is within to/from for complete outcomes? if years is NULL?
                    # Make sure dimension.values work
                    #                    for (d in names(dimension.values))
                    #                    {
                    #                        if (is.null(ont[[d]]))
                    #                        {
                    #                            # do we need to check that years is within to/from for complete outcomes?
                    #                        }
                    #                        else
                    #                        {
                    #                            invalid.dimension.values = setdiff(dimension.values[[d]], ont[[d]])
                    #                            if (length(invalid.dimension.values)>0)
                    #                                stop(paste0(error.prefix, "For the '", outcome, 
                    #                                            "' outcome, ",
                    #                                            collapse.with.and("'", invalid.dimension.values, "'"),
                    #                                            ifelse(length(invalid.dimension.value.dimensions)==1, " is an invalid value", " are invalid values"),
                    #                                            " for the '", d, "' dimension of the ontology",
                    #                                            ifelse(length(ont[[d]])<=6, 
                    #                                                   paste0(" (", paste0("'", ont[[d]], "'", collapse=", "), ")"),
                    #                                                   "")
                    #                                ))
                    #                        }
                    #                    }
                    
                }
                
                for (d in names(dimension.values))
                {
                    if (is.null(ont[[d]]))
                        ont[[d]] = dimension.values[[d]]
                }
                ont = ont[keep.dimensions]
                keep.dimension.values = dimension.values[intersect(keep.dimensions, names(dimension.values))]
                if (length(keep.dimension.values)>0 && !check.consistency)
                    ont[names(keep.dimension.values)] = resolve.ontology.dimension.values(ont, dimension.values=keep.dimension.values, error.prefix = error.prefix)
                
                ont
            })
            names(ontologies) = outcomes
            
            # Make sure the ontologies are congruous
            ont1 = as.list(ontologies[[1]])
            if (check.consistency)
            {
                outcome1 = outcomes[1]
                for (outcome2 in outcomes[-1])
                {
                    ont2 = as.list(ontologies[[2]])
                    
                    unequal.dimensions.mask = sapply(names(ont1), function(d){
                        !identical(ont1[[d]], ont2[[d]])
                    })
                    
                    if (any(unequal.dimensions.mask))
                    {
                        unequal.dimensions = names(ont1)[unequal.dimensions.mask]
                        stop(paste0(error.prefix, "Cannot get data for outcomes '", outcome1, "' and '", outcome2, 
                                    "' in one function call. Their ontologies have differing values for the ",
                                    collapse.with.and("'", unequal.dimensions, "'"),
                                    ifelse(length(unequal.dimensions)==1, " dimension.", " dimensions.")))
                    }
                }
            }
            # if (debug) browser()
            # Fold together, with outcome and sim as the last dimensions
            # ont1 = ontologies[[1]]
            if (!drop.single.sim.dimension || self$n.sim > 1)
                ont1 = c(ont1, list(sim=1:self$n.sim))
                # return(c(ont1, ontology(sim=1:self$n.sim, incomplete.dimensions = 'sim')))
            if (!drop.single.outcome.dimension || length(outcomes)>1)
                ont1 = c(ont1, list(outcome=outcomes))
            
            return (ont1)
        },
        
        prepare.optimized.get.instructions = function(outcomes,
                                                      keep.dimensions=NULL, # will always include sim
                                                      dimension.values = list(),
                                                      output = c('value', 'numerator', 'denominator')[[1]],
                                                      check.consistency = T,
                                                      drop.single.outcome.dimension = T,
                                                      drop.single.sim.dimension = F,
                                                      replace.inf.values.with.zero = T,
                                                      error.prefix = "Error preparing optimized get info: ")
        {
            if (!is.character(error.prefix) || length(error.prefix)!=1 || is.na(error.prefix))
                stop("Cannot prepare.optimized.get.instructions() - error.prefix must a single, non-NA character value")
            
            OPTIMIZED.GET.INSTRUCTIONS$new(
                sim.metadata = self,
                outcomes = outcomes,
                keep.dimensions = keep.dimensions,
                dimension.values = dimension.values,
                output = output,
                check.consistency = check.consistency,
                drop.single.outcome.dimension = drop.single.outcome.dimension,
                drop.single.sim.dimension = drop.single.sim.dimension,
                replace.inf.values.with.zero = replace.inf.values.with.zero,
                error.prefix = error.prefix
            )
        },
        
        get.labels = function(to.label)
        {
            if (!is.character(to.label) || any(is.na(to.label)))
                stop("Cannot get.labels() - 'to.label' must be a character vector with no NA values")

            labels = private$i.metadata$labels[to.label]
            
            if (is.null(labels))
            {
                labels = rep(NA, length(to.label))
                names(labels) = to.label
            }
            
            unlabeled.mask = is.na(labels)

            if (any(unlabeled.mask))
            {
                # Hard code for msm_idu or msm-idu
                newly.labeled = to.label[unlabeled.mask]
                newly.labeled = gsub('msm[_-]idu', 'MSM/PWID', newly.labeled, ignore.case = T)
                
                newly.labeled = private$str.to.title(newly.labeled)
                
                # Hard code for msm and idu
                newly.labeled = gsub('msm', 'MSM', newly.labeled, ignore.case = T)
                newly.labeled = gsub('idu', 'PWID', newly.labeled, ignore.case = T)
                
                labels[unlabeled.mask] = newly.labeled
            }        
            
            names(labels) = to.label
            labels
        },
        
        update.labels = function()
        {
            spec = get.specification.for.version(private$i.version)
            private$i.metadata$labels = spec$labels
            invisible(self)
        }
    ),
    
    active = list(
        metadata = function(value)
        {
            if (missing(value))
                private$i.metadata
            else
                stop("Cannot modify a simulation.metadata's 'metadata' - it is read-only")
        },
        
        outcomes = function(value)
        {
            if (missing(value))
                names(private$i.metadata$outcome.ontologies)
            else
                stop("Cannot modify a simulation.metadata's 'outcomes' - they are read-only")
        },
        
        non.intrinsic.outcomes = function(value)
        {
            if (missing(value))
                names(private$i.metadata$outcome.metadata[sapply(private$i.metadata$outcome.metadata, function(meta){!meta$is.intrinsic})])
            else
                stop("Cannot modify a simulation.metadata's 'outcomes' - they are read-only")
        },
        
        outcome.ontologies = function(value)
        {
            if (missing(value))
                private$i.metadata$outcome.ontologies
            else
                stop("Cannot modify a simulation.metadata's 'outcome.ontologies' - they are read-only")
        },
        
        outcome.metadata = function(value)
        {
            if (missing(value))
                private$i.metadata$outcome.metadata
            else
                stop("Cannot modify a simulation.metadata's 'outcome.metadata' - it is read-only")
        },
        
        from.year = function(value)
        {
            if (missing(value))
                private$i.metadata$from.year
            else
                stop("Cannot modify a simulation.metadata's 'from.year' - it is read-only")
        },
        
        to.year = function(value)
        {
            if (missing(value))
                private$i.metadata$to.year
            else
                stop("Cannot modify a simulation.metadata's 'to.year' - it is read-only")
        },
        
        outcome.location.mapping = function(value)
        {
            if (missing(value))
            {
                if (is.null(private$i.metadata$outcome.location.mapping))
                    create.default.outcome.location.mapping(jheem.kernel = private$i.jheem.kernel,
                                                            sub.version = private$i.sub.version)
                else
                    private$i.metadata$outcome.location.mapping
            }
            else
                stop("Cannot modify a simulation.metadata's 'outcome.location.mapping' - it is read-only")
        },
        
        n.sim = function(value)
        {
            if (missing(value))
                private$i.metadata$n.sim
            else
                stop("Cannot modify a simulation.set's 'n.sim' - it is read-only")
        },
        
        solver.metadata = function(value)
        {
            if (missing(value))
                private$i.metadata$solver.metadata
            else
                stop("Cannot modify a simulation.set's 'solver.metadata' - it is read-only")
        },
        
        calibration.code = function(value)
        {
            if (missing(value))
                private$i.metadata$calibration.code
            else
                stop("Cannot modify a simulation.set's 'calibration.code' - it is read-only")
        },
        
        intervention.code = function(value)
        {
            if (missing(value))
                private$i.metadata$intervention.code
            else
                stop("Cannot modify a simulation.set's 'intervention.code' - it is read-only")
        }
    ),
    
    private = list(
        
        i.metadata = NULL,
        
        get.current.code.iteration = function()
        {
            JHEEM.SIMULATION.CODE.ITERATION
        },
        
        process.dimension.values = function(dimension.values, ..., check.consistency, error.prefix)
        {
            # Validate dimension values (and fold in ...)
            dot.dot.dot = list(...)
            
            if (check.consistency) {
                check.dimension.values.valid(dot.dot.dot,
                                             variable.name.for.error = "The elements of ...",
                                             error.prefix = error.prefix,
                                             allow.empty = T)
                
                check.dimension.values.valid(dimension.values,
                                             variable.name.for.error = "dimension.values",
                                             error.prefix = error.prefix,
                                             allow.empty = T)
            }
            
            dimension.values[names(dot.dot.dot)] = dot.dot.dot
            
            # Additionally, we must check if the years are within range of "from.year" and "to.year"
            if (any(names(dimension.values)=='year')){
                dimension.values$year = as.character(dimension.values$year)
                
                if (!is.null(self$from.year) && !is.null(self$to.year))
                    dimension.values$year <- intersect(dimension.values$year, as.character(self$from.year:self$to.year))
            }
            # if (length(dimension.values[["year"]])==0)
            #     dimension.values <- dimension.values[names(dimension.values)!="year"]
            
            dimension.values
        },
        
        # Helpers for labels
        toupper.first = function(str)
        {
            str = as.character(str)
            mask = !is.na(str) & nchar(str)>0
            str[mask] = paste0(toupper(base::substr(str[mask], 1, 1)),
                               base::substr(str[mask], 2, nchar(str[mask])))
            str
        },
        
        str.to.title = function(str)
        {
            split.str = base::strsplit(str, "[^-a-zA-Z0-9\\/]", fixed=F)
            str = sapply(split.str, function(one.split){
                paste0(toupper.first(one.split), collapse=' ')
            })
            
            split.str = base::strsplit(str, "-", fixed=T)
            str = sapply(split.str, function(one.split){
                paste0(toupper.first(one.split), collapse='-')
            })
            
            str
        }
    )
    
)

OPTIMIZED.GET.INSTRUCTIONS = R6::R6Class(
    'jheem.simulation.optimized.get.instructions',
    
    public = list(
        
        initialize = function(sim.metadata,
                              outcomes,
                              keep.dimensions=NULL, # will always include sim
                              dimension.values = list(),
                              output = c('value', 'numerator', 'denominator')[[1]],
                              check.consistency = T,
                              drop.single.outcome.dimension = T,
                              drop.single.sim.dimension = F,
                              replace.inf.values.with.zero = T,
                              na.rm = T,
                              error.prefix = "Error preparing optimized get info: ")
        {
            # Set up the value dim.names
            private$i.value.dim.names = value.dim.names = sim.metadata$get.dim.names(
                outcomes = outcomes,
                keep.dimensions = keep.dimensions,
                dimension.values = dimension.values,
                check.consistency = check.consistency,
                drop.single.outcome.dimension = F,
                drop.single.sim.dimension = F,
                error.prefix = error.prefix
            )
            
            if (drop.single.outcome.dimension && length(value.dim.names$outcome)==1)
                private$i.value.dim.names = private$i.value.dim.names[setdiff(names(private$i.value.dim.names), 'outcome')]
            
            if (drop.single.sim.dimension && length(value.dim.names$sim)==1)
                private$i.value.dim.names = private$i.value.dim.names[setdiff(names(private$i.value.dim.names), 'sim')]
            
            # Set up the years

            # Convert year dimension values to character because later we do intersect and want predictable output even when base R can't decide
            # if intersect(numeric, character) is numeric (like in 2022) or character(like in 2024+)
            dimension.values$year <- as.character(dimension.values$year)
            
            raw.years = dimension.values$year
            if (is.null(raw.years) || length(raw.years)==0)
                stop(paste0(error.prefix, "'dimension.values' MUST contain values for 'year'"))
            private$i.target.years = as.numeric(raw.years)
            if (any(is.na(private$i.target.years)))
                stop(paste0(error.prefix, "'dimension.values$year' MUST only contain character representations of numeric values"))
            private$i.min.target.year = min(private$i.target.years)
            private$i.max.target.year = max(private$i.target.years)
            
            if (is.null(private$i.value.dim.names$year))
                stop(paste0(error.prefix, "'keep.dimensions' MUST include 'year'"))
            
            private$i.n.sim = sim.metadata$n.sim
            
            private$i.outcomes = outcomes
            private$i.n.per.outcome = prod(sapply(private$i.value.dim.names, length)) / length(outcomes)
            
            
            if (check.consistency && (!is.character(output) || length(output) != 1 || !(output %in% c('value', 'numerator', 'denominator'))))
                stop(paste0(error.prefix, "'output' must be one of 'value', 'numerator', or 'denominator'"))
            private$i.output = output
            
            if (!is.logical(replace.inf.values.with.zero) || length(replace.inf.values.with.zero)!=1 || is.na(replace.inf.values.with.zero))
                stop(paste0(error.prefix, "'replace.inf.values.with.zero', must be a single, non-NA, logical value"))
            private$i.replace.inf.values.with.zero = replace.inf.values.with.zero
            
            if (!is.logical(na.rm) || length(na.rm)!=1 || is.na(na.rm))
                stop(paste0(error.prefix, "'na.rm', must be a single, non-NA, logical value"))
            private$i.na.rm = na.rm
            
            private$i.info.by.outcome = lapply(private$i.outcomes, function(outcome){
                
                # Make sure output is appropriate to the outcome
                outcome.metadata = sim.metadata$outcome.metadata[[outcome]]
                if (output=='denominator' && 
                    (outcome.metadata$scale=='non.negative.number' || outcome.metadata$scale=='number'))
                {
                    stop(paste0(error.prefix,
                                "output is set to 'denominator', but the outcome '",
                                outcome, "' does not carry a denominator"))
                }
                
                # Set up the outcome ontology
                outcome.ontology = sim.metadata$outcome.ontologies[[outcome]]
                outcome.ontology$year = as.character(private$i.target.years)
                outcome.ontology$sim = as.character(1:private$i.n.sim)
                
                # Figure out which dimensions come before/after the year dimension
                year.dimension.index = (1:length(outcome.ontology))[names(outcome.ontology)=='year']
                before.year.mask = (1:length(outcome.ontology)) < year.dimension.index
                after.year.mask = (1:length(outcome.ontology)) > year.dimension.index
                
                # Set up the subset of the ontology from which we will draw values
                draw.from.dim.names = intersect.joined.dim.names(outcome.ontology, dimension.values)
                
                # Get indices from the ontology to the draw-from subset
                
                outcome.to.draw.from.indices = get.array.access.indices(arr.dim.names = outcome.ontology,
                                                                        dimension.values = draw.from.dim.names,
                                                                        index.from = 1)
                
                # Get indices into the dim.names (just for this outcome)
                outcome.subset.of.dim.names = value.dim.names
                outcome.subset.of.dim.names$outcome = outcome
                
                outcome.subset.to.dim.names = get.array.access.indices(arr.dim.names = private$i.value.dim.names,
                                                                       dimension.values = outcome.subset.of.dim.names,
                                                                       index.from = 0)
                
                draw.from.dim.names$outcome = outcome
                draw.from.to.outcome.subset = get.expand.array.indices(to.expand.dim.names = outcome.subset.of.dim.names,
                                                                       target.dim.names = draw.from.dim.names,
                                                                       index.from = 1)
                
                draw.from.to.dim.names.indices = outcome.subset.to.dim.names[draw.from.to.outcome.subset]
                
                
                list(
                    outcome = outcome,
                    n.before.year.dimension = prod(as.numeric(sapply(outcome.ontology[before.year.mask], length))),
                    n.after.year.dimension = prod(as.numeric(sapply(outcome.ontology[after.year.mask], length))),
                    to.indices = draw.from.to.dim.names.indices,
                    raw.from.indices = outcome.to.draw.from.indices,
                    result.indices = outcome.subset.to.dim.names,
                    pull.numerator.only = output=='numerator' || (output=='value' && (outcome.metadata$scale=='non.negative.number' || outcome.metadata$scale=='number')),
                    pull.denominator.only = output=='denominator',
                    pull.numerator.denominator.ratio = output=='value' && outcome.metadata$scale!='non.negative.number' && outcome.metadata$scale!='number'
                )
            })
            names(private$i.info.by.outcome) = private$i.outcomes
        },
        
        do.get = function(outcome.numerators, 
                          outcome.denominators,
                          n.sim,
                          sim.from.year,
                          sim.to.year,
                          error.prefix)
        {
            # Make sure n.sim is the same in sim and optimized.info
            if (n.sim != private$i.n.sim)
                stop(paste0(error.prefix, "The optimized.get.instructions was prepared for ", 
                            private$i.n.sim, ifelse(private$i.n.sim==1, ' simulation', " simulations"),
                            ", but the simulation.set contains ",
                            ifelse(n.sim < private$i.n.sim, "only ", ""),
                            n.sim, ifelse(n.sim==1, " simulation", " simulations")))
            
            # Make sure years can accomodate
            if (sim.from.year > private$i.min.target.year || sim.to.year < private$i.max.target.year)
                stop(paste0(error.prefix, "In order to execute this optimized get, the simulation must span at least from ",
                            private$i.min.target.year, " to ", private$i.max.target.year,
                            ", but it only spans from ", sim.from.year, " to ", sim.to.year, "."))
            
            # If we need to, re-index by year
            if (is.null(private$i.cached.from.year) || private$i.cached.from.year!=sim.from.year || private$i.cached.to.year!=sim.to.year)
            {
                private$i.info.by.outcome = lapply(private$i.info.by.outcome, function(info){
                    
                    year.indices = get_year_indices_for_optimized_info(outcome_years = as.numeric(dimnames(outcome.numerators[[info$outcome]])$year),
                                                                       target_years = private$i.target.years,
                                                                       n_before_year_dimension = info$n.before.year.dimension,
                                                                       n_after_year_dimension = info$n.after.year.dimension)
                    
                    if (is.null(year.indices))
                        stop(paste0(error.prefix, "There was an error in the cpp function to get year indices for outcome '", info$outcome, "'"))
                    
                    info$from.indices = year.indices[info$raw.from.indices]
                    
                    info
                })
            }
            
            # Do the get
            rv = do_optimized_get(numerators = outcome.numerators[private$i.outcomes],
                                  denominators = outcome.denominators[private$i.outcomes],
                                  info_by_outcome = private$i.info.by.outcome,
                                  n_to_per_outcome = private$i.n.per.outcome,
                                  avoid_infinite = private$i.replace.inf.values.with.zero,
                                  na_rm = private$i.na.rm)
            
            # Set dimnames and return
            dim(rv) = sapply(private$i.value.dim.names, length)
            dimnames(rv) = private$i.value.dim.names
            rv
        }
    ),
    
    active = list(
        
    ),
    
    private = list(
        
        i.outcomes = NULL,
        i.output = NULL,
        i.replace.inf.values.with.zero = NULL,
        i.na.rm = NULL,
        
        i.value.dim.names = NULL,
        i.target.years = NULL,
        i.info.by.outcome = NULL,
        
        i.n.per.outcome = NULL,
        
        i.min.target.year = NULL,
        i.max.target.year = NULL,
        i.cached.from.year = NULL,
        i.cached.to.year = NULL,
        
        i.n.sim = NULL
    )
)

##----------------------------##
##-- SIMULATION SET Objects --##
##----------------------------##

#'@title Load a Simulation Set
#'
#'@param file The file containing the saved simulation set
#'
#'@export
load.simulation.set <- function(file)
{
    x = load(file)
    if (length(x)!=1)
        stop("Error loading simset: the file to load must have only one object saved in it")
    
    simset = get(x)
    if (!is(simset, 'jheem.simulation.set'))
        stop("Error loading simset: the file to load does not contain a jheem.simulation.set object")
    
    copy.simulation.set(simset)
}

#'@title Make a copy of a Simulation Set
#'
#'@param simset The simulation.set object to copy
#'@param update.labels Whether to get updated labels from the latest specification (vs the specification as it was when the simulations were created)
#'
#'@export
copy.simulation.set <- function(simset, update.labels=T)
{
    if (!is(simset, 'jheem.simulation.set'))
        stop("Error copying simset: 'simset' must be a jheem.simulation.set object")
    
    # JHEEM.SIMULATION.SET$new(jheem.kernel = simset$jheem.kernel,
    #                          data = simset$data,
    #                          metadata = simset$metadata,
    #                          sub.version = simset$sub.version,
    #                          error.prefix = "Error loading simulation")
    
    
    # Eventually we can phase this out, but it lets us update chains from previously run simsets
    if (is.null(simset$simulation.chain))
    {
        if (simset$n.sim==1)
            simulation.chain = as.integer(NA)
        else
            simulation.chain = rep(1, simset$n.sim)
    }
    else
        simulation.chain = simset$simulation.chain
    
    do.create.simulation.set(jheem.kernel = simset$jheem.kernel,
                             sub.version = simset$sub.version,
                             outcome.numerators = simset$data$outcome.numerators,
                             outcome.denominators = simset$data$outcome.denominators,
                             parameters = simset$parameters,
                             simulation.chain = simulation.chain,
                             from.year = simset$from.year,
                             to.year = simset$to.year,
                             n.sim = simset$n.sim,
                             outcome.location.mapping = copy.outcome.location.mapping(simset$outcome.location.mapping),
                             calibration.code = simset$calibration.code,
                             intervention.code = simset$intervention.code,
                             run.metadata = copy.run.metadata(simset$run.metadata),
                             solver.metadata = copy.solver.metadata(simset$solver.metadata),
                             is.degenerate = simset$is.degenerate,
                             finalize = simset$is.finalized,
                             update.labels = update.labels)
}

do.create.simulation.set <- function(jheem.kernel,
                                     sub.version,
                                     outcome.numerators, # now must have sim dimension
                                     outcome.denominators, # now must have sim dimension
                                     parameters,
                                     simulation.chain,
                                     from.year,
                                     to.year,
                                     n.sim,
                                     solver.metadata,
                                     run.metadata,
                                     intervention.code,
                                     calibration.code,
                                     outcome.location.mapping,
                                     is.degenerate = NULL,
                                     finalize, #a logical - should we add sampled parameters?
                                     update.labels = T,
                                     error.prefix = "Error constructing simulation")
{
    metadata = make.simulation.metadata.field(jheem.kernel.or.specification = jheem.kernel,
                                              specification.metadata = jheem.kernel$specification.metadata,
                                              sub.version = sub.version,
                                              outcome.location.mapping = outcome.location.mapping,
                                              from.year = from.year,
                                              to.year = to.year,
                                              n.sim = n.sim,
                                              solver.metadata = solver.metadata,
                                              intervention.code = intervention.code,
                                              calibration.code = calibration.code,
                                              update.labels = update.labels,
                                              error.prefix = error.prefix)
    
    do.create.simulation.set.from.metadata(metadata = metadata,
                                           jheem.kernel = jheem.kernel,
                                           outcome.numerators = outcome.numerators,
                                           outcome.denominators = outcome.denominators,
                                           parameters = parameters,
                                           simulation.chain = simulation.chain,
                                           run.metadata = run.metadata,
                                           is.degenerate = is.degenerate,
                                           finalize = finalize,
                                           error.prefix = error.prefix)
}

do.create.simulation.set.from.metadata <- function(metadata,
                                                   jheem.kernel,
                                                   outcome.numerators,
                                                   outcome.denominators,
                                                   parameters,
                                                   simulation.chain,
                                                   run.metadata,
                                                   is.degenerate,
                                                   finalize,
                                                   error.prefix)
{
    
    # I have not yet written the validation code
    

    #-- Validate parameters --#
    
    # should be a matrix with one row for each sim
    # dimnames(parameters)[[1]] must be set
    # if (!is.matrix(parameters) || !is.numeric(parameters))
    #     stop(paste0(error.prefix, "'parameters' must be a numeric matrix"))
    # if (any(is.na(parameters)))
    #     stop(paste0(error.prefix, "'parameters' cannot contain any NA values"))
    # if (is.null(dimnames(parameters)) || is.null(dimnames(parameters)[[1]]) || any(is.na(dimnames(parameters)[[1]])) || any(nchar(dimnames(parameters)[[1]])==0))
    #     stop(paste0(error.prefix, "'parameters' must have dimnames set for its rows (dimension 1), and those names cannot be NA or empty strings"))
    # tabled.parameter.names = table(dimnames(parameters)[[1]])
    # if (any(tabled.parameter.names>1))
    #     stop(paste0(error.prefix, "the parameter names, as given by dimnames(parameters)[[1]], must be UNIQUE. ",
    #                 ifelse(sum(tabled.parameter.names>1)==1, "The parameter ", "Parameters "),
    #                 collapse.with.and("'", names(tabled.parameter.names[tabled.parameter.names>1]), "'"),
    #                 ifelse(sum(tabled.parameter.names>1)==1, " appears", " appear"),
    #                 " more than once."))
    
    
    #-- Store data --#
    data = list(outcome.numerators = outcome.numerators,
                outcome.denominators = outcome.denominators,
                parameters = parameters,
                simulation.chain = simulation.chain,
                unique.chains = sort(unique(simulation.chain[!is.na(simulation.chain)])),
                run.metadata = run.metadata,
                is.degenerate = is.degenerate,
                finalized = finalize)
    
    #-- If we are going to finalize: --#
    #   - Add a random seed to base future numbers on
    #   - Generate sampled parameters
    
    if (is.null(jheem.kernel$calibrated.parameters))
        data$seed = runif(metadata$n.sim, 0, .Machine$integer.max)
    else
        data$seed = apply(data$parameters[jheem.kernel$calibrated.parameters,,drop=F], 2, get.simulation.seed.from.parameters)

    # let's standardize the dimnames here
    dimnames(parameters) = list(parameter = dimnames(parameters)[[1]],
                                sim = as.character(1:metadata$n.sim))
    
    #-- Update the outcome metadata's years for each of the non-cumulative outcomes --#
    #-- Update the ontologies for non-cumulative, non-intrinsic outcomes --#
    outcomes = names(metadata$outcome.ontologies)
    for (outcome.name in outcomes)
    {
        outcome.metadata = metadata$outcome.metadata[[outcome.name]]
        if (!outcome.metadata$is.cumulative && !outcome.metadata$is.intrinsic)
        {
            metadata$outcome.ontologies[[outcome.name]]$year = dimnames(outcome.numerators[[outcome.name]])$year
        }
    }
    
    JHEEM.SIMULATION.SET$new(jheem.kernel = jheem.kernel,
                             data = data,
                             metadata = metadata,
                             sub.version = metadata$sub.version,
                             error.prefix = error.prefix)
}

JHEEM.SIMULATION.SET = R6::R6Class(
    'jheem.simulation.set',
    inherit = SIMULATION.METADATA,
    lock_object = F,
    portable = F,
    
    public = list(
        initialize = function(jheem.kernel,
                              data,
                              metadata,
                              sub.version,
                              error.prefix = "Error constructing simulation")
        {
            #-- Call the superclass constructor --#
            super$initialize(version = jheem.kernel$version,
                             location = jheem.kernel$location,
                             sub.version = sub.version,
                             metadata = metadata,
                             type = "Simulation Metadata",
                             error.prefix = error.prefix)
            
            private$i.data = data
            private$i.jheem.kernel = jheem.kernel
            
            
            #-- Make Active Bindings with the Names of Outcomes --#
            outcomes.to.bind = setdiff(self$outcomes, names(self))
            lapply(outcomes.to.bind, function(outcome.name){
                
                fn = eval(parse(text=paste0("function(value){private$eval.outcome.active.binding(value, outcome.name='",outcome.name,"')}")))
                environment(fn) = self
                
                makeActiveBinding(sym = outcome.name,
                                  fun = fn,
                                  env = self)
            })
            
            lockEnvironment(self)
        },
        
        check = function()
        {
            browser()
        },
        
        # here for now as a hack to let us force updates
        update = function()
        {
            if (!is.null(private$i.metadata$outcome.location.mapping))
                private$i.metadata$outcome.location.mapping = update.outcome.location.mapping(mapping = private$i.metadata$outcome.location.mapping,
                                                                                              jheem.kernel = private$i.jheem.kernel)
        },
        
        print = function(...)
        {
            base::print(paste0(ifelse(private$i.metadata$n.sim==1, 
                                      'A single JHEEM simulation', 
                                      paste0("A set of ", private$i.metadata$n.sim, " JHEEM simulations")),
                               ", from ", self$from.year, " to ", self$to.year, 
                               ", for model version '", private$i.version, "' and location '", private$i.location, "'",
                               ifelse(is.null(self$intervention.code), '',
                                      paste0(" with intervention '", self$intervention.code, "'"))))
        },
        
        optimized.get = function(optimized.get.instructions,
                                 error.prefix = "Error in executing optimized.get(): ")
        {
            if (!is.character(error.prefix) || length(error.prefix)!=1 || is.na(error.prefix))
                stop(paste0("Cannot execute optimized.get() - 'error.prefix' must be a single, non-NA character vector"))
            
            if (!is(optimized.get.instructions, "jheem.simulation.optimized.get.instructions"))
                stop(paste0(error.prefix, "'optimized.get.instructions' must be an object of class 'jheem.simulation.optimized.get.instructions', created by prepare.optimized.get.instructions()"))
            
            optimized.get.instructions$do.get(outcome.numerators = private$i.data$outcome.numerators,
                                              outcome.denominators = private$i.data$outcome.denominators,
                                              n.sim = private$i.metadata$n.sim,
                                              sim.from.year = private$i.metadata$from.year,
                                              sim.to.year = private$i.metadata$to.year,
                                              error.prefix = error.prefix)
        },
        
        # NB: these arguments all need to be duplication in JHEEM_simset_collection.R's get() method
        get = function(outcomes,
                       output = c('value', 'numerator', 'denominator')[[1]],
                       keep.dimensions=NULL,
                       dimension.values = list(),
                       ...,
                       check.consistency = T,
                       drop.single.outcome.dimension = T,
                       drop.single.sim.dimension = F, # BE CAREFUL WITH THE NEW SUMMARY CODE!
                       replace.inf.values.with.zero = T,
                       summary.type = c('individual.simulation', 'mean.and.interval', 'median.and.interval')[1],
                       interval.coverage = 0.95,
                       mapping = NULL, # to do: put in the other get() method mentioned above (simset collection)
                       na.rm = T,
                       error.prefix = "Error getting simulation results: ",
                       debug=F)
        {
            if (debug) browser()
            if (check.consistency && (!is.character(output) || length(output) != 1 || !(output %in% c('value', 'numerator', 'denominator'))))
                stop(paste0(error.prefix, "'output' must be one of 'value', 'numerator', or 'denominator'"))
            
            if (check.consistency && !is.null(mapping) && (!R6::is.R6(mapping) || !is(mapping, 'ontology.mapping')))
                stop(paste0(error.prefix, "'mapping' must be null or an ontology mapping"))
            
            if (length(outcomes) > 1 && !is.null(mapping))
                stop(paste0(error.prefix, "'mapping' must be null if more than one outcome is used"))
            
            if (!(identical(summary.type, 'individual.simulation') || identical(summary.type, 'mean.and.interval') || identical(summary.type, 'median.and.interval')))
                stop(paste0(error.prefix, "'summary.type' must be one of 'individual.simulation', 'mean.and.interval', or 'median.and.interval'"))
            
            # Outcomes must have all keep.dimensions requested
            invalid_keep_dimensions <- keep.dimensions[!sapply(keep.dimensions, function(d) {
                any(sapply(outcomes, function(outcome) {
                    d %in% names(self$outcome.ontologies[[outcome]])}
                    ))}
                )]
            
            if (length(invalid_keep_dimensions) > 0)
                stop(paste0(error.prefix, "the following 'keep.dimensions' are not valid for one or more outcomes: ", paste(invalid_keep_dimensions, collapse=", ")))
            
            # keep.dimensions will be the union of the incomplete dimensions in the outcome ontology and any dimension value dimensions
            if (is.null(keep.dimensions)) {
                incomplete.dimensions = unique(unlist(lapply(outcomes, function(outcome) {incomplete.dimensions(self$outcome.ontologies[[outcome]])}))) # unshared incompletes will catch error below
                keep.dimensions = union(incomplete.dimensions, names(dimension.values))
            }
            
            dimension.values = private$process.dimension.values(dimension.values, ..., check.consistency = check.consistency, error.prefix=error.prefix)
            # if (drop.single.sim.dimension && self$n.sim==1)
            #     keep.dimensions = names(dim.names)
            # else
            #     keep.dimensions = names(dim.names)[-length(dim.names)]
            # 
            # if (!drop.single.sim.dimension || self$n.sim > 1)
            #     keep.dimensions = union(keep.dimensions, 'sim')
            
            # We need to check if we have lost all our years when the dimension values were processed
            # (for example, if the years were all outside our range)
            if (any(sapply(dimension.values, length)==0))
                return(NULL)
            
            rv = lapply(outcomes, function(outcome){
                scale = self$outcome.metadata[[outcome]]$scale
                numerator.needed = output %in% c('value', 'numerator')
                denominator.needed = scale.needs.denominator(scale) && output %in% c('value', 'denominator')
                
                numerator.data = NULL
                denominator.data = NULL
                
                # noting that some outcomes, like aids.diagnoses, do not have all years
                years.this.outcome = NULL
                unused.years.this.outcome = NULL
                dimension.values.this.outcome = dimension.values
                
                if (numerator.needed)
                    numerator.data = private$i.data$outcome.numerators[[outcome]]
                if (denominator.needed) {
                    denominator.data = private$i.data$outcome.denominators[[outcome]]
                    if (check.consistency && is.null(denominator.data))
                        stop(paste0(error.prefix, "outcome '", outcome, "' missing denominator data"))
                }
                if (check.consistency && output == 'denominator' && !scale.needs.denominator(scale))
                    stop(paste0(error.prefix, "outcome '", outcome, "' does not use a denominator"))
                
                # check that this outcome has the years we want before subset
                if (numerator.needed) years.this.outcome = dimnames(numerator.data)$year
                else if (denominator.needed) years.this.outcome = dimnames(denominator.data)$year
                else years.this.outcome = intersect(dimnames(numerator.data)$year, dimnames(denominator.data)$year)
                if ('year' %in% names(dimension.values.this.outcome)) {
                    unused.years.this.outcome = setdiff(dimension.values[['year']], years.this.outcome)
                    dimension.values.this.outcome[['year']] = intersect(dimension.values.this.outcome[['year']], years.this.outcome)
                }
                
                if (numerator.needed) numerator.data = array.access(numerator.data, dimension.values.this.outcome)
                if (denominator.needed) denominator.data = array.access(denominator.data, dimension.values.this.outcome)
                
                if (!(numerator.needed && !(length(numerator.data)>0)) && !(denominator.needed && !(length(denominator.data)>0))) {
                    # Apply mapping
                    if (numerator.needed && !is.null(mapping)) numerator.data = mapping$apply(numerator.data, na.rm=na.rm)
                    if (denominator.needed && !is.null(mapping)) denominator.data = mapping$apply(denominator.data, na.rm=na.rm)
                    
                    # Aggregation
                    if (numerator.needed) pre.agg.dimnames = dimnames(numerator.data)
                    else pre.agg.dimnames = dimnames(denominator.data)
                    
                    dimensions.to.drop = intersect(which(length(pre.agg.dimnames) == 1), which(!(names(pre.agg.dimnames) %in% keep.dimensions)))
                    
                    if (length(dimensions.to.drop) > 0) {
                        pre.agg.dimnames = pre.agg.dimnames[-dimensions.to.drop]
                        if (numerator.needed) numerator.data = array(numerator.data, dim = sapply(pre.agg.dimnames, length), dimnames = pre.agg.dimnames)
                        if (denominator.needed) denominator.data = array(denominator.data, dim = sapply(pre.agg.dimnames, length), dimnames = pre.agg.dimnames)
                    }
                    
                    if (length(pre.agg.dimnames) > length(keep.dimensions)) {
                        if (numerator.needed) numerator.data = apply.robust(numerator.data, c(keep.dimensions, 'sim'), sum, na.rm=na.rm)
                        if (denominator.needed) denominator.data = apply.robust(denominator.data, c(keep.dimensions, 'sim'), sum, na.rm=na.rm)
                    }
                    
                    if (output == 'numerator' || output == 'value' && !denominator.needed) output.array = numerator.data
                    else if (output == 'denominator') output.array = denominator.data
                    else {
                        output.array = numerator.data / denominator.data
                        if (replace.inf.values.with.zero && denominator.needed && output == 'value')
                            output.array[denominator.data == 0] = 0
                    }
                }
                
                else {
                    output.dimnames = dimnames(numerator.data)[names(dimnames(numerator.data)) %in% c(keep.dimensions, 'sim')]
                    output.array = array(NA, sapply(output.dimnames, length), output.dimnames)
                }
                
                # add NAs for unused years so that this outcome's array can be mixed with the other outcomes' arrays
                # don't do this if there's a mapping, since we can only have one outcome anyways, if there's a mapping
                # but we only need this if year is a keep dimension, right? Is year ever allowed to not be a keep dimension? I guess it is, since if someone deliberately forced keep.dimensions to not have it...
                if (length(unused.years.this.outcome)>0 && is.null(mapping) && 'year' %in% keep.dimensions) { # 
                    dimnames.with.all.years = dimnames(output.array)
                    dimnames.with.all.years$year = dimension.values$year
                    output.array.with.all.years = array(NA, sapply(dimnames.with.all.years, length), dimnames.with.all.years)
                    output.array.with.all.years[get.array.access.indices(dimnames.with.all.years, dimnames(output.array))] = output.array
                    output.array=output.array.with.all.years
                }
                
                output.array
            })
            
            individual.outcome.dimnames = dimnames(rv[[1]]) # which might be all we have
            rv = unlist(rv, recursive = FALSE)
            if (drop.single.outcome.dimension && length(outcomes)==1)
            {
                dim(rv) = sapply(individual.outcome.dimnames, length)
                dimnames(rv) = individual.outcome.dimnames
            } else {
                dimnames.with.outcome = c(individual.outcome.dimnames, list(outcome=outcomes))
                dim(rv) = sapply(dimnames.with.outcome, length)
                dimnames(rv) = dimnames.with.outcome
            }
            
            if (drop.single.sim.dimension && dim(rv)[['sim']]==1) {
                dimnames.without.sim = dimnames(rv)[names(dimnames(rv)) != 'sim']
                dim(rv) = sapply(dimnames.without.sim, length)
                dimnames(rv) = dimnames.without.sim
            }
            
            # browser()
            if (summary.type == 'mean.and.interval') {
                alpha = (1-interval.coverage)/2
                
                # Apply doesn't work if sim is the only dimension
                if (!identical(names(dim(rv)), 'sim')) {
                    rv = apply(rv, setdiff(names(dim(rv)), 'sim'), function(x) {
                        c(mean(x, na.rm=T), quantile(x, probs=c(alpha, 1-alpha), na.rm=T))
                    })
                    new.dim.names = c(list(metric = c('mean', 'lower', 'upper')), dimnames(rv)[-1])
                    dimnames(rv) = new.dim.names
                }
                
                else {
                    new.dim.names = c(list(metric = c('mean', 'lower', 'upper')))
                    rv = array(c(mean(rv, na.rm=T), quantile(rv, probs=c(alpha, 1-alpha), na.rm=T)),
                               sapply(new.dim.names, length), new.dim.names)
                }
                
            }
            if (summary.type == 'median.and.interval') {
                alpha = (1-interval.coverage)/2
                # Apply doesn't work if sim is the only dimension
                if (!identical(names(dim(rv)), 'sim')) {
                    rv = apply(rv, setdiff(names(dim(rv)), 'sim'), function(x) {
                        c(median(x, na.rm=T), quantile(x, probs=c(alpha, 1-alpha), na.rm=T))
                    })
                    new.dim.names = c(list(metric = c('median', 'lower', 'upper')), dimnames(rv)[-1])
                    dimnames(rv) = new.dim.names
                }
                
                else {
                    new.dim.names = c(list(metric = c('median', 'lower', 'upper')))
                    rv = array(c(median(x, na.rm=T), quantile(x, probs=c(alpha, 1-alpha), na.rm=T)),
                               sapply(new.dim.names, length), new.dim.names)
                }
            }
            rv
        },
        
        fix.chains = function()
        {
            calibrated.param.names = get.parameter.names.for.version(self$version, type = 'calibrated')
            
            eq.prior = c(0, apply(self$parameters[calibrated.param.names,-1,drop=F] == self$parameters[calibrated.param.names,-self$n.sim,drop=F], 2, mean))
            
            chain.counter = 0
            private$i.data$simulation.chain = sapply(eq.prior, function(val){
                if (val==0)
                    chain.counter <<- chain.counter + 1
                
                chain.counter
            })
            
            private$i.data$unique.chains = 1:chain.counter
        },
        
        subset = function(simulation.indices)
        {
            error.prefix = "Error subsetting jheem.simulation.set: "
            # 'simulation.indices' must be either an integer or logical vector with valid length and values
            if ((!is.numeric(simulation.indices) && !is.logical(simulation.indices)) || any(is.na(simulation.indices)) )
                stop(paste0(error.prefix, "'simulation.indices' must be a numeric or logical vector with no NAs or repeats"))
            if (is.numeric(simulation.indices) && (any(simulation.indices < 1) || any(simulation.indices > self$n.sim)))
                stop(paste0(error.prefix, "if 'simulation.indices' is a numeric vector, all values must be integers between 1 and this simulation.set's 'n.sim'"))
            if (is.logical(simulation.indices) && length(simulation.indices) != self$n.sim)
                stop(paste0(error.prefix, "if 'simulation.indices' is a logical vector, it must have length equal to this simulation.set's 'n.sim'"))
            
            if (is.logical(simulation.indices)) simulation.indices = (1:self$n.sim)[simulation.indices]
            
            new.n.sim = length(simulation.indices)
            new.outcome.numerators = lapply(private$i.data$outcome.numerators, function(outcome.arr) {
                if (is.null(outcome.arr) || length(outcome.arr) == 0) return(NULL)
                new.arr = array.access(outcome.arr, sim=simulation.indices, drop=F)
                dimnames(new.arr)[['sim']] = 1:new.n.sim
                new.arr
            })
            new.outcome.denominators = lapply(private$i.data$outcome.denominators, function(outcome.arr) {
                if (is.null(outcome.arr) || length(outcome.arr) == 0) return(NULL)
                new.arr = array.access(outcome.arr, sim=simulation.indices, drop=F)
                dimnames(new.arr)[['sim']] = 1:new.n.sim
                new.arr
            })
            new.parameters = private$i.data$parameters[,simulation.indices,drop=F]
            
            new.simulation.chain = private$i.data$simulation.chain[simulation.indices]
            
            do.create.simulation.set(jheem.kernel = self$jheem.kernel,
                                     sub.version = self$sub.version,
                                     outcome.numerators = new.outcome.numerators,
                                     outcome.denominators = new.outcome.denominators,
                                     parameters = new.parameters,
                                     simulation.chain = new.simulation.chain,
                                     from.year = self$from.year,
                                     to.year = self$to.year,
                                     n.sim = new.n.sim,
                                     outcome.location.mapping = self$outcome.location.mapping,
                                     calibration.code = self$calibration.code,
                                     intervention.code = self$intervention.code,
                                     run.metadata = self$run.metadata$subset(simulation.indices),
                                     solver.metadata = self$solver.metadata,
                                     is.degenerate = self$is.degenerate[simulation.indices],
                                     finalize = self$is.finalized)
        },
        
        # n: keeps every nth (rounding DOWN) sim counting backwards from the last sim
        # keep: may be fraction or a number to keep. Decimal values above 1 are rounded down to nearest whole number.
        thin = function(n=NULL, keep=NULL,allow.expand=F)
        {
            error.prefix = "Error thinning simulation.set: "
            if (is.null(n) && is.null(keep))
                stop(paste0(error.prefix, "either 'n' or 'keep' must be specified"))
            if (!is.null(n) && !is.null(keep))
                stop(paste0(error.prefix, "exactly one of 'n' or 'keep' must be specified"))
            if (!is.null(n) && (!is.numeric(n) || length(n) != 1 || n > self$n.sim || n < 1))
                stop(paste0(error.prefix, "'n' must be a single integer value between 1 and 'n.sim'"))
            if (!is.null(keep) && (!is.numeric(keep) || length(keep) != 1 || keep <= 0))
                stop(paste0(error.prefix, "'keep' must be either a single integer value >= 1 or a fraction between 0 and 1"))
            
            if (is.null(keep)) keep = ceiling(self$n.sim / n)
            if (keep < 1) keep = ceiling(self$n.sim * keep)
            if (is.null(n)) n = self$n.sim / keep
            
            if (keep == self$n.sim)
                self
            else
            {
                keep.per.chain = rep(floor(keep / self$n.chains), self$n.chains)
                for (i in self$n.chains:1)
                {
                    if (sum(keep.per.chain)==keep)
                        break;
                    
                    keep.per.chain[i] = keep.per.chain[i] + 1
                }
                
                if (keep > self$n.sim)
                {
                    if (!allow.expand)
                        stop(paste0(error.prefix, "You have requested to keep ", keep,
                                    " simulations, but the simset only contains ", self$n.sim,
                                    " simulations. Use allow.expand=T if you want to allow thinning to duplicate simulations"))
                    
                    keep.indices.per.chain = lapply(1:self$n.chains, function(chain.index){
                        chain = self$unique.chains[chain.index]
                        indices.for.chain = (1:n.sim)[self$simulation.chain==chain]
                        
                        if (length(indices.for.chain)==keep.per.chain[chain.index])
                            indices.for.chain
                        else
                        {
                            counts.per.sim = rep(0, self$n.sim)
                            counts.per.sim[indices.for.chain] = rep(floor(keep.per.chain[chain.index] / length(indices.for.chain)), length(indices.for.chain))
                            remaining.to.sample = keep.per.chain[chain.index] - sum(counts.per.sim)
                            if (remaining.to.sample > 0)
                            {
                                n.remaining.for.chain = length(indices.for.chain) / remaining.to.sample
                                duplicate.indices = indices.for.chain[ceiling(length(indices.for.chain) - n.remaining.for.chain * ((remaining.to.sample - 1) : 0))]
                                counts.per.sim[duplicate.indices] = counts.per.sim[duplicate.indices] + 1
                            }
                            
                            unlist(lapply(indices.for.chain, function(i){
                                rep(i, counts.per.sim[i])
                            }))
                        }
                    })
                    
                    self$subset(unlist(keep.indices.per.chain))
                }
                else
                {
                    self$subset(ceiling(self$n.sim - n * ((keep - 1) : 0)))
                }
                
            }
        },
        
        # keep: may be fraction or a number to keep (to stay consistent with simset$thin arguments)
        burn = function(keep=NULL)
        {
            error.prefix = "Error burning sims from simulation.set: "
            if (!is.numeric(keep) || length(keep) != 1 || keep > self$n.sim || keep <= 0)
                stop(paste0(error.prefix, "'keep' must be either a single integer value between 1 and 'n.sim' or a fraction between 0 and 1"))
            
            keep.per.chain = sapply(self$unique.chains, function(chain){
                n.for.chain = sum(self$simulation.chain == chain)
                
                if (keep < 1)
                    floor(n.for.chain * keep)
                else
                    floor(keep / self$n.chains)
            })
            
            if (keep < 1)
                n.keep = ceiling(keep * self$n.sim)
            else
                n.keep = keep
            
            for (i in self$n.chains:1)
            {
                if (sum(keep.per.chain)==n.keep)
                    break;
                
                keep.per.chain[i] = keep.per.chain[i] + 1
            }
            
            # if (keep < 1) keep = ceiling(self$n.sim * keep)
            
            # self$subset((self$n.sim - keep + 1) : self$n.sim) # Keep the LAST part, not the FIRST
            
            keep.indices.per.chain = sapply(1:self$n.chains, function(chain.index){
                
                indices.for.chain = (1:self$n.sim)[ self$simulation.chain == self$unique.chains[chain.index] ]
                indices.for.chain[ (length(indices.for.chain) - keep.per.chain[chain.index] + 1):length(indices.for.chain)]
            })
            
            self$subset(unlist(keep.indices.per.chain))
        },
        
        first.sim = function()
        {
            self$subset(1)
        },
        
        last.sim = function()
        {
            self$subset(self$n.sim)
        },
        
        get.params = function(match.names=NULL,
                              simulation.indices=self$n.sim,
                              drop=T)
        {
            param.names = private$match.parameter.names(match.names)
            
            if (!is.numeric(simulation.indices))
                stop("'simulation.indices' must be a numeric vector")
            
            self$parameters[param.names, simulation.indices, drop=drop]
        },
        
        get.mcmc.mixing.statistic = function(match.names=NULL,
                                             chains = self$unique.chains, # which chains to use
                                             sort = T,
                                             as.matrix = T)
        {
            error.prefix="Cannot get.mcmc.mixing.statistic: "
            param.names = private$match.parameter.names(match.names)
            if (self$n.chains == 1)
            {
                # variance in first 1/4 of params divided by variance in last 1/4 of params
                parameter.values = self$parameters[param.names,,drop=F]
                values.first.quarter = parameter.values[, 1:(floor(self$n.sim * 0.25) + 1), drop=F]
                values.last.quarter = parameter.values[, (self$n.sim - ceiling(self$n.sim * 0.25) + 1):self$n.sim, drop=F]
                mixing.statistic = apply(values.first.quarter, 1, function(values) {var(values)}) / apply(values.last.quarter, 1, function(values) {var(values)})
            }
            else if (self$n.chains > 1)
            {
                n = self$n.sim
                m = self$n.chains
                
                #-- Calculate within-chain variance --#
                within.chain.variance.per.chain = sapply(self$unique.chains, function(chain){
                    chain.mask = self$simulation.chain == chain
                    parameter.values = self$parameters[param.names,chain.mask,drop=F]
                    
                    param.means = rowMeans(parameter.values)
                    1/(n-1) * rowSums( (parameter.values-param.means)^2 )
                })
                W = within.chain.variance = rowMeans(within.chain.variance.per.chain)
                
                #-- Calculate between-chain variance --#
                
                chain.means = sapply(self$unique.chains, function(chain){
                    chain.mask = self$simulation.chain == chain
                    parameter.values = self$parameters[param.names,chain.mask,drop=F]
                    
                    rowMeans(parameter.values)
                })
                overall.means = rowMeans(chain.means)
                
                B = n / (m-1) * sum( (chain.means - overall.means)^2 )
                
                #-- Put it together --#
                
                var.hat = (n-1)/n * W + 1/n * B
                rhat = sqrt(var.hat / W)
                
                mixing.statistic = rhat
            }
            else
                stop(paste0(error.prefix, "We cannot calculate MCMC statistics for a simulation set that was not generated from an MCMC process"))
            
            if (sort)
                rv = sort(mixing.statistic, decreasing=T)
            else
                rv = mixing.statistic
            
            if (as.matrix)
                cbind(rv)
            else
                rv
        },
        
        traceplot = function(match.names,
                             chains = self$unique.chains,
                             burn = 0)
        {
            if (self$n.chains == 0)
                stop("We cannot make a traceplot a simulation set that was not generated from an MCMC process")
            
            param.names = private$match.parameter.names(match.names)
            
            chain.counts = rep(0, self$n.chains)
            iter = sapply(1:n.sim, function(i){
                chain = self$simulation.chain[i]
                chain.counts[chain] <<- chain.counts[chain] + 1
                chain.counts[chain]
            })
            
            df = reshape2::melt(t(private$i.data$parameters[param.names,,drop=F]))
            df$Iteration = rep(iter, length(param.names))
            df$Chain = rep(private$i.data$simulation.chain, length(param.names))

            if (burn > 0)
            {
                df = df[df$Iteration <= burn]
                if (dim(df)[1]==0)
                    stop(paste0("After burning ", burn, " simulations, there are no simulations left from which to plot parameters"))
            }
            
            if (!setequal(chains, self$unique.chains))
            {
                df = df[sapply(df$Chain, function(chain){any(chain==chains)}),]
                if (dim(df)[1]==0)
                    stop(paste0("After limiting to chains ", collapse.with.and(chains), ", there are no simulations left from which to plot parameters"))
            }
            
            df$Chain = factor(df$Chain, levels = chains)
            
            ggplot2::ggplot(df, ggplot2::aes(x=Iteration, y=value, color=Chain)) + 
                ggplot2::geom_line() +
                ggplot2::facet_wrap(~parameter, scales = 'free_y') +
                ggplot2::ylab("Parameter Value")
        },
        
        get.engine = function(start.year = NULL,
                              end.year = NULL,
                              max.run.time.seconds = NULL,
                              keep.from.year = NULL,
                              keep.to.year = NULL,
                              intervention.code = self$intervention.code,
                              error.prefix = "Cannot get JHEEM Engine from simulation set")
        {
            if (!is.character(error.prefix) || length(error.prefix)!=1 || is.na(error.prefix))
                stop(paste0("Error in simulation.set$get.engine() - 'error.prefix' must be a single, non-NA character vector"))
            
            # substitute defaults for years if given NULL
            if (is.null(start.year))
                start.year = self$from.year
            
            if (is.null(end.year))
                end.year = self$to.year
            
            if (is.null(keep.from.year))
                keep.from.year = start.year
            
            if (is.null(keep.to.year))
                keep.to.year = end.year
            
            if (!identical(intervention.code, self$intervention.code))
            {
                # Check the new intervention code
                new.intervention = get.intervention.from.code(intervention.code, throw.error.if.missing=F)
                if (is.null(new.intervention))
                {
                    stop(paste0("Cannot set intervention.code '", intervention.code, 
                                "' in getting a new engine from the simulation: no intervention with that code has been registered.",
                                ifelse(is.intervention.code.temporary(intervention.code),
                                       paste0("'", intervention.code, "' is a temporary code - it was probably created as a one-off intervention that was not formally saved."),
                                       '')))
                }
                
                # Check against the prior intervention
                if (!is.null(self$intervention.code))
                {
                    prior.intervention = get.intervention.from.code(self$intervention.code, throw.error.if.missing = F)
                    if (is.null(prior.intervention))
                    {
                        stop(paste0("Cannot get the engine for the simulation set. The simulation ran with intervention.code '", self$intervention.code, 
                                    "' , but no intervention with that code has been registered.",
                                    ifelse(is.intervention.code.temporary(self$intervention.code),
                                           paste0("'", self$intervention.code, "' is a temporary code - it was probably created as a one-off intervention that was not formally saved."),
                                           '')))
                    }
                    
                    if (is.null(intervention.code))
                        intervention.code = self$intervention.code
                    else if (!is.no.intervention(prior.intervention) && !prior.intervention$equals(new.intervention))
                        stop(paste0(error.prefix, "Cannot change the intervention.code to '", intervention.code, 
                                    "' when getting an engine for the simulation - a different intervention ('",
                                    self$intervention.code, "') was already used to run the simulation"))
                }
            }
            
            if (is.null(intervention.code))
                prior.simulation.set = NULL
            else
                prior.simulation.set = self
            
            engine = do.create.jheem.engine(jheem.kernel = private$i.jheem.kernel,
                                            sub.version = private$i.sub.version,
                                            start.year = start.year,
                                            end.year = end.year,
                                            max.run.time.seconds = max.run.time.seconds,
                                            prior.simulation.set = prior.simulation.set,
                                            keep.from.year = keep.from.year,
                                            keep.to.year = keep.to.year,
                                            intervention.code = intervention.code,
                                            calibration.code = self$calibration.code,
                                            solver.metadata = self$solver.metadata,
                                            finalize = T,
                                            error.prefix = error.prefix)
        },
        
        #'@return Returns a NEW simulation set object, run out to end.year
        extend = function(end.year,
                          keep.from.year = self$from.year,
                          keep.to.year = end.year,
                          max.run.time.seconds = NULL)
        {
            if (!is.numeric(end.year) || length(end.year) != 1 || is.na(end.year))
                stop("Cannot extend simulation set: 'end.year' must be a single, non-NA numeric value")
            
            engine = self$get.engine(start.year = self$to.year + 1,
                                    end.year = end.year,
                                    max.run.time.seconds = max.run.time.seconds,
                                    keep.from.year = keep.from.year,
                                    keep.to.year = keep.to.year,
                                    intervention.code = self$intervention.code,
                                    error.prefix = "Cannot get JHEEM Engine from simulation set")
            
            sim.list = lapply(1:private$i.metadata$n.sim, function(i){
                engine$run(parameters = private$i.parameters[,i],
                           prior.sim.index = i)
            })
            
            do.join.simulation.sets(sim.list, finalize=T)
        },
        
        get.intervention = function()
        {
            if (!is.null(self$intervention.code))
            {
                rv = get.intervention.from.code(selfintervention.code, throw.error.if.missing=F)
                if (is.null(rv))
                    stop(paste0("The simulation set has a registered intervention code of '",
                                self$intervention.code, "' but no intervention has been registered for that code in this R session",
                                ifelse(is.intervention.code.temporary(selfintervention.code),
                                       paste0("('", selfintervention.code, "' was a temporary code created for a one-off intervention that was not formally registered with a code)"),
                                       "")))
                rv
            }
            else
                NULL
        },
        
        save = function(root.dir = get.jheem.root.directory("Cannot save simulation set: "))
        {
            save.simulation.set(simset = self, root.dir = root.dir)
        }
        
    ),
    
    active = list(
        
        descriptor = function(value)
        {
            if (missing(value))
                'simulation'
            else
                stop("Cannot modify a simulation's 'descriptor' - it is read-only")
        },
        
        parameters = function(value)
        {
            if (missing(value))
                private$i.data$parameters
            else
                stop("Cannot modify a simulation's 'parameters' - they are read-only")
        },
        
        parameter.names = function(value)
        {
            if (missing(value))
                dimnames(private$i.data$parameters)[[1]]
            else
                stop("Cannot modify a simulation's 'parameter.names' - they are read-only")
        },
        
        params = function(value)
        {
            if (missing(value))
            {
                if (private$i.metadata$n.sim==1)
                    private$i.data$parameters[,1]
                else
                    stop("The 'params' field is only available for single-simulation sets (ie, when nsim==1)")
            }
            else
                stop("Cannot modify a simulation's 'params' - they are read-only")
        },
        
        data = function(value)
        {
            if (missing(value))
                private$i.data
            else
                stop("Cannot modify a simulation.set's 'data' - it is read-only")
        },
        
        is.degenerate = function(value)
        {
            if (missing(value))
                private$i.data$is.degenerate
            else
                stop("Cannot modify a simulation.set's 'is.degenerate' - it is read-only")
        },
        
        is.finalized = function(value)
        {
            if (missing(value))
                private$i.data$finalized
            else
                stop("Cannot modify a simulation.set's 'is.finalized' - it is read-only")
        },
        
        run.metadata = function(value)
        {
            if (missing(value))
                private$i.data$run.metadata
            else
                stop("Cannot modify a simulation.set's 'run.metadata' - it is read-only")
        },
        
        seed = function(value) #get's a random seed for this simulation set
        {
            if (missing(value))
                private$i.data$seed
            else
                stop("Cannot modify a simulation.set's 'seed' - it is read-only")
        },
        
        jheem.kernel = function(value)
        {
            if (missing(value))
                private$i.jheem.kernel
            else
                stop("Cannot modify a simulation.set's 'jheem.kernel' - it is read-only")
        },
        
        simulation.chain = function(value)
        {
            if (missing(value))
                private$i.data$simulation.chain
            else
                stop("Cannot modify a simulation.set's 'simulation.chain' - it is read-only")
        },
        
        unique.chains = function(value)
        {
            if (missing(value))
                private$i.data$unique.chains
            else
                stop("Cannot modify a simulation.set's 'unique.chains' - it is read-only")
        },
        
        n.chains = function(value)
        {
            if (missing(value))
                length(private$i.data$unique.chains)
            else
                stop("Cannot modify a simulation.set's 'n.chains' - it is read-only")
        }
    ),
    
    private = list(
        
        i.jheem.kernel = NULL,
        i.data = NULL,
        
        match.parameter.names = function(match.names)
        {
            if (length(match.names)==0)
                self$parameter.names
            else
            {
                if (is.character(match.names))
                {
                    regexes = gsub("\\*", ".*", match.names)
                    match.mask = grepl(regexes[1], self$parameter.names)
                    for (regex in regexes[-1])
                        match.mask = match.mask | grepl(regex, self$parameter.names)
                    
                    if (!any(match.mask))
                        stop("The given 'match.names' do not match any parameter.names")
                    
                    self$parameter.names[match.mask]
                }
                else if (is.numeric(match.names))
                {
                    if (any(match.names<1) || any(match.names>length(self$parameter.names)))
                        stop(paste0("If 'match.names' is a numeric vector its values must be between 1 and ", length(self$parameter.names), " (the number of parameters)"))
                    
                    self$parameter.names[match.names]
                }
                else
                {
                    stop("'match.names' must be either a charater or numeric vector")
                }
            }
        },
        
        eval.outcome.active.binding = function(value, outcome.name)
        {
            if (missing(value))
            {
                if (is.null(private$i.data$outcome.denominators[[outcome.name]]) || is.null(private$i.data$outcome.numerators[[outcome.name]]))
                    private$i.data$outcome.numerators[[outcome.name]]
                else
                {
                    rv = private$i.data$outcome.numerators[[outcome.name]] /
                        private$i.data$outcome.denominators[[outcome.name]]
                    rv[private$i.data$outcome.denominators[[outcome.name]]==0] = 0
                    rv
                }
            }
            else
                stop(paste0("Cannot modify a simulation's '", outcome.name, "' - it is read-only"))
        }
    )
)

# gets a seed that is deterministically derived from a set of parameters,
#  but unique for a given set of parameters
get.simulation.seed.from.parameters <- function(parameters)
{
    if (length(parameters)==0)
        stop("Cannot get.simulation.seed.from.parameters() - parameters is an empty vector")
    
    if (any(is.na(parameters)))
        stop("Cannot get.simulation.seed.from.parameters() - parameters cannot be NA")
    
    # we're going to transform each parameter such that it gives a value
    #  >=10 or <= -10
    # (so that even small numbers count towards the integer seed)
    seed = sum((parameters * 10^pmax(0, 1+ceiling(-log10(abs(parameters)))))[parameters!=0], na.rm=T)
    
    if (seed == Inf)
        seed = .Machine$integer.max
    else if (seed == -Inf)
        seed = -.Machine$integer.max
    else if (seed > .Machine$integer.max)
        seed = seed %% .Machine$integer.max
    else if (seed < -.Machine$integer.max)
        seed = -(-seed %% .Machine$integer.max)
    
    as.integer(seed)
}