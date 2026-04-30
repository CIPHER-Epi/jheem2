
#' @title Set Up Calibration
#'@param version The version of the model specification (must have been previously registered)
#'@param location A single character value representing the location for the calibration
#'@param calibration.code
#'@param root.dir
#'@param sub.version
#'@param cache.frequency
#'@param allow.overwrite.cache
#'@param verbose
#'
#'@export
set.up.calibration <- function(version,
                               location,
                               calibration.code,
                               root.dir = get.jheem.root.directory("Cannot set up calibration: "),
                               sub.version = NULL,
                               cache.frequency = 500,
                               allow.overwrite.cache = F,
                               verbose = T)
{
    #------------------------#
    #-- Validate Arguments --#
    #------------------------#
    
    if (!is.character(version) || length(version)!=1 || is.na(version))
        stop("Cannot set.up.calibration: version must be a single, non-NA character value")
    if (!is.specification.registered.for.version(version))
        stop(paste0("Cannot set.up.calibration: no specification has been registered for version '", version, "'"))
    
    if (!is.character(calibration.code) || length(calibration.code)!=1 || is.na(calibration.code))
        stop("Cannot set.up.calibration: calibration.code must be a single, non-NA character value")
    
    if (!is.character(location) || length(location)!=1 || is.na(location))
        stop("Cannot set.up.calibration: location must be a single, non-NA character value")

    error.prefix = paste0("Cannot Set Up Calibration for code '", calibration.code, "' for version '", version, "' and location '", location, "': ")
    verbose.prefix = paste0("CALIBRATION '", calibration.code, "' (", version, " / ", location, "): ")
    
    if (!locations::is.location.valid(location))
        stop(paste0(error.prefix, "'", location, "' is not a valid location code"))
    
    if (!is.character(root.dir) || length(root.dir)!=1 || is.na(root.dir))
        stop(paste0(error.prefix, "'root.dir' must be a single, non-NA character value"))
    if (!dir.exists(root.dir) && length(list.files(root.dir))==0)
        stop(paste0(error.prefix, "The specified root.dir ('", root.dir, "') does not exist"))
    
    # sub-version
    if (!is.null(sub.version) &&
        (!is.character(sub.version) || length(sub.version)!=1 || is.na(sub.version)))
        stop(paste0(error.prefix, "'sub.version' must be either NULL or a single, non-NA character value"))
    
    # cache frequency
    if (!is.numeric(cache.frequency) || length(cache.frequency)!=1 || is.na(cache.frequency) || round(cache.frequency)!=cache.frequency)
        stop(paste0(error.prefix, "'cache.frequency' must be a single, non-NA integer value"))
    if (cache.frequency <= 0)
        stop(paste0(error.prefix, "'cache.frequency' must be >= 1"))
    
    # allow.overwrite.cache
    if (!is.logical(allow.overwrite.cache) || length(allow.overwrite.cache)!=1 || is.na(allow.overwrite.cache))
        stop(paste0(error.prefix, "'allow.overwrite.cache must be a single, non-NA logical value (either TRUE or FALSE)"))
    
    # verbose
    if (!is.logical(verbose) || length(verbose)!=1 || is.na(verbose))
        stop(paste0(error.prefix, "'verbose must be a single, non-NA logical value (either TRUE or FALSE)"))
    
    #--------------------------------------#
    #-- Pull the calibration info object --#
    #--------------------------------------#
    
    calibration.info = get.calibration.info(calibration.code,
                                            throw.error.if.missing = T,
                                            error.prefix = error.prefix)
    
    spec = get.compiled.specification.for.version(version)
    if (calibration.info$draw.from.parent.version)
    {
        if (is.null(spec$parent.version))
            stop(paste0(error.prefix, "We were instructed to draw.from.parent.version in setting up the calibration, but there is no parent version for the '", version, "' specification"))   
        if (!is.specification.registered.for.version(spec$parent.version))
            stop(paste0(error.prefix, "We were instructed to draw.from.parent.version in setting up the calibration, but no specification has been registered for the parent version '", spec$parent.version, "'"))   
    }
    
    if (calibration.info$thin > cache.frequency)
        print(paste0("WARNING: You have set cache.frequency=", cache.frequency,
                     ", but the 'thin' for the registered calibration is ", calibration.info$thin,
                     ". This means you will be saving empty files to the disk (with no simulations in them)"))
    
    #-------------------------#
    #-- Pull Down the Prior --#
    #-------------------------#
    
    if (verbose)
        print(paste0(verbose.prefix, "Pulling prior, setting up defaults..."))
    
    # Pull the prior for the version
    prior = get.parameters.distribution.for.version(version, type='calibrated')
    
    missing.parameter.names = setdiff(calibration.info$parameter.names, prior@var.names)
    if (length(missing.parameter.names)>0)
        stop(paste0(error.prefix,
                    length(missing.parameter.names), " ",
                    ifelse(length(missing.parameter.names)==1, "parameter is", "parameters are"),
                    " included in the calibration's 'parameter.names' but ", 
                    ifelse(length(missing.parameter.names)==1, "is", "are"),
                    " not present in the '", version,
                    "' prior: ",
                    collapse.with.and("'", missing.parameter.names, "'")))
    
    #------------------------------------#
    #-- Sift Through Parameter Aliases --#
    #------------------------------------#
    
    parsed.aliases = parse.calibration.parameter.aliases(calibration.info = calibration.info, prior = prior)

    model.parameters.in.aliases = parsed.aliases$model.parameters.in.aliases
    parameter.aliases = parsed.aliases$parameter.aliases
    non.aliased.model.parameter.names = parsed.aliases$non.aliased.model.parameter.names
    sorted.parameter.aliases = parsed.aliases$sorted.parameter.aliases
    
    mcmc.parameter.names = parsed.aliases$mcmc.parameter.names
    model.parameter.names = parsed.aliases$model.parameter.names

    missing.parameter.names = setdiff(model.parameters.in.aliases, prior@var.names)
    if (length(missing.parameter.names)>0)
        stop(paste0(error.prefix,
                    length(missing.parameter.names), " ",
                    ifelse(length(missing.parameter.names)==1, "parameter appears", "parameters appear"),
                    " in the calibration's 'parameter.aliases' but ",
                    ifelse(length(missing.parameter.names)==1, "is", "are"),
                    " not present in the '", version,
                    "' prior: ",
                    collapse.with.and("'", missing.parameter.names, "'")))
    
    missing.fixed.parameter.names = setdiff(names(calibration.info$fixed.initial.parameter.values), 
                                            model.parameter.names)
    if (length(missing.fixed.parameter.names)>0)
        stop(paste0(error.prefix,
                    length(missing.fixed.parameter.names), " ",
                    ifelse(length(missing.fixed.parameter.names)==1, "parameter appears", "parameters appear"),
                    " in the calibration's 'fixed.initial.parameter.values' but ",
                    ifelse(length(missing.fixed.parameter.names)==1, "is", "are"),
                    " not present in among any of the listed 'parameter.names' nor among the values of 'parameter.aliases': ",
                    collapse.with.and("'", missing.fixed.parameter.names, "'")))
    
    #----------------------#
    #-- Subset the Prior --#
    #----------------------#
    
    # Subset the relevant parameters
    relevant.prior.for.sampled.model.parameters = distributions::subset.distribution(prior, model.parameter.names) 
    
    # Pull Basic Info We Are Going to Use from the Prior
    all.parameter.scales = get.distribution.variable.transformation(prior, unknown.value='')
    names(all.parameter.scales) = prior@var.names
    
    all.prior.sds = suppressWarnings(get.sds(prior))
    all.prior.medians = suppressWarnings(get.medians(prior))
    all.prior.means = suppressWarnings(get.means(prior))
    
    
    #---------------------------------#
    #-- Prepare the Sampling Blocks --#
    #---------------------------------#
    
    # Pull down the sampling blocks
    sampling.blocks = get.parameter.sampling.blocks.for.version(version)
    
    # Include only the relevant parameters
    sampling.blocks = lapply(sampling.blocks, function(block){
        intersect(block, calibration.info$parameter.names)
    })
    
    sampling.blocks[names(calibration.info$parameter.aliases)] = 
        as.list(names(calibration.info$parameter.aliases))

    sampling.blocks = sampling.blocks[sapply(sampling.blocks, length)>0]
    
    unnamed.block.mask = is.na(names(sampling.blocks)) | names(sampling.blocks)==''
    names(sampling.blocks)[unnamed.block.mask] = paste0('block', 1:sum(unnamed.block.mask))
    
    
    #-------------------------------------------------------------#
    #-- Prepare Default Initial Values, Step Sizes, and Cov Mat --#
    #-------------------------------------------------------------#

    
    #-- Default parameter values --#
    
    all.default.model.parameter.values = all.prior.medians
    all.default.model.parameter.values[names(calibration.info$fixed.initial.parameter.values)] = 
        calibration.info$fixed.initial.parameter.values
    
    default.model.parameter.values = all.default.model.parameter.values[model.parameter.names]
    default.alias.parameter.values = rep(1, length(parameter.aliases))
    names(default.alias.parameter.values) = names(parameter.aliases)
    default.mcmc.parameter.values = c(all.default.model.parameter.values[non.aliased.model.parameter.names],
                                      default.alias.parameter.values)[mcmc.parameter.names]
    
    #-- MCMC Parameter Scales --#
    mcmc.parameter.scales = all.parameter.scales[non.aliased.model.parameter.names]
    mcmc.parameter.scales[names(parameter.aliases)] = 'log'

    #-- Set up initial.cov.mat --#
    
    DEFAULT.SD.REDUCTION.FACTOR = 20 # The 20 here is arbitrary, but seems to work well
    
    all.raw.sds = all.prior.sds
    improper.param.names = prior@var.names[prior@is.improper]
    all.raw.sds[improper.param.names] = default.model.parameter.values[improper.param.names]
    
    all.raw.means = all.prior.means
    all.raw.means[improper.param.names] = default.model.parameter.values[improper.param.names]

    all.default.parameter.sds = all.raw.sds
    all.default.parameter.sds[all.parameter.scales=='log'] = sqrt(log(all.raw.sds[all.parameter.scales=='log']^2 / all.raw.means[all.parameter.scales=='log']^2 + 1)) # This comes from the relationship between mean and SD in a lognormal
    all.default.parameter.sds = all.default.parameter.sds / DEFAULT.SD.REDUCTION.FACTOR
    
    mcmc.parameter.sds = all.default.parameter.sds[non.aliased.model.parameter.names]
    
    if (length(parameter.aliases)>0)
        mcmc.parameter.sds = c(mcmc.parameter.sds,
                               sapply(parameter.aliases, function(aliased.param.names){
                                   mean(all.default.parameter.sds[aliased.param.names])
                               }))
    mcmc.parameter.sds = mcmc.parameter.sds[mcmc.parameter.names]
    
    default.initial.cov.mat = diag(mcmc.parameter.sds[mcmc.parameter.names]^2)
    dimnames(default.initial.cov.mat) = list(mcmc.parameter.names, mcmc.parameter.names)
    
    # Get the default scaling parameters
    default.initial.scaling.parameters = lapply(sampling.blocks, function(block){
        sapply(block, function(var.in.block){
            2.38^2/length(block)
        })
    })
    
    #--------------------------------------------------------------------------#
    #-- Incorporate prior MCMC runs into Initial Values, Step Sizes, Cov Mat --#
    #--------------------------------------------------------------------------#
    
    if (verbose)
        print(paste0(verbose.prefix, "Incorporating prior MCMC runs..."))
    
    initial.model.parameter.values = default.model.parameter.values
    initial.cov.mat = default.initial.cov.mat[mcmc.parameter.names, mcmc.parameter.names]
    initial.scaling.parameters = default.initial.scaling.parameters
    
    dynamic.preceding.codes = calibration.info$preceding.calibration.codes #we'll potentially add to this list in the while block below
    mcmc.parameters.already.pulled.from.preceding = character()
    preceding.index = 1
    
    if (calibration.info$draw.from.parent.version)
        version.for.preceding = spec$parent.version
    else
        version.for.preceding = version
    
    while (preceding.index <= length(dynamic.preceding.codes))
    {
        preceding.code = dynamic.preceding.codes[preceding.index]
        preceding.info = get.calibration.info(code = preceding.code, throw.error.if.missing = F)
        if (is.null(preceding.info))
            stop(paste0(error.prefix, "No calibration has been registered for the preceding.calibration.code of '", preceding.code, "'"))
        
        # Separate out mcmc parameters from the preceding info
        parsed.preceding.aliases = parse.calibration.parameter.aliases(calibration.info = preceding.info, prior = prior)
        
        # Figure out which mcmc parameters we want to pull covariance/scaling for
        overlapping.non.aliased.model.parameter.names = intersect(non.aliased.model.parameter.names,
                                                                  parsed.preceding.aliases$non.aliased.model.parameter.names)
        
        overlapping.parameter.aliases = intersect(names(parameter.aliases),
                                                  names(parsed.preceding.aliases$parameter.aliases))
        overlapping.parameter.aliases = overlapping.parameter.aliases[vapply(overlapping.parameter.aliases, function(alias.name){
            setequal(parameter.aliases[[alias.name]], parsed.preceding.aliases[[alias.name]])
        }, FUN.VALUE = logical(1))]
        mcmc.parameters.to.pull.from.preceding = setdiff(c(overlapping.non.aliased.model.parameter.names, overlapping.parameter.aliases),
                                                         mcmc.parameters.already.pulled.from.preceding)
        
        # Pull from preceding
        if (preceding.index==1 || length(mcmc.parameters.to.pull.from.preceding)>0)
        {
            mcmc.summary = NULL
            mcmc.summary.file = get.mcmc.summary.file(version = version.for.preceding,
                                                      location = location,
                                                      calibration.code = preceding.code,
                                                      root.dir = root.dir)
            
            if (file.exists(mcmc.summary.file))
            {
                summary.mtime = get.mcmc.summary.modified.time(version = version.for.preceding,
                                                               location = location,
                                                               calibration.code = preceding.code,
                                                               root.dir = root.dir)
                
                calibration.mtime = get.calibration.cache.modified.time(version = version.for.preceding,
                                                                        location = location,
                                                                        calibration.code = preceding.code,
                                                                        root.dir = root.dir)
                
                if (is.na(calibration.mtime) || summary.mtime >= calibration.mtime)
                {
                    # if (verbose)
                    # {
                    #     print(paste0("Pulling cached mcmc.summary for '",
                    #                  preceding.code, "'/'", version.for.preceding, "'"))
                    # }
                    
                    mcmc.summary = get(load(mcmc.summary.file)[1])
                }
            }
            
            if (is.null(mcmc.summary))
            {
                # if (verbose)
                # {
                #     print(paste0("Preparing mcmc.summary for '",
                #                  preceding.code, "'/'", version.for.preceding, "'"))
                # }
                
                dir = file.path(get.calibration.dir(version = version.for.preceding,
                                                    location = location,
                                                    calibration.code = preceding.code, 
                                                    root.dir = root.dir),
                                'cache')
                
                if (!dir.exists(dir))
                {
                    stop("The current calibration, '",
                         calibration.code, "' for version '", version,
                         "' depends on prior calibration '",
                         preceding.code, "' for version '", 
                         version.for.preceding, 
                         "'. However, there is no calibration cache for '",
                         preceding.code, "'/'", version.for.preceding,
                         "' nor a cached mcmc.summary")
                }
                
                # Make it and cache it
                mcmc.summary = cache.mcmc.summary(version = version.for.preceding,
                                                  location = location,
                                                  calibration.code = preceding.code,
                                                  root.dir = root.dir,
                                                  get.one.set.of.parameters = calibration.info$is.preliminary,
                                                  throw.error.if.incomplete = T)
            }
            
            # mcmc.summary = prepare.mcmc.summary(version = version.for.preceding,
            #                                     location = location,
            #                                     calibration.code = preceding.code,
            #                                     root.dir = root.dir,
            #                               #      parameter.scales = all.parameter.scales,
            #                                     get.one.set.of.parameters = calibration.info$is.preliminary,
            #                                     error.prefix = error.prefix)
            
            # Pull the model parameter values
            if (preceding.index==1)
            {
                shared.parameter.names = intersect(names(mcmc.summary$last.sim.parameters),
                                                   names(all.default.model.parameter.values))
                
                initial.model.parameter.values[shared.parameter.names] = mcmc.summary$last.sim.parameters[shared.parameter.names]
            }
            
            other.parameters.for.cov = setdiff(mcmc.parameter.names, mcmc.parameters.to.pull.from.preceding)
            mcmc.parameters.already.pulled.from.preceding = union(mcmc.parameters.already.pulled.from.preceding,
                                                                  mcmc.parameters.to.pull.from.preceding)
            
            # Pull for the cov mat
            parameters.with.na.initial.cov.mat.mask = is.na(diag(initial.cov.mat)[mcmc.parameters.to.pull.from.preceding])
            parameters.to.pull.all.cov.from.preceding = mcmc.parameters.to.pull.from.preceding[parameters.with.na.initial.cov.mat.mask]
            parameters.to.pull.weighted.cov.from.preceding = mcmc.parameters.to.pull.from.preceding[!parameters.with.na.initial.cov.mat.mask]
            parameters.to.pull.weighted.cov.from.preceding = intersect(parameters.to.pull.weighted.cov.from.preceding,
                                                                       dimnames(mcmc.summary$cov.mat)[[1]])
            
            initial.cov.mat[parameters.to.pull.weighted.cov.from.preceding,parameters.to.pull.weighted.cov.from.preceding] = initial.cov.mat[parameters.to.pull.weighted.cov.from.preceding,parameters.to.pull.weighted.cov.from.preceding] * (1-calibration.info$weight.to.preceding.variance.estimates) +
                mcmc.summary$cov.mat[parameters.to.pull.weighted.cov.from.preceding,parameters.to.pull.weighted.cov.from.preceding] * calibration.info$weight.to.preceding.variance.estimates
            initial.cov.mat[parameters.to.pull.all.cov.from.preceding,parameters.to.pull.all.cov.from.preceding] = 
                mcmc.summary$cov.mat[parameters.to.pull.all.cov.from.preceding,parameters.to.pull.all.cov.from.preceding]
            initial.cov.mat[parameters.to.pull.all.cov.from.preceding,parameters.to.pull.weighted.cov.from.preceding] = 
                mcmc.summary$cov.mat[parameters.to.pull.all.cov.from.preceding,parameters.to.pull.weighted.cov.from.preceding]
            initial.cov.mat[parameters.to.pull.weighted.cov.from.preceding,parameters.to.pull.all.cov.from.preceding] = 
                mcmc.summary$cov.mat[parameters.to.pull.weighted.cov.from.preceding,parameters.to.pull.all.cov.from.preceding]
            
            initial.cov.mat[mcmc.parameters.to.pull.from.preceding,other.parameters.for.cov] = initial.cov.mat[mcmc.parameters.to.pull.from.preceding,other.parameters.for.cov] = 0
            initial.cov.mat[other.parameters.for.cov, mcmc.parameters.to.pull.from.preceding] = initial.cov.mat[other.parameters.for.cov, mcmc.parameters.to.pull.from.preceding] = 0
            
            initial.scaling.parameters.original=initial.scaling.parameters
            
            # Pull for the scaling parameters
            initial.scaling.parameters.names = names(initial.scaling.parameters)
            initial.scaling.parameters = lapply(names(initial.scaling.parameters), function(scaling.name){
                scaling = initial.scaling.parameters[[scaling.name]]
                to.pull.scaling = mcmc.summary$initial.scaling.parameters[[scaling.name]]
                if (!is.null(to.pull.scaling))
                {
                    to.pull.params = intersect(mcmc.parameters.to.pull.from.preceding, names(to.pull.scaling))
                    scaling[to.pull.params] = to.pull.scaling[to.pull.params]
                }
                
                scaling
            })
            names(initial.scaling.parameters) = initial.scaling.parameters.names
        }
        
        # add it's preceding codes to the pile
        dynamic.preceding.codes = union(dynamic.preceding.codes, preceding.info$preceding.calibration.codes)
        
        # increment for the next iteration through the while loop
        preceding.index = preceding.index + 1
    }
    
    all.initial.model.parameter.values = all.default.model.parameter.values
    all.initial.model.parameter.values[names(initial.model.parameter.values)] = initial.model.parameter.values
    
    #-- Set up starting values --#
    # For starting values, we need one row for each chain
    
    # Begin by using the parameter values from the last sim of the preceding calibration, if exists.
    single.starting.mcmc.parameter.values = initial.model.parameter.values[non.aliased.model.parameter.names]
    single.starting.mcmc.parameter.values[names(parameter.aliases)] = 1
    
    starting.mcmc.parameter.values = t(sapply(1:calibration.info$n.chains, function(chain){
        single.starting.mcmc.parameter.values[mcmc.parameter.names]
    }))
    dim(starting.mcmc.parameter.values) = c(calibration.info$n.chains, length(single.starting.mcmc.parameter.values))
    dimnames(starting.mcmc.parameter.values) = list(NULL, parameter=mcmc.parameter.names)
    
    # If is not preliminary, we'll be using the mcmc summary's samples for the parameters that had been sampled in the preceding calibration.
    if (!calibration.info$is.preliminary)
    {
        n.samples = nrow(mcmc.summary$samples)
        if (n.samples < calibration.info$n.chains)
            stop(paste0(error.prefix, "There are not enough samples in the previous calibration run (",
                        n.samples, ") to seed each of the ", calibration.info$n.chains,
                        " new chains separately"))
        else if (calibration.info$n.chains==1)
            sample.indices = n.samples
        else
        {
            spacing = floor(n.samples/(calibration.info$n.chains-1))
            sample.indices = n.samples - spacing * ((calibration.info$n.chains-1):0)
        }
        
        # To seed multiple chains with different values from a sample of the preceding calibration's sampled parameter values,
        # we'll overwrite the 'starting.mcmc.parameter.values' for parameters that had been sampled.
        
        # This might contain some parameters in their aliased form... I guess we'll just drop them if they're not now needed.
        # Although maybe we should un-alias them first to be sure? Intersect gets rid of them for now.
        params_previously_sampled <- intersect(dimnames(mcmc.summary$samples)[[2]], mcmc.parameter.names)
        
        starting.mcmc.parameter.values[,params_previously_sampled] = mcmc.summary$samples[sample.indices,params_previously_sampled,drop=F]
        
    }
    
    #-----------------#
    #-- Some Checks --#
    #-----------------#
    
    if (any(is.na(starting.mcmc.parameter.values)))
    {
        na.parameters = calibration.info$parameter.names[apply(is.na(starting.mcmc.parameter.values),2,any)]
        stop(paste0(error.prefix,
                    "NA starting values are present for ",
                    ifelse(length(na.parameters)==1, "parameter ", "parameters "),
                    collapse.with.and("'", na.parameters, "'")))
    }

    #-----------------------------#
    #-- Set up the MCMC Control --#
    #-----------------------------#

    
    #-- Set up the compute likelihood function --#
    if (verbose)
        print(paste0(verbose.prefix, "Instantiate the likelihood..."))
    
    if (any(names(calibration.info$special.case.likelihood.instructions)==location))
        lik.instr = calibration.info$special.case.likelihood.instructions[[location]]
    else
        lik.instr = calibration.info$likelihood.instructions
    
    likelihood = lik.instr$instantiate.likelihood(version = version,
                                                  location = location,
                                                  sub.version = sub.version,
                                                  data.manager = calibration.info$data.manager)

    compute.likelihood <- function(sim) {
        lik = likelihood$compute(sim=sim, log=T, use.optimized.get=T, check.consistency=F)
        
        if (is.na(lik))
        {
            error.dir = file.path(get.jheem.root.directory(), 'errors')
            if (!dir.exists(error.dir))
                dir.create(error.dir)
            error.filename = file.path(error.dir, 
                                      paste0("NA_lik_", location, "_", Sys.Date(), ".Rdata"))
            
            print(paste0("THE LIKELIHOOD FOR THE SIMULATION EVALUATED TO NA"))
            print(paste0("  TO ALLOW THE MCMC TO KEEP RUNNING, WILL RETURN log likelihood = -Inf"))
            print(paste0("  SAVING THE SIM THAT GENERATED THIS ERROR TO FILE: '", error.filename, "'"))
            
            -Inf
        }
        else
            lik
    }
    

    #-- Set up the Run Simulation function --#
    if (verbose)
        print(paste0(verbose.prefix, "Setting up the engine..."))
    
    jheem.kernel = create.jheem.kernel(version=version, location=location)

    engine = do.create.jheem.engine(jheem.kernel = jheem.kernel,
                                    sub.version = sub.version,
                                    start.year = NULL, #will set start.year to the version's start.year
                                    end.year = calibration.info$end.year, 
                                    max.run.time.seconds = calibration.info$max.run.time.seconds,
                                    calibration.code = calibration.code,
                                    solver.metadata = calibration.info$solver.metadata,
                                    outcome.location.mapping = likelihood$get.outcome.location.mapping(),
                                    finalize = F)
    
    engine$crunch(parameters = all.initial.model.parameter.values)

    # 'params' is the subset of parameters that we will be modifying in the MCMC
    run.simulation <- function(params) {
        all.parameters = all.initial.model.parameter.values
        params.after.aliases = apply.calibration.parameter.aliases(sorted.parameter.aliases = sorted.parameter.aliases,
                                                                   non.aliased.parameter.names = non.aliased.model.parameter.names,
                                                                   parameter.initial.values = all.initial.model.parameter.values,
                                                                   sampled.values = params)
        all.parameters[names(params.after.aliases)] = params.after.aliases
        engine$run(all.parameters)
    }
    
    #-- Check the initial sims to make sure the likelihood computes (not to -Inf) --#
    if (verbose)
        print(paste0(verbose.prefix, "Making sure the likelihood computes on initial simulation(s)..."))
    for (i in 1:dim(starting.mcmc.parameter.values)[1])
    {
        sim = run.simulation(starting.mcmc.parameter.values[i,])
        if (likelihood$compute(sim, use.optimized.get=T)==-Inf)
        {
            lik.pieces = likelihood$compute.piecewise(sim, use.optimized.get=T)
            .GlobalEnv$errored.likelihood = likelihood
            .GlobalEnv$errored.sim = sim
            
            stop(paste0("The likelihood evaluates to -Inf on the ", 
                        get.ordinal(i), " set of initial parameter values. The likelihood components are:\n",
                        paste0(paste0(" - ", names(lik.pieces), " = ", lik.pieces), collapse='\n'),
                        "\nThe simulation and likelihood have been saved in the global environment as 'errored.sim' and 'errored.likelihood'"))
        }
    }
    

    #-- Set MCMC parameters depending on whether this is prelim or not --#
    if (calibration.info$is.preliminary) # get is.preliminary of calibration info object
    {
        target.acceptance.rate=0.1
        
        scaling.base.update = 1
        scaling.update.prior=10
        scaling.update.decay=.5
        
        n.iter.before.cov = 0
        cov.base.update=0.2
        cov.update.prior=500
        cov.update.decay=0.5
    }
    else
    {
        target.acceptance.rate=0.238
        
        scaling.base.update = 1
        scaling.update.prior=100
        scaling.update.decay=.5
        
        n.iter.before.cov = 0
        cov.base.update=1
        cov.update.prior=500
        cov.update.decay=1
    }

    #-- Make the call to create the control --#
    
    if (verbose)
        print(paste0(verbose.prefix, "Set up the MCMC control..."))

    prior.density.function = distributions::get.density.function(relevant.prior.for.sampled.model.parameters, default.log = T)
    mcmc.prior = function(parameters){
        parameters.after.aliases = apply.calibration.parameter.aliases(sorted.parameter.aliases = sorted.parameter.aliases,
                                                                       non.aliased.parameter.names = non.aliased.model.parameter.names,
                                                                       parameter.initial.values = initial.model.parameter.values,
                                                                       sampled.values = parameters)
        
        prior.density.function(parameters.after.aliases)
    }
    
    ctrl = bayesian.simulations::create.adaptive.blockwise.metropolis.control(
        var.names = mcmc.parameter.names,
        simulation.function = run.simulation,
        log.prior.distribution = mcmc.prior,
        log.likelihood = compute.likelihood, # saves the data manager in here!
        burn = calibration.info$n.burn,
        thin = calibration.info$thin,
        var.blocks = sampling.blocks,
        reset.adaptive.scaling.update.after = 0,
        transformations = mcmc.parameter.scales,
        
        initial.covariance.mat = initial.cov.mat,
        initial.scaling.parameters = initial.scaling.parameters,
        
        target.acceptance.probability = target.acceptance.rate,
        
        n.iter.before.use.adaptive.covariance = n.iter.before.cov,
        adaptive.covariance.base.update = cov.base.update,
        adaptive.covariance.update.prior.iter = cov.update.prior,
        adaptive.covariance.update.decay = cov.update.decay,
        adaptive.scaling = 'componentwise',
        adaptive.scaling.base.update = scaling.base.update,
        adaptive.scaling.update.prior.iter= scaling.update.prior,
        adaptive.scaling.update.decay = scaling.update.decay
    )
    
    #----------------------#
    #-- Set up the Cache --#
    #----------------------#
    
    
    calibration.dir = get.calibration.dir(version=version,
                                          location=location,
                                          calibration.code=calibration.code,
                                          root.dir=root.dir)
    
    if (!dir.exists(calibration.dir))
        dir.create(calibration.dir, recursive = T)
    
    bayesian.simulations::create.mcmc.cache(
        dir = calibration.dir,
        control = ctrl,
        n.iter = calibration.info$n.iter, # just get from calibration object info
        starting.values = starting.mcmc.parameter.values, # harder
        prior.mcmc = NULL,
        cache.frequency = cache.frequency,
        allow.overwrite.cache = allow.overwrite.cache)
    
    
    if (verbose)
        print(paste0(verbose.prefix, "All Done!"))
}

##-------------------------------------------##
##-- PARSING CALIBRATION PARAMETER ALIASES --##
##-------------------------------------------##

apply.calibration.parameter.aliases <- function(sorted.parameter.aliases,
                                                non.aliased.parameter.names,
                                                parameter.initial.values,
                                                sampled.values)
{
    rv = sampled.values[non.aliased.parameter.names]
    if (length(sorted.parameter.aliases)>0)
        rv[names(sorted.parameter.aliases)] = sapply(names(sorted.parameter.aliases), function(aliased.parameter.name){
            parameter.initial.values[aliased.parameter.name] * prod(sampled.values[ sorted.parameter.aliases[[aliased.parameter.name]] ])
        })
    
    rv
}

parse.calibration.parameter.aliases <- function(calibration.info,
                                                prior)
{
    model.parameters.in.aliases = unique(unlist(calibration.info$parameter.aliases))
    parameter.aliases = calibration.info$parameter.aliases
    model.parameters.to.be.overriden.as.aliases = intersect(model.parameters.in.aliases,
                                                            calibration.info$parameter.names)
    parameter.aliases[model.parameters.to.be.overriden.as.aliases] = 
        as.list(model.parameters.to.be.overriden.as.aliases)
    
    non.aliased.model.parameter.names = setdiff(calibration.info$parameter.names, model.parameters.in.aliases)
    
    sorted.parameter.aliases = lapply(model.parameters.in.aliases, function(param.name){
        alias.mask = sapply(parameter.aliases, function(aliased.parameters){
            any(aliased.parameters==param.name)
        })
        names(parameter.aliases)[alias.mask]
    })
    names(sorted.parameter.aliases) = model.parameters.in.aliases
    
    mcmc.parameter.names = union(non.aliased.model.parameter.names, names(parameter.aliases))
    model.parameter.names = union(non.aliased.model.parameter.names, model.parameters.in.aliases)
    
    list(
        model.parameters.in.aliases = model.parameters.in.aliases,
        parameter.aliases = parameter.aliases,
        non.aliased.model.parameter.names = non.aliased.model.parameter.names,
        sorted.parameter.aliases = sorted.parameter.aliases,
        mcmc.parameter.names = mcmc.parameter.names,
        model.parameter.names = model.parameter.names
    )
}

#'@title Run A Calibration
#'@inheritParams set.up.calibration
#'@param chains
#'@param update.frequency
#'@param update.detail
#'
#'@return An MCMC object
#'
#'@export
run.calibration <- function(version,
                            location,
                            calibration.code,
                            root.dir = get.jheem.root.directory("Cannot set up calibration: "),
                            chains,
                            update.frequency = 500,
                            update.detail = 'low')
{
    # Make sure a cache directory has been set up in place
    rv = bayesian.simulations::run.mcmc.from.cache(dir = get.calibration.dir(version=version,
                                                                        location=location,
                                                                        calibration.code=calibration.code,
                                                                        root.dir=root.dir),
                                              chains = chains,
                                              update.frequency = update.frequency,
                                              update.detail = update.detail,
                                              remove.cache.when.done = F)
                    
    cache.mcmc.summary(version = version,
                       location = location,
                       calibration.code = calibration.code,
                       root.dir = root.dir,
                       get.one.set.of.parameters = F,
                       throw.error.if.incomplete = F)
    
    rv                          
}

#' @title Cache A Summary of an MCMC
#'@export
cache.mcmc.summary <- function(version,
                               location,
                               calibration.code,
                               root.dir = get.jheem.root.directory(),
                               get.one.set.of.parameters = F,
                               throw.error.if.incomplete = T)
{
    
    mcmc.summary = prepare.mcmc.summary(version = version,
                                        location = location,
                                        calibration.code = calibration.code,
                                        root.dir = root.dir,
                                        get.one.set.of.parameters = get.one.set.of.parameters,
                                        error.prefix = 'Error preparing mcmc summary to cache: ',
                                        throw.error.if.incomplete = throw.error.if.incomplete)
    
    if (!is.null(mcmc.summary))
    {
        save.to = get.mcmc.summary.file(version = version,
                                        location = location,
                                        calibration.code = calibration.code, 
                                        root.dir = root.dir)
        
        if (!dir.exists(dirname(save.to)))
            dir.create(dirname(save.to), recursive = T)
        
        save(mcmc.summary, file=save.to)
    }
    
    invisible(mcmc.summary)
}

#'@title Get Calibration Progress
#'
#'@description Summarize the progress of a calibration task
#'
#'@param version,calibration.code,locations The version, locations, and calibration code for which to gauge progress
#'@param as.pct Whether the progress should be shown as a percentage instead of a fraction
#'@param round.to.digits How many digits to round progress to
#'@param root.dir The root directory from which to search for calibrations
#' 
#'@export
get.calibration.progress <- function(version,
                                    locations,
                                    calibration.code,
                                    root.dir = get.jheem.root.directory(),
                                    round.to.digits = ifelse(as.pct, 0, 2),
                                    as.pct = T)
{
    max.chains = 1
    
    list.fracs = lapply(locations, function(loc){
        cache.dir = file.path(get.calibration.dir(version = version,
                                                  location = loc,
                                                  calibration.code = calibration.code, 
                                                  root.dir = root.dir),
                              'cache')
        
        files = list.files(cache.dir)
        files = file.path(cache.dir, files[grepl('^chain[0-9]+_control.Rdata$', files)])
        
        if (length(files)>max.chains)
            max.chains <<- length(files)
        
        if (length(files)==0)
            NA
        else
        {
            loc.rv = sapply(files, function(file){
                chain.control = get(load(file)[1])
                mean(chain.control@chunk.done)
            })
            
            loc.rv
        }
    })
    
    rv = matrix(NA, nrow=length(locations), ncol=max.chains,
                dimnames = list(location=locations, chain=paste0("chain", 1:max.chains)))
    
    for (i in 1:length(locations))
    {
        loc.rv = list.fracs[[i]]
        rv[i,1:length(loc.rv)] = loc.rv 
    }
    
    if (as.pct)
        rv = rv*100
    
    if (!is.na(round.to.digits))
        rv = floor(rv*10^round.to.digits) / (10^round.to.digits)
  #      rv = round(rv, digits = round.to.digits)
  #      rv = format(round(rv, digits = round.to.digits), nsmall=round.to.digits)
    
    rv
    
}
#'@title Check if Simulations Have Been Saved
#'
#'@description Summarize the progress of a calibration task
#'
#'@param version,calibration.code,locations The version, locations, and calibration code for which to gauge progress
#'@param as.pct Whether the progress should be shown as a percentage instead of a fraction
#'@param round.to.digits How many digits to round progress to
#'@param root.dir The root directory from which to search for calibrations
#' 
#'@export
are.simulations.present <- function(version,
                                    locations,
                                    calibration.code,
                                    interventions,
                                    n.sim,
                                    sub.version = NULL,
                                    root.dir = get.jheem.root.directory())
{
    sapply(interventions, function(int.code){
        sapply(locations, function(loc){
            
            file = get.simset.filename(version = version,
                                       location = loc,
                                       calibration.code = calibration.code,
                                       intervention.code = int.code, 
                                       n.sim = n.sim, 
                                       root.dir = root.dir, 
                                       sub.version = sub.version)
            
            file.exists(file)
            
        })
    })
}

get.calibration.cache.modified.time <- function(version, location, calibration.code, root.dir = get.jheem.root.directory())
{
    dir = file.path(get.calibration.dir(version = version,
                              location = location,
                              calibration.code = calibration.code, 
                              root.dir = root.dir),
                    'cache')
    
    if (dir.exists(dir))
    {
        files = list.files(dir, full.names = T)
        max(sapply(files, file.mtime))
    }
    else
        NA
}

get.mcmc.summary.modified.time <- function(version, location, calibration.code, root.dir = get.jheem.root.directory())
{
    file = get.mcmc.summary.file(version = version,
                                 location = location,
                                 calibration.code = calibration.code, 
                                 root.dir = root.dir)
    if (file.exists(file))
        file.mtime(file)
    else
        NA
}

#' @title Clear a Calibration Cache
#'@inheritParams set.up.calibration
#'@param allow.remove.incomplete
#'
#'@export
clear.calibration.cache <- function(version,
                                    location,
                                    calibration.code,
                                    root.dir = get.jheem.root.directory("Cannot set up calibration: "),
                                    remove.mcmc.summary = F,
                                    allow.remove.incomplete = F)
{
    bayesian.simulations::remove.mcmc.cache(get.calibration.dir(version=version,
                                                                location=location,
                                                                calibration.code=calibration.code,
                                                                root.dir=root.dir),
                                            allow.remove.incomplete = allow.remove.incomplete)    
    
    if (remove.mcmc.summary)
    {
        # @Andrew
    }
}

#' @title Assemble an MCMC From a Calibration
#'@inheritParams set.up.calibration
#'@param allow.remove.incomplete
#'@param chains
#'
#'@export
assemble.mcmc.from.calibration <- function(version,
                                           location,
                                           calibration.code,
                                           root.dir = get.jheem.root.directory("Cannot set up calibration: "),
                                           allow.incomplete=F,
                                           chains = NULL)
{
    mcmc = bayesian.simulations::assemble.mcmc.from.cache(dir = get.calibration.dir(version=version,
                                                                                    location=location,
                                                                                    calibration.code=calibration.code,
                                                                                    root.dir=root.dir),
                                                          allow.incomplete = allow.incomplete,
                                                          chains = chains)
    
    mcmc
}

#' @title Extract the Last Simulation From A Calibration
#'@inheritParams assemble.mcmc.from.calibration
#'@param include.first.sim Whether to also pull the first simulation from the calibration (will then return a 2-simulation simset)
#'
#'@export
extract.last.simulation.from.calibration <- function(version,
                                                     location,
                                                     calibration.code,
                                                     root.dir = get.jheem.root.directory("Cannot set up calibration: "),
                                                     allow.incomplete=T,
                                                     include.first.sim=F,
                                                     chains = NULL)
{
    dir = get.calibration.dir(version=version,
                              location=location,
                              calibration.code=calibration.code,
                              root.dir=root.dir)
    
    cache.dir = file.path(dir, 'cache')
    chain.dirs = list.dirs(cache.dir, recursive = F, full.names = F)
    n.chains = length(chain.dirs)
    
    if (!dir.exists(cache.dir))
        stop(paste0("Cannot extract.last.simulation.from.calibration() - no MCMC cache has been set up in '", dir, "'"))
    
    if (is.null(chains))
        chains = 1:n.chains
    else
    {
        missing.chains = setdiff(chains, 1:n.chains)
        if (length(missing.chains)>0)
            stop(paste0("The calibration '", calibration.code,
                        "' only has ",
                        n.chains,
                        ifelse(n.chains==1, " chain", " chains"),
                        ". The given ",
                        ifelse(length(missing.chains)==1, "value", "values"),
                        " for 'chains' (",
                        paste0(missing.chains, collapse=','),
                        ifelse(length(missing.chains)==1, ") is invalid", ") are invalid")))
    }
    
    max.chunk = -1
    chain.with.max.chunk = -1
    for (chain in chains)
    {
        chain.dir = file.path(cache.dir, paste0('chain_', chain))
        chunk.files = list.files(chain.dir)
        chunk = as.numeric(substr(chunk.files, start=nchar(paste0("chain", chain, "_chunk"))+1, nchar(chunk.files)-6))
        
        if (length(chunk)>0)
        {
            max.chunk.for.chain = max(chunk)
            if (max.chunk.for.chain > max.chunk)
            {
                max.chunk = max.chunk.for.chain
                chain.with.max.chunk = chain
            }
        }
    }
    
    if (max.chunk<0)
        stop("Cannot extract.last.simulation.from.calibration - no simulations have been saved")
    
    chain.control = get(load(file.path(cache.dir, paste0('chain', chain.with.max.chunk, "_control.Rdata"))))

    
    if (!all(chain.control@chunk.done))
    {
        if (!allow.incomplete)
            stop("Cannot extract.last.simulation.from.calibration - no chains have finished running. Use allow.incomplete = T to get the last simulation that has been finished")
        
        print(paste0("Sampling incomplete: Using the ",
                     get.ordinal(max.chunk),
                     " chunk (out of ",
                     length(chain.control@chunk.done), ")",
                     ifelse(n.chains==1, '',
                            paste0(" from the ", get.ordinal(chain.with.max.chunk), " chain"))))
    }
    
    for (chunk in max.chunk:1)
    {
        mcmc.last = get(load(file.path(cache.dir, 
                                       paste0('chain_', chain.with.max.chunk), 
                                       paste0('chain', chain.with.max.chunk, "_chunk", chunk, ".Rdata"))))

        if (mcmc.last@n.iter>0)
            break;
    }
    
    sim.last = mcmc.last@simulations[[length(mcmc.last@simulations)]]
    
    if (include.first.sim)
    {
        sim.first = NULL
        chunk.index = 1
        while (is.null(sim.first))
        {
            mcmc.first = get(load(file.path(cache.dir, 
                                            paste0('chain_', chain.with.max.chunk), 
                                            paste0('chain', chain.with.max.chunk, "_chunk", chunk.index, ".Rdata"))))
            
            if (length(mcmc.first@simulations)>0)
                sim.first = mcmc.first@simulations[[1]]
            
            chunk.index = chunk.index + 1
        }
        
        sim = do.join.simulation.sets(sim.first, sim.last)
    }
    else
        sim = do.join.simulation.sets(sim.last) #this finalizes things
    
    sim
}

#' @title Assemble Simulations From A Calibration
#'@inheritParams assemble.mcmc.from.calibration
#'
#'@export
# INTERNAL NOTE: NB - we have analogous functionality in do.join.simulation.sets()
#                in JHEEM_simulation.R If we ever edit this, we need to edit that in parallel too
#                (It is here for memory efficiency)
assemble.simulations.from.calibration <- function(version,
                                                  location,
                                                  calibration.code,
                                                  root.dir = get.jheem.root.directory("Cannot set up calibration: "),
                                                  allow.incomplete=F,
                                                  chains = NULL,
                                              #    burn = 0,
                                                  verbose = T)
{
    #-- Initial Set-Up (pull control files, check arguments) --#
    error.prefix = paste0("Cannot assemble simulations: ")
    
    calibration.dir = get.calibration.dir(version = version,
                                          location = location,
                                          calibration.code = calibration.code,
                                          root.dir = root.dir)
    
    global.control = get(load(file.path(calibration.dir, 'cache', 'global_control.Rdata')))
    
    # if (!is.numeric(burn) || length(burn)!=1 || is.na(burn) || burn<0)
    #     stop(paste0(error.prefix, "'burn' must be either a fraction between 0 and 1, or a positive integer"))
    # if (burn > 1 && round(burn)!=burn)
    #     stop(paste0(error.prefix, "'burn' must be either a fraction between 0 and 1, or a positive integer"))
        
    
    if (is.null(chains))
        chains = 1:global.control@n.chains
    else if (!is.numeric(chains) || length(chains)==0 || any(is.na(chains)) || any(chains<=0))
        stop(paste0(error.prefix, "'chains' must be a numeric vector with only positive values and no NA values"))
    else
    {
        missing.chains = setdiff(chains, 1:global.control@n.chains)
        if (length(missing.chains)>0)
            stop(paste0(error.prefix,
                        "The calibration only has ",
                        global.control@n.chains,
                        ifelse(global.control@n.chains==1, " chain", " chains"),
                        ". ",
                        collapse.with.and(missing.chains),
                        ifelse(length(missing.chains)==1, " is not a valid chain index", " are not valid chain indices")))
    }
    chain.controls = lapply(file.path(calibration.dir, 'cache', paste0("chain", chains, "_control.Rdata")), function(file){
        get(load(file))
    })
    
    max.done.chunk.per.chain = sapply(chain.controls, function(ctrl){
        max(c(0, (1:length(ctrl@chunk.done))[ctrl@chunk.done]))
    })
    
    first.chunk.to.save = (1:length(global.control@save.chunk))[global.control@save.chunk][1]
    
    if (all(max.done.chunk.per.chain<first.chunk.to.save))
    {
        stop(paste0(error.prefix,
                    ifelse(sum(max.done.chunk.per.chain<first.chunk.to.save)==1, "Chain ", "Chains "),
                    collapse.with.and(chains[max.done.chunk.per.chain<first.chunk.to.save]),
                    ifelse(sum(max.done.chunk.per.chain<first.chunk.to.save)==1, " has not", " have not"),
                    " saved ANY samples (ie, gotten past the ",
                    get.ordinal(first.chunk.to.save), " of ", global.control@n.chunks, " chunk)"
        ))
    }
    
    if (!allow.incomplete & any(max.done.chunk.per.chain<global.control@n.chunks))
    {
        incomplete.chains = chains[max.done.chunk.per.chain<global.control@n.chunks]
        stop(paste0(error.prefix,
                    ifelse(length(incomplete.chains)==1, "Chain ", "Chains "),
                    collapse.with.and(incomplete.chains),
                    ifelse(length(incomplete.chains)==1, " has not", " have not"),
                    " finished sampling. Use allow.incomplete=T to assemble simulations anyway"
        ))
    }
    
    #-- Loop through the individually saved MCMCs --#
    
    n.sim.per.chain = sapply(chain.controls, function(ctrl){
        floor(ctrl@chain.state@n.unthinned.after.burn / global.control@control@thin)
    })
    
    if (all(n.sim.per.chain==0))
        stop(paste0(error.prefix, "No simulations were saved after the burn-in period for this calibration - there are no simulations to assemble"))
        #stop(paste0(error.prefix, "There is an internal error, likely reflecting inconsistencies in multiple merged runs of the MCMC - one calculation thinks there are simulations run, but another thinks no simulations were run"))
    
    n.sim = sum(n.sim.per.chain)
    
    outcome.dimnames = NULL
    combined.outcome.numerators = NULL
    combined.outcome.denominators = NULL
    combined.is.degenerate = rep(as.numeric(NA), n.sim)
    combined.parameters = NULL
    n.parameters = NULL
    run.metadata.list = list()
    simulation.chain = rep(as.numeric(NA), n.sim)
    sims.done = 0
    
    for ( chain.index in (1:length(chains))[n.sim.per.chain>0] )
    {
        chain = chains[chain.index]
        chain.dir = file.path(calibration.dir, 'cache', paste0('chain_', chain))
        if (max.done.chunk.per.chain[chain.index] >= first.chunk.to.save)
        {
            # cutoffs = first.chunk.to.save + floor((max.done.chunk.per.chain[chain.index] - first.chunk.to.save)/11)
            for (chunk in first.chunk.to.save:max.done.chunk.per.chain[chain.index])
            {
                if (verbose && (chunk==first.chunk.to.save || chunk%%100==1))
                {
                    cat(paste0("Assembling chain ", chain, ", chunks ", chunk, " to ",
                               min(max.done.chunk.per.chain[chain.index], 100 * (floor(chunk/100)+1)),
                               " of ", max.done.chunk.per.chain[chain.index], "..."))
                }
                
                sub.mcmc = get(load(file.path(chain.dir, paste0("chain", chain, "_chunk", chunk, ".Rdata"))))
                
                if (sub.mcmc@n.iter>0 && length(sub.mcmc@simulations)>0)
                {
                    #-- Set up our data structures --#
                    if (is.null(outcome.dimnames))
                    {
                        sample.simset = sub.mcmc@simulations[[1]]   
                        outcomes = sample.simset$outcomes
                        
                        outcome.dimnames = lapply(sample.simset$outcome.ontologies, function(outcome.ontology) {
                            dim.names = outcome.ontology
                            dim.names[['sim']] = 1:n.sim
                            dim.names
                        })
                        
                        combined.outcome.numerators = lapply(outcome.dimnames, function(dim.names){
                            array(as.numeric(NA), 
                                  dim = sapply(dim.names, length),
                                  dimnames = dim.names)
                        })
                        
                        combined.outcome.denominators = lapply(names(combined.outcome.numerators), function(outcome.name){
                            dim.names = outcome.dimnames[[outcome.name]]
                            
                            if (is.null(sample.simset$data$outcome.denominators[[outcome.name]]))
                                NULL
                            else
                                array(as.numeric(NA), 
                                      dim = sapply(dim.names, length),
                                      dimnames = dim.names)
                        })
                        names(combined.outcome.denominators) = names(combined.outcome.numerators)
                        
                        
                        combined.parameters = array(as.numeric(NA),
                                                    dim = c(parameter = dim(sample.simset$parameters)[1],
                                                               sim = n.sim),
                                                    dimnames = list(parameter = dimnames(sample.simset$parameters)$parameter,
                                                                    sim = NULL))
                        
                        n.parameters = dim(combined.parameters)[1]
                    }
                    
                    new.simulations = sub.mcmc@simulations[as.integer(sub.mcmc@simulation.indices)]
                    
                    for (i in 1:length(new.simulations))
                    {
                        # Write into column-major indexing
                        for (outcome.name in names(combined.outcome.numerators))
                        {
                            src = new.simulations[[i]]$data$outcome.numerators[[outcome.name]]
                            src.indices = 1:length(src)
                            dst.indices = src.indices + sims.done * length(src)
                            
                            combined.outcome.numerators[[outcome.name]] = overwrite_arr(
                                dst = combined.outcome.numerators[[outcome.name]],
                                dst_indices = dst.indices,
                                src = src,
                                src_indices = src.indices)
                            
                            denom = new.simulations[[i]]$data$outcome.denominators[[outcome.name]]
                            if (!is.null(denom))
                            {
                                combined.outcome.denominators[[outcome.name]] = overwrite_arr(
                                    dst = combined.outcome.denominators[[outcome.name]],
                                    dst_indices = dst.indices,
                                    src = denom,
                                    src_indices = src.indices)
                            }
                        }
                        
                        combined.parameters = overwrite_arr(
                            dst = combined.parameters,
                            dst_indices = 1:n.parameters + sims.done * n.parameters,
                            src = new.simulations[[i]]$parameters,
                            src_indices = 1:n.parameters
                        )
                        
                        simulation.chain = overwrite_arr(
                            dst = simulation.chain,
                            dst_indices = sims.done + 1,
                            src = chain,
                            src_indices = 1)
                        
                        combined.is.degenerate = overwrite_arr(
                            dst = combined.is.degenerate,
                            dst_indices = sims.done + 1,
                            src = as.numeric(new.simulations[[i]]$is.degenerate),
                            src_indices = 1)
                        
                        sims.done = sims.done + 1
                    }               
                    
                    # Pull the run.metadata
                    run.times = as.numeric(sub.mcmc@run.times)#[sim.indices]
                    last.valid = run.times[1]
                    run.times = sapply(run.times, function(time){
                        if (!is.na(time))
                            last.valid = time
                        last.valid
                    })
                    
                    run.metadata.list = c(run.metadata.list, 
                                          lapply(1:length(new.simulations), function(i){
                                              r.meta = new.simulations[[i]]$run.metadata
                                              copy.run.metadata(r.meta, run.time = run.times[i] / sub.mcmc@thin)
                                          }))
                    
                    sub.mcmc = NULL
                    
                    # We're going to gc() here to keep memory usage down
                    gc()
                }
                
                if (verbose && (chunk%%100==0 || chunk==max.done.chunk.per.chain[chain.index]))
                {
                    cat("Done\n")
                }
            }
        }
    }
    
    #-- Do we have fewer sims than expected?? --#
    if (sims.done < n.sim)
    {
        combined.outcome.numerators = lapply(combined.outcome.numerators, function(num){
            array.access(num, sim=1:sims.done)
        })
        
        combined.outcome.denominators = lapply(combined.outcome.denominators, function(denom){
            if (is.null(denom))
                NULL
            else
                array.access(denom, sim=1:sims.done)
        })
        
        combined.parameters = array.access(combined.parameters, sim=1:sims.done)
        
        simulation.chain = simulation.chain[1:sims.done]
        
        combined.is.degenerate = combined.is.degenerate[1:sims.done]
    }
    
    #-- Make the simulation set --#
    do.create.simulation.set(jheem.kernel = sample.simset$jheem.kernel,
                             sub.version = sample.simset$sub.version,
                             outcome.numerators = combined.outcome.numerators,
                             outcome.denominators = combined.outcome.denominators,
                             parameters = combined.parameters,
                             simulation.chain = simulation.chain,
                             from.year = sample.simset$from.year,
                             to.year = sample.simset$to.year,
                             n.sim = sims.done,
                             intervention.code = sample.simset$intervention.code,
                             calibration.code = sample.simset$calibration.code,
                             outcome.location.mapping = sample.simset$outcome.location.mapping,
                             solver.metadata = sample.simset$solver.metadata,
                             run.metadata = join.run.metadata(run.metadata.list),
                             finalize = T,
                             is.degenerate = as.logical(combined.is.degenerate))
}

newer.old.assemble.simulations.from.calibration <- function(version,
                                                  location,
                                                  calibration.code,
                                                  root.dir = get.jheem.root.directory("Cannot set up calibration: "),
                                                  allow.incomplete=F,
                                                  chains = NULL,
                                                  verbose = T)
{
    error.prefix = paste0("Cannot assemble simulations: ")
    
    calibration.dir = get.calibration.dir(version = version,
                                         location = location,
                                         calibration.code = calibration.code,
                                         root.dir = root.dir)
    
    global.control = get(load(file.path(calibration.dir, 'cache', 'global_control.Rdata')))
    
    if (is.null(chains))
        chains = 1:global.control@n.chains
    else if (!is.numeric(chains) || length(chains)==0 || any(is.na(chains) || any(chains<=0)))
        stop(paste0(error.prefix, "'chains' must be a numeric vector with only positive values and no NA values"))
    else
    {
        missing.chains = setdiff(chains, 1:global.control@n.chains)
        if (length(missing.chains)>0)
            stop(paste0(error.prefix,
                        "The calibration only has ",
                        global.control@n.chains,
                        ifelse(global.control@n.chains==1, " chain", " chains"),
                        ". ",
                        collapse.with.and(missing.chains),
                        ifelse(length(missing.chains)==1, " is not a valid chain index", " are not valid chain indices")))
    }
    chain.controls = lapply(file.path(calibration.dir, 'cache', paste0("chain", chains, "_control.Rdata")), function(file){
        get(load(file))
    })
    
    max.done.chunk.per.chain = sapply(chain.controls, function(ctrl){
        max(c(0, (1:length(ctrl@chunk.done))[ctrl@chunk.done]))
    })
    
    first.chunk.to.save = (1:length(global.control@save.chunk))[global.control@save.chunk][1]
    
    if (any(max.done.chunk.per.chain==first.chunk.to.save))
    {
        stop(paste0(error.prefix,
                    ifelse(sum(max.done.chunk.per.chain<first.chunk.to.save)==1, "Chain ", "Chains "),
                    collapse.with.and(chains[max.done.chunk.per.chain<first.chunk.to.save]),
                    ifelse(sum(max.done.chunk.per.chain<first.chunk.to.save)==1, " has not", " have not"),
                    " saved ANY samples (ie, gotten past the ",
                    get.ordinal(first.chunk.to.save), " of ", global.control@n.chunks, " chunk)"
                    ))
    }
    
    if (!allow.incomplete & any(max.done.chunk.per.chain<global.control@n.chunks))
    {
        incomplete.chains = chains[max.done.chunk.per.chain<global.control@n.chunks]
        stop(paste0(error.prefix,
                    ifelse(length(incomplete.chains)==1, "Chain ", "Chains "),
                    collapse.with.and(incomplete.chains),
                    ifelse(length(incomplete.chains)==1, " has not", " have not"),
                    " finished sampling. Use allow.incomplete=T to assemble simulations anyway"
        ))
    }
    
    simulations = list()
    simulation.chain = list()
    run.metadatas = list()
    
    for (chain.index in 1:length(chains))
    {
        chain = chains[chain.index]
        chain.dir = file.path(calibration.dir, 'cache', paste0('chain_', chain))
        if (max.done.chunk.per.chain[chain.index] >= first.chunk.to.save)
        {
           # cutoffs = first.chunk.to.save + floor((max.done.chunk.per.chain[chain.index] - first.chunk.to.save)/11)
            for (chunk in first.chunk.to.save:max.done.chunk.per.chain[chain.index])
            {
                if (verbose && (chunk==first.chunk.to.save || chunk%%100==1))
                {
                    cat(paste0("Assembling chain ", chain, ", chunks ", chunk, " to ",
                               min(max.done.chunk.per.chain[chain.index], 100 * (floor(chunk/100)+1)),
                               " of ", max.done.chunk.per.chain[chain.index], "..."))
                }
                
                sub.mcmc = get(load(file.path(chain.dir, paste0("chain", chain, "_chunk", chunk, ".Rdata"))))
                
                if (sub.mcmc@n.iter>0)
                {
                    new.simulations = sub.mcmc@simulations[as.integer(sub.mcmc@simulation.indices)]
                    simulations = c(simulations, new.simulations)
                    simulation.chain = c(simulation.chain, rep(chain, length(new.simulations)))
                    
                    run.times = as.numeric(sub.mcmc@run.times)#[sim.indices]
                    last.valid = run.times[1]
                    run.times = sapply(run.times, function(time){
                        if (!is.na(time))
                            last.valid = time
                        last.valid
                    })
                    
                    run.metadatas = c(run.metadatas, 
                                      lapply(1:length(new.simulations), function(i){
                                          r.meta = new.simulations[[i]]$run.metadata
                                          copy.run.metadata(r.meta, run.time = run.times[i] / sub.mcmc@thin)
                                      }))
                }
                
                if (verbose && chunk%%100==0)
                {
                    cat("Done\n")
                }
            }
        }
    }
    
    do.join.simulation.sets(simulations, 
                            simulation.chain = unlist(simulation.chain),
                            finalize = T, 
                            run.metadata = join.run.metadata(run.metadatas))
}

OLD.assemble.simulations.from.calibration <- function(version,
                                                  location,
                                                  calibration.code,
                                                  root.dir = get.jheem.root.directory("Cannot set up calibration: "),
                                                  allow.incomplete=F,
                                                  chains = NULL)
{
    mcmc = assemble.mcmc.from.calibration(version = version,
                                                      location = location,
                                                      calibration.code = calibration.code,
                                                      root.dir = root.dir,
                                                      allow.incomplete = allow.incomplete,
                                                      chains = chains)
    
    
    sim.indices = as.integer(mcmc@simulation.indices)
    sim.chain = rep(1:mcmc@n.chains, length(sim.indices)/mcmc@n.chains)
    if (any(is.na(sim.indices))) # This is a patch to the error in the MCMC package code with propagating NA values
    {
        print("Error in assembling simset - NAs introduced by MCMC package - we will try to recover")
        search.from = 1
        sim.indices = sapply(1:length(mcmc@simulation.indices), function(i){
            
            params = mcmc@samples[1,i,]
            if (any(is.na(params)))
                return(search.from)
            
            for (j in search.from:length(mcmc@simulations))
            {
                if (all(mcmc@simulations[[j]]$params == params))
                {
                    search.from <<- j
                    return (j)
                }
            }
            
            NA
        })
        
        if (any(is.na(sim.indices)))
            stop("Cannot assemble simulations from calibration - NAs were introduced in the MCMC package code and we cannot recover from this (despite trying)")
    }
    
    run.times = as.numeric(mcmc@run.times)#[sim.indices]
    last.valid = run.times[1]
    run.times = sapply(run.times, function(time){
        if (!is.na(time))
            last.valid = time
        last.valid
    })

    run.metadatas = lapply(1:length(sim.indices), function(i){
        r.meta = mcmc@simulations[[ sim.indices[i] ]]$run.metadata
        copy.run.metadata(r.meta, run.time = run.times[i] / mcmc@thin)
    })
    simulations = mcmc@simulations[sim.indices]
    
    # could choose to delete cache now but will not, undecided
    do.join.simulation.sets(simulations, 
                         simulation.chain = sim.chain,
                         finalize = T, 
                         run.metadata = join.run.metadata(run.metadatas))
}


#' @title Register Calibration Info
#'@param code
#'@param likelihood.instructions
#'@param is.preliminary
#'@param end.year
#'@param parameter.names
#'@param parameter.aliases A named list of character vectors. Each character vector must be one or more parameter names from the parameter distribution, and the names represent the aliases to be used in sampling
#'@param n.iter
#'@param thin
#'@param description
#'@param fixed.initial.parameter.values
#'@param max.run.time.seconds
#'@param solver.metadata
#'@param n.chains
#'@param n.burn
#'@param data.manager
#'@param preceding.calibration.codes
#'@param draw.from.parent.version
#'@param weight.to.preceding.variance.estimates
#'@param error.prefix
#'
#'@export
register.calibration.info <- function(code,
                                      likelihood.instructions,
                                      is.preliminary,
                                      end.year,
                                      parameter.names,
                                      parameter.aliases = NULL,
                                      n.iter,
                                      thin,
                                      description,
                                      special.case.likelihood.instructions = list(),
                                      fixed.initial.parameter.values = numeric(),
                                      max.run.time.seconds = Inf,
                                      solver.metadata = NULL,
                                      n.chains = if (is.preliminary) 1 else 4,
                                      n.burn = if (is.preliminary) 0 else floor(n.iter / 2),
                                      data.manager = get.default.data.manager(),
                                      preceding.calibration.codes = character(),
                                      draw.from.parent.version = F,
                                      weight.to.preceding.variance.estimates = 0.5,
                                      error.prefix = "Error registering calibration info: ")
{
    #-- Validate arguments --#
    
    # error.prefix
    if (!is.character(error.prefix) || length(error.prefix)!=1 || is.na(error.prefix))
        stop("Cannot register.calibration.info(): 'error.prefix' must be a single, non-NA character value")
    
    # code
    validate.calibration.code(code = code,
                              error.prefix = error.prefix,
                              code.name.for.error = 'code')
    # - We should not let calibration code overlap with any intervention codes?
    
    if (!is.numeric(n.iter) || length(n.iter)!=1 || is.na(n.iter) || n.iter<=0 || round(n.iter)!=n.iter)
        stop(paste0(error.prefix, "n.iter must be a single, non-NA, positive integer"))
    
    if (!is.numeric(n.burn) || length(n.burn)!=1 || is.na(n.burn) || n.burn<0 || round(n.burn)!=n.burn)
        stop(paste0(error.prefix, "n.burn must be a single, non-NA, non-negative integer"))
    
    if (!is.numeric(thin) || length(thin)!=1 || is.na(thin) || thin<=0 || round(thin)!=thin)
        stop(paste0(error.prefix, "thin must be a single, non-NA, positive integer"))
    
    if (n.burn >= n.iter)
        stop(paste0(error.prefix, "n.burn (", n.burn, ") must be less than n.iter (", n.iter, ") - otherwise you won't save any simulations"))
    
    # Make sure this is going to save at least one simulation after burn
    if ((n.iter - n.burn) < thin)
    {
        stop(paste0(paste0(error.prefix, "With only ", n.iter-n.burn,
                           ifelse((n.iter-n.burn)==1, " iteration", " iterations"),
                           ifelse(n.burn==0, '',
                                  paste0(" after burning the first ", n.burn, ",")),
                           " and a thin of ", thin,
                           ", no simulations will be saved. Either increase n.iter",
                           ifelse(n.burn==0, '',
                                  ", decrease n.burn,"),
                           " or decrease thin.")))
    }
    
    # likelihood.instructions
    if (!is(likelihood.instructions, 'jheem.likelihood.instructions'))
        stop(paste0(error.prefix, "'likelihood.instructions' must be an object of class 'jheem.likelihood.instructions'"))
    
    # end.year
    if (!is.numeric(end.year) || length(end.year)!=1 || is.na(end.year))
        stop(paste0(error.prefix, "'end.year' must be a single, non-NA numeric value"))
    
    current.year = as.numeric(format(Sys.Date(), "%Y"))
    if (end.year < current.year)
        stop(paste0(error.prefix, "'end.year' must be no earlier than the current year (", current.year, ")"))
    
    # parameter.aliases
    if (is.null(parameter.aliases))
    {}
    else
    {
        if (!is.list(parameter.aliases))
            stop(paste0(error.prefix, "'parameter.aliases' must be a named LIST of character vectors"))
        
        if (any(!sapply(parameter.aliases, is.character)))
            stop(paste0(error.prefix, "'parameter.aliases' must be a named list of CHARACTER VECTORS"))
        
        if (is.null(names(parameter.aliases)))
            stop(paste0(error.prefix, "'parameter.aliases' must be a NAMED list of character vectors"))
        
        if (any(sapply(parameter.aliases, function(values){
            length(values)==0 || any(is.na(values))
        })))
            stop(paste0(error.prefix, "The elements of 'parameter.aliases' must be non-empty and contain no NA values"))
    }
    
    # n.chains is an integer >= 1
    if (!is.numeric(n.chains) || length(n.chains)!=1 || is.na(n.chains) || round(n.chains)!=n.chains || n.chains<1)
        stop(paste0(error.prefix, "'n.chains' must be a single, non-NA integer value greater than or equal to 1"))
    
    if (!is.numeric(fixed.initial.parameter.values) || any(is.na(fixed.initial.parameter.values)))
        stop(paste0(error.prefix, "'fixed.initial.parameter.values' must be a numeric vector with no NA values"))
    
    if (length(fixed.initial.parameter.values) > 0 &&
        (is.null(names(fixed.initial.parameter.values)) || any(is.na(names(fixed.initial.parameter.values)))))
        stop(paste0(error.prefix, "'fixed.initial.parameter.values' must be a NAMED numeric vector (with no NA names)"))
    
    # description is a single, non-NA character value
    
    if (!is.logical(draw.from.parent.version) || length(draw.from.parent.version)!=1 || is.na(draw.from.parent.version))
        stop(paste0(error.prefix, "'draw.from.parent.version' must be a single, non-NA logical value (TRUE or FALSE)"))
    
    if (length(preceding.calibration.codes)>0)
    {
        for (preceding.code in preceding.calibration.codes)
        {
            preceding.info = get.calibration.info(code = preceding.code, throw.error.if.missing = F)
            if (is.null(preceding.info))
                stop(paste0(error.prefix, "No calibration has been registered for the preceding.calibration.code of '", preceding.code, "'"))
        }
    }
    else
    {
        if (draw.from.parent.version)
            stop(paste0(error.prefix, "If 'draw.from.parent.version' is TRUE, 'preceding.calibration.codes' must contain at least one preceding calibration code"))
    }
    
    # For now, just going to package up and store it so things work
    calibration.info = list(
        code = code,
        likelihood.instructions = likelihood.instructions,
        special.case.likelihood.instructions = special.case.likelihood.instructions,
        data.manager = data.manager,
        end.year = end.year,
        parameter.names = parameter.names,
        parameter.aliases = parameter.aliases,
        n.chains = n.chains,
        n.iter = n.iter,
        n.burn = n.burn,
        thin = thin,
        solver.metadata = solver.metadata,
        preceding.calibration.codes = preceding.calibration.codes,
        fixed.initial.parameter.values = fixed.initial.parameter.values,
        draw.from.parent.version = draw.from.parent.version,
        weight.to.preceding.variance.estimates = weight.to.preceding.variance.estimates,
        is.preliminary = is.preliminary,
        max.run.time.seconds = max.run.time.seconds,
        description = description
    )
    
    CALIBRATION.MANAGER$run.info[[code]] = calibration.info
    
    invisible(NULL)
}

CALIBRATION.MANAGER = new.env()
CALIBRATION.MANAGER$run.info = list()
CALIBRATION.MANAGER$transmute.info = list()

#'@title Clear JHEEM Calibrations
#'@export
clear.calibrations <- function()
{
     CALIBRATION.MANAGER$run.info = list()
     CALIBRATION.MANAGER$transmute.info = list()
}

#'@title Copy Calibration Info to a New Code
#'
#'@param from.code The code for the the calibration info to copy
#'@param to.code The calibration code to be used for the copied calibration info
#'
#'@export
copy.calibration.info <- function(from.code,
                                  to.code,
                                  n.iter = NULL,
                                  n.burn = NULL,
                                  thin = NULL,
                                  solver.metadata = NULL)
{
    validate.calibration.code(code = from.code,
                              error.prefix = "Cannot copy calibration info: ",
                              code.name.for.error = 'from.code')
    validate.calibration.code(code = to.code,
                              error.prefix = "Cannot copy calibration info: ",
                              code.name.for.error = 'to.code')
    
    calibration.info = get.calibration.info(code = from.code,
                                            throw.error.if.missing = T,
                                            error.prefix = paste0("Cannot copy calibration info from '", from.code, "'"))
    
    calibration.info$code = to.code
    if (!is.null(n.iter))
        calibration.info$n.iter = n.iter
    if (!is.null(thin))
        calibration.info$thin = thin
    if (!is.null(n.burn))
        calibration.info$n.burn = n.burn
    if (!is.null(solver.metadata))
        calibration.info$solver.metadata = solver.metadata
        
     CALIBRATION.MANAGER$run.info[[to.code]] = calibration.info
    
        
    
    invisible(NULL)
}


get.calibration.info <- function(code, throw.error.if.missing=T, error.prefix='')
{
    rv = CALIBRATION.MANAGER$run.info[[code]]
    if (is.null(rv) && throw.error.if.missing)
        stop(paste0(error.prefix, "No calibration info has been registered under code '", code, "'"))
    
    rv
}


prepare.mcmc.summary <- function(version,
                                 location,
                                 calibration.code,
                                 root.dir,
#                                 parameter.scales,
                                 get.one.set.of.parameters = T,
                                 burn.fraction = 0.75,
                                 error.prefix = '',
                                 throw.error.if.incomplete = T)
{
    dir = file.path(get.calibration.dir(version=version,
                                        location=location,
                                        calibration.code=calibration.code,
                                        root.dir=root.dir),
                    'cache')
 
    error.prefix = paste0(error.prefix, 
                          "Cannot prepare a summary of the mcmc for calibration '", calibration.code, " for ' and version '", version, "' and location '", location, "' - ")   
    if (!file.exists(dir))
        stop(paste0(error.prefix,
                    "No prior MCMC cache has been set up in the root directory (", root.dir, "')"))
    
    files = list.files(dir)
    chain.control.files = files[grepl('chain[0-9]+_control', files)]
    
    x = load(file.path(dir, 'global_control.Rdata'))
    global.control = get(x)
    
    if (length(chain.control.files)==0)
        stop(paste0(error.prefix,
                    "The prior MCMC cache is malformed"))
    
    chain.controls = lapply(file.path(dir, chain.control.files), function(file){
         x = load(file)
         get(x)
    })
    
    chain.done = sapply(chain.controls, function(ctrl){
        all(ctrl@chunk.done)
    })
    
    if (any(!chain.done))
    {
        if (throw.error.if.incomplete)
        {
            stop(paste0(error.prefix,
                        "The prior MCMC has not finished running"))
        }
        else
            return (NULL)
    }
    
    state1 = chain.controls[[1]]@chain.state
    
    #-- Prepare the covariance matrix --#
    all.cov.mats = sapply(chain.controls, function(ctrl){
        ctrl@chain.state@cov.mat
    })
    
    cov.mat = rowMeans(all.cov.mats)
    dim(cov.mat) = dim(state1@cov.mat)
    dimnames(cov.mat) = dimnames(state1@cov.mat)
    
    
    #-- Prepare the parameter values --#
    
    sample.mean = NULL
    sample.cov = NULL
    n.samples = NULL

    samples = NULL
    
    # Get the last set of parameters sampled
    last.chunk.file = file.path(dir, "chain_1", paste0("chain1_chunk", global.control@n.chunks, ".Rdata"))
    if (!file.exists(last.chunk.file))
        stop("Cannot prepare MCMC summary - we have not finished the sampling for the prior calibration")
    load(last.chunk.file)
    last.sim.parameters = mcmc@simulations[[length(mcmc@simulations)]]$params
    
    
    if (!get.one.set.of.parameters)
    {
        # Figure out which chunks we are going to use
        chunks.to.use = (1:global.control@n.chunks)[(1:global.control@n.chunks)/global.control@n.chunks>burn.fraction]
        hypothetical.chunk.files = unlist(lapply(1:global.control@n.chains, function(chain){
            file.path(paste0("chain_", chain),
                      paste0("chain", chain, "_chunk", chunks.to.use, ".Rdata"))
        }))
        chunk.files = hypothetical.chunk.files[sapply(file.path(dir, hypothetical.chunk.files), file.exists)]
        
        if (length(chunk.files)/length(hypothetical.chunk.files) < 1)
            stop("Cannot prepare MCMC summary - we have not finished the sampling for the prior calibration")
        
        # Iterate through each chunk and use to calculate sample mean and covariance    
        for (chunk.file in chunk.files)
        {
            # Calculate the mean and covariance of the new samples
            new.samples = mcmc@samples
            n.new.samples = prod(dim(new.samples)) / dim(new.samples)['variable']
            
            dim.names = list(NULL, variable=dimnames(new.samples)$variable)
            dim(new.samples) = c(n.new.samples, variable=length(new.samples)/n.new.samples)
            dimnames(new.samples) = dim.names
            
            samples = rbind(samples, new.samples)
            
            # new.samples = sapply(dimnames(new.samples)$variable, function(var.name){
            #     values = new.samples[,var.name]
            #     scale = parameter.scales[var.name]
            #     if (scale=='identity')
            #         values
            #     else if (scale=='log')
            #         log(values)
            #     else if (scale=='logit')
            #         log(values) - log(1-values)
            #     else
            #         stop(paste0("Don't know what to do with scale = '", scale, "'"))
            # })
            # 
            # mean.new.samples = colMeans(new.samples)
            # cov.new.samples = cov(new.samples)
            # 
            # # Incorporate into the overall mean and cov mat
            # if (is.null(sample.mean))
            # {
            #     sample.mean = mean.new.samples
            #     sample.cov = cov.new.samples
            #     n.samples = n.new.samples
            # }
            # else
            # {
            #     old.n = n.samples
            #     old.mean = sample.mean
            #     old.cov = sample.cov
            #     n.samples = old.n + n.new.samples
            #     
            #     sample.mean = old.n/n.samples * old.mean + n.new.samples/n.samples * mean.new.samples
            #     sample.cov = 1/(n.samples-1) *
            #         ( (old.n-1) * old.cov + old.n * old.mean %*% t(old.mean) +
            #           (n.new.samples-1) * cov.new.samples + n.new.samples * mean.new.samples %*% t(mean.new.samples) - 
            #           n.samples * sample.mean %*% t(sample.mean) )
            # }
        }
    }
    
    # all.transformed.parameter.values = sapply(chain.controls, function(ctrl){
    #     ctrl@chain.state@mean.transformed.parameters
    # })
    # 
    # transformed.parameter.values = rowMeans(all.transformed.parameter.values)
    
    #-- Initial Scaling Steps --#
    # Andrew says lapply would be safer here since we're expecting a list anyway
    initial.scaling.parameters = lapply(1:length(state1@log.scaling.parameters), function(i){
        all.chain.log.values = sapply(chain.controls, function(ctrl){
            ctrl@chain.state@log.scaling.parameters[[i]]
        })
        
        dim(all.chain.log.values) = c(length(all.chain.log.values) / length(chain.controls),
                                      length(chain.controls))
        
        rv = exp(rowMeans(all.chain.log.values))
        names(rv) = names(chain.controls[[1]]@chain.state@log.scaling.parameters[[i]])
        rv
    })
    names(initial.scaling.parameters) = names(global.control@control@var.blocks)
    
    
    #-- Return --#
    list(
        cov.mat = cov.mat,
        initial.scaling.parameters = initial.scaling.parameters,
        last.sim.parameters = last.sim.parameters,
        
        # transformed.parameter.values = transformed.parameter.values,
        # parameter.names = names(transformed.parameter.values),
        # sample.mean = sample.mean,
        # sample.cov = sample.cov,
        samples = samples
    )
}

#' Get Percentage of Run Cache Completed
#' @return Matrix of percentages shape [chain x location]
#' @export
percentage.cache.complete <- function(version,
                                      calibration.code,
                                      locations,
                                      chains) {
    
    all.percentages = lapply(locations, function(location) {
        percentages.this.location = sapply(chains, function(chain) {
            obj.path = file.path("../../files/mcmc_runs", version, location, calibration.code, "cache", paste0("chain", chain, "_control.Rdata"))
            if (!file.exists(obj.path)) return(NA)
            chain.control.obj = get(load(obj.path))
            mean(chain.control.obj@chunk.done)
        })
    })
    names(all.percentages) = locations
    percentage.matrix = do.call(cbind, all.percentages)
    colnames(percentage.matrix) = locations
    rownames(percentage.matrix) = chains
    percentage.matrix
}

##-------------##
##-- HELPERS --##
##-------------##





get.distribution.variable.transformation = function(dist, unknown.value)
{
    if (is(dist, "Joint_Independent_Distributions"))
        rv = unlist(lapply(dist@subdistributions, 
                      get.distribution.variable.transformation, 
                      unknown.value = unknown.value))
    else if (is.null(dist@transformation))
        rv = rep(unknown.value, dist@n.var)
    else
        rv = rep(dist@transformation@name, dist@n.var)
    
    names(rv) = dist@var.names
    rv
}
