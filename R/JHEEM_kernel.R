
#-- A JHEEM.KERNEL is all the data from a specification object necessary to run one engine in a location
#--  as well as to run interventions on a simulation
#-- These get saved independently of the specification code, and can live on in simulation objects

# Internal to the package
create.jheem.kernel <- function(version, location)
{
    specification = get.compiled.specification.for.version(version)
    specification$get.jheem.kernel(location = location)
}

JHEEM.KERNEL = R6::R6Class(
    'jheem.kernel',
    portable = F,
    
    public = list(
        
        initialize = function(version,
                              location,
                              
                              specification,
                              
                              dependent.quantity.names,
                              dependee.element.names,
                              dependee.quantity.names,
                              dependent.top.level.quantity.names,
                              non.cumulative.dependent.outcome.names,
                              outcome.dependee.element.names,
                              outcome.dependee.quantity.names,
                              outcome.non.cumulative.dependendee.outcome.names,
                              outcome.non.cumulative.direct.dependendee.outcome.names,
                              outcome.non.cumulative.dependent.outcome.names,
                              outcome.direct.dependee.outcome.names,
                              outcome.numerator.direct.dependee.outcome.names,
                              direct.dependent.outcome.numerator.names,
                              outcome.direct.dependee.quantity.names)
        {
            error.prefix = paste0("Error creating '", version, "' specification kernel for '", location, "': ")
            
            # We're not going to error check these
            private$i.version = version
            private$i.location = location
            private$i.created.date = Sys.Date()
            private$i.created.time = Sys.time()
            
            #-- Some simple pulls from the specification --#
            private$i.sub.versions = specification$sub.versions
            private$i.labels = specification$labels
            private$i.order.locations.by.outcomes = specification$order.locations.by.outcomes
            
            #-- Get the specification metadata --#
            private$i.specification.metadata = do.get.specification.metadata(specification=specification,
                                                                          location=location,
                                                                          error.prefix=error.prefix)

            #-- Dependency data structures --#
            private$i.dependent.quantity.names = dependent.quantity.names
            private$i.dependee.element.names = dependee.element.names
            private$i.dependee.quantity.names = dependee.quantity.names
            private$i.dependent.top.level.quantity.names = dependent.top.level.quantity.names
            private$i.non.cumulative.dependent.outcome.names = non.cumulative.dependent.outcome.names
            private$i.outcome.dependee.element.names = outcome.dependee.element.names
            private$i.outcome.dependee.quantity.names = outcome.dependee.quantity.names
            private$i.outcome.non.cumulative.dependendee.outcome.names = outcome.non.cumulative.dependendee.outcome.names
            private$i.outcome.non.cumulative.direct.dependendee.outcome.names = outcome.non.cumulative.direct.dependendee.outcome.names
            private$i.outcome.non.cumulative.dependent.outcome.names = outcome.non.cumulative.dependent.outcome.names
            private$i.outcome.direct.dependee.outcome.names = outcome.direct.dependee.outcome.names
            private$i.outcome.numerator.direct.dependee.outcome.names = outcome.numerator.direct.dependee.outcome.names
            private$i.direct.dependent.outcome.numerator.names = direct.dependent.outcome.numerator.names
            private$i.outcome.direct.dependee.quantity.names = outcome.direct.dependee.quantity.names
            
            #-- Quantity Stuff --#
            private$i.ordered.quantity.names = specification$ordered.quantity.names
            private$i.ordered.quantity.names.except.initial.population = specification$ordered.quantity.names.except.initial.population
            
            private$i.top.level.quantity.names = specification$top.level.quantity.names
            private$i.top.level.quantity.names.except.initial.population = specification$top.level.quantity.names.except.initial.population
            
            
            private$i.quantity.kernels = lapply(specification$quantities, function(quantity){
                
                quantity.kernel = private$copy.r6.to.list(quantity, light=T)
                
                quantity.kernel$original.name = quantity$get.original.name(private$i.version)
                quantity.kernel$max.dim.names = private$i.specification.metadata$apply.aliases(quantity$max.dim.names, error.prefix=error.prefix)
                quantity.kernel$required.dim.names = private$i.specification.metadata$apply.aliases(quantity$required.dim.names, error.prefix = error.prefix)

                quantity.kernel$components = lapply(quantity$components, function(comp){
                    
                    comp.kernel = private$copy.r6.to.list(comp, light=T)
                    
                    comp.kernel$max.dim.names = private$i.specification.metadata$apply.aliases(comp$max.dim.names, error.prefix=error.prefix)
                    comp.kernel$applies.to = private$i.specification.metadata$apply.aliases(comp$applies.to, error.prefix=error.prefix)
                    
                    if (comp$value.type == 'function')
                        comp.kernel$value.function.name = comp$value$value.function.name

                    if (comp$value.type == 'numeric')
                        comp.kernel$value.dim.names = dimnames(comp$value)
                    
                    comp.kernel$evaluate = comp$get.evaluate.function(parent.environment = self,
                                                                      error.prefix = error.prefix)
                    comp.kernel
                })
                
                quantity.kernel
                
                # list(
                #     name = quantity$name,
                #     depends.on = quantity$depends.on,
                #     
                #     original.name = quantity$get.original.name(private$i.version),
                #     max.dim.names = private$i.specification.metadata$apply.aliases(quantity$max.dim.names, error.prefix=error.prefix),
                #     required.dim.names = private$i.specification.metadata$apply.aliases(quantity$required.dim.names, error.prefix = error.prefix),
                #     
                #     functional.form.scale = quantity$functional.form.scale,
                #     scale = quantity$scale,
                #     
                #     ramp.value.application = quantity$ramp.value.application,
                #     all.ramp.applications.identity = quantity$all.ramp.applications.identity,
                #     ramp.interpolate.links = quantity$ramp.interpolate.links,
                #     
                #     taper.value.application = quantity$taper.value.application,
                #     all.taper.applications.identity = quantity$all.taper.applications.identity,
                #     taper.interpolate.links = quantity$taper.interpolate.links,
                #     
                #     n.components = quantity$n.components,
                #     components = lapply(quantity$components, function(comp){
                #         
                #         kernel = list(
                #             max.dim.names = comp$max.dim.names,
                #             applies.to = comp$applies.to,
                #             value.type = comp$value.type,
                #             depends.on = comp$depends.on,
                #             apply.function = comp$apply.function,
                #             reversed.dimension.alias.mapping = comp$reversed.dimension.alias.mapping,
                #             reversed.aliases = comp$reversed.aliases,
                #             
                #             evaluate = comp$get.evaluate.function(parent.environment = self,
                #                                                   error.prefix = error.prefix)
                #         )
                #         
                #         if (comp$value.type == 'function')
                #             kernel$value.function.name = comp$value$value.function.name
                #         
                #         if (comp$value.type == 'numeric')
                #             kernel$value.dim.names = dimnames(comp$value)
                #         
                #         kernel
                #     })
                #  )
                
            })
            
            private$i.element.backgrounds = lapply(specification$element.names, function(elem.name){
                elem = specification$get.quantity(elem.name)
                elem$get.element.background(specification.metadata = private$i.specification.metadata,
                                            error.prefix = paste0("Error creating specification kernel for version '", private$i.version, "' and location '", private$i.location, "': "))
            })
            names(private$i.element.backgrounds) = private$i.element.names = specification$element.names
            private$i.element.name.mappings = specification$element.name.mappings
            
            #-- Outcome Stuff --#
            private$i.outcome.kernels = lapply(specification$outcome.names, function(outcome.name){
                outcome = specification$get.outcome(outcome.name)
                
                outcome.kernel = private$copy.r6.to.list(outcome, light=T)
                
                outcome.kernel$ontology = private$i.specification.metadata$apply.aliases(outcome$ontology, error.prefix = error.prefix)
                outcome.kernel$dim.names = private$i.specification.metadata$apply.aliases(outcome$dim.names, error.prefix = error.prefix)
                outcome.kernel$unrenamed.dim.names = private$i.specification.metadata$apply.aliases(outcome$unrenamed.dim.names, error.prefix = error.prefix)
                
                outcome.kernel$subset.dimension.values = private$i.specification.metadata$apply.aliases(outcome$subset.dimension.values, error.prefix = error.prefix)
                
                outcome.kernel$calculate.values = outcome$get.calculate.values.function(parent.environment = self,
                                                                                        error.prefix = error.prefix)
                
                outcome.kernel$metadata = outcome$metadata
                
                outcome.kernel
                # 
                # list(
                #     name = outcome$name,
                #     
                #     is.intrinsic = outcome$is.intrinsic,
                #     is.dynamic = outcome$is.dynamic,
                #     is.cumulative = outcome$is.cumulative,
                #     
                #     from.year = outcome$from.year,
                #     to.year = outcome$to.year,
                #     
                #     dim.names = private$i.specification.metadata$apply.aliases(outcome$dim.names, error.prefix = error.prefix),
                #     unrenamed.dim.names = outcome$unrenamed.dim.names,
                #     
                #     depends.on = outcome$depends.on,
                #     
                #     denominator.outcome = outcome$denominator.outcome,
                #     value.is.numerator = outcome$value.is.numerator,
                #     subset.dimension.values = outcome$subset.dimension.values,
                #     rename.dimension.values = outcome$rename.dimension.values,
                #     
                #     dimension.aliases = outcome$dimension.aliases,
                #     dimension.alias.suffix = outcome$dimension.alias.suffix,
                #     
                #     # For diffeq settings
                #     dynamic.quantity.name = outcome$dynamic.quantity.name,
                #     multiply.by = outcome$multiply.by,
                #     groups = outcome$groups,
                #     trackable.type = outcome$trackable.type,
                #     
                #     # The calculate function
                #     calculate.values = outcome$get.calculate.values.function(parent.environment = self,
                #                                                              error.prefix = error.prefix)
                # )
            })
            names(private$i.outcome.kernels) = specification$outcome.names
            
            
            private$i.outcome.names.for.null.sub.version = specification$get.outcome.names.for.sub.version(NULL)
            private$i.outcome.names.for.sub.version = lapply(specification$sub.versions, specification$get.outcome.names.for.sub.version)
            names(private$i.outcome.names.for.sub.version) = specification$sub.versions
            
            private$i.outcome.sub.version.details = specification$outcome.sub.version.details
            
            
            #-- Foregrounds --#
            private$i.foregrounds = specification$foregrounds
            
            #-- Parameter Mapping --#
            calibrated.parameter.names = get.parameter.names.for.version(private$i.version, type='calibrated')
            if (!is.null(calibrated.parameter.names))
            {
                private$i.calibrated.parameter.names = calibrated.parameter.names
                private$i.calibrated.parameters.distribution = get.parameters.distribution.for.version(private$i.version, type='calibrated')
                calibrated.parameters.apply.fn = get.parameters.apply.function.for.version(private$i.version, type='calibrated')
                private$i.calibrated.parameters.apply.function = bundle.function.and.dependees(calibrated.parameters.apply.fn,
                                                                                               parent.environment = self,
                                                                                         fn.name.for.error = "The calibrated parameters apply function",
                                                                                         error.prefix = error.prefix)
            }
                      
            sampled.parameter.names = get.parameter.names.for.version(private$i.version, type='sampled')
            if (!is.null(sampled.parameter.names))
            {
                private$i.sampled.parameter.names = sampled.parameter.names
                private$i.sampled.parameters.distribution = get.parameters.distribution.for.version(private$i.version, type='sampled')
                sampled.parameters.apply.fn = get.parameters.apply.function.for.version(private$i.version, type='sampled')
                private$i.sampled.parameters.apply.function = bundle.function.and.dependees(sampled.parameters.apply.fn,
                                                                                            parent.environment = self,
                                                                                      fn.name.for.error = "The sampled parameters apply function",
                                                                                      error.prefix = error.prefix)
            }
            
            set.parameter.names = get.parameter.names.for.version(private$i.version, type='set')
            if (!is.null(set.parameter.names))
            {
                private$i.set.parameter.names = set.parameter.names
                set.parameters.apply.fn = get.parameters.apply.function.for.version(private$i.version, type='set')
                private$i.set.parameters.apply.function = bundle.function.and.dependees(set.parameters.apply.fn,
                                                                                        parent.environment = self,
                                                                                        fn.name.for.error = "The set parameters apply function",
                                                                                        error.prefix = error.prefix)
            }
            
            #-- Ontologies --#
            
            private$i.ontologies = lapply(specification$ontologies, 
                                           private$i.specification.metadata$apply.aliases,
                                           error.prefix=error.prefix)
            
            #-- Core Components --#
            
            dynamic.outcome.names = names(private$i.outcome.kernels)[vapply(private$i.outcome.kernels, function(outcome){outcome$is.dynamic}, FUN.VALUE = logical(1))]
            if (is.null(dynamic.outcome.names)) # when there are no outcome kernels at all
                dynamic.outcome.names = character(0)
            dynamic.outcomes = sapply(dynamic.outcome.names, specification$get.outcome)
            
            sorted.components = compile.and.sort.core.components.for.location(specification = specification,
                                                                              specification.metadata = private$i.specification.metadata,
                                                                              ontologies = private$i.ontologies,
                                                                              error.prefix = error.prefix)
            
            private$i.core.components = lapply(sorted.components, function(comps){
                lapply(comps, function(comp){
                    
                    # Pull mechanism types
                    comp$mechanism.types = comp$schema$mechanism.types
                    
                    # Figure out which dynamic outcomes apply
                    outcome.applies.to.comp = vapply(dynamic.outcomes, comp$schema$dynamic.tracker.involves.component, FUN.VALUE = logical(1), comp=comp)
                    comp$outcome.names.that.apply = dynamic.outcome.names[outcome.applies.to.comp]
                    
                    # Get rid of the schema so we don't inadvertently save its environment
                    comp$schema = NULL
                    
                    comp
                })
            })
            
            #-- Diffeq Settings --#
            
            private$i.fixed.strata.info = lapply(specification$fixed.strata.info,
                                                 private$copy.r6.to.list, 
                                                 light=T)
            
            #-- Misc --#
            private$i.default.parameter.values = specification$default.parameter.values
            private$i.start.year = specification$start.year
            private$i.default.solver.metadata = specification$default.solver.metadata
            
            # for (elem.name in names(private))
            # {
            #     print(elem.name)
            #     x = private[[elem.name]]
            #     save(x, file=paste0("size_check/", elem.name, ".Rdata"))
            # }
            # env = environment(private$i.calibrated.parameters.apply.function)
            # save(env, file = "size_check/env.Rdata")
             # save(self, file = 'size_check/kernel.Rdata')
             # stop('stop here')
            # 
            # browser()
        },
        
        get.quantity.kernel = function(quantity.name)
        {
            private$i.quantity.kernels[[quantity.name]]
        },
        
        get.outcome.kernel = function(outcome.name)
        {
            private$i.outcome.kernels[[outcome.name]]  
        },
        
        get.outcome.sub.version.details = function(outcome.name, sub.version)
        {
            private$i.outcome.sub.version.details[[sub.version]][[outcome.name]]
        },
        
        # Get dependency functions
        get.dependent.quantity.names = function(quantity.names)
        {
            unlist(private$i.dependent.quantity.names[quantity.names])
        },
        
        get.dependee.element.names = function(quantity.names)
        {
            unlist(private$i.dependee.element.names[quantity.names])
        },
        
        get.dependee.quantity.names = function(quantity.names)
        {
            unlist(private$i.dependee.quantity.names[quantity.names])
        },
        
        get.dependent.top.level.quantity.names = function(quantity.names)
        {
            unlist(private$i.dependent.top.level.quantity.names[quantity.names])
        },
        
        get.non.cumulative.dependent.outcome.names = function(quantity.names)
        {
            unlist(private$i.non.cumulative.dependent.outcome.names[quantity.names])
        },
        
        get.outcome.dependee.element.names = function(outcome.names)
        {
            unlist(private$i.outcome.dependee.element.names[outcome.names])
        },
        
        get.outcome.dependee.quantity.names = function(outcome.names)
        {
            unlist(private$i.outcome.dependee.quantity.names[outcome.names])
        },
        
        get.outcome.non.cumulative.dependendee.outcome.names = function(outcome.names)
        {
            unlist(private$i.outcome.non.cumulative.dependendee.outcome.names[outcome.names])
        },
        
        get.outcome.non.cumulative.direct.dependendee.outcome.names = function(outcome.names)
        {
            unlist(private$i.outcome.non.cumulative.direct.dependendee.outcome.names[outcome.names])
        },
        
        get.outcome.non.cumulative.dependent.outcome.names = function(outcome.names)
        {
            unlist(private$i.outcome.non.cumulative.dependent.outcome.names[outcome.names])
        },
        
        get.outcome.direct.dependee.outcome.names = function(outcome.names)
        {
            unlist(private$i.outcome.direct.dependee.outcome.names[outcome.names])
        },
        
        get.outcome.numerator.direct.dependee.outcome.names = function(outcome.names)
        {
            unlist(private$i.outcome.numerator.direct.dependee.outcome.names[outcome.names])
        },
        
        get.direct.dependent.outcome.numerator.names = function(quantity.names)
        {
            unlist(private$i.direct.dependent.outcome.numerator.names[quantity.names])
        },
        
        get.outcome.direct.dependee.quantity.names = function(outcome.names)
        {
            unlist(private$i.outcome.direct.dependee.quantity.names[outcome.names])
        },
        
        get.outcome.names.for.sub.version = function(sub.version)
        {
            if (is.null(sub.version))
                private$i.outcome.names.for.null.sub.version
            else
                private$i.outcome.names.for.sub.version[[sub.version]]
        }
    ),
    
    active = list(
        
        version = function(value)
        {
            if (missing(value))
                private$i.version
            else
                stop("Cannot modify a specification kernel's version - it is read-only")
        },
        
        location = function(value)
        {
            if (missing(value))
                private$i.location
            else
                stop("Cannot modify a specification kernel's location - it is read-only")
        },
        
        sub.versions = function(value)
        {
            if (missing(value))
                private$i.sub.versions
            else
                stop("Cannot modify a specification kernel's sub.versions - they are read-only")
        },
        
        outcome.sub.version.details = function(value)
        {
            if (missing(value))
                private$i.outcome.sub.version.details
            else
                stop("Cannot modify a specification kernel's outcome.sub.version.details - they are read-only")
        },
        
        specification.metadata = function(value)
        {
            if (missing(value))
                private$i.specification.metadata
            else
                stop("Cannot modify a specification kernel's specification.metadata - it is read-only")
        },
        
        ordered.quantity.names = function(value)
        {
            if (missing(value))
                private$i.ordered.quantity.names
            else
                stop("Cannot modify a specification kernel's ordered.quantity.names - they are read-only")
        },
        
        ordered.quantity.names.except.initial.population = function(value)
        {
            if (missing(value))
                private$i.ordered.quantity.names.except.initial.population
            else
                stop("Cannot modify a specification kernel's ordered.quantity.names.except.initial.population - they are read-only")
        },
        
        top.level.quantity.names = function(value)
        {
            if (missing(value))
                private$i.top.level.quantity.names
            else
                stop("Cannot modify a specification kernel's top.level.quantity.names - they are read-only")
        },
        
        top.level.quantity.names.except.initial.population = function(value)
        {
            if (missing(value))
                private$i.top.level.quantity.names.except.initial.population
            else
                stop("Cannot modify a specification kernel's top.level.quantity.names.except.initial.population - they are read-only")
        },
        
        quantity.kernels = function(value)
        {
            if (missing(value))
                private$i.quantity.kernels
            else
                stop("Cannot modify a specification kernel's quantity.kernels - they are read-only")
        },
        
        quantity.names = function(value)
        {
            if (missing(value))
                names(private$i.quantity.kernels)
            else
                stop("Cannot modify a specification kernel's element.names - they are read-only")
        },
        
        element.names = function(value)
        {
            if (missing(value))
                private$i.element.names
            else
                stop("Cannot modify a specification kernel's element.names - they are read-only")
        },
        
        element.backgrounds = function(value)
        {
            if (missing(value))
                private$i.element.backgrounds
            else
                stop("Cannot modify a specification kernel's element.backgrounds - they are read-only")
        },
        
        element.name.mappings = function(value)
        {
            if (missing(value))
                private$i.element.name.mappings
            else
                stop("Cannot modify a specification kernel's element.name.mappings - they are read-only")
        },
        
        outcome.kernels = function(value)
        {
            if (missing(value))
                private$i.outcome.kernels
            else
                stop("Cannot modify a specification kernel's outcomes - they are read-only")
        },
        
        outcome.names = function(value)
        {
            if (missing(value))
                names(private$i.outcome.kernels)
            else
                stop("Cannot modify a specification kernel's outcome.names - they are read-only")
        },
        
        default.parameter.values = function(value)
        {
            if (missing(value))
                names(private$i.default.parameter.values)
            else
                stop("Cannot modify a specification kernel's default.parameter.values - they are read-only")
        },
        
        foregrounds = function(value)
        {
            if (missing(value))
                names(private$i.foregrounds)
            else
                stop("Cannot modify a specification kernel's foregrounds - they are read-only")
        },
        
        calibrated.parameter.names = function(value)
        {
            if (missing(value))
                private$i.calibrated.parameter.names
            else
                stop("Cannot modify a specification kernel's calibrated.parameter.names - they are read-only")
        },
        
        sampled.parameter.names = function(value)
        {
            if (missing(value))
                private$i.sampled.parameter.names
            else
                stop("Cannot modify a specification kernel's sampled.parameter.names - they are read-only")
        },
        
        set.parameter.names = function(value)
        {
            if (missing(value))
                private$i.set.parameter.names
            else
                stop("Cannot modify a specification kernel's set.parameter.names - they are read-only")
        },
        
        calibrated.parameters.distribution = function(value)
        {
            if (missing(value))
                private$i.calibrated.parameters.distribution
            else
                stop("Cannot modify a specification kernel's calibrated.parameters.distribution - they are read-only")
        },
        
        sampled.parameters.distribution = function(value)
        {
            if (missing(value))
                private$i.sampled.parameters.distribution
            else
                stop("Cannot modify a specification kernel's sampled.parameters.distribution - they are read-only")
        },
        
        calibrated.parameters.apply.function = function(value)
        {
            if (missing(value))
                private$i.calibrated.parameters.apply.function
            else
                stop("Cannot modify a specification kernel's calibrated.parameters.apply.function - they are read-only")
        },
        
        sampled.parameters.apply.function = function(value)
        {
            if (missing(value))
                private$i.sampled.parameters.apply.function
            else
                stop("Cannot modify a specification kernel's sampled.parameters.apply.function - they are read-only")
        },
        
        set.parameters.apply.function = function(value)
        {
            if (missing(value))
                private$i.set.parameters.apply.function
            else
                stop("Cannot modify a specification kernel's set.parameters.apply.function - they are read-only")
        },
        
        ontologies = function(value)
        {
            if (missing(value))
                private$i.ontologies
            else
                stop("Cannot modify a specification kernel's ontologies - they are read-only")
        },
        
        core.components = function(value)
        {
            if (missing(value))
                private$i.core.components
            else
                stop("Cannot modify a specification kernel's core.components - they are read-only")
        },
        
        fixed.strata.info = function(value)
        {
            if (missing(value))
                private$i.fixed.strata.info
            else
                stop("Cannot modify a specification kernel's fixed.strata.info - they are read-only")
        },
        
        aliases = function(value)
        {
            if (missing(value))
                private$i.aliases
            else
                stop("Cannot modify a specification kernel's aliases - they are read-only")
        },
        dim.names = function(value)
        {
            if (missing(value))
                private$i.dim.names
            else
                stop("Cannot modify a specification kernel's dim.names - they are read-only")
        },
        
        dimensions = function(value)
        {
            if (missing(value))
                names(private$i.dim.names)
            else
                stop("Cannot modify a specification kernel's dimensions - they are read-only")
        },
        
        n.ages = function(value)
        {
            if (missing(value))
                length(private$i.age.upper.bounds)
            else
                stop("Cannot modify a specification kernel's n.ages - it is read-only") 
        },
        
        age.spans = function(value)
        {
            if (missing(value))
                private$i.age.upper.bounds - private$i.age.lower.bounds
            else
                stop("Cannot modify a specification kernel's age.spans - they are read-only")
        },
        
        age.lower.bounds = function(value)
        {
            if (missing(value))
                private$i.age.lower.bounds
            else
                stop("Cannot modify a specification kernel's age.lower.bounds - they are read-only")
        },
        
        age.upper.bounds = function(value)
        {
            if (missing(value))
                private$i.age.upper.bounds
            else
                stop("Cannot modify a specification kernel's age.upper.bounds - they are read-only")
        },
        
        age.endpoints = function(value)
        {
            if (missing(value))
                private$i.age.endpoints
            else
                stop("Cannot modify a specification kernel's age.endpoints - they are read-only")
        },
        
        labels = function(value)
        {
            if (missing(value))
                private$i.labels
            else
                stop("Cannot modify a specification kernel's labels - they are read-only")
        },
        
        start.year = function(value)
        {
            if (missing(value))
                private$i.start.year
            else
                stop("Cannot modify a specification kernel's start.year - it is read-only")
        },
        
        default.solver.metadata = function(value)
        {
            if (missing(value))
                private$i.default.solver.metadata
            else
                stop("Cannot modify a specification kernel's default.solver.metadata - it is read-only")
        },
        
        order.locations.by.outcomes = function(value)
        {
            if (missing(value))
                private$i.order.locations.by.outcomes
            else
                stop("Cannot modify a specification kernel's order.locations.by.outcomes - it is read-only")
        }
    ),
    
    private = list(
        
        #-- General Stuff --#
        i.version = NULL,
        i.location = NULL,
        i.created.date = NULL,
        i.created.time = NULL,
        
        i.sub.versions = NULL,
        
        i.dim.names = NULL,
        i.specification.metadata = NULL,
        i.labels = NULL,
        
        #-- Aliases --#
        i.aliases = NULL,
        i.categorized.aliases = NULL,
        
        #-- Age Stuff --#
        i.age.lower.bounds = NULL,
        i.age.upper.bounds = NULL,
        i.age.endpoints = NULL,
        
        #-- Quantity Stuff --#
        i.ordered.quantity.names = NULL,
        i.ordered.quantity.names.except.initial.population = NULL,
        
        i.top.level.quantity.names = NULL,
        i.top.level.quantity.names.except.initial.population = NULL,
        
        i.quantity.kernels = NULL,
        i.element.names = NULL,
        i.element.backgrounds = NULL,
        i.element.name.mappings = NULL,
        
        #-- Foregrounds --#
        i.foregrounds = NULL,
        
        #-- Outcome Stuff --#
        i.outcome.kernels = NULL,
        i.outcome.sub.version.details = NULL,
        
        i.outcome.names.for.sub.version = NULL,
        i.outcome.names.for.null.sub.version = NULL,
        
        #-- Ontologies --#
        i.ontologies = NULL,
        
        #-- Core Components --#
        i.core.components = NULL,
        
        #-- Parameter Mappings --#
        i.calibrated.parameter.names = NULL,
        i.sampled.parameter.names = NULL,
        i.set.parameter.names = NULL,
        
        i.calibrated.parameters.distribution = NULL,
        i.sampled.parameters.distribution = NULL,
        
        i.calibrated.parameters.apply.function = NULL,
        i.sampled.parameters.apply.function = NULL,
        i.set.parameters.apply.function = NULL,
        
        #-- Misc --#
        i.default.parameter.values = NULL,
        i.start.year = NULL,
        i.default.solver.metadata = NULL,
        i.order.locations.by.outcomes = NULL,
        
        #-- Dependency data structures --#
        i.dependent.quantity.names = NULL,
        i.dependee.element.names = NULL,
        i.dependee.quantity.names = NULL,
        i.dependent.top.level.quantity.names = NULL,
        i.non.cumulative.dependent.outcome.names = NULL,
        i.outcome.dependee.element.names = NULL,
        i.outcome.dependee.quantity.names = NULL,
        i.outcome.non.cumulative.dependendee.outcome.names = NULL,
        i.outcome.non.cumulative.direct.dependendee.outcome.names = NULL,
        i.outcome.non.cumulative.dependent.outcome.names = NULL,
        i.outcome.direct.dependee.outcome.names = NULL,
        i.outcome.numerator.direct.dependee.outcome.names = NULL,
        i.direct.dependent.outcome.numerator.names = NULL,
        i.outcome.direct.dependee.quantity.names = NULL,
        
        #-- Fixed Strata Info --#
        i.fixed.strata.info = NULL,

        
        #-- Diffeq Settings Helpers --#
        
        #-- Helper Function --#
        copy.r6.to.list = function(obj, light=T)
        {
            obj.names = names(obj)
            
            rv = lapply(obj.names, function(name){
                obj[[name]]
            })
            names(rv) = obj.names
            
            if (light)
                rv = rv[!sapply(rv, private$contains.environment.or.function)]
            
            rv
        },
        
        contains.environment.or.function = function(elem)
        {
            is.environment(elem) || is.function(elem) ||
                (is.list(elem) && any(vapply(elem, private$contains.environment.or.function, FUN.VALUE = logical(1))))
        }
    )
)