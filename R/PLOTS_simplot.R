
#' @title Plot Simulations And Data
#'@param ... One or more jheem.simulation.set objects and at most one character vector of outcomes (as an alternative to the 'outcomes' argument)
#'@param corresponding.data.outcomes Specify directly which data outcomes should be plotted against simulation outcomes. Must be NULL or a character vector with outcomes as names; all of those outcomes must be present in either the 'outcomes' argument or in '...'"
#'@param outcomes A character vector of which simulation outcomes to plot
#'@param split.by At most one dimension
#'@param facet.by Any number of dimensions but cannot include the split.by dimension
#'@param dimension.values.post.mapping How to subset data in the form it will take after mappings have been applied to align sim and data ontologies. A list.
#'@param plot.which Should simulation data and calibration data be plotted ('sim.and.data'), or only simulation data ('sim.only')
#'@param label.function A function to reformat labels. If NULL, will use the first provided simset's "get.labels" property, or will do no transformation if plotting data only.
#'@param title NULL or a single, non-NA character value. If "location", the location of the first provided simset (if any) will be used for the title.
#'@param title.suffix NULL or a single, non-NA character value. Something to append to the end of the title that would be generated automatically based on the 'title' argument. Note that this pastes with no space, so add the space in if you want it.
#'@param data.manager The data.manager from which to draw real-world data for the plots
#'@param style.manager We are going to have to define this down the road. It's going to govern how we do lines and sizes and colors. For now, just hard code those in, and we'll circle back to it
#'@param show.data.pull.error Not finding data does not block the plot from showing simulation projections, but if you'd like to see in error when data doesn't appear, set this to TRUE.
#'
#'@details Returns a ggplot object:
#'  - With one panel for each combination of outcome x facet.by
#'  - x-axis is year
#'  - y-axis is outcome
#'
#'@export
simplot <- function(...,
                    outcomes=NULL,
                    corresponding.data.outcomes = NULL,
                    split.by = NULL,
                    facet.by = NULL,
                    dimension.values = list(),
                    dimension.values.post.mapping = list(),
                    target.ontology = NULL,
                    plot.which = c('sim.and.data', 'sim.only')[1],
                    summary.type = c('individual.simulation', 'mean.and.interval', 'median.and.interval')[1],
                    label.function = NULL,
                    plot.year.lag.ratio = F,
                    title = "location",
                    title.suffix = NULL,
                    n.facet.rows = NULL,
                    append.url = F,
                    data.manager = get.default.data.manager(),
                    style.manager = get.default.style.manager(),
                    show.data.pull.error = F,
                    debug = F)
{
    plot.data = plot.data.validation(list(...),
                                     match.call(expand.dots = F)$...,
                                     outcomes, 
                                     corresponding.data.outcomes, 
                                     plot.which, 
                                     summary.type)
    
    # These values are possibly modified by the plot.data.validation call, so 
    # they need to be extracted from the returned list.
    simset.list = plot.data$simset.list
    outcomes = plot.data$outcomes
    
    prepared.plot.data = prepare.plot(simset.list,
                                      outcomes=outcomes,
                                      locations=NULL,
                                      corresponding.data.outcomes = corresponding.data.outcomes,
                                      split.by=split.by,
                                      facet.by=facet.by,
                                      dimension.values=dimension.values,
                                      dimension.values.post.mapping=dimension.values.post.mapping,
                                      target.ontology=target.ontology,
                                      plot.which=plot.which,
                                      summary.type=summary.type,
                                      label.function=label.function,
                                      plot.year.lag.ratio=plot.year.lag.ratio,
                                      title=title,
                                      title.suffix=title.suffix,
                                      append.url=append.url,
                                      data.manager=data.manager,
                                      style.manager=style.manager,
                                      show.data.pull.error=show.data.pull.error,
                                      debug=debug)
    
    execute.simplot(prepared.plot.data,
                    outcomes=outcomes,
                    split.by=split.by,
                    facet.by=facet.by,
                    plot.which=plot.which,
                    summary.type=summary.type,
                    plot.year.lag.ratio=plot.year.lag.ratio,
                    n.facet.rows=n.facet.rows,
                    style.manager=style.manager,
                    debug=debug)
}

#' Cleaning and verifying the plotting data so it can be properly plotted
#'@param simset.args A list of the ... parameter in the previous function, containing simset data.  Will be processed and returned as $simset.list
#'@param deparsed.substituted.args.simset.args The ... parameter with each argument converted to a character exactly as it is, without evaluating any expressions. Used to give un-named simsets names later.
#'@param outcomes The outcomes asked for by the plotting function.  Will be processed and returned as $outcomes
#'@param corresponding.data.outcomes Checked for proper value, not passed out of the function
#'@param plot.which Checked for proper value, not passed out of the function
#'@param summary.type Checked for proper value, not passed out of the function
#'@return A list containing processed $simset.list and $outcomes (a vector)
#' @export
plot.data.validation = function(simset.args,
                                deparsed.substituted.args.simset.args,
                                outcomes,
                                corresponding.data.outcomes,
                                plot.which,
                                summary.type) {
    rv = list()
    
    error.prefix = "Cannot generate simplot: "
    
    if (!(identical(plot.which, 'sim.and.data') || identical(plot.which, 'sim.only')))
        stop(paste0(error.prefix, "'plot.which' must be either 'sim.and.data' or 'sim.only'"))
    
    if (!(identical(summary.type, 'individual.simulation') || identical(summary.type, 'mean.and.interval') || identical(summary.type, 'median.and.interval')))
        stop(paste0(error.prefix, "'summary.type' must be one of 'individual.simulation', 'mean.and.interval', or 'median.and.interval'"))
    
    # *corresponding.data.outcomes' is NULL or a vector with outcomes as names
    if (!is.null(corresponding.data.outcomes) && (!is.character(corresponding.data.outcomes) || any(is.na(corresponding.data.outcomes)) || is.null(names(corresponding.data.outcomes)) || !all(names(corresponding.data.outcomes) %in% outcomes)))
        stop(paste0(error.prefix, "'corresponding.data.outcomes' must be NULL or a character vector with outcomes as names and all of those outcomes specified in either the 'outcomes' argument or in '...'"))
    
    # each element of 'sim.list' should be a simset
    arg.is.simset = sapply(simset.args, function(element) {
        if (!R6::is.R6(element) || !is(element, 'jheem.simulation.set')) {
            if (is.character(element))
                return(FALSE)
            else
                stop(paste0(error.prefix, "arguments supplied in '...' must be jheem.simulation.set objects and at most one character vector of outcomes"))
        }
        return(TRUE)
    })
    
    # Pull out outcomes, which may be at most one element of the ...
    rv$outcomes = outcomes
    if (sum(!arg.is.simset)>0)
    {
        if (!is.null(outcomes))
            stop(paste0(error.prefix, "outcomes must be specified either in '...' arguments or in the 'outcomes' argument, but not both"))
        if (sum(!arg.is.simset)>1)
            stop(paste0(error.prefix, "at most one character vector of outcomes may be suppied in '...' arguments as an alternative to the 'outcomes' argument"))
        rv$outcomes = unlist(simset.args[!arg.is.simset])
    }
    if (!is.character(rv$outcomes) || is.null(rv$outcomes) || any(is.na(rv$outcomes)) || any(duplicated(rv$outcomes))) {
        if (sum(!arg.is.simset)>0)
            stop(paste0(error.prefix, "'outcomes' found as unnamed argument in '...' must be a character vector with no NAs or duplicates"))
        else
            stop(paste0(error.prefix, "'outcomes' must be a character vector with no NAs or duplicates"))
    }
    
    if (sum(arg.is.simset)<1)
        stop(paste0(error.prefix, "one or more jheem.simulation.set objects must be supplied"))
    # browser()
    # Add names to simsets
    rv$simset.list = setNames(simset.args[arg.is.simset], deparsed.substituted.args.simset.args[arg.is.simset])
    if (is.null(names(simset.args)))
        simset.explicitly.named = rep(F, sum(arg.is.simset))
    else
        simset.explicitly.named = sapply(names(simset.args[arg.is.simset]), function(name) {nchar(name)>0})
    names(rv$simset.list)[simset.explicitly.named] = names(simset.args)[simset.explicitly.named]
    
    # - make sure they are all the same version and the location
    if (length(unique(sapply(rv$simset.list, function(simset) {simset$version}))) > 1)
        stop(paste0(error.prefix, "all simulation sets must have the same version"))
    if (length(unique(sapply(rv$simset.list, function(simset) {simset$location}))) > 1)
        stop(paste0(error.prefix, "all simulation sets must have the same location"))
    
    # Check outcomes
    # - make sure each outcome is present in sim$outcomes for at least one sim/simset
    if (any(sapply(rv$outcomes, function(outcome) {!any(sapply(rv$simset.list, function(simset) {outcome %in% simset$outcomes}))})))
        stop(paste0("There weren't any simulation sets for one or more outcomes. Should this be an error?"))
    
    return (rv)
}


#' @title Simplot Data Only
#'@inheritParams simplot
#'@param A character vector of which simulation outcomes to plot. Note: This should be the names of the outcomes in the provided data manager, NOT in the simulations.
#'@param title NULL or a single, non-NA character value. If "location", the first location provided in "locations" will be used for the title.
#'@export
simplot.data.only <- function(outcomes,
                              locations,
                              split.by=NULL,
                              facet.by = NULL,
                              dimension.values = list(),
                              dimension.values.post.mapping = list(),
                              target.ontology = NULL,
                              label.function = NULL,
                              plot.year.lag.ratio = F,
                              title = "location",
                              title.suffix = NULL,
                              n.facet.rows = NULL,
                              append.url = F,
                              data.manager = get.default.data.manager(),
                              style.manager = get.default.style.manager(),
                              show.data.pull.error=F,
                              debug = F) {
    
    error.prefix = "Cannot generate simplot: "
    
    #@ validate locations
    
    prepared.plot.data = prepare.plot(simset.list=NULL,
                                      outcomes=outcomes,
                                      locations=locations,
                                      corresponding.data.outcomes = corresponding.data.outcomes,
                                      split.by=split.by,
                                      facet.by=facet.by,
                                      dimension.values=dimension.values,
                                      dimension.values.post.mapping=dimension.values.post.mapping,
                                      target.ontology=target.ontology,
                                      plot.which='data.only',
                                      summary.type=summary.type,
                                      label.function=label.function,
                                      plot.year.lag.ratio=plot.year.lag.ratio,
                                      title=title,
                                      title.suffix=title.suffix,
                                      append.url=append.url,
                                      data.manager=data.manager,
                                      style.manager=style.manager,
                                      show.data.pull.error=show.data.pull.error,
                                      debug=debug)
    execute.simplot(prepared.plot.data,
                    outcomes=outcomes,
                    split.by=split.by,
                    facet.by=facet.by,
                    plot.which=plot.which,
                    summary.type=summary.type,
                    plot.year.lag.ratio=plot.year.lag.ratio,
                    n.facet.rows=n.facet.rows,
                    style.manager=style.manager,
                    debug=debug)
}

#' Prepare Plot Data
#' A utility that performs the data-pulling half of the simplot operation
#' for applications like webtools that need the step to be separate.
#' @inheritParams simplot
#' @param simset.list A list of jheem.simulation.set objects
#' @param plot.which Should simulation data and calibration data be plotted ('sim.and.data'), or only simulation data ('sim.only'), or only calibration data ('data.only')
#' @value A list with three components:
#' df.sim, a data frame containing simulation data (may be NULL)
#' df.truth, a data frame containing calibration data (may be NULL)
#' details, a list with components "y.label", "plot.title", "outcome.metadata.list", "source.metadata.list", and "sim.labels.list"
#' @export
prepare.plot <- function(simset.list=NULL,
                         outcomes=NULL,
                         locations=NULL,
                         corresponding.data.outcomes = NULL,
                         split.by = NULL,
                         facet.by = NULL,
                         dimension.values = list(),
                         dimension.values.post.mapping = list(),
                         target.ontology = NULL,
                         plot.which = c('sim.and.data', 'sim.only', 'data.only')[1],
                         summary.type = c('individual.simulation', 'mean.and.interval', 'median.and.interval')[1],
                         plot.year.lag.ratio = F,
                         label.function = NULL,
                         title="location",
                         title.suffix=NULL,
                         append.url=F,
                         data.manager = get.default.data.manager(),
                         style.manager=get.default.style.manager(),
                         show.data.pull.error=F,
                         debug = F)
{
    #-- VALIDATION ----
    if (debug) browser()
    error.prefix = "Cannot generate simplot: "
    
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    # *split.by* is NULL or a single, non-NA character vector
    if (!is.null(split.by) && (!is.character(split.by) || length(split.by) > 1 || is.na(split.by)))
        stop(paste0(error.prefix, "'split.by' must be NULL or a length one, non-NA character vector"))
    
    # *facet.by* is NULL or a character vector of length > 0 with no NAs or duplicates
    if (!is.null(facet.by) && (!is.character(facet.by) || length(facet.by) < 1 || any(is.na(facet.by)) || any(duplicated(facet.by))))
        stop(paste0(error.prefix, "'facet.by' must be NULL or a character vector with at least one element and no NAs or duplicates"))
    
    if (!is.null(split.by) && split.by %in% facet.by)
        stop(paste0(error.prefix, "'facet.by' must not contain the dimension in 'split.by'"))
    
    if (!is.null(split.by) && split.by == 'year')
        stop(paste0(error.prefix, "'split.by' cannot equal 'year'"))
    
    if (!is.null(facet.by) && 'year' %in% facet.by)
        stop(paste0(error.prefix, "'facet.by' cannot contain 'year'"))
    
    if (!is.null(target.ontology) &&
        !is.ontology(target.ontology) &&
        !(is.list(target.ontology) && all(sapply(target.ontology, function(x) {is.ontology((x))})) && !is.null(names(target.ontology))))
        stop(paste0(error.prefix, "'target.ontology' must be NULL, an ontology, or a list of ontologies with outcomes as names"))
    
    # Must supply a target ontology if using simplot.data.only, because otherwise multiple outcomes won't be alignable...?
    
    # label.function must be NULL or a function that takes one argument.
    if (!is.null(label.function) &&(!is.function(label.function) || length(formals(label.function)) != 1)) {
        stop(paste0(error.prefix, "'label.function' must be NULL or a function that takes one argument"))
    }
    
    if (!identical(plot.year.lag.ratio, T) && !identical(plot.year.lag.ratio, F))
        stop(paste0(error.prefix, "'plot.year.lag.ratio' must be either T or F"))
    
    # if *plot.year.lag.ratio* is true, we can have only one outcome
    if (plot.year.lag.ratio && length(outcomes)>1)
        stop(paste0(error.prefix, "only one outcome can be used with 'plot.year.lag.ratio'"))
    
    if (!is.null(title) && (!is.character(title) || length(title)!=1 || is.na(title)))
        stop(paste0(error.prefix, "'title' must be NULL or a single, non-NA character value"))
    
    if (!is.null(title.suffix) && (!is.character(title.suffix) || length(title.suffix)!=1 || is.na(title.suffix)))
        stop(paste0(error.prefix, "'title.suffix' must be NULL or a single, non-NA character value"))
    
    if (!identical(append.url, T) && !identical(append.url, F))
        stop(paste0(error.prefix, "'append.url' must be either T or F"))
    
    if (!identical(show.data.pull.error, T) && !identical(show.data.pull.error, F))
        stop(paste0(error.prefix, "'show.data.pull.error' must be either T or F"))
    
    # Get the real-world outcome names
    # - eventually we're going to want to pull this from info about the likelihood if the sim notes which likelihood was used on it
    # - what we'll do now will be the back-up to above
    #   sim$outcome.metadata[[outcome]]$corresponding.observed.outcome
    # sims do not all have each outcome because of sub-versions
    
    
    #-- GET OUTCOME METADATA ----
    if (plot.which=='data.only')
        outcome.metadata.list = lapply(outcomes, function(outcome) {data.manager$outcome.info[[outcome]]$metadata})
    else {
        outcome.metadata.list = lapply(outcomes, function(outcome) {
            i = 1
            while (i <= length(simset.list)) {
                if (outcome %in% names(simset.list[[i]]$outcome.metadata)) {
                    return(simset.list[[i]]$outcome.metadata[[outcome]])
                } else i = i + 1
            }
            stop(paste0(error.prefix, "Consult Andrew: this simplot bug shouldn't happen"))
        })
    }
    names(outcome.metadata.list) = outcomes
    
    # likelihoods need to share their outcome for sim and data, and think about what joint likelihoods. One simulation has one (usually joint) likelihood (instructions)
    if (plot.which=="data.only") outcomes.for.data = setNames(outcomes, outcomes)
    else {
        outcomes.for.data = sapply(outcomes, function(outcome) {
            if (outcome %in% names(corresponding.data.outcomes))
                return(corresponding.data.outcomes[[outcome]])
            corresponding.observed.outcome = NULL
            i = 1
            while (i <= length(simset.list)) {
                if (outcome %in% names(simset.list[[i]]$outcome.metadata)) {
                    corresponding.observed.outcome = simset.list[[i]]$outcome.metadata[[outcome]]$corresponding.observed.outcome
                    break
                } else i = i + 1
            }
            corresponding.observed.outcome
        })
    }
    
    if (plot.which=='data.only') outcome.ontologies=NULL
    else {
        outcome.ontologies = lapply(outcomes, function(outcome) {
            if (is.null(target.ontology) || FALSE)
                outcome.ontology = NULL
            i = 1
            while (i <= length(simset.list)) {
                if (outcome %in% names(simset.list[[i]]$outcome.ontologies)) {
                    outcome.ontology = simset.list[[i]]$outcome.ontologies[[outcome]]
                    break
                } else i = i + 1
            }
            if (is.null(outcome.ontology))
                stop(paste0("No outcome ontology found for outcome '", outcome, "'")) # Shouldn't happen
            outcome.ontology
        }) 
    }
    
    if (plot.which=='data.only') outcome.locations = locations
    else {
        outcome.locations = lapply(outcomes, function(outcome) {
            locations.this.outcome = unique(unlist(lapply(simset.list, function(simset) {
                simset$outcome.location.mapping$get.observed.locations(outcome, simset$location)
            })))
        })
        names(outcome.locations) = outcomes.for.data
    }
    
    # Get labels (like "MSM/PWID" instead of "msm_idu")
    if (!is.null(label.function)) {
        label_function <- label.function
    } else if (plot.which!="data.only") {
        label_function <- simset.list[[1]]$get.labels
    } else {
        label_function <- function(x) {x}
    }
    
    #-- MAKE A DATA FRAME WITH ALL THE REAL-WORLD DATA ----
    
    outcome.mappings = list() # note: not all outcomes will have corresponding data outcomes
    source.metadata.list <- list()
    if (append.url) append.attributes='url' else append.attributes=NULL
    
    df.truth = NULL
    for (i in seq_along(outcomes.for.data))
    {
        if (plot.which == "data.only" && is.null(outcome.metadata.list[[i]])) {
            stop(paste0(error.prefix, "'", outcomes[i], "' is not a registered outcome in the data manager. Find valid outcomes in the data manager with <data.manager>$outcomes"))
        }
        if (plot.which != 'sim.only' && !is.null(outcomes.for.data[[i]]))
        {
            outcome.data = tryCatch(
                {
                    # browser()
                    if (!is.null(target.ontology) && !is.list(target.ontology))
                        result = data.manager$pull(outcome = outcomes.for.data[[i]],
                                                   dimension.values = c(dimension.values, dimension.values.post.mapping, list(location = outcome.locations[[i]])),
                                                   keep.dimensions = c('year', 'location', facet.by, split.by), #'year' can never be in facet.by
                                                   target.ontology = target.ontology,
                                                   allow.mapping.from.target.ontology = F,
                                                   append.attributes=append.attributes,
                                                   na.rm=T,
                                                   debug=F)
                    else if (is.list(target.ontology) && outcomes.for.data[[i]] %in% names(target.ontology))
                        result = data.manager$pull(outcome = outcomes.for.data[[i]],
                                                   dimension.values = c(dimension.values, dimension.values.post.mapping, list(location = outcome.locations[[i]])),
                                                   keep.dimensions = c('year', 'location', facet.by, split.by), #'year' can never be in facet.by
                                                   target.ontology = target.ontology[[outcomes.for.data[[i]]]],
                                                   allow.mapping.from.target.ontology = F,
                                                   append.attributes=append.attributes,
                                                   na.rm=T,
                                                   debug=F)
                    else if (plot.which=='sim.and.data')
                        result = data.manager$pull(outcome = outcomes.for.data[[i]],
                                                   dimension.values = c(dimension.values, dimension.values.post.mapping, list(location = outcome.locations[[i]])),
                                                   keep.dimensions = c('year', 'location', facet.by, split.by), #'year' can never be in facet.by
                                                   target.ontology = outcome.ontologies[[i]],
                                                   allow.mapping.from.target.ontology = T,
                                                   append.attributes=append.attributes,
                                                   na.rm=T,
                                                   debug=F)
                    else # See if we really want this or not
                        result = data.manager$pull(outcome = outcomes.for.data[[i]],
                                                   dimension.values = c(dimension.values, dimension.values.post.mapping, list(location = outcome.locations[[i]])),
                                                   keep.dimensions = c('year', 'location', facet.by, split.by), #'year' can never be in facet.by
                                                   target.ontology = NULL,
                                                   append.attributes=append.attributes,
                                                   na.rm=T,
                                                   debug=F)
                    
                },
                error = function(e) {
                    if (show.data.pull.error) stop(paste0(error.prefix, e))
                    else NULL
                }
            )
            if (!is.null(attr(outcome.data, 'mapping')))
                outcome.mappings = c(outcome.mappings, list(attr(outcome.data, 'mapping')))
            else
                outcome.mappings = c(outcome.mappings, list(NULL))
            if (!is.null(outcome.data)) {
                
                # If the scale is proportion, multiply data by 100 to match the "%" symbol the label will have
                if (data.manager$outcome.info[[outcomes.for.data[[i]]]]$metadata$display.as.percent)
                    outcome.data = outcome.data * 100
                
                # If we have multiple outcomes that may map differently (for example, with years), the factor levels unavoidably determined by the first outcome for reshape2::melt may not be valid for subsequent outcomes
                one.df.outcome = reshape2::melt(outcome.data, na.rm = T, as.is=T)
                
                # Convert labels
                columns_to_labelize <- setdiff(colnames(one.df.outcome), c("year", "location", "source", "value"))
                one.df.outcome[columns_to_labelize] <- lapply(one.df.outcome[columns_to_labelize], function(col) {
                    label_function(col)
                })
                
                if (append.url) {
                    one.df.outcome = cbind(one.df.outcome, reshape2::melt(attr(outcome.data, 'url'), na.rm=T, as.is=T))
                    colnames(one.df.outcome)[ncol(one.df.outcome)] = 'url'
                }
                
                # Check that we don't have year ranges if we are trying to do the year lag ratio thing
                if (!any(sapply(one.df.outcome$year, is.year.range)))
                    one.df.outcome$year = as.numeric(one.df.outcome$year)
                else if (plot.year.lag.ratio)
                    stop(paste0(error.prefix, "cannot use 'plot.year.lag.ratio' when data is in year ranges"))
                
                # Add the source info to the source metadata list
                sources.this.outcome <- unique(one.df.outcome$source)
                source.metadata.list = c(source.metadata.list, data.manager$source.info[setdiff(sources.this.outcome, names(source.metadata.list))])
                
                corresponding.outcome = names(outcomes.for.data)[[i]]
                one.df.outcome['outcome'] = corresponding.outcome
                one.df.outcome['outcome.display.name'] = outcome.metadata.list[[corresponding.outcome]]$display.name
                df.truth = rbind(df.truth, one.df.outcome)
            }
        }
        
        else
        {
            outcome.mappings = c(outcome.mappings, list(NULL))
        }
    }
    if (!is.null(df.truth)) {
        # make whatever column corresponds to split by actually be called "stratum" and same for facet.by.
        if (!is.null(split.by)) names(df.truth)[names(df.truth)==split.by] = "stratum"
        if (!is.null(facet.by))
            for (i in seq_along(facet.by)) {
                names(df.truth)[names(df.truth)==facet.by[i]] = paste0("facet.by", i)
            }
        
        df.truth$outcome.display.name <- factor(df.truth$outcome.display.name, levels = sapply(outcome.metadata.list, function(outcome) {outcome$display.name}))
        # if there is no 'stratum' because no split, then we should fill it with ""
        if (!('stratum' %in% names(df.truth))) df.truth['stratum'] = rep('', nrow(df.truth))
        
        # sort the split.by column alphabetically so that when we assign colors, it will be the same for sim.
        if (!is.null(split.by))
            df.truth = df.truth[order(df.truth$stratum),]
    }
    names(outcome.mappings) = outcomes
    
    #-- MAKE A DATA FRAME WITH THE SIMULATION DATA ----
    
    df.sim = NULL
    if (plot.which != 'data.only') {
        for (outcome in outcomes) {
            
            keep.dimensions = c('year', facet.by, split.by)
            for (i in seq_along(simset.list)) {
                
                simset = simset.list[[i]]
                if (!is.null(outcome.mappings[[outcome]])) mapping.this.outcome = outcome.mappings[[outcome]] # case when target ontology is NULL or is a list but doesn't include this outcome
                else if (is.list(target.ontology)) mapping.this.outcome = get.ontology.mapping(outcome.ontologies[[i]], target.ontology[[outcome]])
                else if (!is.null(target.ontology)) mapping.this.outcome = get.ontology.mapping(outcome.ontologies[[i]], target.ont)
                else mapping.this.outcome = NULL
                # browser()
                simset.data.this.outcome = simset$get(outcomes = outcome,
                                                      dimension.values = dimension.values,
                                                      keep.dimensions = keep.dimensions,
                                                      drop.single.outcome.dimension = T,
                                                      mapping=mapping.this.outcome,
                                                      summary.type = summary.type)
                
                if (is.null(simset.data.this.outcome)) next
                
                # Subset post mapping
                simset.data.this.outcome <- array.access(simset.data.this.outcome, dimension.values.post.mapping)
                
                # If the scale is proportion, multiply data by 100 to match the "%" symbol the label will have
                if (simset.list[[i]][['outcome.metadata']][[outcome]]$display.as.percent)
                    simset.data.this.outcome = simset.data.this.outcome * 100
                
                # If we have multiple outcomes that may map differently (for example, with years), the factor levels unavoidably determined by the first outcome for reshape2::melt may not be valid for subsequent outcomes
                one.df.sim.this.outcome = reshape2::melt(simset.data.this.outcome, na.rm = T)
                one.df.sim.this.outcome = as.data.frame(lapply(one.df.sim.this.outcome, function(col) {
                    if (is.factor(col)) as.character(col)
                    else col
                }))
                
                # Convert columns not "year", "sim", "metric" or "value" into proper labels
                columns_to_labelize <- setdiff(colnames(one.df.sim.this.outcome), c("year", "sim", "value", "metric"))
                one.df.sim.this.outcome[columns_to_labelize] <- lapply(one.df.sim.this.outcome[columns_to_labelize], function(col) {
                    label_function(col)
                })
                
                one.df.sim.this.outcome['simset'] = names(simset.list)[[i]]
                one.df.sim.this.outcome['outcome'] = outcome
                one.df.sim.this.outcome['linewidth'] = style.manager$linewidth.baseline/(style.manager$linewidth.slope * log10(simset$n.sim) + 1)
                if (!is.null(style.manager$alpha.line)) {
                    one.df.sim.this.outcome['alpha'] = style.manager$alpha.line
                } else {
                    one.df.sim.this.outcome['alpha'] = one.df.sim.this.outcome['linewidth']
                }
                
                # Make a "outcome.long.name" column so that the facet.by can present it instead of the short name
                one.df.sim.this.outcome['outcome.display.name'] = outcome.metadata.list[[outcome]]$display.name
                
                df.sim = rbind(df.sim, one.df.sim.this.outcome)
            }
        }
        
        # Pivot wider to convert column "metric" to columns "value.mean", "value.lower", "value.upper" or such
        if (summary.type != 'individual.simulation') {
            df.sim = reshape(df.sim, direction='wide', idvar=names(df.sim)[!(names(df.sim) %in% c('metric', 'value'))], timevar='metric')
            if (!is.null(df.sim[['value.mean']])) df.sim$value = df.sim$value.mean
            if (!is.null(df.sim[['value.median']])) df.sim$value = df.sim$value.median
        }
        # browser()
        # make whatever column corresponds to split by actually be called "stratum" and same for facet.by.
        if (!is.null(split.by)) df.sim["stratum"] = df.sim[split.by]
        if (!is.null(facet.by))
            for (i in seq_along(facet.by)) {
                df.sim[paste0("facet.by", i)] = df.sim[facet.by[i]]
            }
        # df.sim["facet.by"] = df.sim[facet.by]
        
        df.sim$outcome.display.name <- factor(df.sim$outcome.display.name, levels = sapply(outcome.metadata.list, function(outcome) {outcome$display.name}))
        
        # if we don't have a 'stratum' col because no split, make an empty one
        if (!('stratum' %in% names(df.sim))) df.sim['stratum'] = rep('', nrow(df.sim))
        
        df.sim$simset = factor(df.sim$simset)
        df.sim$sim = factor(df.sim$sim)
        df.sim$groupid = paste0(df.sim$outcome, '_', df.sim$simset, '_', df.sim$sim, '_', df.sim$stratum)
        
        # sort split by alphabetically to line it up with df.truth when colors are picked
        if (!is.null(split.by))
            df.sim = df.sim[order(df.sim$stratum),]
    }
    
    #-- YEAR LAG RATIO #----
    if (plot.year.lag.ratio) {
        # browser()
        ## We will take log of values, then difference, then exponentiate result
        if (!is.null(df.truth)) {
            df.truth$value = log(df.truth$value)
            if (!is.null(split.by)) {
                if (!is.null(facet.by))
                    df.truth[['stratum']] = do.call(paste, c(list(df.truth$stratum),
                                                             lapply(seq_along(facet.by), function(i) {df.truth[[paste0("facet.by", i)]]}),
                                                             list(sep="__")))
            }
            else if (!is.null(facet.by))
                df.truth[['stratum']] = do.call(paste, c(lapply(seq_along(facet.by), function(i) {df.truth[[paste0("facet.by", i)]]}),
                                                         list(sep="__")))
            else df.truth[['stratum']] = rep(0, nrow(df.truth))
            truth.lag.indices = generate_lag_matrix_indices(as.integer(as.factor(df.truth$year)),
                                                            as.integer(as.factor(df.truth$location)),
                                                            as.integer(as.factor(df.truth$stratum)),
                                                            as.integer(as.factor(df.truth$source)),
                                                            nrow(df.truth))
            truth.n.lag.pairs = length(truth.lag.indices)/2
            
            truth.lag.values = apply_lag_to_vector(df.truth$value, truth.lag.indices, rep(0, truth.n.lag.pairs), truth.n.lag.pairs)
            truth.rows.to.keep = truth.lag.indices[rep(c(T,F), truth.n.lag.pairs)] + 1 # add one because CPP is zero-indexed
            df.truth = df.truth[truth.rows.to.keep,]
            df.truth$value = exp(truth.lag.values)
            
            # Remove NAs or Infs generated in this process
            df.truth = df.truth[!is.na(df.truth$value) & !is.infinite(df.truth$value),]
            
            #If we end up with 0 rows, we need to consider the df.truth to be NULL
            if (nrow(df.truth)==0) df.truth=NULL
        }
        if (!is.null(df.sim)) {
            df.sim$value = log(df.sim$value)
            if (!is.null(df.sim$value.lower) && !is.null(df.sim$value.upper)) {
                df.sim$value.lower = log(df.sim$value.lower)
                df.sim$value.upper = log(df.sim$value.upper)
            }
            if (!is.null(split.by)) {
                if (!is.null(facet.by))
                    df.sim[['stratum']] = do.call(paste, c(list(df.sim$stratum),
                                                           lapply(seq_along(facet.by), function(i) {df.sim[[paste0("facet.by", i)]]}),
                                                           list(sep="__")))
            }
            else if (!is.null(facet.by))
                df.sim[['stratum']] = do.call(paste, c(lapply(seq_along(facet.by), function(i) {df.sim[[paste0("facet.by", i)]]}),
                                                       list(sep="__")))
            else df.sim[['stratum']] = rep(0, nrow(df.sim))
            # browser()
            sim.lag.indices = generate_lag_matrix_indices(as.integer(as.factor(df.sim$year)),
                                                          as.integer(as.factor(df.sim$sim)),
                                                          as.integer(as.factor(df.sim$stratum)),
                                                          as.integer(as.factor(df.sim$simset)),
                                                          nrow(df.sim))
            sim.n.lag.pairs = length(sim.lag.indices)/2
            
            sim.lag.values = apply_lag_to_vector(df.sim$value, sim.lag.indices, rep(0, sim.n.lag.pairs), sim.n.lag.pairs)
            sim.rows.to.keep = sim.lag.indices[rep(c(T,F), sim.n.lag.pairs)] + 1 # add one because CPP is zero-indexed
            
            
            if (!is.null(df.sim$value.lower) && !is.null(df.sim$value.upper)) {
                
                sim.lower.lag.values = apply_lag_to_vector(df.sim$value.lower, sim.lag.indices, rep(0, sim.n.lag.pairs), sim.n.lag.pairs)
                sim.upper.lag.values = apply_lag_to_vector(df.sim$value.upper, sim.lag.indices, rep(0, sim.n.lag.pairs), sim.n.lag.pairs)
                
            }
            
            df.sim = df.sim[sim.rows.to.keep,]
            df.sim$value = exp(sim.lag.values)
            
            if (!is.null(df.sim$value.lower) && !is.null(df.sim$value.upper)) {
                df.sim$value.lower = exp(sim.lower.lag.values)
                df.sim$value.upper = exp(sim.upper.lag.values)
            }
            
            # Remove NAs or Infs generated in this process. Won't bother doing this for value.lower and upper... hopefully fine
            df.sim = df.sim[!is.na(df.sim$value) & !is.infinite(df.sim$value),]
            
            #If we end up with 0 rows, we need to consider the df.sim to be NULL
            if (length(df.sim)==0) df.sim=NULL
        }
    }
    
    #-- PACKAGE AND RETURN ----
    if (plot.which=="data.only")
        y.label = sapply(outcomes, function(outcome) {data.manager$outcome.info[[outcome]]$metadata$units})
    else
        y.label = paste0(sapply(outcomes, function(outcome) {simset.list[[1]][['outcome.metadata']][[outcome]][['units']]}), collapse='/')
    if (plot.year.lag.ratio)
        y.label = paste0("Log difference in ", y.label)
    if (title=="location" && plot.which=="data.only") {
        plot.title = paste0(get.location.name(locations[[1]]), " (", locations[[1]], ")") # need to check we've got valid location
    }
    else if (title=="location")
        plot.title = paste0(get.location.name(simset.list[[1]]$location), " (", simset.list[[1]]$location, ")")
    else plot.title = title
    if (!is.null(title.suffix))
        plot.title <- paste0(plot.title, title.suffix)
    # browser()
    return(list(df.sim=df.sim,
                df.truth=df.truth,
                details=list(y.label=y.label,
                             plot.title=plot.title,
                             outcome.metadata.list = outcome.metadata.list,
                             source.metadata.list = source.metadata.list)))
}

#' Execute Simplot
#' A utility that performs the plot-rendering half of the simplot operation
#' for applications like webtools that need the step to be separate.
#' @inheritParams simplot
#' @param prepared.plot.data The named list output by "prepare.plot"
#' @param n.facet.rows An optional argument to specify how many rows the figure should facet plots into
#' @value A ggplot object
#' @export
execute.simplot <- function(prepared.plot.data,
                            outcomes=NULL,
                            split.by=NULL,
                            facet.by=NULL,
                            plot.which=c('sim.and.data', 'sim.only', 'data.only')[1],
                            summary.type=c('individual.simulation', 'mean.and.interval', 'median.and.interval')[1],
                            plot.year.lag.ratio=F,
                            n.facet.rows=NULL,
                            style.manager=get.default.style.manager(),
                            debug=F)
{
    if (debug) browser()
    # browser()
    #-- UNPACK DATA --#
    df.sim=prepared.plot.data$df.sim
    df.truth=prepared.plot.data$df.truth
    y.label = prepared.plot.data$details$y.label
    plot.title = prepared.plot.data$details$plot.title
    
    #-- PREPARE PLOT COLORS, SHADES, SHAPES, ETC. --#
    
    if (!is.null(df.sim)) {
        df.sim['linetype.sim.by'] = df.sim[style.manager$linetype.sim.by]
        df.sim['shape.sim.by'] = df.sim[style.manager$shape.sim.by]
        df.sim['color.sim.by'] = df.sim[style.manager$color.sim.by]
    }
    
    if (!is.null(df.truth)) {
        # make some other columns
        df.truth['location.type'] = locations::get.location.type(df.truth$location)
        df.truth['shape.data.by'] = df.truth[style.manager$shape.data.by]
        df.truth['color.data.by'] = df.truth[style.manager$color.data.by]
        if (!is.null(style.manager$shade.data.by))
            df.truth['shade.data.by'] = df.truth[style.manager$shade.data.by]
        
        # If either stratifying and use either color or shade for "stratum", use the other one
        # unless possibly we're skipping shading.
        if (!is.null(style.manager$shade.data.by) &&
            style.manager$color.data.by == 'stratum' && !is.null(df.truth$stratum) && all(df.truth$stratum==""))
            df.truth['color.and.shade.data.by'] = df.truth['shade.data.by']
        else if (is.null(style.manager$shade.data.by) ||
                 style.manager$shade.data.by == 'stratum' && !is.null(df.truth$stratum) && all(df.truth$stratum==""))
            df.truth['color.and.shade.data.by'] = df.truth['color.data.by']
        else
            df.truth['color.and.shade.data.by'] = do.call(paste, c(df.truth['shade.data.by'], df.truth['color.data.by'], list(sep="__")))
    }
    
    ## COLORS
    colors.for.sim = NULL
    color.data.primary.colors = NULL
    
    sim.color.groups = sort(unique(df.sim$color.sim.by))
    data.color.groups = sort(unique(df.truth$color.data.by))
    
    # if coloring by the same thing, use the same palette (defaulting to SIM's palette) unless one is missing
    if (style.manager$color.sim.by == style.manager$color.data.by) {
        all.color.groups = sort(union(sim.color.groups, data.color.groups))
        
        if (!is.null(df.sim))
            all.colors = style.manager$get.sim.colors(length(all.color.groups))
        else if (!is.null(df.truth))
            all.colors = style.manager$get.data.colors(length(all.color.groups))
        else
            all.colors = NULL # doesn't matter?
        
        names(all.colors) = all.color.groups
        colors.for.sim = all.colors[sim.color.groups]
        color.data.primary.colors = all.colors[data.color.groups]
    }
    
    # otherwise, assign colors individually
    else {
        if (!is.null(df.sim)) {
            colors.for.sim = style.manager$get.sim.colors(length(sim.color.groups))
            names(colors.for.sim) = sim.color.groups
        }
        if (!is.null(df.truth)) {
            color.data.primary.colors = style.manager$get.data.colors(length(data.color.groups))
            names(color.data.primary.colors) = data.color.groups
        }
    }
    
    ## RIBBON COLOR
    color.ribbon.by = NULL
    if (!is.null(df.sim)) {
        color.ribbon.by = ggplot2::alpha(colors.for.sim, style.manager$alpha.ribbon)
    }
    
    ## SHADES FOR DATA
    color.data.shaded.colors = NULL
    if (!is.null(df.truth)) {
        if (is.null(style.manager$shade.data.by))
            color.data.shaded.colors <- color.data.primary.colors
        else {
            color.data.shaded.colors = unlist(lapply(color.data.primary.colors, function(prim.color) {style.manager$get.shades(base.color=prim.color, length(unique(df.truth$shade.data.by)))}))
            # This can lead to problems if we have either of these being "" because then we'll get an underscore that won't match the actual column values in the data frame
            if (identical(unique(df.truth$color.data.by), ""))
                names(color.data.shaded.colors) = unique(df.truth$shade.data.by)
            else
                names(color.data.shaded.colors) = do.call(paste, c(expand.grid(unique(df.truth$shade.data.by), unique(df.truth$color.data.by)), list(sep="__")))
            
        }
    }
    
    ## SHAPES
    shapes.for.data = NULL
    shapes.for.sim = NULL
    if (!is.null(df.truth)) {
        shapes.for.data = style.manager$get.shapes(length(unique(df.truth$shape.data.by)))
        names(shapes.for.data) = unique(df.truth$shape.data.by)
    }
    if (!is.null(df.sim)) {
        shapes.for.sim = style.manager$get.shapes(length(unique(df.sim$shape.sim.by)))
        names(shapes.for.sim) = unique(df.sim$shape.sim.by)
    }
    all.shapes.for.scale = c(shapes.for.data, shapes.for.sim)
    
    ## LINETYPES
    linetypes.for.sim = NULL
    if (!is.null(df.sim)) {
        linetypes.for.sim = style.manager$get.linetypes(length(unique(df.sim$linetype.sim.by)))
        names(linetypes.for.sim) = unique(df.sim$linetype.sim.by)
    }
    
    ## GROUPS
    # break df.sim into two data frames, one for outcomes where the sim will be lines and the other for where it will be points
    df.sim.groupids.one.member = NULL
    df.sim.groupids.many.members = NULL
    if (!is.null(df.sim)) {
        groupids.with.one.member = setdiff(unique(df.sim$groupid), df.sim$groupid[which(duplicated(df.sim$groupid))])
        df.sim$groupid_has_one_member = with(df.sim, groupid %in% groupids.with.one.member)
        df.sim.groupids.one.member = subset(df.sim, groupid_has_one_member)
        df.sim.groupids.many.members = subset(df.sim, !groupid_has_one_member)
    }
    
    #-- MAKE THE PLOT --#
    # browser()
    rv = ggplot2::ggplot()
    rv = rv +
        ggplot2::labs(y=y.label) +
        ggplot2::ggtitle(plot.title) +
        ggplot2::scale_alpha_identity(guide='none')
    
    
    if (!plot.year.lag.ratio)
        rv = rv + ggplot2::scale_y_continuous(limits=c(0, NA), labels = scales::comma)
    else
        rv = rv + ggplot2::scale_y_continuous(labels = scales::comma)
    # browser()
    # SIM ELEMENTS
    if (!is.null(df.sim)) {
        # Note: the key to avoiding warning messages about scale is to only add a scale if it is used by the data frames that are actually plotted.
        
        # PLOT
        if (!is.null(split.by)) {
            rv = rv + ggplot2::scale_color_manual(name = "sim color", values = colors.for.sim)
            has.added.color.scale = T
            if (nrow(df.sim.groupids.many.members)>0) {
                rv = rv + ggplot2::geom_line(data=df.sim.groupids.many.members, ggplot2::aes(x=year,y=value,group=groupid,
                                                                                             linetype = linetype.sim.by,
                                                                                             color = color.sim.by,
                                                                                             linewidth=linewidth,
                                                                                             alpha = alpha))
            }
            if (nrow(df.sim.groupids.one.member)>0) {
                rv = rv +
                    ggplot2::geom_point(data=df.sim.groupids.one.member, ggplot2::aes(x=year, y=value,
                                                                                      size = size,
                                                                                      fill = color.sim.by,
                                                                                      shape = shape.sim.by), show.legend = F) +
                    
                    ggplot2::scale_size_manual(values=c(size=2))
                
            }
            if (summary.type != 'individual.simulation') {
                rv = rv + ggplot2::geom_ribbon(data=df.sim.groupids.many.members, ggplot2::aes(x=year, y=value,group=groupid,
                                                                                               fill = color.sim.by,
                                                                                               ymin = value.lower,
                                                                                               ymax = value.upper),
                                               alpha = style.manager$alpha.ribbon,
                                               outline.type = 'full')
                rv = rv + ggplot2::scale_fill_manual(name = "sim color", values = colors.for.sim)
            }
        } else {
            if (nrow(df.sim.groupids.many.members)>0) {
                if (style.manager$color.sim.by=="simset") {
                    rv = rv + ggplot2::geom_line(data=df.sim.groupids.many.members, ggplot2::aes(x=year, y=value, group=groupid,
                                                                                                 linetype = linetype.sim.by,
                                                                                                 color = color.sim.by,
                                                                                                 linewidth=linewidth,
                                                                                                 alpha = alpha))
                } else
                    rv = rv + ggplot2::geom_line(data=df.sim.groupids.many.members, ggplot2::aes(x=year, y=value, group=groupid,
                                                                                                 linetype = linetype.sim.by,
                                                                                                 linewidth=linewidth,
                                                                                                 alpha = alpha))
            }
            if (nrow(df.sim.groupids.one.member)>0) {
                rv = rv +
                    ggplot2::geom_point(data=df.sim.groupids.one.member, ggplot2::aes(x=year, y=value,
                                                                                      size = size,
                                                                                      fill = color.sim.by,
                                                                                      shape=shape.sim.by)) +
                    ggplot2::scale_size_manual(values=c(size=2))
            }
            
            if (summary.type != 'individual.simulation') {
                rv = rv + ggplot2::geom_ribbon(data=df.sim.groupids.many.members, ggplot2::aes(x=year, y=value,group=groupid,
                                                                                               fill = color.sim.by,
                                                                                               ymin = value.lower,
                                                                                               ymax = value.upper),
                                               alpha = style.manager$alpha.ribbon,
                                               outline.type = 'full')
                # Remove the fill scale since we don't have more than one sim ribbon color
                if (style.manager$color.sim.by == "stratum")
                    rv = rv + ggplot2::guides(fill = "none")
            }
        }
        if (nrow(df.sim.groupids.many.members)>0) {  #  && summary.type == 'individual.simulation' used to have this... why? We can have lines with individual sims too
            rv = rv + ggplot2::scale_linetype_manual(name="sim linetype", values = linetypes.for.sim, breaks = names(linetypes.for.sim))
            rv = rv + ggplot2::scale_linewidth(NULL, range=c(min(df.sim$linewidth), 1), guide = 'none')
        }
        if (nrow(df.sim.groupids.one.member)>0) {
            rv = rv + ggplot2::scale_fill_manual(name = "sim color", values = colors.for.sim)
            rv = rv + ggplot2::scale_shape_manual(name = "sim shape", values = shapes.for.sim)
        }
        if (style.manager$color.sim.by=="simset") {
            rv = rv + ggplot2::scale_color_manual(name = "sim color", values = colors.for.sim)
            rv = rv + ggplot2::scale_fill_manual(name = "sim color", values = colors.for.sim)
        }
    }
    # browser()
    # DATA ELEMENTS
    if (!is.null(df.truth)) {
        # if we already have a shape scale, clear it before adding new points
        if (!is.null(df.sim.groupids.one.member) && nrow(df.sim.groupids.one.member)>0)
            rv = rv + ggnewscale::new_scale('shape')
        rv = rv + ggnewscale::new_scale_fill() + ggplot2::scale_fill_manual(values = color.data.shaded.colors) # We're changing the scale because the data fills differently
        
        rv = rv + ggplot2::guides(fill = ggplot2::guide_legend("data color", override.aes = list(shape = 21)))
        
        # PLOT
        if (!is.null(split.by)) {
            rv = rv + ggplot2::geom_point(data=df.truth, ggplot2::aes(x=year,
                                                                      y=value,
                                                                      size="size",
                                                                      fill=color.and.shade.data.by, # fill
                                                                      shape=shape.data.by))
        } else {
            # Why is this plotting all black fill, even though we remade the fill scale?? (in Melissa's outcome="new" simplot call)
            rv = rv + ggplot2::geom_point(data=df.truth, ggplot2::aes(x=year,
                                                                      y=value,
                                                                      size="size",
                                                                      fill=color.and.shade.data.by,
                                                                      shape = shape.data.by))
        }
        
        rv = rv + ggplot2::scale_size_manual(values=c(size=2)) + ggplot2::guides(size="none")
        
        # Now create the shape scale, either for the first time or the second time
        if (!is.null(df.sim.groupids.one.member) && nrow(df.sim.groupids.one.member)>0)
            rv = rv + ggplot2::scale_shape_manual(name = "data shape", values = all.shapes.for.scale)
        else
            rv = rv + ggplot2::scale_shape_manual(name = "data shape", values = all.shapes.for.scale)
    }
    
    #----
    # If don't have a split.by, and thus only 1 color for sim, probably, then remove legend for it.
    if (style.manager$color.sim.by == 'stratum' && is.null(split.by))
        rv = rv + ggplot2::guides(color = "none")
    # browser()
    #-- FACET --#
    if (is.null(facet.by))
        facet.formula = as.formula("~outcome.display.name")
    else
        facet.formula = as.formula(paste0("~outcome.display.name + ", paste(sapply(seq_along(facet.by), function(i) {paste0("facet.by", i)}), collapse=" + ")))
    if (!is.null(df.sim) || !is.null(df.truth))
    {
        if (!is.null(n.facet.rows))
            rv = rv + ggplot2::facet_wrap(facet.formula, scales = 'free_y', nrow=n.facet.rows)
        else
            rv = rv + ggplot2::facet_wrap(facet.formula, scales = 'free_y')
    }
    # browser()
    
    if (plot.year.lag.ratio) rv = rv + ggplot2::xlab("latter year")
    
    rv
}