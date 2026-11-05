
DATA.MANAGER.CODE.ITERATION = 3

# Depends on:
# R6 package
# HELPERS_misc_helpers
# HELPERS_dim_name_helpers
# HELPERS_array_helpers

if (!exists('default.data.manager.holder'))
{
    default.data.manager.holder = new.env()
    default.data.manager.holder$default.data.manager = NULL
}

DATA.MANAGER.ONTOLOGY.ERRORS = new.env()
DATA.MANAGER.ONTOLOGY.ERRORS$did.you.remember.details = list()

# The metadata fields every data manager starts with. Names are field names; values are whether the field is required on every put
DATA.MANAGER.DEFAULT.METADATA.FIELDS = c(url = T, details = T)

#'@title Get Ontology Error Debug Info
#'@description Returns information useful for debugging ontology mapping errors
#'@export
get.ontology.error.debug.info <- function()
{
    DATA.MANAGER.ONTOLOGY.ERRORS$did.you.remember.details # there's not an issue returning an environment, is there?
}

#'@title Clear Ontology Error Debug Info
#'@export
clear.ontology.error.debug.info <- function()
{
    DATA.MANAGER.ONTOLOGY.ERRORS$did.you.remember.details = list()
}

##----------------------##
##-- PUBLIC INTERFACE --##
##----------------------##

#'@title Get the default data manager
#'
#'@export
get.default.data.manager <- function()
{
    default.data.manager.holder$default.data.manager
}

#'@title Set the default data manager
#'
#'@export
set.default.data.manager <- function(data.manager)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    default.data.manager.holder$default.data.manager = data.manager
}

#'@title Create a JHEEM Data Manager
#'
#'@param name The name of the data manager
#'@param description A short description
#'
#'@export
create.data.manager <- function(name,
                                description,
                                set.as.default = F)
{
    new.data.manager = JHEEM.DATA.MANAGER$new(name,
                                              description=description)
    if (set.as.default) default.data.manager.holder$default.data.manager = new.data.manager
    invisible(new.data.manager)
}

#'@title Load a cached data manager
#'
#'@param file A file storing a cached data manager object (only)
#'@param name A name for the local data manager. If NULL, the name of the cached data manager.
#'@param description A description for the local data manager. If NULL, the description of the cached data manager.
#'@param set.as.default Should the loaded data manager be set as the default data manager
#'@param provenance Optional: what the caller knows about the file's release, as a named list
#' with any of 'sha256', 'release' and 'repository'. Kept with the file's name and MD5 digest;
#' see \code{\link{get.data.manager.provenance}}
#'
#'@export
load.data.manager <- function(file,
                              name=NULL,
                              description=NULL,
                              set.as.default=F,
                              provenance=NULL)
{
    error.prefix = "Error loading cached data manager: "
    imported.object.names = suppressWarnings(load(file))
    if (length(imported.object.names) != 1) {
        stop(paste0(error.prefix, "'file' must store exactly one cached R object"))
    }
    
    loaded.data.manager = get(imported.object.names[[1]])
    
    if (!R6::is.R6(loaded.data.manager) || !is(loaded.data.manager, 'jheem.data.manager'))
        stop(paste0(error.prefix, "'file' must store an R6 object with class 'jheem.data.manager'"))
    
    copy.name = ifelse(is.null(name),
                       loaded.data.manager$name,
                       name)
    copy.description = ifelse(is.null(description),
                              loaded.data.manager$description,
                              description)
    new.data.manager = copy.data.manager(loaded.data.manager,
                                         name=copy.name,
                                         description=copy.description,
                                         preserve.original.creation.date=T,
                                         preserve.original.last.modified.date=T)
    set.data.manager.provenance(new.data.manager,
                                make.data.manager.provenance(file, provenance, error.prefix))
    
    if (set.as.default)
        default.data.manager.holder$default.data.manager = new.data.manager
    
    
    invisible(new.data.manager)
}

#'@title Create a copy of a JHEEM Data Manager
#'
#'@param data.manager A jheem.data.manager object to be copied
#'@param name The name of the new data manager
#'@param description A short description of the new data manager. If NULL, the description of the copied data manager.
#'@param set.as.default Should the new data manager be set as the default data manager
#'@param preserve.original.creation.date,preserve.original.last.modified.date Should the new data manager keep the copied data manager's creation date/last modified date (T) or set that date to now (F)?
#'
#'@export
copy.data.manager <- function(data.manager = get.default.data.manager(),
                              name,
                              description,
                              set.as.default=F,
                              preserve.original.creation.date=T,
                              preserve.original.last.modified.date=T)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    new.data.manager = JHEEM.DATA.MANAGER$new(name=name,
                                              description=description,
                                              copy.from.data.manager = data.manager,
                                              preserve.copied.data.manager.creation.date = preserve.original.creation.date,
                                              preserve.copied.data.manager.last.modified.date = preserve.original.last.modified.date)
    
    if (set.as.default) default.data.manager.holder$default.data.manager = new.data.manager
    invisible(new.data.manager)
}

#'@title Import data from another JHEEM Data Manager
#'
#'@param to.data.manager A jheem.data.manager object
#'@param from.data.manager A jheem.data.manager object
#'
#'@export
import.data <- function(to.data.manager = get.default.data.manager(),
                        from.data.manager)
{
    if (!R6::is.R6(to.data.manager) || !is(to.data.manager, 'jheem.data.manager'))
        stop("'to.data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    to.data.manager$import.data(from.data.manager)
}

#' @title Subset a JHEEM Data Manager
#' @param A jheem.data.manager object
#' @param outcomes.to.keep,sources.to.keep,ontologies.to.keep Character vectors of which outcomes, sources, and/or ontologies to retain data for.
#' @param incomplete.dimension.values.to.keep A list of which years, locations, etc. to retain data for. Must be character vectors with the dimensions as names.
#' @param retain.ontology.registrations Should registered ontologies be un-registered if no data remains that uses them?
#' @export
subset_data <- function(data.manager = get.default.data.manager(),
                        outcomes.to.keep=NULL,
                        sources.to.keep=NULL,
                        ontologies.to.keep=NULL,
                        incomplete.dimension.values.to.keep=NULL,
                        retain.ontology.registrations=T)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$subset.data(outcomes.to.keep=outcomes.to.keep,
                             sources.to.keep=sources.to.keep,
                             ontologies.to.keep=ontologies.to.keep,
                             incomplete.dimension.values.to.keep=incomplete.dimension.values.to.keep,
                             retain.ontology.registrations=retain.ontology.registrations)
}

#'@title Register a new data ontology to a data manager before putting data to it
#'
#'@param data.manager A jheem.data.manager object
#'@param name The name of the data group
#'@param ont An ontology object as created by \code{\link{ontology()}}
#'
#'@export
register.data.ontology <- function(data.manager = get.default.data.manager(),
                                   name,
                                   ont)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$register.ontology(name=name,
                                   ont=ont)
}

#'@title Register an outcome to a data manager before putting data for that outcome
#'
#'@param data.manager A jheem.data.manager object
#'@param outcome The name (a single character value) of the outcome. This is the 'internal' name by which the outcome will be referenced in accessing the data manager
#'@param metadata An object of class 'outcome.metadata', as created by \code{\link{create.outcome.metadata}} that contains information about how to display the outcome
#'@param denominator.outcome The denominator outcome type that should be used when aggregating data (taking a weighted average) for this outcome type. Must be a previously registered outcome with scale='non.negative.number'. Only applies if scale is 'rate', 'proportion', or 'time'
#'@param denominator.lags.by.one.year Flag indicating whether the denominator data is shifting one year earlier than this outcome's data. For example, data for this outcome in year 2020 will use the 2019 denominator data if this flag is set to TRUE.
#'@param overwrite A logical indicating whether the information on this outcome should overwrite previously-registered information about this outcome. However, this registration must include the same metadata$scale and denominator.outcome (ie, can't change the structure of the outcome, only the display 'trappings')
#'@param allow.missing.denominator.outcome A logical allowing the user to register an outcome of scale 'rate', 'proportion' or 'time' without supplying a denominator outcome
#'
#'@export
register.data.outcome <- function(data.manager = get.default.data.manager(),
                                  outcome,
                                  metadata,
                                  denominator.outcome=NULL,
                                  denominator.lags.by.one.year=F,
                                  overwrite = F,
                                  allow.missing.denominator.outcome = F)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$register.outcome(outcome = outcome,
                                  metadata = metadata,
                                  denominator.outcome = denominator.outcome,
                                  denominator.lags.by.one.year = denominator.lags.by.one.year,
                                  overwrite = overwrite,
                                  allow.missing.denominator.outcome = allow.missing.denominator.outcome)
}

#'@title Register a data source to a data manager before putting data from that source
#'
#'@param data.manager A jheem.data.manager object
#'@param source The name (a single character value) of the source. This is the 'internal' name by which the source will be referenced in accessing the data manager
#'@param parent.source A parent source registered with this data manager that reflects the origin of the data this source uses
#'@param full.name A descriptive, fully-formatted and capitalized name to use in generating figures and tables (eg in popovers). Avoid abbreviations. Should be unique to the data source (although this is not enforced)
#'@param short.name A name for the data source to use in setting where brevity is important. Ideal to use abbreviations, but can be the same as full.name
#'
#'@details Note: a source is conceived such that one source cannot contain two sets of data for the same set of dimension values
#'
#'@export
register.data.source <- function(data.manager = get.default.data.manager(),
                                 source,
                                 parent.source,
                                 full.name,
                                 short.name = full.name)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$register.source(source=source,
                                 parent.source=parent.source,
                                 full.name=full.name,
                                 short.name=short.name)
}

#'@title Register a parent data source to a data manager
#'
#'@param data.manager A jheem.data.manager object
#'@param parent.source The name (a single character value) of the parent source. This is the 'internal' name by which the parent source will be referenced in accessing the data manager
#'@param full.name A descriptive, fully-formatted and capitalized name to use in generating figures and tables (eg in popovers). Avoid abbreviations. Should be unique to the parent data source (although this is not enforced)
#'@param short.name A name for the parent data source to use in setting where brevity is important. Ideal to use abbreviations, but can be the same as full.name
#'
#'@export
register.parent.data.source <- function(data.manager = get.default.data.manager(),
                                        parent.source,
                                        full.name,
                                        short.name = full.name)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$register.source(parent.source=parent.source,
                                 full.name=full.name,
                                 short.name=short.name)
}

#'@title Register a metadata field to a data manager
#'
#'@param data.manager A jheem.data.manager object
#'@param name The name (a single character value) of the field, such as "author" or "year"
#'@param required A logical indicating whether every subsequent put must supply a value for this field. Data already in the data manager are not affected
#'
#'@details Every data manager starts with the fields "url" and "details", both required
#'
#'@export
register.data.metadata.field <- function(data.manager = get.default.data.manager(),
                                         name,
                                         required = F)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")

    data.manager$register.metadata.field(name = name,
                                         required = required)
}

#'@title Put data into a data manager
#'
#'@inheritParams distribute.dimension.values
#'@param data.manager A jheem.data.manager object
#'@param data A numeric array or scalar value containing the data to store. If it is an array, it must have named dimnames set
#'@param outcome The outcome type for the data. Must be an outcome previously registered with \code{\link{register.data.outcome}}
#'@param metric The type of measurement. The default value is "estimate", but other options include "cv", "variance", and "sd".
#'@param ontology.name The name of the ontology which the data follow. Must be an ontology previously registered with \code{\link{register.data.ontology}}
#'@param dimension.values A named list that indicates what subset of a bigger data element these particular data should be stored into. Each element must be a named character, numeric, or logical vector.
#'@param source The (single) character name of the source from which these data derive. Must be a source previously registered with \code{\link{register.data.source}} Note: a source is conceived such that one source cannot contain two sets of data for the same set of dimension values
#'@param url A character vector with at least one element giving one or more URLs indicating where the data derive from
#'@param details A character vector with at least one element giving one or more URLs giving data collection details. In general, data collected and tabulated using the same approach should have the same details, whereas data with different methods/tabulation should have different details
#'@param metadata A named list of metadata describing every data point in this put, such as list(url=..., details=..., author="CDC", year="2021"). Names must be fields registered with \code{\link{register.data.metadata.field}} ("url" and "details" are always registered). Each element must be a character (or numeric) vector with no NA or empty values. 'url' and 'details' may be given here or in their own arguments, but not both with different values
#'@param allow.na.to.overwrite A logical indicator for whether NA values in data should be allowed to overwrite previous values put to the data manager (if data have been put previously)
#'
#'@export
put.data <- function(data.manager = get.default.data.manager(),
                     data,
                     outcome,
                     metric,
                     source,
                     ontology.name,
                     dimension.values=NULL,
                     url=NULL,
                     details=NULL,
                     metadata=NULL,
                     allow.na.to.overwrite=F,
                     dimension.values.to.distribute=list(),
                     allow.distributing.on.proportion.scale=F,
                     round.distributed.dimension.values=T,
                     debug=F,
                     printouts=F)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$put(data=data,
                     outcome=outcome,
                     metric=metric,
                     source=source,
                     ontology.name=ontology.name,
                     dimension.values=dimension.values,
                     url=url,
                     details=details,
                     metadata=metadata,
                     allow.na.to.overwrite=allow.na.to.overwrite,
                     dimension.values.to.distribute = dimension.values.to.distribute,
                     allow.distributing.on.proportion.scale = allow.distributing.on.proportion.scale,
                     round.distributed.dimension.values = round.distributed.dimension.values,
                     debug=F,
                     printouts=printouts)
}

#'@title Put long-form data into a data manager
#'
#'#'@inheritParams distribute.dimension.values
#'@inheritParams put.data
#'@param data A data frame or other 2-dimensional data structure with named columns. Must contain a column named 'value' of numeric values. If the 'source' argument is NULL, then data must also have a column named 'source' that gives the source for each row. May contain additional columns with names matching the specified ontology, which have the dimension values for each row. Any other columns are ignored
#'
#'@export
put.data.long.form <- function(data.manager = get.default.data.manager(),
                               data,
                               outcome,
                               metric,
                               source,
                               ontology.name,
                               dimension.values=NULL,
                               url=NULL,
                               details=NULL,
                               metadata=NULL,
                               allow.na.to.overwrite=F,
                               dimension.values.to.distribute=list(),
                               allow.distributing.on.proportion.scale=F,
                               round.distributed.dimension.values=T,
                               debug=F,
                               printouts=F)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")

    data.manager$put.long.form(data=data,
                               outcome=outcome,
                               metric=metric,
                               source=source,
                               ontology.name=ontology.name,
                               dimension.values=dimension.values,
                               url=url,
                               details=details,
                               metadata=metadata,
                               allow.na.to.overwrite=allow.na.to.overwrite,
                               dimension.values.to.distribute = dimension.values.to.distribute,
                               allow.distributing.on.proportion.scale = allow.distributing.on.proportion.scale,
                               round.distributed.dimension.values=round.distributed.dimension.values,
                               debug=F,
                               printouts=printouts)
}

#'@title Pull data from a data manager
#'
#'@param data.manager A jheem.data.manager object
#'@param outcome The outcome type for the data. Must be an outcome previously registered to this data manager with \code{\link{register.data.outcome}}
#'@param metric The type of measurement. The default value is "estimate", but other options include "cv", "variance", and "sd".
#'@param keep.dimensions The dimensions that should be retained in the returned value
#'@param dimension.values A named list, indicating which values for specific dimensions to pull data for. Each element must be a named character, numeric, or logical vector.
#'@param sources The data sources from which to pull data (if available). If NULL, will pull from all data sources that have any relevant data
#'@param include.sources.without.data.in.output When 'sources' is not NULL, include all sources in 'sources' in the output array even if no data was found for some, or even all, sources. If 'sources' is NULL, will include all registered sources for the outcome in the output array.
#'@param target.ontology Optional argument, indicating the ontology according to which results are desired. The data manager will apply an ontology mapping (if it can) to align its data to the desired ontology
#'@param allow.mapping.from.target.ontology A logical indicator. If TRUE, if target.ontology is specified, but the data manager does not have data that can be mapped to the target ontology, it will search for data such that an ontology.mapping can be applied to data in the target.ontology that make those data align with the data pulled
#'@param from.ontology.names The names of the ontologies from which to pull data (if available). If NULL, will pull from all ontologies that have any relevant data and can be mapped to the requested ontology. Must refer to ontologies previously registered to this data manager with \code{\link{register.data.ontology}}
#'@param append.attributes A character vector indicating requested data attributes to include with the result. May contain "url", "details", the name of any other registered metadata field, and/or "metadata"
#'
#'@param na.rm Whether to disregard NA values when aggregating out dimensions not in keep.dimensions
#'@param ignore.ontologies.without.requested.locations If 'dimension.values' includes a location dimension, should ontologies that don't have any of those locations for the requested outcome be ignored? See detailed documentation about how ontologies factor into the output. (NOT YET WRITTEN)
#'@param ... Optional alternative way to specify dimension.values. Each element must be a named character, numeric, or logical vector.
#'
#'@return A numeric array with named dimnames, with one dimension for each value of 'keep.dimensions', plus an additional dimension, "source" at the beginning, with one value for each source from which data were pulled
#' The return value may have several attributes:
#' (1) If target.ontology is not-NULL and allow.mapping.from.target.ontology==T, 'ontology.mapping' with refer to an ontology.mapping object that can be applied to data in the target.ontology to make them align with the returned array
#' (2) If append.attributes includes "url", an array with the same dimensions as the returned array, where each element is one or more URLs separated by "__"
#' (3) If append.attributes includes "details", an with the same dimensions as the returned array, where each element is one or more details about data collection separated by "__"
#' (4) For any other metadata field named in append.attributes (eg "author"), a character array of the same form as for "url"
#' (5) If append.attributes includes "metadata", 'metadata.ids', a list array with the same dimensions as the returned array where each element is an integer vector of metadata record IDs (NA where the data are NA),
#'     and 'metadata', a data frame of the records those IDs refer to (as returned by the data manager's get.metadata.records() method)
#' For all of these, an element is NA wherever the returned data are NA
#'
#'@export
pull.data <- function(data.manager = get.default.data.manager(),
                      outcome,
                      metric = 'estimate',
                      keep.dimensions = NULL,
                      dimension.values = NULL,
                      sources = NULL,
                      from.ontology.names = NULL,
                      exclude.ontology.names = NULL,
                      target.ontology = NULL,
                      allow.mapping.from.target.ontology = T,
                      append.attributes = NULL,
                      append.metrics = NULL,
                      allow.other.sources.for.denominator = F,
                      na.rm = F,
                      ignore.ontologies.without.requested.locations = T,
                      check.arguments = T,
                      debug = F,
                      ...)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$pull(outcome = outcome,
                      metric = metric,
                      keep.dimensions = keep.dimensions,
                      dimension.values = dimension.values,
                      sources = sources,
                      from.ontology.names = from.ontology.names,
                      target.ontology = target.ontology,
                      allow.mapping.from.target.ontology = allow.mapping.from.target.ontology,
                      append.attributes = append.attributes,
                      append.metrics = append.metrics,
                      allow.other.sources.for.denominator = allow.other.sources.for.denominator,
                      na.rm = na.rm,
                      ignore.ontologies.without.requested.locations = ignore.ontologies.without.requested.locations,
                      check.arguments = check.arguments,
                      debug = debug,
                      ...)
}

#' @title Remove Data From Data Manager
#' @description Replaces data with NA if data exists
#' @inheritParams put.data
#' @param details.for.removal,url.for.removal Single character values to use for these points' 'details' and 'url'
#' @export
remove.data = function(data.manager,
                       outcome,
                       source,
                       ontology.name,
                       dimension.values,
                       metric = 'estimate',
                       details.for.removal='removed',
                       url.for.removal='url') {
    
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    # Put NAs with overwrite=T
    data.manager$put(as.numeric(NA),
                     outcome=outcome,
                     metric=metric,
                     source=source,
                     ontology.name=ontology.name,
                     dimension.values=dimension.values,
                     details=details.for.removal,
                     url=url.for.removal,
                     allow.na.to.overwrite = T,
                     is.removal = T
    )
    
}

#'@title Get pretty names for outcomes
#'
#'@details Gets the pretty.names, labels, or descriptions registered for the outcomes with the data manager
#'
#'@param data.manager A jheem.data.manager object
#'@param outcomes A character vector with the names of outcomes previously registered to this data manager
#'
#'@return A character vector of pretty names, labels, or descriptions
#'
#'@export
get.data.outcome.pretty.names <- function(data.manager = get.default.data.manager(), outcomes)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$get.outcome.pretty.names(outcomes)
}

#'@describeIn get.data.outcome.pretty.names Get the labels for outcomes
#'@export
get.data.outcome.labels <- function(data.manager = get.default.data.manager(), outcomes)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$get.outcome.labels(outcomes)
}

#'@describeIn get.data.outcome.pretty.names Get descriptions for outcomes
#'@export
get.data.outcome.descriptions <- function(data.manager = get.default.data.manager(), outcomes)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$get.outcome.descriptions(outcomes)
}

#'@title Get full names for data sources
#'
#'@details Gets the full.names or short.names for the data sources in the data manager
#'
#'@param data.manager A jheem.data.manager object
#'@param sources A character vector with the names of sources previously registered to this data manager
#'
#'@return A character vector of full names or short names
#'
#'@export
get.data.source.full.names <- function(data.manager = get.default.data.manager(), sources)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$get.source.full.names(sources)
}

#'@describeIn get.data.source.full.names Get short names for data sources
#'@export
get.data.source.short.names <- function(data.manager = get.default.data.manager(), sources)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$get.source.short.names(sources)
}

#'param ontology.name The name of an ontology (must be registered in the data manager)
#'
#'@export
get.registered.ontology <- function(data.manager = get.default.data.manager(), ontology.name)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    
    data.manager$get.registered.ontology(ontology.name)
}

#'@title Get year bounds for outcome
#'
#'@details Gets the earliest and latest years for which a data manager has data for a given outcome and metric. Searches all sources and all ontologies that haven't been specifically excluded
#'
#'@param outcome The outcome for which to find earliest and latest years of data (must be registered in the data manager)
#'@param metric The type of measurement to find earliest and latest years of data within the outcome. The default value is "estimate", but other options include "cv", "variance", and "sd".
#'@param exclude.ontology.names The names of ontologies to exclude from this search. Must be NULL or a character vector with no NA's or empty values.
#'
#'@export
get.year.bounds.for.outcome <- function(data.manager = get.default.data.manager(), outcome, metric='estimate', exclude.ontology.names = NULL)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    data.manager$get.year.bounds.for.outcome(outcome,
                                             metric=metric,
                                             exclude.ontology.names=exclude.ontology.names)
}

#'@title Get locations with data for an outcome
#'
#'@details Get locations that have data for a given outcome, metric and set of years. Searches all sources and all ontologies that haven't been specifcally excluded
#'
#'@inheritParams get.year.bounds.for.outcome
#'@param years The years to include. May be NULL to search all available years, or a numeric vector with no NA's or duplicates.
#'@export
get.locations.with.data <- function(data.manager = get.default.data.manager(), outcome, metric='estimate', years, exclude.ontology.names = NULL)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    data.manager$get.locations.with.data(outcome, metric=metric, years=years)
}

#' @title Inspect marginal sums between stratifications
#' @description Compares the percentage difference between the marginal sums of 
#' more stratified data and the data it corresponds to in more aggregated 
#' stratifications of the same source and ontology.
#'  
#' @inheritParams put.data
#' @param ontology The name of a registered ontology
#' @param sub.stratification The name of a less-aggregated stratification
#' @param super.stratification The name of a more-aggregated stratification
#' @param years,locations Show results for certain years (numeric, non-NA vector) or locations (character, non-NA vector)
#' @param threshold.percent A percentage below which discrepancies will not be reported (default 10).
#' @param threshold.magnitude A magnitude of difference between marginal sums and more-aggregated data below which discrepancies will not be reported (default 50).
#' @param check.up.to.n.way.stratified An integer greater than 0 indicating the highest level of stratification to report marginal sum discrepancies for (default 1).
inspect.marginals <- function(data.manager = get.default.data.manager,
                              outcome=NULL,
                              source=NULL,
                              ontology=NULL,
                              sub.stratification=NULL,
                              super.stratification=NULL,
                              years=NULL,
                              locations=NULL,
                              threshold.percent = 10,
                              threshold.magnitude = 50,
                              check.up.to.n.way.stratified = 1) {
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("'data.manager' must be an R6 object with class 'jheem.data.manager'")
    data.manager$inspect_marginals(outcome=outcome,
                                   source=source,
                                   ontology=ontology,
                                   sub.stratification=sub.stratification,
                                   super.stratification=super.stratification,
                                   years=years,
                                   locations=locations,
                                   threshold.percent = threshold.percent,
                                   threshold.magnitude = threshold.magnitude,
                                   check.up.to.n.way.stratified = check.up.to.n.way.stratified)
}

##----------------------##
##-- CLASS DEFINITION --##
##----------------------##



JHEEM.DATA.MANAGER = R6::R6Class(
    'jheem.data.manager',
    
    public = list(
        check = function()
        {
            browser()
        },
        
        initialize = function(name, description, copy.from.data.manager=NULL, preserve.copied.data.manager.creation.date=T, preserve.copied.data.manager.last.modified.date=T)
        {
            # Validate arguments
            # *name* is a single, non-empty, non-NA character value
            if (!is.character(name) || length(name)!=1 || is.na(name) || nchar(name)==0)
                stop(paste0(error.prefix, "'name' must be a single, non-empty, non-NA character value"))
            
            # *description* must be a single, non-empty, non-NA character value
            if (!is.character(description) || length(description)!=1 || is.na(description) || nchar(description)==0)
                stop(paste0(error.prefix, "'description' must be a single, non-empty, non-NA character value"))
            
            # Store them
            private$i.name = name
            private$i.description = description
            
            
            private$i.code.iteration = DATA.MANAGER.CODE.ITERATION
            
            if (is.null(copy.from.data.manager))
            {
                # Create the empty values for other member variables
                private$i.data = list()
                private$i.url = list()
                private$i.details = list()
                private$i.url.list = character(0)
                private$i.details.list = character(0)

                private$i.metadata = list()
                private$i.metadata.fields = DATA.MANAGER.DEFAULT.METADATA.FIELDS
                private$i.metadata.records = list()
                private$i.metadata.keys = character(0)

                private$i.outcome.info = list()
                private$i.ontologies = list()
                private$i.parent.source.info = list()
                private$i.source.info = list()
            }
            else
            {
                #               if (is.null(copy.from.data.manager$code.iteration) ||
                #                   private$i.code.iteration != copy.from.data.manager$code.iteration)
                #               {
                #                   stop("Cannot copy from data manager: the code iterations are incompatible")
                #               }
                
                private$i.data = copy.from.data.manager$data
                private$i.url = copy.from.data.manager$url
                private$i.details = copy.from.data.manager$details
                private$i.url.list = unlist(copy.from.data.manager$url.list)
                private$i.details.list = unlist(copy.from.data.manager$details.list) # needed during rollout of vector form

                private$i.outcome.info = copy.from.data.manager$outcome.info
                private$i.ontologies = copy.from.data.manager$ontologies
                private$i.parent.source.info = copy.from.data.manager$parent.source.info
                private$i.source.info = copy.from.data.manager$source.info

                # Data managers saved before the metadata records existed have no 'metadata' binding, so build the records from their url and details
                if (is.null(copy.from.data.manager$metadata))
                    private$build.metadata.from.url.and.details()
                else {
                    private$i.metadata = copy.from.data.manager$metadata
                    private$i.metadata.fields = copy.from.data.manager$metadata.fields
                    private$i.metadata.records = copy.from.data.manager$metadata.records
                    private$i.metadata.keys = copy.from.data.manager$metadata.keys
                }
            }
            
            # Set creation date and last modified date to now; make backwards compatible
            if (!is.null(copy.from.data.manager) && preserve.copied.data.manager.creation.date)
                private$i.creation.date = copy.from.data.manager[['creation.date']]
            else
                private$i.creation.date = Sys.time()
            
            if (!is.null(copy.from.data.manager) && preserve.copied.data.manager.last.modified.date)
                private$i.last.modified.date = copy.from.data.manager[['last.modified.date']]
            else
                private$i.last.modified.date = Sys.time()
            
            # A copy keeps the identity of the manager it was copied from
            if (!is.null(copy.from.data.manager))
                private$i.provenance = get.data.manager.provenance(copy.from.data.manager)
        },
        
        import.data = function(from.data.manager)
        {
            error.prefix = "Can't import data from data manager: "
            if (!R6::is.R6(from.data.manager) || !is(from.data.manager, 'jheem.data.manager'))
                stop("'from.data.manager' must be an R6 object with class 'jheem.data.manager'")
            
            # register ontologies if necessary
            for (ontology.name in from.data.manager$ontology.names) {
                if (!(ontology.name %in% self$ontology.names))
                    self$register.ontology(ontology.name,
                                           from.data.manager$ontologies[[ontology.name]])
                else {
                    from.ont = from.data.manager$ontologies[[ontology.name]]
                    self.ont = self$ontologies[[ontology.name]]
                    # check that these two ontologies really are the same
                    if (!identical(from.ont[setdiff(names(from.ont), incomplete.dimensions(from.ont))],
                                   self.ont[setdiff(names(self.ont), incomplete.dimensions(self.ont))]))
                        stop(paste0(error.prefix, "The corresponding ontologies for '", ontology.name, "' do not have identical complete dimensions"))
                    if (!identical(names(from.ont), names(self.ont)))
                        stop(paste0(error.prefix, "The corresponding ontologies for '", ontology.name, "' do not have the same dimensions (in order) as each other"))
                    # The put() will handle updating the incomplete dimensions
                }
            }
            
            # register outcomes if necessary
            for (outcome.name in names(from.data.manager$outcome.info)) {
                if (!(outcome.name %in% names(self$outcome.info)))
                    self$register.outcome(outcome = from.data.manager$outcome.info[[outcome.name]][['outcome']],
                                          metadata = from.data.manager$outcome.info[[outcome.name]][['metadata']],
                                          denominator.outcome = from.data.manager$outcome.info[[outcome.name]][['denominator.outcome']])
            }
            
            # register parent sources if necessary
            for (parent.source.name in names(from.data.manager$parent.source.info)) {
                self$register.parent.source(parent.source = from.data.manager$parent.source.info[[parent.source.name]][['parent.source']],
                                            full.name = from.data.manager$parent.source.info[[parent.source.name]][['full.name']],
                                            short.name = from.data.manager$parent.source.info[[parent.source.name]][['short.name']])
            }
            
            # register sources if necessary
            for (source.name in names(from.data.manager$source.info)) {
                if (!(source.name %in% names(self$source.info)))
                    self$register.source(source = from.data.manager$source.info[[source.name]][['source']],
                                         parent.source = from.data.manager$source.info[[source.name]][['parent.source']],
                                         full.name = from.data.manager$source.info[[source.name]][['full.name']],
                                         short.name = from.data.manager$source.info[[source.name]][['short.name']])
            }
            
            # import data
            data = from.data.manager$data
            from.metadata = from.data.manager$metadata

            if (!is.null(from.metadata)) {
                # register any metadata fields we don't have yet (as optional, since our existing data lack them)
                for (field in setdiff(names(from.data.manager$metadata.fields), names(private$i.metadata.fields)))
                    self$register.metadata.field(field, required = F)
                from.records = from.data.manager$metadata.records

                for (outcome in names(data)) {
                    for (metric in names(data[[outcome]])) {
                        for (source in names(data[[outcome]][[metric]])) {
                            for (ontology in names(data[[outcome]][[metric]][[source]])) {
                                for (stratification in names(data[[outcome]][[metric]][[source]][[ontology]])) {

                                    # We must do one put PER metadata record, because every put() has exactly one record
                                    data.this.strat = data[[outcome]][[metric]][[source]][[ontology]][[stratification]]
                                    ids.this.strat = from.metadata[[outcome]][[metric]][[source]][[ontology]][[stratification]]

                                    for (one.id in unique(as.vector(ids.this.strat))) {
                                        if (is.na(one.id)) next
                                        array.indices = which(ids.this.strat==one.id)

                                        # Because we have to put arrays, not vectors corresponding to sporadic locations in an array, we fill one with NA everywhere else
                                        one.data = array(NA, dim(data.this.strat), dimnames(data.this.strat))
                                        one.data[array.indices] = data.this.strat[array.indices]

                                        self$put(data = one.data,
                                                 outcome=outcome,
                                                 metric=metric,
                                                 source=source,
                                                 ontology.name=ontology,
                                                 metadata=from.records[[one.id]],
                                                 allow.na.to.overwrite = F)
                                    }
                                }
                            }
                        }
                    }
                }

                # Modified
                private$i.last.modified.date = Sys.time()
                if (!is.null(private$i.provenance)) private$i.provenance$modified = TRUE
                return(invisible(self))
            }

            # The from.data.manager predates metadata records, so import using its url and details
            url = from.data.manager$url
            details = from.data.manager$details

            for (outcome in names(data)) {
                for (metric in names(data[[outcome]])) {
                    for (source in names(data[[outcome]][[metric]])) {
                        for (ontology in names(data[[outcome]][[metric]][[source]])) {
                            for (stratification in names(data[[outcome]][[metric]][[source]][[ontology]])) {
                                
                                # We must do one put PER unique pair of details/url, because every put() can have only one url and details
                                details.this.strat = details[[outcome]][[metric]][[source]][[ontology]][[stratification]]
                                url.this.strat = url[[outcome]][[metric]][[source]][[ontology]][[stratification]]
                                hashed.details.this.strat = unique(as.vector(details.this.strat))
                                hashed.url.this.strat = unique(as.vector(url.this.strat))
                                
                                for (one.details in hashed.details.this.strat) {
                                    for (one.url in hashed.url.this.strat) {
                                        if (is.na(one.details) || is.na(one.url)) next
                                        # browser()
                                        array.indices = which(details.this.strat==one.details & url.this.strat==one.url)
                                        
                                        array.indices[is.na(array.indices)] = F
                                        # Figure out what the details and url actually are in the from.data.manager
                                        unhashed.one.details = from.data.manager$unhash.details(one.details)
                                        unhashed.one.url = from.data.manager$unhash.url(one.url)
                                        
                                        # Because we have to put arrays, not vectors corresponding to sporadic locations in an array, we fill one with NA everywhere else
                                        one.data = array(NA,
                                                         dim(data[[outcome]][[metric]][[source]][[ontology]][[stratification]]),
                                                         dimnames(data[[outcome]][[metric]][[source]][[ontology]][[stratification]]))
                                        one.data[array.indices] = data[[outcome]][[metric]][[source]][[ontology]][[stratification]][array.indices]
                                        
                                        self$put(data = one.data,
                                                 outcome=outcome,
                                                 metric=metric,
                                                 source=source,
                                                 ontology.name=ontology,
                                                 url=unhashed.one.url,
                                                 details=unhashed.one.details,
                                                 allow.na.to.overwrite = F)
                                    }
                                }
                            }
                        }
                    }
                }
            }
            
            # Modified
            private$i.last.modified.date = Sys.time()
            if (!is.null(private$i.provenance)) private$i.provenance$modified = TRUE
            invisible(self)
            
        },
        
        subset.data = function(outcomes.to.keep=NULL,
                               sources.to.keep=NULL,
                               ontologies.to.keep=NULL,
                               incomplete.dimension.values.to.keep=NULL,
                               retain.ontology.registrations=T)
        {
            # Validate arguments ----
            
            error.prefix = paste0("Unable to subset data.manager '", private$i.name, "': ")
            
            if (!is.null(outcomes.to.keep)) {
                if (!is.character(outcomes.to.keep) || length(outcomes.to.keep)==0 || any(is.na(outcomes.to.keep)) || any(duplicated(outcomes.to.keep)))
                    stop(paste0(error.prefix, "'outcomes.to.keep' must be NULL or a character vector with no NAs or repeats"))
                if (!all(outcomes.to.keep %in% names(private$i.outcome.info)))
                    stop(paste0(error.prefix, "all outcomes in 'outcomes.to.keep' must be registered in the data manager"))
            }
            if (!is.null(sources.to.keep)) {
                if (!is.character(sources.to.keep) || length(sources.to.keep)==0 || any(is.na(sources.to.keep)) || any(duplicated(sources.to.keep)))
                    stop(paste0(error.prefix, "'sources.to.keep' must be NULL or a character vector with no NAs or repeats"))
                if (!all(sources.to.keep %in% names(private$i.source.info)))
                    stop(paste0(error.prefix, "all sources in 'sources.to.keep' must be registered in the data manager"))
            }
            if (!is.null(ontologies.to.keep)) {
                if (!is.character(ontologies.to.keep) || length(ontologies.to.keep)==0 || any(is.na(ontologies.to.keep)) || any(duplicated(ontologies.to.keep)))
                    stop(paste0(error.prefix, "'ontologies.to.keep' must be NULL or a character vector with no NAs or repeats"))
                if (!all(ontologies.to.keep %in% names(private$i.ontologies)))
                    stop(paste0(error.prefix, "all ontologies in 'ontologies.to.keep' must be registered in the data manager"))
            }
            
            kept.outcomes = outcomes.to.keep
            kept.sources = sources.to.keep
            kept.ontologies = ontologies.to.keep
            
            # *incomplete.dimension.values.to.keep* must be NULL or a named list of character vectors
            if (!is.null(incomplete.dimension.values.to.keep)) {
                if (!is.list(incomplete.dimension.values.to.keep) || is.null(names(incomplete.dimension.values.to.keep)) || any(is.na(names(incomplete.dimension.values.to.keep))))
                    stop(paste0(error.prefix, "'incomplete.dimension.values.to.keep' must be NULL or a named list of character vectors"))
                if (any(sapply(incomplete.dimension.values.to.keep, function(dimension) {
                    !is.character(dimension) || length(dimension)==0 || any(is.na(dimension) || any(duplicated(dimension)))
                })))
                    stop(paste0(error.prefix, "the elements of 'incomplete.dimension.values.to.keep' must be character vectors with no NAs or repeats"))
                if (!is.character(names(incomplete.dimension.values.to.keep)) || any(is.na(names(incomplete.dimension.values.to.keep))) || any(duplicated(names(incomplete.dimension.values.to.keep))))
                    stop(paste0(error.prefix, "the names of 'incomplete.dimension.values.to.keep' must be characters and contain no NAs or repeats. These correspond to the names of dimensions."))
            }
            
            if (is.null(outcomes.to.keep) && is.null(sources.to.keep) && is.null(ontologies.to.keep) && is.null(incomplete.dimension.values.to.keep))
                stop(paste0(error.prefix, "at least one of 'outcomes.to.keep', 'sources.to.keep', 'ontologies.to.keep' or 'incomplete.dimension.values.to.keep' must be used (non-NULL)"))
            
            if (!identical(retain.ontology.registrations, T) && !identical(retain.ontology.registrations, F))
                stop(paste0(error.prefix, "'retain.ontology.registrations' must be TRUE or FALSE"))
            
            
            # Subset data, url, details, metadata ----
            # (records in i.metadata.records that no longer have any data are left in place; they are harmless)
            for (data.type in c('i.data', 'i.url', 'i.details', 'i.metadata')) {
                
                used.outcomes = names(private[[data.type]])
                if (!is.null(kept.outcomes))
                    used.outcomes = intersect(kept.outcomes, used.outcomes)
                
                private[[data.type]] = lapply(used.outcomes, function(outcome.name) {
                    
                    used.metrics = names(private[[data.type]][[outcome.name]])
                    
                    rv.this.outcome = lapply(used.metrics, function(metric.name) {
                        
                        used.sources = names(private[[data.type]][[outcome.name]][[metric.name]])
                        if (!is.null(kept.sources))
                            used.sources = intersect(kept.sources, used.sources)
                        
                        rv.this.metric = lapply(used.sources, function(source.name) {
                            
                            used.ontologies = names(private[[data.type]][[outcome.name]][[metric.name]][[source.name]])
                            if (!is.null(kept.ontologies))
                                used.ontologies = intersect(kept.ontologies, used.ontologies)
                            
                            rv.this.source = lapply(used.ontologies, function(ontology.name) {
                                
                                used.stratifications = names(private[[data.type]][[outcome.name]][[metric.name]][[source.name]][[ontology.name]])
                                # browser()
                                
                                # Assume every stratification has all the incomplete dimensions for the ontology
                                if (!is.null(incomplete.dimension.values.to.keep)) {
                                    subset.dimension.values = incomplete.dimension.values.to.keep[names(incomplete.dimension.values.to.keep) %in% incomplete.dimensions(private$i.ontologies[[ontology.name]])]
                                    subset.dimensions = names(subset.dimension.values)
                                } else
                                    return(private[[data.type]][[outcome.name]][[metric.name]][[source.name]][[ontology.name]])
                                
                                rv.this.ontology = lapply(used.stratifications, function(stratification.name) {
                                    
                                    strat.data = private[[data.type]][[outcome.name]][[metric.name]][[source.name]][[ontology.name]][[stratification.name]]
                                    
                                    subset.dimension.values.this.strat = lapply(subset.dimensions, function(dimension) {
                                        
                                        intersect(dimnames(strat.data)[[dimension]], subset.dimension.values[[dimension]])
                                        
                                    })
                                    names(subset.dimension.values.this.strat) = subset.dimensions
                                    
                                    # If we have a dimension where the vals are length zero, it means we should cut out the whole stratfication because it doesn't have our location/year.
                                    
                                    if (all(sapply(subset.dimension.values.this.strat, length) > 0))
                                        fast.array.access(strat.data, subset.dimension.values.this.strat)
                                    else
                                        NULL
                                })
                                
                                # Now cut out stratifications that are NULL (didn't have our location/year). This may leave rv as "list()"
                                names(rv.this.ontology) = used.stratifications
                                rv.this.ontology = rv.this.ontology[!sapply(rv.this.ontology, is.null)]
                                
                            })
                            
                            # Now cut out ontologies that are empty lists (had no retained stratifications)
                            names(rv.this.source) = used.ontologies
                            rv.this.source = rv.this.source[sapply(rv.this.source, length) > 0]
                            
                        })
                        
                        # Now cut out sources that are empty lists (had no retained ontologies)
                        names(rv.this.metric) = used.sources
                        rv.this.metric = rv.this.metric[sapply(rv.this.metric, length) > 0]
                        
                    })
                    
                    # Now cut out metrics that are empty lists (had no retained sources)
                    names(rv.this.outcome) = used.metrics
                    rv.this.outcome = rv.this.outcome[sapply(rv.this.outcome, length) > 0]
                    
                })
                
                # Now cut out outcomes that are empty lists (had no retained metrics)
                names(private[[data.type]]) = used.outcomes
                private[[data.type]] = private[[data.type]][sapply(private[[data.type]], length) > 0]
                
            }
            
            # Update ontologies with union of incomplete dimension values ----
            ontology.names = names(private$i.ontologies)
            private$i.ontologies = lapply(names(private$i.ontologies), function(ontology.name) {
                
                updated.ontology = private$i.ontologies[[ontology.name]]
                
                # Go through each outcome, metric, source, -- this ontology --, and stratification to find values that are present post-subset
                for (incomplete.dimension in incomplete.dimensions(updated.ontology)) {
                    
                    updated.ontology[[incomplete.dimension]] = unique(unlist(lapply(private$i.data, function(outcome.data) {
                        unique(unlist(lapply(outcome.data, function(metric.data) {
                            unique(unlist(lapply(metric.data, function(source.data) {
                                if (ontology.name %in% names(source.data)) {
                                    unique(unlist(lapply(source.data[[ontology.name]], function(strat.data) {
                                        dimnames(strat.data)[[incomplete.dimension]]
                                    })))
                                }
                                else
                                    NULL
                            })))
                        })))
                    })))
                    
                }
                
                updated.ontology
                
            })
            names(private$i.ontologies) = ontology.names
            
            # Remove registrations if requested ----
            if (!retain.ontology.registrations) {
                used.ontology.names <- sort(unique(unlist(lapply(private$i.data, function(outcome) {
                    unique(unlist(lapply(outcome, function(metric) {
                        unique(unlist(lapply(metric, function(source) {
                            names(source)
                        })))
                    })))
                }))))
                private$i.ontologies <- private$i.ontologies[names(private$i.ontologies) %in% used.ontology.names]
            }
            
            # Return ----
            private$i.last.modified.date = Sys.time()
            if (!is.null(private$i.provenance)) private$i.provenance$modified = TRUE
            invisible(self)
        },
        
        register.ontology = function(name, ont)
        {
            error.prefix = "Cannot register.ontology: "
            # Validate arguments
            # - name is a single, non-empty, non-NA character value
            # - ont is an ontology
            if (!is.character(name) || length(name)!=1 || is.na(name))
                stop(paste0(error.prefix, "'name' must be a single, non-NA character value"))
            if (!is.ontology(ont))
                stop(paste0(error.prefix, "'ont' must be an ontology"))
            
            if (name %in% names(private$i.ontologies)) {
                if (identical(ont, private$i.ontologies[[name]]))
                    return(invisible(self))
                else
                    stop(paste0(error.prefix, "an ontology with this name has already been registered in this data manager"))
            }
            
            # If this name has not already been registered, store it
            
            # Todd's old instructions. I'm choosing not to do this because
            # it's too complex to modify the data stored under the old ontology.
            
            # If it has, make sure the new ontology is a 'superset'
            # - all dimensions in the old are still present in the new
            # - dimensions present in the old have the same values as in the new
            # - complete dimensions present in the old are still complete, ditto for incomplete
            # If it is a superset, then store it. Otherwise throw an error
            
            #@need to implement
            
            # Store it in sorted form
            sorted.ont = as.ontology(lapply(ont, function(d) {sort(d)}), incomplete.dimensions=incomplete.dimensions(ont))
            private$i.ontologies[[name]] = sorted.ont
            
            # Modified
            private$i.last.modified.date = Sys.time()
            if (!is.null(private$i.provenance)) private$i.provenance$modified = TRUE
            
            #-- Invisibly return the data manager for convenience --#
            invisible(self)
        },
        
        register.outcome = function(outcome,
                                    metadata,
                                    denominator.outcome=NULL,
                                    denominator.lags.by.one.year=NULL,
                                    overwrite=F,
                                    allow.missing.denominator.outcome = F)
        {
            #-- Validate arguments --#
            error.prefix = paste0("Unable to register outcome for data.manager '", private$i.name, "': ")
            
            # - outcome is a single, non-empty, non-NA character value
            if (!is.character(outcome) || length(outcome)!=1 || is.na(outcome) || nchar(outcome)==0)
                stop(paste0(error.prefix, "'outcome' must be a single, non-empty, non-NA character value"))
            error.prefix = paste0("Unable to register outcome '", outcome,
                                  "' for data.manager '", private$i.name, "': ")
            
            # - metadata is a 'outcome.metadata' object
            if (!is(metadata, "outcome.metadata"))
                stop(paste0(error.prefix, "'metadata' must be an object of class 'outcome.metadata' as returned by create.outcome.metadata()"))
            
            # *denominator.lags.by.one.year* is NULL or a single logical value (if *denominator.outcome* isn't NULL)
            if (!is.null(denominator.outcome) && !is.null(denominator.lags.by.one.year) && (!is.logical(denominator.lags.by.one.year) || length(denominator.lags.by.one.year)!=1 || is.na(denominator.lags.by.one.year)))
                stop(paste0(error.prefix, "if 'denominator.outcome' is supplied, then 'denominator.lags.by.one.year' must be NULL or a single, non-NA logical value"))
            
            if (!is.logical(overwrite) || length(overwrite)!=1 || is.na(overwrite))
                stop(paste0(error.prefix, "'overwrite' must be a single, non-empty, non-NA logical value"))
            if (!is.logical(allow.missing.denominator.outcome) || length(allow.missing.denominator.outcome)!=1 || is.na(allow.missing.denominator.outcome))
                stop(paste0(error.prefix, "'allow.missing.denominator.outcome' must be a single, non-empty, non-NA logical value"))
            
            # - If metadata$scale is 'rate', 'proportion', or 'time',
            #   denominator outcome is non-NULL.
            #   Otherwise it is NULL
            # - If non-NULL, denominator outcome:
            # -- is a single, non-empty, non-NA character value
            # -- corresponds to a previously-registered outcome with scale 'non.negative.number'
            
            if (metadata$scale %in% c('rate', 'proportion', 'time', 'ratio')) {
                if (!is.character(denominator.outcome) || length(denominator.outcome)!=1 || is.na(denominator.outcome) ||
                    is.null(private$i.outcome.info[[denominator.outcome]]) || private$i.outcome.info[[denominator.outcome]]$metadata$scale != 'non.negative.number')
                    stop(paste0(error.prefix, "outcomes with scale 'rate', 'proportion', 'time', or 'ratio' must have a 'denominator.outcome' that corresponds to a previously-registered outcome with scale 'non.negative.number'"))
            }
            else {
                if (!is.null(denominator.outcome))
                    stop(paste0(error.prefix, "'denominator.outcome' must be NULL for outcomes of scale '", metadata$scale, "'"))
            }
            
            # If this outcome has not previously been registered, store it
            # Otherwise, if scale and denominator outcome are the same as before, ignore.
            #   If different, throw an error
            
            previous.outcome.info = private$i.outcome.info[[outcome]]
            if (!is.null(previous.outcome.info))
            {
                if (metadata$scale!=previous.outcome.info$metadata$scale ||
                    (is.null(denominator.outcome) && !is.null(previous.outcome.info$denominator.outcome)) ||
                    (!is.null(denominator.outcome) && is.null(previous.outcome.info$denominator.outcome)) ||
                    (!is.null(denominator.outcome) && !is.null(previous.outcome.info$denominator.outcome) &&
                     denominator.outcome != previous.outcome.info$denominator.outcome))
                    stop(paste0(error.prefix, "The outcome '", outcome, "' was previously registered with a different scale and/or denominator outcome"))
            }
            
            if (metadata$scale %in% c('rate', 'proportion', 'time', 'ratio') && is.null(denominator.outcome) && !allow.missing.denominator.outcome)
                stop(paste0(error.prefix, "'denominator.outcome' must be supplied for scale ", metadata$scale))
            
            # Store it
            outcome.info = list(
                outcome = outcome,
                metadata = metadata,
                denominator.outcome = denominator.outcome,
                denominator.lags.by.one.year = denominator.lags.by.one.year
            )
            
            private$i.outcome.info[[outcome]] = outcome.info
            
            # Modified
            private$i.last.modified.date = Sys.time()
            if (!is.null(private$i.provenance)) private$i.provenance$modified = TRUE
            
            #-- Invisibly return the data manager for convenience --#
            invisible(self)
        },
        
        register.source = function(source,
                                   parent.source,
                                   full.name,
                                   short.name,
                                   overwrite=F)
        {
            #-- Validate arguments --#
            error.prefix = paste0("Unable to register source for data.manager '", private$i.name, "': ")
            
            # - source is a single, non-empty, non-NA character value
            if (!is.character(source) || length(source)!=1 || is.na(source) || nchar(source)==0)
                stop(paste0(error.prefix, "'source' must be a single, non-empty, non-NA character value"))
            error.prefix = paste0("Unable to register source '", source,
                                  "' for data.manager '", private$i.name, "': ")
            
            # - parent.source is a single, non-empty, non-NA character value and must be the name of a previously registered parent.source
            if (!is.character(source) || length(source)!=1 || is.na(source) || nchar(source)==0)
                stop(paste0(error.prefix, "'source' must be a single, non-empty, non-NA character value"))
            if (is.null(private$i.parent.source.info[[parent.source]]))
                stop(paste0(error.prefix, "'parent.source' must be a registered parent source with this data manager"))
            
            # - full.name is a single, non-empty, non-NA character value
            if (!is.character(full.name) || length(full.name)!=1 || is.na(full.name) || nchar(full.name)==0)
                stop(paste0(error.prefix, "'full.name' must be a single, non-empty, non-NA character value"))
            
            # - short.name is a single, non-empty, non-NA character value
            if (!is.character(short.name) || length(short.name)!=1 || is.na(short.name) || nchar(short.name)==0)
                stop(paste0(error.prefix, "'short.name' must be a single, non-empty, non-NA character value"))
            
            # If this source has not previously been registered, store it
            # Otherwise, if overwrite==T, store the new one
            # Otherwise, throw an error if full.name or short.name is different
            #   If different, throw an error
            
            previous.source.info = private$i.source.info[[source]]
            if (overwrite || is.null(previous.source.info))
            {
                source.info = list(
                    source = source,
                    parent.source = parent.source,
                    full.name = full.name,
                    short.name = short.name)
                
                private$i.source.info[[source]] = source.info
            }
            else if (previous.source.info$full.name != full.name || previous.source.info$short.name != short.name)
            {
                stop(paste0(error.prefix, "A data source named '", source, "' has already been registered. If you want to overwrite the previously registered source, use overwrite==T"))
            }
            
            # Modified
            private$i.last.modified.date = Sys.time()
            if (!is.null(private$i.provenance)) private$i.provenance$modified = TRUE
            
            #-- Invisibly return the data manager for convenience --#
            invisible(self)
        },
        
        register.parent.source = function(parent.source,
                                          full.name,
                                          short.name,
                                          overwrite=F)
        {
            #-- Validate arguments --#
            error.prefix = paste0("Unable to register parent source for data.manager '", private$i.name, "': ")
            
            # - parent.source is a single, non-empty, non-NA character value
            if (!is.character(parent.source) || length(parent.source)!=1 || is.na(parent.source) || nchar(parent.source)==0)
                stop(paste0(error.prefix, "'parent.source' must be a single, non-empty, non-NA character value"))
            error.prefix = paste0("Unable to register parent.source '", parent.source,
                                  "' for data.manager '", private$i.name, "': ")
            
            # - full.name is a single, non-empty, non-NA character value
            if (!is.character(full.name) || length(full.name)!=1 || is.na(full.name) || nchar(full.name)==0)
                stop(paste0(error.prefix, "'full.name' must be a single, non-empty, non-NA character value"))
            
            # - short.name is a single, non-empty, non-NA character value
            if (!is.character(short.name) || length(short.name)!=1 || is.na(short.name) || nchar(short.name)==0)
                stop(paste0(error.prefix, "'short.name' must be a single, non-empty, non-NA character value"))
            
            # If this parent source has not previously been registered, store it
            # Otherwise, if overwrite==T, store the new one
            # Otherwise, throw an error if full.name or short.name is different
            #   If different, throw an error
            
            previous.parent.source.info = private$i.parent.source.info[[parent.source]]
            if (overwrite || is.null(previous.parent.source.info))
            {
                parent.source.info = list(
                    parent.source = parent.source,
                    full.name = full.name,
                    short.name = short.name)
                
                private$i.parent.source.info[[parent.source]] = parent.source.info
            }
            else if (previous.parent.source.info$full.name != full.name || previous.parent.source.info$short.name != short.name)
            {
                stop(paste0(error.prefix, "A data parent source named '", parent.source, "' has already been registered. If you want to overwrite the previously registered parent source, use overwrite==T"))
            }
            
            # Modified
            private$i.last.modified.date = Sys.time()
            if (!is.null(private$i.provenance)) private$i.provenance$modified = TRUE
            
            #-- Invisibly return the data manager for convenience --#
            invisible(self)
        },

        register.metadata.field = function(name, required = F)
        {
            error.prefix = "Cannot register metadata field: "
            # *name* is a single, non-empty, non-NA character value, which can't be "metadata" because that is reserved in pull's 'append.attributes'
            if (!is.character(name) || length(name)!=1 || is.na(name) || nchar(name)==0)
                stop(paste0(error.prefix, "'name' must be a single, non-empty, non-NA character value"))
            if (name == 'metadata')
                stop(paste0(error.prefix, "'metadata' is reserved and cannot be the name of a field"))
            if (name %in% names(DATA.MANAGER.DEFAULT.METADATA.FIELDS))
                stop(paste0(error.prefix, "'", name, "' is a default field and cannot be re-registered"))

            # *required* is a single, non-NA logical value
            if (!is.logical(required) || length(required)!=1 || is.na(required))
                stop(paste0(error.prefix, "'required' must be a single, non-NA logical value"))

            private$i.metadata.fields[[name]] = required

            # Modified
            private$i.last.modified.date = Sys.time()
            if (!is.null(private$i.provenance)) private$i.provenance$modified = TRUE

            #-- Invisibly return the data manager for convenience --#
            invisible(self)
        },

        put = function(data,
                       outcome,
                       metric = 'estimate',
                       source,
                       ontology.name,
                       dimension.values=NULL,
                       url=NULL,
                       details=NULL,
                       metadata=NULL,
                       allow.na.to.overwrite=F,
                       dimension.values.to.distribute=list(),
                       allow.distributing.on.proportion.scale=F,
                       round.distributed.dimension.values=T,
                       is.removal=F,
                       debug=F,
                       printouts=F)
        {
            if (printouts) {
                dots = "........"
                puttime=Sys.time()
                print(paste0("Beginning put for outcome '", outcome, "', source '", source, "' at ", puttime))
            }
            if (debug) browser()
            #------------------------#
            #-- Validate arguments --#
            #------------------------#
            error.prefix = paste0("Unable to put data to data.manager '", private$i.name, "': ")
            # 1) *outcome* is a single, non-empty, non-NA character value
            #     Which corresponds to a registered outcome
            if (!is.character(outcome) || length(outcome)!=1 || is.na(outcome) || nchar(outcome)==0)
                stop(paste0(error.prefix, "'outcome' must be a single, non-empty, non-NA character value"))
            
            outcome.info = private$i.outcome.info[[outcome]]
            if (is.null(outcome.info))
                stop(paste0(error.prefix, "'", outcome, "' is not a registered outcome. Call register.data.outcome() to register it before putting data for that outcome"))
            
            error.prefix = paste0("Unable to put '", outcome, "' data to data.manager '", private$i.name, "': ")
            
            # 2) *ontology.name* is single, non-empty, non-NA character value
            #     Which refers to a registered ontology
            if (!is.character(ontology.name) || length(ontology.name)!=1 || is.na(ontology.name) || nchar(ontology.name)==0)
                stop(paste0(error.prefix, "'ontology.name' must be a single, non-empty, non-NA character value"))
            
            ont = private$i.ontologies[[ontology.name]]
            if (is.null(ont))
                stop(paste0(error.prefix, "'", ontology.name, "' is not a registered ontology. Call register.data.ontology() to register it before putting data for that ontology"))
            
            ont.dimensions.complete = is_complete(ont)
            
            error.prefix = paste0("Unable to put '", outcome, "' data to ontology '",
                                  ontology.name, "' in data.manager '", private$i.name, "': ")
            
            if (!is.logical(is.removal) || length(is.removal)!=1 || is.na(is.removal))
                stop(paste0(error.prefix, "'is.removal' must be T or F"))
            
            # 3) *data* is
            #   -- numeric
            #   -- appropriate for the scale of the outcome
            #   -- either a single scalar value, or an array with named dimnames
            #      where either
            #   --- the corresponding dimension in the ontology is incomplete OR
            #   --- the values are a subset of the values of the corresponding dimension in the ontology
            #   --- the dimension values for any dimension named "location" contains only valid locations
            if (!is.numeric(data))
                stop(paste0(error.prefix,
                            "'data' must be a numeric object"))
            if (length(data)==0)
                stop(paste0(error.prefix,
                            "'data' must have length > 0"))
            
            if (is.removal && (length(data)!=1 || !is.na(data)))
                stop(paste0(error.prefix,
                            "'data' must be 'NA' if 'is.removal' is TRUE"))
            
            ###
            if (metric %in% c('estimate', 'upper.bound', 'lower.bound'))
                do.check.values.for.model.scale(data, scale=outcome.info$metadata$scale,
                                                variable.name.for.error = 'data',
                                                error.prefix = error.prefix)
            else
                do.check.values.for.model.scale(data, scale = 'non.negative.number',
                                                variable.name.for.error = 'data',
                                                error.prefix = error.prefix)
            
            if (is.null(dimension.values)) dimension.values = list()
            
            # *dimension.values.to.distribute* must be a named list with
            # names that are dimensions of the array
            # and values that are in the dimnames of the array
            # and can only be used if the scale is non.negative.number
            # and the input is an array
            if (!is.list(dimension.values.to.distribute))
                stop(paste0(error.prefix, "'dimension.values.to.distribute' must be a named list; defaults to list()"))
            if (length(dimension.values.to.distribute) > 0 && !is.array(data))
                stop(paste0(error.prefix, "'dimension.values.to.distribute' can only be used when the data is an array; should be left as the default, list(), otherwise"))
            if ((length(dimension.values.to.distribute) > 0 && outcome.info$metadata$scale != 'non.negative.number') &&
                !allow.distributing.on.proportion.scale)
                stop(paste0(error.prefix, "'dimension.values.to.distribute' can only be used when the data has scale 'non.negative.number'; should be left as the default, list(), otherwise"))
            # We cannot distribute on incomplete dimensions
            if (any(names(dimension.values.to.distribute) %in% incomplete.dimensions(ont)))
                stop(paste0(error.prefix, "'dimension.values.to.distribute' cannot contain any incomplete dimensions"))
            
            # *round.distributed.dimension.values* is logical
            if (!is.logical(round.distributed.dimension.values) || length(round.distributed.dimension.values)!=1 || is.na(round.distributed.dimension.values))
                stop(paste0(error.prefix, "'round.distributed.dimension.values' must be a single, non-NA logical value"))
            
            if (is.array(data))
            {
                if (is.null(dimnames(data)))
                    stop(paste0(error.prefix,
                                "If 'data' is an array, then it must be an array with named dimnames"))
                
                if (is.null(names(dimnames(data))))
                    stop(paste0(error.prefix,
                                "If 'data' is an array, then it must be an array with NAMED dimnames"))
                
                # Distribute dimension values, if applicable, BEFORE checks on the data dimnames. Note that we won't distribute dimensions we don't have, but no error
                if (length(dimension.values.to.distribute) > 0) {
                    dvtd.names = intersect(names(dimension.values.to.distribute), names(dimnames(data)))
                    if (is.null(dvtd.names) || any(is.na(dvtd.names)))
                        stop(paste0(error.prefix, "'dimension.values.to.distribute' must be a named list"))
                    if (any(sapply(dvtd.names, function(d) {
                        length(dimension.values.to.distribute[[d]])==0 ||
                            any(is.na(dimension.values.to.distribute[[d]])) ||
                            any(duplicated(dimension.values.to.distribute[[d]]))
                    })))
                        stop(paste0(error.prefix, "the elements of 'dimension.values.to.distribute' must have positive length and contain no NAs or repeats"))
                    # Use only the values we actually have without throwing error if there are extras, which may leave us with none
                    dimension.values.to.distribute = lapply(dvtd.names, function(d) {
                        intersect(dimension.values.to.distribute[[d]], dimnames(data)[[d]])
                    })
                    names(dimension.values.to.distribute)=dvtd.names
                    dimension.values.to.distribute = dimension.values.to.distribute[sapply(dimension.values.to.distribute, length)>0]
                }
                
                if (length(dimension.values.to.distribute) > 0) {
                    data = distribute.dimension.values(data, dimension.values.to.distribute[names(dimension.values.to.distribute) %in% dvtd.names], round.distributed.dimension.values)
                    # We must now have exactly the dimension values in the ontology for distributed dimensions
                    if (any(sapply(dvtd.names, function(d) {
                        length(setdiff(ont[[d]], dimnames(data)[[d]]))>0
                    })))
                        stop(paste0(error.prefix, "'dimension.values.to.distribute' can only be used when the dataset contains all values in the ontology for relevant dimensions"))
                }
                
                invalid.dimensions = setdiff(names(dimnames(data)), names(ont))
                if (length(invalid.dimensions)>0)
                    stop(paste0(error.prefix,
                                "The dimensions of 'data' include ",
                                collapse.with.and("'", invalid.dimensions, "'"),
                                " which ",
                                ifelse(length(invalid.dimensions)==1, 'is', 'are'),
                                " not present in the '", ontology.name, "' ontology"))
                
                sapply(names(dimnames(data)), function(d){
                    if (ont.dimensions.complete[d])
                    {
                        invalid.values = setdiff(dimnames(data)[[d]], ont[[d]])
                        if (length(invalid.values)>0)
                            stop(paste0(error.prefix,
                                        "dimnames(data)[['", d, "']] contains value",
                                        ifelse(length(invalid.values)==0, '', 's'),
                                        collapse.with.and("'", invalid.values, "'"),
                                        " but ",
                                        ifelse(length(invalid.values)==0,
                                               "this is not a valid value",
                                               "these are not valid values"),
                                        " for the '", ontology.name, "' ontology's '", d, "' dimension"))
                    }
                })
                
                if ('location' %in% names(dimnames(data))) {
                    if (!all(sapply(dimnames(data)['location'], function(v) {locations::is.location.valid(v)} ))) {
                        stop(paste0(error.prefix, "some or all locations in data are invalid"))
                    }
                }
                
            }
            else if (length(data)!=1)
                stop(paste0(error.prefix,
                            "If 'data' is not a single (scalar) numeric value, then it must be an array with named dimnames"))
            
            #@Andrew Data cannot be missing any incomplete dimensions unless they are present in dimension.values
            missing.dimensions = setdiff(names(ont), c(names(dimnames(data)), names(dimension.values)))
            if (!all(ont.dimensions.complete[missing.dimensions]))
                stop(paste0(error.prefix, "either data or 'dimension.values' must contain any incomplete dimensions in the ontology"))
            
            # check dimension.values are valid
            # map them to character values
            if (printouts) {
                checktime = Sys.time()
                print(paste0(dots, "Performing checks at ", checktime))
            }
            dimension.values = resolve.ontology.dimension.values(ont = ont,
                                                                 dimension.values = dimension.values,
                                                                 error.prefix = paste0(error.prefix, "Error resolving 'dimension.values' - "))
            
            # If dimension.values contains dimension "location", check that every value is a valid location.
            if ("location" %in% names(dimension.values)) {
                if (!all(sapply(dimension.values['location'], function(v) {locations::is.location.valid(v)} ))) {
                    stop(paste0(error.prefix, "some or all locations in 'dimension.values' are invalid"))
                }
            }
            
            # *metric* is one of 'estimate', 'upper.bound', 'lower.bound', or 'coefficient.of.variance' until more options are added
            if (!is.character(metric) || length(metric) != 1 || !(metric %in% c('estimate', 'variance', 'standard.deviation', 'upper.bound', 'lower.bound', 'coefficient.of.variance')))
                stop(paste0(error.prefix, "'metric' must be a character vector with value 'estimate', 'variance', 'standard.deviation', 'upper.bound', 'lower.bound' or 'coefficient.of.variance'"))
            
            # 4) *source* is
            #    a single, non-NA, non-empty character value
            #    Which corresponds to a registered outcome
            if (!is.character(source))
                stop(paste0(error.prefix, "'source' must be a single character value"))
            if (length(source) != 1)
                stop(paste0(error.prefix, "'source' must be a SINGLE character value"))
            if (is.na(source))
                stop(paste0(error.prefix, "'source' cannot be NA"))
            if (nchar(source)=='')
                stop(paste0(error.prefix, "'source' cannot be empty ('')"))
            
            source.info = private$i.source.info[[source]]
            if (is.null(source.info))
                stop(paste0(error.prefix, "'", source, "' is not a registered data source. Call register.data.source() to register it before putting data from that source"))
            
            # 5) *metadata* (including *url* and *details*, which may be given in their own arguments) is
            #    a named list of registered fields, each of which is a character vector
            #    -- with at least one value
            #    -- with no NA values
            #    -- with no empty values
            #    and which contains every required field (including url and details)

            metadata.record = private$normalize.metadata.record(metadata = metadata,
                                                                url = url,
                                                                details = details,
                                                                error.prefix = error.prefix)
            url = metadata.record$url
            details = metadata.record$details

            # 6) *allow.na.to.overwrite* is a single, non-NA, logical value
            if (!is.logical(allow.na.to.overwrite) || length(allow.na.to.overwrite)!=1 || is.na(allow.na.to.overwrite))
                stop(paste0(error.prefix, "'allow.na.to.overwrite' must be a single, non-NA logical value"))
            
            #--------------------------#
            #-- Hash url and details --#
            #--------------------------#
            if (printouts) {
                hashtime = Sys.time()
                print(paste0(dots, "Checktime took ", hashtime-checktime))
            }
            url.hashed = paste0(sort(url), collapse='__')
            details.hashed = paste0(sort(details), collapse='__')
            if (url.hashed %in% private$i.url.list) url = which(private$i.url.list==url.hashed)
            else {
                private$i.url.list = c(private$i.url.list, url.hashed)
                url = length(private$i.url.list)
            }
            if (details.hashed %in% private$i.details.list) details = which(private$i.details.list==details.hashed)
            else {
                private$i.details.list = c(private$i.details.list, details.hashed)
                details = length(private$i.details.list)
            }
            url = as.integer(url)
            details = as.integer(details) # don't know why which() and length() didn't already make it integer...

            # One record ID for every data point in this put
            metadata.id = private$get.or.add.metadata.record(metadata.record)
            if (printouts) {
                phase3time = Sys.time()
                print(paste0(dots, "Hashtime took ", phase3time - hashtime))
            }
            
            #--------------------------------#
            #-- Set up to receive the data --#
            #--------------------------------#
            
            data.element.names = c('i.data','i.url','i.details','i.metadata')
            metadata.element.names = data.element.names[-1]
            
            # Figure out what stratification it goes in as the union of:
            # (1) dimensions in the data
            # (2) dimensions in dimension.values
            
            stratification = private$get.required.stratification(dimension.values = dimension.values,
                                                                 data = data,
                                                                 ontology.name = ontology.name,
                                                                 return.as.dimensions = F)
            if (printouts) {
                prepdntime = Sys.time()
                print(paste0(dots, "getting required stratification took ", prepdntime-phase3time))
            }
            # What dim.names do we need to accommodate the new data?
            put.dim.names = private$prepare.put.dim.names(outer.join.dim.names(if (is.array(data)) dimnames(data) else list(), dimension.values),
                                                          ontology.name = ontology.name)
            if (printouts) {
                reqstratdimtime = Sys.time()
                print(paste0(dots, "preparing put dimnames took ", reqstratdimtime-prepdntime))
            }
            
            # Check that our data doesn't have year ranges if our ontology has single years or vice versa.
            
            if (!is.null(ont$year)) {
                if (all(is.year.range(ont$year)) && any(!is.year.range(put.dim.names$year)))
                    stop(paste0(error.prefix, "the data has single years while the existing ontology has year ranges"))
                if (all(!is.year.range(ont$year)) && any(is.year.range(put.dim.names$year)))
                    stop(paste0(error.prefix, "the data has year ranges while the existing ontology has single years"))
            }
            
            ## @AZ re-order the put.dim.names to ensure it has the same order of dimensions as the stratification does.
            stratification.dimensions = private$get.required.stratification(dimension.values = dimension.values,
                                                                            data = data,
                                                                            ontology.name = ontology.name,
                                                                            return.as.dimensions = T)
            put.dim.names = put.dim.names[stratification.dimensions]
            if (printouts) {
                metrictime = Sys.time()
                print(paste0(dots, "getting required strat dimensions took ", metrictime-reqstratdimtime))
                print(paste0(dots, "Phase three stuff took ", metrictime-phase3time))
            }
            # if (debug) browser()
            ## ANDREW'S NEW LOGIC TO ACCOMMODATE MULTIPLE METRICS AND ENSURING ALIGNED DIMNAMES AMONG ALL
            existing.dim.names.this.metric = dimnames(private$i.data[[outcome]][[metric]][[source]][[ontology.name]][[stratification]])
            data.already.present.this.metric = !is.null(existing.dim.names.this.metric)
            
            # Here, if this put is intended for removal, we will limit our put dim names to the intersection with existing dim names.
            if (is.removal) {
                put.dim.names = intersect.shared.dim.names(put.dim.names, existing.dim.names.this.metric)
                dimension.values = put.dim.names
            }
            
            all.metric.names = names(private$i.data[[outcome]])
            existing.dim.names = lapply(names(private$i.data[[outcome]]), function(metr) {
                dimnames(private$i.data[[outcome]][[metr]][[source]][[ontology.name]][[stratification]])
            })
            names(existing.dim.names) = names(private$i.data[[outcome]])
            if (length(existing.dim.names) > 0)
                existing.dim.names = existing.dim.names[sapply(existing.dim.names, function(metr.dimnames) {!is.null(metr.dimnames)})]
            metrics.with.data = names(existing.dim.names)
            
            data.already.present = length(existing.dim.names) > 0
            
            # find aligning dimnames
            dimnames.aligning.all.metrics = put.dim.names
            if (data.already.present) {
                dimnames.aligning.all.metrics = private$prepare.put.dim.names(do.call(outer.join.dim.names, c(existing.dim.names, list(put.dim.names))), ontology.name)
            }
            if (printouts) {
                backuptime = Sys.time()
                print(paste0(dots, "Metric stuff took ", backuptime-metrictime))
            }
            if (data.already.present && !dim.names.equal(existing.dim.names, dimnames.aligning.all.metrics))
            {
                # Backup old data
                existing.data.and.metadata = lapply(data.element.names, function(name) {
                    data.this.element = lapply(metrics.with.data, function(metr) {
                        private[[name]][[outcome]][[metr]][[source]][[ontology.name]][[stratification]]
                    })
                    names(data.this.element) = metrics.with.data
                    return(data.this.element)
                })
                names(existing.data.and.metadata) = data.element.names
                
                # Make the new (empty) data structures... make it for this metric too, even if it's not one of the existing ones
                for (name in data.element.names) {
                    for (metr in union(metrics.with.data, metric))
                        private[[name]][[outcome]][[metr]][[source]][[ontology.name]][[stratification]] =
                            array(NaN, dim=sapply(dimnames.aligning.all.metrics, length), dimnames = dimnames.aligning.all.metrics)
                }
                if (printouts) {
                    print(paste0(dots, "Performing array access..."))
                }
                # Overwrite the new structure with the old data
                for (name in data.element.names) {
                    for (metr in metrics.with.data) {
                        array.access(private[[name]][[outcome]][[metr]][[source]][[ontology.name]][[stratification]], existing.dim.names[[metr]]) =
                            existing.data.and.metadata[[name]][[metr]]
                    }
                }
            }
            else if (!data.already.present)
            {
                # Make the new (empty) data structures
                for (name in data.element.names) {
                    private[[name]][[outcome]][[metric]][[source]][[ontology.name]][[stratification]] =
                        array(NaN, dim=sapply(dimnames.aligning.all.metrics, length), dimnames = dimnames.aligning.all.metrics)
                }
            }
            
            # Update ontology
            for (d in names(dimnames.aligning.all.metrics)) {
                private$i.ontologies[[ontology.name]][[d]] = sort(union(private$i.ontologies[[ontology.name]][[d]], dimnames.aligning.all.metrics[[d]]))
            }
            
            #-- Put the data and its metadata --#
            
            # get the indices we're going to write into
            #@Andrew get.array.access.indices makes an array full of 1:prod(sapply(arr.dim.names, length))
            #@ then it calls fast.array.access with that array and the data dimnames and dimension.values
            #@ fast.array.access loops five times if five dimensions (location, year, age, risk, sex...) with lapply
            #@ within fast.array.access, subset.values is a list of length(dims) and each element has all the dim values
            # browser()
            if (printouts) {
                finalputtime = Sys.time()
                print(paste0(dots, "Backup took ", finalputtime-backuptime))
            }
            
            overwrite.indices = get.array.access.indices(arr.dim.names = dimnames(private$i.data[[outcome]][[metric]][[source]][[ontology.name]][[stratification]]),
                                                         dimension.values = c(dimnames(data), dimension.values))
            if (!allow.na.to.overwrite)
            {
                overwrite.indices = overwrite.indices[!is.na(data)]
                data = data[!is.na(data)]
            }
            
            # Put data
            private$i.data[[outcome]][[metric]][[source]][[ontology.name]][[stratification]][overwrite.indices] = data
            
            # Put metadata
            private$i.url[[outcome]][[metric]][[source]][[ontology.name]][[stratification]][overwrite.indices] = url
            private$i.details[[outcome]][[metric]][[source]][[ontology.name]][[stratification]][overwrite.indices] = details
            private$i.metadata[[outcome]][[metric]][[source]][[ontology.name]][[stratification]][overwrite.indices] = metadata.id
            if (printouts) {
                totalputtime = Sys.time()
                print(paste0(dots, "Final putting took ", totalputtime-finalputtime))
                print(paste0(dots, "Total put took ", totalputtime - puttime))
            }
            
            # Modified
            private$i.last.modified.date = Sys.time()
            if (!is.null(private$i.provenance)) private$i.provenance$modified = TRUE
            
            #-- Invisibly return the data manager for convenience --#
            invisible(self)
        },
        
        put.long.form = function(data,
                                 outcome,
                                 metric = 'estimate',
                                 source,
                                 ontology.name,
                                 dimension.values=NULL,
                                 url=NULL,
                                 details=NULL,
                                 metadata=NULL,
                                 allow.na.to.overwrite=F,
                                 dimension.values.to.distribute=list(),
                                 allow.distributing.on.proportion.scale=F,
                                 round.distributed.dimension.values=T,
                                 debug=F,
                                 printouts=F)
        {
            #-- Initial validate arguments --#
            # *data* must be a 2d object with named columns
            if (is.null(dim(data)) || length(dim(data))!=2)
                stop("Cannot put long-form data to the data.manager. 'data' must be a 2-dimensional object")
            if (is.null(dimnames(data)[[2]]))
                stop("Cannot put long-form data to the data.manager. 'data' must have named columns")
            
            if (missing(outcome) || is.null(outcome))
            {
                # If 'outcome' is NULL, then data must have a named outcome
                if (all(names(data)!='outcome'))
                    stop("Cannot put long-form data to the data.manager. Either 'outcome' must be set or it must be a column in the data object")
                
                unique.outcomes = unique(data[['outcome']])
                for (one.outcome in unique.outcomes)
                {
                    self$put.long.form(outcome = one.outcome,
                                       metric = metric,
                                       ontology.name = ontology.name,
                                       source = source,
                                       dimension.values = dimension.values,
                                       data = data[data[['outcome']]==one.outcome,],
                                       url = url,
                                       details = details,
                                       metadata = metadata,
                                       allow.na.to.overwrite=F,
                                       dimension.values.to.distribute = dimension.values.to.distribute,
                                       allow.distributing.on.proportion.scale = allow.distributing.on.proportion.scale,
                                       round.distributed.dimension.values = round.distributed.dimension.values,
                                       debug=debug,
                                       printouts=printouts)
                }
            }
            else
            {
                #-- Validate Arguments --#
                error.prefix = paste0("Cannot put long-form '", outcome, "' data to the data.manager: ")
                col.names = dimnames(data)[[2]]
                # *data* must have a column named 'value' which is numeric
                if (all(col.names != 'value'))
                    stop(paste0(error.prefix, "'data' must have a column named 'value'"))
                
                if (!is.numeric(data[['value']]))
                    stop(paste0(error.prefix, "The value column in 'data' must contain numeric values"))
                
                # Ontology name must be a single, non-NA character vector that refers to a registered ontology
                if (!is.character(ontology.name) || length(ontology.name)!=1 || is.na(ontology.name) || nchar(ontology.name)==0)
                    stop(paste0(error.prefix, "'ontology.name' must be a single, non-empty, non-NA character value"))
                
                ont = private$i.ontologies[[ontology.name]]
                if (is.null(ont))
                    stop(paste0(error.prefix, "'", ontology.name, "' is not a registered ontology. Call register.data.ontology() to register it before putting data for that ontology"))
                
                # All the other columns of data which are dimensions in the ontology must be either character or numeric vectors
                # Resolve them to character vectors
                dimensions = intersect(names(ont), setdiff(col.names, c('value','outcome')))
                sapply(dimensions, function(d){
                    if (!is.character(data[[d]]) && !is.numeric(data[[d]]))
                        stop(paste0(error.prefix, "Column '", d, "' in 'data' must contain character or numeric values"))
                    T
                })
                # browser()
                
                # NOTE: We will no longer check the dimension values here, because that interferes with the possibility of redistributing them.
                # Instead, we'll check like we already do in the put function
                # data[dimensions] = resolve.ontology.dimension.values(ont=ont, dimension.values=data[dimensions],
                # error.prefix=paste0(error.prefix, " Error resolving dimension values - "))
                
                # Hydrate it to an array
                dim.names = lapply(dimensions, function(d){
                    unique(data[[d]])
                })
                names(dim.names) = dimensions
                
                arr.data = array(as.numeric(NA), dim=sapply(dim.names, length), dimnames=dim.names)
                indices.arr = array(1:length(arr.data), dim=sapply(dim.names, length), dimnames=dim.names)
                dimensions = names(dim.names)
                indices.for.rows = sapply(1:nrow(data), function(i){
                    do.call('[', args=c(list(indices.arr), as.list(data[i,dimensions])))
                })
                na.mask = is.na(data[,'value'])
                arr.data[indices.for.rows[!na.mask]] = data[!na.mask,'value']
                
                #-- Pass through to main put function --#
                self$put(outcome = outcome,
                         metric = metric,
                         ontology.name = ontology.name,
                         source = source,
                         dimension.values = dimension.values,
                         data = arr.data,
                         url = url,
                         details = details,
                         metadata = metadata,
                         allow.na.to.overwrite = allow.na.to.overwrite,
                         dimension.values.to.distribute = dimension.values.to.distribute,
                         allow.distributing.on.proportion.scale = allow.distributing.on.proportion.scale,
                         round.distributed.dimension.values= round.distributed.dimension.values,
                         debug=debug,
                         printouts=printouts)
            }
            
            if (printouts) print('done with put long form')
        },
        
        pull = function(outcome,
                        metric = 'estimate',
                        keep.dimensions = NULL,
                        dimension.values = NULL,
                        sources = NULL,
                        from.ontology.names = NULL,
                        exclude.ontology.names = NULL,
                        target.ontology = NULL,
                        allow.mapping.from.target.ontology = T,
                        append.attributes = NULL,
                        append.metrics = NULL,
                        allow.other.sources.for.denominator = F,
                        na.rm = F,
                        ignore.ontologies.without.requested.locations = T,
                        check.arguments = T,
                        debug = F,
                        ...)
        {
            # if (debug) browser()
            error.prefix = paste0("Cannot pull '", outcome, "' data from the data manager: ")
            # *extra dimensions* are an alternative to 'dimension.values' and must pass the same checks if used
            extra.dimension.values = list(...)
            if (length(extra.dimension.values)>0) {
                # check that either this or dimension.values is len(0)
                if (length(extra.dimension.values)>0 && length(dimension.values)>0)
                    stop(paste0(error.prefix, "'dimension.values' must be specified in either the 'dimension.values' argument or as additional arguments to the function"))
                dimension.values = extra.dimension.values
                check.dimension.values.valid.error.name = "pull function extra arguments"
            }
            # If *keep.dimensions* are NULL and there is a target ontology, set keep.dimensions to empty char vector
            if (!is.null(target.ontology) && is.null(keep.dimensions))
                keep.dimensions = character(0)
            
            outcome.info = private$i.outcome.info[[outcome]]
            scale = outcome.info$metadata$scale
            
            if (check.arguments) {
                
                # *outcome* is a single, non-NA character value
                #  that has been previously registered as an outcome for this data manager
                if (!is.character(outcome) || length(outcome)!=1 || is.na(outcome) || nchar(outcome)==0)
                    stop(paste0(error.prefix, "'outcome' must be a single, non-empty, non-NA character value"))
                
                if (is.null(outcome.info))
                    stop(paste0(error.prefix, "'", outcome, "' is not a registered outcome."))
                
                # *metric* is one of 'estimate', 'upper.bound', 'lower.bound', or 'coefficient.of.variance' until more options are added #@ add variance and standard deviation
                if (!is.character(metric) || length(metric) != 1 || !(metric %in% c('estimate', 'variance', 'standard.deviation', 'upper.bound', 'lower.bound', 'coefficient.of.variance')))
                    stop(paste0(error.prefix, "'metric' must be a character vector with value 'estimate', 'variance', 'standard.deviation', 'upper.bound', 'lower.bound' or 'coefficient.of.variance'"))
                
                # if (metric %in% c('upper.bound', 'lower.bound'))
                #     stop(paste0(error.prefix, "pulling with metric '", metric, "' is not yet supprted"))
                
                # *keep.dimensions* is either NULL or a character vector with no NA values or repeats
                if (!is.null(keep.dimensions) && (!is.character(keep.dimensions) || any(duplicated(keep.dimensions)) || anyNA(keep.dimensions)))
                    stop(paste0(error.prefix, "'keep.dimensions' must be either NULL or a character vector with no NA values or repeats"))
                
                # make a variable name for error to pass to check.dimension.values.valid
                check.dimension.values.valid.error.name = "dimension.values"
                
                # *dimension.values* are valid
                #   - check.dimension.values.valid() doesn't accept NULL because it wants a list
                if (!is.null(dimension.values))
                    check.dimension.values.valid(dimension.values, "dimension.values")
                
                # *sources* is either NULL or a character vector with at least one element and no NA or empty values
                #  that have all been registered previously as sources for this outcome with this data manager
                if (!is.null(sources) && (!is.character(sources) || !length(sources)>0 || anyNA(sources) || any(nchar(sources)==0)))
                    stop(paste0(error.prefix, "'sources' must be NULL or a character vector with at least one element and no NA or empty values"))
                unregistered.sources = sapply(sources, function(x){
                    !(x %in% unique(unlist(lapply(private$i.data[[outcome]], function(metric.data) {names(metric.data)}))))
                })
                if (any(unregistered.sources))
                    stop(paste0(error.prefix, "all sources must be registered for this outcome with this data manager"))
                
                # *target.ontology* is either NULL or an ontology object
                if (!is.null(target.ontology) && !is.ontology(target.ontology))
                    stop(paste0(error.prefix, "'target.ontology' must be either NULL or an ontology object"))
                
                # The target ontology cannot have any NULL dimensions (ones that do have never had any data put to them)
                if (any(sapply(target.ontology, is.null)))
                    stop(paste0(error.prefix, "'target.ontology' cannot have any NULL dimensions"))
                
                # *allow.mapping.from.target.ontology* is a single, non-NA logical value
                if (!is.logical(allow.mapping.from.target.ontology) || length(allow.mapping.from.target.ontology)!=1 || is.na(allow.mapping.from.target.ontology))
                    stop(paste0(error.prefix, "'allow.mapping.from.target.ontology' must be a single, non-NA logical value"))
                
                # *from.ontology.names* is either NULL or a character vector with no NA or empty values
                if (!is.null(from.ontology.names) && (!is.character(from.ontology.names) || anyNA(from.ontology.names) || any(nchar(from.ontology.names)==0)))
                    stop(paste0(error.prefix, "'from.ontology.names' must be either NULL or a character vector with no NA or empty values"))
                
                #  all of which have been previously registered with this data manager (if not NULL)
                unregistered.ontologies = sapply(from.ontology.names, function(x){is.null(private$i.ontologies[[x]])})
                if (!is.null(from.ontology.names) && any(unregistered.ontologies))
                    stop(paste0(error.prefix, "all ontologies in 'from.ontology.names' must be registered with this data manager"))
                
                # *exclude.ontology.names* is either NULL or a character vector with no NA or empty values
                if (!is.null(exclude.ontology.names) && (!is.character(exclude.ontology.names) || anyNA(exclude.ontology.names) || any(nchar(exclude.ontology.names)==0)))
                    stop(paste0(error.prefix, "'exclude.ontology.names' must be either NULL or a character vector with no NA or empty values"))
                
                # *append.attributes* is either NULL or a character vector with no NA values that
                #  contains only registered metadata fields (such as "details" or "url") or "metadata"
                if (!is.null(append.attributes) && (!is.character(append.attributes) || anyNA(append.attributes) || !all(append.attributes %in% c(names(private$i.metadata.fields), "metadata"))))
                    stop(paste0(error.prefix, "append.attributes' must be either NULL or a character vector with no NA values that contains only registered metadata fields (",
                                collapse.with.and("'", names(private$i.metadata.fields), "'"), ") or 'metadata'"))
                
                # *na.rm* is a single, non-NA logical value
                if (!is.logical(na.rm) || length(na.rm)!=1 || is.na(na.rm))
                    stop(paste0(error.prefix, "na.rm must be a single, non-NA, logical value"))
            }
            
            # Check if we have any data at all for this outcome!
            if (is.null(private$i.data[[outcome]][[metric]])) {
                return (NULL)
            }
            
            # Reduce ontology space by ignoring ontologies that don't have our locations if locations are in the dimension values
            # We'll do this first even though we won't *technically* know that 'location' is an incomplete dimension in the ontology we'll end up in.
            
            if (ignore.ontologies.without.requested.locations && 'location' %in% names(dimension.values)) {
                ontologies.with.these.locations = names(private$i.ontologies)[sapply(private$i.ontologies, function(ont) {any(dimension.values$location %in% ont[['location']])})]
                ontologies.in.this.outcome = unique(unlist(sapply(private$i.data[[outcome]][[metric]], function(source) {names(source)})))
                relevant.ontologies = intersect(ontologies.with.these.locations, ontologies.in.this.outcome)
                if (!is.null(from.ontology.names))
                    from.ontology.names = intersect(from.ontology.names, relevant.ontologies)
                else
                    from.ontology.names = relevant.ontologies
                
            }
            
            # Get the universal ontology (replaces 'target.ontology') and the returned mapping, which may be replaced with an identity mapping if keep.dimensions are not in the mapping's 'to' dimensions
            return.mapping.flag = !is.null(target.ontology) && allow.mapping.from.target.ontology
            target.from.arguments = target.ontology
            if (debug) browser()
            if (is.null(target.ontology) || allow.mapping.from.target.ontology) {
                target.ontology = private$get.universal.ontology(outcome = outcome,
                                                                 sources = sources,
                                                                 from.ontology.names = from.ontology.names,
                                                                 exclude.ontology.names = exclude.ontology.names,
                                                                 target.ontology = target.ontology,
                                                                 debug=F)
            }
            # This gets NULL if the ontologies which had the requested locations were not for this outcome
            if (is.null(target.ontology)) return(NULL)
            
            dv.names = names(dimension.values)
            dimension.values = lapply(seq_along(dimension.values), function(d) {
                if (names(dimension.values)[[d]] %in% incomplete.dimensions(target.ontology)) as.character(dimension.values[[d]])
                else dimension.values[[d]]
            })
            names(dimension.values) = dv.names
            resolved.dimension.values = resolve.ontology.dimension.values(target.ontology, dimension.values, error.prefix = error.prefix, throw.error.if.unresolvable = F)
            if ((is.null(resolved.dimension.values) || any(sapply(resolved.dimension.values, is.null))) && !is.null(dimension.values))
                stop(paste0(error.prefix, "'dimension.values' cannot be resolved"))
            
            # need.to.set.keep.dimensions.flag = is.null(keep.dimensions)
            if (is.null(keep.dimensions)) {
                if (is.null(resolved.dimension.values)) {
                    dimension.values.dimensions.longer.than.one = dv.names[sapply(resolved.dimension.values, length)>1]
                    keep.dimensions = setdiff(incomplete.dimensions(target.ontology), dimension.values.dimensions.longer.than.one)
                }
                else {
                    keep.dimensions = incomplete.dimensions(target.ontology)
                }
                if (return.mapping.flag) attr(post.processed.data, 'mapping') = target.to.universal.mapping
            }
            
            # Reduce target to needed dimensions and find a mapping
            if (!is.null(keep.dimensions))
                target.ontology = target.ontology[names(target.ontology) %in% union(keep.dimensions, names(dimension.values))]
            if (return.mapping.flag) target.to.universal.mapping = get.ontology.mapping(target.from.arguments, target.ontology, allow.non.overlapping.incomplete.dimensions = T)
            
            # If sources is NULL, use all the sources from the outcome
            if (is.null(sources))
                sources.used.names = names(private$i.data[[outcome]][[metric]])
            else
                sources.used.names = sources
            sources.successful.names = c()
            
            pre.processed.data = lapply(sources.used.names, function(source.name) {
                source.ontology.names = names(private$i.data[[outcome]][[metric]][[source.name]])
                if (is.null(from.ontology.names))
                    ontologies.used.names = source.ontology.names
                else
                    ontologies.used.names = intersect(source.ontology.names, from.ontology.names)
                ontologies.used.names = setdiff(ontologies.used.names, exclude.ontology.names)
                pulled.source.data = NULL
                
                for (ont.name in ontologies.used.names) {
                    pulled.ont.data = NULL
                    ont = private$i.ontologies[[ont.name]]
                    stratification.names = names(private$i.data[[outcome]][[metric]][[source.name]][[ont.name]])
                    
                    for (strat in stratification.names) {
                        strat.data = private$i.data[[outcome]][[metric]][[source.name]][[ont.name]][[strat]]
                        strat.dimensions = names(dim(strat.data))
                        strat.dimnames = as.ontology(dimnames(strat.data), incomplete.dimensions = intersect(incomplete.dimensions(ont), strat.dimensions))
                        
                        if (all(is.na(strat.data))) next
                        
                        mapping.to.apply = get.ontology.mapping(strat.dimnames, target.ontology, allow.non.overlapping.incomplete.dimensions = T)
                        if (is.null(mapping.to.apply)) next
                        
                        dimnames.for.apply = mapping.to.apply$apply.to.dim.names(strat.dimnames)
                        if (!setequal(names(dimnames.for.apply), union(keep.dimensions, dv.names))) next
                        
                        # Check that the mapped stratification won't aggregate illegally due to missing some values in an incomplete dimension that will be aggregated (since it is in dimension.values but not keep.dimensions)
                        missing.dimension.values.for.aggregated.dimension = F
                        aggregated.dimensions = setdiff(incomplete.dimensions(dimnames.for.apply), keep.dimensions)
                        for (d in aggregated.dimensions) {
                            if (d %in% dv.names) {
                                if (length(setdiff(dimension.values[[d]], dimnames.for.apply[[d]])) > 0) {
                                    missing.dimension.values.for.aggregated.dimension = T
                                    break
                                }
                            }
                        }
                        if (missing.dimension.values.for.aggregated.dimension) next
                        
                        # Insert dimension.values
                        
                        dimension.has.no.intersection = F
                        for (d in dv.names) {
                            if (d == 'year') {
                                replacement = get.range.robust.year.intersect(dimnames.for.apply[[d]], resolved.dimension.values[[d]])
                            }
                            else replacement = intersect(dimnames.for.apply[[d]], resolved.dimension.values[[d]])
                            if (length(replacement) == 0) {
                                dimension.has.no.intersection = T
                                break
                            }
                            dimnames.for.apply[[d]] = replacement
                        }
                        if (dimension.has.no.intersection) {
                            next
                        }
                        
                        # Whatever attributes are requested, we carry only the metadata record IDs through mapping and aggregation, and resolve them to fields at the end
                        metadata.types = if (length(append.attributes) > 0) 'metadata' else NULL
                        data.types = c('data', metadata.types)

                        if (!is.null(metadata.types)) {
                            # Points with no data (eg removed ones) still have a record ID; drop it so it can't be merged into aggregated cells
                            strat.metadata = private$i.metadata[[outcome]][[metric]][[source.name]][[ont.name]][[strat]]
                            strat.metadata[is.na(strat.data)] = NA
                            append.attributes.data <- list(strat.metadata)
                        } else append.attributes.data <- NULL

                        pulled.ont.data <- private$do.pre.aggregation.processing(metadata.types,
                                                                                 mapping.to.apply,
                                                                                 strat.dimnames,
                                                                                 dimnames.for.apply,
                                                                                 strat.data,
                                                                                 metric=metric,
                                                                                 outcome=outcome,
                                                                                 source.name=source.name,
                                                                                 keep.dimensions=keep.dimensions,
                                                                                 dv.names=dv.names,
                                                                                 outcome.info=outcome.info,
                                                                                 na.rm=na.rm,
                                                                                 append.attributes.data=append.attributes.data)
                        
                        # Aggregate if needed
                        initial.dimnames = dimnames.for.apply
                        dimensions.to.drop = intersect(which(sapply(initial.dimnames, length) == 1), which(!(names(initial.dimnames) %in% keep.dimensions)))
                        
                        pulled.ont.data = lapply(data.types, function(data.type) {
                            if (debug) browser()
                            data.by.data.type = pulled.ont.data[[data.type]]
                            pre.agg.dimnames = initial.dimnames
                            if (length(dimensions.to.drop) > 0) {
                                pre.agg.dimnames = pre.agg.dimnames[-dimensions.to.drop]
                                data.by.data.type = array(data.by.data.type,
                                                          dim = sapply(pre.agg.dimnames, length),
                                                          dimnames = pre.agg.dimnames)
                            }
                            
                            if (length(pre.agg.dimnames) > length(keep.dimensions)) {
                                if (data.type=="data")
                                    aggregated.data <- private$do.aggregation(pre.agg.dimnames=pre.agg.dimnames,
                                                                              data.to.aggregate=data.by.data.type,
                                                                              keep.dimensions=keep.dimensions,
                                                                              data.type=data.type,
                                                                              metric=metric,
                                                                              scale=scale,
                                                                              source.name=source.name,
                                                                              outcome=outcome,
                                                                              dimension.values=dimension.values,
                                                                              target.ontology=target.ontology,
                                                                              outcome.info=outcome.info,
                                                                              na.rm=na.rm)
                                else
                                    aggregated.data = apply.robust(data.by.data.type, intersect(names(pre.agg.dimnames), keep.dimensions), private$merge.metadata.ids)
                            }
                            else
                                aggregated.data = data.by.data.type

                            dimnames(aggregated.data) = as.list(dimnames(aggregated.data))

                            # Metadata stay as record IDs here; they are resolved after sources are combined
                            aggregated.data
                        }) # end of lapply for data.types
                        
                        if (all(is.na(pulled.ont.data[[1]]))) {
                            pulled.ont.data = NULL
                            next
                        }
                        names(pulled.ont.data) = data.types
                        
                        # SUCCESS FOR THIS ONTOLOGY - save info if this is the first ontology we've succeeded on for this source
                        if (is.null(pulled.source.data)) {
                            sources.successful.names <<- c(sources.successful.names, source.name)
                            # keep.dimensions <<- keep.dimensions
                            # need.to.set.keep.dimensions.flag <<- FALSE
                        }
                        break
                        
                    } # end of loop for stratification
                    # if (source.lacks.denominator.data.flag) break
                    
                    if (all(is.na(pulled.ont.data))) next
                    # Integrate ontology's pulled data into the source's pulled data -- they should all be in the same (universal) ontology and therefore compatible
                    new.source.data.dimnames = dimnames(pulled.ont.data[[1]])
                    if (!is.null(pulled.source.data)) {
                        for (d in names(new.source.data.dimnames)) {new.source.data.dimnames[[d]] = sort(union(new.source.data.dimnames[[d]], dimnames(pulled.source.data[[1]])[[d]]))}
                    }
                    pulled.source.data = lapply(data.types, function(data.type) {
                        
                        if (!is.null(pulled.source.data)) {
                            new.pulled.source.data.this.type = array(NA, sapply(new.source.data.dimnames, length), new.source.data.dimnames)
                            new.pulled.source.data.this.type[get.array.access.indices(new.source.data.dimnames, dimnames(pulled.source.data[[data.type]]))] = pulled.source.data[[data.type]]
                            pulled.data.na.mask = !is.na(pulled.ont.data[[data.type]])
                            new.pulled.source.data.this.type[get.array.access.indices(new.source.data.dimnames, dimnames(pulled.ont.data[[data.type]]))[pulled.data.na.mask]]= pulled.ont.data[[data.type]][pulled.data.na.mask]
                            dim(new.pulled.source.data.this.type) = sapply(new.source.data.dimnames, length)
                            dimnames(new.pulled.source.data.this.type) = new.source.data.dimnames
                        }
                        else
                            new.pulled.source.data.this.type = pulled.ont.data[[data.type]]
                        new.pulled.source.data.this.type
                        
                    })
                    names(pulled.source.data) = data.types
                    pulled.source.data <<- pulled.source.data
                    
                }  # end of loop for ontology
                pulled.source.data
            }) # end of lapply for sources
            
            # we have a list (one element per source) of lists (one element per data type, i.e. 'data' and possibly 'metadata')
            # repackage this to be a data array with the requested metadata attributes (eg 'url', 'details') and possibly a mapping as attributes
            post.processed.data = NULL
            if (debug) browser()
            # Some sources may have returned NULL above and should be removed.
            pre.processed.data = pre.processed.data[!unlist(lapply(pre.processed.data, is.null))]
            # Extract data and metadata IDs out of what lapply returned above
            if (length(pre.processed.data) > 0) {
                for (data.type in c('data', if (length(append.attributes) > 0) 'metadata')) {
                    
                    # make a list of the data from the sources
                    data.by.source = lapply(pre.processed.data, function(x) {x[[data.type]]})
                    names(data.by.source) = sources.successful.names
                    
                    return.for.this.data.type = NULL
                    
                    overall.dim.names = c(dimnames(data.by.source[[1]]), list(source=sources.successful.names))
                    
                    # if more than one source, the overall.dim.names will have the union of all incomplete dimension values... sorted!
                    if (length(data.by.source) > 1) {
                        incomplete.dimension.values = lapply(incomplete.dimensions(target.ontology), function(d) {
                            sort(unique(unlist(lapply(data.by.source, function(src) {dimnames(src)[[d]]}))))
                        })
                        incomplete.dimension.values = incomplete.dimension.values[!sapply(incomplete.dimension.values, is.null)]
                        overall.dim.names[names(overall.dim.names) %in% incomplete.dimensions(target.ontology)] = incomplete.dimension.values
                        if (data.type == 'data')
                            return.for.this.data.type = array(NA, sapply(overall.dim.names, length), overall.dim.names)
                        else
                            return.for.this.data.type = array(list(NA), sapply(overall.dim.names, length), overall.dim.names)
                        
                        # for each source, write its data into the new array
                        for (s in seq_along(data.by.source)) {
                            src.dimnames = c(dimnames(data.by.source[[s]]), list(source=names(data.by.source)[[s]]))
                            return.for.this.data.type[get.array.access.indices(overall.dim.names, src.dimnames)] = data.by.source[[s]]
                        }
                    }
                    
                    else {
                        if (data.type == 'data') {
                            return.for.this.data.type = sapply(data.by.source, function(x) {x})
                            
                        } else {
                            for (source.data in data.by.source) {
                                return.for.this.data.type = append(return.for.this.data.type, source.data)
                            }
                        }
                        
                        dim(return.for.this.data.type) = sapply(overall.dim.names, length)
                        dimnames(return.for.this.data.type) = overall.dim.names
                    }
                    
                    # incorporate it into final.return
                    if (data.type == 'data') {
                        post.processed.data = return.for.this.data.type
                    } else if (data.type == 'metadata') {
                        post.processed.data = private$attach.metadata.attributes(post.processed.data,
                                                                                 id.array = return.for.this.data.type,
                                                                                 append.attributes = append.attributes)
                    }
                    
                }
                if (return.mapping.flag) attr(post.processed.data, 'mapping') = target.to.universal.mapping
            }
            # Rprof(NULL)
            post.processed.data
        },
        
        pull.age.robust = function(outcome,
                                   metric = 'estimate',
                                   keep.dimensions = NULL,
                                   dimension.values = NULL,
                                   sources = NULL,
                                   from.ontology.names = NULL,
                                   exclude.ontology.names = NULL,
                                   target.ontology = NULL,
                                   allow.mapping.from.target.ontology = T,
                                   append.attributes = NULL,
                                   allow.other.sources.for.denominator = F,
                                   na.rm = F,
                                   check.arguments = T,
                                   debug = F,
                                   ...,
                                   restratify.age = F,
                                   desired.age.brackets = NULL,
                                   smooth.infinite.age.to = 100,
                                   allow.extrapolation = F,
                                   method = c('monoH.FC','hyman')[1])
        {
            # validate pull arguments internally
            if (!restratify.age || !('age' %in% union(keep.dimensions, names(dimension.values)))) {
                rv = self$pull(outcome = outcome,
                               metric = metric,
                               keep.dimensions = keep.dimensions,
                               dimension.values = dimension.values,
                               sources = sources,
                               from.ontology.names = from.ontology.names,
                               exclude.ontology.names = exclude.ontology.names,
                               target.ontology = target.ontology,
                               allow.mapping.from.target.ontology = allow.mapping.from.target.ontology,
                               append.attributes = append.attributes,
                               allow.other.sources.for.denominator = allow.other.sources.for.denominator,
                               na.rm = na.rm,
                               check.arguments = check.arguments,
                               debug = debug,
                               ...)
            }
            else {
                rv = tryCatch(
                    {self$pull(outcome = outcome,
                               metric = metric,
                               keep.dimensions = keep.dimensions,
                               dimension.values = dimension.values,
                               sources = sources,
                               from.ontology.names = from.ontology.names,
                               exclude.ontology.names = exclude.ontology.names,
                               target.ontology = target.ontology,
                               allow.mapping.from.target.ontology = allow.mapping.from.target.ontology,
                               append.attributes = append.attributes,
                               allow.other.sources.for.denominator = allow.other.sources.for.denominator,
                               na.rm = na.rm,
                               check.arguments = check.arguments,
                               debug = debug,
                               ...)},
                    error=function(e){ NULL}
                )
                
                if (is.null(rv)) {
                    target.ontology = target.ontology[names(target.ontology)!='age']
                    rv = self$pull(outcome = outcome,
                                   metric = metric,
                                   keep.dimensions = keep.dimensions,
                                   dimension.values = dimension.values,
                                   sources = sources,
                                   from.ontology.names = from.ontology.names,
                                   exclude.ontology.names = exclude.ontology.names,
                                   target.ontology = target.ontology,
                                   allow.mapping.from.target.ontology = allow.mapping.from.target.ontology,
                                   append.attributes = append.attributes,
                                   allow.other.sources.for.denominator = allow.other.sources.for.denominator,
                                   na.rm = na.rm,
                                   check.arguments = check.arguments,
                                   debug = debug,
                                   ...)
                }
                if (is.null(rv)) return (rv)
                
                # check rv scale?
                url = attr(rv, 'url')
                details = attr(rv, 'details')
                pulled.mapping = attr(rv, 'mapping')
                # will need to transform a numerator and denominator to pass in to restratify age counts for proportions
                restratified.data = restratify.age.counts(counts = rv,
                                                          desired.age.brackets = desired.age.brackets,
                                                          smooth.infinite.age.to = smooth.infinite.age.to,
                                                          allow.extrapolation = allow.extrapolation,
                                                          na.rm = na.rm,
                                                          method = method,
                                                          error.prefix = "error restratifying age counts in age robust pull: ") #set
                restratify.mapping = attr(restratified.data, 'mapping')
                # I'll need to use the mapping returned as an attr on the above to map details and url
                attr(restratified.data, 'mapping') = attr(rv, 'mapping')
                if (any(append.attributes)=='url')
                    attr(restratified.data, 'url') = restratify.mapping$apply(url)
                if (any(append.attributes)=='details')
                    attr(restratified.data, 'details') = restratify.mapping$apply(details)
                #  if (!is.null(pulled.mapping)) attr(restratified.data, 'mapping') = pulled.mapping # NOT TRUE ANYMORE... (? what did I write this for?)
                rv = restratified.data
            }
            rv
        },
        
        remove_data = function(outcome,
                               source,
                               ontology.name,
                               dimension.values,
                               metric = 'estimate',
                               details.for.removal='removed',
                               url.for.removal='url') {
            # Put NAs with overwrite=T
            self$put(as.numeric(NA),
                     outcome=outcome,
                     metric=metric,
                     source=source,
                     ontology.name=ontology.name,
                     dimension.values=dimension.values,
                     details=details.for.removal,
                     url=url.for.removal,
                     allow.na.to.overwrite = T,
                     is.removal = T)
        },
        
        get.outcome.pretty.names = function(outcomes)
        {
            sapply(private$i.outcome.info[outcomes], function(info){
                info$pretty.name
            })
        },
        
        get.outcome.units = function(outcomes)
        {
            sapply(private$i.outcome.info[outcomes], function(info){
                info$units
            })
        },
        
        get.outcome.descriptions = function(outcomes)
        {
            sapply(private$i.outcome.info[outcomes], function(info){
                info$description
            })
        },
        
        get.registered.ontology = function(ontology.name)
        {
            # ontology.name must be in the list of registered ontologies
            if (!ontology.name %in% names(private$i.ontologies))
                stop(paste0(ontology.name, " is not a registered ontology"))
            
            rv = private$i.ontologies[[ontology.name]]
            
        },
        
        get.source.full.names = function(sources)
        {
            sapply(private$i.source.info[sources], function(info){
                info$full.name
            })
        },
        
        get.source.short.names = function(sources)
        {
            sapply(private$i.source.info[sources], function(info){
                info$short.name
            })
        },
        
        get.year.bounds.for.outcome = function(outcome,
                                               metric='estimate',
                                               exclude.ontology.names = NULL)
        {
            if (is.null(outcome) || !is.character(outcome) || length(outcome) > 1 || is.na(outcome))
                stop("'outcome' must be a single, non-NA character value")
            if (!(outcome %in% names(private$i.outcome.info)))
                stop(paste0("'", outcome, "' is not a registered outcome."))
            if (!(outcome %in% names(private$i.data)))
                stop(paste0("there is no data for outcome '", outcome, "'"))
            if (!is.character(metric) || length(metric) != 1)
                stop("'metric' must be a single character value")
            if (!(metric %in% names(private$i.data[[outcome]])))
                stop(paste0("there is no data for metric '", metric, "' for outcome '", outcome, "'"))
            
            # *exclude.ontology.names* is either NULL or a character vector with no NA or empty values
            if (!is.null(exclude.ontology.names) && (!is.character(exclude.ontology.names) || anyNA(exclude.ontology.names) || any(nchar(exclude.ontology.names)==0)))
                stop(paste0(error.prefix, "'exclude.ontology.names' must be either NULL or a character vector with no NA or empty values"))
            # browser()
            earliest.year = Inf
            latest.year = -Inf
            for (source in private$i.data[[outcome]][[metric]]) {
                ontology.names = setdiff(names(source), exclude.ontology.names)
                for (ontology.name in ontology.names) {
                    for (strat in source[[ontology.name]]) {
                        if ('year' %in% names(dimnames(strat))) {
                            if (all(is.year.range(dimnames(strat)$year))) {
                                parsed.range = parse.year.ranges(dimnames(strat)$year)
                                starts = sort(parsed.range$start)
                                ends = sort(parsed.range$end, decreasing = T)
                                if (starts[[1]] < earliest.year) earliest.year = starts[[1]]
                                if (ends[[1]] > latest.year) latest.year = ends[[1]]
                            }
                            else {
                                years = sort(dimnames(strat)[['year']])
                                if (years[[1]] < earliest.year) earliest.year = years[[1]]
                                if (years[[length(years)]] > latest.year) latest.year = years[[length(years)]]
                            }
                        }
                    }
                }
            }
            if (earliest.year == Inf) {
                earliest.year = NULL
                latest.year = NULL
            }
            list(earliest.year = earliest.year, latest.year = latest.year)
        },
        
        get.locations.with.data = function(outcome,
                                           metric='estimate',
                                           years = NULL,
                                           exclude.ontology.names = NULL)
        {
            if (is.null(outcome) || !is.character(outcome) || length(outcome) > 1 || is.na(outcome))
                stop("'outcome' must be a single, non-NA character value")
            if (!(outcome %in% names(private$i.outcome.info)))
                stop(paste0("'", outcome, "' is not a registered outcome."))
            if (!(outcome %in% names(private$i.data)))
                stop(paste0("there is no data for outcome '", outcome, "'"))
            if (!is.character(metric) || length(metric) != 1)
                stop("'metric' must be a single character value")
            if (!(metric %in% names(private$i.data[[outcome]])))
                stop(paste0("there is no data for metric '", metric, "' for outcome '", outcome, "'"))
            # *years* is NULL or a numeric vector with no NAs or duplicates
            if (!is.null(years) && (!is.numeric(years) || any(is.na(years)) || any(duplicated(years))))
                stop("'years' must be NULL or a numeric vector with no NAs or duplicates")
            # *exclude.ontology.names* is either NULL or a character vector with no NA or empty values
            if (!is.null(exclude.ontology.names) && (!is.character(exclude.ontology.names) || anyNA(exclude.ontology.names) || any(nchar(exclude.ontology.names)==0)))
                stop(paste0(error.prefix, "'exclude.ontology.names' must be either NULL or a character vector with no NA or empty values"))
            
            unique(unlist(lapply(private$i.data[[outcome]][[metric]], function(source.data) {
                ontology.names = setdiff(names(source.data), exclude.ontology.names)
                unique(unlist(lapply(ontology.names, function(ontology.name) {
                    unique(unlist(lapply(source.data[[ontology.name]], function(stratification.data) {
                        if (!is.null(years)) {
                            new.dimnames = dimnames(stratification.data)
                            new.dimnames$year = intersect(years, new.dimnames$year)
                            data.for.years = array(stratification.data[get.array.access.indices(dimnames(stratification.data), dimension.values = list(year = intersect(years, dimnames(stratification.data)$year)))],
                                                   dim = sapply(new.dimnames, length),
                                                   dimnames = new.dimnames)
                            location.has.data = apply(data.for.years, MARGIN='location', FUN=function(x) {!all(is.na(x))})
                        } else
                            location.has.data = apply(stratification.data, MARGIN='location', FUN=function(x) {!all(is.na(x))})
                        names(location.has.data[location.has.data])
                    })))
                })))
            })))
        },
        
        get.ontologies.for.outcome = function(outcome, sources = NULL, exclude.ontology.names = NULL)
        {
            # VALIDATE ARGUMENTS
            error.prefix = "Error getting ontologies for outcome: "
            if (is.null(outcome) || !is.character(outcome) || length(outcome) > 1 || is.na(outcome))
                stop(paste0(error.prefix, "'outcome' must be a single, non-NA character value"))
            if (!(outcome %in% names(private$i.outcome.info)))
                stop(paste0(error.prefix, "'", outcome, "' is not a registered outcome"))
            if (!(outcome %in% names(private$i.data))) return (NULL)
            # *sources* is either NULL or a character vector with at least one element and no NA or empty values
            #  that have all been registered previously as sources for this outcome with this data manager
            if (!is.null(sources) && (!is.character(sources) || !length(sources)>0 || anyNA(sources) || any(nchar(sources)==0)))
                stop(paste0(error.prefix, "'sources' must be NULL or a character vector with at least one element and no NA or empty values"))
            unregistered.sources = sapply(sources, function(x){
                !(x %in% unique(unlist(lapply(private$i.data[[outcome]], function(metric.data) {names(metric.data)}))))
            })
            if (any(unregistered.sources))
                stop(paste0(error.prefix, "all sources must be registered for this outcome with this data manager"))
            # *exclude.ontology.names* is either NULL or a character vector with no NA or empty values
            if (!is.null(exclude.ontology.names) && (!is.character(exclude.ontology.names) || anyNA(exclude.ontology.names) || any(nchar(exclude.ontology.names)==0)))
                stop(paste0(error.prefix, "'exclude.ontology.names' must be either NULL or a character vector with no NA or empty values"))
            
            # GET ONTOLOGIES IN REQUESTED SOURCES
            ont.names = unique(unlist(lapply(names(private$i.data[[outcome]]), function(metric.name) {
                unique(unlist(lapply(names(private$i.data[[outcome]][[metric.name]]), function(source.name) {
                    source.ontologies = names(private$i.data[[outcome]][[metric.name]][[source.name]])
                    if (is.null(sources) || source.name %in% sources) source.ontologies
                    else NULL
                })))
            })))
            ont.names = setdiff(ont.names, exclude.ontology.names)
            onts = lapply(ont.names, function(n) {self$get.registered.ontology(n)})
            names(onts) = ont.names
            onts
        },
        
        get.universal.ontology.for.outcome = function(outcome, exclude.ontology.names = NULL)
        {
            # VALIDATE ARGUMENTS
            error.prefix = "Error getting universal ontology for outcome: "
            if (is.null(outcome) || !is.character(outcome) || length(outcome) > 1 || is.na(outcome))
                stop(paste0(error.prefix, "'outcome' must be a single, non-NA character value"))
            if (!(outcome %in% names(private$i.outcome.info)))
                stop(paste0(error.prefix, "'", outcome, "' is not a registered outcome"))
            # *exclude.ontology.names* is either NULL or a character vector with no NA or empty values
            if (!is.null(exclude.ontology.names) && (!is.character(exclude.ontology.names) || anyNA(exclude.ontology.names) || any(nchar(exclude.ontology.names)==0)))
                stop(paste0(error.prefix, "'exclude.ontology.names' must be either NULL or a character vector with no NA or empty values"))
            if (!(outcome %in% names(private$i.data))) return (NULL)
            private$get.universal.ontology(outcome, exclude.ontology.names=exclude.ontology.names, return.target.to.universal.mapping = F)
        },
        
        unhash.url = function(arr)
        {
            private$do.unhash(arr, private$i.url.list)
        },
        
        unhash.details = function(arr)
        {
            private$do.unhash(arr, private$i.details.list)
        },

        get.metadata.fields = function()
        {
            names(private$i.metadata.fields)
        },

        # Returns a data frame with one row per record and one column per registered field (plus 'id').
        # Fields with several values are collapsed with "__"; fields a record lacks are NA
        get.metadata.records = function(ids = NULL)
        {
            if (is.null(ids)) ids = seq_along(private$i.metadata.records)
            ids = as.integer(ids)
            if (any(is.na(ids)) || any(ids < 1) || any(ids > length(private$i.metadata.records)))
                stop("Cannot get metadata records: 'ids' must be valid record IDs for this data manager")

            rv = data.frame(id = ids)
            for (field in names(private$i.metadata.fields)) {
                rv[[field]] = vapply(private$i.metadata.records[ids], function(record) {
                    if (is.null(record[[field]])) NA_character_ else paste(record[[field]], collapse = "__")
                }, character(1))
            }
            rv
        },

        # Takes an array (or list array) whose elements are vectors of record IDs and returns a character array of the same dimensions,
        # where each element is the sorted, unique values of 'field' across those records, collapsed with 'collapse'.
        # Elements with no IDs, or whose records all lack the field, are NA
        resolve.metadata = function(id.array, field, collapse = "__")
        {
            if (!is.character(field) || length(field)!=1 || !(field %in% names(private$i.metadata.fields)))
                stop("Cannot resolve metadata: 'field' must be the name of a single registered metadata field")

            cache = new.env()
            rv = vapply(seq_along(id.array), function(i) {
                ids = unlist(id.array[[i]])
                ids = sort(unique(ids[!is.na(ids)]))
                if (length(ids) == 0) return(NA_character_)

                cache.index = paste(ids, collapse = "_")
                if (!is.null(cache[[cache.index]])) return(cache[[cache.index]])

                values = sort(unique(unlist(lapply(private$i.metadata.records[ids], function(record) {record[[field]]}))))
                resolved = if (length(values) == 0) NA_character_ else paste(values, collapse = collapse)
                cache[[cache.index]] = resolved
                resolved
            }, character(1))
            dim(rv) = dim(id.array)
            dimnames(rv) = dimnames(id.array)
            rv
        },
        
        inspect_marginals = function(outcome=NULL,
                                     source=NULL,
                                     ontology=NULL,
                                     sub.stratification=NULL,
                                     super.stratification=NULL,
                                     years=NULL,
                                     locations=NULL,
                                     threshold.percent = 10,
                                     threshold.magnitude = 50,
                                     check.up.to.n.way.stratified = 1,
                                     debug=F) {
            
            # For each outcome/metric/source/ontology, check each stratification against the others for correct marginals
            # Only for strats that are subsidiary to others, though (like "age" vs. "age__sex")
            # I call the more stratified strat the "sub" and the aggregate the "super" in a given pair
            error.prefix = "Error inspecting marginals: "
            if (!is.numeric(threshold.percent) || length(threshold.percent)!=1 || is.na(threshold.percent) || threshold.percent<=0)
                stop(paste0(error.prefix, "'threshold.percent' must be a single, non-NA numeric value greater than zero"))
            
            if (!is.numeric(threshold.magnitude) || length(threshold.magnitude)!=1 || is.na(threshold.magnitude) || threshold.magnitude<0)
                stop(paste0(error.prefix, "'threshold.magnitude' must be a single, non-NA numeric value greater than or equal to zero"))
            
            if (!is.numeric(check.up.to.n.way.stratified) || length(check.up.to.n.way.stratified)!=1 || is.na(check.up.to.n.way.stratified) || check.up.to.n.way.stratified < 1)
                stop(paste0(error.prefix, "'check.up.to.n.way.stratified' must be a single, non-NA integer value greater than or equal to one"))
            
            if (!is.null(outcome) && (!is.character(outcome) || length(outcome) > 1 || is.na(outcome)))
                stop(paste0(error.prefix, "'outcome' must be NULL or a single, non-NA character value"))
            if (!is.null(outcome) && !(outcome %in% names(private$i.outcome.info)))
                stop(paste0(error.prefix, "'", outcome, "' is not a registered outcome."))
            if (!is.null(source) && (!is.character(source) || length(source) > 1 || is.na(source)))
                stop(paste0(error.prefix, "'source' must be NULL or a single, non-NA character value"))
            if (!is.null(source) && !(source %in% names(private$i.source.info)))
                stop(paste0(error.prefix, "'", source, "' is not a registered source"))
            if (!is.null(ontology) && (!is.character(ontology) || length(ontology) > 1 || is.na(ontology)))
                stop(paste0(error.prefix, "'ontology' must be NULL or a single, non-NA character value"))
            if (!is.null(ontology) && !(ontology %in% names(private$i.ontologies)))
                stop(paste0(error.prefix, "'", ontology, "' is not a registered ontology"))
            
            if (!is.null(years) && (!is.numeric(years) || any(is.na(years))))
                stop(paste0(error.prefix, "'years' must be NULL or a numeric vector with no NAs"))
            if (!is.null(locations) && (!is.character(locations) || any(is.na(locations))))
                stop(paste0(error.prefix, "'locations' must be NULL or a character vector with no NAs"))
            
            rv <- private$do_inspect_marginals(outcome=outcome,
                                               source=source,
                                               ontology=ontology,
                                               sub.stratification=sub.stratification,
                                               super.stratification = super.stratification,
                                               years = years,
                                               locations = locations,
                                               threshold.percent=threshold.percent,
                                               threshold.magnitude=threshold.magnitude,
                                               check.up.to.n.way.stratified=check.up.to.n.way.stratified,
                                               error.prefix=error.prefix,
                                               debug=debug)
            rv
        }
    ),
    
    active = list(
        
        name = function(value)
        {
            if (missing(value))
                private$i.name
            else
                stop("Cannot modify 'name' in jheem.data.manager - it is read-only")
        },
        
        description = function(value)
        {
            if (missing(value))
                private$i.description
            else
                stop("Cannot modify 'description' in jheem.data.manager - it is read-only")
        },
        
        creation.date = function(value)
        {
            if (missing(value))
                private$i.creation.date
            else
                stop("Cannot modify 'creation.date' in jheem.data.manager - it is read-only")
        },
        
        last.modified.date = function(value)
        {
            if (missing(value))
                private$i.last.modified.date
            else
                stop("Cannot modify 'last.modified.date' in jheem.data.manager - it is read-only")
        },
        
        outcomes = function(value)
        {
            if (missing(value))
                names(private$i.outcome.info)
            else
                stop("Cannot modify 'outcomes' in jheem.data.manager - they are read-only")
        },
        
        ontology.names = function(value)
        {
            if (missing(value))
                names(private$i.ontologies)
            else
                stop("Cannot modify 'ontology.names' in jheem.data.manager - they are read-only")
        },
        
        data = function(value)
        {
            if (missing(value))
                private$i.data
            else
                stop("Cannot modify 'data' in jheem.data.manager - it is read-only")
        },
        
        url = function(value)
        {
            if (missing(value))
                private$i.url
            else
                stop("Cannot modify 'url' in jheem.data.manager - it is read-only")
        },
        
        details = function(value)
        {
            if (missing(value))
                private$i.details
            else
                stop("Cannot modify 'details' in jheem.data.manager - it is read-only")
        },
        
        url.list = function(value)
        {
            if (missing(value))
                private$i.url.list
            else
                stop("Cannot modify 'url.list' in jheem.data.manager - it is read-only")
        },
        
        details.list = function(value)
        {
            if (missing(value))
                private$i.details.list
            else
                stop("Cannot modify 'details.list' in jheem.data.manager - it is read-only")
        },

        metadata = function(value)
        {
            if (missing(value))
                private$i.metadata
            else
                stop("Cannot modify 'metadata' in jheem.data.manager - it is read-only")
        },

        metadata.fields = function(value)
        {
            if (missing(value))
                private$i.metadata.fields
            else
                stop("Cannot modify 'metadata.fields' in jheem.data.manager - it is read-only")
        },

        metadata.records = function(value)
        {
            if (missing(value))
                private$i.metadata.records
            else
                stop("Cannot modify 'metadata.records' in jheem.data.manager - it is read-only")
        },

        metadata.keys = function(value)
        {
            if (missing(value))
                private$i.metadata.keys
            else
                stop("Cannot modify 'metadata.keys' in jheem.data.manager - it is read-only")
        },
        
        outcome.info = function(value)
        {
            if (missing(value))
                private$i.outcome.info
            else
                stop("Cannot modify 'outcome.info' in jheem.data.manager - it is read-only")
        },
        
        source.info = function(value)
        {
            if (missing(value))
                private$i.source.info
            else
                stop("Cannot modify 'source.info' in jheem.data.manager - it is read-only")
        },
        
        parent.source.info = function(value)
        {
            if (missing(value))
                private$i.parent.source.info
            else
                stop("Cannot modify 'parent.source.info' in jheem.data.manager - it is read-only")
        },
        
        ontologies = function(value)
        {
            if (missing(value))
                private$i.ontologies
            else
                stop("Cannot modify 'ontologies' in jheem.data.manager - it is read-only")
        },
        
        code.iteration = function(value)
        {
            if (missing(value))
                private$i.code.iteration
            else
                stop("Cannot modify 'code.iteration' in jheem.data.manager - it is read-only")
        }
    ),
    
    private = list(
        
        #-- About the Data Manager itself --#
        i.name = NULL,
        i.description = NULL,
        i.code.iteration = NULL,
        i.creation.date = NULL,
        i.last.modified.date = NULL,
        i.provenance = NULL, # see DATA_MANAGER_provenance.R
        
        #-- Storage structures for data and metadata --#
        # These three are lists of lists of lists of lists, indexed
        # [[outcome]][[source]][[ontology.name]][[stratification]]
        # for i.data, each element of this depth-4 access is a numeric array
        # for i.url and i.details, each element is a list array (ie a list with dimnames set), each element of which is a character vector
        i.data = NULL,
        i.url = NULL,
        i.details = NULL,
        i.url.list = NULL,
        i.details.list = NULL,
        
        # And the new structures for metadata
        # (i.url and i.details are still written by put() so that saved data managers stay readable by older code, but pull() uses only i.metadata)
        i.metadata.fields  = NULL,  # named logical vector; names are the allowed field names (e.g. "url","details","author"), values are whether the field is required
        i.metadata.records = NULL,  # list; element k is record k, a named list of sorted, unique character vectors (only the fields that put supplied)
        i.metadata.keys    = NULL,  # character; canonical key for record k, used for dedupe via match()
        i.metadata         = NULL,  # same nesting as i.url: [[outcome]][[metric]][[source]][[ont]][[strat]] -> integer array of record IDs
        
        #-- Metadata --#
        # These are named lists, with the names being the names of outcomes or data groups
        i.outcome.info = NULL,
        i.source.info = NULL,
        i.parent.source.info = NULL,
        i.ontologies = NULL,
        
        ##------------------------------##
        ##-- Private Member Functions --##
        ##------------------------------##
        
        # Gets the stratification to either
        # - get data with keep.dimensions and dimension.values
        # - put the given 'data' using dimension.values
        get.required.stratification = function(dimension.values,
                                               data=NULL,
                                               keep.dimensions=NULL,
                                               ontology.name,
                                               return.as.dimensions) #if true, returns the vector of dimensions. If false, collapses to the stratification name
        {
            dimensions = intersect(names(private$i.ontologies[[ontology.name]]),
                                   union(names(dimension.values),
                                         union(names(dimnames(data)), keep.dimensions)))
            
            if (return.as.dimensions)
                dimensions
            else
                paste0(dimensions, collapse='__')
        },
        
        prepare.put.dim.names = function(dim.names,
                                         ontology.name)
        {
            ont = private$i.ontologies[[ontology.name]]
            
            ontology.dimensions.complete = is_complete(ont)
            rv = lapply(names(dim.names), function(d){
                if (ontology.dimensions.complete[d])
                    ont[[d]]
                else
                    sort(dim.names[[d]])
            })
            names(rv) = names(dim.names)
            
            rv
        },
        
        get.universal.ontology = function(outcome, sources = NULL, from.ontology.names = NULL, exclude.ontology.names = NULL, target.ontology = NULL, return.target.to.universal.mapping = T, debug = F)
        {
            if (debug) browser()
            onts = self$get.ontologies.for.outcome(outcome, sources, exclude.ontology.names = exclude.ontology.names)
            if (!is.null(from.ontology.names)) onts = onts[names(onts) %in% from.ontology.names]
            if (length(onts)==0) return(NULL) # means we have no ontologies in "from.ontology.names" that are for this outcome
            uni = onts[[1]]
            if (length(onts) > 1) {
                for (i in 2:length(onts)) {
                    mps = get.mappings.to.align.ontologies(onts[[i]], uni, allow.non.overlapping.incomplete.dimensions = T)
                    if (is.null(mps))
                    {
                        DATA.MANAGER.ONTOLOGY.ERRORS$did.you.remember.details = list(
                            onts.i = onts[[i]],
                            uni = uni
                        )
                        stop("Error mapping ontologies for outcome '", outcome, "': did you remember to register your mappings for ontology '", names(onts)[[i]],"'?")
                    }
                    uni = mps[[2]]$apply.to.ontology(uni)
                    
                }
            }
            if (!is.null(target.ontology)) {
                mps = get.mappings.to.align.ontologies(target.ontology, uni, allow.non.overlapping.incomplete.dimensions = T)
                if (is.null(mps))
                {
                    DATA.MANAGER.ONTOLOGY.ERRORS$did.you.remember.details = list(
                        target.ontology = target.ontology,
                        uni = uni
                    )
                    examine.get.mappings.to.align.ontologies(target.ontology, uni, allow.non.overlapping.incomplete.dimensions = T)
                    stop("Error mapping ontologies to target ontology for outcome '", outcome, "': did you remember to register your mappings?")
                }
                uni = mps[[2]]$apply.to.ontology(uni)
                if (return.target.to.universal.mapping) attr(uni, 'target.to.universal.mapping') = mps[[1]]
            }
            uni
        },
        do.unhash = function(arr, hash.list) {
            # We will take hashed vectors like c(1,3) and unhash each member
            # and also un-collapse components like "first__second" by "__" char.
            # Then we will sort/unique them and check if it matches any. If does,
            # add to our cache
            cache = hash.list # which should now be a vector
            names(cache) = as.character(seq_along(hash.list))
            # returns a vector array
            new.arr = sapply(arr, function(hashed.vector) {
                # ignore any with NA because this implies that the corresponding
                # data point becomes NA in the mapping that was applied
                hashed.vector <- sort(unique(unlist(hashed.vector)))
                if (any(is.na(hashed.vector))) NA
                else {
                    # skip a step if we have cached this combo
                    cache.index = paste(hashed.vector, collapse="__")
                    if (cache.index %in% names(cache)) return(cache[[cache.index]])
                    unhashed = paste(sort(unique(unlist(strsplit(sapply(hashed.vector, function(hashed.value) {
                        hash.list[hashed.value]}), "__")))), collapse = "__")
                    cache[[cache.index]] <<- unhashed
                    unhashed
                }
            })
            dim(new.arr) = dim(arr)
            dimnames(new.arr) = dimnames(arr)
            new.arr
        },

        #-- Metadata records --#

        # Validates the metadata for one put and returns it as a record: a named list, in field-registration order, of sorted, unique character vectors.
        # 'url' and 'details' may come from their own arguments; if they are also in 'metadata', the values must agree
        normalize.metadata.record = function(metadata, url = NULL, details = NULL, error.prefix = '')
        {
            if (is.null(metadata)) metadata = list()
            if (!is.list(metadata) || (length(metadata) > 0 && (is.null(names(metadata)) || any(is.na(names(metadata))) || any(names(metadata)==''))))
                stop(paste0(error.prefix, "'metadata' must be NULL or a named list"))
            if (any(duplicated(names(metadata))))
                stop(paste0(error.prefix, "'metadata' cannot contain repeated names"))

            # Fold in url and details from their own arguments
            from.own.arguments = list(url = url, details = details)
            for (field in names(from.own.arguments)) {
                if (is.null(from.own.arguments[[field]])) next
                if (!is.null(metadata[[field]]) && !setequal(as.character(metadata[[field]]), as.character(from.own.arguments[[field]])))
                    stop(paste0(error.prefix, "'", field, "' was given both in its own argument and in 'metadata', with different values"))
                metadata[[field]] = from.own.arguments[[field]]
            }

            unregistered.fields = setdiff(names(metadata), names(private$i.metadata.fields))
            if (length(unregistered.fields) > 0)
                stop(paste0(error.prefix, "'metadata' contains ", collapse.with.and("'", unregistered.fields, "'"),
                            ", which ", ifelse(length(unregistered.fields)==1, "is not a registered field", "are not registered fields"),
                            ". Call register.data.metadata.field() to register ", ifelse(length(unregistered.fields)==1, "it", "them")))

            missing.required.fields = setdiff(names(private$i.metadata.fields)[private$i.metadata.fields], names(metadata))
            if (length(missing.required.fields) > 0)
                stop(paste0(error.prefix, "a value must be given for required metadata field", ifelse(length(missing.required.fields)==1, " ", "s "),
                            collapse.with.and("'", missing.required.fields, "'")))

            for (field in names(metadata)) {
                values = metadata[[field]]
                if (is.numeric(values)) values = as.character(values)
                if (!is.character(values))
                    stop(paste0(error.prefix, "'", field, "' must be a character vector"))
                if (length(values)==0)
                    stop(paste0(error.prefix, "'", field, "' must contain at least one value"))
                if (any(is.na(values)))
                    stop(paste0(error.prefix, "'", field, "' cannot contain NA values"))
                if (any(nchar(values)==0))
                    stop(paste0(error.prefix, "'", field, "' cannot contain empty values ('')"))
                metadata[[field]] = sort(unique(values))
            }

            # Registration order, so that identical records always get identical keys
            metadata[intersect(names(private$i.metadata.fields), names(metadata))]
        },

        # A string that is identical for two records exactly when the records are identical. The separators are control characters, which won't appear in real values
        metadata.record.key = function(record)
        {
            paste(names(record), vapply(record, paste, character(1), collapse = "\u001f"), sep = "\u001e", collapse = "\u001d")
        },

        # Takes a normalized record and returns its integer ID, adding it to the records if it is new
        get.or.add.metadata.record = function(record)
        {
            key = private$metadata.record.key(record)
            id = match(key, private$i.metadata.keys)
            if (is.na(id)) {
                private$i.metadata.records = c(private$i.metadata.records, list(record))
                private$i.metadata.keys = c(private$i.metadata.keys, key)
                id = length(private$i.metadata.keys)
            }
            as.integer(id)
        },

        # Used when mapping or aggregating metadata in a pull: combines the record IDs of the cells being merged into one sorted vector.
        # Missing (NA) points contribute nothing; if all points are missing, the result is NA
        merge.metadata.ids = function(x, ...)
        {
            ids = unlist(x)
            ids = sort(unique(as.integer(ids[!is.na(ids)])))
            list(if (length(ids) > 0) ids else NA_integer_)
        },

        # At the end of a pull: turns the (list) array of record IDs into the requested attributes on 'result'.
        # 'id.array' must have the same dimensions as 'result'. Cells where 'result' is NA get NA metadata
        attach.metadata.attributes = function(result, id.array, append.attributes)
        {
            data.is.na = is.na(result)
            ids = lapply(seq_along(id.array), function(i) {
                if (data.is.na[[i]]) return(NA_integer_)
                one.cell = unlist(id.array[[i]])
                one.cell = sort(unique(as.integer(one.cell[!is.na(one.cell)])))
                if (length(one.cell) == 0) NA_integer_ else one.cell
            })
            dim(ids) = dim(result)
            dimnames(ids) = dimnames(result)

            for (field in setdiff(append.attributes, 'metadata'))
                attr(result, field) = self$resolve.metadata(ids, field)

            if ('metadata' %in% append.attributes) {
                used.ids = unlist(ids)
                used.ids = sort(unique(used.ids[!is.na(used.ids)]))
                attr(result, 'metadata.ids') = ids
                attr(result, 'metadata') = self$get.metadata.records(used.ids)
            }
            result
        },

        # For data managers created before metadata records existed: makes one record per distinct (url, details) pair and
        # builds i.metadata from i.url and i.details
        build.metadata.from.url.and.details = function()
        {
            private$i.metadata = list()
            private$i.metadata.fields = DATA.MANAGER.DEFAULT.METADATA.FIELDS
            private$i.metadata.records = list()
            private$i.metadata.keys = character(0)

            # the hashed url and details strings were made by pasting values with "__"
            split.hashed = function(hashed) {
                values = strsplit(hashed, "__", fixed = T)[[1]]
                values = values[nchar(values) > 0]
                if (length(values) == 0) hashed else values
            }

            pair.cache = new.env()
            n.skipped = 0
            for (outcome in names(private$i.data)) {
                for (metric in names(private$i.data[[outcome]])) {
                    for (source in names(private$i.data[[outcome]][[metric]])) {
                        for (ont in names(private$i.data[[outcome]][[metric]][[source]])) {
                            for (strat in names(private$i.data[[outcome]][[metric]][[source]][[ont]])) {

                                strat.data = private$i.data[[outcome]][[metric]][[source]][[ont]][[strat]]
                                url.ids = private$i.url[[outcome]][[metric]][[source]][[ont]][[strat]]
                                details.ids = private$i.details[[outcome]][[metric]][[source]][[ont]][[strat]]

                                record.ids = array(NaN, dim(strat.data), dimnames(strat.data))

                                # The url and details should be numeric arrays of hashes; anything else is from a format we can't convert
                                if (!is.numeric(url.ids) || !is.numeric(details.ids) || length(url.ids) != length(strat.data) || length(details.ids) != length(strat.data)) {
                                    n.skipped = n.skipped + 1
                                    private$i.metadata[[outcome]][[metric]][[source]][[ont]][[strat]] = record.ids
                                    next
                                }

                                has.both = which(!is.na(url.ids) & !is.na(details.ids))
                                pair.keys = paste(url.ids[has.both], details.ids[has.both], sep = "_")
                                for (pair in unique(pair.keys)) {
                                    id = pair.cache[[pair]]
                                    if (is.null(id)) {
                                        hashes = as.integer(strsplit(pair, "_", fixed = T)[[1]])
                                        record = private$normalize.metadata.record(list(url = split.hashed(private$i.url.list[[hashes[1]]]),
                                                                                        details = split.hashed(private$i.details.list[[hashes[2]]])))
                                        id = private$get.or.add.metadata.record(record)
                                        pair.cache[[pair]] = id
                                    }
                                    record.ids[has.both[pair.keys == pair]] = id
                                }

                                private$i.metadata[[outcome]][[metric]][[source]][[ont]][[strat]] = record.ids
                            }
                        }
                    }
                }
            }

            if (n.skipped > 0)
                warning(paste0("While building metadata records for data manager '", private$i.name, "', ", n.skipped,
                               " stratification", ifelse(n.skipped==1, " had", "s had"), " url or details in an unrecognized format; ",
                               "those data points will have no metadata"))
        },
        
        do.aggregation = function(pre.agg.dimnames,
                                  data.to.aggregate,
                                  keep.dimensions,
                                  data.type,
                                  metric,
                                  scale,
                                  source.name,
                                  outcome,
                                  dimension.values,
                                  target.ontology,
                                  outcome.info,
                                  na.rm) {
            
            default.failure.return <- NULL
            
            post.agg.dimensions = intersect(names(pre.agg.dimnames), keep.dimensions)
            post.agg.dimnames = pre.agg.dimnames[post.agg.dimensions]
            
            if (metric %in% c("coefficient.of.variance", "standard.deviation")) {
                # browser()
                converted.and.cv.estimate <- private$do.conversion.to.variance(metric=metric,
                                                                               data.to.process=data.to.aggregate,
                                                                               outcome,
                                                                               source.name,
                                                                               keep.dimensions.for.pull = names(pre.agg.dimnames),
                                                                               dimension.values.for.pull = dimension.values[names(dimension.values) %in% names(pre.agg.dimnames)],
                                                                               strat.dimnames=strat.dimnames,
                                                                               target.ontology=target.ontology) # Difference, here want target but before don't
                # Other difference, use special dimension values and keep dimensions for the recursive pull
                data.to.aggregate <- converted.and.cv.estimate$converted.data
                if (is.null(data.to.aggregate)) return(default.failure.return)
            }
            if (scale %in% c('non.negative.number', 'number')) {
                aggregated.data = apply.robust(data.to.aggregate, post.agg.dimensions, sum, na.rm=na.rm)
            }
            
            else if (scale %in%  c('rate', 'time', 'proportion', 'ratio')) {
                aggregated.data = private$do.mapping.or.aggregation.of.fraction(is.aggregation=T,
                                                                                data.to.process = data.to.aggregate,
                                                                                denom.dim.vals=pre.agg.dimnames,
                                                                                source.name = source.name,
                                                                                denominator.outcome = outcome.info$denominator.outcome,
                                                                                denominator.sources=NULL,
                                                                                denominator.lags.by.one.year = outcome.info$denominator.lags.by.one.year,
                                                                                target.ontology.for.pull=target.ontology, ## for mapping would be strat.dimnames
                                                                                na.rm=na.rm,
                                                                                square.denominator= metric %in% c("coefficient.of.variance", "standard.deviation", "variance"),
                                                                                mapping.to.apply=NULL,
                                                                                dimnames.for.apply=NULL,
                                                                                post.agg.dimnames=post.agg.dimnames)
            }
            else stop(paste0(error.prefix, 'aggregating ', scale, ' data is not yet implemented'))
            
            ## PROCESS METRICS
            
            aggregated.data
            
        },
        
        do.pre.aggregation.processing =function(append.attributes,
                                                mapping.to.apply,
                                                strat.dimnames,
                                                dimnames.for.apply,
                                                strat.data,
                                                metric,
                                                outcome,
                                                source.name,
                                                keep.dimensions,
                                                dv.names,
                                                outcome.info,
                                                na.rm,
                                                append.attributes.data=NULL) {
            # Return all NULL if failure due to being unable to map or lacking a needed denominator
            default.failure.return <- lapply(c("data", append.attributes), function(data.type) {NULL})
            
            if (!mapping.to.apply$can.apply.to.dim.names(from.dim.names = strat.dimnames,
                                                         to.dim.names = dimnames.for.apply,
                                                         throw.errors = F))
                return(default.failure.return)
            
            data.to.process <- strat.data
            
            if (metric %in% c("coefficient.of.variance", "standard.deviation")) {
                converted.and.cv.estimate <- private$do.conversion.to.variance(metric=metric,
                                                                               data.to.process=data.to.process,
                                                                               outcome=outcome,
                                                                               source.name=source.name,
                                                                               keep.dimensions.for.pull = union(keep.dimensions, dv.names),
                                                                               dimension.values.for.pull = strat.dimnames,
                                                                               target.ontology=NULL)
                data.to.process <- converted.and.cv.estimate$converted.data
                if (is.null(data.to.process)) return(default.failure.return)
            }
            
            # Mappings inherently perform sum operations, but that is invalid for these scales. We therefore can only map the counts and then reproduce the rate/time/proportions/ratios afterwards.
            # If we have an identity mapping, then we can skip this
            if (outcome.info[['metadata']][['scale']] %in% c('rate', 'time', 'proportion', 'ratio') && !mapping.to.apply$is.identity.mapping) {
                mapped.data <- private$do.mapping.or.aggregation.of.fraction(is.aggregation=F,
                                                                             data.to.process=data.to.process,
                                                                             denom.dim.vals=strat.dimnames,
                                                                             source.name=source.name,
                                                                             denominator.outcome=outcome.info$denominator.outcome,
                                                                             denominator.sources=NULL,
                                                                             denominator.lags.by.one.year = outcome.info$denominator.lags.by.one.year,
                                                                             target.ontology.for.pull = strat.dimnames,
                                                                             na.rm=na.rm,
                                                                             square.denominator= metric %in% c("coefficient.of.variance", "standard.deviation", "variance"),
                                                                             mapping.to.apply=mapping.to.apply,
                                                                             dimnames.for.apply=dimnames.for.apply,
                                                                             post.agg.dimnames=NULL)
                if (is.null(mapped.data)) return(default.failure.return)
            }
            
            else
                mapped.data <- mapping.to.apply$apply(data.to.process,
                                                      na.rm = na.rm,
                                                      to.dim.names = dimnames.for.apply,
                                                      fun = "sum")
            
            if (metric %in% c("coefficient.of.variance", "standard.deviation"))
                mapped.data <- private$do.conversion.from.variance(metric,
                                                                   mapped.data,
                                                                   converted.and.cv.estimate$estimate.data.for.cv,
                                                                   mapping.to.apply,
                                                                   dimnames.for.apply,
                                                                   na.rm)
            
            mapped.metadata <- lapply(append.attributes.data, function(metadata.to.process) {
                mapped.metadata.this.type <- mapping.to.apply$apply(metadata.to.process,
                                                                    na.rm=na.rm,
                                                                    to.dim.names=dimnames.for.apply,
                                                                    fun = private$merge.metadata.ids)

                # We might need to subset the metadata if the 'data' was unexpectedly subset due to denominator data for a proportion having fewer years or locations
                if (outcome.info$metadata$scale %in% c("rate", "time", "proportion", "ratio") || metric =="coefficient.of.variance")
                    mapped.metadata.this.type <- array.access(mapped.metadata.this.type, dimnames(mapped.data))
                mapped.metadata.this.type
            })
            
            mapped.data.by.type <- setNames(c(list(mapped.data), mapped.metadata), c("data", append.attributes))
            
        },
        
        do.conversion.to.variance = function(metric,
                                             data.to.process,
                                             outcome,
                                             source.name,
                                             keep.dimensions.for.pull, #union(keep.dimensions, dv.names) vs. names(pre.agg.dimnames)
                                             dimension.values.for.pull, #strat.dimnames vs. dimension.values[names(dimension.values) %in% names(pre.agg.dimnames)]
                                             strat.dimnames,
                                             target.ontology) {
            # Convert to standard.deviation
            estimate.data.for.cv = NULL
            if (metric == "coefficient.of.variance") {
                
                # Check if this source has data, otherwise use all available sources and apply mean over result, or give up.
                if (source.name %in% names(private$i.data[[outcome]][['estimate']]))
                    estimate.source = source.name
                else if (length(names(private$i.data[[outcome]][['estimate']]))>0)
                    estimate.source = names(private$i.data[[outcome]][['estimate']])
                else return(NULL)
                
                # pull with no target ontology because this has to mesh the strat dimnames as they are here, and we know they will
                estimate.data.for.cv = self$pull(outcome = outcome,
                                                 metric = 'estimate',
                                                 source = estimate.source,
                                                 keep.dimensions = union(keep.dimensions.for.pull, names(dimension.values.for.pull)),
                                                 dimension.values = dimension.values.for.pull)
                
                if (is.null(estimate.data.for.cv)) return(NULL)
                
                estimate.data.for.cv = private$do.strip.source.dimension(estimate.data.for.cv)
                
                # Intersect to achieve overlap
                dimnames.in.common = get.dimension.values.overlap(dimnames(data.to.process), dimnames(estimate.data.for.cv))
                data.to.process = array.access(data.to.process, dimnames.in.common)
                estimate.data.for.cv = array.access(estimate.data.for.cv, dimnames.in.common)
                
                # sd = cv * mean
                data.to.process = data.to.process * estimate.data.for.cv
            }
            
            # Convert to variance
            if (metric %in% c("coefficient.of.variance", "standard.deviation"))
                data.to.process = data.to.process ** 2
            
            # Return both data.to.process and estimate.data.for.cv, which will be needed later for re-conversion
            list(converted.data=data.to.process, estimate.data.for.cv=estimate.data.for.cv)
        },
        
        do.conversion.from.variance = function(metric,
                                               data.to.process,
                                               estimate.data.for.cv,
                                               mapping.to.apply,
                                               dimnames.for.apply,
                                               na.rm) {
            
            # Convert back to standard deviation from variance
            if (metric %in% c("standard.deviation", "coefficient.of.variance")) data.to.process <- sqrt(data.to.process)
            
            # Convert back to coefficient of variance from standard deviation
            if (metric == "coefficient.of.variance") {
                # map the estimate data for cv
                estimate.data.for.cv <- mapping.to.apply$apply(estimate.data.for.cv,
                                                               na.rm=na.rm,
                                                               to.dim.names = dimnames.for.apply,
                                                               fun = "sum")
                
                # We may have lost dimension values compared to what we started with
                if (!dim.names.equal(dimnames(data.to.process), dimnames(estimate.data.for.cv))) {
                    dimnames.in.common = get.dimension.values.overlap(dimnames(data.to.process), dimnames(estimate.data.for.cv))
                    data.to.process = array.access(data.to.process, dimnames.in.common)
                    estimate.data.for.cv = array.access(estimate.data.for.cv, dimnames.in.common)
                }
                
                # cv = sd / mean
                data.to.process = data.to.process / estimate.data.for.cv
            }
            
            data.to.process
        },
        
        #' @param is.aggregation If F, then is considered a mapping step.
        #' @param denom.dim.vals Used to subset the denominator the same way the proportion is.
        #' @param square.denominator For metrics that have been converted to variance, so that the denominator data is squared during the weighted average.
        do.mapping.or.aggregation.of.fraction = function(is.aggregation,
                                                         data.to.process,
                                                         denom.dim.vals,
                                                         source.name,
                                                         denominator.outcome,
                                                         denominator.sources,
                                                         denominator.lags.by.one.year,
                                                         target.ontology.for.pull,
                                                         na.rm=na.rm,
                                                         square.denominator=F,
                                                         mapping.to.apply=NULL,
                                                         dimnames.for.apply=NULL,
                                                         post.agg.dimnames=NULL,
                                                         debug=F) {
            if (debug) browser()
            # The dimension.values for this pull are usually the strat.dimnames, but if we have a denominator offset,
            # then we need the year part of it to be changed by <offset>, like 2020 -> 2019.
            # denom.dim.vals = strat.dimnames
            if (!is.null(denominator.lags.by.one.year))
                denom.dim.vals$year = as.character(as.numeric(denom.dim.vals$year) - denominator.lags.by.one.year)
            
            # Now we'll just do all the denominator sources
            
            
            
            # The same source might not be the right one if we aggregated locations into another source.
            # We could try each of our sources until we get one, starting with whichever matches, but trying others.
            denominator.sources = names(private$i.data[[denominator.outcome]][['estimate']])
            
            denominator.array = NULL
            
            
            denominator.array = self$pull(outcome = denominator.outcome,
                                          metric = 'estimate',
                                          keep.dimensions = names(denom.dim.vals),
                                          dimension.values = denom.dim.vals,
                                          sources = denominator.sources,
                                          target.ontology = target.ontology.for.pull,
                                          allow.mapping.from.target.ontology = F,
                                          from.ontology.names = NULL, # I suppose we have no choice since the same source could use different ontologies for its denominator
                                          na.rm = na.rm,
                                          check.arguments = T) ##
            if (is.null(denominator.array)) return(NULL)
            
            denominator.array <- private$do.strip.source.dimension(denominator.array)
            
            # If we had an offset, rename the year dimension names to match the main data
            if (!is.null(denominator.lags.by.one.year))
                dimnames(denominator.array)$year = as.character(as.numeric(dimnames(denominator.array)$year) + denominator.lags.by.one.year)
            
            # So apparently it's possible that the ontology we get denominator data from can have the same dimension values but in a different order from those in our main data
            denom.to.data.mapping = get.ontology.mapping(dimnames(denominator.array), as.ontology(dimnames(data.to.process), incomplete.dimensions=c('year', 'location'))) # made it as.ontology b/c couldn't map year otherwise
            if (is.null(denom.to.data.mapping))
                stop(paste0(error.prefix, 'bug in aggregation code: denominator array dimensions cannot be mapped to main data dimensions'))
            
            # It's possible that we didn't find as many years or locations in the denominator as we did in the data.to.process
            # In fact, we might have lost years/locations that we needed to have according to our dimension.values
            if (!setequal(dimnames(denominator.array)$year, dimnames(data.to.process)$year) || !setequal(dimnames(denominator.array)$location, dimnames(data.to.process)$location))
                data.to.process = array.access(data.to.process, year=dimnames(denominator.array)$year, location=dimnames(denominator.array)$location)
            
            denominator.array = denom.to.data.mapping$apply(denominator.array, to.dim.names = dimnames(data.to.process))
            
            # Note that if some of data.to.process is NA when the denominator array isn't, we don't want the denominator to have contributions the numerator won't have
            denominator.array[is.na(data.to.process)]=NA
            
            # This is used for variance-type metrics where the denominator must be squared
            if (square.denominator)
                denominator.array = denominator.array ** 2
            
            # Perform weighted average
            unmapped.numerator = weighted.value.array = data.to.process * denominator.array
            
            ## I'D LIKE TO HAVE A CHECK FOR WHETHER THIS MAPPING CAN BE APPLIED, NOW THAT WE HAVE DIFFERENT DIMENSION VALUES, LIKE MISSING LOCATIONS
            ## BUT CHECK.CAN.APPLY.TO.DIM.NAMES METHOD OF MAPPINGS DOESN'T WORK YET
            # Context: Asking for Baltimore in dimnames.for.apply, but denominator data doesn't have it. The mapping will fail.
            
            ## IF AGGREGATING
            if (is.aggregation) {
                numerator.totals.array = apply.robust(weighted.value.array, names(post.agg.dimnames), sum, na.rm=na.rm)
                denominator.totals.array = apply.robust(denominator.array, names(post.agg.dimnames), sum, na.rm=na.rm)
                numerator.totals.array/denominator.totals.array
            }
            
            ## IF MAPPING
            else {
                mapped.numerator = mapping.to.apply$apply(unmapped.numerator,
                                                          na.rm = na.rm,
                                                          to.dim.names = dimnames.for.apply,
                                                          fun = "sum")
                mapped.denominator = mapping.to.apply$apply(denominator.array,
                                                            na.rm = na.rm,
                                                            to.dim.names = dimnames.for.apply,
                                                            fun = "sum")
                mapped.numerator/mapped.denominator
            }
        },
        
        
        #' @param arr An array. If it has no source dimension, this function does nothing.
        #' @param allow.mean.across.multiple.sources Should multiple sources be condensed by taking their mean?
        #' @return The input array but without a source dimension
        #' @export
        do.strip.source.dimension = function(arr, allow.mean.across.multiple.sources=T) {
            non.source.dimensions = setdiff(names(dim(arr)), "source")
            if (!("source" %in% names(dim(arr))))
                arr
            else if (dim(arr)["source"]==1)
                array(arr, dim(arr)[non.source.dimensions], dimnames(arr)[non.source.dimensions])
            else if (allow.mean.across.multiple.sources)
                apply.robust(arr, non.source.dimensions, mean, na.rm=T)
            else
                stop("Cannot strip source dimension since there are multiple sources yet 'allow.mean.across.multiple.sources' is FALSE")
        },
        
        do_inspect_marginals = function(outcome,
                                        source,
                                        ontology,
                                        sub.stratification,
                                        super.stratification,
                                        years,
                                        locations,
                                        threshold.magnitude,
                                        threshold.percent,
                                        check.up.to.n.way.stratified,
                                        error.prefix,
                                        debug=F) {
            if (debug) browser()
            
            # Find usable outcomes
            usable_outcomes <- names(private$i.outcome.info)[sapply(private$i.outcome.info, function(outcome) {outcome$metadata$scale == "non.negative.number"})]
            usable_outcomes <- intersect(names(private$i.data), usable_outcomes)
            if (!is.null(outcome) && !(outcome %in% usable_outcomes))
                stop(paste0(error.prefix, "'outcome', if specified, must be have scale 'non.negative.number'"))
            if (!is.null(outcome)) usable_outcomes <- intersect(outcome, usable_outcomes)
            
            # Return a nested list of data frames, like our old method
            return_all_outcomes <- lapply(usable_outcomes, function(outcome) {
                
                sources_this_outcome <- names(private$i.data[[outcome]][["estimate"]])
                if (!is.null(source)) sources_this_outcome <- intersect(source, sources_this_outcome)
                
                return_this_outcome <- lapply(sources_this_outcome, function(source) {
                    
                    ontologies_this_source <- names(private$i.data[[outcome]][["estimate"]][[source]])
                    if (!is.null(ontology)) ontologies_this_source <- intersect(ontology, ontologies_this_source)
                    
                    return_this_source <- lapply(ontologies_this_source, function(ontology) {
                        
                        super_strats_this_ont <- sub_strats_this_ont <- names(private$i.data[[outcome]][["estimate"]][[source]][[ontology]])
                        if (!is.null(super.stratification)) super_strats_this_ont <- intersect(super.stratification, super_strats_this_ont)
                        if (!is.null(sub.stratification)) sub_strats_this_ont <- intersect(sub.stratification, sub_strats_this_ont)
                        
                        return_this_ont <- lapply(super_strats_this_ont, function(super_strat) {
                            return_this_super_strat <- lapply(sub_strats_this_ont, function(sub_strat) {
                                # Return a data frame for each. It may have no rows.
                                df <- private$do_inspect_particular_marginals(outcome = outcome,
                                                                              source = source,
                                                                              ontology = ontology,
                                                                              sub.stratification = sub_strat,
                                                                              super.stratification = super_strat,
                                                                              years = years,
                                                                              locations = locations,
                                                                              threshold.magnitude = threshold.magnitude,
                                                                              threshold.percent = threshold.percent,
                                                                              check.up.to.n.way.stratified = check.up.to.n.way.stratified,
                                                                              error.prefix=error.prefix,
                                                                              debug=debug)
                            })
                            names(return_this_super_strat) <- sub_strats_this_ont
                            return_this_super_strat <- return_this_super_strat[!sapply(return_this_super_strat, is.null)]
                            if (length(return_this_super_strat)==0) return(NULL)
                            return_this_super_strat
                        })
                        names(return_this_ont) <- super_strats_this_ont
                        return_this_ont <- return_this_ont[!sapply(return_this_ont, is.null)]
                        if (length(return_this_ont)==0) return(NULL)
                        return_this_ont
                    })
                    names(return_this_source) <- ontologies_this_source
                    return_this_source <- return_this_source[!sapply(return_this_source, is.null)]
                    if (length(return_this_source)==0) return(NULL)
                    return_this_source
                })
                names(return_this_outcome) <- sources_this_outcome
                return_this_outcome <- return_this_outcome[!sapply(return_this_outcome, is.null)]
                if (length(return_this_outcome)==0) return(NULL)
                return_this_outcome
            })
            names(return_all_outcomes) <- usable_outcomes
            return_all_outcomes <- return_all_outcomes[!sapply(return_all_outcomes, is.null)]
            if (length(return_all_outcomes)==0) return(NULL)
            return_all_outcomes
        },
        
        do_inspect_particular_marginals = function(outcome,
                                                   source,
                                                   ontology,
                                                   sub.stratification,
                                                   super.stratification,
                                                   years,
                                                   locations,
                                                   threshold.magnitude,
                                                   threshold.percent,
                                                   check.up.to.n.way.stratified,
                                                   error.prefix,
                                                   debug=F) {
            # TO ADD: year and location subsetting.
            if (debug) browser()
            
            # If the same, return NULL
            if (super.stratification == sub.stratification)
                return(NULL)
            
            # Break the strats into their dimensions
            super_dimensions <- unlist(strsplit(super.stratification, "__"))
            sub_dimensions <- unlist(strsplit(sub.stratification, "__"))
            
            if (length(super_dimensions) > 2 + check.up.to.n.way.stratified ||
                length(sub_dimensions) > 2 + check.up.to.n.way.stratified)
                return(NULL)
            
            # If the super is not actually super to the sub, return NULL
            if (length(setdiff(sub_dimensions, super_dimensions))==0 || length(setdiff(super_dimensions, sub_dimensions)!=0))
                return(NULL)
            
            # If there are not any values to compare, return NULL
            shared_dimnames <- intersect.shared.dim.names(dimnames(private$i.data[[outcome]][["estimate"]][[source]][[ontology]][[sub.stratification]]),
                                                          dimnames(private$i.data[[outcome]][["estimate"]][[source]][[ontology]][[super.stratification]]))
            if (any(sapply(shared_dimnames, length)==0))
                return(NULL)
            
            # Else, find marginals of sub and compare to super. We use na.rm=FALSE because there's no use finding a sum of only a portion of the stratum.
            sub_data <- array.access(private$i.data[[outcome]][["estimate"]][[source]][[ontology]][[sub.stratification]], shared_dimnames)
            super_data <- array.access(private$i.data[[outcome]][["estimate"]][[source]][[ontology]][[super.stratification]], shared_dimnames)
            sub_marginals <- apply(sub_data, MARGIN=names(dim(super_data)), FUN=sum, na.rm=F)
            
            # # Check for sub_marginals where they really are just NA
            # sub_all_NA <- apply(is.na(sub_data), MARGIN=names(dim(super_data)), FUN=all)
            
            # Verify that the dimnames are the same now (values should be ordered within a dimension, but you never know...)
            if (!identical(dimnames(super_data), dimnames(sub_marginals)))
                stop(paste0("Error inspecting totals: dimension value order assumption broken! Contact Andrew"))

            # Now make a data frame with columns for each dimension as well as
            # columns for the marginal sum, super value, diff, and % diff
            
            # We checked that they have exactly the same dimnames, so we can cbind them.
            super_df <- reshape2::melt(super_data, value.name = "super_value")
            marginals_df <- reshape2::melt(sub_marginals, value.name = "marginal_sum")
            df <- cbind(super_df, marginal_sum = marginals_df$marginal_sum)
            df["magnitude_diff"] <- df$super_value - df$marginal_sum
            df["percent_diff"] <- round(100 * df$magnitude_diff / df$super_value, 1)
            
            # Now cut out NA rows and those below thresholds
            df <- df[!is.na(df$percent_diff),]
            df <- df[abs(df$magnitude_diff) >= threshold.magnitude,]
            df <- df[abs(df$percent_diff) >= threshold.percent,]
            
            # Now cut down to the years and locations requested
            if (!is.null(years)) {
                if (!("year" %in% colnames(df)))
                    stop(paste0(error.prefix, "Unusual situation encountered: 'year' is not a dimension in the data!"))
                df <- df[df$year %in% years,]
            }
            if (!is.null(locations)) {
                if (!("location" %in% colnames(df)))
                    stop(paste0(error.prefix, "Unusual situation encountered: 'location' is not a dimension in the data!"))
                df <- df[df$location %in% locations,]
            }
            
            if (nrow(df) == 0)
                return(NULL)
            
            df
        }
    )
)
