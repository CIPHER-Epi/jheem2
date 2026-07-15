
## ***OVERVIEW***
##
## See ONTOLOGY_ontology.R for description and implementation of what an ontology is
## 
## An ontology.mapping comprises the information for how to convert data that accords to one ontology
##  into data that accords to a different ontology.
## For example, if ontology A partitions race as "black", "white", "hispanic", "aapi", "other"
##  and ontology B partitions race as "black", "hispanic", "other"
##  a mapping from A to B would know that "black" in B = "black" in A, "hispanic" in B = "hispanic" in A,
##  and "other" in B = "white" + "aapi" + "other" in A
## 
## We implement the mappings as R6 objects that store the information on how to convert as member variables,
##  and also bundle in methods to execute conversions
##  
## While ontology.mappings can be created and applied directly, we also include functionality to
##  "register" mappings - this means that they are globally accessible and can be searched.
## This allows some client that knows it has data in one ontology to request a mapping to a different
##  ontology without having to track those mappings itself
## Note: we may also encounter a situation where we have some data in ontology A and some other data
##  in ontology B that we would like to use together, but have no mapping from either A -> B or B -> A. 
##  However, we may have mappings from both A and B to a 3rd ontology (call it C): A -> C and B -> C,
##  that would let us use the data together. We refer to this as finding a set of mappings that
##  "aligns" data from ontology A with data from ontology B.
##  (Note that, since we can consider no change to an ontology as an "identity" ontology mapping, aligning
##   A and B could be satisfied if we have either a mapping A -> B or B -> A)

##----------------------##
##----------------------##
##-- PUBLIC INTERFACE --##
##----------------------##
##----------------------##

#'@title Create a mapping that allows transformations of arrays
#'
#'@param from.dimensions A character vector of names of the dimensions we are mapping from
#'@param to.dimensions A character vector of names of the dimensions we are mapping to
#'@param mappings A character matrix, with a number of columns == length(from.dimensions) + length(to.dimensions). The first length(from.dimensions) columns represent values for the from.dimensions (in the same order as from.dimensions), and the last length(to.dimensions) colums represent values for to.dimensions (in the order of to.dimensions). Each row represents a mapping from a set of values for from.dimension to a set of values for to.dimensions. NB: any dimnames on mappings will be ignored
#'@param error.prefix A text string to prepend to any error messages generated in executing the function
#'
#'@export
create.ontology.mapping <- function(mappings,
                                    from.dimensions,
                                    to.dimensions=from.dimensions,
                                    name = NULL,
                                    error.prefix = "Error creating ontology mapping: ")
{
    #-- Validate the error prefix --#
    if (!is.character(error.prefix) || length(error.prefix)!=1 || is.na(error.prefix) || nchar(error.prefix)==0)
        stop("Error creating ontology mapping: 'error.prefix' must be a single, non-NA, non-empty character value")
    
    #-- Validate the name --#
    if (!is.null(name) && (!is.character(name) || length(name)!=1 || is.na(name) || nchar(name)==0))
        stop(paste0(error.prefix), "If it is not NULL, 'name' must be a non-empty, non-NA single character value")
    
    #-- Validate from.dimensions --#
    if (!is.character(from.dimensions) || length(from.dimensions)==0)
        stop(paste0(error.prefix, "'from.dimensions' must be a non-empty character vector"))
    if (any(is.na(from.dimensions)))
        stop(paste0(error.prefix, "'from.dimensions' cannot contain NA values"))
    if (any(nchar(from.dimensions)==0))
        stop(paste0(error.prefix, "'from.dimensions' cannot contain empty ('') values"))
    
    n.from.dimensions = length(from.dimensions)
    
    #-- Validate to.dimensions --#
    if (!is.character(to.dimensions) || length(to.dimensions)==0)
        stop(paste0(error.prefix, "'to.dimensions' must be a non-empty character vector"))
    if (any(is.na(to.dimensions)))
        stop(paste0(error.prefix, "'to.dimensions' cannot contain NA values"))
    if (any(nchar(to.dimensions)==0))
        stop(paste0(error.prefix, "'to.dimensions' cannot contain empty ('') values"))
    
    n.to.dimensions = length(to.dimensions)
    
    
    #-- Validate dimensions on mappings --#
    
    if (is.null(dim(mappings)) || length(dim(mappings))!=2)
        stop(paste0(error.prefix, "'mappings' must be a two-dimensional data structure (eg, matrix or data frame)"))
    
    if (dim(mappings)[1]==0)
        stop(paste0(error.prefix, "'mappings' cannot be empty"))
    
    if (dim(mappings)[2] != (n.from.dimensions+n.to.dimensions))
        stop(paste0(error.prefix, "'mappings' must have ", 
                    (n.from.dimensions+n.to.dimensions),
                    " columns (one for each dimension in from.dimensions and to.dimensions)"))
    
    if (!all(apply(mappings, 2, is.character)))
        stop(paste0(error.prefix, "The columns of 'mappings' must all be character vectors"))
    
    mappings = as.matrix(mappings)
    n.values = dim(mappings)[1]
    
    from.values = mappings[,1:n.from.dimensions, drop=F]
    dimnames(from.values)[[2]] = from.dimensions
    
    to.values = mappings[,n.from.dimensions + 1:n.to.dimensions, drop=F]
    dimnames(to.values)[[2]] = to.dimensions
    
    #-- Check for rows with all NAs - not allowed --#
    all.na.rows = apply(is.na(mappings), 1, all)
    if (any(all.na.rows))
        stop(paste0(error.prefix,
                    "The ",
                    collapse.with.and(get.ordinal((1:n.values)[all.na.rows])),
                    ifelse(sum(all.na.rows)==1, ' row contains', ' rows contain'),
                    " only NA values"))
    
    
    #-- Check for NAs in the from.values - allowed ONLY if all values in a row are NA --#
    #   (And at least some values have to be non-NA)
    if (n.from.dimensions>1)
    {
        invalid.na.mask = apply(is.na(from.values), 1, any) &
            !apply(is.na(from.values), 1, all)
        
        if (any(invalid.na.mask))
            stop(paste0(error.prefix,
                        "Rows representing 'from' values in 'mappings' (in the 1st",
                        "-", get.ordinal(n.from.dimensions),
                        " columns) cannot contain NA values unless all 'from' values in the row are NA."))
    }
    
    if (all(is.na(from.values)))
        stop(paste0(error.prefix,
                    "The column",
                    ifelse(n.from.dimensions==1, '', 's'),
                    " representing 'to' values in 'mappings' (the 1st",
                    ifelse(n.from.dimensions==1, '',
                           paste0("-", get.ordinal(n.from.dimensions))),
                    ") cannot contain *ALL* NA values"))
    
    #-- Check for NAs in the to.values - allowed ONLY if all values in a row are NA --#
    if (n.to.dimensions>1)
    {
        invalid.na.mask = apply(is.na(to.values), 1, any) &
            !apply(is.na(to.values), 1, all)
        
        if (any(invalid.na.mask))
            stop(paste0(error.prefix,
                        "Rows representing 'to' values in 'mappings' (in the ",
                        get.ordinal(n.from.dimensions+1),
                        "-", get.ordinal(n.from.dimensions+n.to.dimensions),
                        " columns) cannot contain NA values unless all 'to' values in the row are NA."))
    }
    
    if (all(is.na(to.values)))
        stop(paste0(error.prefix,
                    "The column",
                    ifelse(n.to.dimensions==1, '', 's'),
                    " representing 'to' values in 'mappings' (the ",
                    get.ordinal(n.from.dimensions+1),
                    ifelse(n.to.dimensions==1, '',
                           paste0("-", get.ordinal(n.from.dimensions+n.to.dimensions))),
                    ") cannot contain *ALL* NA values"))
    
    
    
    #-- Make sure every combination of (non-NA) from values is used --#
    
    no.from.na.mask = !apply(is.na(from.values), 1, all)
    num.unique.non.na.from.combos = dim(unique(from.values[no.from.na.mask,,drop=F]))[1]
    
    unique.non.na.from.values = lapply(1:n.from.dimensions, function(i){
        unique(from.values[!is.na(from.values[,i]), i])
    })
    n.possible.from.combos = prod(sapply(unique.non.na.from.values, length))
    if (n.from.dimensions>1 &&
        num.unique.non.na.from.combos < n.possible.from.combos) 
    {
        all.from.combos = get.every.combination(unique.non.na.from.values)
        missing.from.combos = setdiff.rows(all.from.combos,
                                           from.values[no.from.na.mask,,drop=F])
        
        stop(paste0(error.prefix,
                    "Rows representing 'from' values in 'mappings' (in the 1st",
                    "-", get.ordinal(n.from.dimensions),
                    " columns) must contain every possible combination of from values. The following combination",
                    ifelse(dim(missing.from.combos)[1]==1, " is", "s are"),
                    " missing: ",
                    collapse.with.and("<",
                                      apply(missing.from.combos, 1, function(vals){
                                          paste0("'", vals, "'", collapse=',')
                                      }),
                                      ">")
        ))
    }
    
    
    #-- Pad NAs to fill out every combination of 'to' values --#
    
    no.to.na.mask = !apply(is.na(to.values), 1, all)
    num.unique.non.na.to.combos = dim(unique(to.values[no.to.na.mask,,drop=F]))[1]
    
    unique.non.na.to.values = lapply(1:n.to.dimensions, function(i){
        unique(to.values[!is.na(to.values[,i]), i])
    })
    n.possible.to.combos = prod(sapply(unique.non.na.to.values, length))
    if (n.to.dimensions>1 && 
        num.unique.non.na.to.combos < n.possible.to.combos)
    {
        all.to.combos = get.every.combination(unique.non.na.to.values)
        missing.to.combos = setdiff.rows(all.to.combos, to.values)
        
        from.values = rbind(from.values,
                            matrix(NA, nrow=dim(missing.to.combos)[1], ncol=n.from.dimensions))
        to.values = rbind(to.values, missing.to.combos)
    }
    
    #-- Make the mapping --#
    
    BASIC.ONTOLOGY.MAPPING$new(name=name,
                               from.dimensions=from.dimensions,
                               to.dimensions=to.dimensions,
                               from.values=from.values,
                               to.values=to.values)
}


#'@title Create and register a mapping that allows transformations of arrays
#'
#'@param name A descriptive single character value
#'@inheritParams create.ontology.mapping
#'
#'@details The mappings must contain every possible combination of 'from' values (ie, every combo of from values must map to something). A row of 'mappings' may have NA values in ALL the 'from' columns (indicating there is no set of 'from' values that maps to the corresponding 'to' values), or may have NA values in ALL the 'to' columns (indicating the that the given set of 'from' values will be discarded in the transformation)
#'
#'@export
register.ontology.mapping <- function(name,
                                      mappings,
                                      from.dimensions,
                                      to.dimensions=from.dimensions)
{
    #-- Validate the Name --#
    if (!is.character(name) || length(name)!=1 || is.na(name) || nchar(name)==0)
        stop("Error registering ontology mapping: 'name' must be a single, non-NA, non-empty character value")
    
    error.prefix = paste0("Error registering ontology mapping '", name, "': ")
    mapping = create.ontology.mapping(mappings=mappings,
                                      from.dimension=from.dimensions,
                                      to.dimensions=to.dimensions,
                                      name = name,
                                      error.prefix=error.prefix)
    
    ##??? Do we need to parse out subsets of mappings and register those if there are multiple dimensions? ???##
    
    #-- Register the mapping --#
    #   (Only if it has not been registered previously)
    
    equals.mapping = sapply(ONTOLOGY.MAPPING.MANAGER$mappings, function(other.mapping){
        mapping$equals(other.mapping)
    })
    
    if (any(equals.mapping))
    {
        other.name = ONTOLOGY.MAPPING.MANAGER$mappings[equals.mapping][[1]]$name
        if (other.name!=name)
            stop(paste0(error.prefix, "This ontology mapping has already been registered under a different name ('",
                        other.name, "')"))
        #else, just ignore - it's already registered with the same name
    }
    else
    {
        if (any(names(ONTOLOGY.MAPPING.MANAGER$mappings)==name))
            stop(paste0(error.prefix, "A different ontology mapping has already been registered under the name '",
                        name, "'"))
        else
            do.register.mapping(mapping)
        #      ONTOLOGY.MAPPING.MANAGER$mappings[[name]] = mapping
    }
    
    
    #-- If this is a reversible mapping, reverse it and register it --#
    
    n.values = dim(mappings)[1]
    from.values = mapping$from.values
    to.values = mapping$to.values
    
    # it's reversible if it's a one-to-one, complete mapping
    is.reversible = !any(is.na(from.values)) &&
        !any(is.na(to.values)) && 
        dim(unique(from.values))[1] == n.values &&
        dim(unique(to.values))[1] == n.values #ie, there are no duplicate to value combos
    
    if (is.reversible)
    {
        reverse.mapping = BASIC.ONTOLOGY.MAPPING$new(name=paste0(name, " (reversed)"),
                                                     from.dimensions=to.dimensions,
                                                     to.dimensions=from.dimensions,
                                                     from.values=to.values,
                                                     to.values=from.values,
                                                     reverse.of.name=name)
        
        
        equals.reverse.mapping = sapply(ONTOLOGY.MAPPING.MANAGER$mappings, function(other.mapping){
            reverse.mapping$equals(other.mapping)
        })
        
        if (!any(equals.reverse.mapping))
        {
            if (any(names(ONTOLOGY.MAPPING.MANAGER$mappings)==reverse.mapping$name))
                stop(paste0(error.prefix, "A different ontology mapping has already been registered under the name '",
                            name, "' that we sought to use for the reverse of this mapping"))
            else
                do.register.mapping(reverse.mapping)
            #ONTOLOGY.MAPPING.MANAGER$mappings[[reverse.mapping$name]] = reverse.mapping
        }
        #else just ignore - it's already registered 
    }
    
    
    #-- (Invisibly) Return the Mapping --#
    invisible(mapping)
}

do.register.mapping <- function(mapping)
{
    if (!is(mapping, 'basic.ontology.mapping'))
        stop(paste0("Cannot register mapping '", mapping$name, "' - only basic.ontology.mappings can be registered"))
    
    #-- Store it --#
    ONTOLOGY.MAPPING.MANAGER$mappings[[mapping$name]] = mapping
    
    
    #-- Sort the mappings by number of involved dimensions --#
    #   (Since finding a mapping searches through this list from first to last, this will favor more parsimonious solutions)
    
    n.from.per.mapping = sapply(ONTOLOGY.MAPPING.MANAGER$mappings, function(one.mapping){
        length(one.mapping$from.dimensions)
    })
    n.to.per.mapping = sapply(ONTOLOGY.MAPPING.MANAGER$mappings, function(one.mapping){
        length(one.mapping$to.dimensions)
    })
    
    o = order(n.from.per.mapping, n.to.per.mapping, decreasing = F)
    ONTOLOGY.MAPPING.MANAGER$mappings = ONTOLOGY.MAPPING.MANAGER$mappings[o]
    
    
    #-- Put it in lists sorted by dimension --#
    for (d in mapping$from.dimensions)
    {
        sorted.mappings = ONTOLOGY.MAPPING.MANAGER$sorted.mappings.by.from.dimension[[d]]
        if (is.null(sorted.mappings))
            sorted.mappings = list()
        
        sorted.mappings[[mapping$name]] = mapping
        
        n.from.per.mapping = sapply(sorted.mappings, function(one.mapping){
            length(one.mapping$from.dimensions)
        })
        n.to.per.mapping = sapply(sorted.mappings, function(one.mapping){
            length(one.mapping$to.dimensions)
        })
        
        o = order(n.from.per.mapping, n.to.per.mapping, decreasing = F)
        sorted.mappings = sorted.mappings[o]
        
        ONTOLOGY.MAPPING.MANAGER$sorted.mappings.by.from.dimension[[d]] = sorted.mappings
    }
    
    #-- Calculate linkages across dimensions --#
    
    from.and.to.dimensions = union(mapping$from.dimensions, mapping$to.dimensions)
    for (d1 in from.and.to.dimensions)
    {
        if (is.null(ONTOLOGY.MAPPING.MANAGER$dimensions.directly.linked.by.mappings[[d1]]))
            ONTOLOGY.MAPPING.MANAGER$dimensions.directly.linked.by.mappings[[d1]] = d1
        
        # direct linkages
        for (d2 in from.and.to.dimensions)
        {
            if (d1 != d2)
            {
                ONTOLOGY.MAPPING.MANAGER$dimensions.directly.linked.by.mappings[[d1]] = 
                    union(ONTOLOGY.MAPPING.MANAGER$dimensions.directly.linked.by.mappings[[d1]], d2)
            }
        }
    }
    
    
    # direct linkages
    for (d in mapping$from.dimensions)
    {
        if (is.null(ONTOLOGY.MAPPING.MANAGER$dimensions.linked.by.mappings[[d]]))
            ONTOLOGY.MAPPING.MANAGER$dimensions.linked.by.mappings[[d]] = d
        
        dimensions.to.add = setdiff(from.and.to.dimensions,
                                    ONTOLOGY.MAPPING.MANAGER$dimensions.linked.by.mappings[[d]])
        
        while (length(dimensions.to.add)>0)
        {
            ONTOLOGY.MAPPING.MANAGER$dimensions.linked.by.mappings[[d]] = union(
                ONTOLOGY.MAPPING.MANAGER$dimensions.linked.by.mappings[[d]],
                dimensions.to.add
            )
            
            dimensions.linked.to.added.dimensions = unlist(
                ONTOLOGY.MAPPING.MANAGER$dimensions.linked.by.mappings[[dimensions.to.add]]
            )
            dimensions.to.add = setdiff(dimensions.linked.to.added.dimensions,
                                        ONTOLOGY.MAPPING.MANAGER$dimensions.linked.by.mappings[[d]])
        }
    }
    
    #-- Clear cached mappings --#
    ONTOLOGY.MAPPING.MANAGER$cached.one.way.mappings=list()
    ONTOLOGY.MAPPING.MANAGER$cached.two.way.mappings=list()
}


#'@title Get an Ontology Mapping to Transform Data
#'
#'@param from.ontology The ontology to which the data to be transformed conform. Must be an 'ontology' object as created by \code{\link{ontology}}
#'@param to.ontology The ontology to which the transformed data should conform. Must be an 'ontology' object as created by \code{\link{ontology}}
#'@param allow.non.overlapping.incomplete.dimensions Logical indicating whether, when we map to an incomplete dimension, there can be no overlap in the from and to values for that dimension
#'
#'@return Either an object of class 'ontology.mapping' or NULL if no mapping that would bridge the differences is found
#'@export
get.ontology.mapping <- function(from.ontology,
                                 to.ontology,
                                 allow.non.overlapping.incomplete.dimensions = F)
{
    do.get.ontology.mapping(from.ontology = from.ontology,
                            to.ontology = to.ontology,
                            allow.non.overlapping.incomplete.dimensions = allow.non.overlapping.incomplete.dimensions,
                            for.examination = F)   
}

do.get.ontology.mapping <- function(from.ontology,
                                    to.ontology,
                                    allow.non.overlapping.incomplete.dimensions,
                                    for.examination)
{
    #-- Validate Arguments --#
    # check.dim.names.valid(from.ontology,
    #                       variable.name.for.error = 'from.ontology',
    #                       allow.duplicate.values.across.dimensions = T)
    # check.dim.names.valid(to.ontology,
    #                       variable.name.for.error = 'to.ontology',
    #                       allow.duplicate.values.across.dimensions = T)
    
    from.ontology = derive.ontology(from.ontology, var.name.for.error = "'from.ontology'", error.prefix = "Error in get.ontology.mapping(): ")
    to.ontology = derive.ontology(to.ontology, var.name.for.error = "'to.ontology'", error.prefix = "Error in get.ontology.mapping(): ")
    
    hash.from = hash.ontology(from.ontology)
    hash.to = hash.ontology((to.ontology))
    rv = ONTOLOGY.MAPPING.MANAGER$cached.one.way.mappings[[hash.from]][[hash.to]]
    if (is.null(rv) || for.examination || DEBUG.ONTOLOGY.MAPPINGS)
    {
        mappings = find.ontology.mapping(from.ontology = from.ontology,
                                         to.ontology = to.ontology,
                                         required.dimensions = names(to.ontology),
                                         required.dim.names = NULL,
                                         get.two.way.alignment = F,
                                         allow.non.overlapping.incomplete.dimensions = allow.non.overlapping.incomplete.dimensions,
                                         get.details = for.examination)
        
        #-- Package up and cache --#
        rv = combine.ontology.mappings(mappings[[1]], thorough.validation=F)
        
        ONTOLOGY.MAPPING.MANAGER$cached.one.way.mappings[[hash.from]][[hash.to]] = rv
    }
    
    if (is.null(rv) && !for.examination)
    {
        RECENT.FAILED.ONTOLOGY.MAPPINGS$calls = c(
            RECENT.FAILED.ONTOLOGY.MAPPINGS$calls,
            list(list(
                call = 'get.ontology.mapping',
                from.ontology = from.ontology,
                to.ontology = to.ontology,
                allow.non.overlapping.incomplete.dimensions = allow.non.overlapping.incomplete.dimensions
            )))
        
        if (length(RECENT.FAILED.ONTOLOGY.MAPPINGS$calls)>N.FAILED.ONTOLOGY.MAPPINGS.TO.TRACK)
            RECENT.FAILED.ONTOLOGY.MAPPINGS$calls = RECENT.FAILED.ONTOLOGY.MAPPINGS$calls[1:N.FAILED.ONTOLOGY.MAPPINGS.TO.TRACK]
    }
    
    if (for.examination)
        mappings$details
    else
        rv
}
#TRACKED.CALLS = list()

#'@title Get a Pair of Ontology Mappings that Aligns two Data Elements
#'
#'@param ontology.1,ontology.2 The ontologies of two data elements to be transformed
#'@param align.on.dimensions The dimensions which should match in the transformed data
#'@param include.dim.names Optional argument, specifying specific dimension values that must be present in the transformed data
#'@param allow.non.overlapping.incomplete.dimensions Logical indicating whether, when we map to an incomplete dimension, there can be no overlap in the from and to values for that dimension
#'
#'@return Either a (1) NULL - if no mappings could align the ontologies or (2) a list with two elements, each an ontology.mapping object, such that applying the first to data conforming to ontology.1 and applying the second to data conforming to ontology.2 yields two data objects with the same dim.names
#'
#'@export
get.mappings.to.align.ontologies <- function(ontology.1,
                                             ontology.2,
                                             align.on.dimensions = intersect(names(ontology.1), names(ontology.2)),
                                             allow.non.overlapping.incomplete.dimensions = F,
                                             include.dim.names = NULL)
{
    do.get.mappings.to.align.ontologies(ontology.1 = ontology.1,
                                        ontology.2 = ontology.2,
                                        align.on.dimensions = align.on.dimensions,
                                        allow.non.overlapping.incomplete.dimensions = allow.non.overlapping.incomplete.dimensions,
                                        include.dim.names = include.dim.names,
                                        for.examination = F)
}

do.get.mappings.to.align.ontologies <- function(ontology.1,
                                                ontology.2,
                                                align.on.dimensions,
                                                allow.non.overlapping.incomplete.dimensions,
                                                include.dim.names,
                                                for.examination)
{
    #-- Validate Arguments --#
    # check.dim.names.valid(ontology.1,
    #                       variable.name.for.error = 'ontology.1',
    #                       allow.duplicate.values.across.dimensions = T)
    # check.dim.names.valid(ontology.2,
    #                       variable.name.for.error = 'ontology.2',
    #                       allow.duplicate.values.across.dimensions = T)
    
    ontology.1 = derive.ontology(ontology.1, var.name.for.error = "'ontology.1'", error.prefix = "Error getting aligning ontology mappings: ")
    ontology.2 = derive.ontology(ontology.2, var.name.for.error = "'ontology.2'", error.prefix = "Error getting aligning ontology mappings: ")
    
    hash1 = hash.ontology(ontology.1)
    hash2 = hash.ontology(ontology.2)
    rv = ONTOLOGY.MAPPING.MANAGER$cached.two.way.mappings[[hash1]][[hash2]]
    
    if (is.null(rv) || for.examination || DEBUG.ONTOLOGY.MAPPINGS)
    {
        mappings = find.ontology.mapping(from.ontology = ontology.1,
                                         to.ontology = ontology.2,
                                         required.dimensions = align.on.dimensions,
                                         required.dim.names = include.dim.names,
                                         get.two.way.alignment = T,
                                         allow.non.overlapping.incomplete.dimensions = allow.non.overlapping.incomplete.dimensions,
                                         get.details = for.examination)
        
        #-- Package up and return --#
        if (is.null(mappings))
            rv = NULL
        else
            rv = list(mapping.from.1 = combine.ontology.mappings(mappings[[1]], thorough.validation = F),
                      mapping.from.2 = combine.ontology.mappings(mappings[[2]], thorough.validation = F) )
        
        ONTOLOGY.MAPPING.MANAGER$cached.two.way.mappings[[hash1]][[hash2]] = rv
    }
    
    if (is.null(rv) && !for.examination)
    {
        RECENT.FAILED.ONTOLOGY.MAPPINGS$calls = c(
            RECENT.FAILED.ONTOLOGY.MAPPINGS$calls,
            list(list(
                call = 'get.mappings.to.align.ontologies',
                ontology.1 = ontology.1,
                ontology.2 = ontology.2,
                align.on.dimensions = align.on.dimensions,
                allow.non.overlapping.incomplete.dimensions = allow.non.overlapping.incomplete.dimensions,
                include.dim.names = include.dim.names
            )))
        
        if (length(RECENT.FAILED.ONTOLOGY.MAPPINGS$calls)>N.FAILED.ONTOLOGY.MAPPINGS.TO.TRACK)
            RECENT.FAILED.ONTOLOGY.MAPPINGS$calls = RECENT.FAILED.ONTOLOGY.MAPPINGS$calls[1:N.FAILED.ONTOLOGY.MAPPINGS.TO.TRACK]
    }
    
    if (for.examination)
        mappings$details
    else
        rv
}

#'@title Get a Matrix that Applies an Ontology Mapping
#'
#'@inheritParams apply.ontology.mapping
#'
#'@export
get.ontology.mapping.matrix <- function(ontology.mapping,
                                        from.dim.names,
                                        to.dim.names=NULL,
                                        error.prefix='')
{
    if (!is(ontology.mapping, 'ontology.mapping'))
        stop("'ontology.mapping' must be an object of class 'ontology.mapping'")
    
    ontology.mapping$get.matrix(from.dim.names=from.dim.names,
                                to.dim.names=to.dim.names,
                                error.prefix=error.prefix)
}

#'@title Apply an Ontology Mapping to Data
#'
#'@param ontology.mapping An object of class 'ontology.mapping' - obtained either through \link{register.ontology.mapping}, \link{get.ontology.mapping}, \link{get.mappings.to.align.ontologies}
#'@param from.arr An array to transform. Can have any underlying data type, although the function is optimized for numeric data
#'@param to.dim.names The dimnames that the resulting transformed data should have (can represent a subset of the dimnames that the ontology mapping knows how to transform). If NULL, the ontology mapping will keep the dimnames in from.arr except for dimensions that are transformed
#'@param fun A function used to combine values, or the name of a function. The function must be able to accept any number of arguments (including none). The default, 'sum', is optimized for numeric arrays
#'@param na.rm Passed through to fun
#'@param error.prefix A character value to be prepended to any error messages thrown by the function
#'
#'@family Functions to identify why ontologies fail to map
#'
#'@export
apply.ontology.mapping <- function(ontology.mapping,
                                   from.arr,
                                   to.dim.names=NULL,
                                   fun='sum',
                                   na.rm=F,
                                   error.prefix='')
{
    if (!is(ontology.mapping, 'ontology.mapping'))
        stop("'ontology.mapping' must be an object of class 'ontology.mapping'")
    
    ontology.mapping$apply(from.arr = from.arr, 
                           to.dim.names = to.dim.names,
                           fun = fun,
                           error.prefix = error.prefix)
}

#'@title Get an "Identity" Ontology Mapping (that does not change data structure when applied)
#'
#'@return An object of class "ontology.mapping"
#'
#'@family Functions to identify why ontologies fail to map
#'
#'@export
get.identity.ontology.mapping <- function()
{
    IDENTITY.ONTOLOGY.MAPPING$new()
}


#'@title Map Value's Ontology
#'
#'@param value
#'@param target.dim.names
#'@param fun
#'@param allow.expand.values
#'@param allow.map.to.subset.of.target 
#'@param na.rm
#'@param error.prefix
#'
#'@family Functions to identify why ontologies fail to map
#'
#'@export
map.value.ontology <- function(value,
                               target.dim.names,
                               fun = 'sum',
                               allow.expand.values = T,
                               allow.map.to.subset.of.target = T,
                               allow.restratify.ages = T,
                               allow.age.extrapolation = F,
                               na.rm = T,
                               smooth.infinite.age.to = 101,
                               error.prefix = '')
{
    #-- Check Arguments --#
    
    if (!is.array(value))
        stop(paste0(error.prefix, "Cannot map value's ontology - value must be an array"))
    
    if (is.null(dimnames(value)) || is.null(names(dimnames(value))))
        stop(paste0(error.prefix, "Cannot map value's ontology - value must have named dimnames set"))
    
    check.dim.names.valid(dim.names = target.dim.names,
                          variable.name.for.error = 'target.dim.names', 
                          allow.empty = F,
                          error.prefix = paste0(error.prefix, "Cannot map value's ontology - "))
    
    if (!is.logical(allow.expand.values) || length(allow.expand.values)!=1 || is.na(allow.expand.values))
        stop(paste0(error.prefix, "Cannot map value's ontology - 'allow.expand.values' must be a single, non-NA logical value"))
    
    if (!is.logical(allow.map.to.subset.of.target) || length(allow.map.to.subset.of.target)!=1 || is.na(allow.map.to.subset.of.target))
        stop(paste0(error.prefix, "Cannot map value's ontology - 'allow.map.to.subset.of.target' must be a single, non-NA logical value"))
    
    #-- Get Mapping(s) --#
    need.to.restratify.age = F
    
    if (allow.expand.values)
    {
        mappings = get.mappings.to.align.ontologies(ontology.1 = dimnames(value),
                                                    ontology.2 = target.dim.names)
        
        if (is.null(mappings))
        {
            if (allow.restratify.ages && any(names(dimnames(value))=='age'))
            {
                target.dim.names.sans.age = target.dim.names
                target.dim.names.sans.age$age = NULL
                
                value.dim.names.sans.age = dimnames(value)
                value.dim.names.sans.age$age = NULL
                
                mappings = get.mappings.to.align.ontologies(ontology.1 = value.dim.names.sans.age,
                                                            ontology.2 = target.dim.names.sans.age)
                
                need.to.restratify.age = T
            }
            
            if (is.null(mappings))
            {
                stop(paste0(error.prefix, "Cannot map value's ontology - there is no set of mappings that aligns the dimnames of 'value' with 'target.dim.names'"))
            }
        }
        
        mapping.from.value = mappings[[1]]
        mapping.from.target = mappings[[2]]
    }
    else
    {
        mapping.from.value = get.ontology.mapping(from.ontology = dimnames(value),
                                                  to.ontology = target.dim.names)
        
        if (is.null(mapping.from.value))
        {
            if (allow.restratify.ages && any(names(dimnames(value))=='age'))
            {
                target.dim.names.sans.age = target.dim.names
                target.dim.names.sans.age$age = NULL
                
                value.dim.names.sans.age = dimnames(value)
                value.dim.names.sans.age$age = NULL
                
                mapping.from.value = get.ontology.mapping(from.ontology = value.dim.names.sans.age,
                                                          to.ontology = target.dim.names.sans.age)
                
                need.to.restratify.age = T
            }
            
            if (is.null(mapping.from.value))
                stop(paste0(error.prefix, "Cannot map value's ontology - there is no mapping that maps from the dimnames of 'value' to 'target.dim.names'"))
        }
        
        mapping.from.target = NULL
    }
    
    #-- Apply Mapping --#
    
    if (is.null(mapping.from.target) || mapping.from.target$is.identity.mapping)
    {
        if (allow.map.to.subset.of.target)
        {
            rv = mapping.from.value$apply(value, fun=fun, na.rm=na.rm)
            rv = apply(rv, names(target.dim.names), fun, na.rm=na.rm)
            if (is.null(dim(rv)))
            {
                dim(rv) = sapply(target.dim.names, length)
                dimnames(rv) = target.dim.names
            }
        }
        else
            rv = mapping.from.value$apply(value, fun=fun, to.dim.names=target.dim.names, na.rm=na.rm)
    }
    else
    {
        mapped.value = mapping.from.value$apply(value, fun=fun, na.rm=na.rm)
        if (allow.map.to.subset.of.target)
        {
            rv = mapping.from.target$reverse.apply(mapped.value, na.rm=na.rm)
            rv = apply(rv, names(target.dim.names), fun, na.rm=na.rm)
            if (is.null(dim(rv)))
            {
                dim(rv) = sapply(target.dim.names, length)
                dimnames(rv) = target.dim.names
            }
        }
        else
            rv = mapping.from.target$reverse.apply(mapped.value, from.dim.names=target.dim.names, na.rm=na.rm)
    }
    
    #-- Restratify ages if needed --#
    if (need.to.restratify.age)
        rv = restratify.age.counts(counts = rv,
                                   desired.age.brackets = target.dim.names$age,
                                   smooth.infinite.age.to = smooth.infinite.age.to,
                                   allow.extrapolation = allow.age.extrapolation,
                                   na.rm = na.rm,
                                   error.prefix = error.prefix)
    
    
    #-- One last check, to make sure our mappings span all the requested dim.names --#
    dimensions.not.a.subset.mask = sapply(names(target.dim.names), function(d){
        length(setdiff(target.dim.names[[d]], dimnames(rv)[[d]]))>0
    })
    if (any(dimensions.not.a.subset.mask))
    {
        dimensions.not.a.subset = names(target.dim.names)[dimensions.not.a.subset.mask]
        missing.values = sapply(dimensions.not.a.subset, function(d){
            setdiff(target.dim.names[[d]], dimnames(rv)[[d]])
        })
        stop(paste0(error.prefix,
                    "After mapping the values, ",
                    ifelse(length(dimensions.not.a.subset)==1, "dimension ", "dimensions "),
                    collapse.with.and("'", dimensions.not.a.subset, "'"),
                    
                    ifelse(length(dimensions.not.a.subset)==1, " is", " are"),
                    " missing values: ",
                    ifelse(length(dimensions.not.a.subset==1), 
                           paste0("'", missing.values[[1]], "'", collapse=', '),
                           paste0(sapply(dimensions.not.a.subset, function(d){
                               paste0("- '", d, "' is missing: ", paste0("'", missing.values[[d]], "'", collapse=', '))
                           }), collapse='\n'))
        ))
    }
    
    #-- Put it in order and return --#
    get.identity.ontology.mapping()$apply(rv, to.dim.names = target.dim.names)
}

#'@title Get Detailed Information on a Recent Failed Attempt to Map Ontologies
#'
#'@param which The index of the recent failure. 1 denotes the most recent failed attempt, 2 the second most recent, and so on
#'@param print.ontologies Whether to print the full ontologies in the details
#'
#'@description Prints detailed information on why we were unable to find a mapping
#'@export
examine.recent.failed.ontology.mapping <- function(which=1,
                                                   print.ontologies = T)
{
    if (!is.numeric(which) || length(which)!=1 || is.na(which) || which <= 0)
        stop("Cannot examine recent failed ontology mapping - 'which' must be a single, positive, non-NA integer value")
    
    if (!is.logical(print.ontologies) || length(print.ontologies)!=1 || is.na(print.ontologies))
        stop("Cannot examine recent failed ontology mapping - 'print.ontologies' must be a single, non-NA logical value")
    
    if (which > N.FAILED.ONTOLOGY.MAPPINGS.TO.TRACK)
        stop(paste0("We only track up to ", N.FAILED.ONTOLOGY.MAPPINGS.TO.TRACK, " failed attempts to map ontologies, so we can't examine the ",
                    get.ordinal(which), " most recent failed attempt"))
    
    if (which > length(RECENT.FAILED.ONTOLOGY.MAPPINGS$calls))
    {
        if (length(RECENT.FAILED.ONTOLOGY.MAPPINGS$calls)==0)
            stop(paste0("We have not had any failed attempts to map ontologies, so we can't examine the ",
                        ifelse(which==1, "", paste0(get.ordinal(which), " ")),
                        "most recent failed attempt"))
        
        else
            stop(paste0("We have only had ", length(RECENT.FAILED.ONTOLOGY.MAPPINGS$calls),
                        " failed attempt",
                        ifelse(length(RECENT.FAILED.ONTOLOGY.MAPPINGS$calls)==1, '', 's'),
                        " to map ontologies, so we can't examine the ",
                        ifelse(which==1, "", paste0(get.ordinal(which), " ")),
                        "most recent failed attempt"))
    }
    
    failed.call = RECENT.FAILED.ONTOLOGY.MAPPINGS$calls[[which]]
    if (failed.call$call == 'get.ontology.mapping')
        examine.get.ontology.mapping(from.ontology = failed.call$from.ontology,
                                     to.ontology = failed.call$to.ontology,
                                     allow.non.overlapping.incomplete.dimensions = failed.call$allow.non.overlapping.incomplete.dimensions,
                                     print.ontologies = print.ontologies)
    else
        examine.get.mappings.to.align.ontologies(ontology.1 = failed.call$ontology.1,
                                                 ontology.2 = failed.call$ontology.2,
                                                 align.on.dimensions = failed.call$align.on.dimensions,
                                                 allow.non.overlapping.incomplete.dimensions = failed.call$allow.non.overlapping.incomplete.dimensions,
                                                 include.dim.names = failed.call$include.dim.names,
                                                 print.ontologies = print.ontologies)
    
}

#'@title Identify any Reasons for a Failure to Map Ontologies
#'
#'@description For use in figuring out why a call to \code{\link{get.ontology.mapping}} did not work
#'
#'@inheritParams get.ontology.mapping
#'@inheritParams examine.recent.failed.ontology.mapping
#'
#'@description If no mapping can be found, prints detailed information on why we were unable to find a mapping
#'
#'@value Invisibly returns a list containing details of the call to get.ontology.mapping, including the ontologies used
#'
#'@export
examine.get.ontology.mapping <- function(from.ontology,
                                         to.ontology,
                                         allow.non.overlapping.incomplete.dimensions = F,
                                         print.ontologies = T)
{
    details = do.get.ontology.mapping(from.ontology = from.ontology,
                                      to.ontology = to.ontology,
                                      allow.non.overlapping.incomplete.dimensions = allow.non.overlapping.incomplete.dimensions,
                                      for.examination = T)   
    
    do.examine.ontology.mapping(details = details,
                                call = list(
                                    call = 'get.ontology.mapping',
                                    from.ontology = from.ontology,
                                    to.ontology = to.ontology,
                                    allow.non.overlapping.incomplete.dimensions = allow.non.overlapping.incomplete.dimensions),
                                print.ontologies = print.ontologies)
}

#'@title Identify any Reasons for a Failure to Get Mappings to Align Ontologies
#'
#'@description For use in figuring out why a call to \code{\link{get.mappings.to.align.ontologies}} did not work
#'
#'@inheritParams get.mappings.to.align.ontologies
#'@inheritParams examine.recent.failed.ontology.mapping
#'
#'@description If no mappings can be found, prints detailed information on why we were unable to find mappings
#'
#'@value Invisibly returns a list containing details of the call to get.mappings.to.align.ontologies, including the ontologies used
#'
#'@export
examine.get.mappings.to.align.ontologies <- function(ontology.1,
                                                     ontology.2,
                                                     align.on.dimensions = intersect(names(ontology.1), names(ontology.2)),
                                                     allow.non.overlapping.incomplete.dimensions = F,
                                                     include.dim.names = NULL,
                                                     print.ontologies = T)
{
    details = do.get.mappings.to.align.ontologies(ontology.1 = ontology.1,
                                                  ontology.2 = ontology.2,
                                                  align.on.dimensions = align.on.dimensions,
                                                  allow.non.overlapping.incomplete.dimensions = allow.non.overlapping.incomplete.dimensions,
                                                  include.dim.names = include.dim.names,
                                                  for.examination = T)
    
    do.examine.ontology.mapping(details = details,
                                call = list(
                                    call = 'get.mappings.to.align.ontologies',
                                    ontology.1 = ontology.1,
                                    ontology.2 = ontology.2,
                                    align.on.dimensions = align.on.dimensions,
                                    allow.non.overlapping.incomplete.dimensions = allow.non.overlapping.incomplete.dimensions,
                                    include.dim.names = include.dim.names
                                ),
                                print.ontologies = print.ontologies)
}

do.examine.ontology.mapping <- function(details,
                                        call,
                                        print.ontologies)
{
    cat("************************************************\n")
    cat("************************************************\n")
    cat(paste0("EXAMINING call to ", call$call),"\n")
    cat(paste0("   (with allow.non.overlapping.incomplete.dimensions = ", call$allow.non.overlapping.incomplete.dimensions, ")"),"\n")
    
    cat("\n")
    cat("******************\n")
    
    if (call$call == 'get.ontology.mapping')
    {
        from.name = "FROM ONTOLOGY"
        to.name = "TO ONTOLOGY"
        from.ontology = call$from.ontology
        to.ontology = call$to.ontology
    }
    else
    {
        from.name = "ONTOLOGY 1"
        to.name = "ONTOLOGY 2"
        from.ontology = call$ontology.1
        to.ontology = call$ontology.2
    }
    
    if (print.ontologies || all(details$mapped))
    {
        cat(paste0(from.name, ": \n"))
        print(from.ontology)
        cat("******************\n")
        cat(paste0(to.name, ": \n"))
        print(to.ontology)
        cat("******************\n")
    }
    
    if (all(details$mapped))
        print("We we able to map the ontologies")
    else
    {
        if (any(details$mapped))
            cat(paste0("We WERE able to map for dimension(s) ", collapse.with.and("'", details$dset.in.ontologies.name[details$mapped], "'"), ", but..."), '\n')
        
        cat(paste0("We were NOT able to map for dimension(s) ",  collapse.with.and("'", details$dset.in.ontologies.name[!details$mapped], "'")), "\n")
        
        cat("\n")
        
        for (i in (1:length(details$dset))[!details$mapped])
        {
            cat(paste0("** For '", details$dset.in.ontologies.name[i], "' - ", details$msg[i]), "\n")
            
            dimensions.in.dset.and.from = intersect(details$dset[[i]], names(from.ontology))
            if (length(dimensions.in.dset.and.from)>0)
            {
                cat(paste0(from.name, ": \n"))
                print(from.ontology[dimensions.in.dset.and.from])
            }
            
            dimensions.in.dset.and.to = intersect(details$dset[[i]], names(to.ontology))
            if (length(dimensions.in.dset.and.to)>0)
            {
                cat(paste0(to.name, ": \n"))
                print(to.ontology[dimensions.in.dset.and.to])
            }
            
        }
    }
    
    cat("\n")
    cat("************************************************\n")
    cat("************************************************\n")
    
    invisible(call)
}

RECENT.FAILED.ONTOLOGY.MAPPINGS = new.env()
N.FAILED.ONTOLOGY.MAPPINGS.TO.TRACK = 5

##-------------##
##-------------##
##-- HELPERS --##
##-------------##
##-------------##


# To Note: 
#  A "complete" dimension means that the dimension's values comprise all possible values
#       This implies that the dimension can be summed over
#       A particular value (eg "other") may have different meanings in different complete dimensions, depending
#           on what else is in the dimension (eg, in 'black','hispanic','other' vs 'black','hispanic','white','other')
#       An ontology mapping to a complete dimension can only apply if the ontology mapping produces exactly the
#           set of values for that dimension (ie, extra values are not permitted)
#       An ontology mapping from a complete dimension can only apply if the ontology mapping takes exactly the
#           set of values for that dimension
#       Applying an ontology mapping yields complete dimensions if all the from dimensions are complete
#  An "incomplete" dimension means that the dimension's values may not be all possible values for the dimension
#       This implies that the dimension CANNOT be summed over
#       A particular value for an incomplete dimension is presumed to always have the same value
#       An ontology mapping to an incomplete dimension applies so long as the set of values it
#           produces are a superset of the values for the dimension (ie, extra values are permitted)
#       An ontology mapping from an incomplete dimension applies so long as the set of values it takes are a
#           subset of the values present
#       Applying an ontology mapping yields incomplete dimensions if any of the from dimensions are incomplete


DEBUG.ONTOLOGY.MAPPINGS = F


#' @title Find Ontology Mapping
#'@param from.ontology,to.ontology The ontologies of the from and to arrays
#'@param required.dimensions The dimensions that should be present in the aligned product
#'@param required.dim.names Any specific dimension values that should be present in the aligned product
#'@param get.two.way.alignment Logical indicating whether we need one mapping, that maps from.dim.names such that
#'@param allow.non.overlapping.incomplete.dimensions Logical indicating whether, when we map to an incomplete dimension, there can be no overlap in the from and to values for that dimension
#'
#'@return If get.two.way.alignment is false, returns a list of one or more ontology.mapping objects,
#'          such that applying these mappings to an array with from.dim.names ('from.arr')
#'          would yield an array with dimnames ('to.dim.names')
#'          
#'        If get.two.way.alignment is true, returns a list with two elements,
#'          each of which is itself a list of one or more ontology.mapping objects, such that
#'          applying the first set of mappings to an array with from.dimnames
#'          and applying the second set of mappings to an array with to.dimnames
#'          would both yield arrays with dimensions 'align.on.dimensions', both with
#'          the same dimnames
find.ontology.mapping <- function(from.ontology,
                                  to.ontology,
                                  required.dimensions,
                                  required.dim.names,
                                  get.two.way.alignment,
                                  allow.non.overlapping.incomplete.dimensions,
                                  from.ontology.name = ifelse(get.two.way.alignment, "ontology.1", "the from.ontology"),
                                  to.ontology.name = ifelse(get.two.way.alignment, "ontology.2", "the to.ontology"),
                                  get.details=F
)
{
    ##-- Partition all dimensions into sets linked by mappings --##
    all.dimensions = union(required.dimensions, 
                           union(names(from.ontology), names(to.ontology)))
    
    dimension.sets = list()
    for (d in all.dimensions)
    {
        already.in.set = F
        for (dset in dimension.sets)
        {
            if (any(dset==d))
                already.in.set = T
        }
        
        if (!already.in.set)
            dimension.sets = c(dimension.sets,
                               list(union(d, ONTOLOGY.MAPPING.MANAGER$dimensions.linked.by.mappings[[d]])))
    }
    #   names(dimension.sets) = sapply(dimension.sets, paste0, collapse='/')
    
    ##-- Set up the mappings for each dimension set --#
    
    viable.mappings.for.dimension.set = lapply(dimension.sets, function(dset){
        viable = lapply(dset, function(d){
            
            mappings = ONTOLOGY.MAPPING.MANAGER$sorted.mappings.by.from.dimension[[d]]
            if (is.null(mappings))
                mappings = list()
            
            if (d=='age')
                mappings = c(mappings, list('age'))
            
            if (d=='year')
                mappings = c(mappings, list('year'))
            
            mappings = c(list('drop'),
                         mappings,
                         list('other'))
            
        }) 
        
        names(viable) = dset
        viable
    })
    
    ##-- Order the dimension sets --##
    
    n.mappings.per.set = sapply(viable.mappings.for.dimension.set, function(viable){
        sum(sapply(viable, length))
    })
    
    o = order(n.mappings.per.set)
    
    ##-- Recursively find a mapping for each dimension set --##
    
    rv = list(from=list(), to=list(), 
              details=list(dset=list(),
                           dset.name=character(), 
                           dset.in.ontologies.name=character(),
                           msg=character(),
                           mapped=logical()))
    
    for (i in (1:length(dimension.sets))[o])
    {
        dset = dimension.sets[[i]]
        
        if (get.details)
        {
            dset.name = paste0(dset, collapse='/')
            dset.in.ontologies.name = paste0(intersect(dset, union(names(from.ontology), names(to.ontology))), collapse='/')
            rv$details$dset = c(rv$details$dset, list(dset))
            rv$details$dset.name = c(rv$details$dset.name, dset.name)
            rv$details$dset.in.ontologies.name = c(rv$details$dset.in.ontologies.name, dset.in.ontologies.name)
        }
        
        #-- Figure out which dimensions we will use --#
        dimensions.in.set.and.from = intersect(names(from.ontology), dset)
        if (length(dimensions.in.set.and.from)==0 && !get.two.way.alignment) # we can never find a mapping
        {
            missing.dimensions = intersect(dset, names(to.ontology))
            if (length(missing.dimensions)==1)
                msg = paste0("dimension '", missing.dimensions, "' is not in ", from.ontology.name)
            else
                msg = paste0("Neither ", collapse.with.or("'", missing.dimensions, "'"), "' is a dimension in ", from.ontology.name)
            
            if (DEBUG.ONTOLOGY.MAPPINGS)
                print(paste0("For ", paste0(dset, collapse='/'), " - ", msg))
            
            if (get.details)
            {
                rv$details$msg = c(rv$details$msg, msg)
                rv$details$mapped = c(rv$details$mapped, F)
            }
            else
            {
                return (NULL)
            }
        }
        else
        {
            from.ontology.for.set = from.ontology[dimensions.in.set.and.from]
            
            dimensions.in.set.and.to = intersect(names(to.ontology), dset)
            to.ontology.for.set = to.ontology[dimensions.in.set.and.to]
            
            required.dim.names.for.set = required.dim.names[intersect(dset, names(required.dim.names))]
            
            #-- Set up the mappings --#
            viable.mappings = viable.mappings.for.dimension.set[[i]]
            
            failed.mappings = lapply(ONTOLOGY.MAPPING.MANAGER$sorted.mappings.by.from.dimension, function(mps){
                list()
            })
            
            mappings.to.try = list()#new.env()
            mappings.to.try$viable.mappings = viable.mappings
            mappings.to.try$failed.mappings = failed.mappings
            
            
            #-- Find the mappings --#
            
            if (DEBUG.ONTOLOGY.MAPPINGS)
                print(paste0("Find mappings for ", paste0(dset, collapse="/")))
            
            required.dimensions.in.dset = intersect(required.dimensions, dset)
            
            mappings.for.dset = find.ontology.mapping.for.dimensions(from.ontology = from.ontology.for.set,
                                                                     to.ontology = to.ontology.for.set,
                                                                     required.dimensions = required.dimensions.in.dset,
                                                                     required.dim.names = required.dim.names.for.set,
                                                                     get.two.way.alignment = get.two.way.alignment,
                                                                     allow.non.overlapping.incomplete.dimensions = allow.non.overlapping.incomplete.dimensions,
                                                                     try.allowing.non.overlapping.incomplete.dimensions = F,
                                                                     is.for.two.way = get.two.way.alignment,
                                                                     used.mappings = list(),
                                                                     mappings.to.try = mappings.to.try,
                                                                     orig.mappings.to.try = mappings.to.try,
                                                                     search.depth = 0)
            
            # We are going to prioritize 'simpler' mappings - that just go one way
            if (get.two.way.alignment && !is.null(mappings.for.dset) &&
                (!all(sapply(mappings.for.dset[[1]], function(mapping){mapping$is.identity.mapping})) && 
                 !all(sapply(mappings.for.dset[[2]], function(mapping){mapping$is.identity.mapping}))) )
            {
                if (DEBUG.ONTOLOGY.MAPPINGS)
                    print(paste0("- Found a two-way mapping, going to see if we can find a simpler one-way mapping"))
            
                # First, try without two-way
                simpler.mapping = find.ontology.mapping.for.dimensions(from.ontology = from.ontology.for.set,
                                                                         to.ontology = to.ontology.for.set,
                                                                         required.dimensions = required.dimensions.in.dset,
                                                                         required.dim.names = required.dim.names.for.set,
                                                                         get.two.way.alignment = F,
                                                                         allow.non.overlapping.incomplete.dimensions = allow.non.overlapping.incomplete.dimensions,
                                                                         try.allowing.non.overlapping.incomplete.dimensions = F,
                                                                         is.for.two.way = F,
                                                                         used.mappings = list(),
                                                                         mappings.to.try = mappings.to.try,
                                                                         orig.mappings.to.try = mappings.to.try,
                                                                         search.depth = 0)
                
                if (is.null(simpler.mapping))
                {
                    simpler.mapping = find.ontology.mapping.for.dimensions(from.ontology = to.ontology.for.set,
                                                                             to.ontology = from.ontology.for.set,
                                                                             required.dimensions = required.dimensions.in.dset,
                                                                             required.dim.names = required.dim.names.for.set,
                                                                             get.two.way.alignment = F,
                                                                             allow.non.overlapping.incomplete.dimensions = allow.non.overlapping.incomplete.dimensions,
                                                                             try.allowing.non.overlapping.incomplete.dimensions = F,
                                                                             is.for.two.way = F,
                                                                             used.mappings = list(),
                                                                             mappings.to.try = mappings.to.try,
                                                                             orig.mappings.to.try = mappings.to.try,
                                                                             search.depth = 0)
                    
                    if (!is.null(simpler.mapping))
                    {
                        simpler.mapping$to = simpler.mapping$from
                        simpler.mapping$from = get.identity.ontology.mapping()
                    }
                }
                else
                    simpler.mapping$to = get.identity.ontology.mapping()
                
                if (!is.null(simpler.mapping))
                    mappings.for.dset = simpler.mapping
                
                if (DEBUG.ONTOLOGY.MAPPINGS)
                {
                    if (is.null(simpler.mapping))
                        print(paste0("- Did NOT find simpler one-way mapping"))
                    else
                        print(paste0("- DID find a simpler one-way mapping"))
                }
                
            }
            
            if (is.null(mappings.for.dset))
            {
                if (length(intersect(dset, names(from.ontology)))==0)
                {
                    to.dimensions.in.dset = intersect(dset, names(to.ontology))
                    incomplete.to.dimensions.in.dset = intersect(dset, incomplete.dimensions(to.ontology))
                    msg = paste0("Could not collapse ",
                                 ifelse(length(to.dimensions.in.dset)==1, "dimension ", "dimensions "),
                                 collapse.with.and("'", to.dimensions.in.dset, "'"),
                                 ifelse(length(to.dimensions.in.dset)==1, " which is", " which are"),
                                 " present in ", to.ontology.name, " but not ", from.ontology.name,
                                 ". (",
                                 collapse.with.and("'", incomplete.to.dimensions.in.dset, "'"),
                                 ifelse(length(incomplete.to.dimensions.in.dset)==1, " is an incomplete dimension)", " are incomplete dimensions)"))
                }
                else if (length(intersect(dset, names(to.ontology)))==0)
                {
                    from.dimensions.in.dset = intersect(dset, names(from.ontology))
                    incomplete.from.dimensions.in.dset = intersect(dset, incomplete.dimensions(from.ontology))
                    msg = paste0("Could not collapse ",
                                 ifelse(length(from.dimensions.in.dset)==1, "dimension ", "dimensions "),
                                 collapse.with.and("'", from.dimensions.in.dset, "'"),
                                 ifelse(length(from.dimensions.in.dset)==1, " which is", " which are"),
                                 " present in ", from.ontology.name, " but not ", to.ontology.name,
                                 ". (",
                                 collapse.with.and("'", incomplete.from.dimensions.in.dset, "'"),
                                 ifelse(length(incomplete.from.dimensions.in.dset)==1, " is an incomplete dimension)", " are incomplete dimensions)"))
                    
                }
                else
                    msg = paste0("Could not find any mappings between ", from.ontology.name, "'s ",
                                 paste0("'", intersect(dset, names(from.ontology)), "'", collapse="/"),
                                 " and ", to.ontology.name, "'s ",
                                 paste0("'", intersect(dset, names(to.ontology)), "'", collapse="/")
                    )
                
                if (DEBUG.ONTOLOGY.MAPPINGS)
                    print(paste0("For ", paste0(dset, collapse='/'), " - ", msg))
                
                #   if (DEBUG.ONTOLOGY.MAPPINGS)
                #      print(paste0("Could not find any mappings for ", paste0(dset, collapse="/"), " - giving up"))
                
                if (get.details)
                {
                    rv$details$msg = c(rv$details$msg, msg)
                    rv$details$mapped = c(rv$details$mapped, F)
                }
                else
                    return (NULL)
            }
            else
            {
                rv$from = c(rv$from, mappings.for.dset$from)
                rv$to = c(rv$to, mappings.for.dset$to)
                
                rv$details$msg = c(rv$details$msg, 'mapped')
                rv$details$mapped = c(rv$details$mapped, T)
            }
        }
    }
    
    rv
}


find.ontology.mapping.for.dimensions <- function(from.ontology,
                                                 to.ontology,
                                                 required.dimensions,
                                                 required.dim.names,
                                                 get.two.way.alignment,
                                                 allow.non.overlapping.incomplete.dimensions,
                                                 try.allowing.non.overlapping.incomplete.dimensions,
                                                 is.for.two.way,
                                                 used.mappings,
                                                 mappings.to.try,
                                                 orig.mappings.to.try,
                                                 search.depth # for debugging
)
{
    error.prefix = "Error getting ontology mapping: "
    
    # We are done (successfully) iff
    # 1) to.dim.names contains only required.dimensions and no others
    # 2) from.dim.names contains only required dimensions and no others
    # 3) All to.dimensions which are complete have equal values in from.dim.names and to.dim.names
    # 4) All to.dimensions which are incomplete have at least one value present in both from.dim.names and to.dim.names OR try.allowing.non.overlapping.incomplete.dimensions == TRUE
    # 5) All required.dimensions are present in to.dim.names, and all required.dim.names are present in to.dim.names (if this is not true and get.two.way.alignment==F, then we can give up now)
    
    # check (5) - if not met, and we are not going for two-way, we will never succeed
    required.dimensions.are.present.in.to = length(setdiff(required.dimensions, names(to.ontology)))==0
    required.dim.names.are.present.in.to = dim.names.are.subset(sub.dim.names=required.dim.names, super.dim.names=to.ontology)
    
    if (!get.two.way.alignment && !required.dim.names.are.present.in.to)
        return (NULL)
    
    #we can only modify to.ontology by reversing when get.two.way.alignment==T
    if (!get.two.way.alignment && !required.dimensions.are.present.in.to) 
        return (NULL)
    
    from.dimensions.are.complete = is_complete(from.ontology)
    to.dimensions.are.complete = is_complete(to.ontology)
    
    to.has.only.required = setequal(names(to.ontology), required.dimensions)
    from.has.only.required = setequal(names(from.ontology), required.dimensions)
    
    from.dimensions = names(from.ontology)
    to.dimensions = names(to.ontology)
    
    # below satisfies...
    from.out.of.alignment.mask = is.out.of.alignment(from.dimensions = from.dimensions,
                                                     to.dimensions = to.dimensions,
                                                     from.ontology = from.ontology,
                                                     to.ontology = to.ontology,
                                                     from.dimensions.are.complete = from.dimensions.are.complete,
                                                     to.dimensions.are.complete = to.dimensions.are.complete,
                                                     get.two.way.alignment = get.two.way.alignment,
                                                     is.for.two.way = is.for.two.way,
                                                     try.allowing.non.overlapping.incomplete.dimensions = try.allowing.non.overlapping.incomplete.dimensions)
    
    success = from.has.only.required && 
        to.has.only.required &&
        !any(from.out.of.alignment.mask) &&
        required.dimensions.are.present.in.to &&
        required.dim.names.are.present.in.to
    
    if (DEBUG.ONTOLOGY.MAPPINGS)
        debug.prefix = paste0(paste0(rep(" ", search.depth+1), collapse=''), "(", search.depth, ") ")
    
    if (success)
    {
        if (DEBUG.ONTOLOGY.MAPPINGS)
            print(paste0(debug.prefix, "SUCCESS!"))
        
        if (get.two.way.alignment)
            return (list(from=list(get.identity.ontology.mapping()),
                         to=list(get.identity.ontology.mapping())))
        else
            return (list(from=list(get.identity.ontology.mapping()), to=NULL))
    }
    
    # Let's work it out
    dimensions.out.of.alignment = from.dimensions[from.out.of.alignment.mask]
    
    for (d in union(dimensions.out.of.alignment, from.dimensions)) #put the out of alignment dimensions first
    {
        #for (try.index in 1:length(mappings.to.try$viable.mappings[[d]]))
        while (length(mappings.to.try$viable.mappings[[d]])>0)
        {
            try.modification = F
            mapping = orig.mapping = mappings.to.try$viable.mappings[[d]][[1]]
            mappings.to.try$viable.mappings[[d]] = mappings.to.try$viable.mappings[[d]][-1]
            
            if (is.character(mapping))
            {
                if (mapping=='drop')
                {        
                    try.modification = from.out.of.alignment.mask[d] && from.dimensions.are.complete[d] &&
                        # this next line speeds us up, but could conceivably be wrong if we need to drop a dimension, then add it back with a mapping
                        #!any(to.dimensions==d)
                        !any(required.dimensions==d)
                }
                else if (mapping=='other')
                {
                    if (other.catchall.mapping.applies(from.values=from.ontology[[d]],
                                                       to.values=to.ontology[[d]]))
                    {
                        mapping = create.other.catchall.ontology.mapping(dimension=d,
                                                                         from.values=from.ontology[[d]],
                                                                         to.values=to.ontology[[d]])
                        try.modification = T
                    }
                }
                else if (mapping=='age')
                {
                    if (any(dimensions.out.of.alignment == 'age') &&
                        !is.null(from.ontology[['age']]) &&
                        !is.null(to.ontology[['age']]))
                    {
                        mapping = create.age.ontology.mapping(from.values=from.ontology[['age']],
                                                              to.values=to.ontology[['age']],
                                                              allow.incomplete.span.of.infinite.age.range = T,
                                                              allow.partial.to.parsing = !to.dimensions.are.complete['age'],
                                                              require.map.all.to = to.dimensions.are.complete['age'])
                        # NEW: if we couldn't map all the way to to.ontology's age values directly,
                        # but we're in a two-way search, try aggregating just to the coarsest partition
                        # shared by from.ontology and to.ontology -- the two-way fallback below will then
                        # look for a way to bring to.ontology the rest of the way to that same partition
                        if (is.null(mapping) && (get.two.way.alignment || is.for.two.way))
                        {
                            common.age.values = get.common.age.refinement.values(from.ontology[['age']], to.ontology[['age']])
                            
                            if (!is.null(common.age.values))
                                mapping = create.age.ontology.mapping(from.values=from.ontology[['age']],
                                                                      to.values=common.age.values,
                                                                      allow.incomplete.span.of.infinite.age.range = T,
                                                                      allow.partial.to.parsing = F,
                                                                      require.map.all.to = T)
                        }
                        
                        try.modification = !is.null(mapping)
                    }
                }
                else if (mapping=='year')
                {
                    if (any(dimensions.out.of.alignment == 'year') &&
                        !is.null(from.ontology[['year']]) &&
                        !is.null(to.ontology[['year']]))
                    {
                        mapping = create.year.ontology.mapping(from.values=from.ontology[['year']],
                                                               to.values=to.ontology[['year']],
                                                               require.map.all.to = to.dimensions.are.complete['year'])
                        
                        try.modification = !is.null(mapping)
                    }
                }
            }
            else
            {
                is.reverse.of.used.mapping = as.logical(sapply(used.mappings, function(other.mapping){
                    mapping$is.reverse.of(other.mapping)
                }))
                
                try.modification = !any(is.reverse.of.used.mapping) &&
                    length(intersect(mapping$from.dimensions, dimensions.out.of.alignment)) > 0 && #== length(dimensions.out.of.alignment)
                    mapping$can.apply.to.ontology(from.ontology,
                                                  thorough.validation=F,
                                                  error.prefix=error.prefix)
                # not sure the first condition above is totally correct
                # ie, do we apply the mapping only if it only involves all dimensions out of alignment (what == length(dimensions.out.of.alignment)
                # or do we apply the mapping if it involves at least one dimension out of alignment (what >0 would give us)
            }
            
            if (try.modification)
            {
                #-- Store our viable/failed mappings before we update them in case we need to roll back --#
                pre.try.viable.mappings.for.d = mappings.to.try$viable.mappings[[d]]
                pre.try.failed.mappings.for.d = mappings.to.try$failed.mappings[[d]]
                
                is.drop = is.character(mapping)
                
                #-- Apply the mapping --#
                if (is.drop) # it is a drop the dimension
                {
                    if (DEBUG.ONTOLOGY.MAPPINGS)
                        print(paste0(debug.prefix, "Try dropping '", d, "'"))
                    
                    post.try.from.ontology = from.ontology[from.dimensions != d]
                    post.try.used.mappings = used.mappings
                }
                else
                {
                    # Update the ontology if we were to apply this mapping
                    post.try.from.ontology = mapping$apply.to.ontology(from.ontology,
                                                                       error.prefix = error.prefix,
                                                                       thorough.validation = F)
                    
                    # Add to the the used mappings we are tracking
                    post.try.used.mappings = c(used.mappings, list(mapping))
                }
                
                #-- Before we recurse using this mapping,
                #--  if it does not perfectly set up its to.dimensions,
                #--  try some other mappings first
                
                if (!is.character(orig.mapping)) #otherwise, we know the mapping worked or we should have dropped the dimension
                {
                    still.out.of.alignment.mask = is.out.of.alignment(from.dimensions = mapping$to.dimensions,
                                                                      to.dimensions = to.dimensions,
                                                                      from.ontology = post.try.from.ontology,
                                                                      to.ontology = to.ontology,
                                                                      from.dimensions.are.complete = is_complete(post.try.from.ontology),
                                                                      to.dimensions.are.complete = to.dimensions.are.complete,
                                                                      get.two.way.alignment = get.two.way.alignment,
                                                                      is.for.two.way = is.for.two.way,
                                                                      try.allowing.non.overlapping.incomplete.dimensions = try.allowing.non.overlapping.incomplete.dimensions)
                    
                    if (all(still.out.of.alignment.mask))
                    {
                        other.mappings = find.ontology.mapping.for.dimensions(from.ontology = from.ontology,
                                                                              to.ontology = to.ontology,
                                                                              required.dimensions = required.dimensions,
                                                                              required.dim.names = required.dim.names,
                                                                              get.two.way.alignment = get.two.way.alignment,
                                                                              is.for.two.way = is.for.two.way,
                                                                              allow.non.overlapping.incomplete.dimensions = allow.non.overlapping.incomplete.dimensions,
                                                                              try.allowing.non.overlapping.incomplete.dimensions = try.allowing.non.overlapping.incomplete.dimensions,
                                                                              used.mappings = used.mappings,
                                                                              mappings.to.try = mappings.to.try,
                                                                              orig.mappings.to.try = orig.mappings.to.try,
                                                                              search.depth = search.depth)
                        
                        if (!is.null(other.mappings)) # we succeeded (without using this mapping) and we're done! Return what we got and go home
                        {
                            return(other.mappings)
                        }
                    }
                }
                
                #-- Go ahead and recurse using this mapping --#
                
                # Mark previously failed mappings for the modified dimensions as now viable
                if (!is.drop)
                {
                    if (DEBUG.ONTOLOGY.MAPPINGS)
                        print(paste0(debug.prefix, "Try mapping '", mapping$name, "'"))
                    
                    if (length(mapping$to.dimensions)==1 && length(mapping$from.dimensions)==1 &&
                        mapping$to.dimensions == mapping$from.dimensions)
                    {
                        # we don't need to re-try dropping 'd'
                        drop.mask = sapply(mappings.to.try$failed.mappings[[d]], function(mp){
                            is.character(mp) && mp=='drop'
                        })
                        
                        mappings.to.try$viable.mappings[[d]] = c(mappings.to.try$failed.mappings[[d]][!drop.mask],
                                                                 mappings.to.try$viable.mappings[[d]])
                        mappings.to.try$failed.mappings[[d]] = mappings.to.try$failed.mappings[[d]][drop.mask]
                    }
                    else
                    {
                        mappings.to.try$viable.mappings[[d]] = c(mappings.to.try$failed.mappings[[d]],
                                                                 mappings.to.try$viable.mappings[[d]])
                        mappings.to.try$failed.mappings[[d]] = list()
                    }
                }
                
                additional.mappings = find.ontology.mapping.for.dimensions(from.ontology = post.try.from.ontology,
                                                                           to.ontology = to.ontology,
                                                                           required.dimensions = required.dimensions,
                                                                           required.dim.names = required.dim.names,
                                                                           get.two.way.alignment = get.two.way.alignment,
                                                                           is.for.two.way = is.for.two.way,
                                                                           allow.non.overlapping.incomplete.dimensions = allow.non.overlapping.incomplete.dimensions,
                                                                           try.allowing.non.overlapping.incomplete.dimensions = try.allowing.non.overlapping.incomplete.dimensions,
                                                                           used.mappings = post.try.used.mappings,
                                                                           mappings.to.try = mappings.to.try,
                                                                           orig.mappings.to.try = orig.mappings.to.try,
                                                                           search.depth = search.depth + 1)
                
                if (!is.null(additional.mappings)) # we succeeded and we're done! Append and go home
                {
                    if (is.character(mapping))
                        to.add = list()
                    else
                        to.add = list(mapping)
                    
                    return(list(from=c(to.add, additional.mappings$from),
                                to=additional.mappings$to))
                }
                
                
                #-- If we get here, we tried and failed --#
                
                # Roll back the viable/failed mappings
                mappings.to.try$viable.mappings[[d]] = pre.try.viable.mappings.for.d
                mappings.to.try$failed.mappings[[d]] = pre.try.failed.mappings.for.d
            }
            
            #-- If we get here, either we didn't try or we tried and failed --#
            
            # Add to failed mappings
            mappings.to.try$failed.mappings[[d]] = c(mappings.to.try$failed.mappings[[d]], list(orig.mapping))
        }
        
        #-- If we get here, we didn't find any mappings that could align dimension d --#
        #   if it is still out of alignment,
        #   unless we can move on to a two-way alignment or allow incomplete dimensions to be non-overlapping, we never will get it aligned
        if (any(dimensions.out.of.alignment==d) && 
            !get.two.way.alignment &&
            (from.dimensions.are.complete[d] || try.allowing.non.overlapping.incomplete.dimensions || !allow.non.overlapping.incomplete.dimensions))
        {
            if (DEBUG.ONTOLOGY.MAPPINGS)
                print(paste0(debug.prefix, "Cannot find any mappings that align '", d, "' - giving up here"))
            return (NULL)
        }
    }
    
    # We got all the way to the the end and could not apply any mappings
    # If we need to go two ways, then try to map the other way
    # If that doesn't work, and we can, try.allowing.non.overlapping.incomplete.dimensions
    # Otherwise, give up and return NULL
    
    rv = NULL
    if (get.two.way.alignment)
    {
        if (DEBUG.ONTOLOGY.MAPPINGS)
            print(paste0(debug.prefix, "Try finding the reverse mapping for two-way"))
        
        reverse.mappings = find.ontology.mapping.for.dimensions(from.ontology=to.ontology,
                                                                to.ontology=from.ontology,
                                                                required.dimensions=required.dimensions,
                                                                # required.dim.names=required.dim.names,
                                                                required.dim.names=required.dim.names[from.out.of.alignment.mask],
                                                                allow.non.overlapping.incomplete.dimensions=allow.non.overlapping.incomplete.dimensions,
                                                                try.allowing.non.overlapping.incomplete.dimensions = try.allowing.non.overlapping.incomplete.dimensions,
                                                                get.two.way.alignment=F,
                                                                is.for.two.way = T, 
                                                                used.mappings = list(),
                                                                mappings.to.try = orig.mappings.to.try,
                                                                orig.mappings.to.try = orig.mappings.to.try,
                                                                search.depth = search.depth + 1)
        
        if (!is.null(reverse.mappings)) # this works! package it up and return
            return (list(from=list(get.identity.ontology.mapping()),
                         to=reverse.mappings$from))
    }
    
    if (allow.non.overlapping.incomplete.dimensions && !try.allowing.non.overlapping.incomplete.dimensions &&
        any(!from.dimensions.are.complete[dimensions.out.of.alignment]))
    {
        if (DEBUG.ONTOLOGY.MAPPINGS)
            print(paste0(debug.prefix, "Try allowing non-overlapping incomplete dimensions"))
        
        for (d in names(mappings.to.try$viable.mappings))
        {
            drop.mask = sapply(mappings.to.try$failed.mappings[[d]], function(mps){
                is.character(mps) && mps=='drop'
            })
            if (any(drop.mask))
            {
                mappings.to.try$failed.mappings[[d]] = mappings.to.try$failed.mappings[[d]][!drop.mask]
                mappings.to.try$viable.mappings[[d]] = c(list('drop'), mappings.to.try$viable.mappings[[d]])
            }
        }
        
        # Try with allow non overlapping incomplete mappings now
        find.ontology.mapping.for.dimensions(from.ontology = from.ontology,
                                             to.ontology = to.ontology,
                                             required.dimensions = required.dimensions,
                                             required.dim.names = required.dim.names,
                                             get.two.way.alignment = get.two.way.alignment,
                                             is.for.two.way = is.for.two.way,
                                             allow.non.overlapping.incomplete.dimensions = allow.non.overlapping.incomplete.dimensions,
                                             try.allowing.non.overlapping.incomplete.dimensions = T,
                                             used.mappings = used.mappings,
                                             mappings.to.try = mappings.to.try,
                                             orig.mappings.to.try = orig.mappings.to.try,
                                             search.depth = search.depth + 1)
    }
    #   else if (search.depth==0)
    #       browser()
    else
        NULL
}

# A helper for the do.get.ontology
# to tell if dimensions are now aligned correctly
is.out.of.alignment <- function(from.dimensions,
                                to.dimensions,
                                from.ontology,
                                to.ontology,
                                from.dimensions.are.complete,
                                to.dimensions.are.complete,
                                get.two.way.alignment,
                                is.for.two.way,
                                try.allowing.non.overlapping.incomplete.dimensions)
{
    from.out.of.alignment.mask = !sapply(from.dimensions, function(d){
        
        if (all(to.dimensions!=d))
            F
        else if (!is.null(to.ontology[d]) && to.dimensions.are.complete[d])
        {
            if (is.null(from.ontology[[d]])) # satisfies part of condition (1) 
                T
            else if (from.dimensions.are.complete[d])
            {
                setequal(from.ontology[[d]], to.ontology[[d]]) # satisfies condition (3)
            }
            else # to[[d]] is complete, from[[d]] is incomplete
            {
                if ((get.two.way.alignment || is.for.two.way))
                {
                    if (try.allowing.non.overlapping.incomplete.dimensions)
                        T
                    else
                        length(intersect(from.ontology[[d]], to.ontology[[d]]))>0
                }
                else
                {
                    length(setdiff(to.ontology[[d]], from.ontology[[d]]))==0 # satisfies condition (3)
                }
            }
        }
        else if (!is.null(to.ontology[d]) && !to.dimensions.are.complete[d] && try.allowing.non.overlapping.incomplete.dimensions)
            !is.null(from.ontology[[d]])
        else if (!is.null(from.ontology[[d]]) && from.dimensions.are.complete[d])
        {
            # we know by now that it is NOT that to[d] is non-NULL and complete
            is.null(to.ontology[[d]]) || # if it's NULL, we'll take anything
                length(intersect(from.ontology[[d]], to.ontology[[d]]))>0
        }
        else
        {
            is.null(to.ontology[[d]]) || # if it's NULL, we'll take anything
                (!is.null(from.ontology[[d]]) && 
                     length(intersect(from.ontology[[d]], to.ontology[[d]]))>0)
        }
    })
    names(from.out.of.alignment.mask) = from.dimensions
    
    from.out.of.alignment.mask
}

#'@title Combine multiple ontology mappings into a single mapping
#'
#'@param ... Either ontology.mapping objects or lists containing ontology.mapping objects#'
#'
#'@export
combine.ontology.mappings <- function(..., thorough.validation=T)
{
    args = list(...)
    args = args[!sapply(args, is.null)]
    if (length(args)==0)
        return (NULL)
    
    sub.mappings = list()
    for (elem in args)
    {
        if (is(elem, 'combination.ontology.mapping'))
            sub.mappings = c(sub.mappings, elem$sub.mappings)
        else if (is(elem, 'ontology.mapping'))
            sub.mappings = c(sub.mappings, list(elem))
        else if (is.list(elem))
        {
            for (sub.elem in elem)
            {
                if (is(sub.elem, 'combination.ontology.mapping'))
                    sub.mappings = c(sub.mappings, sub.elem$sub.mappings)
                else if (is(sub.elem, 'ontology.mapping'))
                    sub.mappings = c(sub.mappings, list(sub.elem))
                else
                    stop("Cannot create combination ontology mapping: ... must contain ONLY ontology.mapping objects or lists of ontology.mapping objects")
            }
        }
        else
            stop("Cannot create combination ontology mapping: ... must contain ONLY ontology.mapping objects or lists of ontology.mapping objects")
    }
    
    if (length(sub.mappings)==0)
        return (NULL)
    
    identity.mapping.mask = sapply(sub.mappings, function(m){m$is.identity.mapping})
    sub.mappings = sub.mappings[!identity.mapping.mask]
    if (length(sub.mappings)==0)
        get.identity.ontology.mapping()
    else if (length(sub.mappings)==1)
        sub.mappings[[1]]
    else # we need to combine them
    {
        new.name = paste0(length(sub.mappings), "-mapping combo")
        # First, see if we can combine into a single basic mapping
        #  We can do this IFF both
        #  (1) all the mappings are basic mappings
        #  (2) none of the to.dimensions of any mapping are from.dimensions of any subsequent mappings
        #  
        # If we can't combine into a basic mapping, 
        #  If we're down to two mappings, then we have to make a combination mapping
        #  If we have more than two, recurse to see what we get by
        #  (1) combining all but the last mapping and
        #  (2) combining all but the first mapping
        #  
        # We also need to confirm that we CAN combine these mappings
        #  ie, if the from.dimensions for any sub.mapping match the to.dimensions of a prior sub.mapping, then the values of 
        
        can.directly.combine = all(sapply(sub.mappings, is, class2='basic.ontology.mapping')) &&
            all(sapply(2:length(sub.mappings), function(i){
                m1 = sub.mappings[i]
                all(sapply(sub.mappings[1:(i-1)], function(m2){
                    length(intersect(m1$from.dimensions, m2$from.dimensions)==0) &&
                        length(intersect(m1$to.dimensions, m2$to.dimensions)==0) #cannot directly join if they involve the same from or to dimensions
                }))
            }))
        
        mapped.dim.names = sub.mappings[[1]]$to.dim.names
        for (sub.mapping in sub.mappings[-1])
        {
            # Check that dim.names line up
            if (length(intersect(names(mapped.dim.names), sub.mapping$to.dimensions)))
            {
                can.directly.combine = F
                dim.names.for.check = mapped.dim.names
                
                missing.dimensions.for.check = setdiff(sub.mapping$from.dimensions, names(mapped.dim.names))
                dim.names.for.check[missing.dimensions.for.check] = sub.mapping$from.dim.names[missing.dimensions.for.check]
                
                if (thorough.validation)
                    sub.mapping$can.apply.to.dim.names(dim.names.for.check,
                                                       thorough.validation = F,
                                                       throw.errors = T,
                                                       error.prefix = "Cannot combine ontology mappings: ")
            }
            mapped.dim.names[sub.mapping$to.dimensions] = sub.mapping$to.dim.names
        }
        
        if (can.directly.combine)
        {
            combining.indices = get.every.combination(lapply(sub.mappings, function(sub.mapping){
                1:sub.mapping$n.from.and.to.values
            }))
            
            from.values = as.character(unlist(sapply(1:length(sub.mappings), function(i){
                sub.mappings[[i]]$from.values[combining.indices[,i],]
            })))
            to.values = as.character(unlist(sapply(1:length(sub.mappings), function(i){
                sub.mappings[[i]]$to.values[combining.indices[,i],]
            })))
            
            dim(from.values) = c(dim(combining.indices)[1], length(from.values)/dim(combining.indices)[1])
            dim(to.values) = c(dim(combining.indices)[1], length(to.values)/dim(combining.indices)[1])
            
            from.dimension.chunks = lapply(sub.mappings, function(sub.mapping){
                sub.mapping$from.dimensions
            })
            
            to.dimension.chunks = sapply(sub.mappings, function(sub.mapping){
                sub.mapping$to.dimensions
            })
            
            BASIC.ONTOLOGY.MAPPING$new(name = new.name,
                                       from.dimensions = unlist(from.dimension.chunks),
                                       to.dimensions = unlist(to.dimension.chunks),
                                       from.values = from.values,
                                       to.values = to.values,
                                       from.dimension.chunks = from.dimension.chunks,
                                       to.dimension.chunks = to.dimension.chunks,
                                       component.names = unlist(sapply(sub.mappings, function(map){map$component.names})))
        }
        else
        {
            if (length(sub.mappings)>2)
                sub.mappings = c(sub.mappings[1], 
                                 combine.ontology.mappings(sub.mappings[-1],
                                                           thorough.validation = thorough.validation))
            if (length(sub.mappings)>2)
                sub.mappings = c(combine.ontology.mappings(sub.mappings[-length(sub.mappings)],
                                                           thorough.validation = thorough.validation),
                                 sub.mappings[length(sub.mappings)])
            
            COMBINATION.ONTOLOGY.MAPPING$new(name = new.name,
                                             sub.mappings=sub.mappings)
        }
    }
}

#'@title Create an ontology mapping that lumps otherwise unspecified categories into an "other" category
#'
#'@param dimension A single character value indicating what dimension this mapping applies to
#'@param from.values The possible values for the dimension in the ontology to map from
#'@param to.values The possible values for the dimension in the ontology to map to
#'@param other.category.value What the "other" category should be called in the to ontology
#'
#'@export
create.other.catchall.ontology.mapping <- function(dimension,
                                                   from.values,
                                                   to.values,
                                                   other.category.value='other')
{
    #-- Sanitize arguments --#
    if (!is.character(dimension))
        stop("In create.other.catchall.ontology.mapping(), 'dimension' must be a single character value")
    if (length(dimension) != 1)
        stop("In create.other.catchall.ontology.mapping(), 'dimension' must be a SINGLE character value")
    if (is.na(dimension))
        stop("In create.other.catchall.ontology.mapping(), 'dimension' cannot be NA")
    
    if (!is.character(from.values))
        stop("In create.other.catchall.ontology.mapping(), 'from.values' must be a character vector")
    if (length(from.values)==0)
        stop("In create.other.catchall.ontology.mapping(), 'from.values' cannot be an empty vector")
    if (any(is.na(from.values)))
        stop("In create.other.catchall.ontology.mapping(), 'from.values' cannot contain NA values")
    
    if (!is.character(to.values))
        stop("In create.other.catchall.ontology.mapping(), 'to.values' must be a character vector")
    if (length(to.values)==0)
        stop("In create.other.catchall.ontology.mapping(), 'to.values' cannot be an empty vector")
    if (any(is.na(to.values)))
        stop("In create.other.catchall.ontology.mapping(), 'to.values' cannot contain NA values")
    
    if (!is.character(other.category.value))
        stop("In create.other.catchall.ontology.mapping(), 'other.category.value' must be a single character value")
    if (length(other.category.value) != 1)
        stop("In create.other.catchall.ontology.mapping(), 'other.category.value' must be a SINGLE character value")
    if (is.na(other.category.value))
        stop("In create.other.catchall.ontology.mapping(), 'other.category.value' cannot be NA")
    
    missing.from.from = setdiff(to.values, c(from.values, other.category.value))
    if (length(missing.from.from)>0)
        stop(paste0("In create.other.catchall.ontology.mapping(), no from.values will map to ",
                    collapse.with.or("'", missing.from.from, "'"), " (in to.values)"))
    
    
    #-- Do the work --#
    missing.from.to = setdiff(from.values, c(to.values, other.category.value))
    if (length(missing.from.to)==0)
        stop("You have tried to construct an 'other-catchall' mapping using create.other.catchall.ontology.mapping(), but none of the from.values will map to other")
    
    new.to.values = from.values
    names(new.to.values) = from.values
    new.to.values[missing.from.to] = other.category.value
    dim(new.to.values) = dim(from.values) = c(length(from.values),1)
    
    BASIC.ONTOLOGY.MAPPING$new(name=paste0(dimension, "/", other.category.value),
                               from.dimensions=dimension,
                               to.dimensions=dimension,
                               from.values=from.values,
                               to.values=new.to.values)
}

other.catchall.mapping.applies <- function(from.values, to.values)
{
    !is.null(from.values) && !is.null(to.values) &&
        any(to.values=='other') &&
        length(setdiff(to.values, c(from.values, 'other'))) == 0 &&
        length(setdiff(from.values, to.values)) > 0
}

# If x is an ontology, just returns x
# If x is a named list of character vectors, converts to an ontology
# else throws an error
derive.ontology <- function(x,
                            var.name.for.error,
                            error.prefix)
{
    if (is.ontology(x))
        x
    else if (is.list(x))
    {
        if (is.null(names(x)))
            stop(paste(error.prefix, "If ", var.name.for.error, " is a list, it must be NAMED"))
        if (any(!sapply(x, is.character)))
            stop(paste(error.prefix, "If ", var.name.for.error, " is a list, it must contain only character vectors"))
        
        as.ontology(x)
    }
    else
        stop(paste(error.prefix, var.name.for.error, " must be either (1) an object of class 'ontology' as created by the ontology() function or (2) a named list of character vectors"))
}


##-------------------------##
##-------------------------##
##-- YEAR RANGE MAPPINGS --##
##-------------------------##
##-------------------------##

create.year.ontology.mapping <- function(from.values,
                                         to.values,
                                         require.map.all.to)
{
    from.bounds = parse.year.names(from.values)
    to.bounds = parse.year.names(to.values)
    
    if (is.null(from.bounds) || is.null(to.bounds))
        return (NULL)
    
    do.create.age.or.year.ontology.mapping(from.values = from.values,
                                           to.values = to.values,
                                           from.bounds = from.bounds,
                                           to.bounds = to.bounds,
                                           dimension = 'year',
                                           require.map.all.to = require.map.all.to,
                                           allow.incomplete.span.of.infinite.range = F)
}

do.create.age.or.year.ontology.mapping <- function(from.values,
                                                   to.values,
                                                   from.bounds,
                                                   to.bounds,
                                                   dimension,
                                                   require.map.all.to,
                                                   allow.incomplete.span.of.infinite.range,
                                                   allow.incomplete.span.of.to = F,
                                                   return.mapping = T)
{
    if (is.null(from.bounds) || is.null(to.bounds))
        return (NULL)
    
    # Get the 'from's for each 'to'
    # NB: A 'from' may map to more than one 'to' if the 'to's are overlapping
    
    froms.for.to = lapply(1:length(to.values), function(to){
        mask = from.bounds$lower >= to.bounds$lower[to] &
            from.bounds$upper <= to.bounds$upper[to]
        
        if (sum(mask)==0)
            NULL
        else
            (1:length(mask))[mask]
    })
    
    # If a 'to' has no 'froms' - give up
    
    no.froms.map.to.to = sapply(froms.for.to, is.null)
    if (require.map.all.to)
    {
        if (any(no.froms.map.to.to))
            return (NULL)
    }
    else
    {
        if (all(no.froms.map.to.to))
            return (NULL)
    }
    
    # Make sure the 'from's for each 'to' span the 'to' completely
    froms.span.tos = sapply(1:length(to.values), function(to){
        
        if (no.froms.map.to.to[to])
            return (F)
        
        from.lowers = from.bounds$lower[froms.for.to[[to]]]
        from.uppers = from.bounds$upper[froms.for.to[[to]]]
        o = order(from.lowers)
        from.lowers = from.lowers[o]
        from.uppers = from.uppers[o]
        
        # from's span to the bottom
        from.lowers[1] == to.bounds$lower[to] && 
            # from's span to the top or the top is infinity
            ( from.uppers[length(from.uppers)] == to.bounds$upper[to] ||
                  (allow.incomplete.span.of.infinite.range
                   && is.infinite(to.bounds$upper[to])) ) && 
            # from's are contiguous
            all( from.lowers[-1] == from.uppers[-length(from.uppers)] ) 
    })
    
    if (all(froms.span.tos) ||
        (!require.map.all.to && any(froms.span.tos)) )
    {
        if (allow.incomplete.span.of.to)
            mask = rep(T, length(froms.for.to))
        else
            mask = froms.span.tos
        
        iterated.from.values = from.values[unlist(froms.for.to[mask])]
        iterated.to.values = unlist(sapply((1:length(to.values))[mask], function(i){
            rep(to.values[i], length(froms.for.to[[i]]))
        }))
        
        missing.from.values = setdiff(from.values, iterated.from.values)
        iterated.from.values = c(iterated.from.values, missing.from.values)
        iterated.to.values = c(iterated.to.values, rep(NA, length(missing.from.values)))
        
        if (return.mapping)
            BASIC.ONTOLOGY.MAPPING$new(name = paste0(dimension, ' ', length(from.values), "->", length(to.values), " strata"),
                                       from.dimensions = dimension,
                                       to.dimensions = dimension,
                                       from.values = matrix(iterated.from.values, ncol=1),
                                       to.values = matrix(iterated.to.values, ncol=1))
        else
        {
            names(iterated.to.values) = iterated.from.values
            iterated.to.values
        }
    }
    else
        return (NULL)
}

##------------------##
##------------------##
##-- AGE MAPPINGS --##
##------------------##
##------------------##

# Fundamentally we can do this iff
# For every 'to' value, the 'from' values within each to value completely span that to value's range
#     (unless the upper bound is infinity and allow.incomplete.span.of.infinite.age.range==T)
create.age.ontology.mapping <- function(from.values,
                                        to.values,
                                        allow.incomplete.span.of.infinite.age.range,
                                        allow.partial.to.parsing = T,
                                        require.map.all.to = !allow.partial.to.parsing)
{
    from.bounds = parse.age.strata.names(from.values, allow.partial.parsing = allow.partial.to.parsing)
    to.bounds = parse.age.strata.names(to.values, allow.partial.parsing = F)
    
    do.create.age.or.year.ontology.mapping(from.values = from.values,
                                           to.values = to.values,
                                           from.bounds = from.bounds,
                                           to.bounds = to.bounds,
                                           dimension = 'age',
                                           require.map.all.to = require.map.all.to,
                                           allow.incomplete.span.of.infinite.range = allow.incomplete.span.of.infinite.age.range)
}

create.overlapping.age.ontology.mapping <- function(from.values,
                                                    to.values)
{
    from.bounds = parse.age.strata.names(from.values)
    to.bounds = parse.age.strata.names(to.values)
    
    if (is.null(from.bounds) || is.null(to.bounds))
        return (NULL)
    
    # Get the 'from's for each 'to'
    # NB: A 'from' may map to more than one 'to' if the 'to's are overlapping
    
    froms.for.to = lapply(1:length(to.values), function(to){
        mask = from.bounds$upper > to.bounds$lower[to] &
            from.bounds$lower < to.bounds$upper[to]
        
        if (sum(mask)==0)
            as.integer(NA)
        else
            (1:length(mask))[mask]
    })
    
    iterated.from.values = from.values[from.bounds$mapped.mask][unlist(froms.for.to)]
    iterated.to.values = unlist(sapply(1:length(to.values), function(i){
        rep(to.values[to.bounds$mapped.mask][i], length(froms.for.to[[i]]))
    }))
    
    missing.from.values = setdiff(from.values, iterated.from.values)
    iterated.from.values = c(iterated.from.values, missing.from.values)
    iterated.to.values = c(iterated.to.values, rep(NA, length(missing.from.values)))
    
    BASIC.ONTOLOGY.MAPPING$new(name = paste0('age ', length(from.values), "->", length(to.values), " strata by overlap"),
                               from.dimensions = 'age',
                               to.dimensions = 'age',
                               from.values = matrix(iterated.from.values, ncol=1),
                               to.values = matrix(iterated.to.values, ncol=1))
}


##----------------------------------##
##----------------------------------##
##-- The ONTOLOGY MAPPING MANAGER --##
##----------------------------------##
##----------------------------------##


ONTOLOGY.MAPPING.MANAGER = new.env()

#'@title Clear All Registered Ontology Mappings
#'
#'@details Clears all registered ontology mappings, such that new mappings can be added
#'
#'@export
clear.ontology.mappings <- function()
{
    ONTOLOGY.MAPPING.MANAGER$mappings=list()
    ONTOLOGY.MAPPING.MANAGER$sorted.mappings.by.from.dimension=list()
    ONTOLOGY.MAPPING.MANAGER$cached.one.way.mappings=list()
    ONTOLOGY.MAPPING.MANAGER$cached.two.way.mappings=list()
    ONTOLOGY.MAPPING.MANAGER$dimensions.directly.linked.by.mappings=list()
    ONTOLOGY.MAPPING.MANAGER$dimensions.linked.by.mappings=list()
    
    RECENT.FAILED.ONTOLOGY.MAPPINGS$calls = list()
}

clear.ontology.mappings()


##-----------------------##
##-----------------------##
##-- CLASS DEFINITIONS --##
##-----------------------##
##-----------------------##


ONTOLOGY.MAPPING = R6::R6Class(
    'ontology.mapping',
    
    public = list(
        
        #'@title The Constructor
        initialize = function(name, component.names=name)
        {
            private$i.name = name
            private$i.component.names = component.names
        },
        
        #'@title The Print Function
        print = function(...)
        {
            from.dimensions = self$from.dimensions
            to.dimensions = self$to.dimensions
            
            if (setequal(from.dimensions, to.dimensions))
            {
                if (length(private$i.component.names)==1)
                    print(paste0("A basic ontology mapping ('",
                                 self$name, "') over dimension",
                                 ifelse(length(from.dimensions)==1, ' ', 's '),
                                 collapse.with.and("'", from.dimensions, "'")))
                else            if (setequal(from.dimensions, to.dimensions))
                    print(paste0("A combination of ", length(private$i.component.names),
                                 " ontology mappings (",
                                 collapse.with.and("'", private$i.component.names, "'"),
                                 ")",
                                 " over dimension",
                                 ifelse(length(from.dimensions)==1, ' ', 's '),
                                 collapse.with.and("'", from.dimensions, "'")))
            }
            
            else
            {
                if (length(private$i.component.names)==1)
                    print(paste0("A basic ontology mapping ('",
                                 self$name, "') over dimension",
                                 ifelse(length(from.dimensions)==1, '', 's'),
                                 " <", collapse.with.and("'", from.dimensions, "'"), ">",
                                 " to <",
                                 collapse.with.and("'", to.dimensions, "'"), ">"))
                else
                    print(paste0("A combination of ", length(private$i.component.names),
                                 " ontology mappings (",
                                 collapse.with.and("'", private$i.component.names, "'"),
                                 ")",
                                 " from dimension",
                                 ifelse(length(from.dimensions)==1, '', 's'),
                                 " <", collapse.with.and("'", from.dimensions, "'"), ">",
                                 " to <",
                                 collapse.with.and("'", to.dimensions, "'"), ">"))
            }
        },
        
        #'@title A test for equality - to be overwritten in subclass --#
        equals = function(other)
        {
            stop("This subclass of 'ontology.mapping' is incompletely specified. The 'equals' method must be implemented at the subclass level")            
        },
        
        #'@title Test if this ontology.mapping can apply to the given dim.names
        can.apply.to.dim.names = function(from.dim.names,
                                          to.dim.names = NULL,
                                          thorough.validation = T,
                                          throw.errors=F,
                                          error.prefix='')
        {
            private$check.can.apply(from.dim.names=from.dim.names,
                                    to.dim.names=to.dim.names,
                                    thorough.validation=thorough.validation,
                                    throw.errors=throw.errors,
                                    error.prefix=error.prefix)
        },
        
        #'@title Test if this ontology.mapping can apply to the given ontology
        can.apply.to.ontology = function(from.ontology,
                                         to.ontology = NULL,
                                         thorough.validation = T,
                                         throw.errors=F,
                                         error.prefix='')
        {
            if (!is.ontology(from.ontology))
                stop("Error in ontology mapping's can.apply.to.ontology(): 'from.ontology' must be an object of class ontology")
            if (!is.null(to.ontology) && !is.ontology(to.ontology))
                stop("Error in ontology mapping's can.apply.to.ontology(): If it is not NULL, 'to.ontology' must be an object of class ontology")
            
            private$check.can.apply(from.dim.names=from.ontology,
                                    to.dim.names=to.ontology,
                                    thorough.validation=thorough.validation,
                                    throw.errors=throw.errors,
                                    error.prefix=error.prefix)
        },
        
        #'@title Get the dimnames that would be generated after applying this mapping to some data with the given dim.names
        apply.to.dim.names = function(from.dim.names, 
                                      thorough.validation = T,
                                      error.prefix='Cannot apply ontology.mapping to dim names: ')
        {
            if (thorough.validation)
                private$check.can.apply(from.dim.names=from.dim.names,
                                        to.dim.names=NULL,
                                        thorough.validation=thorough.validation,
                                        throw.errors=T,
                                        error.prefix=error.prefix)
            
            private$do.apply.to.dim.names.or.ontology(from.dim.names=from.dim.names,
                                                      error.prefix=error.prefix)
        },
        
        #'@title Get the ontology that would be generated after applying this mapping to some data
        #'
        #'@details An alias of the apply.to.dim.names function (which appropriately handles if dim.names is additionally an ontology)
        apply.to.ontology = function(ontology, thorough.validation = T, error.prefix='Cannot apply ontology.mapping to ontology: ')
        {
            self$apply.to.dim.names(from.dim.names=ontology, thorough.validation = thorough.validation, error.prefix=error.prefix)
        },
        
        #'@title Get the dimnames that would be generated after applying this mapping to some data with the given dim.names
        reverse.apply.to.dim.names = function(to.dim.names, error.prefix='Cannot apply ontology.mapping from dim names: ')
        {
            # I do not have a check can apply here
            # This omission could lead to some pretty unintelligible error messages if there is an error
            
            private$do.reverse.apply.to.dim.names.or.ontology(to.dim.names=to.dim.names,
                                                              error.prefix=error.prefix)
        },
        
        
        apply = function(from.arr, 
                         to.dim.names=NULL, 
                         fun='sum', 
                         na.rm=F,
                         error.prefix='Cannot apply ontology.mapping: ')
        {
            # Validate dim.names
            private$check.can.apply(from.dim.names=dimnames(from.arr),
                                    to.dim.names=to.dim.names,
                                    thorough.validation=T,
                                    throw.errors=T,
                                    error.prefix=error.prefix)
            
            # Make to.dim.names if missing
            if (is.null(to.dim.names))
                to.dim.names = private$do.apply.to.dim.names.or.ontology(from.dim.names = dimnames(from.arr),
                                                                         error.prefix = error.prefix)
            
            # Validate 'fun'
            if (is.character(fun))
            {
                if (length(fun)!=1 || is.na(fun))
                    stop("If 'fun' is the name of a function, it must be a single, non-NA character value")
                
                apply.sum = fun=='sum'
                
                if (!apply.sum)
                    fun = tryCatch(get(fun),
                                   error=function(e){
                                       stop(paste0(error.prefix,
                                                   "The value passed to fun (", fun, 
                                                   "') is not the name of a valid function"))
                                   })
            }
            else if (is.function(fun))
                apply.sum = identical(fun, sum)
            else
                stop(paste0(error.prefix, "The 'fun' to apply an ontology mapping must be either a function or the name of a function"))
            
            if (apply.sum && is.numeric(from.arr))
                private$do.apply.sum(from.arr, to.dim.names, na.rm=na.rm, error.prefix=error.prefix)
            else
                private$do.apply.non.sum(from.arr, to.dim.names, fun=fun, na.rm=na.rm, error.prefix=error.prefix)
        },
        
        reverse.apply = function(to.arr, 
                                 from.dim.names=NULL, 
                                 na.rm=F,
                                 error.prefix='Cannot reverse-apply ontology.mapping: ')
        {
            # Make from.dim.names if missing
            if (is.null(from.dim.names))
                from.dim.names = self$reverse.apply.to.dim.names(dimnames(to.arr))
            
            # Validate dim.names
            private$check.can.apply(from.dim.names = from.dim.names,
                                    to.dim.names = dimnames(to.arr),
                                    thorough.validation=T,
                                    throw.errors=T,
                                    error.prefix=error.prefix)
            
            rv = to.arr[self$get.reverse.mapping.indices(from.dim.names=from.dim.names,
                                                         to.dim.names=dimnames(to.arr),
                                                         error.prefix=error.prefix)]
            
            dim(rv) = sapply(from.dim.names, length)
            dimnames(rv) = from.dim.names
            rv
        },
        
        get.matrix = function(from.dim.names, to.dim.names, error.prefix='Cannot get matrix from ontology.mapping: ')
        {
            # Validate dim.names
            private$check.can.apply(from.dim.names=from.dim.names,
                                    to.dim.names=to.dim.names,
                                    thorough.validation=T,
                                    throw.errors=T,
                                    error.prefix=error.prefix)
            
            if (is.null(to.dim.names))
                to.dim.names = private$do.apply.to.dim.names.or.ontology(from.dim.names = from.dim.names,
                                                                         error.prefix = error.prefix)
            
            private$do.get.matrix(from.dim.names=from.dim.names,
                                  to.dim.names=to.dim.names,
                                  error.prefix=error.prefix)
        },
        
        #'@return A list, with length the same as the length that would be generated by
        #' applying the ontology mapping to data with the given from.dim.names
        #' The ith element of the return value is an integer vector containing 
        #' the indices into the from data which are combined 
        #' ie - to[i] would be equal to fun(from[ get.mapping.indices()[[i]] ])
        get.mapping.indices = function(from.dim.names, to.dim.names, error.prefix='Cannot get mapping indices from ontology.mapping: ')
        {   
            # Validate dim.names
            private$check.can.apply(from.dim.names=from.dim.names,
                                    to.dim.names=to.dim.names,
                                    thorough.validation=T,
                                    throw.errors=T,
                                    error.prefix=error.prefix)
            
            if (is.null(to.dim.names))
                to.dim.names = private$do.apply.to.dim.names.or.ontology(from.dim.names = from.dim.names,
                                                                         error.prefix = error.prefix)
            
            private$do.get.mapping.indices(from.dim.names=from.dim.names,
                                           to.dim.names=to.dim.names,
                                           error.prefix=error.prefix)
        },
        
        get.mapping.vector = function(from.values=character(), from.dimension=self$from.dimensions[1], to.dimension=self$to.dimensions[1])
        {
            if (!is.character(from.values) || any(is.na(from.values)))
                stop("'from.values' must be a character vector with no NA values")
            
            if (!is.character(from.dimension) || length(from.dimension)!=1 || is.na(from.dimension))
                stop("'from.dimension' must be a single, non-NA character value")
            
            if (!is.character(to.dimension) || length(to.dimension)!=1 || is.na(to.dimension))
                stop("'to.dimension' must be a single, non-NA character value")
            
            if (!is(self, 'identity.ontology.mapping'))
            {
                if (all(from.dimension!=self$from.dimensions))
                    stop("'from.dimension' ('", from.dimension, "') must be one of the ontology mapping's from.dimensions")
                
                if (all(to.dimension!=self$to.dimensions))
                    stop("'to.dimension' ('", to.dimension, "') must be one of the ontology mapping's to.dimensions")
            }
            
            private$do.get.mapping.vector(from.values = from.values,
                                          from.dimension = from.dimension,
                                          to.dimension = to.dimension)
        },
        
        get.reverse.mapping.vector = function(to.values=character(), from.dimension=self$from.dimensions[1], to.dimension=self$to.dimensions[1])
        {
            if (!is.character(to.values) || any(is.na(to.values)))
                stop("'to.values' must be a character vector with no NA values")
            
            if (!is.character(from.dimension) || length(from.dimension)!=1 || is.na(from.dimension))
                stop("'from.dimension' must be a single, non-NA character value")
            
            if (!is.character(to.dimension) || length(to.dimension)!=1 || is.na(to.dimension))
                stop("'to.dimension' must be a single, non-NA character value")
            
            if (!is(self, 'identity.ontology.mapping'))
            {
                if (all(from.dimension!=self$from.dimensions))
                    stop("'from.dimension' ('", from.dimension, "') must be one of the ontology mapping's from.dimensions")
                
                if (all(to.dimension!=self$to.dimensions))
                    stop("'to.dimension' ('", to.dimension, "') must be one of the ontology mapping's to.dimensions")
            }
            
            private$do.get.reverse.mapping.vector(to.values = to.values,
                                                  from.dimension = from.dimension,
                                                  to.dimension = to.dimension)
        },
        
        get.reverse.mapping.indices = function(from.dim.names, to.dim.names, error.prefix='Cannot get mapping indices from ontology.mapping: ')
        {   
            forward.indices = self$get.mapping.indices(from.dim.names = from.dim.names,
                                                       to.dim.names = to.dim.names,
                                                       error.prefix = error.prefix)
            
            n.from = prod(sapply(from.dim.names, length))
            n.to = prod(sapply(to.dim.names, length))
            
            #  sapply(1:n.from, function(to.index){
            #     mask = sapply(forward.indices, function(indices){
            #         any(indices==to.index)
            #     })
            #     
            #     (1:n.to)[mask][1]
            # })
            
            do_get_reverse_indices_from_forward(forward_indices = forward.indices,
                                                n_from = n.from)
        },
        
        get.required.from.dim.names = function(to.dim.names)
        {
            if (is.list(to.dim.names) && length(to.dim.names)==0)
                list()
            else
            {
                check.dim.names.valid(dim.names = to.dim.names,
                                      variable.name.for.error = 'to.dim.names',
                                      allow.empty = T,
                                      allow.duplicate.values.within.dimensions = T,
                                      allow.duplicate.values.across.dimensions = T,
                                      error.prefix='Error in get.required.from.dim.names(): ')
                
                private$do.get.required.from.dim.names(to.dim.names)
            }
        },
        
        get.required.from.dimensions = function(to.dimensions)
        {
            if (!is.character(to.dimensions) || length(to.dimensions)==0 || any(is.na(to.dimensions)) || any(nchar(to.dimensions)==0))
                stop("'to.dimensions' must be a non-empty character vector with no NA values")
            
            to.dim.names = self$to.dim.names[to.dimensions]
            to.dim.names[sapply(to.dim.names, is.null)] = 'placeholder'
            names(to.dim.names) = to.dimensions
            
            names(private$do.get.required.from.dim.names(to.dim.names))
        }
    ),
    
    active = list(
        
        name = function(value)
        {
            if (missing(value))
                private$i.name
            else
                stop("Cannot set value for 'name' in ontology.mapping - it is read-only")
        },
        
        component.names = function(value)
        {
            if (missing(value))
                private$i.component.names
            else
                stop("Cannot set value for 'component.names' in ontology.mapping - they are read-only")
        },
        
        is.identity.mapping = function(value)
        {
            if (missing(value))
                length(self$from.dimensions)==0 && length(self$to.dimensions)==0
            else
                stop("Cannot set value for 'is.identity.mapping' in ontology.mapping - it is read-only")
        },
        
        from.dimensions = function(value)
        {
            if (missing(value))
                names(self$from.dim.names)
            else
                stop("Cannot set value for 'from.dimensions' in ontology.mapping - they are read-only")
        },
        
        to.dimensions = function(value)
        {
            if (missing(value))
                names(self$to.dim.names)
            else
                stop("Cannot set value for 'to.dimensions' in ontology.mapping - they are read-only")
        },
        
        from.dim.names = function(value)
        {
            stop(paste0("This subclass (",
                        paste0("'", class(self), "'", collapse=', '),
                        ") of 'ontology.mapping' is incompletely specified. The 'from.dim.names' method must be implemented at the subclass level"))
            #            stop("This subclass of 'ontology.mapping' is incompletely specified. The 'from.dim.names' method must be implemented at the subclass level")
        },
        
        to.dim.names = function(value)
        {
            stop("This subclass of 'ontology.mapping' is incompletely specified. The 'to.dim.names' method must be implemented at the subclass level")
        }
    ),
    
    private = list(
        i.name = NULL,
        i.component.names = NULL,
        
        do.apply.sum = function(from.arr,
                                to.dim.names,
                                na.rm,
                                error.prefix)
        {
            stop("This subclass of 'ontology.mapping' is incompletely specified. The private 'do.apply.sum' method must be implemented at the subclass level")
        },
        
        do.apply.non.sum = function(from.arr,
                                    to.dim.names,
                                    fun,
                                    na.rm,
                                    error.prefix)
        {
            indices = private$do.get.mapping.indices(from.dim.names=dimnames(from.arr),
                                                     to.dim.names=to.dim.names,
                                                     error.prefix=error.prefix)
            
            rv = sapply(1:prod(sapply(to.dim.names, length)), function(i){
                fun(from.arr[ indices[[i]] ])
            })
            
            dim(rv) = sapply(to.dim.names, length)
            dimnames(rv) = to.dim.names
            
            rv  
        },
        
        do.get.matrix = function(from.dim.names,
                                 to.dim.names,
                                 error.prefix)
        {
            stop("This subclass of 'ontology.mapping' is incompletely specified. The private 'do.get.matrix' method must be implemented at the subclass level")            
        },
        
        do.get.mapping.indices = function(from.dim.names,
                                          to.dim.names,
                                          error.prefix)
        {
            stop("This subclass of 'ontology.mapping' is incompletely specified. The private 'do.get.mapping.indices' method must be implemented at the subclass level")           
        },
        
        do.get.mapping.vector = function(from.values, from.dimension, to.dimension)
        {
            stop("This subclass of 'ontology.mapping' is incompletely specified. The private 'do.get.mapping.vector' method must be implemented at the subclass level")     
        },
        
        do.get.reverse.mapping.vector = function(to.values, from.dimension, to.dimension)
        {
            stop("This subclass of 'ontology.mapping' is incompletely specified. The private 'do.get.reverse.mapping.vector' method must be implemented at the subclass level")     
        },
        
        do.apply.to.dim.names.or.ontology = function(from.dim.names,
                                                     error.prefix)
        {
            stop("This subclass of 'ontology.mapping' is incompletely specified. The private 'do.apply.to.dim.names.or.ontology' method must be implemented at the subclass level")            
        },
        
        check.can.apply = function(from.dim.names, 
                                   to.dim.names,
                                   thorough.validation = T,
                                   throw.errors,
                                   error.prefix)
        {
            stop("This subclass of 'ontology.mapping' is incompletely specified. The private 'check.can.apply' method must be implemented at the subclass level")            
        },
        
        do.get.required.from.dim.names = function(to.dim.names)
        {
            stop("do.get.required.from.dim.names() for an ontology.mapping must be implented at the subclass level")
        }
        
    )
)

IDENTITY.ONTOLOGY.MAPPING = R6::R6Class(
    'identity.ontology.mapping',
    inherit = ONTOLOGY.MAPPING,
    
    public = list(
        initialize = function()
        {
            super$initialize(name='Identity Ontology Mapping')
        },
        
        print = function(...)
        {
            print("An identity ('no-change') ontology mapping")
        },
        
        equals = function(other)
        {
            is(other, 'ontology.mapping') && other$is.identity.mapping
        },
        
        is.reverse.of = function(other)
        {
            F
        }
    ),
    
    active = list(
        
        from.dim.names = function(value)
        {
            list()
        },
        
        to.dim.names = function(value)
        {
            list()
        }
    ),
    
    private = list(
        do.apply.sum = function(from.arr,
                                to.dim.names,
                                na.rm,
                                error.prefix)
        {
            fast.array.access(from.arr, to.dim.names)
        },
        
        do.apply.non.sum = function(from.arr,
                                    to.dim.names,
                                    fun,
                                    na.rm,
                                    error.prefix)
        {
            fast.array.access(from.arr, to.dim.names)
        },
        
        do.get.matrix = function(from.dim.names,
                                 to.dim.names,
                                 error.prefix)
        {
            expand.indices = get.expand.array.indices(to.expand.dim.names = to.dim.names,
                                                      target.dim.names = from.dim.names)
            n.from = length(expand.indices)
            n.to = prod(sapply(to.dim.names, length))
            
            nonzero.indices = as.integer((1:n.from-1)*n.to + expand.indices)
            
            rv = matrix(0, nrow=n.to, ncol=n.from)
            rv[nonzero.indices] = 1
            
            rv
        },
        
        do.get.mapping.indices = function(from.dim.names,
                                          to.dim.names,
                                          error.prefix)
        {
            as.list(get.array.access.indices(from.dim.names, dimension.values=to.dim.names))
        },
        
        do.get.mapping.vector = function(from.values, from.dimension, to.dimension)
        {
            from.values = unique(from.values)
            names(from.values) = from.values
            from.values
        },
        
        do.get.reverse.mapping.vector = function(to.values, from.dimension, to.dimension)
        {
            to.values = unique(to.values)
            names(to.values) = to.values
            to.values
        },
        
        do.apply.to.dim.names.or.ontology = function(from.dim.names,
                                                     error.prefix)
        {
            from.dim.names
        },
        
        do.reverse.apply.to.dim.names.or.ontology = function(to.dim.names,
                                                             error.prefix)
        {
            to.dim.names
        },
        
        check.can.apply = function(from.dim.names, 
                                   to.dim.names,
                                   thorough.validation=T,
                                   throw.errors,
                                   error.prefix)
        {
            # if (is.null(to.dim.names) || dim.names.are.subset(sub.dim.names = to.dim.names,
            #                                               super.dim.names = from.dim.names))
            #     T
            if (is.null(to.dim.names))
                T
            else
            {
                dimensions.are.subset = length(setdiff(names(to.dim.names), names(from.dim.names))) == 0
                if (dimensions.are.subset) {
                    dim.names.intersect = sapply(names(to.dim.names), function(d) {
                        length(intersect(to.dim.names[[d]], from.dim.names[[d]])) > 0
                    })
                } else
                    dim.names.intersect = F
                
                if (!dimensions.are.subset || !all(dim.names.intersect)) {
                    if (throw.errors)
                        stop(paste0(error.prefix, "'to.dim.names' must intersect 'from.dim.names' in every dimension"))
                    else
                        F
                } else T
                
            }
        },
        
        do.get.required.from.dim.names = function(to.dim.names)
        {
            to.dim.names
        }
        
    )
)

BASIC.ONTOLOGY.MAPPING = R6::R6Class(
    'basic.ontology.mapping',
    inherit = ONTOLOGY.MAPPING,
    
    public = list(
        
        initialize = function(name,
                              from.dimensions,
                              to.dimensions,
                              from.values,
                              to.values,
                              from.dimension.chunks = list(from.dimensions),
                              to.dimension.chunks = list(to.dimensions),
                              component.names=name,
                              reverse.of.name=NULL)
        {
            super$initialize(name, component.names = component.names)
            
            # presume the error checking has been done by the
            # public-facing constructor
            
            if (dim(to.values)[1]==0)
                row.order = integer()
            else
            {
                order.arguments = c(lapply(1:dim(to.values)[2], function(i){to.values[,i]}),
                                    lapply(1:dim(from.values)[2], function(i){from.values[,i]}))
                row.order = do.call(order, args=order.arguments)
            }
            
            from.order = order(from.dimensions)
            to.order = order(to.dimensions)
            
            na.from.mask = apply(is.na(from.values), 1, any)[row.order]
            na.to.mask = apply(is.na(to.values), 1, any)[row.order]
            
            private$i.from.dimensions = from.dimensions[from.order]
            private$i.to.dimensions = to.dimensions[to.order]
            private$i.from.values = from.values[row.order,from.order,drop=F]
            private$i.to.values = to.values[row.order,to.order,drop=F]
            private$i.mapped.from.values = private$i.from.values[!na.from.mask & !na.to.mask,,drop=F]
            private$i.mapped.to.values = private$i.to.values[!na.from.mask & !na.to.mask,,drop=F]
            
            private$i.from.dimension.chunks = from.dimension.chunks
            private$i.to.dimension.chunks = to.dimension.chunks
            
            dimnames(private$i.from.values)[[2]] = dimnames(private$i.mapped.from.values)[[2]] = private$i.from.dimensions
            dimnames(private$i.to.values)[[2]] = dimnames(private$i.mapped.to.values)[[2]] = private$i.to.dimensions
            
            private$i.unique.from.values = lapply(1:length(private$i.from.dimensions), function(d){
                unique(private$i.from.values[!na.from.mask,d])
            })
            names(private$i.unique.from.values) = private$i.from.dimensions
            
            private$i.unique.to.values = lapply(1:length(private$i.to.dimensions), function(d){
                unique(private$i.to.values[!na.to.mask,d])
            })
            names(private$i.unique.to.values) = private$i.to.dimensions
            
            private$i.reverse.of.name = reverse.of.name
        },
        
        equals = function(other)
        {
            all(dim(self$from.values)==dim(other$from.values)) &&
                all(dim(self$to.values)==dim(other$to.values)) &&
                all(self$from.dimensions == other$from.dimensions) &&
                all(self$to.dimensions == other$to.dimensions) &&
                identical(self$from.values, other$from.values) &&
                identical(self$to.values, other$to.values)
        },
        
        is.reverse.of = function(other)
        {
            is(other, 'basic.ontology.mapping') &&
                ((!is.null(private$i.reverse.of.name) && other$name == private$i.reverse.of.name) ||
                     (!is.null(other$reverse.of.name) && private$i.name == other$reverse.of.name))
        }
    ),
    
    active = list(
        
        from.values = function(value)
        {
            if (missing(value))
                private$i.from.values
            else
                stop("Cannot set value for 'from.values' in ontology.mapping - they are read-only")
        },
        
        to.values = function(value)
        {
            if (missing(value))
                private$i.to.values
            else
                stop("Cannot set value for 'to.values' in ontology.mapping - they are read-only")
        },
        
        n.from.and.to.values = function(value)
        {
            if (missing(value))
                dim(private$i.from.values)[1]
            else
                stop("Cannot set value for 'n.from.and.to.values' in ontology.mapping - they are read-only")
        },
        
        from.dim.names = function(value)
        {
            if (missing(value))
                private$i.unique.from.values
            else
                stop("Cannot set value for 'from.dim.names' in ontology.mapping - they are read-only")
        },
        
        to.dim.names = function(value)
        {
            if (missing(value))
                private$i.unique.to.values
            else
                stop("Cannot set value for 'to.dim.names' in ontology.mapping - they are read-only")
        },
        
        reverse.of.name = function(value)
        {
            if (missing(value))
                private$i.reverse.of.name
            else
                stop("Cannot set value for 'reverse.of.name' in ontology.mapping - it is read-only")
        }
    ),
    
    private = list(
        i.name = NULL,
        i.component.names = NULL,
        
        i.from.dimensions = NULL,
        i.to.dimensions = NULL,
        
        i.from.dimension.chunks = NULL,
        i.to.dimension.chunks = NULL,
        
        i.mapped.from.values = NULL,
        i.from.values  = NULL,
        i.mapped.to.values = NULL, #leaves out rows where the corresponding from.values were NA
        i.to.values = NULL,
        
        i.unique.from.values = NULL,
        i.unique.to.values = NULL,
        
        i.reverse.of.name = NULL,
        
        do.apply.sum = function(from.arr,
                                to.dim.names,
                                na.rm,
                                error.prefix)
        {
            rv = array(as.numeric(NA), dim=sapply(to.dim.names, length), dimnames=to.dim.names)
            
            rv = apply_ontology_mapping(src = from.arr,
                                        dst = rv,
                                        from_values = private$i.mapped.from.values,
                                        to_values = private$i.mapped.to.values,
                                        na_rm = na.rm)
            
            if (is.null(rv)) #theoretically should not happen - should have been caught by error checking
                stop(paste0(error.prefix, "There was an error applying the ontology.mapping"))
            
            rv
        },
        
        do.get.matrix = function(from.dim.names,
                                 to.dim.names,
                                 error.prefix = '')
        {
            rv = matrix(0, nrow=prod(sapply(to.dim.names, length)), ncol=prod(sapply(from.dim.names, length)))
            
            rv = get_ontology_mapping_matrix(src_dim_names = from.dim.names,
                                             dst_dim_names = to.dim.names,
                                             dst = rv,
                                             from_values = private$i.mapped.from.values,
                                             to_values = private$i.mapped.to.values)
            
            if (is.null(rv)) #theoretically should not happen - should have been caught by error checking
                stop(paste0(error.prefix, "There was an error applying the ontology.mapping to get a matrix"))
            
            rv
        },
        
        do.get.mapping.indices = function(from.dim.names,
                                          to.dim.names,
                                          error.prefix = '')
        {   
            rv = get_ontology_mapping_indices(src_dim_names = from.dim.names,
                                              dst_dim_names = to.dim.names,
                                              from_values = private$i.mapped.from.values,
                                              to_values = private$i.mapped.to.values)
            
            if (is.null(rv)) #theoretically should not happen - should have been caught by error checking
                stop(paste0(error.prefix, "There was an error applying the ontology.mapping to get a mapping.indices"))
            
            rv   
        },
        
        do.get.mapping.vector = function(from.values, from.dimension, to.dimension)
        {
            mask = !is.na(private$i.from.values[,from.dimension])
            
            rv = private$i.to.values[mask, to.dimension]
            names(rv) = private$i.from.values[mask, from.dimension]
            
            rv
        },
        
        do.get.reverse.mapping.vector = function(to.values, from.dimension, to.dimension)
        {
            mask = !is.na(private$i.to.values[,to.dimension])
            
            rv = private$i.from.values[mask, from.dimension]
            names(rv) = private$i.to.values[mask, to.dimension]
            
            rv
        },
        
        do.apply.to.dim.names.or.ontology = function(from.dim.names,
                                                     error.prefix)
        {
            given.from.values = get.every.combination(from.dim.names[self$from.dimensions])
            
            resulting.to.indices = unlist(apply(given.from.values, 1, row.indices.of, haystack = private$i.mapped.from.values))
            resulting.to.values = unique(private$i.mapped.to.values[resulting.to.indices,,drop=F])
            
            resulting.to.dim.names = lapply(self$to.dimensions, function(d){
                if (is.ontology(from.dim.names) && !is.null(from.dim.names[[d]]) && is_complete(from.dim.names)[d])
                    self$to.dim.names[[d]]
                else
                    unique(resulting.to.values[,d])
            })
            names(resulting.to.dim.names) = self$to.dimensions
            
            dimensions.to.keep = setdiff(union(names(from.dim.names), setdiff(self$to.dimensions, self$from.dimensions)), 
                                         setdiff(self$from.dimensions, self$to.dimensions))
            rv = lapply(dimensions.to.keep, function(d){
                if (any(d==self$to.dimensions))
                    resulting.to.dim.names[[d]]
                else
                    from.dim.names[[d]]
            })
            names(rv) = dimensions.to.keep
            
            if (is.ontology(from.dim.names))
            {
                is.complete = is_complete(from.dim.names)
                is.complete.after.mapping = unlist(lapply(1:length(private$from.dimension.chunks), function(i){
                    chunk.is.complete = all(is.complete[ private$from.dimension.chunks[[i]] ])
                    chunk.is.complete = rep(chunk.is.complete, length(private$to.dimension.chunks[[i]]))
                    names(chunk.is.complete) = private$to.dimension.chunks[[i]]
                    chunk.is.complete
                }))
                is.complete[names(is.complete.after.mapping)] = is.complete.after.mapping
                
                as.ontology(rv, incomplete.dimensions = names(is.complete)[!is.complete])
            }
            else
                rv
        },
        
        do.reverse.apply.to.dim.names.or.ontology = function(to.dim.names,
                                                             error.prefix)
        {
            # if (length(setdiff(self$to.dimensions, names(to.dim.names)))>0)
            #     stop(paste0(error.prefix,
            #                 "Cannot reverse apply mapping to dim.names - to.dim.names do not contain ",
            #                 collapse.with.or("'", setdiff(self$to.dimensions, names(to.dim.names)), "'")
            #                 ))
            
            relevant.dimensions = intersect(self$to.dimensions, names(to.dim.names))
            given.to.values = get.every.combination(to.dim.names[relevant.dimensions])
            
            resulting.from.indices = unlist(apply(given.to.values, 1, row.indices.of, haystack = private$i.mapped.to.values[,relevant.dimensions,drop=F]))
            resulting.from.values = unique(private$i.mapped.from.values[resulting.from.indices,,drop=F])
            
            resulting.from.dim.names = lapply(self$from.dimensions, function(d){
                if (is.ontology(to.dim.names) && !is.null(to.dim.names[[d]]) && is_complete(to.dim.names)[d])
                    self$from.dim.names[[d]]
                else
                    unique(resulting.from.values[,d])
            })
            names(resulting.from.dim.names) = self$from.dimensions
            
            # dimensions.to.keep = setdiff(names(to.dim.names), setdiff(self$to.dimensions, self$from.dimensions))
            dimensions.to.keep = setdiff(union(names(to.dim.names), self$from.dimensions), setdiff(self$to.dimensions, self$from.dimensions))
            rv = lapply(dimensions.to.keep, function(d){
                if (any(d==self$from.dimensions))
                    resulting.from.dim.names[[d]]
                else
                    to.dim.names[[d]]
            })
            names(rv) = dimensions.to.keep
            
            if (is.ontology(to.dim.names))
            {
                is.complete = is_complete(to.dim.names)
                is.complete.after.mapping = unlist(lapply(1:length(private$to.dimension.chunks), function(i){
                    chunk.is.complete = all(is.complete[ private$to.dimension.chunks[[i]] ])
                    chunk.is.complete = rep(chunk.is.complete, length(private$from.dimension.chunks[[i]]))
                    names(chunk.is.complete) = private$from.dimension.chunks[[i]]
                    chunk.is.complete
                }))
                is.complete[names(is.complete.after.mapping)] = is.complete.after.mapping
                
                as.ontology(rv, incomplete.dimensions = names(is.complete)[!is.complete])
            }
            else
                rv
        },
        
        check.can.apply = function(from.dim.names, 
                                   to.dim.names,
                                   thorough.validation = T,
                                   throw.errors,
                                   error.prefix)
        {
            
            if (!initial.check.can.apply(mapping=self,
                                         from.dim.names=from.dim.names,
                                         to.dim.names=to.dim.names,
                                         thorough.validation = thorough.validation,
                                         throw.errors=throw.errors,
                                         error.prefix=error.prefix))
                return (F)
            
            if (is.null(to.dim.names))
            {
                #?we can skip this if all from.dimensions in from.dim.names are complete
                given.from.values = get.every.combination(from.dim.names[self$from.dimensions])
                
                resulting.to.indices = unlist(apply(given.from.values, 1, row.indices.of, haystack = private$i.mapped.from.values))
                resulting.to.values = unique(private$i.mapped.to.values[resulting.to.indices,,drop=F])
            }
            else
            {
                resulting.to.values = get.every.combination(to.dim.names[intersect(self$to.dimensions,
                                                                                   names(to.dim.names))])
            }
            
            required.from.indices = unlist(apply(resulting.to.values, 1, row.indices.of, haystack = private$i.mapped.to.values))
            
            missing.required.from.values = sapply(self$from.dimensions, function(d){
                missing.values = setdiff(private$i.mapped.from.values[required.from.indices,d],
                                         from.dim.names[[d]])
                if (length(missing.values)==0)
                    F
                else
                {
                    if (throw.errors)
                    {
                        stop(paste0(error.prefix, 
                                    "from.dim.names ['", d, "'] is missing ", 
                                    length(missing.values),
                                    ifelse(length(missing.values)==1, ' value', ' values'),
                                    ": ", collapse.with.and("'", missing.values, "'")))
                    } 
                    else
                        T
                }
            })
            
            !any(missing.required.from.values)
        },
        
        do.get.required.from.dim.names = function(to.dim.names)
        {
            present.to.dimensions = intersect(names(to.dim.names), self$to.dimensions)
            absent.to.dimensions = setdiff(self$to.dimensions, present.to.dimensions)
            unmapped.dim.names = to.dim.names[setdiff(names(to.dim.names), present.to.dimensions)]
            
            if (length(present.to.dimensions)==0)
                unmapped.dim.names
            else
            {
                to.values = get.every.combination(to.dim.names[present.to.dimensions])
                required.from.indices = unlist(apply(to.values, 1, row.indices.of, 
                                                     haystack = private$i.mapped.to.values[,present.to.dimensions,drop=F]))
                
                if (length(absent.to.dimensions)>0) # Check to see if any from.dimensions are unnecessary
                {
                    relevant.from.dimensions = get.relevant.mapping.from.dimensions(from.values=private$i.mapped.from.values,
                                                                                    to.values=private$i.mapped.to.values[,present.to.dimensions,drop=F])
                }
                else #we *could* check if all from dimensions are relevant even if we're using all to dimension - but we presume no one creates a mapping with irrelevant dimensions
                    relevant.from.dimensions = self$from.dimensions
                
                mapped.from.dim.names = lapply(relevant.from.dimensions, function(d){
                    unique(private$i.mapped.from.values[required.from.indices,d])
                })
                names(mapped.from.dim.names) = relevant.from.dimensions
                
                c(mapped.from.dim.names, unmapped.dim.names)
            }
        }
    )
)

flatten.sub.mappings.list <- function(sub.mappings)
{
    rv = list()
    for (map in sub.mappings)
    {
        if (is(map, 'combination.ontology.mapping'))
            rv = c(rv, flatten.sub.mappings.list(map$sub.mappings))
        else
            rv = c(rv, list(map))
    }
    
    rv
}

COMBINATION.ONTOLOGY.MAPPING = R6::R6Class(
    'combination.ontology.mapping',
    inherit = ONTOLOGY.MAPPING,
    
    public = list(
        
        initialize = function(name,
                              sub.mappings)
        {
            if (!is.list(sub.mappings))
                stop("Cannot create combination ontology mapping: sub.mappings must be a list")
            
            sub.mappings = flatten.sub.mappings.list(sub.mappings)
            
            super$initialize(name,
                             component.names=unlist(sapply(sub.mappings, function(map){map$component.names})))
            
            private$i.sub.mappings = sub.mappings
        },
        
        equals = function(other)
        {
            is(other, 'combination.ontology.mapping') &&
                length(self$sub.mappings) == length(other$sub.mappings) &&
                all(sapply(1:length(self$sub.mappings), function(i){
                    self$sub.mappings[[i]]$equals(other$sub.mappings[[i]])
                }))
        }
    ),
    
    active = list(
        
        from.dim.names = function(value)
        {
            if (missing(value))
            {
                rv = private$i.sub.mappings[[length(private$i.sub.mappings)]]$from.dim.names
                for (sub.mapping in rev(private$i.sub.mappings)[-1])
                {
                    rv[names(sub.mapping$from.dim.names)] = sub.mapping$from.dim.names
                    rv = rv[setdiff(names(rv),
                                    setdiff(names(sub.mapping$to.dim.names),
                                            names(sub.mapping$from.dim.names)))]
                }
                
                rv
            }
            else
                stop("Cannot set value for 'from.dim.names' in ontology.mapping - they are read-only")  
        },
        
        to.dim.names = function(value)
        {
            if (missing(value))
            {
                rv = private$i.sub.mappings[[1]]$to.dim.names
                for (sub.mapping in private$i.sub.mappings[-1])
                {
                    rv[names(sub.mapping$to.dim.names)] = sub.mapping$to.dim.names
                    rv = rv[setdiff(names(rv),
                                    setdiff(names(sub.mapping$from.dim.names),
                                            names(sub.mapping$to.dim.names)))]
                }
                
                rv
            }
            else
                stop("Cannot set value for 'from.dim.names' in ontology.mapping - they are read-only")  
            
        },
        
        sub.mappings = function(value)
        {
            if (missing(value))
                private$i.sub.mappings
            else
                stop("Cannot set value for 'sub.mappings' in ontology.mapping - they are read-only")  
        }
    ),
    
    private = list(
        
        i.sub.mappings = NULL,
        
        do.apply.sum = function(from.arr,
                                to.dim.names,
                                na.rm,
                                error.prefix)
        {
            from.dim.names = dimnames(from.arr)
            dimensions.unaffected.by.mappings = names(from.dim.names)
            dimensions.that.will.be.dropped = setdiff(names(from.dim.names), names(to.dim.names))
            for (sub.mapping in private$i.sub.mappings)
            {
                dimensions.unaffected.by.mappings = setdiff(dimensions.unaffected.by.mappings,
                                                            c(sub.mapping$from.dimensions, sub.mapping$to.dimensions))
                dimensions.that.will.be.dropped = setdiff(dimensions.that.will.be.dropped, sub.mapping$from.dimensions)
                
            }
            dimensions.unaffected.by.mappings = setdiff(dimensions.unaffected.by.mappings, dimensions.that.will.be.dropped)
            
            rv = from.arr
            for (i in 1:length(private$i.sub.mappings))
            {
                sub.mapping = private$i.sub.mappings[[i]]
                
                if (i == length(private$i.sub.mappings))
                    next.to.dim.names = to.dim.names
                else
                {
                    next.to.dim.names = sub.mapping$apply.to.dim.names(from.dim.names, error.prefix=error.prefix)
                    if (i==1)
                    {
                        next.to.dim.names = next.to.dim.names[setdiff(names(next.to.dim.names), dimensions.that.will.be.dropped)]
                        next.to.dim.names[dimensions.unaffected.by.mappings] = to.dim.names[dimensions.unaffected.by.mappings]
                    }
                }
                
                rv = sub.mapping$apply(rv, to.dim.names=next.to.dim.names, na.rm=na.rm, error.prefix=error.prefix)
                
                from.dim.names = next.to.dim.names
            }
            
            rv
        },
        
        do.get.matrix = function(from.dim.names,
                                 to.dim.names,
                                 error.prefix)
        {
            dimensions.unaffected.by.mappings = names(from.dim.names)
            dimensions.unaffected.by.mappings = names(from.dim.names)
            dimensions.that.will.be.dropped = setdiff(names(from.dim.names), names(to.dim.names))
            for (sub.mapping in private$i.sub.mappings)
            {
                dimensions.unaffected.by.mappings = setdiff(dimensions.unaffected.by.mappings,
                                                            c(sub.mapping$from.dimensions, sub.mapping$to.dimensions))
                dimensions.that.will.be.dropped = setdiff(dimensions.that.will.be.dropped, sub.mapping$from.dimensions)
                
            }
            dimensions.unaffected.by.mappings = setdiff(dimensions.unaffected.by.mappings, dimensions.that.will.be.dropped)
            
            rv = NULL
            for (i in 1:length(private$i.sub.mappings))
            {
                sub.mapping = private$i.sub.mappings[[i]]
                
                if (i == length(private$i.sub.mappings))
                    next.to.dim.names = to.dim.names
                else
                {
                    next.to.dim.names = sub.mapping$apply.to.dim.names(from.dim.names, error.prefix=error.prefix)
                    if (i==1)
                    {
                        next.to.dim.names = next.to.dim.names[setdiff(names(next.to.dim.names), dimensions.that.will.be.dropped)]
                        next.to.dim.names[dimensions.unaffected.by.mappings] = to.dim.names[dimensions.unaffected.by.mappings]
                    }
                }
                
                to.add = sub.mapping$get.matrix(from.dim.names, next.to.dim.names, error.prefix=error.prefix)
                
                if (is.null(rv))
                    rv = to.add
                else
                    rv = to.add %*% rv
                
                from.dim.names = next.to.dim.names
            }
            
            rv
        },
        
        do.get.mapping.indices = function(from.dim.names,
                                          to.dim.names,
                                          error.prefix)
        {
            dimensions.unaffected.by.mappings = names(from.dim.names)
            dimensions.that.will.be.dropped = setdiff(names(from.dim.names), names(to.dim.names))
            for (sub.mapping in private$i.sub.mappings)
            {
                dimensions.unaffected.by.mappings = setdiff(dimensions.unaffected.by.mappings,
                                                            c(sub.mapping$from.dimensions, sub.mapping$to.dimensions))
                dimensions.that.will.be.dropped = setdiff(dimensions.that.will.be.dropped, sub.mapping$from.dimensions)
                
            }
            dimensions.unaffected.by.mappings = setdiff(dimensions.unaffected.by.mappings, dimensions.that.will.be.dropped)
            
            rv = NULL
            for (i in 1:length(private$i.sub.mappings))
            {
                sub.mapping = private$i.sub.mappings[[i]]
                
                if (i == length(private$i.sub.mappings))
                    next.to.dim.names = to.dim.names
                else
                {
                    next.to.dim.names = sub.mapping$apply.to.dim.names(from.dim.names, error.prefix=error.prefix)
                    if (i==1)
                    {
                        next.to.dim.names = next.to.dim.names[setdiff(names(next.to.dim.names), dimensions.that.will.be.dropped)]
                        next.to.dim.names[dimensions.unaffected.by.mappings] = to.dim.names[dimensions.unaffected.by.mappings]
                    }
                }
                
                indices = sub.mapping$get.mapping.indices(from.dim.names, to.dim.names=next.to.dim.names, error.prefix=error.prefix)
                
                if (is.null(rv))
                    rv = indices
                else
                    rv = lapply(indices, function(indices.for.one.dst.elem){
                        unique(unlist(rv[indices.for.one.dst.elem]))
                    })
                
                from.dim.names = next.to.dim.names
            }
            
            rv
        },
        
        do.get.mapping.vector = function(from.values, from.dimension, to.dimension)
        {
            if (from.dimension != to.dimension)
                stop("Cannot get a mapping vector from a combined ontology mapping unless from.dimension == to.dimension")
            
            mapping = private$i.sub.mappings[[1]]$get.mapping.vector(from.values, from.dimension=from.dimension, to.dimension=to.dimension)
            for (sub.mapping in private$i.sub.mappings[-1])
            {
                mapping2 = sub.mapping$get.mapping.vector(mapping, from.dimension=from.dimension, to.dimension=to.dimension)
                mapping = mapping2[mapping]
            }
            
            mapping
        },
        
        do.get.reverse.mapping.vector = function(to.values, from.dimension, to.dimension)
        {
            if (from.dimension != to.dimension)
                stop("Cannot get a reverse mapping vector from a combined ontology mapping unless from.dimension == to.dimension")
            
            mapping = private$i.sub.mappings[[length(private$i.sub.mappings)]]$get.reverse.mapping.vector(to.values, from.dimension=from.dimension, to.dimension=to.dimension)
            for (sub.mapping in rev(private$i.sub.mappings)[-1])
            {
                mapping2 = sub.mapping$get.reverse.mapping.vector(mapping, from.dimension=from.dimension, to.dimension=to.dimension)
                mapping = mapping2[mapping]
            }
            
            mapping
        },
        
        do.apply.to.dim.names.or.ontology = function(from.dim.names,
                                                     error.prefix)
        {
            rv = from.dim.names
            for (sub.mapping in private$i.sub.mappings)
                rv = sub.mapping$apply.to.dim.names(rv, thorough.validation = F, error.prefix = error.prefix)
            
            rv
        },
        
        do.reverse.apply.to.dim.names.or.ontology = function(to.dim.names,
                                                             error.prefix)
        {
            rv = to.dim.names
            for (sub.mapping in private$i.sub.mappings)
                rv = sub.mapping$reverse.apply.to.dim.names(rv, error.prefix = error.prefix)
            
            rv
        },
        
        check.can.apply = function(from.dim.names, 
                                   to.dim.names,
                                   thorough.validation = T,
                                   throw.errors,
                                   error.prefix)
        {
            initial.check.can.apply(mapping=self,
                                    from.dim.names=from.dim.names,
                                    to.dim.names=to.dim.names,
                                    thorough.validation = thorough.validation,
                                    throw.errors=throw.errors,
                                    error.prefix=error.prefix)
            
            dim.names = from.dim.names
            for (sub.mapping in private$i.sub.mappings[-length(private$i.sub.mappings)])
            {
                if (!sub.mapping$can.apply.to.dim.names(dim.names,
                                                        to.dim.names = NULL,
                                                        throw.errors = throw.errors,
                                                        error.prefix = error.prefix))
                    return (F)
                
                dim.names = sub.mapping$apply.to.dim.names(dim.names, thorough.validation = thorough.validation, error.prefix=error.prefix)
            }
            
            last.sub.mapping = private$i.sub.mappings[[length(private$i.sub.mappings)]]
            
            last.sub.mapping$can.apply.to.dim.names(dim.names,
                                                    to.dim.names = to.dim.names,
                                                    thorough.validation = thorough.validation,
                                                    throw.errors = throw.errors,
                                                    error.prefix = error.prefix)
            
        },
        
        do.get.required.from.dim.names = function(to.dim.names)
        {
            rv = to.dim.names
            for (sub.mapping in rev(private$i.sub.mappings))
                rv = sub.mapping$get.required.from.dim.names(rv)
            
            rv
        }
    )
)



#IDENTITY.MAPPING = IDENTITY.ONTOLOGY.MAPPING$new()


##-- HELPER --##

# From dimenions are relevant if any given combination of from values in from.values
#   always maps to same combination of to.values
# (ie, a dimension is irrelevant if its presence does not change mapping based on the other values)
get.relevant.mapping.from.dimensions <- function(from.values,
                                                 to.values)
{
    from.dimensions = dimnames(from.values)[[2]]
    n.from.dimensions = length(from.dimensions)
    n.from.dimension.combos = 2^n.from.dimensions
    
    for (n.to.include in 0:(n.from.dimensions-1))
    {
        if (n.to.include==0)
        {
            all(apply(to.values, 1, function(x){
                all(x==to.values[1,])
            }))
        }
        
        combos = combn(from.dimensions, m=n.to.include)
        for (i in 1:dim(combos)[2])
        {
            dimensions.to.include = combos[,i]
            from.values.to.include = from.values[,dimensions.to.include,drop=F]
            
            valid = T
            for (j in 1:dim(from.values.to.include)[1]) #we could iterate over just the unique from values, but I think calling unique is more expensive than duplicating efforts
            {
                indices = row.indices.of(from.values.to.include, needle=from.values.to.include[j,drop=F])
                to.needle = to.values[indices,,drop=F]
                valid = all(apply(to.values[indices[-1],,drop=F], 1, function(x){
                    all(x==to.needle)
                }))
                
                if (!valid)
                    break
            }
            
            if (valid)
                return (dimensions.to.include)
        }
    }
    
    return (from.dimensions)
}

initial.check.can.apply <- function(mapping,
                                    from.dim.names,
                                    to.dim.names,
                                    throw.errors,
                                    error.prefix,
                                    thorough.validation = T)
{
    # Make sure they are valid dimnames 
    #  (this will also be true if from.dim.names is an ontology)
    
    #** Turns out this step is very expensive, and we only need to do it once in 
    #* finding ontology mappings - so we will skip if not needed
    
    if (thorough.validation)
    {
        check.dim.names.valid(from.dim.names,
                              variable.name.for.error = 'from.dim.names',
                              allow.duplicate.values.across.dimensions = T,
                              error.prefix = error.prefix)
    }
    
    if (!is.null(to.dim.names))
    {
        check.dim.names.valid(to.dim.names,
                              variable.name.for.error = 'to.dim.names',
                              allow.duplicate.values.across.dimensions = T,
                              error.prefix = error.prefix)
        
        if (length(intersect(mapping$to.dimensions, names(to.dim.names)))==0)
        {
            if (throw.errors)
                stop(paste0(error.prefix,
                            ifelse(length(mapping$to.dimensions)==1,
                                   paste0("The mapped 'to' dimension ('", mapping$to.dimensions, "') is"),
                                   paste0("None of the mapped 'to' dimensions (", collapse.with.and("'", mapping$to.dimensions, "'"), ") are")),
                            " contained in 'to.dim.names'"))
            else
                return (F)
        }
        
        non.mapped.from.dimensions = setdiff(names(from.dim.names), mapping$from.dimensions)
        non.mapped.to.dimensions = setdiff(names(to.dim.names), mapping$to.dimensions)
        extra.to.dimensions = setdiff(non.mapped.to.dimensions, non.mapped.from.dimensions)
        if (length(extra.to.dimensions)>0)
        {
            if (throw.errors)
                stop(paste0(error.prefix,
                            "'to.dim.names' contains ",
                            ifelse(length(extra.to.dimensions)==1, "a dimension that is", "dimensions that are"),
                            " not present in 'from.dim.names' (",
                            collapse.with.and("'", extra.to.dimensions, "'"), ")"))
            else
                return (F)
        }
        
    }
    
    if (is.ontology(from.dim.names))
        is.complete = is_complete(from.dim.names)
    required.from.values.are.present = sapply(mapping$from.dimensions, function(d){
        if (is.null(from.dim.names[[d]]))
        {
            if (throw.errors)
                stop(paste0(error.prefix,
                            "Dimension '", d, "' is missing from 'from.dim.names"))
            else
                F
        }
        else if (is.ontology(from.dim.names) && is.complete[d])
        {
            if (setequal(from.dim.names[[d]], mapping$from.dim.names[[d]]))
                T
            else
            {
                if (throw.errors)
                {
                    extra.values = setdiff(from.dim.names[[d]], mapping$from.dim.names[[d]])
                    missing.values = setdiff(mapping$from.dim.names[[d]], from.dim.names[[d]])
                    
                    if (length(extra.values)>0)
                        extra.msg = paste0("contains ",
                                           ifelse(length(extra.values)==1, 'an extraneous value', 'extraneous values'),
                                           " (", collapse.with.and("'", extra.values, "'"), ") that ",
                                           ifelse(length(extra.values)==1, 'is', 'are'),
                                           " not present in the mapping")
                    if (length(missing.values)>0)
                        missing.msg = paste0("is missing ", length(missing.values),
                                             ifelse(length(missing.values)==1, ' value', ' values'),
                                             ": ", collapse.with.and("'", missing.values, "'"))
                    
                    error.prefix = paste0(error.prefix,
                                          "from.dim.names['", d, "'] ")
                    if (length(extra.values)==0)
                        stop(paste0(error.prefix, missing.msg))
                    else if (length(missing.values)==0)
                        stop(paste0(error.prefix, extra.msg))
                    else
                        stop(paste0(error.prefix, missing.msg, " and ", extra.msg))
                }
                else
                    F
            }
        }
        else
        {
            if (length(intersect(mapping$from.dim.names[[d]], from.dim.names[[d]]))>0)
                T
            else
            {
                if (throw.errors)
                    stop(paste0(error.prefix, "'from.dim.names' does not contain any mappable dimension values"))
                else
                    F
            }
            
            #            if (is.subset(super=mapping$from.dim.names[[d]], sub=from.dim.names[[d]]))
            #                T
            #            else
            #            {
            #                if (throw.errors)
            #                {
            #                    missing.values = setdiff(mapping$from.dim.names[[d]], from.dim.names[[d]])
            #                    stop(paste0(error.prefix, 
            #                                "'from.dim.names' is missing ", length(missing.values),
            #                                ifelse(length(missing.values)==1, ' value', ' values'),
            #                                ": ", collapse.with.and("'", missing.values, "'")))
            #                }
            #                else
            #                    F
            #            }
        }
    })
    
    all(required.from.values.are.present)
}

# Given two sets of age bracket labels that each fully/contiguously partition the age range,
# find the labels for the coarsest partition that BOTH from.values and to.values can be
# aggregated up to (the "meet" of the two partitions), computed by intersecting their
# boundary points. Returns NULL if no useful (strictly coarser) common partition exists.
get.common.age.refinement.values <- function(from.values, to.values)
{
    from.bounds = parse.age.strata.names(from.values)
    to.bounds = parse.age.strata.names(to.values)
    
    if (is.null(from.bounds) || is.null(to.bounds))
        return (NULL)
    
    from.edges = sort(unique(c(from.bounds$lower, from.bounds$upper)))
    to.edges = sort(unique(c(to.bounds$lower, to.bounds$upper)))
    
    common.edges = sort(intersect(from.edges, to.edges))
    
    # Need at least one bracket, and it must be a genuine (strict) coarsening of from.values,
    # or we'd loop without making progress
    if (length(common.edges) < 2 || length(common.edges) >= length(from.edges))
        return (NULL)
    
    label.suffix = sub("^[0-9+-]+", "", from.values[1])
    
    sapply(1:(length(common.edges)-1), function(i){
        lower = common.edges[i]
        upper = common.edges[i+1]
        
        if (is.infinite(upper))
            paste0(lower, "+", label.suffix)
        else
            paste0(lower, "-", upper-1, label.suffix)
    })
}
