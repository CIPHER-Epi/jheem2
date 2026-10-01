
# depends on:
#   SPECIFICATION_transformations.R
#   HELPERS_dim_names_helpers.R
#   HELPERS_misc_helpers.R
#   stats packages

##----------------------##
##----------------------##
##-- PUBLIC INTERFACE --##
##----------------------##
##----------------------##

##----------------------------##
##-- STATIC FUNCTIONAL FORM --##
##----------------------------##

#'@title Create an Static (Time-Invariant) Functional Form
#'
#'@param value array representing the value
#'@param link The name of the transformation to the scale on which alphas are added to the value. One of "identity", "log", "logit"
#'@param min,max The minimum and maximum values this functional form can create. The default (NA) sets to the min/max for the link specified (eg, 0-Inf for a log link, 0-1 for a logistic link)
#'@param value.is.on.transformed.scale Logical indicating whether the value is already on the transformed scale at which alphas apply
#'@param overwrite.parameters.with.alphas Logical indicating whether, when alphas are applied, they OVERWRITE values. If F, alphas are added to the transformed value (on the transformed scale)
#'@param error.prefix String to prepend to any error message in case of an error
#'
#'@export
create.static.functional.form <- function(value,
                                          link=c('identity','log','logit')[1],
                                          min = NA,
                                          max = NA,
                                          value.name='value',
                                          value.is.on.transformed.scale = F,
                                          overwrite.parameters.with.alphas = F,
                                          error.prefix = 'Cannot create static functional.form: ')
{
    STATIC.FUNCTIONAL.FORM$new(value = value,
                               link = link,
                               min = min,
                               max = max,
                               value.name = value.name,
                               value.is.on.transformed.scale = value.is.on.transformed.scale,
                               overwrite.parameters.with.alphas = overwrite.parameters.with.alphas,
                               error.prefix = error.prefix)
}


##-------------------------------------------------##
##-- LINEAR (INTERCEPT + SLOPE) FUNCTIONAL FORMS --##
##-------------------------------------------------##


#'@title Create an Functional Form that is Linear (possibly on a transformed scale)
#'
#'@inheritParams create.static.functional.form
#'@param intercept,slope Arrays or scalar values representing the intercept and slope on the transformed scale
#'@param parameters.is.on.transformed.scale Logical indicating whether the intercept and slope are already on the transformed scale at which alphas apply
#'@param anchor.year The year at which the functional form evaluates to the intercept
#'@param intercept.robust.intercept.linkto.negative,slope.robust.to.negative Whether intercept and slope should be allowed to be negative, even if a log or logit link is used (this is handled by letting the the alphas apply to the negative of the parameter, then negating it)
#'@param intercept.link,slope.link The names of the transformations to the scale on which alphas are added to calculate the intercept and slope. One of "identity", "log", "logit". If NULL, inferred automatically from the link for the entire functional form
#'@param intercept.min,intercept.max,slope.min,slope.max The min and max values to which intercept and slope can evaluate
#'
#'@seealso create.static.functional.form, create.log.linear.functional.form, create.logistic.linear.functional.form
#'
#'@export
create.linear.functional.form <- function(intercept,
                                          slope,
                                          anchor.year,
                                          link = c('identity','log','logit')[1],
                                          min = NA,
                                          max = NA,
                                          
                                          intercept.link = NULL,
                                          intercept.min = NA,
                                          intercept.max = NA,
                                          intercept.robust.to.negative = F,
                                          
                                          slope.link = intercept.link,
                                          slope.min = NA,
                                          slope.max = NA,
                                          slope.robust.to.negative = F,
                                          
                                          parameters.are.on.transformed.scale = T,
                                          overwrite.parameters.with.alphas = F,
                                          error.prefix = 'Cannot create linear functional.form: ')
{
    LINEAR.FUNCTIONAL.FORM$new(intercept = intercept,
                               slope = slope,
                               anchor.year = anchor.year,
                               link = link,
                               min = min,
                               max = max,
                               
                               intercept.link = intercept.link,
                               intercept.min = intercept.min,
                               intercept.max = intercept.max,
                               intercept.robust.to.negative = intercept.robust.to.negative,
                               
                               slope.link = slope.link,
                               slope.min = slope.min,
                               slope.max = slope.max,
                               slope.robust.to.negative = slope.robust.to.negative,
                               
                               parameters.are.on.transformed.scale = parameters.are.on.transformed.scale,
                               overwrite.parameters.with.alphas = overwrite.parameters.with.alphas,
                               error.prefix = error.prefix)
}

#'@title Create a Functional Form that is Linear on the Log Scale
#'
#'@inheritParams create.linear.functional.form
#'@param intercept,slope Arrays or scalar values representing the intercept and slope on the log scale
#'@param parameters.are.on.log.scale Logical indicating whether the intercept and slope are already on the log scale
#'
#'@seealso create.static.functional.form, create.linear.functional.form, create.logistic.linear.functional.form
#'
#'@export
create.log.linear.functional.form <- function(intercept,
                                              slope,
                                              anchor.year,
                                              min = 0,
                                              max = Inf,
                                              parameters.are.on.log.scale = T,
                                              overwrite.parameters.with.alphas=F,
                                              error.prefix = 'Cannot create log-linear functional.form: ')
{
    create.linear.functional.form(intercept = intercept,
                                  slope = slope,
                                  anchor.year = anchor.year,
                                  link = 'log',
                                  min = min,
                                  max = max,
                                  parameters.are.on.transformed.scale = parameters.are.on.log.scale,
                                  overwrite.parameters.with.alphas = overwrite.parameters.with.alphas,
                                  error.prefix = error.prefix)
}

#'@title Create a Functional Form that is Linear on the Logistic Scale
#'
#'@inheritParams create.linear.functional.form
#'@param intercept,slope Arrays or scalar values representing the intercept and slope on the logistic scale
#'@param parameters.are.on.logit.scale Logical indicating whether the intercept and slope are already on the logistic scale
#'
#'@seealso create.static.functional.form, create.linear.functional.form, create.log.linear.functional.form
#'
#'@export
create.logistic.linear.functional.form <- function(intercept,
                                                   slope,
                                                   anchor.year,
                                                   min=0,
                                                   max=1,
                                                   parameters.are.on.logit.scale = T,
                                                   overwrite.parameters.with.alphas = F,
                                                   error.prefix = 'Cannot create logistic-linear functional.form: ')
{
    create.linear.functional.form(intercept = intercept,
                                  slope = slope,
                                  anchor.year = anchor.year,
                                  link = 'logit',
                                  min = min,
                                  max = max,
                                  parameters.are.on.transformed.scale = parameters.are.on.logit.scale,
                                  overwrite.parameters.with.alphas = overwrite.parameters.with.alphas,
                                  error.prefix = error.prefix)
}


#'@title Create a "Logistic Tail" Functional Form
#'
#'@details A "logistic tail" functional form follows a linear (intercept + slope) in its first part, and a logistic-linear form in its second part. This allows a linear trend to taper off
#'
#'@inheritParams create.linear.functional.form
#'@param intercept,slope Scalar values or arrays representing the intercept and slope. Note that the final form constrains the slope to be non-negative
#'@param logistic.after.frac.of.span The fraction of the the total span (max - min) after which the model follows a logistic curve
#'@param min,max The upper and lower limits to which the functional form can evaluate
#'@param intercept.link,slope.link The names of the transformations to the scale on which alphas are added to calculate the intercept and slope. One of "identity", "log", "logit"
#'@param intercept.min,intercept.max,slope.min,slope.max The min and max values to which intercept and slope can evaluate
#'
#'@export
create.logistic.tail.functional.form <- function(intercept,
                                                 slope,
                                                 anchor.year,
                                                 
                                                 logistic.after.frac.of.span=0.5,
                                                 min=0,
                                                 max=1,
                                                 
                                                 intercept.link = 'logistic',
                                                 intercept.min = NA,
                                                 intercept.max = NA,
                                                 intercept.robust.to.negative = F,
                                                 
                                                 slope.link = intercept.link,
                                                 slope.min = NA,
                                                 slope.max = NA,
                                                 slope.robust.to.negative = F,

                                                 parameters.are.on.transformed.scale = F,
                                                 overwrite.parameters.with.alphas = F,
                                                 error.prefix = 'Cannot create logistic-tail functional.form: ')
{
    LOGISTIC.TAIL.FUNCTIONAL.FORM$new(intercept = intercept,
                                      slope = slope,
                                      anchor.year = anchor.year,
                                      
                                      logistic.after.frac.of.span = logistic.after.frac.of.span,
                                      min = min,
                                      max = max,
                                      
                                      intercept.link = intercept.link,
                                      intercept.min = intercept.min,
                                      intercept.max = intercept.max,
                                      intercept.robust.to.negative = intercept.robust.to.negative,
                                      
                                      slope.link = slope.link,
                                      slope.min = slope.min,
                                      slope.max = slope.max,
                                      slope.robust.to.negative = slope.robust.to.negative,
                                      
                                      parameters.are.on.transformed.scale = parameters.are.on.transformed.scale,
                                      overwrite.parameters.with.alphas = overwrite.parameters.with.alphas,
                                      error.prefix = error.prefix)
}


##-----------------------------##
##-- SPLINE FUNCTIONAL FORMS --##
##-----------------------------##


#'@title Create a Linear Spline Functional Form
#'
#'@inheritParams create.linear.functional.form
#'@param knot.times The (numeric) times at which the knots of the spline apply. Must have names set
#'@param knot.values Initial values for the knots. Must have names set, matching the names of knot times
#'@param link The name of a transformation to the scale at which the SPLINE should apply. The knots are transformed to this scale, the spline is applied, and then the splined values are back-transformed. One of 'identity', 'log', or 'logistic'
#'
#'@param knot.link The name of a transformation to the scale at which the knot values have alphas ADDED. The knots are back-transformed from this scale prior to the spline scale being applied
#'@param knot.min,knot.max The min and max values that KNOTS can take before being splined (note, the min and max values the functional form can take, after splining, are determined by parameters min and max)
#'@param knots.are.on.transformed.scale Logical indicating whether the knots are already on the transformed scale (ie knot.link has been applied)
#'
#'@param before.modifier,after.modifier If either of these is set, an additional knot is added to the specified knots. For before.modifier, this knot is calculated as before.modifier added or multiplier by the value of the first knot (on the model scale); after.modifier is added or multiplied by the value of the last knot
#'@param before.time,after.time The times at which the knots calculated from before.modifier and after.modifier apply, respectively
#'
#'@param modifiers.apply.to.change If TRUE, after.modifier (the untransformed-by-link value) is multiplied by the change between the penultimate and last knots and added to the last knot to generate an additional knot at after.time (similarly, before.modifier is multiplied by the change from the first to the second and subtracted from the first). If FALSE, the transformed after.multiplier is added to the last knot after the last knot is transformed to modifier.link scale, and the resulting value is back-transformed to yield the knot at after.time (similarly, before.modifier is added to the first knot on modifier.link scale)
#'@param modifier.link The name of a transformation to the scale at which the before.modifer and after.modifier have alphas added
#'@param modifier.min,modifier.max The min and max values for before.modifier and after.modifier
#'@param overwrite.modifiers.with.alphas Logical indicating whether, when alphas are applied to the before.modifier or after.modifier, they OVERWRITE the modifiers. If F, alphas are added on the scale specified by modifier.link
#'
#'@export
create.linear.spline.functional.form <- function(knot.times,
                                                 knot.values,
                                                 
                                                 link = c('identity','log','logit')[1],
                                                 min = NA,
                                                 max = NA,
                                                 
                                                 knot.link = link,
                                                 knot.min = min,
                                                 knot.max = max,
                                                 
                                                 knots.are.on.transformed.scale=F,
                                                 overwrite.knot.values.with.alphas=F,
                                                 
                                                 before.time=NULL,
                                                 before.modifier=NULL,
                                                 after.time=NULL,
                                                 after.modifier=NULL,
                                                 
                                                 modifiers.apply.to.change=T,
                                                 modifier.link = link,
                                                 after.modifier.increasing.change.link = modifier.link,
                                                 after.modifier.decreasing.change.link = modifier.link,
                                                 before.modifier.increasing.change.link = modifier.link,
                                                 before.modifier.decreasing.change.link = modifier.link,
                                                 modifier.min = min,
                                                 modifier.max = max,
                                                 overwrite.modifiers.with.alphas = F,
                                                 
                                                 error.prefix = 'Cannot create linear spline functional.form: ')
{
    LINEAR.SPLINE.FUNCTIONAL.FORM$new(knot.times = knot.times,
                                      knot.values = knot.values,
                                      
                                      link = link,
                                      min = min,
                                      max = max,
                                      
                                      knot.link = knot.link,
                                      knot.min = knot.min,
                                      knot.max = knot.max,
                                      
                                      knots.are.on.transformed.scale = knots.are.on.transformed.scale,
                                      overwrite.knot.values.with.alphas = overwrite.knot.values.with.alphas,
                                      
                                      before.time = before.time,
                                      before.modifier = before.modifier,
                                      after.time = after.time,
                                      after.modifier = after.modifier,
                                      
                                      modifiers.apply.to.change = modifiers.apply.to.change,
                                      modifier.link = modifier.link,
                                      after.modifier.increasing.change.link = after.modifier.increasing.change.link,
                                      after.modifier.decreasing.change.link = after.modifier.decreasing.change.link,
                                      before.modifier.increasing.change.link = before.modifier.increasing.change.link,
                                      before.modifier.decreasing.change.link = before.modifier.decreasing.change.link,
                                      modifier.min = modifier.min,
                                      modifier.max = modifier.max,
                                      overwrite.modifiers.with.alphas = overwrite.modifiers.with.alphas,
                                      
                                      error.prefix = error.prefix)
}


#'@title Create a Natural Spline Functional Form
#'
#'@inheritParams create.linear.spline.functional.form
#'
#'@export
create.natural.spline.functional.form <- function(knot.times,
                                                  knot.values,
                                                  
                                                  link = c('identity','log','logit')[1],
                                                  min = NA,
                                                  max = NA,
                                                  
                                                  knot.link = link,
                                                  knot.min = min,
                                                  knot.max = max,
                                                  
                                                  knots.are.on.transformed.scale=F,
                                                  overwrite.knot.values.with.alphas=F,
                                                  
                                                  before.time=NULL,
                                                  before.modifier=NULL,
                                                  after.time=NULL,
                                                  after.modifier=NULL,
                                                  
                                                  modifiers.apply.to.change=T,
                                                  modifier.link = link,
                                                  after.modifier.increasing.change.link = modifier.link,
                                                  after.modifier.decreasing.change.link = modifier.link,
                                                  before.modifier.increasing.change.link = modifier.link,
                                                  before.modifier.decreasing.change.link = modifier.link,
                                                  modifier.min = min,
                                                  modifier.max = max,
                                                  overwrite.modifiers.with.alphas = F,
                                                  
                                                  error.prefix = 'Cannot create natural spline functional.form: ')
{
    NATURAL.SPLINE.FUNCTIONAL.FORM$new(knot.times = knot.times,
                                       knot.values = knot.values,
                                       
                                       link = link,
                                       min = min,
                                       max = max,
                                       
                                       knot.link = knot.link,
                                       knot.min = knot.min,
                                       knot.max = knot.max,
                                       
                                       knots.are.on.transformed.scale = knots.are.on.transformed.scale,
                                       overwrite.knot.values.with.alphas = overwrite.knot.values.with.alphas,
                                       
                                       before.time = before.time,
                                       before.modifier = before.modifier,
                                       after.time = after.time,
                                       after.modifier = after.modifier,
                                       
                                       modifiers.apply.to.change = modifiers.apply.to.change,
                                       modifier.link = modifier.link,
                                       after.modifier.increasing.change.link = after.modifier.increasing.change.link,
                                       after.modifier.decreasing.change.link = after.modifier.decreasing.change.link,
                                       before.modifier.increasing.change.link = before.modifier.increasing.change.link,
                                       before.modifier.decreasing.change.link = before.modifier.decreasing.change.link,
                                       modifier.min = modifier.min,
                                       modifier.max = modifier.max,
                                       overwrite.modifiers.with.alphas = overwrite.modifiers.with.alphas,
                                       
                                       error.prefix = error.prefix)
}



#'@title Create a Logistic Spline Functional Form
#'
#'@inheritParams create.linear.spline.functional.form
#'
#'@param fraction.of.asymptote.after.end,fraction.of.asymptote.before.start The fraction of the total change in the first or last segment that would take place before the first knot or after the last
#'@param fraction.of.asymptote.for.change.dir The fraction of the total change in a segment that should take place before between two segments
#'@param fraction.asymptote.link The name of a transformation to the scale at which alphas are added to the betas for the fraction.asymptote.<x> values
#'@param overwrite.fraction.asymptote.with.alphas Logical indicating whether, when alphas are applied to the fraction.of.asymptote.<x>, they OVERWRITE the values. If F, alphas are interpreted as either additive or multiplicative to the knot.values, depending on the value of fraction.asymptote.alphas.are.multipliers
#'
#'@export
create.logistic.spline.functional.form <- function(knot.times,
                                                   knot.values,
                                                   
                                                   min = -Inf,
                                                   max = Inf,
                                                   
                                                   knot.link = c('identity','log','logit')[2],
                                                   knot.min = NA,
                                                   knot.max = NA,
                                                   
                                                   knots.are.on.transformed.scale = F,
                                                   overwrite.knot.values.with.alphas = F,
                                                   
                                                   fraction.of.asymptote.after.end=0.05,
                                                   fraction.of.asymptote.before.start=0.025,
                                                   fraction.of.asymptote.for.change.dir=0.02,
                                                   fraction.asymptote.link = 'log',
                                                   overwrite.fraction.asymptote.with.alphas = F,
                                                   
                                                   error.prefix = 'Cannot create logistic spline functional.form: '
)
{
    LOGISTIC.SPLINE.FUNCTIONAL.FORM$new(knot.times = knot.times,
                                        knot.values = knot.values,
                                        
                                        min = min,
                                        max = max,
                                        
                                        knot.link = knot.link,
                                        knot.min = knot.min,
                                        knot.max = knot.max,
                                        
                                        knots.are.on.transformed.scale = knots.are.on.transformed.scale,
                                        overwrite.knot.values.with.alphas = overwrite.knot.values.with.alphas,
                                        
                                        fraction.of.asymptote.after.end=fraction.of.asymptote.after.end,
                                        fraction.of.asymptote.before.start=fraction.of.asymptote.before.start,
                                        fraction.of.asymptote.for.change.dir=fraction.of.asymptote.for.change.dir,
                                        fraction.asymptote.link = fraction.asymptote.link,
                                        overwrite.fraction.asymptote.with.alphas = overwrite.fraction.asymptote.with.alphas,
                                        
                                        error.prefix = error.prefix)
}

create.logistic.taper.functional.form <- function(base.functional.form,
                                                  taper.start.year,
                                                  p.at.taper.start,
                                                  taper.year.2,
                                                  p.at.year.2,
                                                  ps.are.dynamic = F,
                                                  link = 'identity',
                                                  error.prefix = 'Cannot create logistic taper functional.form: ')
{
    LOGISTIC.TAPER.FUNCTIONAL.FORM$new(base.functional.form = base.functional.form,
                                       taper.start.year = taper.start.year,
                                       p.at.taper.start = p.at.taper.start,
                                       taper.year.2 = taper.year.2,
                                       p.at.year.2 = p.at.year.2,
                                       ps.are.dynamic = ps.are.dynamic,
                                       link = link,
                                       error.prefix = error.prefix)
}

##---------------------------------------##
##---------------------------------------##
##-- FUNCTIONAL FORM CLASS DEFINITIONS --##
##---------------------------------------##
##---------------------------------------##


#'@export
FUNCTIONAL.FORM = R6::R6Class(
    'functional.form',
    
    public = list(
        
        #'@param type A single character name of the type of functional form
        #'@param betas A named list of the values for the betas
        #'@param link A link object
        #'@param alphas.are.additive Either a single logical value or a named logical vector with the same names as betas
        #'@param beta.links,alpha.links Either a single link object or a named list of link objects with the same names as betas
        #'@param is.static A single logical value
        initialize = function(type,
                              betas,
                              link,
                              future.slope.link,
                              alphas.are.additive, #either a single logical value or a named logical vector with the same names as betas
                              beta.links, #either a single value ('identity','log','logit','custom') or named character vector with the same names as betas
                              negate.beta.masks = NULL, #either a single vlaue or a named list with the same names as betas. Each element is either NULL or a logical vector
                              alpha.links, #either a single value ('identity','log','logit','custom') or named character vector with the same names as betas
                              is.static,
                              error.prefix='') #whether the model is time-varying
        {
            #-- Check Type --#
            if (!is.character(type) || length(type)!=1 || is.na(type) || nchar(type)==0)
                stop(paste0(error.prefix, "A functional.form's 'type' must be a non-empty, non-NA single character value"))
            
            #-- Check link --#
            if (!is(link, 'link'))
                stop(paste0(error.prefix, "A functional.form's 'link' must be a an object of class 'link' (obtained by calling 'get.link')"))
            
            # update type
            if (link$type != 'identity')
                type = paste0(link$type, " ", type)

                        
            #-- Check future.slope.link --#
            if (!is.null(future.slope.link) && !is(future.slope.link, 'link'))
                stop(paste0(error.prefix, "A functional.form's 'future.slope.link' must be either NULL or a an object of class 'link' (obtained by calling 'get.link')"))

            #-- Check Betas --#
            if (!is.list(betas) || (length(betas)>0 && (is.null(names(betas)) || any(names(betas)==''))))
                stop("'betas' must be a named list")
            sapply(names(betas), function(beta.name){
                beta = betas[[beta.name]]
                if (!is.numeric(beta) || length(beta)==0 || any(is.na(beta)))
                    stop(paste0(error.prefix, "'", beta.name, "' must be a non-NA, non-empty, numeric object"))
                T
            })
            
            alpha.names = names(betas)
            if (length(alpha.names)>0 && max(table(alpha.names)>1))
                stop(paste0(error.prefix, "Functional.form 'betas' cannot have repeated names"))
            
            #-- Make the dim.names --#
            dim.names = NULL
            for (i in seq_along(betas))
            {
                beta.name = names(betas)[i]
                beta = betas[[i]]
                beta.dim.names = dimnames(beta)
                if (is.null(beta.dim.names))
                {
                    if (length(beta) != 1)
                        stop(paste0("'", beta.name, 
                                    "' must either be a scalar (single) numeric value or have dim.names set"))
                }
                else
                {
                    if (is.null(names(beta.dim.names)))
                        stop(paste0("The dimnames of '", beta.name, 
                                    "' must be named"))
                    
                    if (!are.dim.names.compatible(dim.names, beta.dim.names))
                        stop(paste0("The dimension names for '", beta.name, 
                                    "' are not compatible with the dimension.names for ",
                                    paste0("'", names(betas)[1:(i-1)], "'", collapse=', ')))
                    
                    dim.names = union.matching.dim.names(dim.names, beta.dim.names)
                }
            }
            
            #-- Check alphas.are.additive --#
            if (!is.logical(alphas.are.additive) || length(alphas.are.additive)==0 || any(is.na(alphas.are.additive)))
                stop(paste0(error.prefix,
                            "'alphas.are.additive' for a functional.form ('",
                            type, "') must be either an unnamed logical scalar or a named logical vector"))
            
            if (length(alphas.are.additive)==1)
            {
                if (!is.null(names(alphas.are.additive)) && length(betas)>1)
                    stop(paste0(error.prefix,
                                "'alphas.are.additive' for a functional.form ('",
                                type, "') must be either an unnamed logical scalar or a named logical vector with the same names as 'betas'"))
                
                alphas.are.additive = rep(alphas.are.additive, length(betas))
                names(alphas.are.additive) = alpha.names
            }
            else
            {
                if (length(alphas.are.additive) != length(betas) || 
                    !setequal(names(alphas.are.additive), alpha.names))
                    stop(paste0(error.prefix,
                                "'alphas.are.additive' for a functional.form ('",
                                type, "') must be either an unnamed logical scalar or a named logical vector with the same names as 'betas'"))
                
                alphas.are.additive = alphas.are.additive[alpha.names]
            }
            
            #-- Check beta.links --#
            if (is(beta.links, 'link'))
                beta.links = list(beta.links)
            
            if (!is.list(beta.links) || (length(beta.links)==0 && length(betas)>0))
                stop(paste0(error.prefix, "'beta.links' for a functional.form ('",
                            type, "') must be either a single link object, or a list containing only link objects"))
            
            if (any(!sapply(beta.links, is, 'link')))
                stop(paste0(error.prefix, "If 'beta.links' for a functional.form ('",
                            type, "') is a list, it may contain ONLY link objects"))
            
            if (length(beta.links)==1)
            {
                if (!is.null(names(beta.links)) && length(betas)>1)
                    stop(paste0("'beta.links' for a functional.form ('",
                                type, "') must be either a link object or a named list of link objects with the same names as 'betas'"))
                
                beta.links = lapply(1:length(betas), function(i){beta.links[[1]]})
                names(beta.links) = alpha.names
            }
            else
            {
                if (length(beta.links) != length(betas) || 
                    !setequal(names(beta.links), alpha.names))
                    stop(paste0(error.prefix, "'beta.links' for a functional.form ('",
                                type, "') must be either a link object or a named list of link objects with the same names as 'betas'"))
                
                beta.links = beta.links[alpha.names]
            }
            
            #-- Check negate.beta.masks --#
            if (is.null(negate.beta.masks) || is(negate.beta.masks, 'logical'))
                negate.beta.masks = list(negate.beta.masks)
            
            if (!is.list(negate.beta.masks) || length(negate.beta.masks)==0)
                stop(paste0(error.prefix, "'negate.beta.masks' for a functional.form ('",
                            type, "') must be either a single NULL value or logical vector, or a list containing only NULL and/or logical vectors"))
            
            if (any(!sapply(negate.beta.masks, is.null) & !sapply(negate.beta.masks, is.logical)))
                stop(paste0(error.prefix, "If 'negate.beta.masks' for a functional.form ('",
                            type, "') is a list, it may contain ONLY NULL and/or logical vector values"))
            
            if (length(negate.beta.masks)==1)
            {
                if (!is.null(names(negate.beta.masks)) && length(betas)>1)
                    stop(paste0("'negate.beta.masks' for a functional.form ('",
                                type, "') must be either a single NULL value or logical vector or a named list of link objects with the same names as 'betas'"))
                
                negate.beta.masks = lapply(1:length(betas), function(i){negate.beta.masks[[1]]})
                names(negate.beta.masks) = alpha.names
            }
            else
            {
                if (length(negate.beta.masks) != length(betas) || 
                    !setequal(names(negate.beta.masks), alpha.names))
                    stop(paste0(error.prefix, "'negate.beta.masks' for a functional.form ('",
                                type, "') must be either NULL or a logical vector or a named list of NULL values and/or logical vectors with the same names as 'betas'"))
                
                negate.beta.masks = negate.beta.masks[alpha.names]
            }
            
            sapply(alpha.names, function(name){
                if (!is.null(negate.beta.masks[[name]]) && (!is.null(dimnames(betas[[name]])) || !is.null(dimnames(negate.beta.masks[[name]]))) &&
                    !dim.names.equal(dimnames(betas[[name]]), dimnames(negate.beta.masks[[name]])))
                    stop(paste0(error.prefix, "The values of 'negate.beta.masks' must have the same dimnames as the corresponding beta values"))
                T
            })
            
            #-- Check alpha.links --#
            if (is(alpha.links, 'link'))
                alpha.links = list(alpha.links)
            
            if (!is.list(alpha.links) || (length(alpha.links)==0 && length(betas)>0))
                stop(paste0(error.prefix, "'alpha.links' for a functional.form ('",
                            type, "') must be either a single link object, or a list containing only link objects"))
            
            if (any(!sapply(alpha.links, is, 'link')))
                stop(paste0(error.prefix, "If 'alpha.links' for a functional.form ('",
                            type, "') is a list, it may contain ONLY link objects"))

            if (length(alpha.links)==1)
            {
                if (!is.null(names(alpha.links)) && length(betas)>1)
                    stop(paste0("'alpha.links' for a functional.form ('",
                                type, "') must be either a link object or a named list of link objects with the same names as 'betas'"))
                
                alpha.links = lapply(1:length(betas), function(i){alpha.links[[1]]})
                names(alpha.links) = alpha.names
            }
            else
            {
                if (length(alpha.links) != length(betas) || 
                    !setequal(names(alpha.links), alpha.names))
                    stop(paste0(error.prefix, "'alpha.links' for a functional.form ('",
                                type, "') must be either a link object or a named list of link objects with the same names as 'betas'"))
                
                alpha.links = alpha.links[alpha.names]
            }
            
            #-- Check is.static --#
            if (!is.logical(is.static) || length(is.static)!=1 || is.na(is.static))
                stop(paste0(error.prefix, "'is.static' for a functional.form ('", 
                            type, "') must be a single, non-NA logical value"))
            
            #-- Store and Return --#
            
            private$i.type = type
            private$i.betas = betas
            private$i.minimum.dim.names = dim.names
            private$i.link = link
            private$i.future.slope.link = future.slope.link
            private$i.alphas.are.additive = alphas.are.additive
            private$i.beta.links = beta.links
            private$i.negate.beta.masks = negate.beta.masks
            private$i.alpha.links = alpha.links
            private$i.is.static = is.static
        },
        
        project = function(years,
                           alphas = NULL,
                           dim.names = self$minimum.dim.names,
                           future.slope = NULL,
                           future.slope.after.year = NULL,
                           future.slope.is.on.transformed.scale=F,
                           check.consistency=T,
                           error.prefix='')
        {
            #-- Check Arguments --#
            if (check.consistency)
            {
                # Check years
                if (length(years)==0 || !is.numeric(years) || any(is.na(years)))
                    stop(paste0(error.prefix, "years must be a non-empty, non-NA numeric vector"))
                if (!is.null(alphas) && length(alphas)>0 && 
                    (!is.list(alphas) || any(!sapply(alphas, is, class2='functional.form.alphas'))))
                    stop(paste0(error.prefix, "alphas must be a list of 'functional.form.alphas' objects"))
                
                # Check dim.names is a valid list
                if (length(dim.names)>0 && 
                    (!is.list(dim.names) || is.null(names(dim.names)) || any(is.na(names(dim.names))) ||
                     any(!sapply(dim.names, is.character)) || any(sapply(dim.names, length)==0) ||
                     any(sapply(dim.names, function(vals){any(is.na(vals))}))))
                    stop(paste0(error.prefix, "dim.names must be either NULL or a named list of non-NA character vectors"))
                    
                # Check that dim.names is sufficient
                if (!are.dim.names.compatible(dim.names.1=dim.names, dim.names.2=private$i.minimum.dim.names,
                                              match.order = F, dim.names.2.is.subset = T))
                    stop(paste0(error.prefix, "The given 'dim.names' are not compatible with the minimum dim.names for the functional form"))
                
                # Check future.slope and future.slope.after.year
                if (!is.null(future.slope))
                {
                    if (!is.numeric(future.slope) || length(future.slope)!=1 || is.na(future.slope))
                        stop("'future.slope' must be a single numeric value")
                    if (!is.numeric(future.slope.after.year) || length(future.slope.after.year)!=1 || is.na(future.slope.after.year))
                        stop("'future.slope.after.year' must be a single numeric value")
                }
                
                # Check alphas
                invalid.alphas = setdiff(names(alphas), self$alpha.names)
                if (length(invalid.alphas)>0)
                    stop(paste0(error.prefix,
                                "Invalid alpha(s) for functional.form of type '", private$i.type, "': ",
                                paste0("'", invalid.alphas, "'", collapse=', ')))
            }
            
            #-- Transform future.slope --#
            if (is.null(private$i.future.slope.link))
            {
                if (!is.null(future.slope) && future.slope != 0)
                    stop(paste0(error.prefix, private$i.type, " functional.form cannot accomodate 'future.slope' - it must be set to NULL or 0"))
                
                future.slope = 0
                future.slope.after.year = Inf
            }
            else
            {
                if (is.null(future.slope))
                {
                    future.slope = 0
                    future.slope.after.year = Inf
                }
                else
                {
                    if (!future.slope.is.on.transformed.scale)
                    {
                        if (check.consistency)
                            private$i.future.slope.link$check.untransformed.values(future.slope,
                                                                                   variable.name.for.error = 'future.slope',
                                                                                   error.prefix = error.prefix)
                        future.slope = private$i.future.slope.link$apply(future.slope)
                    }
                    
                    if (is.null(future.slope.after.year))
                        stop(paste0(error.prefix, "'future.slope.after.year' must be set in order to project from functional.form"))
                }
            }
            
            #-- Incorporate Alphas --#
            terms = lapply(names(private$i.betas), function(name){
                rv = private$i.beta.links[[name]]$reverse.apply(
                    incorporate.alphas(betas=private$i.betas[[name]],
                                       alphas=alphas[[name]],
                                       target.dim.names=dim.names,
                                       error.prefix=error.prefix))
                
                if (!is.null(private$i.negate.beta.masks[[name]]))
                {
                    negate.mask = private$i.negate.beta.masks[[name]][ alphas[[name]]$crunched$expand.beta.indices ]
                    rv[negate.mask] = -rv[negate.mask]
                }
                
                rv
            })
            names(terms) = names(private$i.betas)
            
            if (check.consistency)
            {
                na.mask = sapply(terms, function(term){
                    any(is.na(term))
                })
                
                if (any(na.mask))
                    stop(paste0(error.prefix,
                                "Generated NA values in ",
                                ifelse(sum(na.mask)==1, 'term ', 'terms '),
                                collapse.with.and("'", names(terms)[na.mask], "'"),
                                " for the ", private$i.type, " functional.form"))
            }
            
            #-- Call the sub-function --#

            rv = private$do.project(terms = terms,
                                    years = years,
                                    future.slope = future.slope,
                                    future.slope.after.year = future.slope.after.year,
                                    dim.names = dim.names,
                                    check.consistency = check.consistency,
                                    error.prefix = error.prefix)
            
            names(rv) = as.character(years)
            rv
        },
        
        project.static = function(alphas,
                                  dim.names,
                                  check.consistency=T,
                                  error.prefix='')
        {
            if (!private$i.is.static)
                stop(paste0(error.prefix, "Cannot project.static from a dynamic functional.form ('", private$i.type, "')"))
            
            rv = self$project(years = 0,
                              alphas = alphas,
                              future.slope = 0,
                              future.slope.after.year = 0,
                              dim.names = dim.names,
                              check.consistency = check.consistency,
                              error.prefix = error.prefix)[[1]]
            
            names(rv) = NULL
            rv
        },
        
        apply.ontology.mapping = function(mapping, 
                                           modify.in.place = F,
                                           error.prefix = error.prefix)
        {
            if (!modify.in.place)
            {
                rv = self$clone(deep=T)
                rv$apply.ontology.mapping(mapping, modify.in.place=T)
            }
            else
            {
                private$i.betas = lapply(private$i.betas, mapping$apply)
                private$i.minimum.dim.names = mapping$apply.to.dim.names(private$i.minimum.dim.names)
            }
        },
        
        # FOR DEBUGGING
        check = function()
        {
            browser()
        }
    ),
    
    active = list(
        
        type = function(value)
        {
            if (missing(value))
                private$i.type
            else
                stop("Cannot modify 'type' for a functional.form object - it is read-only")
        },
        
        is.static = function(value)
        {
            if (missing(value))
                private$i.is.static
            else
                stop("Cannot modify 'is.static' for a functional.form object - it is read-only")
        },
        
        minimum.dim.names = function(value)
        {
            if (missing(value))
                private$i.minimum.dim.names
            else
                stop("Cannot modify 'minimum.dim.names' for a functional.form object - they are read-only")
        },
        
        betas = function(value)
        {
            if (missing(value))
                private$i.betas
            else
                stop("Cannot modify 'betas' for a functional.form object - they are read-only")
        },
        
        link = function(value)
        {
            if (missing(value))
                private$i.link
            else
                stop("Cannot modify 'link' for a functional.form object - it is read-only")
        },
        
        alpha.names = function(value)
        {
            if (missing(value))
                names(private$i.betas)
            else
                stop("Cannot modify 'alpha.names' for a functional.form object - they are read-only")
        },
        
        alpha.links = function(value)
        {
            if (missing(value))
                private$i.alpha.links
            else
                stop("Cannot modify 'alpha.links' for a functional.form object - they are read-only")
        },
        
        alphas.are.additive = function(value)
        {
            if (missing(value))
                private$i.alphas.are.additive
            else
                stop("Cannot modify 'alphas.are.additive' for a functional.form object - they are read-only")
        }
    ),
    
    private = list(
        
        i.type = NULL,
        i.is.static = NULL,
        i.link = NULL,
        i.future.slope.link = NULL,
        
        i.minimum.dim.names = NULL,
        i.betas = NULL,
        
        i.beta.links = NULL,
        i.alpha.links = NULL,
        i.alphas.are.additive = NULL,
        i.negate.beta.masks = NULL,
        

        do.project = function(terms,
                              years,
                              future.slope,
                              future.slope.after.year,
                              dim.names,
                              check.consistency,
                              error.prefix)
        {
            stop(paste0(error.prefix,
                        "This subclass of 'functional.form' ('", private$i.type,
                        "') is incompletely specified. The private 'do.project' method must be implemented at the subclass level"))       
        }
    )
)

STATIC.FUNCTIONAL.FORM = R6::R6Class(
    'static.functional.form',
    inherit = FUNCTIONAL.FORM,
    
    public = list(
        
        #'@title Create an Static (Time-Invariant) Functional Form
        #'
        #'@param value array representing the value
        #'@param link The name of the transformation to the scale on which alphas are added to the value. One of "identity", "log", "logistic"
        #'@param min,max The minimum and maximum values this functional form can create. The default (NA) sets to the min/max for the link specified (eg, 0-Inf for a log link, 0-1 for a logistic link)
        #'@param value.is.on.transformed.scale Logical indicating whether the the value is already on the transformed scale at which alphas apply
        #'@param overwrite.parameters.with.alphas Logical indicating whether, when alphas are applied, they OVERWRITE values. If F, alphas are added to the transformed value (on the transformed scale)
        #'
        #'@export
        initialize = function(value,
                              link=c('identity','log','logistic')[1],
                              min = NA,
                              max = NA,
                              value.name='value',
                              value.is.on.transformed.scale = F,
                              overwrite.parameters.with.alphas = F,
                              error.prefix = 'Cannot create static functional.form: ')
        {
            error.prefix = 'Cannot create static functional.form: '
            
            #-- Set up link and alpha.link --#
            link = get.link(link, min=min, max=max, error.prefix=error.prefix)
            if (overwrite.parameters.with.alphas)
                alpha.link = link
            else
                alpha.link = link$get.coefficient.link()
            
            #-- Transform value if needed --#
            if (!value.is.on.transformed.scale)
            {
                link$check.untransformed.values(value, variable.name.for.error='value', error.prefix=error.prefix)
                value = link$apply(value)
            }
            
            #-- Set up names for beta(s) --#
            betas = list(value)
            names(betas) = value.name
            
            super$initialize(type = "static",
                             betas = betas,
                             link = link,
                             future.slope.link = link$get.coefficient.link(),
                             alphas.are.additive = !overwrite.parameters.with.alphas, #either a single logical value or a named logical vector with the same names as betas
                             beta.links = get.link('identity'),
                             alpha.links = alpha.link, #either a single value ('identity','log','logit','custom') or named character vector with the same names as betas
                             is.static = T,
                             error.prefix = error.prefix)
        }
        
    ),
    
    private = list(
        
        do.project = function(terms,
                              years,
                              future.slope,
                              future.slope.after.year,
                              dim.names,
                              check.consistency,
                              error.prefix)
        {
            value = private$i.link$reverse.apply(terms$value)

            lapply(years, function(year){
                value
            })       
        }
    )
)

LINEAR.FUNCTIONAL.FORM = R6::R6Class(
    'linear.functional.form',
    inherit = FUNCTIONAL.FORM,
    
    public = list(
        
        initialize = function(intercept,
                              slope,
                              anchor.year,
                              link=c('identity','log','logistic')[1],
                              min = NA,
                              max = NA,
                              intercept.link = NULL,
                              intercept.min = NA,
                              intercept.max = NA,
                              intercept.robust.to.negative = F,
                              slope.link = NULL,
                              slope.min = NA,
                              slope.max = NA,
                              slope.robust.to.negative = F,
                              parameters.are.on.transformed.scale = T,
                              overwrite.parameters.with.alphas = F,
                              error.prefix = 'Cannot create linear functional.form: ')
        {
            
            #-- Set up link --#
            link = get.link(link, min=min, max=max, error.prefix=error.prefix)
            
            #-- Check anchor.year --#
            if (length(anchor.year)!=1 || !is.numeric(anchor.year) || is.na(anchor.year))
                stop(paste0(error.prefix, "'anchor.year' must be a single, non-NA numeric value"))
            
            #-- Check overwrite.parameters.with.alphas --#
            if (!is.logical(overwrite.parameters.with.alphas) || length(overwrite.parameters.with.alphas)!=1 || is.na(overwrite.parameters.with.alphas))
                stop(paste0(error.prefix, "'overwrite.parameters.with.alphas' must be a single, non-NA logical value"))
            
            #-- Set up alpha.links --#
            beta.links = list()
            alpha.links = list()
            if (is.null(intercept.link))
            {
                beta.links$intercept = get.link('identity')
                if (overwrite.parameters.with.alphas)
                {
                    intercept.link = link
                }
                else
                {
                    intercept.link = link$get.coefficient.link()
                }
            }
            else
            {
                intercept.link = get.link(intercept.link, min=intercept.min, max=intercept.max)
                beta.links$intercept = intercept.link
            }
            
            if (is.null(slope.link))
            {
                beta.links$slope = get.link('identity')
                if (overwrite.parameters.with.alphas)
                {
                    slope.link = link$get.coefficient.link()
                }
                else
                {
                    slope.link = link$get.coefficient.link()
                }
            }
            else
            {
                slope.link = get.link(slope.link, min=slope.min, max=slope.max)
                beta.links$slope = slope.link
            }
            
            parameter.links = list(intercept = intercept.link,
                                   slope = slope.link)
            
            #-- Set up alpha.links --#
            alpha.links = list(intercept=intercept.link, slope=slope.link)
            if (overwrite.parameters.with.alphas)
                alpha.links = lapply(alpha.links, function(l){l$get.coefficient.link()})
            
            #-- Check Intercept and Slope --#
            if (!parameters.are.on.transformed.scale)
            {
                # Check intercept
                if (intercept.robust.to.negative && link$min==0)
                {
                    intercept.negate.mask = intercept < 0
                    if (any(intercept.negate.mask))
                        intercept[intercept.negate.mask] = -intercept[intercept.negate.mask]
                    else
                        intercept.negate.mask = NULL
                }
                else
                {
                    link$check.untransformed.values(intercept, variable.name.for.error = 'intercept', error.prefix=error.prefix)
                    intercept.negate.mask = NULL
                }
                
                intercept = link$apply(intercept)
                
                # Check slope
                slope.link = alpha.links$slope
                if (slope.robust.to.negative && slope.link$min==0)
                {
                    slope.negate.mask = intercept < 0
                    if (any(slope.negate.mask))
                        slope[slope.negate.mask] = -slope[slope.negate.mask]
                    else
                        slope.negate.mask = NULL
                }
                else
                {
                    slope.link$check.untransformed.values(slope, variable.name.for.error = 'slope', error.prefix=error.prefix)
                    slope.negate.mask = NULL
                }
                
                slope = slope.link$apply(slope)
                
                negate.beta.masks = list(intercept = intercept.negate.mask,
                                         slope = slope.negate.mask)
            }
            else
                negate.beta.masks = NULL
            
            betas = list(intercept = intercept,
                         slope = slope)
            
            #-- Call the superclass constructor --#
            super$initialize(type = "linear",
                             betas = betas,
                             link = link,
                             future.slope.link = link$get.coefficient.link(),
                             alphas.are.additive = !overwrite.parameters.with.alphas,
                             beta.links = beta.links,
                             negate.beta.masks = negate.beta.masks,
                             alpha.links = alpha.links, 
                             is.static = F,
                             error.prefix = error.prefix)
            
            private$i.anchor.year = anchor.year
        }
    ),
    
    active = list(
      
        anchor.year = function(value)
        {
            if (missing(value))
                private$i.anchor.year
            else
                stop("Cannot modify 'anchor.year' for a functional.form object - it is read-only")
        }
          
    ),
    
    private = list(
        i.anchor.year = NULL,
        
        do.project = function(terms,
                              years,
                              future.slope,
                              future.slope.after.year,
                              dim.names,
                              check.consistency,
                              error.prefix)
        {
            intercept = terms$intercept
            slope = terms$slope
            
            rv = lapply(years, function(year){
                
                if (year==private$i.anchor.year)
                    x = intercept
                else
                {
                    x = intercept + 
                        slope * (year - private$i.anchor.year)
                }
                
                if (!is.null(future.slope) && year > future.slope.after.year)
                    x = x + future.slope * (year - future.slope.after.year)
                
                private$i.link$reverse.apply(x)
            })  
            
            rv
        }
    )
)

LOGISTIC.TAIL.FUNCTIONAL.FORM = R6::R6Class(
    'logistic.tail.functional.form',
    inherit = FUNCTIONAL.FORM,
    
    public = list(
        
        initialize = function(intercept,
                              slope,
                              anchor.year,
                              
                              logistic.after.frac.of.span=0.5,
                              min=0,
                              max=1,
                              
                              intercept.link = 'logit',
                              intercept.min = NA,
                              intercept.max = NA,
                              intercept.robust.to.negative = F,
                              
                              slope.link = 'log',
                              slope.min = NA,
                              slope.max = NA,
                              slope.robust.to.negative = F,
                              
                              parameters.are.on.transformed.scale = T,
                              overwrite.parameters.with.alphas = F,
                              error.prefix = 'Cannot create logistic-tail functional.form: ')
        {
            #-- Check min/max and get links --#
            link = get.link('identity', min, max, error.prefix = error.prefix)
            if (is.infinite(min))
                stop(paste0(error.prefix, "'min' must be finite"))
            if (is.infinite(max))
                stop(paste0(error.prefix, "'max' must be finite"))
            
            intercept.link = get.link(intercept.link, intercept.min, intercept.max, error.prefix = error.prefix)
            if (is.infinite(intercept.min))
                stop(paste0(error.prefix, "'intercept.min' must be finite"))
            if (is.infinite(intercept.max))
                stop(paste0(error.prefix, "'intercept.max' must be finite"))
            
            slope.link = get.link(slope.link, slope.min, slope.max, error.prefix = error.prefix)
            if (is.infinite(slope.min))
                stop(paste0(error.prefix, "'slope.min' must be finite"))
            if (is.infinite(slope.max))
                stop(paste0(error.prefix, "'slope.max' must be finite"))
            
            #-- Check logistic.after.frac.of.span --#
            if (!is.numeric(logistic.after.frac.of.span))
                stop("'logistic.after.frac.of.span' must be a single numeric value")
            if (length(logistic.after.frac.of.span)!=1)
                stop("'logistic.after.frac.of.span' must be a *SINGLE* numeric value")
            if (is.na(logistic.after.frac.of.span))
                stop("'logistic.after.frac.of.span' cannot be NA")
            if (logistic.after.frac.of.span<0 || logistic.after.frac.of.span>1)
                stop(paste0("'logistic.after.frac.of.span' (", logistic.after.frac.of.span, ") must be between 0 and 1"))
            
            #-- Check anchor.year --#
            if (length(anchor.year)!=1 || !is.numeric(anchor.year) || is.na(anchor.year))
                stop(paste0(error.prefix, "'anchor.year' must be a single, non-NA numeric value"))
            
            #-- Overwrite.parameters.with.alphas --#
            if (!is.logical(overwrite.parameters.with.alphas) || length(overwrite.parameters.with.alphas)!=1 || is.na(overwrite.parameters.with.alphas))
                stop("'overwrite.parameters.with.alphas' must be a single, non-NA logical value")
            
            #-- Betas --#
            if (!parameters.are.on.transformed.scale)
            {
                # Check intercept
                if (intercept.robust.to.negative && intercept.link$min==0)
                {
                    intercept.negate.mask = intercept < 0
                    if (any(intercept.negate.mask))
                        intercept[intercept.negate.mask] = -intercept[intercept.negate.mask]
                    else
                        intercept.negate.mask = NULL
                }
                else
                {
                    intercept.link$check.untransformed.values(intercept, variable.name.for.error = 'intercept', error.prefix=error.prefix)
                    intercept.negate.mask = NULL
                }
                
                intercept = intercept.link$apply(intercept)
                
                # Check slope
                if (slope.robust.to.negative && slope.link$min==0)
                {
                    slope.negate.mask = slope < 0
                    if (any(slope.negate.mask))
                        slope[slope.negate.mask] = -slope[slope.negate.mask]
                    else
                        slope.negate.mask = NULL
                }
                else
                {
                    slope.link$check.untransformed.values(slope, variable.name.for.error = 'slope', error.prefix=error.prefix)
                    slope.negate.mask = NULL
                }
                
                slope = slope.link$apply(slope)
                
                negate.beta.masks = list(intercept = intercept.negate.mask,
                                         slope = slope.negate.mask)
            }
            else
                negate.beta.masks = NULL

            betas = list(intercept = intercept, slope = slope)
            beta.links = list(intercept = intercept.link, slope = slope.link)
            
            #-- Set up alpha.links --#
            if (overwrite.parameters.with.alphas)
                alpha.links = beta.links
            else
                alpha.links = lapply(beta.links, function(l){l$get.coefficient.link()})
            
            #-- Call the super-class constructor --#
            
            super$initialize(type = "logistic tail",
                             betas = betas,
                             link = link,
                             future.slope.link = slope.link$get.coefficient.link(),
                             alphas.are.additive = !overwrite.parameters.with.alphas,
                             beta.links = beta.links,
                             negate.beta.masks = negate.beta.masks,
                             alpha.links = alpha.links, 
                             is.static = F,
                             error.prefix = error.prefix)
            
            private$i.span = max - min
            private$i.logistic.after.value = min + private$i.span * logistic.after.frac.of.span
            private$i.anchor.year = anchor.year
        }
        
    ),
    
    private = list(
        
        i.span = NULL,
        i.logistic.after.value = NULL,
        i.anchor.year = NULL,
        
        do.project = function(terms,
                              years,
                              future.slope,
                              future.slope.after.year,
                              dim.names,
                              check.consistency,
                              error.prefix)
        {
            if (future.slope.after.year==-Inf)
                stop(paste0(error.prefix, "For a logistic.tail functional form, future.slope.after.year cannot be -Inf"))
            
            slope = pmax(0, terms$slope)
            if (is.null(future.slope) || future.slope.after.year==Inf)
                slope.with.future = slope
            else
            {
                slope.with.future = pmax(0, private$i.beta.links$slope$reverse.apply(
                    private$i.beta.links$slope$apply(slope) + future.slope
                ))
            }
            
            rv = do_project_logistic_tail(intercept = terms$intercept,
                                          slope = slope,
                                          slope_with_future = slope.with.future,
                                          future_slope = future.slope,
                                          future_slope_after_year = future.slope.after.year,
                                          span = private$i.span,
                                          min = private$i.link$min,
                                          max = private$i.link$max,
                                          logistic_after_value = private$i.logistic.after.value,
                                          anchor_year = private$i.anchor.year,
                                          years = years)
            names(rv) = as.character(years)
            
            rv
        },
        
        # keeping this unused function in here because it makes the cpp easier to read
        OLD.do.project = function(terms,
                              years,
                              future.slope,
                              future.slope.after.year,
                              dim.names,
                              check.consistency,
                              error.prefix)
        {
            # Convert from logit scale
            # intercept = model$min.proportion + model$p.span / (1+exp(-terms$intercept)) 
            # slope = model$min.proportion + model$p.span / (1+exp(-terms$slope))
            
            intercept = terms$intercept
            slope = pmax(0, terms$slope)
            
            if (is.null(future.slope))
                slope.with.future = slope
#            else if (private$i.beta.links$slope$type=='identity')
#                slope.with.future = slope + future.slope
#            else if (private$i.beta.links$slope$type=='log' || private$i.beta.links$slope$type=='logistic')
#                slope.with.future = slope * exp(future.slope)
            else
            {
                slope.with.future = pmax(0, private$i.beta.links$slope$reverse.apply(
                    private$i.beta.links$slope$apply(slope) + future.slope
                ))
            }
            
            #-- Fold in additional.slope.after.year --#
            logistic.slope.sans.additional = slope * private$i.span / (private$i.logistic.after.value - private$i.link$min) /
                (private$i.link$max - private$i.logistic.after.value)
            
            logistic.slope.with.additional = slope.with.future * private$i.span / 
                (private$i.logistic.after.value - private$i.link$min) /
                (private$i.link$max - private$i.logistic.after.value)
            
            
            logistic.after.year = private$i.anchor.year + (private$i.logistic.after.value - intercept) / slope
            
            if (future.slope.after.year==-Inf)
                stop(paste0(error.prefix, "For a logistic.tail functional form, future.slope.after.year cannot be -Inf"))
            else if (!is.infinite(future.slope.after.year) && !future.slope==0)
            {
                val.at.additional.year = intercept + slope * (future.slope.after.year - private$i.anchor.year)
                mask = val.at.additional.year < private$i.logistic.after.value
    
                logistic.after.year.after.additional = future.slope.after.year + 
                    (private$i.logistic.after.value - val.at.additional.year) / slope.with.future
                logistic.after.year[mask] = logistic.after.year.after.additional[mask]
            }
            
            logistic.intercept = rep(log(private$i.logistic.after.value - private$i.link$min) - 
                log(private$i.link$max - private$i.logistic.after.value), length(logistic.slope.sans.additional))
            
            nonzero.logistic.slope.sans.additional = logistic.slope.sans.additional != 0
            logistic.intercept[nonzero.logistic.slope.sans.additional] = logistic.intercept[nonzero.logistic.slope.sans.additional] -
                logistic.slope.sans.additional[nonzero.logistic.slope.sans.additional] * (pmin(logistic.after.year[nonzero.logistic.slope.sans.additional], future.slope.after.year) - private$i.anchor.year)
            
            nonzero.logistic.slope.with.additional = logistic.slope.with.additional != 0
            logistic.intercept[nonzero.logistic.slope.with.additional] = logistic.intercept[nonzero.logistic.slope.with.additional] -
                logistic.slope.with.additional[nonzero.logistic.slope.with.additional] * pmax(0, logistic.after.year[nonzero.logistic.slope.with.additional] - future.slope.after.year)
            
            rv = lapply(years, function(year){
                # Calculate the RV
                # sub in anything past the use logistic tail threshold
                sub.rv = intercept + 
                    slope * min(year - private$i.anchor.year, future.slope.after.year - private$i.anchor.year) +
                    slope.with.future * max(0, year - future.slope.after.year)
                
                mask = sub.rv > private$i.logistic.after.value
                
                # The value based off logistic model
                log.ors = logistic.intercept[mask] +
                    logistic.slope.sans.additional[mask] * (pmin(year, future.slope.after.year)-private$i.anchor.year) +
                    logistic.slope.with.additional[mask] * pmax(0, year-future.slope.after.year)
                
                sub.rv[mask] = private$i.link$min + private$i.span / (1 + exp(-log.ors))
                
                sub.rv[sub.rv<private$i.link$min] = private$i.link$min
                
                if (any(is.na(sub.rv)))
                    browser()
                sub.rv
            })
            
            rv
        }
    )
)

SPLINE.FUNCTIONAL.FORM = R6::R6Class(
    'spline.functional.form',
    inherit = FUNCTIONAL.FORM,
    
    public = list(
        
        initialize = function(type,
                              apply.spline.to.list,
                              can.accomodate.future.slope,
                              
                              knot.times,
                              knot.values,
                              
                              link,
                              min,
                              max,
                              
                              knot.link,
                              knot.min,
                              knot.max,
                              
                              knots.are.on.transformed.scale,
                              overwrite.knot.values.with.alphas,
                              
                              before.time,
                              before.modifier,
                              after.time,
                              after.modifier,
                              
                              modifiers.apply.to.change,
                              modifier.link,
                              after.modifier.increasing.change.link,
                              after.modifier.decreasing.change.link,
                              before.modifier.increasing.change.link,
                              before.modifier.decreasing.change.link,
                              
                              modifier.min,
                              modifier.max,
                              overwrite.modifiers.with.alphas,

                              additional.betas = NULL,
                              additional.beta.links = NULL,
                              additional.alpha.links = NULL,
                              additional.overwrite.alphas=NULL,
                              
                              error.prefix='')
        {
            #-- Set up links (and check min, max) --#
            link = get.link(link, min=min, max=max)
            knot.link = get.link(knot.link, min=knot.min, max=knot.max)
            
            #-- Check knot.times --#
            if (!is.numeric(knot.times))
                stop(paste0(error.prefix, "'knot.times' must be a numeric vector"))
            if (length(knot.times)==0)
                stop(paste0(error.prefix, "'knot.times' must contain at least one value"))
            if (any(is.na(knot.times)))
                stop(paste0(error.prefix, "'knot.times' cannot contain NA values"))
            tabled.knot.times = table(knot.times)
            if (max(tabled.knot.times)>1)
                stop(paste0(error.prefix, "'knot.times' must be unique. ",
                            collapse.with.and(names(tabled.knot.times)[tabled.knot.times>1]),
                            ifelse(sum(tabled.knot.times>1)==1, " is", " are"),
                            " repeated"))

            
            n.knots = length(knot.times)
            
            #-- Check knot.values --#
            if (!is.list(knot.values))
            {
                if (n.knots==1)
                    knot.values = list(knot.values)
                else
                    stop(paste0(error.prefix, "If there is more than one knot, 'knot.values' must be a list"))
            }
            
            if (length(knot.values) != n.knots)
                stop(paste0("'knot.values' (length ", length(knot.values),
                            ") must have the same length as knot.times (", n.knots, ")"))
            
            #-- Check knot names --#
            knot.names = names(knot.times)
            if (is.null(knot.names))
                stop(paste0(error.prefix, "'knot.times' must have names set"))
            if (any(is.na(knot.names)))
                stop(paste0(error.prefix, "The names of 'knot.times' cannot be NA"))
            if (any(nchar(knot.names)==0))
                stop(paste0(error.prefix, "The names of 'knot.times' cannot be empty ('')"))
            tabled.knot.names = table(knot.names)
            if (max(tabled.knot.names)>1)
                stop(paste0(error.prefix, "The names of 'knot.times' must be unique. ",
                            collapse.with.and("'", names(tabled.knot.names)[tabled.knot.names>1], "'"),
                            ifelse(sum(tabled.knot.names>1)==1, " is", " are"),
                            " repeated"))
            
            disallowed.knot.names = c('before.modifier', 'after.modifier')
            invalid.names = intersect(knot.names, disallowed.knot.names)
            if (length(invalid.names)>0)
                stop(paste0(error.prefix, 
                            collapse.with.and("'", invalid.names, "'"),
                            ifelse(length(invalid.names)==1, " is", " are"),
                            " not allowed as names of knot.times"))
            
            if (is.null(names(knot.values)))
                stop(paste0(error.prefix, "'knot.values' must have names set"))
            if (any(is.na(names(knot.values))))
                stop(paste0(error.prefix, "The names of 'knot.values' cannot be NA"))
            
            if (!setequal(knot.names, names(knot.values)))
                stop(paste0(error.prefix, "'knot.values' and 'knot.times' must have the same names"))
            
            knot.values = knot.values[knot.names]
            
            #-- Transform the knot values --#
            if (!is.logical(knots.are.on.transformed.scale) || length(knots.are.on.transformed.scale)!=1 ||
                is.na(knots.are.on.transformed.scale))
                stop(paste0(error.prefix, "'knots.are.on.transformed.scale' must be a single, non-NA logical value"))
            if (!knots.are.on.transformed.scale)
            { 
                knot.values = lapply(knot.names, function(knot.name){
                    knot.link$check.untransformed.values(knot.values[[knot.name]],
                                                         variable.name.for.error = paste0("values for knot '", knot.name, "'"),
                                                         error.prefix = error.prefix)
                    
                    knot.link$apply(knot.values[[knot.name]])
                })
                names(knot.values) = knot.names
            }
            
            #-- Order knots by time --#
            
            o = order(knot.times)
            knot.times = knot.times[o]
            knot.values = knot.values[o]
            knot.names = knot.names[o]
            
            #-- Check overwrite.knot.values.with.alphas --#
            if (!is.logical(overwrite.knot.values.with.alphas) || length(overwrite.knot.values.with.alphas)!=1 || 
                is.na(overwrite.knot.values.with.alphas))
                stop("'overwrite.knot.values.with.alphas' must be a single, non-NA logical value")
            
            #-- Set up Betas from Knots --#
            betas = knot.values
            
            alphas.are.additive = rep(!overwrite.knot.values.with.alphas, n.knots)
            if (overwrite.knot.values.with.alphas)
                alpha.links = lapply(1:n.knots, function(i){knot.link})
            else
                alpha.links = lapply(1:n.knots, function(i){knot.link$get.coefficient.link()})
            
            beta.links = lapply(1:n.knots, function(i){
                knot.link
            })
            
            names(alphas.are.additive) = names(alpha.links) = names(betas) = names(beta.links) = knot.names
            
            
            #-- Check Modifier Metadata (link, application, overwrite) --#
            
            if (!is.null(before.modifier) || !is.null(after.modifier))
            {
                modifier.link = get.link(modifier.link, min=modifier.min, max=modifier.max)
                
                if (!is.logical(modifiers.apply.to.change) || length(modifiers.apply.to.change)!=1 || 
                    is.na(modifiers.apply.to.change))
                    stop("'modifiers.apply.to.change' must be a single, non-NA logical value")
                
                if (!is.logical(overwrite.modifiers.with.alphas) || length(overwrite.modifiers.with.alphas)!=1 || 
                    is.na(overwrite.modifiers.with.alphas))
                    stop("'overwrite.modifiers.with.alphas' must be a single, non-NA logical value")
                
                if (!is.null(before.modifier))
                {
                    before.modifier.increasing.change.link = get.link(before.modifier.increasing.change.link, min=modifier.min, max=modifier.max)
                    before.modifier.decreasing.change.link = get.link(before.modifier.decreasing.change.link, min=modifier.min, max=modifier.max)
                }
                
                if (!is.null(after.modifier))
                {
                    after.modifier.increasing.change.link = get.link(after.modifier.increasing.change.link, min=modifier.min, max=modifier.max)
                    after.modifier.decreasing.change.link = get.link(after.modifier.decreasing.change.link, min=modifier.min, max=modifier.max)
                }
            }
            
            #-- Check Before Modifier/Application/Time --#
            
            
            if (is.null(before.modifier))
            {
                if (!is.null(before.time))
                    stop(paste0(error.prefix, "If 'before.time' is non-NULL, 'before.modifier' must be non-NULL as well"))
            }
            else
            {
                # Check before time
                if (is.null(before.time))
                    stop(paste0(error.prefix, "If 'before.modifier' is non-NULL, 'before.time' must be non-NULL as well"))
                
                if (!is.numeric(before.time) || length(before.time) != 1 || is.na(before.time))
                    stop(paste0(error.prefix, "'before.time' must be a single, non-NA, numeric value"))
                if (before.time >= knot.times[1])
                    stop(paste0(error.prefix, "'before.time' must PRECEDE all the knot.times"))
                
                # Check the modifier value
                if (!is.numeric(before.modifier) || length(before.modifier)==0)
                    stop(paste0(error.prefix, "'before.modifier' must be a non-empty numeric object"))

                # Apply link
                modifier.link$check.untransformed.values(before.modifier,
                                                         variable.name.for.error = 'before.modifier',
                                                         error.prefix = error.prefix)
                
                # Put it in the vectors
                betas = c(list(modifier.link$apply(before.modifier)), betas)
                alphas.are.additive = c(!overwrite.modifiers.with.alphas, alphas.are.additive)
                
                if (modifiers.apply.to.change)
                    beta.links = c(list(modifier.link), beta.links)
                else
                    beta.links = c(list(get.link('identity')), beta.links) # We don't want the ff code to apply this before we add it in
                
                if (overwrite.modifiers.with.alphas)
                    alpha.links = c(list(modifier.link), alpha.links)
                else
                    alpha.links = c(list(modifier.link$get.coefficient.link()), alpha.links)
                names(betas)[1] = names(alphas.are.additive)[1] = names(alpha.links)[1] = names(beta.links)[1] = 'before.modifier'
            }
            
            
            #-- Check After Modifier/Application/Time --#
            
            if (is.null(after.modifier))
            {
                if (!is.null(after.time))
                    stop(paste0(error.prefix, "If 'after.time' is non-NULL, 'after.modifier' must be non-NULL as well"))
            }
            else
            {
                # Check after time
                if (is.null(after.time))
                    stop(paste0(error.prefix, "If 'after.modifier' is non-NULL, 'after.time' must be non-NULL as well"))
                
                if (!is.numeric(after.time) || length(after.time) != 1 || is.na(after.time))
                    stop(paste0(error.prefix, "'after.time' must be a single, non-NA, numeric value"))
                if (after.time <= knot.times[n.knots])
                    stop("'after.time' must be AFTER all the knot.times")
                
                # Check the modifier value
                if (!is.numeric(after.modifier) || length(after.modifier)==0)
                    stop(paste0(error.prefix, "'after.modifier' must be a non-empty numeric object"))
                

                # Apply link
                modifier.link$check.untransformed.values(after.modifier,
                                                         variable.name.for.error = 'after.modifier',
                                                         error.prefix = error.prefix)
                
                # Put it in the vectors
                betas = c(betas, list(modifier.link$apply(after.modifier)))
                alphas.are.additive = c(alphas.are.additive, !overwrite.modifiers.with.alphas)
                
                if (modifiers.apply.to.change)
                    beta.links = c(beta.links, list(modifier.link))
                else
                    beta.links = c(beta.links, list(get.link('identity'))) # We don't want the ff code to apply this before we add it in
                
                if (overwrite.modifiers.with.alphas)
                    alpha.links = c(alpha.links, list(modifier.link))
                else
                    alpha.links = c(alpha.links, list(modifier.link$get.coefficient.link()))
                names(betas)[length(betas)] = names(alphas.are.additive)[length(betas)] = names(alpha.links)[length(betas)] = names(beta.links)[length(betas)] = 'after.modifier'
            }
            
            #-- Check apply.spline.to.list --#
            if (!is.logical(apply.spline.to.list) || length(apply.spline.to.list)!=1 || is.na(apply.spline.to.list))
                stop(paste0(error.prefix, "'apply.spline.to.list' must be a single, non-NA logical value"))
            
            
            #-- Set up future slope link --#
            if (can.accomodate.future.slope)
                future.slope.link = link$get.coefficient.link()
            else
                future.slope.link = NULL

            #-- Fold in additional betas/alphas --#
            if (length(additional.betas) != length(additional.beta.links))
                stop(paste0(error.prefix, "'additional.beta.links' must have the same names as 'additional.betas'"))
            
            if (length(additional.betas) != length(additional.alpha.links))
                stop(paste0(error.prefix, "'additional.alpha.links' must have the same names as 'additional.betas'"))
            
            if (length(additional.betas) != length(additional.overwrite.alphas))
                stop(paste0(error.prefix, "'additional.overwrite.alphas' must have the same names as 'additional.betas'"))
            
            if (length(additional.betas)>0)
            {
                if (!is.list(additional.betas))
                    stop(paste0(error.prefix, "'additional.betas' must be a list"))
                if (!is.list(additional.beta.links))
                    stop(paste0(error.prefix, "'additional.beta.links' must be a list"))
                if (!is.list(additional.alpha.links))
                    stop(paste0(error.prefix, "'additional.alpha.links' must be a list"))
                if (!is.logical(additional.overwrite.alphas))
                    stop(paste0(error.prefix, "'additional.overwrite.alphas' must be a logical vector"))
                
                if (is.null(names(additional.betas)))
                    stop(paste0(error.prefix, "'additional.betas' must have names set"))
                if (is.null(names(additional.beta.links)))
                    stop(paste0(error.prefix, "'additional.beta.links' must have names set"))
                if (is.null(names(additional.alpha.links)))
                    stop(paste0(error.prefix, "'additional.alpha.links' must have names set"))
                if (is.null(names(additional.overwrite.alphas)))
                    stop(paste0(error.prefix, "'additional.overwrite.alphas' must have names set"))
                
                if (!setequal(names(additional.betas), names(additional.beta.links)))
                    stop(paste0(error.prefix, "'additional.beta.links' must have the same names as 'additional.betas'"))
                if (!setequal(names(additional.betas), names(additional.alpha.links)))
                    stop(paste0(error.prefix, "'additional.alpha.links' must have the same names as 'additional.betas'"))
                if (!setequal(names(additional.betas), names(additional.overwrite.alphas)))
                    stop(paste0(error.prefix, "'additional.overwrite.alphas' must have the same names as 'additional.betas'"))
            }
            
            
            #-- Call the super-class constructor --#
            betas = c(betas, additional.betas)
            beta.links = c(beta.links, additional.beta.links)
            alpha.links = c(alpha.links, additional.alpha.links)
            if (!is.null(additional.overwrite.alphas))
                alphas.are.additive = c(alphas.are.additive, !additional.overwrite.alphas)
            super$initialize(type = type,
                             betas = betas,
                             link = link,
                             future.slope.link = future.slope.link,
                             alphas.are.additive = alphas.are.additive, 
                             beta.links = beta.links,
                             alpha.links = alpha.links, 
                             is.static = F,
                             error.prefix = error.prefix)
            
            #-- Store the extra member variables --#
            private$i.knot.times = knot.times
            private$i.knot.names = knot.names

            private$i.before.time = before.time
            private$i.after.time = after.time
            private$i.modifier.link = modifier.link
            
            private$i.modifiers.apply.to.change = modifiers.apply.to.change
            
            private$i.after.modifier.increasing.change.link = after.modifier.increasing.change.link
            private$i.after.modifier.decreasing.change.link = after.modifier.decreasing.change.link
            
            private$i.before.modifier.increasing.change.link = before.modifier.increasing.change.link
            private$i.before.modifier.decreasing.change.link = before.modifier.decreasing.change.link
            
            private$i.apply.spline.to.list = apply.spline.to.list
        }
        
    ),
    
    active = list(
        
        knot.times = function(value)
        {
            if (missing(value))
            {
                c(private$i.before.time,
                  private$i.knot.times,
                  private$i.after.time)
            }
            else
                stop("Cannot modify 'knot.times' for a functional.form object - it is read-only")
        },
        
        n.knots = function(value)
        {
            if (missing(value))
                length(self$knot.times)
            else
                stop("Cannot modify 'n.knots' for a functional.form object - it is read-only")
        }
    ),
    
    private = list(
        
        i.knot.times = NULL,
        i.knot.names = NULL,
        
        i.before.time = NULL,
        i.after.time = NULL,
        i.modifier.link = NULL,
        
        i.after.modifier.increasing.change.link = NULL,
        i.after.modifier.decreasing.change.link = NULL,
        
        i.before.modifier.increasing.change.link = NULL,
        i.before.modifier.decreasing.change.link = NULL,
        
        i.modifiers.apply.to.change = NULL,
        
        i.apply.spline.to.list = NULL,
        
        apply.spline = function(knot.values,
                                knot.times,
                                desired.times)
        {
            stop(paste0(error.prefix,
                        "This subclass of 'functional.form' ('", private$i.type,
                        "') is incompletely specified. The private 'apply.spline' method must be implemented at the subclass level"))       
        },
        
        do.project = function(terms,
                              years,
                              future.slope,
                              future.slope.after.year,
                              dim.names,
                              check.consistency,
                              error.prefix)
        {
            #-- Pull out knot values --#
            knot.values = terms[private$i.knot.names]
            knot.times = self$knot.times
            n.knots = length(knot.times)
            
            #-- Set up before multiplier (if present) --#
            if (!is.null(private$i.before.time))
            {
                if (private$i.modifiers.apply.to.change)
                {
#                    before.knot.value = knot.values[[1]] - terms$before.modifier * (knot.values[[2]] - knot.values[[1]])
                    transformed.knot1 = private$i.link$apply(knot.values[[1]])
                    before.knot.value = private$i.link$reverse.apply(
                        transformed.knot1 - terms$before.modifier * (private$i.link$apply(knot.values[[2]]) - transformed.knot1)
                    )
                    
                    first.knot = knot.values[[1]]
                    second.knot = knot.values[[2]]
                    
                    transformed.first.knot = private$i.before.modifier.increasing.change.link$apply(first.knot)
                    delta = private$i.before.modifier.increasing.change.link$apply(second.knot) - transformed.first.knot
                    transformed.before.knot.value = transformed.first.knot - terms$before.modifier * delta
                    before.knot.value = private$i.before.modifier.increasing.change.link$reverse.apply(transformed.before.knot.value)
                    
                    if (!private$i.before.modifier.increasing.change.link$equals(private$i.before.modifier.decreasing.change.link))
                    {
                        decreasing.mask = first.knot < second.knot
                        
                        transformed.first.knot.decreasing = private$i.before.modifier.decreasing.change.link$apply(first.knot[decreasing])
                        delta = private$i.before.modifier.decreasing.change.link$apply(second.knot[decreasing.mask]) - transformed.first.knot[decreasing.mask]
                        transformed.before.knot.value.decreasing = transformed.first.knot.decreasing - terms$before.modifier[decreasing.mask] * delta
                        before.knot.value[decreasing.mask] = private$i.before.modifier.decreasing.change.link$reverse.apply(transformed.before.knot.value.decreasing)
                    }
                }
                else
                {
                    before.knot.value = private$i.modifier.link$reverse.apply(
                        private$i.modifier.link$apply(knot.values[[1]]) +
                            terms$before.modifier
                    )
                }
                
                knot.values = c(list(before.knot.value), knot.values)
            }
            
            #-- Set up after multiplier (if present) --#
            if (!is.null(private$i.after.time))
            {
                if (private$i.modifiers.apply.to.change)
                {
                   # after.knot.value = knot.values[[length(knot.values)]] + 
                   #     terms$after.modifier * (knot.values[[length(knot.values)]] - knot.values[[length(knot.values)-1]])     
                    
                    last.knot = knot.values[[length(knot.values)]]
                    penultimate.knot = knot.values[[length(knot.values)-1]]

                    transformed.last.knot = private$i.after.modifier.increasing.change.link$apply(last.knot)
                    delta = transformed.last.knot - private$i.after.modifier.increasing.change.link$apply(penultimate.knot)
                    transformed.after.knot.value = transformed.last.knot + terms$after.modifier * delta
                    after.knot.value = private$i.after.modifier.increasing.change.link$reverse.apply(transformed.after.knot.value)


                    if (!private$i.after.modifier.increasing.change.link$equals(private$i.after.modifier.decreasing.change.link))
                    {
                        decreasing.mask = last.knot < penultimate.knot
                        
                        transformed.last.knot.decreasing = private$i.after.modifier.decreasing.change.link$apply(last.knot[decreasing.mask])
                        delta = transformed.last.knot.decreasing - private$i.after.modifier.decreasing.change.link$apply(penultimate.knot[decreasing.mask])
                        transformed.after.knot.value.decreasing = transformed.last.knot.decreasing + terms$after.modifier[decreasing.mask] * delta
                        after.knot.value[decreasing.mask] = private$i.after.modifier.decreasing.change.link$reverse.apply(transformed.after.knot.value.decreasing)
                    }
                }
                else
                {
                    after.knot.value = privatei.$modifier.link$reverse.apply(
                        private$i.modifier.link$apply(knot.values[[length(knot.values)]]) + 
                            terms$after.modifier
                    )
                }
                
                knot.values = c(knot.values, list(after.knot.value))
            }
            
            
            #-- Fit the spline for each value in the array and project --#
            n = length(knot.values[[1]])
            
            future.slope.to.add = future.slope * pmax(0, years-future.slope.after.year)
            
            if (private$i.apply.spline.to.list)
            {
                arr.rv = sapply(1:n, function(i){
                    knot.values.for.i = sapply(knot.values, function(v){v[i]})
                    
                    private$apply.spline(knot.values = private$i.link$apply(knot.values.for.i),
                                         knot.times = knot.times,
                                         desired.times = years)
                })
                dim(arr.rv) = c(year=length(years), length(arr.rv)/length(years))
                
                lapply(1:length(years), function(y){
                    sub.rv = private$i.link$reverse.apply(arr.rv[y,] + 
                                                                  future.slope * max(0, years[y]-future.slope.after.year))
                    
                    
                    if (length(dim.names)>0)
                    {
                        dim(sub.rv) = sapply(dim.names, length)
                        dimnames(sub.rv) = dim.names
                    }
                    else
                        names(sub.rv) = NULL
                    
                    sub.rv
                })
            }
            else
            {
                rv = private$apply.spline(knot.values = private$i.link$apply(knot.values),
                                          knot.times = knot.times,
                                          desired.times = years)
                
                lapply(1:length(years), function(y){
                    private$i.link$reverse.apply(rv[[y]] + 
                                                         future.slope * max(0, years[y]-future.slope.after.year))
                })
            }
        }
    )
)

LINEAR.SPLINE.FUNCTIONAL.FORM = R6::R6Class(
    'linear.spline.functional.form',
    inherit = SPLINE.FUNCTIONAL.FORM,
    
    public = list(
        
        initialize = function(knot.times,
                              knot.values,
                              
                              link = c('identity','log','logit')[1],
                              min = NA,
                              max = NA,
                              
                              knot.link = c('identity','log','logit')[1],
                              knot.min = NA,
                              knot.max = NA,
                              
                              knots.are.on.transformed.scale=F,
                              overwrite.knot.values.with.alphas=F,
                              
                              before.time=NULL,
                              before.modifier=NULL,
                              after.time=NULL,
                              after.modifier=NULL,
                              
                              modifiers.apply.to.change=T,
                              modifier.link = link,
                              after.modifier.increasing.change.link = modifier.link,
                              after.modifier.decreasing.change.link = modifier.link,
                              before.modifier.increasing.change.link = modifier.link,
                              before.modifier.decreasing.change.link = modifier.link,
                              modifier.min = min,
                              modifier.max = min,
                              overwrite.modifiers.with.alphas = F,
                              
                              error.prefix = 'Cannot create linear spline functional.form: ')
        {
            super$initialize(type = 'linear spline',
                             apply.spline.to.list = T,
                             can.accomodate.future.slope = T,
                             
                             knot.times = knot.times,
                             knot.values = knot.values,
                             
                             link = link,
                             min = min,
                             max = max,
                             
                             knot.link = knot.link,
                             knot.min = knot.min,
                             knot.max = knot.max,
                             
                             knots.are.on.transformed.scale = knots.are.on.transformed.scale,
                             overwrite.knot.values.with.alphas = overwrite.knot.values.with.alphas,
                             
                             before.time = before.time,
                             before.modifier = before.modifier,
                             after.time = after.time,
                             after.modifier = after.modifier,
                             
                             modifiers.apply.to.change = modifiers.apply.to.change,
                             modifier.link = modifier.link,
                             after.modifier.increasing.change.link = after.modifier.increasing.change.link,
                             after.modifier.decreasing.change.link = after.modifier.decreasing.change.link,
                             before.modifier.increasing.change.link = before.modifier.increasing.change.link,
                             before.modifier.decreasing.change.link = before.modifier.decreasing.change.link,
                             modifier.min = modifier.min,
                             modifier.max = modifier.max,
                             overwrite.modifiers.with.alphas = overwrite.modifiers.with.alphas,
                             
                             error.prefix = error.prefix)
        }
        
    ),
    
    private = list(
        
        apply.spline = function(knot.values,
                                knot.times,
                                desired.times)
        {
            interpolate(values = knot.values,
                        value.times = knot.times,
                        desired.times = desired.times)
        }
    )
)

NATURAL.SPLINE.FUNCTIONAL.FORM = R6::R6Class(
    'natural.spline.functional.form',
    inherit = SPLINE.FUNCTIONAL.FORM,
    
    public = list(
        
        initialize = function(knot.times,
                              knot.values,
                              
                              link = c('identity','log','logit')[1],
                              min = NA,
                              max = NA,
                              
                              knot.link = link,
                              knot.min = NA,
                              knot.max = NA,
                              
                              knots.are.on.transformed.scale=F,
                              overwrite.knot.values.with.alphas=F,
                              
                              before.time=NULL,
                              before.modifier=NULL,
                              after.time=NULL,
                              after.modifier=NULL,
                              
                              modifiers.apply.to.change=T,
                              modifier.link = link,
                              after.modifier.increasing.change.link = modifier.link,
                              after.modifier.decreasing.change.link = modifier.link,
                              before.modifier.increasing.change.link = modifier.link,
                              before.modifier.decreasing.change.link = modifier.link,
                              modifier.min = min,
                              modifier.max = min,
                              overwrite.modifiers.with.alphas = F,
                              
                              error.prefix = 'Cannot create natural spline functional.form: ')
        {
            super$initialize(type = 'natural spline',
                             apply.spline.to.list = T,
                             can.accomodate.future.slope = T,
                             
                             knot.times = knot.times,
                             knot.values = knot.values,
                             
                             link = link,
                             min = min,
                             max = max,
                             
                             knot.link = knot.link,
                             knot.min = knot.min,
                             knot.max = knot.max,
                             
                             knots.are.on.transformed.scale = knots.are.on.transformed.scale,
                             overwrite.knot.values.with.alphas = overwrite.knot.values.with.alphas,
                             
                             before.time = before.time,
                             before.modifier = before.modifier,
                             after.time = after.time,
                             after.modifier = after.modifier,
                             
                             modifiers.apply.to.change = modifiers.apply.to.change,
                             modifier.link = modifier.link,
                             after.modifier.increasing.change.link = after.modifier.increasing.change.link,
                             after.modifier.decreasing.change.link = after.modifier.decreasing.change.link,
                             before.modifier.increasing.change.link = before.modifier.increasing.change.link,
                             before.modifier.decreasing.change.link = before.modifier.decreasing.change.link,
                             modifier.min = modifier.min,
                             modifier.max = modifier.max,
                             overwrite.modifiers.with.alphas = overwrite.modifiers.with.alphas,
                             
                             error.prefix = error.prefix)
        }
        
    ),
    
    private = list(
        
        apply.spline = function(knot.values,
                                knot.times,
                                desired.times)
        {
            stats::spline(x = knot.times, 
                          y = knot.values, 
                          xout = desired.times,
                          method='natural')$y
        }
    )
)

LOGISTIC.SPLINE.FUNCTIONAL.FORM = R6::R6Class(
    'logistic.spline.form',
    inherit = SPLINE.FUNCTIONAL.FORM,
    
    public = list(
        
        initialize = function(knot.times,
                              knot.values,
                              
                              min = -Inf,
                              max = Inf,
                              
                              knot.link = c('identity','log','logit')[2],
                              knot.min = NA,
                              knot.max = NA,
                              
                              knots.are.on.transformed.scale = F,
                              overwrite.knot.values.with.alphas = F,
                              
                              fraction.of.asymptote.after.end=0.05,
                              fraction.of.asymptote.before.start=0.025,
                              fraction.of.asymptote.for.change.dir=0.02,
                              fraction.asymptote.link = 'log',
                              overwrite.fraction.asymptote.with.alphas = F,
                              
                              error.prefix = 'Cannot create logistic spline functional.form: ')
        {
            #-- Set up betas for asymptotes --#
            fraction.asymptotes = list(
                fraction.of.asymptote.before.start = fraction.of.asymptote.before.start,
                fraction.of.asymptote.for.change.dir = fraction.of.asymptote.for.change.dir,
                fraction.of.asymptote.after.end = fraction.of.asymptote.after.end
            )[c(T,length(knot.values)>2,T)]
            
            
            #-- Check overwrite.fraction.asymptote.with.alphas --#
            if (!is.logical(overwrite.fraction.asymptote.with.alphas) || length(overwrite.fraction.asymptote.with.alphas)!=1 || is.na(overwrite.fraction.asymptote.with.alphas))
                stop("'overwrite.fraction.asymptote.with.alphas' must be a single, non-NA logical value")
            
            overwrite.fraction.asymptote.with.alphas = rep(overwrite.fraction.asymptote.with.alphas, 
                                                           length(fraction.asymptotes))
            names(overwrite.fraction.asymptote.with.alphas) = names(fraction.asymptotes)


            #-- fraction.asymptote.link --#
            fraction.asymptote.links = lapply(1:length(fraction.asymptotes), function(i){
                get.link(fraction.asymptote.link, min=0, max=1,
                         variable.name.for.error = 'fraction.asymptote.link',
                         error.prefix = error.prefix)
            })
            names(fraction.asymptote.links) = names(fraction.asymptotes)
            
            if (overwrite.knot.values.with.alphas[1])
                alpha.links = fraction.asymptote.links
            else
                alpha.links = lapply(fraction.asymptote.links, function(l){l$get.coefficient.link()})
            
            #-- Apply link scale to fraction.asymptotes --#
            fraction.asymptotes = lapply(names(fraction.asymptote.links), function(name){
                fraction.asymptote.links[[name]]$check.untransformed.values(fraction.asymptotes[[name]],
                                                                            variable.name.for.error = name,
                                                                            error.prefix = error.prefix)
                
                fraction.asymptote.links[[name]]$apply(fraction.asymptotes[[name]])
            })
            names(fraction.asymptotes) = names(fraction.asymptote.links)
            
            
            #-- Call the super-class (spline) constructor --#
            
            super$initialize(type = 'logistic spline',
                             apply.spline.to.list = F,
                             can.accomodate.future.slope = F,
                             
                             knot.times = knot.times,
                             knot.values = knot.values,
                             
                             link = 'identity',
                             min = min,
                             max = max,
                             
                             knot.link = knot.link,
                             knot.min = knot.min,
                             knot.max = knot.max,
                             
                             knots.are.on.transformed.scale = knots.are.on.transformed.scale,
                             overwrite.knot.values.with.alphas = overwrite.knot.values.with.alphas,
                             
                             additional.betas = fraction.asymptotes,
                             additional.beta.links = fraction.asymptote.links,
                             additional.alpha.links = alpha.links,
                             additional.overwrite.alphas = overwrite.fraction.asymptote.with.alphas,
                             
                             before.time = NULL,
                             before.modifier = NULL,
                             after.time = NULL,
                             after.modifier = NULL,

                             # This next set of values is ignored, since there are no before/after modifiers
                             modifiers.apply.to.change = F,
                             modifier.link = 'identity',
                             after.modifier.increasing.change.link = 'identity',
                             after.modifier.decreasing.change.link = 'identity',
                             before.modifier.increasing.change.link = 'identity',
                             before.modifier.decreasing.change.link = 'identity',
                             modifier.min = NA,
                             modifier.max = NA,
                             
                             error.prefix = error.prefix)
          
            #-- Check n.knots >2 --#
            if (self$n.knots<2)
                stop(paste0(error.prefix, "A logistic spline must have at least two knots"))
            
            #-- Check fraction.of.asymptote.for.change.dir --#
            if (self$n.knots > 2)
            {
                if (any(fraction.of.asymptote.for.change.dir<0) || any(fraction.of.asymptote.for.change.dir>1))
                    stop(paste0(error.prefix, 
                                "'fraction.of.asymptote.for.change.dir' must have values between 0 and 1"))
            }
            
            #-- Check fraction.of.asymptote.before.start and fraction.of.asymptote.after.end --#
            
            if (any(fraction.of.asymptote.before.start<0) || any(fraction.of.asymptote.before.start>1))
                stop("'fraction.of.asymptote.before.start' must have values between 0 and 1")
            
            if (any(fraction.of.asymptote.after.end<0) || any(fraction.of.asymptote.after.end>1))
                stop("'fraction.of.asymptote.after.end' must have values between 0 and 1")

            #-- One last check (that adjacent fractions don't sum to more than 1)
            fraction.asymptote.before.start = private$i.alpha.links$fraction.of.asymptote.before.start$reverse.apply(private$i.betas$fraction.of.asymptote.before.start)
            
            fraction.of.asymptote.after.end = private$i.alpha.links$fraction.of.asymptote.after.end$reverse.apply(private$i.betas$fraction.of.asymptote.after.end)
            if (self$n.knots>2)
            {
                fraction.of.asymptote.for.change.dir = private$i.alpha.links$fraction.of.asymptote.for.change.dir$reverse.apply(private$i.betas$fraction.of.asymptote.for.change.dir)
                
                if (any((fraction.of.asymptote.before.start + 
                         fraction.of.asymptote.for.change.dir)>=1))
                    stop(paste0(error.prefix,
                                "With >2 knots, the sum of 'fraction.of.asymptote.before.start' + 'fraction.of.asymptote.for.change.dir' must always be less than 1"))
                
                if (any((fraction.of.asymptote.for.change.dir + 
                        fraction.of.asymptote.after.end)>=1))
                    stop(paste0(error.prefix,
                                "With >2 knots, the sum of 'fraction.of.asymptote.for.change.dir' + 'fraction.of.asymptote.after.end' must always be less than 1"))
                
                if (self$n.knots>3)
                { 
                    if (any(fraction.of.asymptote.for.change.dir>=0.5))
                        stop(paste0(error.prefix, "With >3 knots, 'fraction.of.asymptote.for.change.dir' must always be less than 0.5"))
                }
            }
            else
            {
                if (any((fraction.of.asymptote.before.start + 
                         fraction.of.asymptote.after.end)>1))
                    stop(paste0(error.prefix,
                                "With two knots, the sum of fraction.of.asymptote.before.start + fraction.of.asymptote.after.end must always be less than 1"))
            }
        }
        
    ),
    
    private = list(
        
        do.project = function(terms,
                              years,
                              future.slope,
                              future.slope.after.year,
                              dim.names,
                              check.consistency,
                              error.prefix)
        {
            knot.values = terms[private$i.knot.names]

            #-- Set fraction asymptote --#
            fraction.of.asymptote.before.start = terms$fraction.of.asymptote.before.start
            fraction.of.asymptote.after.end = terms$fraction.of.asymptote.after.end
            fraction.of.asymptote.for.change.dir = terms$fraction.of.asymptote.for.change.dir
            
            # check asymptotes
            if (length(knot.values)>2)
            {
                if (any((fraction.of.asymptote.before.start + fraction.of.asymptote.for.change.dir)>=1))
                    stop(paste0(error.prefix, "fraction.of.asymptote.before.start and fraction.of.asymptote.for.change.dir cannot sum to one or greater"))
                if (any((fraction.of.asymptote.after.end + fraction.of.asymptote.for.change.dir)>=1))
                    stop(paste0(error.prefix, "fraction.of.asymptote.after.end and fraction.of.asymptote.for.change.dir cannot sum to one or greater"))
                
                if (length(knot.values)>3)
                {
                    if (any(fraction.of.asymptote.for.change.dir>=0.5))
                        stop(paste0(error.prefix, "With >3 knots, fraction.of.asymptote.for.change.dir cannot be 0.5 or greater"))
                }
            }
            else
            {
                if (any((fraction.of.asymptote.before.start + fraction.of.asymptote.after.end)>1))
                    stop(paste0(error.prefix, "fraction.of.asymptote.before.start and fraction.of.asymptote.after.end cannot sum to more than one"))
            }
            
            #-- Fit the spline for each value in the array and project --#
            
            n = length(knot.values[[1]])
            if (length(knot.values)==2)
            {
                arr.rv = sapply(1:n, function(i){
                    knot.values.for.i = sapply(knot.values, function(v){v[i]})
                    
                    calculate.change.ratios.logistic(r0 = knot.values.for.i[1],
                                                     r1 = knot.values.for.i[2],
                                                     times = years,
                                                     t0 = private$i.knot.times[1],
                                                     t1 = private$i.knot.times[2],
                                                     fraction.of.asymptote.after.end = fraction.of.asymptote.after.end[i],
                                                     fraction.of.asymptote.before.start = fraction.of.asymptote.before.start[i])
                })
                
            }
            else if (length(knot.values)==3)
            {
                arr.rv = sapply(1:n, function(i){
                    knot.values.for.i = sapply(knot.values, function(v){v[i]})
                    
                    calculate.change.ratios.two.logistic(r0 = knot.values.for.i[1],
                                                         r1 = knot.values.for.i[2],
                                                         r2 = knot.values.for.i[3],
                                                         times = years,
                                                         t0 = private$i.knot.times[1],
                                                         t1 = private$i.knot.times[2],
                                                         t2 = private$i.knot.times[3],
                                                         
                                                         fraction.of.asymptote.after.end = fraction.of.asymptote.after.end[i],
                                                         fraction.of.asymptote.before.start = fraction.of.asymptote.before.start[i],
                                                         fraction.of.asymptote.for.change.dir = fraction.of.asymptote.for.change.dir[i])
                })
            }
            else
            {
                n.chunks = length(knot.values)-1
                years.for.chunk = lapply(1:n.chunks, function(chunk){
                    if (chunk==1)
                        years[years<=knot.times[2]]
                    else if (chunk==n.chunks)
                        years[years>knot.times[n.chunks]]
                    else
                        years[years>knot.times[chunk] & years<=knot.times[chunk+1]]
                })
                
                start.asymptote.name = rep('fraction.of.asymptote.for.change.dir', n.chunks)
                start.asymptote.name[1] = 'fraction.of.asymptote.before.start'
                
                end.asymptote.name = rep('fraction.of.asymptote.for.change.dir', n.chunks)
                end.asymptote.name[n.chunks] = 'fraction.of.asymptote.after.end'
                
                chunks.to.do = (1:n.chunks)[sapply(years.for.chunk, length)>0]
                
                arr.rv = sapply(1:n, function(i){
                    knot.values.for.i = sapply(knot.values, function(v){v[i]})
                    
                    unlist(sapply(chunks.to.do, function(chunk){
                        calculate.change.ratios.logistic(r0 = knot.values.for.i[chunk],
                                                         r1 = knot.values.for.i[chunk+1],
                                                         times = years.for.chunk[[chunk]],
                                                         t0 = private$i.knot.times[chunk],
                                                         t1 = private$i.knot.times[chunk+1],
                                                         fraction.of.asymptote.after.end = fraction.asymptotes[[ end.asymptote.name[chunk] ]][i],
                                                         fraction.of.asymptote.before.start = fraction.asymptotes[[ start.asymptote.name[chunk] ]][i])
                    }))
                })
            }
            dim(arr.rv) = c(year=length(years), length(arr.rv)/length(years))
            
            lapply(1:length(years), function(y){
                sub.rv = arr.rv[y,]
                
                if (length(dim.names)>0)
                {
                    dim(sub.rv) = sapply(dim.names, length)
                    dimnames(sub.rv) = dim.names
                }
                else
                    names(sub.rv) = NULL
                
                private$i.link$reverse.apply(sub.rv)
            })
        }
    )
)



LOGISTIC.TAPER.FUNCTIONAL.FORM = R6::R6Class(
    'logistic.taper.functional.form',
    inherit = FUNCTIONAL.FORM,
    
    public = list(
        
        initialize = function(base.functional.form,
                              taper.start.year,
                              p.at.taper.start,
                              taper.year.2,
                              p.at.year.2,
                              ps.are.dynamic = F,
                              link = 'identity',
                              error.prefix = 'Cannot create logistic taper functional.form: ')
        {
            #-- ps.are.dynamic --#
            if (!is.logical(ps.are.dynamic) || length(ps.are.dynamic)!=1 || is.na(ps.are.dynamic))
                stop(paste0(error.prefix, "'ps.are.dynamic' must be either TRUE or FALSE"))
            
            #-- base.functional.form --#
            if (!is(base.functional.form, 'functional.form'))
                stop(paste0(error.prefix, "'base.functional.form' must be an object of class 'functional.form'"))
            
            #-- Years --#
            if (!is.numeric(taper.start.year) || length(taper.start.year)!=1 || is.na(taper.start.year))
                stop(paste0(error.prefix, "'taper.start.year' must be a single, non-NA numeric value"))
            if (!is.numeric(taper.year.2) || length(taper.year.2)!=1 || is.na(taper.year.2))
                stop(paste0(error.prefix, "'taper.year.2' must be a single, non-NA numeric value"))
            if (taper.year.2 <= taper.start.year)
                stop(paste0(error.prefix, "'taper.year.2' (", taper.year.2, ") must be AFTER 'taper.start.year' (", taper.start.year, ")"))
            
            #-- ps --#
            if (ps.are.dynamic)
            {
                stop(paste0(error.prefix, "We have not yet implemented dynamic ps"))
            }
            else
            {
                if (!is.numeric(p.at.taper.start) || length(p.at.taper.start)!=1 || is.na(p.at.taper.start) || 
                    p.at.taper.start <= 0 || p.at.taper.start >=1)
                    stop(paste0(error.prefix, "If ps are NOT dynamic, 'p.at.taper.start' must be a single, non-NA numeric value between 0 and 1 (exclusive)"))
                
                if (!is.numeric(p.at.year.2) || length(p.at.year.2)!=1 || is.na(p.at.year.2) || 
                    p.at.year.2 <= p.at.taper.start || p.at.year.2 >=1)
                    stop(paste0(error.prefix, "If ps are NOT dynamic, 'p.at.year.2' must be a single, non-NA numeric value between 'p.at.taper.start' (", p.at.taper.start, ") and 1 (exclusive)"))
                
                betas = list()
                beta.links = list()
                alpha.links = list()
                overwrite.ps.with.alphas = F
            }
            
            #-- link --#
            link = get.link(link,
                            min = base.functional.form$link$min,
                            max = base.functional.form$link$max)
            
            
            #-- Call the superclass constructor --#
            super$initialize(type = "logistic-taper",
                             betas = betas,
                             link = link,
                             future.slope.link = link$get.coefficient.link(),
                             alphas.are.additive = !overwrite.ps.with.alphas,
                             beta.links = beta.links,
                             alpha.links = alpha.links, 
                             is.static = F,
                             error.prefix = error.prefix)
            
            private$i.base.functional.form = base.functional.form
            private$i.ps.are.dynamic = ps.are.dynamic
            private$i.taper.start.year = taper.start.year
            private$i.taper.year.2 = taper.year.2
            private$i.taper.start.delta.year = taper.start.year + 1/52
            private$i.p.at.taper.start = p.at.taper.start
            
            if (!ps.are.dynamic)
            {
                p1 = p.at.taper.start
                p2 = p.at.year.2
                t1 = taper.start.year
                t2 = taper.year.2

                private$i.precalculated.m = m = log((1/p1 - 1) / (1/p2 - 1)) / (t2 - t1)
                private$i.precalculated.b = -log(1/p1 - 1) - m*t1
            }
        },
        
        project = function(years,
                           alphas = NULL,
                           dim.names = self$minimum.dim.names,
                           future.slope = NULL,
                           future.slope.after.year = NULL,
                           future.slope.is.on.transformed.scale=F,
                           check.consistency=T,
                           error.prefix='')
        {
            years.before.taper.mask = years <= private$i.taper.start.year
            no.need.to.taper = all(years.before.taper.mask)
            if (no.need.to.taper)
            {
                years.to.project.base = years[years.before.taper.mask]
            }
            else
            {
                years.to.project.base = union(years[years.before.taper.mask],
                                              c(private$i.taper.start.year,
                                                private$i.taper.start.delta.year))
            }
            
            private$i.base.projected.values = private$i.base.functional.form$project(years = years.to.project.base,
                                                                                     alphas = alphas,
                                                                                     dim.names = dim.names,
                                                                                     future.slope = future.slope,
                                                                                     future.slope.after.year = future.slope.after.year,
                                                                                     future.slope.is.on.transformed.scale = future.slope.is.on.transformed.scale,
                                                                                     check.consistency = check.consistency,
                                                                                     error.prefix = error.prefix)
            
            if (no.need.to.taper)
            {
                private$i.base.projected.values
            }
            else
            {
                super$project(years = years,
                              alphas = alphas,
                              dim.names = dim.names,
                              future.slope = future.slope,
                              future.slope.after.year = future.slope.after.year,
                              future.slope.is.on.transformed.scale = future.slope.is.on.transformed.scale,
                              check.consistency = check.consistency,
                              error.prefix = error.prefix)
            }
        },
        
        apply.ontology.mapping = function(mapping, 
                                          modify.in.place = F,
                                          error.prefix = error.prefix)
        {
            super$apply.ontology.mapping(mapping = mapping,
                                         modify.in.place = modify.in.place,
                                         error.prefix = error.prefix)
            private$i.base.functional.form$apply.ontology.mapping(mapping = mapping,
                                                                  modify.in.place = modify.in.place,
                                                                  error.prefix = error.prefix)
        }
    ),
    
    active = list(
        
        minimum.dim.names = function(value)
        {
            if (missing(value))
                private$i.minimum.dim.names
            else
                stop("Cannot modify 'minimum.dim.names' for a functional.form object - they are read-only")
        },
        
        betas = function(value)
        {
            if (missing(value))
                c(private$i.betas, private$i.base.functional.form$betas)
            else
                stop("Cannot modify 'betas' for a functional.form object - they are read-only")
        },
        
        link = function(value)
        {
            if (missing(value))
                private$i.link
            else
                stop("Cannot modify 'link' for a functional.form object - it is read-only")
        },
        
        alpha.names = function(value)
        {
            if (missing(value))
                names(self$betas)
            else
                stop("Cannot modify 'alpha.names' for a functional.form object - they are read-only")
        },
        
        alpha.links = function(value)
        {
            if (missing(value))
                c(private$i.alpha.links, private$i.base.functional.form$alpha.links)
            else
                stop("Cannot modify 'alpha.links' for a functional.form object - they are read-only")
        },
        
        alphas.are.additive = function(value)
        {
            if (missing(value))
                c(private$i.alphas.are.additive, private$i.base.functional.form$alphas.are.additive)
            else
                stop("Cannot modify 'alphas.are.additive' for a functional.form object - they are read-only")
        }  
    ),
    
    private = list(
        
        i.taper.start.year = NULL,
        i.taper.year.2 = NULL,
        i.base.functional.form = NULL,
        
        i.p.at.taper.start = NULL,
        
        i.ps.are.dynamic = NULL,
        i.precalculated.m = NULL,
        i.precalculated.b = NULL,
        
        i.taper.start.delta.year = NULL,
        i.base.projected.values = NULL,
        
        do.project = function(terms,
                              years,
                              future.slope,
                              future.slope.after.year,
                              dim.names,
                              check.consistency,
                              error.prefix)
        {
            years.before.taper = years[years <= private$i.taper.start.year]
            years.after.taper = years[years > private$i.taper.start.year]
            
            v1 = private$i.link$apply(private$i.base.projected.values[[as.character(private$i.taper.start.year)]])
            delta = (private$i.link$apply(private$i.base.projected.values[[as.character(private$i.taper.start.delta.year)]]) - v1) / 
                (private$i.taper.start.delta.year - private$i.taper.start.year)
            
            delta.zero = delta == 0
            
            t1 = private$i.taper.start.year
            t2 = private$i.taper.year.2
            
            if (private$i.ps.are.dynamic)
            {
                p1 = terms$p.at.taper.start
                p2 = terms$p.at.year.2
                
                if (any(p1 <= 0) || any(p1 >= 0))
                    stop(paste0(error.prefix, "The terms for 'p.at.taper.start' must all be between 0 and 1 (exclusive)"))
                if (any(p2 <= p1) || any(p2 >= 0))
                    stop(paste0(error.prefix, "The terms for 'p.at.year.2' must all be between the terms for 'p.at.taper.start' and 1 (exclusive)"))
                    
                m = log((1/p1 - 1) /(1/p2 - 1)) / (t2 - t1)
                b = -log(1/p1 - 1) - m*t1
            }
            else
            {
                m = private$i.precalculated.m
                b = private$i.precalculated.b
            }
            
            K = delta * (1 + exp(-m*t1-b))^2 / (m * exp(-m*t1-b))
            
            A = v1 - K / (1 + exp(-m*t1-b))
            
            if (private$i.link$min > -Inf)
            {
                beneath.min.mask = (delta < 0) & ( (A+K) < private$i.link$min)
                n.beneath.min = sum(beneath.min.mask)
                if (n.beneath.min > 0)
                {
                    if (private$i.ps.are.dynamic)
                        p1 = terms$p.at.taper.start[beneath.min.mask]
                    else
                        p1 = rep(private$i.p.at.taper.start, n.beneath.min)
                    
                    l = private$i.link$min
                    A.new = (v1[beneath.min.mask] - l * p1) / (1 - p1)
                    K.new = l - A.new
                    m.new = delta[beneath.min.mask] / K.new / p1^2 / (1/p1 - 1)
                    b.new = -log(1/p1 - 1) - m.new * t1
                    
                    A[beneath.min.mask] = A.new
                    K[beneath.min.mask] = K.new
                    
                    if (length(m)==1)
                        m = rep(m, length(A))
                    m[beneath.min.mask] = m.new
                    
                    if (length(b)==1)
                        b = rep(b, length(A))
                    b[beneath.min.mask] = b.new
                }
            }
            
            if (private$i.link$max < Inf)
            {
                above.max.mask = (delta < 0) & ( (A+K) > private$i.link$max)
                n.above.max = sum(above.max.mask)
                if (n.above.max > 0)
                {
                    if (private$i.ps.are.dynamic)
                        p1 = terms$p.at.taper.start[above.max.mask]
                    else
                        p1 = rep(private$i.p.at.taper.start, n.above.max)
                    
                    u = private$i.link$max
                    A.new = (v1[above.max.mask] - u * p1) / (1 - p1)
                    K.new = u - A.new
                    m.new = delta[above.max.mask] / K.new / p1^2 / (1/p1 - 1)
                    b.new = -log(1/p1 - 1) - m.new * t1
                    
                    A[above.max.mask] = A.new
                    K[above.max.mask] = K.new
                    
                    if (length(m)==1)
                        m = rep(m, length(v1))
                    m[above.max.mask] = m.new
                    
                    if (length(b)==1)
                        b = rep(b, length(v1))
                    b[above.max.mask] = b.new
                }
            }
            values.before.taper = private$i.base.projected.values[as.character(years.before.taper)]
            
            values.after.taper = lapply(years.after.taper, function(year){
                
                val = A + K / (1 + exp(-m*year-b))
            #    val[delta.zero] = v1[delta.zero]
                
                private$i.link$reverse.apply(val)
            })
            names(values.after.taper) = as.character(years.after.taper)
            
            values = c(values.before.taper, values.after.taper)
            values[as.character(years)]
        }
    )
)


##---------------------------------##
##---------------------------------##
##-- HELPERS for LOGISTIC SPLINE --##
##---------------------------------##
##---------------------------------##

calculate.change.ratios.two.logistic <- function(r1,r2,
                                                 times=0:t2,
                                                 r0=1,
                                                 t0=0,t1=5,t2=10,
                                                 fraction.of.asymptote.after.end=0.05,
                                                 fraction.of.asymptote.before.start=0.025,
                                                 fraction.of.asymptote.for.change.dir=0.02)
{
    r0 = round(r0, 10)
    r1 = round(r1, 10)
    r2 = round(r2, 10)
    
    if ((fraction.of.asymptote.after.end+fraction.of.asymptote.before.start)>=1)
        stop("The sum of fraction.of.asymptote.after.end and fraction.of.asymptote.before.start must be less than 1")
    
    if (r0==0 && r1==0)
        r0.to.1 = 1
    else
        r0.to.1 = r1/r0
    
    if (r0==0 && r2==0)
        r0.to.2 = 1
    else
        r0.to.2 = r2/r0
    
    if (r1==0 && r2==0)
        r1.to.2 = 1
    else if (r0==0)
    {
        if (r1==0 && r2==0)
            r1.to.2 = 0
        else
            r1.to.2 = r2/r1
    }
    else
        r1.to.2 = r0.to.2 / r0.to.1
    
    if (r0.to.1 == 1 && r1.to.2 == 1)
    {
        model.1 = model.2 = get.no.change.logistic.model(r0)
    }
    else if ((r0.to.1 > 1 && r1.to.2 > 1) ||
             (r0.to.1 < 1 && r1.to.2 < 1)) #both in the same directions
    {
        delta.K.A = (r2 - r0) / (1 - fraction.of.asymptote.for.change.dir - fraction.of.asymptote.before.start)
        A.overall = r0 - delta.K.A * fraction.of.asymptote.before.start
        K.overall = r2 + delta.K.A * fraction.of.asymptote.for.change.dir
        
        first.is.smaller = abs(r1-r0) < abs(r2-r1)
        model.1 = fit.logistic.model(r1=r1, t1=t1, A=A.overall, K=K.overall, r0=r0, t0=t0)
        model.2 = fit.logistic.model(r1=r2, t1=t2, A=A.overall, K=K.overall, r0=r1, t0=t1)
        
        if (get.logistic.slope(model.1, t1)==0 || get.logistic.slope(model.2, t1)==0)
        {}
        else if (first.is.smaller)
        {
            A.2 = r1 + (A.overall-r1) * get.logistic.slope(model.1, t1) / get.logistic.slope(model.2, t1)
            model.2 = fit.logistic.model(r1=r2, t1=t2, A=A.2, K=K.overall, r0=r1, t0=t1)
        }
        else
        {
            K.1 = r1 + (K.overall-r1) * get.logistic.slope(model.2, t1) / get.logistic.slope(model.1, t1)
            model.1 = fit.logistic.model(r1=r1, t1=t1, A=A.overall, K=K.1, r0=r0, t0=t0)
        }
    }
    else #change direction
    {
        if (r0.to.1==1)
            model.1 = get.no.change.logistic.model(r0)
        else
        {
            delta.K.A = (r1 - r0) / (1 - fraction.of.asymptote.for.change.dir - fraction.of.asymptote.before.start)
            A = r0 - delta.K.A * fraction.of.asymptote.before.start
            K = r1 + delta.K.A * fraction.of.asymptote.for.change.dir
            
            model.1 = fit.logistic.model(r1=r1, t1=t1, A=A, K=K, r0=r0, t0=t0)
        }
        
        if (r1.to.2==1)
            model.2 = get.no.change.logistic.model(r1)
        else
        {
            delta.K.A = (r2 - r1) / (1 - fraction.of.asymptote.after.end - fraction.of.asymptote.for.change.dir)
            A = r1 - delta.K.A * fraction.of.asymptote.for.change.dir
            K = r2 + delta.K.A * fraction.of.asymptote.after.end
            
            model.2 = fit.logistic.model(r1=r2, t1=t2, A=A, K=K, r0=r1, t0=t1)
        }
    }
    
    times.1 = times[times<t1]
    times.2 = setdiff(times, times.1)
    
    c(get.logistic.points(model.1, times.1), get.logistic.points(model.2, times.2))
}

calculate.change.ratios.logistic <- function(r0=1,r1,
                                             times=t0:t1,
                                             t0=0,t1=5,
                                             fraction.of.asymptote.after.end=0.05,
                                             fraction.of.asymptote.before.start=0.025)
{
    r0 = round(r0, 10)
    r1 = round(r1, 10)
    
    if ((fraction.of.asymptote.after.end+fraction.of.asymptote.before.start)>=1)
        stop("The sum of fraction.of.asymptote.after.end and fraction.of.asymptote.before.start must be less than 1")
    
    r0.to.1 = r1/r0
    
    if (r0.to.1 == 1)
        model.1 = get.no.change.logistic.model(r0)
    else
    {
        delta.K.A = (r1 - r0) / (1 - fraction.of.asymptote.after.end - fraction.of.asymptote.before.start)
        A.overall = r0 - delta.K.A * fraction.of.asymptote.before.start
        K.overall = r1 + delta.K.A * fraction.of.asymptote.after.end
        
        model.1 = fit.logistic.model(r1=r1, t1=t1, A=A.overall, K=K.overall, r0=r0, t0=t0)
    }
    
    get.logistic.points(model.1, times)
}

calculate.change.ratios.logistic.array <- function(r0.arr,
                                                   r1.arr,
                                                   times=t0:t1,
                                                   t0=0,t1=5,
                                                   fraction.of.asymptote.after.end=0.05,
                                                   fraction.of.asymptote.before.start=0.025)
{
    if (any(dim(r0.arr) != dim(r1.arr)))
        stop("r0.arr and r1.arr must have the same dimensions")
    
    if ((fraction.of.asymptote.after.end+fraction.of.asymptote.before.start)>=1)
        stop("The sum of fraction.of.asymptote.after.end and fraction.of.asymptote.before.start must be less than 1")
    
    if (all(r0.arr==r1.arr))
        return (lapply(times, function(i){r0.arr}))
    
    models = lapply(1:length(r0.arr), function(i){
        
        r0 = round(r0.arr[i], 10)
        r1 = round(r1.arr[i], 10)
        
        r0.to.1 = r1/r0
        
        if (r0.to.1 == 1)
            model.1 = get.no.change.logistic.model(r0)
        else
        {
            delta.K.A = (r1 - r0) / (1 - fraction.of.asymptote.after.end - fraction.of.asymptote.before.start)
            A.overall = r0 - delta.K.A * fraction.of.asymptote.before.start
            K.overall = r1 + delta.K.A * fraction.of.asymptote.after.end
            
            model.1 = fit.logistic.model(r1=r1, t1=t1, A=A.overall, K=K.overall, r0=r0, t0=t0)
        }
        
        model.1
    })
    
    lapply(times, function(t){
        arr = sapply(models, get.logistic.points, times=t)
        dim(arr) = dim(r0.arr)
        dimnames(arr) = dimnames(r0.arr)
        arr
    })
}


##-- LOWER-LEVEL LOGISTIC MODELS --##

get.logistic.points <- function(logistic.model, times)
{
    rv = logistic.model$A + (logistic.model$K - logistic.model$A) /
        (1 + logistic.model$Q*exp(-logistic.model$B * (times-logistic.model$t0)))
    names(rv) = times
    rv
}

get.logistic.slope <- function(logistic.model, times)
{
    rv = (logistic.model$K - logistic.model$A) *
        logistic.model$B * logistic.model$Q * exp(-logistic.model$B * times) /
        (1 + logistic.model$Q * exp(-logistic.model$B * (times-logistic.model$t0)))^2
    names(rv) = times
    rv
}

fit.logistic.model <- function(r1, t1, A, K, r0=1, t0=0)
{
    # Solve for Q, B
    Q = (K-r0)/(r0-A)
    B = (log((r1-A)/(K-r1)) + log(Q)) / (t1-t0)
    if (is.na(B))
        stop("NA values generated in fitting logistic model")
    
    
    # Store in an object and return
    list(K=K,
         A=A,
         B=B,
         Q=Q,
         t0=t0)
}

get.no.change.logistic.model <- function(r0=1)
{
    list(K=r0,
         A=0,
         Q=0,
         B=0,
         t0=0)
}


# f(t) = A + (K - A) / (1 + Q*e^(-B*t))
smooth.logistic.one.point <- function(r1, t1, times=t0:t1, r0=1, t0 = 0,
                                      A=NA, K=NA,
                                      r.span.of.total=0.9, min.r=0, max.r=Inf)
{
    if (is.na(A) || is.na(K))
    {
        # Set up upper and lower asymptotes
        r.span = abs(r1-r0)
        r.span.total = r.span / r.span.of.total
        r.extra = r.span.total - r.span
        
        A = min(r1, r0) - r.extra/2
        A = max(min.r, A)
        
        K = max(r1, r0) + r.extra/2
        K = min(max.r, K)
    }
    
    # Plug and chug
}

