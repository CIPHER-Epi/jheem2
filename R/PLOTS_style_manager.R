#' @title Create A Style Manager
#' @param sim.palette,data.palette Must be either a character vector of colors to choose from or a function that takes a number of colors needed and returns a character vector of colors.
#' @param shapes A numeric vector indicating which of the 25 ggplot shapes to select from. Each value must be an integer between 1 and 25.
#' @param shade.increment A single numeric value less than 0 indicating the extent of shading to apply (to alternate values of the 'shade.data.by'/'shade.sim.by' variables)
#' @param alpha.line A single numeric value which determines the transparency of the simulation lines. If NULL, the width of the lines is used (see "linewidth.slope" argument for details).
#' @param linewidth.baseline A single numeric value greater than zero which determines the simset linewidth in combination with the "linewidth.slope" argument.
#' @param linewidth.slope A single numeric value specifying how simset linewidth will be determined for based on how many simulations it has (its "n.sim").
#' The formula used is linewidth = baseline/(slope * log10(n.sim) + 1), where "baseline" is the argument "linewidth.baseline". If this slope is the default of 3, and linewidth.baseline is the default of 1, this will result in simsets with 1000 simulations having a linewidth of 0.1
#' while simsets with only 1 simulation will have a linewidth of 1. If you would like all simsets to plot with the same thickness, use a slope of 0.
#'
#'
#' @export
create.style.manager = function(color.sim.by = 'stratum',
                                color.data.by = 'stratum', # can change it to location.type
                                shade.data.by = 'location',
                                linetype.sim.by = 'simset',
                                shape.data.by = 'source',
                                shape.sim.by = 'simset',
                                sim.palette = ggsci::pal_jama(),
                                data.palette = ggsci::pal_jama(),
                                linetypes = c("solid", "dashed", "dotted","dotdash"),
                                shapes = 21:25,
                                shade.increment = -64,
                                alpha.ribbon = 0.2,
                                alpha.line = 1,
                                linewidth.baseline = 1,
                                linewidth.slope = 3)
{
    JHEEM.STYLE.MANAGER$new(color.sim.by = color.sim.by, 
                            color.data.by = color.data.by,
                            shade.data.by = shade.data.by,
                            linetype.sim.by = linetype.sim.by,
                            shape.data.by = shape.data.by,
                            shape.sim.by = shape.sim.by,
                            sim.palette = sim.palette,
                            data.palette = data.palette,
                            linetypes = linetypes,
                            shapes = shapes,
                            shade.increment = shade.increment,
                            alpha.ribbon = alpha.ribbon,
                            alpha.line = alpha.line,
                            linewidth.baseline = linewidth.baseline,
                            linewidth.slope = linewidth.slope)
}

#' @title Get the Default Style Manager
#'@export
get.default.style.manager = function(type=c('ggplot','plotly')[1])
{
    if (!is.character(type) || length(type)!=1 || is.na(type))
        stop("Cannot get.default.style.manager() - 'type' must be a single, non-NA, character value")
    if (type!='ggplot' && type!='plotly')
        stop("Cannot get.default.style.manager() - 'type' must be either 'ggplot' or")
    
    DEFAULT.STYLE.MANAGERS[[type]]
}

#' @title Set the Default Style Manager
#'@export
set.default.style.manager = function(style.manager,
                                     type=c('ggplot','plotly')[1])
{
    if (!is.character(type) || length(type)!=1 || is.na(type))
        stop("Cannot set.default.style.manager() - 'type' must be a single, non-NA, character value")
    if (type!='ggplot' && type!='plotly')
        stop("Cannot set.default.style.manager() - 'type' must be either 'ggplot' or 'plotly'")
    
    if (!is(style.manager, 'jheem.style.manager'))
            stop("Cannot set.default.style.manager() - 'style.manager' must be an object of class 'jheem.style.manager', as created by the function create.style.manager()")
    
    DEFAULT.STYLE.MANAGERS[[type]] = style.manager
}

# Possiblities for <x>.sim.by are
#   - "simulation" - each simulation set gets a different value for the attribute <x>
#   - "stratum" - each stratum - ie, level of 'split.by' - gets a different value
# Possibilites for <x>.data.by are
#   - "source" - each data source from the data manager gets a different value for the attribute <x>
#   - "location" - each data location gets a different value
#   - "location.type" - each type of location in the data (as given by locations::get.location.type) gets a different value
#   - "stratum" - each stratum - ie level of split by - gets a different value
JHEEM.STYLE.MANAGER = R6::R6Class(
    'jheem.style.manager',
    
    public = list(
        
        initialize = function(color.sim.by,
                              color.data.by,
                              shade.data.by,
                              linetype.sim.by,
                              shape.data.by,
                              shape.sim.by,
                              sim.palette,
                              data.palette,
                              linetypes,
                              shapes,
                              shade.increment,
                              alpha.ribbon,
                              alpha.line,
                              linewidth.baseline,
                              linewidth.slope)
        {
            # need to do error checking
            error.prefix <- "Error: cannot create JHEEM.STYLE.MANAGER: "
            
            if (!(is.character(color.sim.by) && length(color.sim.by)==1 && !is.na(color.sim.by) &&
                  color.sim.by %in% c("simset", "stratum")))
                stop(paste0(error.prefix, "'color.sim.by' must be one of 'simset' or 'stratum'"))
            if (!(is.character(linetype.sim.by) && length(linetype.sim.by)==1 && !is.na(linetype.sim.by) &&
                  linetype.sim.by %in% c("simset", "stratum")))
                stop(paste0(error.prefix, "'color.sim.by' must be one of 'simset' or 'stratum'"))
            if (!(is.character(color.data.by) && length(color.data.by)==1 && !is.na(color.data.by) &&
                  color.data.by %in% c("source", "stratum", "location.type", "location")))
                stop(paste0(error.prefix, "'color.data.by' must be one of 'source', 'stratum', 'location', or 'location.type'"))
            if (!(is.null(shade.data.by) || (is.character(shade.data.by) && length(shade.data.by)==1 && !is.na(shade.data.by)) &&
                  shade.data.by %in% c("source", "stratum", "location.type", "location")))
                stop(paste0(error.prefix, "'shade.data.by' must be either NULL or one of 'source', 'stratum', 'location', or 'location.type'"))
            if (!(is.character(shape.data.by) && length(shape.data.by)==1 && !is.na(shape.data.by) &&
                  shape.data.by %in% c("source", "stratum", "location.type", "location")))
                stop(paste0(error.prefix, "'shape.data.by' must be one of 'source', 'stratum', 'location', or 'location.type'"))
            
            if (!is.function(sim.palette) && !(is.character(sim.palette) && length(sim.palette)>0 && !any(is.na(sim.palette))))
                stop(paste0(error.prefix, "'sim.palette' must be either a function or a character vector with no missing elements"))
            if (!is.function(data.palette) && !(is.character(data.palette) && length(data.palette)>0 && !any(is.na(data.palette))))
                stop(paste0(error.prefix, "'data.palette' must be either a function or a character vector with no missing elements"))
            
            
            
            if (!(is.character(linetypes) && length(linetypes)>0 && !any(is.na(linetypes))))
                stop(paste0(error.prefix, "'linetypes' must be a character vector with no missing values"))
            if (!(is.numeric(shapes) && length(shapes) > 0 && all(shapes %in% 1:25)))
                stop(paste0(error.prefix, "'shapes' must be a numeric vector with each value being an integer between 1 and 25"))
            if (!(is.numeric(shade.increment) && length(shade.increment)==1 && !is.na(shade.increment) && shade.increment<0))
                stop(paste0(error.prefix, "'shade.increment' must be a single numeric value less than 0"))
            
            if (!(is.numeric(alpha.ribbon) && length(alpha.ribbon)==1 && !is.na(alpha.ribbon) && alpha.ribbon>0)) {
                stop(paste0(error.prefix, "'alpha.ribbon' must be a single numeric value greater than 0"))
            }
            if (!(is.null(alpha.line) || (is.numeric(alpha.line) && length(alpha.line)==1 && !is.na(alpha.line) && alpha.line>0))) {
                stop(paste0(error.prefix, "'alpha.line' must be NULL or a single numeric value greater than 0"))
            }
            if (!(is.numeric(linewidth.baseline) && length(linewidth.baseline)==1 && !is.na(linewidth.baseline) && linewidth.baseline>0)) {
                stop(paste0(error.prefix, "'linewidth.baseline' must be a single numeric value greater than 0"))
            }
            if (!(is.numeric(linewidth.slope) && length(linewidth.slope)==1 && !is.na(linewidth.slope))) {
                stop(paste0(error.prefix, "'linewidth.slope' must be a single numeric value"))
            }
            
            private$i.color.sim.by = color.sim.by
            private$i.color.data.by = color.data.by
            private$i.shade.data.by = shade.data.by
            private$i.shape.data.by = shape.data.by
            private$i.shape.sim.by = shape.sim.by
            private$i.linetype.sim.by = linetype.sim.by
            
            private$i.sim.palette = sim.palette
            private$i.data.palette = data.palette
            private$i.linetypes = linetypes
            private$i.shapes = shapes
            private$i.shade.increment = shade.increment
            private$i.alpha.ribbon = alpha.ribbon
            private$i.alpha.line = alpha.line
            private$i.linewidth.baseline = linewidth.baseline
            private$i.linewidth.slope = linewidth.slope
        },
        
        get.sim.colors = function(n)
        {
            private$do.get.color(palette = private$i.sim.palette, n = n)
        },
        
        get.data.colors = function(n)
        {
            private$do.get.color(palette = private$i.data.palette, n = n)
        },
        
        get.shades = function(base.color, n)
        {
            rgb.base = col2rgb(base.color)
            
            reds = pmin(255, pmax(0, rgb.base[1,1] + (1:n-1)*private$i.shade.increment))
            greens = pmin(255, pmax(0, rgb.base[2,1] + (1:n-1)*private$i.shade.increment))
            blues = pmin(255, pmax(0, rgb.base[3,1] + (1:n-1)*private$i.shade.increment))
            
            rv = sapply(1:n, function(i){
                rgb(red=reds[i]/255,
                    green=greens[i]/255,
                    blue=blues[i]/255,
                    alpha=1)
            })
            
            substr(rv, 1, 7)
        },
        
        get.linetypes = function(n, for.plotly=F)
        {
            private$do.get.from.vector(values = private$i.linetypes, n = n)
        },
        
        get.shapes = function(n, for.plotly=F)
        {
            private$do.get.from.vector(values = private$i.shapes, n = n)
        }
    ),
    
    active = list(
        
        color.sim.by = function(value)
        {
            if (missing(value))
                private$i.color.sim.by
            else
                stop("Cannot overwrite a style.manager's 'color.sim.by' - it is read only")
        },
        
        color.data.by = function(value)
        {
            if (missing(value))
                private$i.color.data.by
            else
                stop("Cannot overwrite a style.manager's 'color.sim.by' - it is read only")
        },
        
        shade.data.by = function(value)
        {
            if (missing(value))
                private$i.shade.data.by
            else
                stop("Cannot overwrite a style.manager's 'color.sim.by' - it is read only")
        },
        
        linetype.sim.by = function(value)
        {
            if (missing(value))
                private$i.linetype.sim.by
            else
                stop("Cannot overwrite a style.manager's 'color.sim.by' - it is read only")
        },
        
        shape.data.by = function(value)
        {
            if (missing(value))
                private$i.shape.data.by
            else
                stop("Cannot overwrite a style.manager's 'color.sim.by' - it is read only")
        },
        
        shape.sim.by = function(value)
        {
            if (missing(value))
                private$i.shape.sim.by
            else
                stop("Cannot overwrite a style.manager's 'shape.sim.by' - it is read only")
        },
        
        alpha.ribbon = function(value)
        {
            if (missing(value))
                private$i.alpha.ribbon
            else
                stop("Cannot overwrite a style.manager's 'alpha.ribbon' - it is read only")
        },
        
        alpha.line = function(value)
        {
            if (missing(value))
                private$i.alpha.line
            else
                stop("Cannot overwrite a style.manager's 'alpha.line' - it is read only")
        },
        
        linewidth.baseline = function(value)
        {
            if (missing(value))
                private$i.linewidth.baseline
            else
                stop("Cannot overwrite a style.manager's 'linewidth.baseline' - it is read only")
        },
        
        linewidth.slope = function(value)
        {
            if (missing(value))
                private$i.linewidth.slope
            else
                stop("Cannot overwrite a style.manager's 'linewidth.slope' - it is read only")
        },
        
        shade.increment = function(value)
        {
            if (missing(value))
                private$i.shade.increment
            else
                stop("Cannot overwrite a style.manager's 'shade.increment' - it is read only")
        }
    ),
    
    private = list(
        
        i.color.sim.by = NULL,
        i.color.data.by = NULL,
        i.shade.data.by = NULL,
        i.linetype.sim.by = NULL,
        i.shape.data.by = NULL,
        i.shape.sim.by= NULL,
        
        i.sim.palette = NULL,
        i.data.palette = NULL,
        i.linetypes = NULL,
        i.shapes = NULL,
        i.shade.increment = NULL,
        i.alpha.ribbon = NULL,
        i.alpha.line = NULL,
        i.linewidth.baseline = NULL,
        i.linewidth.slope = NULL,
        
        do.get.from.vector = function(values, n)
        {
            if (length(values)==0)
                stop("The values must have at least one element")
            
            rep(values, ceiling(n/length(values)))[1:n]
        },
        
        do.get.color = function(palette, n)
        {
            if (is.character(palette))
                private$do.get.from.vector(values = palette, n = n)
            else if (is.function(palette))
            {
                colors = palette(n)
                if (!is.character(colors))
                    stop("The palette should return a character vector")
                private$do.get.from.vector(values = colors, n = n)
            }
            else
                stop("'palette' must be either a character vector or a function that takes ")
        }
    )
)



DEFAULT.STYLE.MANAGERS = new.env()
set.default.style.manager(create.style.manager(), 'ggplot')
# set.default.style.manager(create.style.manager(), 'plotly')
