
##--------------------##
##-- SOME CONSTANTS --##
##--------------------##

MCMC.SUB.DIRECTORY = 'mcmc_runs'
MCMC.SUMMARY.SUB.DIRECTORY = 'mcmc_summaries'
SIMULATION.SUB.DIRECTORY = 'simulations'

MIN.SPECIFICATION.VERSION.NCHAR = 2
MAX.SPECIFICATION.VERSION.NCHAR = 12

MIN.SPECIFICATION.SUBVERSION.NCHAR = 1
MAX.SPECIFICATION.SUBVERSION.NCHAR = 3

MINIMUM.INTERVENTION.CODE.NCHAR = 2
MAXIMUM.INTERVENTION.CODE.NCHAR = 25

MINIMUM.CALIBRATION.CODE.NCHAR = 2
MAXIMUM.CALIBRATION.CODE.NCHAR = 25

##--------------------##
##-- ROOT DIRECTORY --##
##--------------------##

JHEEM.ROOT.DIR.STORAGE = new.env()
JHEEM.ROOT.DIR.STORAGE$dir = NULL

#' @title Set JHEEM Root Directory
#' @description
#' Set the directory that calibration directories and files will be written into.
#' 
#'@export
set.jheem.root.directory <- function(dir)
{
    if (!is.character(dir) || length(dir)!=1 || is.na(dir))
        stop("Cannot set.jheem.root.directory() - 'dir' must be a single, non-NA character value")
    
    JHEEM.ROOT.DIR.STORAGE$dir = dir
}

#' @title Get JHEEM Root Directory
#' #' @description
#' Gets the directory that calibration directories and files will be written into,
#' if it has been set with "set.jheem.root.directory"
#'@export
get.jheem.root.directory <- function(error.prefix='')
{
    if (!is.character(error.prefix) || length(error.prefix)!=1 || is.na(error.prefix))
        stop("Cannot get.jheem.root.directory() - 'error.prefix' must be a single, non-NA character value")
    
    if (is.null(JHEEM.ROOT.DIR.STORAGE$dir))
        stop(paste0(error.prefix, "No jheem root directory has been set - use set.jheem.root.directory() to do so"))
    
    JHEEM.ROOT.DIR.STORAGE$dir
}

##----------------------------##
##-- SIMSET FILENAME / PATH --##
##----------------------------##

#' @title Get Simset Filename and Directory
#'@export
get.simset.filename.and.dir <- function(version,
                                        location,
                                        calibration.code,
                                        n.sim,
                                        intervention.code = NULL,
                                        sub.version = NULL,
                                        root.dir = get.jheem.root.directory("Cannot get.simset.filename(): "),
                                        error.prefix = '')
{
    #-- Validate arguments --#
    
    if (!is.character(error.prefix) || length(error.prefix)!=1 || is.na(error.prefix))
        stop("Cannot get.simset.filename() - 'error.prefix' must be a single, non-NA character value")
    
    error.prefix = paste0(error.prefix, "Cannot get simset filename - ")
    
    # version
    if (!is.character(version) || length(version)!=1 || is.na(version))
        stop(paste0(error.prefix, "'version' must be a single, non-NA character value"))
    
    # sub-version
    if (is.null(sub.version))
    {}
    else if (!is.character(sub.version) || length(sub.version)!=1 || is.na(sub.version))
        stop(paste0(error.prefix, "'sub.version' must be either NULL or a single, non-NA character value"))
    
    # calibration.code
    if (is.null(calibration.code))
    {}
    else if (!is.character(calibration.code) || length(calibration.code)!=1 || is.na(calibration.code))
        stop(paste0(error.prefix, "'calibration.code' must be either NULL or a single, non-NA character value"))
    
    # n.sim
    if (!is.numeric(n.sim) || length(n.sim)!=1 || is.na(n.sim) || n.sim<=0 || round(n.sim)!=n.sim)
        stop(paste0(error.prefix, "'n.sim' must be a single, non-NA, positive integer value"))
    
    # location
    if (!is.character(location) || length(location)!=1 || is.na(location))
        stop(paste0(error.prefix, "'location' must be a single, non-NA character value"))
    
    # intervention.code
    if (is.null(intervention.code))
    {}
    else 
    {
        if (!is.character(intervention.code) || length(intervention.code)!=1 || is.na(intervention.code))
            stop(paste0(error.prefix, "'intervention.code' must be either NULL or a single, non-NA character value"))
        
        if (is.intervention.code.temporary(intervention.code))
            stop(paste0(error.prefix, "'", intervention.code, "' is a temporary intervention code, and cannot be used for saving or loading simulation sets"))
    }
    
    # root.dir
    if (!is.character(root.dir) || length(root.dir)!=1 || is.na(root.dir))
        stop(paste0(error.prefix, "'root.dir' must be a single, non-NA character value"))
    
    
    #-- Build elements out of these blocks --#
    
    version.element = version
    if (!is.null(sub.version))
        version.element = paste0(version, '-', sub.version)
    
    if (is.null(calibration.code))
        calibration.element = 'manual'
    else calibration.element = calibration.code
    calibration.element = paste0(calibration.element, '-', n.sim)
    
    location.element = location
    
    if (is.null(intervention.code))
        intervention.element = 'baseline'
    else
        intervention.element = intervention.code
    
    
    #-- Put it together --#
    
    filename = paste0(version.element, "_",
                      calibration.element, "_",
                      location.element, "_",
                      intervention.element, ".Rdata")
    
    dir = file.path(root.dir, 
                    SIMULATION.SUB.DIRECTORY,
                    version.element,
                    calibration.element,
                    location.element)
    
    list(
        filename = filename,
        dir = dir
    )
}

#' @title Get Simset Filename
#'@export
get.simset.filename <- function(version,
                                location,
                                calibration.code,
                                n.sim,
                                intervention.code = NULL,
                                sub.version = NULL,
                                include.path = T,
                                root.dir = get.jheem.root.directory("Cannot get.simset.filename(): "),
                                error.prefix = '')
{
    filename.and.dir = get.simset.filename.and.dir(version = version,
                                                   sub.version = sub.version,
                                                   calibration.code = calibration.code,
                                                   n.sim = n.sim,
                                                   location = location,
                                                   intervention.code = intervention.code,
                                                   root.dir = root.dir,
                                                   error.prefix = error.prefix)
    
    if (include.path)
        file.path(filename.and.dir$dir, filename.and.dir$filename)
    else
        filename.and.dir$filename
}

#' @title Retrieve a Saved Simulation Set
#' @description
#' Loads a simulation set that has been properly saved in JHEEM's calibration
#' file structure.
#' 
#'@export
retrieve.simulation.set <- function(version,
                                    location,
                                    calibration.code,
                                    n.sim,
                                    intervention.code = NULL,
                                    sub.version = NULL,
                                    root.dir = get.jheem.root.directory("Cannot get.simset.filename(): "),
                                    error.prefix = '',
                                    throw.error.if.missing=T)
{
    filename = get.simset.filename(version = version,
                                   sub.version = sub.version,
                                   calibration.code = calibration.code,
                                   n.sim = n.sim,
                                   location = location,
                                   intervention.code = intervention.code,
                                   include.path = T,
                                   root.dir = root.dir,
                                   error.prefix = error.prefix)
    
    if (file.exists(filename))
        load.simulation.set(filename)
    else if (throw.error.if.missing)
        stop(paste0("Cannot retrieve simulation set: no simulations are saved at '", filename, "'"))
    else
        NULL
}

#' @title Parse Simset Filename
#' @description
#' Parses the calibration information that was compressed into the filename of a
#' properly saved calibration simset.
#' @value List with version, sub.version, calibration code
#' 
#'@export
parse.simset.filename <- function(filename, throw.error.if.malformed=T)
{
    if (!is.character(filename) || length(filename)!=1 || is.na(filename))
        stop("Cannot parse.simset.filename() - 'filename' must be a single, non-NA character value")
    
    elements = strsplit(filename, split = '_')[[1]]
    
    if (length(elements)!=4)
    {
        if (throw.error.if.malformed)
            stop("Cannot parse.simset.filename() - 'filename' is not a valid simset filename")
        else
            return (NULL)
    }
    
    # version and sub-version
    version.element = elements[[1]]
    split.version = strsplit(version.element, split="-")[[1]]
    
    version = split.version[1]
    if (length(split.version)>1)
        sub.version = split.version[2]
    else
        sub.version = NULL
    
    # calibration and n.sim
    calibration.element = elements[[2]]
    split.calibration = strsplit(calibration.element, split="-")[[1]]
    
    if (length(split.calibration)!=2)
    {
        if (throw.error.if.malformed)
            stop("Cannot parse.simset.filename() - 'filename' is not a valid simset filename")
        else
            return (NULL)
    }
        
    calibration.code = split.calibration[1]
    if (calibration.code=='manual')
        calibration.code = NULL
        
    n.sim = suppressWarnings(as.numeric(split.calibration[2]))
    if (is.na(n.sim))
    {
        if (throw.error.if.malformed)
            stop("Cannot parse.simset.filename() - 'filename' is not a valid simset filename")
        else
            return (NULL)
    }
    
    # location
    location = elements[3]
    
    # intervention code
    intervention.code = elements[4]
    intervention.code = substr(intervention.code, 1, nchar(intervention.code)-6) #get rid of the ".Rdata" suffix
    
    if (intervention.code=='baseline')
        intervention.code = NULL
        
    # Package up and return
    list(
        version = version,
        sub.version = sub.version,
        calibration.code = calibration.code,
        n.sim = n.sim,
        location = location,
        intervention.code = intervention.code
    )
}

#'@title Save a JHEEM Simulation Set
#'
#'@param simset A jheem.simulation.set object
#'@param root.dir The directory in which the folder tree for saving the simset will be rooted
#'
#'@export
save.simulation.set <- function(simset,
                                root.dir = get.jheem.root.directory("Cannot save simulation set: "))
{
    if (!is(simset, 'jheem.simulation.set'))
        stop("Cannot save simulation set: 'simset' must be an object of class 'jheem.simulation.set'")
    
    filename.and.dir = get.simset.filename.and.dir(version = simset$version,
                                                   sub.version = simset$sub.version,
                                                   calibration.code = simset$calibration.code,
                                                   n.sim = simset$n.sim,
                                                   location = simset$location,
                                                   intervention.code = simset$intervention.code,
                                                   root.dir = root.dir,
                                                   error.prefix = 'Cannot save simulation set: ')
    
    
    if (!dir.exists(filename.and.dir$dir))
        dir.create(filename.and.dir$dir, recursive = T)
    
    save(simset, file=file.path(filename.and.dir$dir, filename.and.dir$filename))
}

infer.stored.simset.n.sim <- function(version = version,
                                      sub.version = sub.version,
                                      calibration.code = calibration.code,
                                      root.dir = get.jheem.root.directory(),
                                      error.prefix = "")
{
    if (!is.character(error.prefix) || length(error.prefix)!=1 || is.na(error.prefix))
        stop("Cannot infer.stored.simset.n.sim() - 'error.prefix' must be a single, non-NA character value")
    
    # version
    if (!is.character(version) || length(version)!=1 || is.na(version))
        stop(paste0(error.prefix, "'version' must be a single, non-NA character value"))
    
    # sub-version
    if (is.null(sub.version))
    {}
    else if (!is.character(sub.version) || length(sub.version)!=1 || is.na(sub.version))
        stop(paste0(error.prefix, "'sub.version' must be either NULL or a single, non-NA character value"))
    
    # calibration.code
    if (is.null(calibration.code))
    {}
    else if (!is.character(calibration.code) || length(calibration.code)!=1 || is.na(calibration.code))
        stop(paste0(error.prefix, "'calibration.code' must be either NULL or a single, non-NA character value"))
    
    version.element = version
    if (!is.null(sub.version))
        version.element = paste0(version, '-', sub.version)
    
    dir = file.path(root.dir, SIMULATION.SUB.DIRECTORY, version.element)

    if (dir.exists(dir))
    {
        sub.dirs = setdiff(list.dirs(dir, full.names = F, recursive = F), '')
        if (length(sub.dirs)==0)
            NULL
        else
        {
            sapply(sub.dirs, function(sub.dir){
                split.sub = strsplit(sub.dir, '-')[[1]]
                if (length(split.sub)==2)
                    suppressWarnings(as.numeric(split.sub[2]))
                else
                    NA
            })
        }
    }
    else
        NULL
}

##---------------------------##
##-- CALIBRATION FILE PATH --##
##---------------------------##

get.calibration.dir <- function(version, 
                                location, 
                                calibration.code, 
                                root.dir = get.jheem.root.directory())
{
    file.path(root.dir, 
              MCMC.SUB.DIRECTORY, 
              version,
              calibration.code,
              location)
}

get.mcmc.summary.file <- function(version,
                                  location,
                                  calibration.code, 
                                  root.dir = get.jheem.root.directory())
{
    filename = paste0("summary_",
                      version, "_",
                      location, "_",
                      calibration.code,
                      ".Rdata")
    file.path(root.dir, 
              MCMC.SUMMARY.SUB.DIRECTORY, 
              version,
              calibration.code,
              filename)
}

get.transmute.calibration.dir <- function(to.version,
                                          location,
                                          transmute.code,
                                          n.sim,
                                          to.sub.version = NULL,
                                          root.dir = get.jheem.root.directory())
{
    if (is.null(to.sub.version))
        version.path = to.version
    else
        version.path = paste0(to.version, "-", to.sub.version)
    
    file.path(root.dir, 
              MCMC.SUB.DIRECTORY, 
              version.path,
              paste0(transmute.code, '-', n.sim),
              location)
}

get.transmute.calibration.control.file <- function(to.version,
                                                   location,
                                                   transmute.code,
                                                   n.sim,
                                                   to.sub.version = NULL,
                                                   root.dir = get.jheem.root.directory())
{
    file.path(
        get.transmute.calibration.dir(to.version = to.version,
                                      location = location,
                                      transmute.code = transmute.code,
                                      n.sim = n.sim,
                                      to.sub.version = to.sub.version,
                                      root.dir = root.dir),
        'control.Rdata'
    )
}

get.transmute.calibration.chunk.files <- function(to.version,
                                                  location,
                                                  transmute.code,
                                                  n.sim = n.sim,
                                                  chunks,
                                                  to.sub.version = NULL,
                                                  root.dir = get.jheem.root.directory())
{
    file.path(
        get.transmute.calibration.dir(to.version = to.version,
                                      location = location,
                                      transmute.code = transmute.code,
                                      n.sim = n.sim,
                                      to.sub.version = to.sub.version,
                                      root.dir = root.dir),
        paste0("chunk_", chunks, "_data.Rdata")
    )
}

get.transmute.calibration.chunk.mcmc.settings.files <- function(to.version,
                                                                location,
                                                                transmute.code,
                                                                n.sim = n.sim,
                                                                chunks,
                                                                to.sub.version = NULL,
                                                                root.dir = get.jheem.root.directory())
{
    file.path(
        get.transmute.calibration.dir(to.version = to.version,
                                      location = location,
                                      transmute.code = transmute.code,
                                      n.sim = n.sim,
                                      to.sub.version = to.sub.version,
                                      root.dir = root.dir),
        paste0("chunk_", chunks, "_mcmc.settings.Rdata")
    )
}

##------------------------------------##
##-- VALIDATE ELEMENTS OF FILE PATH --##
##------------------------------------##

validate.version.code <- function(code, error.prefix, code.name.for.error = 'version')
{
    validate.jheem.file.path.element(code = code,
                                     code.name.for.error = code.name.for.error,
                                     allow.dashes = F,
                                     min.n.char = MIN.SPECIFICATION.VERSION.NCHAR,
                                     max.n.char = MAX.SPECIFICATION.VERSION.NCHAR,
                                     reserved.prefixes = character(),
                                     error.prefix = error.prefix)
}

validate.sub.version.code <- function(code, error.prefix, code.name.for.error = 'sub.version')
{
    validate.jheem.file.path.element(code = code,
                                     code.name.for.error = code.name.for.error,
                                     allow.dashes = F,
                                     min.n.char = MIN.SPECIFICATION.SUBVERSION.NCHAR,
                                     max.n.char = MAX.SPECIFICATION.SUBVERSION.NCHAR,
                                     reserved.prefixes = character(),
                                     error.prefix = error.prefix)
}

validate.calibration.code <- function(code, error.prefix, code.name.for.error = 'code')
{
    validate.jheem.file.path.element(code = code,
                                     code.name.for.error = code.name.for.error,
                                     allow.dashes = F,
                                     min.n.char = MINIMUM.CALIBRATION.CODE.NCHAR,
                                     max.n.char = MAXIMUM.CALIBRATION.CODE.NCHAR,
                                     reserved.prefixes = 'manual',
                                     error.prefix = error.prefix)
}

validate.intervention.code <- function(code, error.prefix, code.name.for.error = 'code')
{
    validate.jheem.file.path.element(code = code,
                                     code.name.for.error = code.name.for.error,
                                     allow.dashes = T,
                                     min.n.char = MINIMUM.INTERVENTION.CODE.NCHAR,
                                     max.n.char = MAXIMUM.INTERVENTION.CODE.NCHAR,
                                     reserved.prefixes = c('baseline', INTERVENTION.TEMPORARY.CODE.PREFIX),
                                     error.prefix = error.prefix)
}


validate.jheem.file.path.element <- function(code,
                                             code.name.for.error,
                                             allow.dashes,
                                             min.n.char,
                                             max.n.char,
                                             reserved.prefixes,
                                             error.prefix)
{
    if (!is.character(error.prefix) || length(error.prefix)!=1 || is.na(error.prefix))
        stop("Cannot validate.jheem.file.path.element() - 'error.prefix' must be a single, non-NA character value")
    
    if (!is.character(code) || length(code)!=1 || is.na(code))
        stop(paste0(error.prefix, code.name.for.error, " must be a single, non-NA character value"))
    
    if (nchar(code) < min.n.char || nchar(code) > max.n.char)
        stop(paste0(error.prefix, code.name.for.error, " must be between ",
                    min.n.char, " and ", max.n.char, " character long. '",
                    code, "' is ", nchar(code)))
    
    sapply(reserved.prefixes, function(prefix){
        if (substr(tolower(code), 1, nchar(prefix))==tolower(prefix))
            stop(paste0(error.prefix, code.name.for.error, " cannot start with '", prefix, "'"))
    })
    
    if (allow.dashes)
    {
        check.for.invalid.characters(str = code,
                                     valid.characters = NUMBERS.LETTERS.DASH.PERIOD,
                                     str.name = code.name.for.error,
                                     valid.characters.description = "numbers, letters, dashes, and periods",
                                     error.prefix = error.prefix)
    }
    else
    {
        check.for.invalid.characters(str = code,
                                     valid.characters = NUMBERS.LETTERS.PERIOD,
                                     str.name = code.name.for.error,
                                     valid.characters.description = "numbers, letters, and periods",
                                     error.prefix = error.prefix)
    }
}
