# Identity of loaded data managers, and a registry of data managers by name.
#
# load.data.manager() records where a manager came from: the file it was loaded
# from, an MD5 digest of that file, and, when the caller knows it, the release it
# belongs to (tag, repository, SHA-256). Any change to the manager's contents
# afterwards marks the record as modified. Records are read with
# get.data.manager.provenance(), which returns NULL for managers that have none.
#
# The registry lets code refer to data managers by name, for example "census",
# rather than through a global variable. It is also the session's data binding
# (see DATA_MANAGER_binding.R).

# Keep registrations when this file is sourced again.
if (!exists('DATA.MANAGER.REGISTRY'))
    DATA.MANAGER.REGISTRY = new.env()

DATA.MANAGER.PROVENANCE.FIELDS = c('sha256', 'release', 'repository')

##----------------------##
##-- PUBLIC INTERFACE --##
##----------------------##

#'@title Get the provenance of a data manager
#'
#'@description Where a data manager came from: the file it was loaded from, an MD5 digest of
#' that file, the release it belongs to when the loader supplied it, when it was loaded, and
#' whether its contents have changed since.
#'
#'@param data.manager A jheem.data.manager object
#'
#'@return A list with elements file, md5, sha256, release, repository, loaded.at and modified,
#' or NULL for a data manager that was not loaded from a file (or was loaded by an earlier
#' version of jheem2)
#'
#'@export
get.data.manager.provenance <- function(data.manager)
{
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop("Cannot get.data.manager.provenance(): 'data.manager' must be an R6 object with class 'jheem.data.manager'")
    private = data.manager$.__enclos_env__$private
    if (is.null(private) || !exists('i.provenance', envir = private, inherits = F))
        return (NULL)
    private$i.provenance
}

#'@title Register a data manager under a name
#'
#'@description Makes a data manager available as get.data.manager(name). Registering a
#' name again replaces the earlier manager.
#'
#'@param name A name for the data manager, such as 'census': letters, digits, '.' and '_',
#' starting with a letter
#'@param data.manager A jheem.data.manager object
#'
#'@return The data manager, invisibly, so the result can also be assigned
#'
#'@export
register.data.manager <- function(name, data.manager)
{
    error.prefix = "Cannot register.data.manager(): "
    check.data.manager.name(name, error.prefix)
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
        stop(paste0(error.prefix, "'data.manager' must be an R6 object with class 'jheem.data.manager'"))
    assign(name, data.manager, envir = DATA.MANAGER.REGISTRY)
    invisible(data.manager)
}

#'@title Get a data manager by name
#'
#'@description With a data binding active (see \code{\link{get.data.binding}}), the data manager
#' bound to the name, and the name is recorded as used by any model object being built.
#' Otherwise, the data manager registered under the name.
#'
#'@param name The name it was bound or registered under
#'
#'@return A jheem.data.manager
#'
#'@export
get.data.manager <- function(name)
{
    error.prefix = "Cannot get.data.manager(): "
    check.data.manager.name(name, error.prefix)
    bound = resolve.bound.data.manager(name, error.prefix)
    if (!is.null(bound))
        return (bound)
    if (!exists(name, envir = DATA.MANAGER.REGISTRY, inherits = F))
    {
        registered = sort(ls(DATA.MANAGER.REGISTRY))
        stop(paste0(error.prefix, "no data manager is registered as '", name, "'",
                    ifelse(length(registered) == 0, " (none are registered)",
                           paste0(" (registered: ", paste0("'", registered, "'", collapse = ", "), ")"))))
    }
    get(name, envir = DATA.MANAGER.REGISTRY, inherits = F)
}

#'@title List registered data managers
#'
#'@return A data frame with one row per registered name: the name, the data manager's own
#' name, and its file, release, MD5 digest and whether it was modified (NA where unknown)
#'
#'@export
list.data.managers <- function()
{
    names = sort(ls(DATA.MANAGER.REGISTRY))
    rows = lapply(names, function(name){
        data.manager = get(name, envir = DATA.MANAGER.REGISTRY, inherits = F)
        provenance = get.data.manager.provenance(data.manager)
        field = function(x) if (is.null(provenance[[x]])) NA else provenance[[x]]
        data.frame(name = name,
                   data.manager.name = data.manager$name,
                   file = as.character(field('file')),
                   release = as.character(field('release')),
                   md5 = as.character(field('md5')),
                   modified = as.logical(field('modified')),
                   stringsAsFactors = F)
    })
    if (length(rows) == 0)
        return (data.frame(name = character(), data.manager.name = character(), file = character(),
                           release = character(), md5 = character(), modified = logical(),
                           stringsAsFactors = F))
    do.call(rbind, rows)
}

##-----------------------##
##-- INTERNAL HELPERS --##
##-----------------------##

check.data.manager.name <- function(name, error.prefix)
{
    if (!is.character(name) || length(name) != 1 || is.na(name) ||
        !grepl('^[A-Za-z][A-Za-z0-9._]*$', name))
        stop(paste0(error.prefix, "'name' must be a single name of letters, digits, '.' and '_', starting with a letter"))
}

# The record load.data.manager() attaches: the file's own identity plus what the caller knows.
make.data.manager.provenance <- function(file, supplied = NULL, error.prefix = '')
{
    if (!is.null(supplied))
    {
        if (!is.list(supplied) || is.null(names(supplied)) || any(names(supplied) == '') ||
            any(!(names(supplied) %in% DATA.MANAGER.PROVENANCE.FIELDS)))
            stop(paste0(error.prefix, "'provenance' must be a named list with elements among ",
                        paste0("'", DATA.MANAGER.PROVENANCE.FIELDS, "'", collapse = ", ")))
        for (field in names(supplied))
        {
            value = supplied[[field]]
            if (!is.null(value) && (!is.character(value) || length(value) != 1 || is.na(value) || nchar(value) == 0))
                stop(paste0(error.prefix, "provenance$", field, " must be a single, non-empty character value"))
        }
        if (!is.null(supplied$sha256) && !grepl('^[0-9a-f]{64}$', supplied$sha256))
            stop(paste0(error.prefix, "provenance$sha256 must be 64 lowercase hexadecimal characters"))
    }
    list(file = basename(file),
         md5 = unname(tools::md5sum(file)),
         sha256 = supplied$sha256,
         release = supplied$release,
         repository = supplied$repository,
         loaded.at = Sys.time(),
         modified = FALSE)
}

set.data.manager.provenance <- function(data.manager, provenance)
{
    data.manager$.__enclos_env__$private$i.provenance = provenance
    invisible(data.manager)
}
