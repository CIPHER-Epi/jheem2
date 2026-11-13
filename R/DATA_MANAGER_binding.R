# Data bindings: which data manager each name refers to while model objects are built.
#
# A binding maps names, such as 'census' and 'syphilis', to loaded data managers. Code gets a
# manager by name with get.data.manager(). Kernel construction, likelihood instantiation, and
# calibration setup build under the active binding: they keep it fixed while they run, note the
# names resolved during the build, and record the binding with those names in the artifact's
# data-inputs record (coverage 'bound'; see get.data.inputs).
#
# The session binding is the registry of register.data.manager(); set.data.binding() makes it
# the active binding for the session. using.data.binding() makes another binding active while an
# expression runs. create.data.manager.alias() turns a global variable, such as CENSUS.MANAGER,
# into a name for a bound manager, so code that reads the variable resolves it through the binding.
#
# With no binding set, nothing changes: get.data.manager() reads the registry and records keep
# the coverage they had before bindings existed.

# Keep the session's state when this file is sourced again.
if (!exists('DATA.BINDING.STATE'))
    DATA.BINDING.STATE = new.env()
local({
    defaults = list(session.active = FALSE,  # set.data.binding() was called
                    stack = list(),          # bindings made active by using.data.binding() and entry points
                    frames = list(),         # recording frames of the entry points now running
                    aliases = list(),        # variables made into aliases by create.data.manager.alias()
                    declared = list(),       # names declared per specification version
                    release.resolver = NULL) # see set.data.release.resolver()
    for (field in names(defaults))
        if (!exists(field, envir = DATA.BINDING.STATE, inherits = F))
            assign(field, defaults[[field]], envir = DATA.BINDING.STATE)
})

##----------------------##
##-- PUBLIC INTERFACE --##
##----------------------##

#'@title Create a data binding
#'
#'@description A map from names to data managers. Pass it to \code{\link{using.data.binding}} or
#' \code{\link{set.data.binding}} to build model objects with it.
#'
#'@param ... Data managers, each named with the name code uses to get it (letters, digits, '.'
#' and '_', starting with a letter), or a single named list of them
#'
#'@return An object of class 'jheem.data.binding'
#'
#'@export
create.data.binding <- function(...)
{
    error.prefix = "Cannot create.data.binding(): "
    managers = list(...)
    if (length(managers) == 1 && is.null(names(managers)) && is.list(managers[[1]]) && !R6::is.R6(managers[[1]]))
        managers = managers[[1]]
    if (length(managers) > 0 && (is.null(names(managers)) || any(names(managers) == '')))
        stop(paste0(error.prefix, "every data manager must be named"))
    if (any(duplicated(names(managers))))
        stop(paste0(error.prefix, "names must be unique"))
    for (name in names(managers))
    {
        check.data.manager.name(name, error.prefix)
        if (!R6::is.R6(managers[[name]]) || !is(managers[[name]], 'jheem.data.manager'))
            stop(paste0(error.prefix, "'", name, "' must be an R6 object with class 'jheem.data.manager'"))
    }
    structure(list(managers = managers), class = 'jheem.data.binding')
}

#'@title Set the session's data binding
#'
#'@description Registers each data manager in the binding under its name, replacing the
#' registry's contents (see \code{\link{register.data.manager}}), and makes the registry the
#' active binding for the session. Data managers registered later join it.
#'
#'@param binding A 'jheem.data.binding' object, or NULL to stop using a session binding (the
#' registry is kept)
#'
#'@return The binding, invisibly
#'
#'@export
set.data.binding <- function(binding)
{
    error.prefix = "Cannot set.data.binding(): "
    if (length(DATA.BINDING.STATE$frames) > 0)
        stop(paste0(error.prefix, "the session binding cannot change while jheem2 is building under a binding"))
    if (is.null(binding))
    {
        DATA.BINDING.STATE$session.active = FALSE
        return (invisible(NULL))
    }
    check.data.binding(binding, error.prefix)
    rm(list = ls(DATA.MANAGER.REGISTRY, all.names = T), envir = DATA.MANAGER.REGISTRY)
    for (name in names(binding$managers))
        register.data.manager(name, binding$managers[[name]])
    DATA.BINDING.STATE$session.active = TRUE
    invisible(binding)
}

#'@title Evaluate an expression under a data binding
#'
#'@description Makes the binding active while 'expr' is evaluated, and restores the previous
#' active binding afterwards, also on error.
#'
#'@param binding A 'jheem.data.binding' object
#'@param expr The expression to evaluate
#'
#'@return The value of 'expr'
#'
#'@export
using.data.binding <- function(binding, expr)
{
    check.data.binding(binding, "Cannot run using.data.binding(): ")
    depth = length(DATA.BINDING.STATE$stack)
    DATA.BINDING.STATE$stack = c(DATA.BINDING.STATE$stack, list(binding))
    on.exit(DATA.BINDING.STATE$stack <- DATA.BINDING.STATE$stack[seq_len(depth)])
    expr
}

#'@title Get the active data binding
#'
#'@return The binding made active most recently by \code{\link{using.data.binding}} or a jheem2
#' function building under it; otherwise the session binding if \code{\link{set.data.binding}}
#' was called; otherwise NULL
#'
#'@export
get.data.binding <- function()
{
    depth = length(DATA.BINDING.STATE$stack)
    if (depth > 0)
        return (DATA.BINDING.STATE$stack[[depth]])
    if (isTRUE(DATA.BINDING.STATE$session.active))
        return (create.data.binding(as.list(DATA.MANAGER.REGISTRY, all.names = T)))
    NULL
}

#'@title Get the identity of a data binding
#'
#'@param binding A 'jheem.data.binding' object
#'
#'@return A data frame with one row per name: name, data.manager.name, and the file, release,
#' repository, sha256, md5, and modified status of the data manager bound to it (NA where unknown;
#' see \code{\link{get.data.manager.provenance}})
#'
#'@export
get.data.binding.identity <- function(binding)
{
    check.data.binding(binding, "Cannot get.data.binding.identity(): ")
    names = sort(names(binding$managers))
    rows = lapply(names, function(name){
        data.manager = binding$managers[[name]]
        provenance = get.data.manager.provenance(data.manager)
        field = function(x) if (is.null(provenance[[x]])) NA_character_ else as.character(provenance[[x]])
        data.frame(name = name,
                   data.manager.name = if (is.null(data.manager$name)) NA_character_ else as.character(data.manager$name),
                   file = field('file'),
                   release = field('release'),
                   repository = field('repository'),
                   sha256 = field('sha256'),
                   md5 = field('md5'),
                   modified = if (is.null(provenance)) NA else as.logical(provenance$modified),
                   stringsAsFactors = F)
    })
    identity = do.call(rbind, c(list(data.frame(name = character(), data.manager.name = character(),
                                                file = character(), release = character(),
                                                repository = character(), sha256 = character(),
                                                md5 = character(), modified = logical(),
                                                stringsAsFactors = F)),
                                rows))
    rownames(identity) = NULL
    identity
}

#'@title Make a variable an alias for a bound data manager
#'
#'@description Turns 'variable' into an active binding whose value is
#' \code{get.data.manager(name)}, so code that reads the variable (for example CENSUS.MANAGER)
#' gets the manager bound to 'name' in the active binding, and the read is recorded. Assigning a
#' data manager to the variable registers it under 'name' (see
#' \code{\link{register.data.manager}}); this is not allowed while a binding other than the
#' session's is active or jheem2 is building under one.
#'
#' If the variable already holds a data manager, it is registered under 'name' unless another
#' manager already is. Aliasing a variable again to the same name does nothing; aliasing it to a
#' different name is an error.
#'
#'@param variable The variable's name, for example 'CENSUS.MANAGER'
#'@param name The data manager's name in the binding, for example 'census'
#'@param envir The environment holding the variable
#'
#'@return NULL, invisibly
#'
#'@export
create.data.manager.alias <- function(variable, name, envir = globalenv())
{
    error.prefix = "Cannot create.data.manager.alias(): "
    if (!is.character(variable) || length(variable) != 1 || is.na(variable) || variable == '')
        stop(paste0(error.prefix, "'variable' must be a single variable name"))
    check.data.manager.name(name, error.prefix)
    if (!is.environment(envir))
        stop(paste0(error.prefix, "'envir' must be an environment"))

    # An alias stays recorded only while its variable is still the active binding we made
    existing = find.data.manager.alias(variable, envir)
    if (!is.null(existing))
    {
        if (identical(existing$name, name))
            return (invisible(NULL))
        stop(paste0(error.prefix, "'", variable, "' is already an alias for the data manager named '",
                    existing$name, "'"))
    }

    if (exists(variable, envir = envir, inherits = F))
    {
        if (bindingIsActive(variable, envir))
            stop(paste0(error.prefix, "'", variable, "' is already an active binding"))
        value = get(variable, envir = envir, inherits = F)
        if (!R6::is.R6(value) || !is(value, 'jheem.data.manager'))
            stop(paste0(error.prefix, "'", variable, "' already exists and is not a data manager"))
        if (!exists(name, envir = DATA.MANAGER.REGISTRY, inherits = F))
            register.data.manager(name, value)
        else if (!identical(get(name, envir = DATA.MANAGER.REGISTRY, inherits = F), value))
            stop(paste0(error.prefix, "'", variable, "' holds a different data manager from the one registered as '",
                        name, "'. Remove the variable or register it under '", name, "' first"))
        rm(list = variable, envir = envir)
    }

    makeActiveBinding(variable, make.data.manager.alias.function(variable, name), envir)
    DATA.BINDING.STATE$aliases = c(DATA.BINDING.STATE$aliases,
                                   list(list(variable = variable, name = name, envir = envir)))
    invisible(NULL)
}

#'@title Declare the data managers a specification uses
#'
#'@description While a kernel for this specification's version is built under a binding, the
#' binding must contain every declared name, and resolving any other name is an error. With no
#' binding active, the declaration is not checked.
#'
#'@param specification A jheem.specification object
#'@param names The names of the data managers its functions use, for example c('census', 'syphilis')
#'
#'@return NULL, invisibly
#'
#'@export
declare.data.managers <- function(specification, names)
{
    error.prefix = "Cannot declare.data.managers(): "
    if (!R6::is.R6(specification) || !is(specification, 'jheem.specification'))
        stop(paste0(error.prefix, "'specification' must be a jheem.specification object"))
    if (!is.character(names) || any(is.na(names)) || any(duplicated(names)))
        stop(paste0(error.prefix, "'names' must be a character vector of distinct names"))
    for (name in names)
        check.data.manager.name(name, error.prefix)
    DATA.BINDING.STATE$declared[[specification$version]] = sort(names)
    invisible(NULL)
}

#'@title Set the function that loads a data manager release
#'
#'@description jheem2 does not fetch releases. \code{\link{rebuild.data.binding}} calls this
#' function to load the data manager each recorded name was bound to.
#'
#'@param resolver A function taking one argument, a list with data.manager.name, file, release,
#' repository, sha256 and md5 (NA where not recorded), and returning the data manager loaded with
#' its provenance (see \code{\link{load.data.manager}}); or NULL to remove it
#'
#'@return NULL, invisibly
#'
#'@export
set.data.release.resolver <- function(resolver)
{
    if (!is.null(resolver) && !is.function(resolver))
        stop("Cannot set.data.release.resolver(): 'resolver' must be a function or NULL")
    DATA.BINDING.STATE$release.resolver = resolver
    invisible(NULL)
}

#'@title Rebuild the data binding an artifact was built under
#'
#'@description Loads each recorded data manager through the release resolver (see
#' \code{\link{set.data.release.resolver}}) and checks it against the recorded digests.
#'
#'@param x A record from \code{\link{get.data.inputs}}, or anything get.data.inputs() accepts
#'
#'@return A 'jheem.data.binding' object with the recorded names
#'
#'@export
rebuild.data.binding <- function(x)
{
    error.prefix = "Cannot rebuild.data.binding(): "
    record = if (is.list(x) && !is.null(x$data.managers) && !is.null(x$coverage)) x else get.data.inputs(x)
    if (is.null(record) || !('bound' %in% record$coverage))
        stop(paste0(error.prefix, "no data binding was recorded"))
    resolver = DATA.BINDING.STATE$release.resolver
    if (is.null(resolver))
        stop(paste0(error.prefix, "no release resolver is set (see set.data.release.resolver())"))

    rows = record$data.managers[!is.na(record$data.managers$bound.as), , drop = F]
    managers = list()
    for (i in seq_len(nrow(rows)))
    {
        row = rows[i, , drop = F]
        names = strsplit(row$bound.as, ', ', fixed = T)[[1]]
        if (isTRUE(row$modified))
            stop(paste0(error.prefix, "the data manager bound as '", names[1],
                        "' had changed after it was loaded, so its release cannot reproduce it"))
        identity = as.list(row[, c('data.manager.name', 'file', 'release', 'repository', 'sha256', 'md5')])
        data.manager = resolver(identity)
        if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
            stop(paste0(error.prefix, "the release resolver did not return a data manager for '", names[1], "'"))
        provenance = get.data.manager.provenance(data.manager)
        for (digest in c('sha256', 'md5'))
            if (!is.na(row[[digest]]) && !identical(provenance[[digest]], row[[digest]]))
                stop(paste0(error.prefix, "the data manager loaded for '", names[1], "' has ", digest, " '",
                            if (is.null(provenance[[digest]])) "none" else provenance[[digest]],
                            "', but the record has '", row[[digest]], "'"))
        for (name in names)
            managers[[name]] = data.manager
    }
    create.data.binding(managers)
}

#'@export
print.jheem.data.binding <- function(x, ...)
{
    identity = get.data.binding.identity(x)
    cat("A jheem2 data binding of ", nrow(identity), " data manager", if (nrow(identity) != 1) "s", "\n", sep = '')
    if (nrow(identity) > 0)
        print(identity[, c('name', 'data.manager.name', 'release', 'md5', 'modified')], row.names = F)
    invisible(x)
}

##-----------------------##
##-- INTERNAL HELPERS --##
##-----------------------##

check.data.binding <- function(binding, error.prefix)
{
    if (!inherits(binding, 'jheem.data.binding'))
        stop(paste0(error.prefix, "'binding' must be a 'jheem.data.binding' object (see create.data.binding())"))
}

# The manager bound to 'name' in the active binding, noting the name in every recording frame;
# NULL when no binding is active.
resolve.bound.data.manager <- function(name, error.prefix)
{
    binding = get.data.binding()
    if (is.null(binding))
        return (NULL)
    if (is.null(binding$managers[[name]]))
    {
        bound = sort(names(binding$managers))
        stop(paste0(error.prefix, "no data manager is bound as '", name, "'",
                    ifelse(length(bound) == 0, " (none are bound)",
                           paste0(" (bound: ", paste0("'", bound, "'", collapse = ", "), ")"))))
    }
    for (frame in DATA.BINDING.STATE$frames)
        if (!is.null(frame$declared) && !(name %in% frame$declared))
            stop(paste0(error.prefix, "'", name, "' is not among the data managers declared for ", frame$label,
                        " (", paste0("'", frame$declared, "'", collapse = ", "), ")"))
    note.data.manager.use(name)
    binding$managers[[name]]
}

note.data.manager.use <- function(names)
{
    for (frame in DATA.BINDING.STATE$frames)
        frame$used = union(frame$used, names)
    invisible(NULL)
}

# The names a data manager object is bound under in a binding
data.binding.names.for <- function(binding, data.manager)
{
    if (is.null(binding) || is.null(data.manager))
        return (character())
    names(binding$managers)[vapply(binding$managers, identical, logical(1), data.manager)]
}

# Called when a jheem2 entry point starts building. With a binding active, keeps it active for the
# whole build (pushing it, so changes to the session binding do not reach the build) and opens a
# recording frame; returns the frame, or NULL when no binding is active. Close it with
# exit.data.binding.frame(), from on.exit().
enter.data.binding.frame <- function(label, declared = NULL, error.prefix = '')
{
    binding = get.data.binding()
    if (is.null(binding))
        return (NULL)
    if (!is.null(declared))
    {
        missing.names = setdiff(declared, names(binding$managers))
        if (length(missing.names) > 0)
            stop(paste0(error.prefix, "the active data binding has no data manager for ",
                        paste0("'", missing.names, "'", collapse = ", "), ", declared for ", label))
    }
    frame = new.env()
    frame$label = label
    frame$binding = binding
    frame$declared = declared
    frame$used = character()
    frame$stack.depth = length(DATA.BINDING.STATE$stack)
    DATA.BINDING.STATE$stack = c(DATA.BINDING.STATE$stack, list(binding))
    DATA.BINDING.STATE$frames = c(DATA.BINDING.STATE$frames, list(frame))
    frame
}

exit.data.binding.frame <- function(frame)
{
    if (is.null(frame))
        return (invisible(NULL))
    DATA.BINDING.STATE$stack = DATA.BINDING.STATE$stack[seq_len(frame$stack.depth)]
    DATA.BINDING.STATE$frames = Filter(function(f) !identical(f, frame), DATA.BINDING.STATE$frames)
    invisible(NULL)
}

get.declared.data.managers <- function(version)
{
    DATA.BINDING.STATE$declared[[version]]
}

find.data.manager.alias <- function(variable, envir)
{
    for (alias in DATA.BINDING.STATE$aliases)
        if (identical(alias$variable, variable) && identical(alias$envir, envir) &&
            exists(variable, envir = envir, inherits = F) && bindingIsActive(variable, envir))
            return (alias)
    NULL
}

make.data.manager.alias.function <- function(variable, name)
{
    force(variable)
    force(name)
    function(value)
    {
        if (missing(value))
            return (get.data.manager(name))
        if (length(DATA.BINDING.STATE$stack) > 0)
            stop(paste0("Cannot assign to '", variable, "' while a binding other than the session's is active ",
                        "or jheem2 is building under one: it is an alias for the data manager named '", name, "'"))
        register.data.manager(name, value)
        invisible(value)
    }
}

##-- Likelihood instructions --##

# The identity of the binding active when likelihood instructions were created (NULL if none).
# Values the analysis code computed while creating them came from these managers.
capture.data.binding.identity <- function()
{
    binding = get.data.binding()
    if (is.null(binding))
        return (NULL)
    get.data.binding.identity(binding)
}

# Instructions created under one binding must not be instantiated under a binding that differs for
# any of the names they were created with.
check.instructions.data.binding <- function(instructions, error.prefix)
{
    private = instructions$.__enclos_env__$private
    if (is.null(private) || !exists('i.data.binding.identity', envir = private, inherits = F))
        return (invisible(NULL))
    captured = private$i.data.binding.identity
    binding = get.data.binding()
    if (is.null(captured) || is.null(binding) || nrow(captured) == 0)
        return (invisible(NULL))
    active = get.data.binding.identity(binding)
    differing = captured$name[!vapply(seq_len(nrow(captured)), function(i){
        match = active[active$name == captured$name[i], , drop = F]
        nrow(match) == 1 && same.data.manager.identity(captured[i, , drop = F], match)
    }, logical(1))]
    if (length(differing) > 0)
        stop(paste0(error.prefix, "these likelihood instructions were created under a data binding that differs from ",
                    "the active one for ", paste0("'", differing, "'", collapse = ", "),
                    ". Create them again under the active binding"))
    invisible(NULL)
}

same.data.manager.identity <- function(a, b)
{
    if (!is.na(a$md5) && !is.na(b$md5))
        return (identical(a$md5, b$md5) && identical(a$modified, b$modified))
    # Managers not loaded from a file: only their names can be compared
    is.na(a$md5) && is.na(b$md5) && identical(a$data.manager.name, b$data.manager.name)
}

##-- Calibration registration --##

# The name under which a calibration's data manager is resolved at setup: the name given, or the
# name the object is bound under in the active binding; NULL if neither.
calibration.data.manager.name <- function(data.manager, error.prefix)
{
    if (is.character(data.manager))
    {
        check.data.manager.name(data.manager, error.prefix)
        return (data.manager)
    }
    names = data.binding.names.for(get.data.binding(), data.manager)
    if (length(names) == 0)
        return (NULL)
    names[1]
}

# The data manager a calibration's likelihood is instantiated with at setup
resolve.calibration.data.manager <- function(calibration.info)
{
    name = calibration.info$data.manager.name
    if (!is.null(name) && (!is.null(get.data.binding()) || is.character(calibration.info$data.manager)))
        return (get.data.manager(name))
    calibration.info$data.manager
}
