# Records of the data managers in use when an artifact was built ("data inputs").
#
# Kernels and likelihoods keep a record of the data managers in the session when they
# were built; simulation sets carry their kernel's record, and calibration setup also
# writes one to data_inputs.dcf in the calibration directory. Read them with
# get.data.inputs(), which returns NULL for artifacts built by earlier versions of jheem2.
#
# A record lists:
#  - the data managers registered by name (register.data.manager), which is how an
#    application declares the managers it selected (coverage "declared");
#  - the default data manager, and a manager passed to the function building the
#    artifact (for a likelihood, the one it was instantiated with);
#  - every data manager load.data.manager() loaded in the session (coverage "loaded").
#
# It can list managers the artifact never read. It does not cover a data manager that was
# loaded some other way (for example with load()) and never registered or set as default.
# Records do not change what any artifact computes.

# Keep the session's loads when this file is sourced again.
if (!exists('DATA.MANAGER.LOADS'))
    DATA.MANAGER.LOADS = new.env()
if (!exists('records', envir = DATA.MANAGER.LOADS, inherits = F))
    DATA.MANAGER.LOADS$records = list()

DATA.INPUTS.FILE.NAME = 'data_inputs.dcf'
DATA.INPUTS.FORMAT = 'jheem2 data inputs 1'

##----------------------##
##-- PUBLIC INTERFACE --##
##----------------------##

#'@title Get the data inputs recorded for an artifact
#'
#'@description The data managers that were in use when a kernel, likelihood, simulation set,
#' or calibration was built: those registered by name, the default data manager, a manager
#' passed in explicitly, and every manager loaded with \code{\link{load.data.manager}} in the
#' session. The record can include managers the artifact did not use.
#'
#'@param x A jheem.kernel, jheem.likelihood, or jheem.simulation.set, or the path of a
#' calibration directory or of the data_inputs.dcf file in one. If missing, the record an
#' artifact built now would get.
#'
#'@return NULL if nothing was recorded (for example, for artifacts built by earlier versions of
#' jheem2). Otherwise a list with
#' \describe{
#'  \item{coverage}{'declared' if any managers were registered by name, 'loaded', and 'unknown'
#'  if any listed manager has no identity (it was not loaded from a file)}
#'  \item{recorded.at}{When the record was made}
#'  \item{run.record}{The JHEEM_RUN_RECORD environment variable at that time, if it was set: a
#'  pointer to the run's own record, for example a container run's records directory}
#'  \item{data.managers}{A data frame with one row per data manager: registered.as (the
#'  registered names, NA if none), default, passed, data.manager.name, file, release,
#'  repository, sha256, md5, modified (whether its contents had changed since loading; NA when
#'  not known) and loaded.at}
#' }
#'
#'@export
get.data.inputs <- function(x)
{
    error.prefix = "Cannot get.data.inputs(): "
    if (missing(x))
        return (make.data.inputs.record())

    if (is.character(x) && length(x) == 1 && !is.na(x))
    {
        file = if (dir.exists(x)) file.path(x, DATA.INPUTS.FILE.NAME) else x
        if (!file.exists(file))
            return (NULL)
        return (read.data.inputs.dcf(file, error.prefix = error.prefix))
    }

    if (R6::is.R6(x) && is(x, 'jheem.simulation.set'))
        x = x$jheem.kernel

    if (!R6::is.R6(x) || !(is(x, 'jheem.kernel') || is(x, 'jheem.likelihood')))
        stop(paste0(error.prefix, "'x' must be a jheem.kernel, jheem.likelihood, or jheem.simulation.set object, or the path of a calibration directory"))

    private = x$.__enclos_env__$private
    if (is.null(private) || !exists('i.data.inputs', envir = private, inherits = F))
        return (NULL)
    private$i.data.inputs
}

##-----------------------##
##-- INTERNAL HELPERS --##
##-----------------------##

# Called by load.data.manager(). Keeps only the identity, not the manager, so loaded
# managers can still be freed.
note.data.manager.load <- function(data.manager)
{
    DATA.MANAGER.LOADS$records = c(DATA.MANAGER.LOADS$records,
                                   list(c(list(data.manager.name = data.manager$name),
                                          get.data.manager.provenance(data.manager))))
    invisible(NULL)
}

make.data.inputs.record <- function(passed.data.manager = NULL)
{
    # Data managers held in memory, each listed once with every role it has
    entries = list()
    add.entry = function(data.manager, registered.as = NA_character_, is.default = F, is.passed = F)
    {
        if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager'))
            return (invisible(NULL))
        for (i in seq_along(entries))
        {
            if (identical(entries[[i]]$data.manager, data.manager))
            {
                if (!is.na(registered.as))
                    entries[[i]]$registered.as <<- c(entries[[i]]$registered.as, registered.as)
                entries[[i]]$default <<- entries[[i]]$default || is.default
                entries[[i]]$passed <<- entries[[i]]$passed || is.passed
                return (invisible(NULL))
            }
        }
        entries[[length(entries) + 1]] <<- list(data.manager = data.manager,
                                                registered.as = if (is.na(registered.as)) character() else registered.as,
                                                default = is.default,
                                                passed = is.passed)
        invisible(NULL)
    }

    for (name in sort(ls(DATA.MANAGER.REGISTRY)))
        add.entry(get(name, envir = DATA.MANAGER.REGISTRY, inherits = F), registered.as = name)
    add.entry(get.default.data.manager(), is.default = T)
    add.entry(passed.data.manager, is.passed = T)

    rows = lapply(entries, function(entry){
        provenance = get.data.manager.provenance(entry$data.manager)
        make.data.inputs.row(registered.as = if (length(entry$registered.as) == 0) NA_character_
                                             else paste0(entry$registered.as, collapse = ', '),
                             default = entry$default,
                             passed = entry$passed,
                             data.manager.name = entry$data.manager$name,
                             provenance = provenance,
                             modified = if (is.null(provenance)) NA else provenance$modified)
    })

    # Loads not already listed. A copy of a loaded manager shares its load record.
    held.loads = lapply(entries, function(entry){ get.data.manager.provenance(entry$data.manager) })
    held.loads = held.loads[!vapply(held.loads, is.null, logical(1))]
    for (load in DATA.MANAGER.LOADS$records)
    {
        already.listed = any(vapply(held.loads, function(held){
            identical(held$file, load$file) && identical(held$md5, load$md5) &&
                identical(held$loaded.at, load$loaded.at)
        }, logical(1)))
        if (!already.listed)
        {
            rows[[length(rows) + 1]] = make.data.inputs.row(registered.as = NA_character_, default = F, passed = F,
                                                            data.manager.name = load$data.manager.name,
                                                            provenance = load,
                                                            modified = NA)
            held.loads[[length(held.loads) + 1]] = load
        }
    }

    data.managers = do.call(rbind, c(list(make.data.inputs.row(registered.as = NA_character_, default = F, passed = F,
                                                                data.manager.name = NA_character_, provenance = NULL,
                                                                modified = NA)[0, ]),
                                     rows))
    rownames(data.managers) = NULL

    coverage = character()
    if (any(!is.na(data.managers$registered.as)))
        coverage = 'declared'
    coverage = c(coverage, 'loaded')
    if (any(is.na(data.managers$md5)))
        coverage = c(coverage, 'unknown')

    run.record = Sys.getenv('JHEEM_RUN_RECORD', unset = '')

    list(coverage = coverage,
         recorded.at = Sys.time(),
         run.record = if (nzchar(run.record)) run.record else NULL,
         data.managers = data.managers)
}

make.data.inputs.row <- function(registered.as, default, passed, data.manager.name, provenance, modified)
{
    field = function(name) if (is.null(provenance[[name]])) NA_character_ else as.character(provenance[[name]])
    data.frame(registered.as = registered.as,
               default = default,
               passed = passed,
               data.manager.name = if (is.null(data.manager.name)) NA_character_ else as.character(data.manager.name),
               file = field('file'),
               release = field('release'),
               repository = field('repository'),
               sha256 = field('sha256'),
               md5 = field('md5'),
               modified = as.logical(modified),
               loaded.at = if (is.null(provenance$loaded.at)) as.POSIXct(NA) else provenance$loaded.at,
               stringsAsFactors = F)
}

# Records an artifact's data inputs in its private field, if its class has one.
set.data.inputs <- function(x, record)
{
    private = x$.__enclos_env__$private
    if (!is.null(private) && exists('i.data.inputs', envir = private, inherits = F))
        private$i.data.inputs = record
    invisible(x)
}

##-- The calibration file --##

DATA.INPUTS.TIME.FORMAT = '%Y-%m-%dT%H:%M:%SZ'

write.data.inputs.dcf <- function(record, dir)
{
    format.time = function(time) ifelse(is.na(time), NA_character_,
                                        format(time, DATA.INPUTS.TIME.FORMAT, tz = 'UTC'))
    header = data.frame(format = DATA.INPUTS.FORMAT,
                        coverage = paste0(record$coverage, collapse = ', '),
                        recorded.at = format.time(record$recorded.at),
                        run.record = if (is.null(record$run.record)) NA_character_ else record$run.record,
                        stringsAsFactors = F)
    data.managers = record$data.managers
    data.managers$loaded.at = format.time(data.managers$loaded.at)
    for (column in c('default', 'passed', 'modified'))
        data.managers[[column]] = as.character(data.managers[[column]])

    file = file.path(dir, DATA.INPUTS.FILE.NAME)
    # (A wide width keeps each value on one line.)
    lines = c(utils::capture.output(write.dcf(header, width = 10000)),
              if (nrow(data.managers) > 0) c('', utils::capture.output(write.dcf(data.managers, width = 10000))))
    writeLines(lines, file)
    invisible(file)
}

read.data.inputs.dcf <- function(file, error.prefix = '')
{
    fields = read.dcf(file, all = T)
    if (nrow(fields) == 0 || is.null(fields$format) || !identical(fields$format[1], DATA.INPUTS.FORMAT))
        stop(paste0(error.prefix, "'", file, "' is not a jheem2 data-inputs file"))
    parse.time = function(text) as.POSIXct(text, format = DATA.INPUTS.TIME.FORMAT, tz = 'UTC')
    column = function(rows, name) if (is.null(rows[[name]])) rep(NA_character_, nrow(rows)) else as.character(rows[[name]])

    header = fields[1, , drop = F]
    rows = fields[-1, , drop = F]
    data.managers = data.frame(registered.as = column(rows, 'registered.as'),
                               default = as.logical(column(rows, 'default')),
                               passed = as.logical(column(rows, 'passed')),
                               data.manager.name = column(rows, 'data.manager.name'),
                               file = column(rows, 'file'),
                               release = column(rows, 'release'),
                               repository = column(rows, 'repository'),
                               sha256 = column(rows, 'sha256'),
                               md5 = column(rows, 'md5'),
                               modified = as.logical(column(rows, 'modified')),
                               loaded.at = parse.time(column(rows, 'loaded.at')),
                               stringsAsFactors = F)
    run.record = column(header, 'run.record')
    list(coverage = strsplit(column(header, 'coverage'), ', ', fixed = T)[[1]],
         recorded.at = parse.time(column(header, 'recorded.at')),
         run.record = if (is.na(run.record)) NULL else run.record,
         data.managers = data.managers)
}
