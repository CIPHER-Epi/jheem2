# Records of the data managers in use when kernels, likelihoods, simulation sets, and
# calibrations are built.
#   Rscript tests/data_inputs.R     (with jheem2 installed)
# JHEEM2_TEST_LIBRARY, if set, is a library to load jheem2 from first.
library.path = Sys.getenv("JHEEM2_TEST_LIBRARY", unset = "")
if (nzchar(library.path)) .libPaths(c(library.path, .libPaths()))
suppressPackageStartupMessages(library(jheem2))
Sys.unsetenv("JHEEM_RUN_RECORD")

expect.error = function(expr, pattern)
{
    message = tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
    if (is.null(message) || !grepl(pattern, message))
        stop("expected an error matching '", pattern, "', got: ", if (is.null(message)) "no error" else message)
}
directory = tempfile("jheem2-data-inputs-")
dir.create(directory)
save.manager = function(data.manager, name)
{
    file = file.path(directory, name)
    saved = data.manager
    save(saved, file = file)
    file
}

#-- A session with no data managers: an empty record --#
record = get.data.inputs()
stopifnot(identical(record$coverage, "loaded"),
          identical(nrow(record$data.managers), 0L),
          is.null(record$run.record),
          inherits(record$recorded.at, "POSIXct"))

#-- A small model: the toy fixture (see the jheem-wiki toy-fixture page) --#
spec = create.jheem.specification(version = "toy", iteration = "1", description = "Toy model",
                                  start.year = 1970, age.endpoints = c(0, 50, Inf),
                                  compartments.for.infected.and.uninfected = list(location = "location",
                                                                                  age = "all.ages",
                                                                                  sex = c("female", "male")),
                                  compartments.for.infected.only = list(continuum = c("undiagnosed", "diagnosed")))
register.model.quantity(spec, name = "toy.mortality.rate", value = 0.01)
register.mortality(spec, groups = c("infected", "uninfected"), mortality.rate.value = "toy.mortality.rate")
register.model.quantity(spec, name = "toy.fertility.rate", value = 0.02)
register.model.quantity(spec, name = "toy.birth.proportions", value = 0)
register.model.quantity.subset(spec, name = "toy.birth.proportions",
                               applies.to = list(sex.to = c("female", "male")), value = 0.5)
register.natality(spec, parent.groups = c("infected", "uninfected"), child.groups = "uninfected",
                  fertility.rate.value = "toy.fertility.rate", birth.proportions.value = "toy.birth.proportions",
                  all.births.into.compartments = list(age = 1))
register.model.element(spec, name = "transmission.rate", scale = "rate",
                       functional.form = create.static.functional.form(value = 0.1, link = "log"))
register.model.quantity(spec, name = "toy.contact", value = 0)
register.model.quantity.subset(spec, name = "toy.contact",
                               applies.to = list(sex.from = c("female", "male"), sex.to = c("female", "male")),
                               value = 0.5)
register.model.quantity(spec, name = "toy.susceptibility", value = 1)
register.model.quantity(spec, name = "toy.new.infection.proportions", value = 1)
register.transmission(spec, contact.value = "toy.contact", susceptibility.value = "toy.susceptibility",
                      transmissibility.value = "transmission.rate",
                      new.infection.proportions.value = "toy.new.infection.proportions",
                      new.infections.applies.to = list(continuum = "undiagnosed"))
register.model.quantity(spec, name = "toy.init.uninfected", value = 0)
register.model.quantity.subset(spec, name = "toy.init.uninfected",
                               applies.to = list(age = c("0-49 years", "50+ years"), sex = c("female", "male")),
                               value = 1000)
register.initial.population(spec, group = "uninfected", value = "toy.init.uninfected")
register.model.quantity(spec, name = "init.infected", value = 0)
register.model.quantity.subset(spec, name = "init.infected",
                               applies.to = list(continuum = "undiagnosed", age = c("0-49 years", "50+ years"),
                                                 sex = c("female", "male")),
                               value = 1)
register.initial.population(spec, group = "infected", value = "init.infected")
metadata = function(name) create.outcome.metadata(display.name = name, description = name,
                                                  scale = "non.negative.number", axis.name = name,
                                                  units = "people", singular.unit = "person")
track.dynamic.outcome(spec, name = "incidence", outcome.metadata = metadata("Incidence"),
                      dynamic.quantity.name = "incidence", keep.dimensions = c("location", "age", "sex"))
track.point.outcome(spec, "point.population", scale = "non.negative.number",
                    value = expression(uninfected + infected), keep.dimensions = c("location", "age", "sex"),
                    outcome.metadata = metadata("Point population"))
track.integrated.outcome(spec, name = "population", outcome.metadata = metadata("Population"),
                         value.to.integrate = "point.population", keep.dimensions = c("location", "age", "sex"))
register.model.specification(spec)
no.parameters = setNames(numeric(0), character(0))

#-- A simulation from before any manager was loaded records none --#
early.sim = create.jheem.engine("toy", "C.12580", end.year = 1980)$run(no.parameters)
early.record = get.data.inputs(early.sim)
stopifnot(identical(early.record$coverage, "loaded"), identical(nrow(early.record$data.managers), 0L))

#-- Loaded, registered, default, passed, and in-memory managers --#
census.file = save.manager(create.data.manager("census.build", description = "Census"), "census.manager.rdata")
syphilis.file = save.manager(create.data.manager("syphilis.build", description = "Syphilis"), "syphilis.manager.rdata")
other.file = save.manager(create.data.manager("other.build", description = "Other"), "other.manager.rdata")
census.sha = paste(rep("c", 64), collapse = "")
census = load.data.manager(census.file, provenance = list(sha256 = census.sha, release = "census-manager-v2026.10.08",
                                                          repository = "example/repository"))
syphilis = load.data.manager(syphilis.file, set.as.default = T)
other = load.data.manager(other.file)
rm(other)  # loaded, then discarded: still listed
register.data.manager("census", census)
register.data.manager("syphilis", syphilis)
in.memory = create.data.manager("in.memory", description = "Not loaded from a file")

Sys.setenv(JHEEM_RUN_RECORD = "/records/C.12580/calib.test")
engine = create.jheem.engine("toy", "C.12580", end.year = 1980)
sim = engine$run(no.parameters)
record = get.data.inputs(sim)
Sys.unsetenv("JHEEM_RUN_RECORD")

stopifnot(identical(record$coverage, c("declared", "loaded")),
          identical(record$run.record, "/records/C.12580/calib.test"),
          identical(record, get.data.inputs(sim$jheem.kernel)))
managers = record$data.managers
stopifnot(identical(managers$registered.as, c("census", "syphilis", NA)),
          identical(managers$default, c(FALSE, TRUE, FALSE)),
          identical(managers$passed, c(FALSE, FALSE, FALSE)),
          identical(managers$data.manager.name, c("census.build", "syphilis.build", "other.build")),
          identical(managers$file, c("census.manager.rdata", "syphilis.manager.rdata", "other.manager.rdata")),
          identical(managers$release, c("census-manager-v2026.10.08", NA, NA)),
          identical(managers$sha256, c(census.sha, NA, NA)),
          identical(managers$md5, unname(tools::md5sum(c(census.file, syphilis.file, other.file)))),
          identical(managers$modified, c(FALSE, FALSE, NA)),
          inherits(managers$loaded.at, "POSIXct"), !any(is.na(managers$loaded.at)))

#-- A likelihood records the manager it was instantiated with --#
instructions = create.bernoulli.likelihood.instructions(outcome.for.sim = "population", years = c(1975, 1980),
                                                        probability.decreasing = 0.5, weights = list())
likelihood = instantiate.likelihood(instructions, version = "toy", location = "C.12580", data.manager = in.memory)
likelihood.managers = get.data.inputs(likelihood)$data.managers
stopifnot(identical(get.data.inputs(likelihood)$coverage, c("declared", "loaded", "unknown")),
          identical(likelihood.managers$data.manager.name, c("census.build", "syphilis.build", "in.memory", "other.build")),
          identical(likelihood.managers$passed, c(FALSE, FALSE, TRUE, FALSE)),
          is.na(likelihood.managers$md5[3]), is.na(likelihood.managers$modified[3]))
likelihood = instantiate.likelihood(instructions, version = "toy", location = "C.12580", data.manager = census)
stopifnot(identical(get.data.inputs(likelihood)$data.managers$passed, c(TRUE, FALSE, FALSE)))

#-- A changed manager is shown as modified; a manager registered twice is listed once --#
census$register.parent.source(parent.source = "PARENT", full.name = "A parent source", short.name = "Parent")
register.data.manager("census.again", census)
managers = get.data.inputs()$data.managers
stopifnot(identical(managers$registered.as, c("census, census.again", "syphilis", NA)),
          identical(managers$modified, c(TRUE, FALSE, NA)))

#-- The record survives saving and loading the simulation set --#
sim.file = file.path(directory, "sim.rdata")
save(sim, file = sim.file)
reloaded.sim = get(load(sim.file))
stopifnot(identical(get.data.inputs(reloaded.sim), record))

#-- The calibration file: written, read back, and found from the directory --#
calibration.dir = file.path(directory, "calibration")
dir.create(calibration.dir)
jheem2:::write.data.inputs.dcf(record, dir = calibration.dir)
from.file = get.data.inputs(calibration.dir)
stopifnot(identical(from.file$coverage, record$coverage),
          identical(from.file$run.record, record$run.record),
          abs(as.numeric(from.file$recorded.at) - as.numeric(record$recorded.at)) < 1)
for (column in setdiff(names(record$data.managers), "loaded.at"))
    stopifnot(identical(from.file$data.managers[[column]], record$data.managers[[column]]))
stopifnot(all(abs(as.numeric(from.file$data.managers$loaded.at) - as.numeric(record$data.managers$loaded.at)) < 1),
          identical(get.data.inputs(file.path(calibration.dir, "data_inputs.dcf")), from.file),
          is.null(get.data.inputs(directory)))
writeLines("Package: not.a.record", file.path(directory, "data_inputs.dcf"))
expect.error(get.data.inputs(directory), "not a jheem2 data-inputs file")

#-- Artifacts from earlier versions have no record --#
EARLIER.KERNEL = R6::R6Class("jheem.kernel", portable = F, private = list(i.version = "toy"))
stopifnot(is.null(get.data.inputs(EARLIER.KERNEL$new())))
expect.error(get.data.inputs(list()), "jheem.kernel, jheem.likelihood, or jheem.simulation.set")

cat("data_inputs.R: all checks passed\n")
