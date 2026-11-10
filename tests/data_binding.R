# Data bindings: building kernels and likelihoods under a binding of names to data managers.
#   Rscript tests/data_binding.R     (with jheem2 installed)
# JHEEM2_TEST_LIBRARY, if set, is a library to load jheem2 from first.
library.path = Sys.getenv("JHEEM2_TEST_LIBRARY", unset = "")
if (nzchar(library.path)) .libPaths(c(library.path, .libPaths()))
suppressPackageStartupMessages(library(jheem2))
Sys.unsetenv("JHEEM_RUN_RECORD")

expect.error = function(expr, pattern)
{
    message = tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
    if (is.null(message) || !grepl(pattern, message, fixed = T))
        stop("expected an error containing '", pattern, "', got: ", if (is.null(message)) "no error" else message)
}
no.binding.state = function()
{
    state = jheem2:::DATA.BINDING.STATE
    is.null(get.data.binding()) && length(state$stack) == 0 && length(state$frames) == 0
}
directory = tempfile("jheem2-data-binding-")
dir.create(directory)
save.manager = function(data.manager, name)
{
    file = file.path(directory, name)
    saved = data.manager
    save(saved, file = file)
    file
}

#-- The toy fixture (see the jheem-wiki toy-fixture page), with its mortality rate read from a --#
#-- data manager when the kernel is built                                                       --#
make.toy.specification = function(version, get.mortality.rate, declared = NULL)
{
    spec = create.jheem.specification(version = version, iteration = "1", description = "Toy model",
                                      start.year = 1970, age.endpoints = c(0, 50, Inf),
                                      compartments.for.infected.and.uninfected = list(location = "location",
                                                                                      age = "all.ages",
                                                                                      sex = c("female", "male")),
                                      compartments.for.infected.only = list(continuum = c("undiagnosed", "diagnosed")))
    register.model.element(spec, name = "toy.mortality.rate", scale = "rate", get.value.function = get.mortality.rate)
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
    if (!is.null(declared))
        declare.data.managers(spec, declared)
    register.model.specification(spec)
}
# Each toy data manager's description holds the mortality rate it supplies
rate.from = function(name) function(location, specification.metadata) as.numeric(get.data.manager(name)$description)
make.toy.specification("toy", rate.from("census"), declared = c("census", "syphilis"))
make.toy.specification("toy.undecl", rate.from("syphilis"), declared = "census")
make.toy.specification("toy.assign", function(location, specification.metadata){
    CENSUS.MANAGER <<- get.data.manager("syphilis")
    0.01
})
no.parameters = setNames(numeric(0), character(0))
run.toy = function(version = "toy") create.jheem.engine(version, "C.12580", end.year = 1980)$run(no.parameters)
mortality.rate = function(sim) sim$jheem.kernel$.__enclos_env__$private$i.element.backgrounds$toy.mortality.rate$value
sha = function(character) paste(rep(character, 64), collapse = "")

#-- Two census releases and a syphilis release --#
census.a.file = save.manager(create.data.manager("census.build.a", description = "0.01"), "census-a.rdata")
census.b.file = save.manager(create.data.manager("census.build.b", description = "0.05"), "census-b.rdata")
syphilis.file = save.manager(create.data.manager("syphilis.build", description = "0.02"), "syphilis.rdata")
load.release = function(file, release, sha256)
    load.data.manager(file, provenance = list(release = release, sha256 = sha256, repository = "example/repository"))
census.a = load.release(census.a.file, "census-a", sha("a"))
census.b = load.release(census.b.file, "census-b", sha("b"))
syphilis = load.release(syphilis.file, "syphilis-1", sha("c"))
binding.a = create.data.binding(census = census.a, syphilis = syphilis)
binding.b = create.data.binding(census = census.b, syphilis = syphilis)
c1.columns = c("registered.as", "default", "passed", "data.manager.name", "file", "release", "repository",
               "sha256", "md5", "modified", "loaded.at")

#-- With no binding, nothing changes: names come from the registry, records are as before --#
stopifnot(no.binding.state())
register.data.manager("census", census.a)
unbound.sim = run.toy()
unbound.record = get.data.inputs(unbound.sim)
stopifnot(identical(mortality.rate(unbound.sim), 0.01),
          identical(unbound.record$coverage, c("declared", "loaded")),
          identical(names(unbound.record$data.managers), c1.columns))

#-- Two bindings in one session: two kernels with different values and records --#
sim.a = using.data.binding(binding.a, run.toy())
sim.b = using.data.binding(binding.b, run.toy())
stopifnot(no.binding.state(),
          identical(mortality.rate(sim.a), 0.01),
          identical(mortality.rate(sim.b), 0.05),
          !isTRUE(all.equal(sim.a$get("population"), sim.b$get("population"))))
record.a = get.data.inputs(sim.a)
record.b = get.data.inputs(sim.b)
stopifnot(identical(record.a$coverage, c("declared", "loaded", "bound")),
          identical(names(record.a$data.managers), c(c1.columns, "bound.as", "used")))
bound.row = function(record, name) record$data.managers[record$data.managers$bound.as %in% name, ]
stopifnot(identical(bound.row(record.a, "census")$release, "census-a"),
          identical(bound.row(record.a, "census")$registered.as, "census"),
          identical(bound.row(record.a, "census")$used, TRUE),
          identical(bound.row(record.a, "syphilis")$used, FALSE),
          identical(bound.row(record.b, "census")$release, "census-b"),
          identical(bound.row(record.b, "census")$used, TRUE))
# census.a is still registered in the session but not bound when building under binding.b
registered.only = record.b$data.managers[record.b$data.managers$registered.as %in% "census", ]
stopifnot(identical(registered.only$release, "census-a"), is.na(registered.only$bound.as), is.na(registered.only$used))

#-- The record survives the calibration file --#
calibration.dir = file.path(directory, "calibration")
dir.create(calibration.dir)
jheem2:::write.data.inputs.dcf(record.a, dir = calibration.dir)
from.file = get.data.inputs(calibration.dir)
stopifnot(identical(from.file$coverage, record.a$coverage))
for (column in setdiff(names(record.a$data.managers), "loaded.at"))
    stopifnot(identical(from.file$data.managers[[column]], record.a$data.managers[[column]]))

#-- Names a binding lacks, or a specification did not declare, stop the build --#
expect.error(using.data.binding(create.data.binding(syphilis = syphilis), get.data.manager("census")),
             "no data manager is bound as 'census' (bound: 'syphilis')")
expect.error(using.data.binding(create.data.binding(census = census.a), run.toy()),
             "has no data manager for 'syphilis', declared for version 'toy'")
expect.error(using.data.binding(binding.a, run.toy("toy.undecl")),
             "'syphilis' is not among the data managers declared for version 'toy.undecl'")
stopifnot(no.binding.state())
# Without a binding the declaration is not checked
register.data.manager("syphilis", syphilis)
stopifnot(identical(mortality.rate(run.toy("toy.undecl")), 0.02))

#-- The session binding --#
set.data.binding(binding.b)
stopifnot(identical(get.data.binding()$managers$census, census.b),
          identical(get.data.manager("census"), census.b),
          identical(sort(list.data.managers()$name), c("census", "syphilis")))
session.sim = run.toy()
stopifnot(identical(mortality.rate(session.sim), 0.05),
          identical(get.data.inputs(session.sim)$coverage, c("declared", "loaded", "bound")))
# A binding made active for an expression takes precedence, and the session binding returns after
stopifnot(identical(mortality.rate(using.data.binding(binding.a, run.toy())), 0.01),
          identical(get.data.manager("census"), census.b))

#-- Aliases --#
create.data.manager.alias("CENSUS.MANAGER", "census")
stopifnot(identical(CENSUS.MANAGER, census.b),
          using.data.binding(binding.a, identical(CENSUS.MANAGER, census.a)))
CENSUS.MANAGER = census.a  # assigning registers the manager in the session binding
stopifnot(identical(get.data.manager("census"), census.a), identical(CENSUS.MANAGER, census.a))
create.data.manager.alias("CENSUS.MANAGER", "census")  # again, to the same name: nothing changes
expect.error(create.data.manager.alias("CENSUS.MANAGER", "syphilis"),
             "'CENSUS.MANAGER' is already an alias for the data manager named 'census'")
expect.error(using.data.binding(binding.b, CENSUS.MANAGER <- census.b), "Cannot assign to 'CENSUS.MANAGER'")
expect.error(run.toy("toy.assign"), "Cannot assign to 'CENSUS.MANAGER'")
stopifnot(identical(length(jheem2:::DATA.BINDING.STATE$stack), 0L),
          identical(length(jheem2:::DATA.BINDING.STATE$frames), 0L),
          identical(CENSUS.MANAGER, census.a))
# A variable that already holds the registered manager becomes an alias; one holding another stops
SYPHILIS.MANAGER = syphilis
create.data.manager.alias("SYPHILIS.MANAGER", "syphilis")
stopifnot(identical(SYPHILIS.MANAGER, syphilis))
OTHER.CENSUS = census.b
expect.error(create.data.manager.alias("OTHER.CENSUS", "census"),
             "'OTHER.CENSUS' holds a different data manager from the one registered as 'census'")
rm(CENSUS.MANAGER, SYPHILIS.MANAGER, OTHER.CENSUS)
set.data.binding(NULL)
stopifnot(no.binding.state())

#-- Likelihood instructions belong to the binding they were created under --#
create.instructions = function() create.bernoulli.likelihood.instructions(outcome.for.sim = "population",
                                                                          years = c(1975, 1980),
                                                                          probability.decreasing = 0.5,
                                                                          weights = list())
instructions = using.data.binding(binding.a, create.instructions())
invisible(using.data.binding(binding.a, instantiate.likelihood(instructions, version = "toy", location = "C.12580")))
expect.error(using.data.binding(binding.b, instantiate.likelihood(instructions, version = "toy", location = "C.12580")),
             "created under a data binding that differs from the active one for 'census'")
stopifnot(no.binding.state())
invisible(instantiate.likelihood(instructions, version = "toy", location = "C.12580"))  # no binding: not checked
# Instructions created with no binding active, or by an earlier jheem2, have no binding to check
unbound.instructions = create.instructions()
invisible(using.data.binding(binding.b, instantiate.likelihood(unbound.instructions, version = "toy",
                                                               location = "C.12580")))
EARLIER.INSTRUCTIONS = R6::R6Class("jheem.likelihood.instructions", private = list(i.name = "earlier"))
stopifnot(is.null(using.data.binding(binding.b, jheem2:::check.instructions.data.binding(EARLIER.INSTRUCTIONS$new(), ""))))

#-- A likelihood takes a data manager by name, and records the names used --#
instructions = using.data.binding(binding.a, create.instructions())
likelihood = using.data.binding(binding.a, instantiate.likelihood(instructions, version = "toy", location = "C.12580",
                                                                  data.manager = "syphilis"))
likelihood.record = get.data.inputs(likelihood)
stopifnot(identical(bound.row(likelihood.record, "syphilis")$passed, TRUE),
          identical(bound.row(likelihood.record, "syphilis")$used, TRUE),
          identical(bound.row(likelihood.record, "census")$used, FALSE))
# Passing a bound object also marks its name as used
likelihood = using.data.binding(binding.a, instantiate.likelihood(instructions, version = "toy", location = "C.12580",
                                                                  data.manager = census.a))
stopifnot(identical(bound.row(get.data.inputs(likelihood), "census")$used, TRUE))

#-- Calibration registrations keep the name a manager is bound under, resolved at setup --#
stopifnot(identical(using.data.binding(binding.a, jheem2:::calibration.data.manager.name(syphilis, "")), "syphilis"),
          identical(jheem2:::calibration.data.manager.name("census", ""), "census"),
          is.null(jheem2:::calibration.data.manager.name(syphilis, "")),
          is.null(using.data.binding(binding.a, jheem2:::calibration.data.manager.name(census.b, ""))))
info = list(data.manager = census.a, data.manager.name = "census")
stopifnot(identical(using.data.binding(binding.b, jheem2:::resolve.calibration.data.manager(info)), census.b),
          identical(jheem2:::resolve.calibration.data.manager(info), census.a),
          identical(jheem2:::resolve.calibration.data.manager(list(data.manager = census.a)), census.a))

#-- Rebuilding a recorded binding through the application's release resolver --#
release.files = c("census-a" = census.a.file, "census-b" = census.b.file, "syphilis-1" = syphilis.file)
expect.error(rebuild.data.binding(sim.a), "no release resolver is set")
set.data.release.resolver(function(identity) load.release(release.files[[identity$release]], identity$release,
                                                          identity$sha256))
rebuilt = rebuild.data.binding(sim.a)
identity.columns = c("name", "release", "sha256", "md5")
stopifnot(identical(get.data.binding.identity(rebuilt)[, identity.columns],
                    get.data.binding.identity(binding.a)[, identity.columns]),
          identical(mortality.rate(using.data.binding(rebuilt, run.toy())), 0.01))
set.data.release.resolver(function(identity) load.release(census.b.file, identity$release, identity$sha256))
expect.error(rebuild.data.binding(sim.a), "the data manager loaded for 'census' has md5")
expect.error(rebuild.data.binding(unbound.sim), "no data binding was recorded")
set.data.release.resolver(NULL)

#-- Artifacts from earlier versions load, with no record --#
EARLIER.KERNEL = R6::R6Class("jheem.kernel", portable = F, private = list(i.version = "toy"))
stopifnot(is.null(get.data.inputs(EARLIER.KERNEL$new())))
expect.error(rebuild.data.binding(EARLIER.KERNEL$new()), "no data binding was recorded")

#-- Bindings check their contents and print their identity --#
expect.error(create.data.binding(census.a), "every data manager must be named")
expect.error(create.data.binding(census = "census"), "'census' must be an R6 object")
expect.error(using.data.binding(list(), 1), "must be a 'jheem.data.binding' object")
stopifnot(any(grepl("census-a", capture.output(print(binding.a)))))

cat("data_binding.R: all checks passed\n")
