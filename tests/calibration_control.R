# What a calibration control's saved functions keep under options(jheem2.lean.calibration.controls = TRUE).
#   Rscript tests/calibration_control.R     (with jheem2 installed)
# JHEEM2_TEST_LIBRARY, if set, is a library to load jheem2 from first.
library.path = Sys.getenv("JHEEM2_TEST_LIBRARY", unset = "")
if (nzchar(library.path)) .libPaths(c(library.path, .libPaths()))
suppressPackageStartupMessages(library(jheem2))

directory = tempfile("jheem2-calibration-control-")
dir.create(directory)
file = file.path(directory, "manager.rdata")
saved = create.data.manager("toy", description = "a toy manager")
save(saved, file = file)
released = load.data.manager(file, provenance = list(release = "toy-1", repository = "example/repository"))
unreleased = load.data.manager(file)

# A stand-in for set.up.calibration()'s frame: what the functions use, and what they should not keep
frame = new.env()
frame$likelihood = "the likelihood"
frame$engine = "the engine"
frame$calibration.info = list(data.manager = released)
frame$calibration.data.manager = released
used = c("likelihood", "engine")

lean = jheem2:::make.lean.calibration.environment(frame, used, data.manager = released)
stopifnot(setequal(ls(lean, all.names = T), used),            # a released manager is not kept
          identical(lean$engine, "the engine"),
          identical(parent.env(lean), parent.env(frame)))
function.in.frame = function() engine
environment(function.in.frame) = lean
stopifnot(identical(function.in.frame(), "the engine"))

lean = jheem2:::make.lean.calibration.environment(frame, used, data.manager = unreleased)
stopifnot(setequal(ls(lean, all.names = T), c(used, "data.manager.without.release")),
          identical(lean$data.manager.without.release, unreleased))   # the only kept copy of its data

lean = jheem2:::make.lean.calibration.environment(frame, used, data.manager = NULL)
stopifnot(setequal(ls(lean, all.names = T), used))                    # no manager (Bernoulli likelihoods)

stopifnot(inherits(tryCatch(jheem2:::make.lean.calibration.environment(frame, c(used, "missing"), released),
                            error = function(e) e), "error"))

cat("calibration_control.R: all checks passed\n")
