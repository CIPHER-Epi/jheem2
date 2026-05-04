# Without a debug added, the error will be thrown by the engine when it runs
# a simulation with a set of parameters which has NA values in it.
# I recommend adding a browser statement anywhere before the "Check the initial
# sims to make sure the likelihiood computes (not to -Inf)" section of 
# CALIBRATION_main.R.

source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
source('../jheem_analyses/applications/SHIELD/shield_calib_register.R')

set.up.calibration(version="shield",
                   location="C.12580",
                   calibration.code = "calib.4.30.stage3.8th",
                   cache.frequency = 500 #100 #how often write the results to disk
)