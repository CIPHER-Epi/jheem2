
# Sourcing this file is an alternative to a call to 
#   library(jheem2)
# For testing

if (!exists("JHEEM2.FUNCTION.NAMES"))
    JHEEM2.FUNCTION.NAMES = character()

PRE.SOURCE.JHEEM2.FUNCTION.NAMES = names(get(".GlobalEnv"))[sapply(get(".GlobalEnv"), is.function)]

source("../jheem2/R/FILE_MANAGER_file_manager.R")


source('../jheem2/R/ONTOLOGY_ontology_mappings.R')
Rcpp::sourceCpp('../jheem2/src/ontology_mappings.cpp')

source('../jheem2/R/HELPERS_misc_helpers.R')
source('../jheem2/R/HELPERS_dim_names_helpers.R')
source('../jheem2/R/HELPERS_array_helpers.R')
source('../jheem2/R/HELPERS_age_year_helpers.R')
source('../jheem2/R/HELPERS_bundle_function.R')
Rcpp::sourceCpp('../jheem2/src/array_helpers.cpp')

source('../jheem2/R/ONTOLOGY_ontology.R')
source('../jheem2/R/ONTOLOGY_ontology_mappings.R')
source('../jheem2/R/JHEEM_outcome_location_mappings.R')

source('../jheem2/R/SPECIFICATION_links.R')
source('../jheem2/R/SPECIFICATION_metadata.R')
source('../jheem2/R/SPECIFICATION_scales.R')
source('../jheem2/R/SPECIFICATION_introspection.R')
source('../jheem2/R/SPECIFICATION_model_specification.R')
source('../jheem2/R/SPECIFICATION_compiled_specification.R')

source('../jheem2/R/SPECIFICATION_functional_forms.R')
source('../jheem2/R/SPECIFICATION_functional_form_alphas.R')
Rcpp::sourceCpp('../jheem2/src/functional_forms.cpp')
source('../jheem2/R/SPECIFICATION_evaluatable_value.R')

source('../jheem2/R/VERSIONS_version_manager.R')


source('../jheem2/R/INTERVENTIONS_target_populations.R')
source('../jheem2/R/INTERVENTIONS_intervention_effects.R')
source("../jheem2/R/INTERVENTIONS_main.R")
source('../jheem2/R/INTERVENTIONS_foreground.R')
source('../jheem2/R/INTERVENTIONS_criteria_based.R')

source('../jheem2/R/JHEEM_entity.R')
source('../jheem2/R/JHEEM_diffeq_interface.R')
source('../jheem2/R/JHEEM_engine.R')
source('../jheem2/R/JHEEM_engine.R')
source('../jheem2/R/JHEEM_transmutation.R')
Rcpp::sourceCpp('../jheem2/src/engine_helpers.cpp')
Rcpp::sourceCpp('../jheem2/src/engine_optimizations.cpp')
Rcpp::sourceCpp('../jheem2/src/diffeq.cpp')
Rcpp::sourceCpp('../jheem2/src/outcomes.cpp')

source('../jheem2/R/JHEEM_run_metadata.R')
source('../jheem2/R/JHEEM_kernel.R')
source('../jheem2/R/JHEEM_simulation.R')
source('../jheem2/R/JHEEM_simset_collection.R')
Rcpp::sourceCpp('../jheem2/src/simulation_helpers.cpp')

source('../jheem2/R/DATA_MANAGER_data_manager.R')
source('../jheem2/R/DATA_MANAGER_provenance.R')
source('../jheem2/R/DATA_MANAGER_data_inputs.R')
source('../jheem2/R/PLOTS_simplot.R')
source('../jheem2/R/PLOTS_plot_simulations.R')
source('../jheem2/R/PLOTS_style_manager.R')

source('../jheem2/R/LIKELIHOODS_basic_likelihood.R')
source('../jheem2/R/LIKELIHOODS_basic_ratio_likelihood.R') 
source('../jheem2/R/LIKELIHOODS_main.R')
source('../jheem2/R/LIKELIHOODS_joint_likelihood.R')
source('../jheem2/R/LIKELIHOODS_nested_proportion_likelihood.R')
source('../jheem2/R/LIKELIHOODS_bernoulli_likelihood.R')
source('../jheem2/R/LIKELIHOODS_ifelse_likelihood_instructions.R')
source('../jheem2/R/LIKELIHOODS_custom_likelihood.R')
Rcpp::sourceCpp('../jheem2/src/correlation_matrix_helpers.cpp')
Rcpp::sourceCpp('../jheem2/src/likelihood_helpers.cpp')
Rcpp::sourceCpp('../jheem2/src/nested_proportion_likelihood.cpp')

source("../jheem2/R/CALIBRATION_main.R")
source("../jheem2/R/CALIBRATION_transmute_calibration.R")


Rcpp::sourceCpp('../jheem2/src/misc_helpers.cpp')
Rcpp::sourceCpp('../jheem2/src/lag_matrix.cpp')

source("../jheem2/R/DEBUGGING_error_manager.R")

JHEEM2.FUNCTION.NAMES = union(JHEEM2.FUNCTION.NAMES,
                              setdiff(names(get(".GlobalEnv"))[sapply(get(".GlobalEnv"), is.function)],
                                      PRE.SOURCE.JHEEM2.FUNCTION.NAMES))