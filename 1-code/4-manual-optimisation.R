# Purpose: manually optimize the main parameter values for which the automatic
# optimization did not work properly. This is done only for parameters of the last
# steps of the optimization, meaning the ones not correlated to others. This is
# just some tweaking because I am a little bit perfectionist.
# Date: 13/05/2022
# Author: R. Vezy

# Install the packages (to do only once) ----------------------------------

# remotes::install_github("SticsRPacks/SticsRPacks")

# Import the packages -----------------------------------------------------

library(SticsRPacks)
library(foreach)
library(doParallel)
library(dplyr)
source("1-code/functions.R")

# STICS set-up -----------------------------------------------------------

javastics_path = normalizePath("0-javastics", winslash = "/")
javastics_workspace_path = normalizePath(file.path("0-data/usms-optim-radiative",names(workspace_usms)[i]), winslash = "/")
# Name of the folder where text files will be written:
stics_inputs_path = file.path(javastics_workspace_path, "manual_optimization")

workspace_usms =
  list(
    "Angers-SC-Barley" = "SC_Barley_Angers_2003_N0",
    "Angers-SC-Pea" = "SC_Pea_Angers_2003_N0",
    "Auzeville-Pea-SC" = "SC_Pea_2005-2006_N0",
    "Auzeville-Wheat-SC" = "SC_Wheat_2005-2006_N0",
    "Auzeville-Pea-2012-SC" = "SC_Pea_2012-2013_N1",
    "Auzeville-Wheat-2012-SC" = "SC_Wheat_2012-2013_N1",
    "sojaTardif2012-SC" = "SojaTardif-SC2012",
    "tourPrecoce2012-SC" = "TournPrecoce-SC2012",
    "Auzeville_wfb-Fababean-SC" = "Fababean_SC_2010",
    "Auzeville_wfb-Wheat-SC" = "Wheat_SC_2010"
  )

# Choose which workspace to simulate:
i = 2
usms = workspace_usms[[i]]

# Choose the variables:
var_name = c("lai_n", "masec_n", "mafruit", "CNgrain", "QNplante")
# var_name = c("vitircarbT", "lai_n")

# Import the observations and filter the variables:
obs_list = get_obs(javastics_workspace_path, usm = usms)
obs_list = filter_obs(obs_list, var= var_name, include=TRUE)

# Create the directory of simulation and generate the text files:
dir.create(stics_inputs_path, showWarnings = FALSE)

gen_usms_xml2txt(
  javastics = javastics_path,
  workspace  = javastics_workspace_path,
  out_dir = stics_inputs_path,
  usm = usms,
  verbose = TRUE
)

# Define the model options:
model_options =
  stics_wrapper_options(
    javastics = javastics_path,
    workspace = stics_inputs_path,
    parallel = FALSE, # Because we have only one usm per workspace so no need
    stics_exe = "Stics_IC_v17-05-2022.exe"
  )

# Get the current (previous) parameter value:
get_param_txt(file.path(stics_inputs_path,usms), "cgrain")

# Simulate the original value without changing parameters:
res_orig = stics_wrapper(
  model_options = model_options,
  situation = usms,
  var = var_name,
)

# Simulate with different parameter values (re-run as many times as needed):
res_opti = stics_wrapper(
  model_options = model_options,
  param_values = c(
    # "stdrpmat" = 550,
    # "stlevamf" = 380,
    "pentlaimax" = 3.8,
    "laicomp" = 0.10,
    "dlaimaxbrut" = 0.00029,
    # "stdrpmat" = 660,
    "vlaimax" = 2.0,
    # "dlaimaxbrut" = 0.00021,
    "ktrou" = 0.9,
    "stamflax" = 320,
    "efcroijuv" = 3.5,
    "efcroirepro" = 4.3,
    "efcroiveg" = 3.5,
    "durvieF" = 290,
    # "nbgrmin" = 1540,
    "cgrain" = 0.138,
    "croirac" = 0.2,
    # "nbgrmin" = 2000,
    "Vmax2" = 0.0039,
    "inngrain2" = 1.3,
    "vitircarbT"= 0.00082
    # "innsen" = 0.2
  ),
  situation = usms,
  var = var_name,
)

# Compare the outputs before and after changing the values:
plot(orig = res_orig$sim_list, optim = res_opti$sim_list, obs = obs_list)

# /!\ Remember that stics_wrapper does not update the values of the parameter in
# the XML files, so you'll need to manually change them when you're happy.

# Intercrop simulation ----------------------------------------------------

# Carefull, this step is just for control, DO NOT optimize parameter values to the
# intercrop USMs as it would defeat the working hypothesis of STICS: the model should
# be optimized on sole crop and then applied to intercrop and simulate the
# interactions. If the outputs are not good, then the formalisms should be updated.
# Keep in mind that IC simulations should be done under the same conditions than
# the ones it was optimized on in SC, has for any SC calibrations and evaluations.

# Plot the intercrop version of this USM
workspace_IC = list("Auzeville-IC-2012" = "IC_Wheat_Pea_2012-2013_N1")
var_name2 = c("lai_n", "masec_n", "mafruit", "QNplante", "hauteur", "fapar", "demande")

res = run_simulation(
  workspaces = normalizePath(file.path("0-data/usms-optim-radiative",names(workspace_IC)), winslash = "/"),
  variables = var_name2,
  javastics = javastics_path,
  usms = workspace_IC
)

plot(
  res$`Auzeville-IC-2012`$sim,
  obs = res_opti_beer$`Auzeville-IC-2012`$obs,
  type = "dynamic", verbose = FALSE,
  var = SticsRFiles:::var_to_col_names(var_name2)
)
