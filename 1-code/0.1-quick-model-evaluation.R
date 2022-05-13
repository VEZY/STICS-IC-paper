# Purpose: simulate USMs with different parameter values and compare the outputs
# Date: 13/05/2022
# Author: R. Vezy

# Install the packages (to do only once) ----------------------------------

# remotes::install_github("SticsRPacks/SticsRPacks")

# Import the packages -----------------------------------------------------

library(SticsRPacks)
library(dplyr)
source("1-code/functions.R")

# Define javaStics installation -------------------------------------------

# STICS version -----------------------------------------------------------

javastics = normalizePath("0-javastics", winslash = "/")

# Define the workspaces ---------------------------------------------------
# workspace_opt = "0-data/usms-optim-radiative/Angers-SC-Pea"
# workspace_opt = "0-data/usms-optim-radiative/Angers-SC-Barley"
# workspace_opt = "0-data/usms-optim-radiative/Angers-IC-Pea_Barley"
# workspace_opt = "0-data/usms-optim-radiative/Auzeville_wfb-Wheat-SC"
# workspace_opt = "0-data/usms-optim-radiative/Auzeville_wfb-Fababean-SC"
# workspace_opt = "0-data/usms-optim-radiative/Auzeville_wfb-Fababean-Wheat-IC"
workspace_opt = normalizePath("0-data/usms-optim-radiative/Auzeville-IC", winslash = "/")
# workspace_opt = "0-data/usms-optim-radiative/1Tprecoce2Stardif2012"

# Define the variables to simulate ----------------------------------------

sim_variables = c("lai(n)","masec(n)","QNplante","Qfix","mafruit","fapar","ilaxs","idrps","imats","tustress","hauteur","chargefruit")
# SticsRFiles::get_var_info("ulai(n)") # you can search variables with this one

# Run the simulations -----------------------------------------------------

# List possible usms in the workspace:
SticsRFiles::get_usms_list(file = file.path(workspace_opt,"usms.xml"))
# choose your usm:
usms = "IC_Wheat_Pea_2012-2013_N1"

# Get the observations:
obs = get_obs(
  workspace = workspace_opt,
  usm = usms,
  usms_file = file.path(workspace_opt,"usms.xml")
)

# Set the variables to simulate:
SticsRFiles::gen_varmod(workspace_opt, sim_variables)

# Run the simulation:
SticsOnR::run_javastics(
  javastics = javastics,
  workspace = workspace_opt,
  stics_exe = "Stics_IC_v13-05-2022.exe",
  usms_list = usms
)

# Get the simulation output:
sim_regular = get_sim(
  workspace = workspace_opt,
  usm = usms,
  usms_file = file.path(workspace_opt,"usms.xml")
)

# Plot the results:
plot(regular = sim_regular, obs = obs)


# We can change the values of a parameter and compare the outputs with the previous simulation:
set_param_xml(
  file = file.path(workspace_opt,"plant","pea-sebastian_plt.xml"),
  param_name = "stdnofno", 
  param_value = 1000, 
  overwrite = TRUE
)

# Re-run the model with the new values:
SticsOnR::run_javastics(
  javastics_path = javastics, 
  workspace_path = workspace_opt,
  stics_exe = "Stics_IC_v13-05-2022.exe",
  usms_list = usms
)

# Import the new results:
sim_new = get_sim(workspace = workspace_opt,
                      usm_name = usms,
                      usms_filepath = file.path(workspace_opt,"usms.xml"))

# And compare the two outputs:
plot(regular = sim_regular, sim_new = sim_new, obs = obs)
