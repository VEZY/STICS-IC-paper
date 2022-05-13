# Purpose: simulate contrasted intercrop systems with Beer-Lambert or Radiative
# Transfer to see if the latter is better in complex systems
# Date: 10/11/2021

# Install the packages (to do only once) ----------------------------------

# remotes::install_github("SticsRPacks/SticsRPacks")

# Import the packages -----------------------------------------------------

library(SticsRPacks)
library(tidyverse)
source("1-code/functions.R")

javastics_path = normalizePath("0-javastics", winslash = "/")

workspace_usms =
  list(
    "Auzeville-IC" = "IC_Wheat_Pea_2005-2006_N0",
    "1Tprecoce2Stardif2012" = "1Tprecoce2Stardif2012"
  )

# workspace_usms is a list of workspace-name -> usm names

worskpaces_beer = file.path("0-data/usms-optim-beer", names(workspace_usms))
worskpaces_rad = file.path("0-data/usms-optim-radiative", names(workspace_usms))


sim_variables = c("cumraint","lai(n)","masec(n)","QNplante","mafruit","Qfix")

# Run the simulations -----------------------------------------------------

for (i in 1:length(workspace_usms)) {
  SticsRFiles::gen_varmod(worskpaces_beer[i], sim_variables)
  SticsOnR::run_javastics(javastics_path = javastics_path,
                          workspace_path = worskpaces_beer[i],
                          stics_exe = "Stics_IC_v13-05-2022.exe",
                          usms_list = workspace_usms[[i]])
  SticsRFiles::gen_varmod(worskpaces_rad[i], sim_variables)
  SticsOnR::run_javastics(javastics_path = javastics_path,
                          workspace_path = worskpaces_rad[i],
                          stics_exe = "Stics_IC_v13-05-2022.exe",
                          usms_list = workspace_usms[[i]])
}

sim_beer = mapply(function(x,y){
  get_sim(workspace = x,
          usm_name = y,
          usms_filepath = file.path(x, "usms.xml"))
},worskpaces_beer, workspace_usms)

sim_rad = mapply(function(x,y){
  get_sim(workspace = x,
          usm_name = y,
          usms_filepath = file.path(x, "usms.xml"))
},worskpaces_rad, workspace_usms)

names(sim_beer) = names(sim_rad) = unlist(workspace_usms)
attr(sim_beer, "class") = attr(sim_rad, "class") = "cropr_simulation"

# Get the observations

obs = mapply(function(x,y){
  get_obs(workspace = x, usm_name = y, usms_filepath = file.path(x, "usms.xml"))
},worskpaces_rad,workspace_usms)
names(obs) = unlist(workspace_usms)


# Make the plots:
plots = plot(
  "Beer" = sim_beer,
  "Rad. trans." = sim_rad,
  obs = obs,
  type = "scatter"
)

plots
