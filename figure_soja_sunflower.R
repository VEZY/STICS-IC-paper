# Purpose: simulate inter-crops with marked dominance either using the beer law 
# for mixed crops or the radiative transert
# Date: 16/04/2021

# Install the packages (to do only once) ----------------------------------

# remotes::install_github("SticsRPacks/SticsRPacks")

# Import the packages -----------------------------------------------------

library(SticsRPacks)
library(tidyverse)
source("1-code/functions.R")

# Define javaStics installation -------------------------------------------

# STICS version -----------------------------------------------------------

javastics = normalizePath("0-javastics", winslash = "/")

# Define the workspaces ---------------------------------------------------

workspace_sunflower = "0-data/usms/tour2012-SC"
workspace_soja = "0-data/usms/soja2012-SC"
workspace_sunflower_soja = "0-data/usms/toursoja2012-IC"
workspaces = list(sunflower = workspace_sunflower, soja = workspace_soja, sunflower_soja = workspace_sunflower_soja)

# Define the variables to simulate ----------------------------------------

# sim_variables = c("lai(n)","masec(n)","QNplante","mafruit","Qfix","profexteau","profextN","hauteur")
sim_variables = c("lai(n)","masec(n)","QNplante","mafruit","Qfix")

# SticsRFiles::get_var_info("Qfix")

# Run the simulations -----------------------------------------------------

# usms = SticsRFiles::get_usms_list(usm_path = file.path(workspace_sunflower_soja,"usms.xml"))

usms_sunflower = "TournPrecoce-SC2012" # Test with the late one also
usms_soja = "SojaPrecoce-SC2012"
usms_sunflower_soja = "1Ttardif2Sprecoce2012"
usms = list(sunflower = usms_sunflower, soja = usms_soja, sunflower_soja = usms_sunflower_soja)

lapply(workspaces, function(x) SticsRFiles::gen_varmod(x, sim_variables))


mapply(function(x,y){
  SticsOnR::run_javastics(javastics_path = javastics, workspace_path = x,
                          stics_exe = "Stics_IC_v18-10-2021.exe",
                          usms_list = y)
},workspaces,usms)

sim = mapply(function(x,y){
  get_sim(workspace = x, usm_name = y, usms_filepath = file.path(x,"usms.xml"))
},workspaces,usms)
names(sim) = usms
attr(sim, "class") = "cropr_simulation"

# Get the observations

obs = mapply(function(x,y){
  get_obs(workspace = x, usm_name = y, usms_filepath = file.path(x,"usms.xml"))
},workspaces,usms)
names(obs) = usms

# Make the plots:
plots = plot(sim,obs=obs)


# ggsave(filename = "Beer_vs_radiative.png", path = "2-outputs/plots",
#        width = 16, height = 16, units = "cm")

