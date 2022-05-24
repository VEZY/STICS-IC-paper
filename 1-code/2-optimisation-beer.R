# Purpose: automatically optimize the main parameter values of the model for
# contrasted situations, using the beer's law of light extinction.
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

# First step --------------------------------------------------------------

# Copy all USMs into a new folder were we will change the parameter values (and keep the original files).
# The original folder is 0-data/usms, the destination folder is 0-data/usms-optim-beer

# Set-up the optimization process:
df_optim = read.csv("0-data/calibration.csv", sep = ";")
# /!\ Important step: change ktrou for extin for beer-lambert law
df_optim$Parameter[grep("ktrou", df_optim$Parameter)] = "extin"
df_optim$low_boundary[grep("extin", df_optim$Parameter)] = 0.6
df_optim$high_boundary[grep("extin", df_optim$Parameter)] = 1.9

javastics_path = normalizePath("0-javastics", winslash = "/")
worspaces_path = normalizePath("0-data/usms-optim-beer", winslash = "/")

# workspace_usms is a list of workspace-name -> usm names for all sole crop USMs
workspace_usms =
  list(
    "Angers-SC-Barley" = "SC_Barley_Angers_2003_N0",
    "Angers-SC-Pea" = "SC_Pea_Angers_2003_N0",
    "Auzeville-Pea-SC" = "SC_Pea_2005-2006_N0", # substitutive (mixed between rows)
    "Auzeville-Wheat-SC" = "SC_Wheat_2005-2006_N0", # substitutive 
    "Auzeville-Pea-2012-SC" = "SC_Pea_2012-2013_N1", # mixed on the row
    "Auzeville-Wheat-2012-SC" = "SC_Wheat_2012-2013_N1", # mixed on the row
    "sojaTardif2012-SC" = "SojaTardif-SC2012",
    "tourPrecoce2012-SC" = "TournPrecoce-SC2012",
    "Auzeville_wfb-Fababean-SC" = "Fababean_SC_2007",
    "Auzeville_wfb-Wheat-SC" = "Wheat_SC_2007"
  )

plant_files = list.files(file.path("0-data/usms-optim-beer",names(workspace_usms), "plant"), full.names = TRUE)

# workspace_usms_IC is a list of workspace-name -> usm names for all intercrop USMs
workspace_usms_IC =
  list(
    "Angers-IC-Pea_Barley" = "IC_PeaBarley_Angers_2003_N0_D50-50", # replace by N1?
    "Auzeville-IC" = "IC_Wheat_Pea_2005-2006_N0",
    "Auzeville-IC-2012" = "IC_Wheat_Pea_2012-2013_N1",
    "1Tprecoce2Stardif2012" = "1Tprecoce2Stardif2012",
    "Auzeville_wfb-Fababean-Wheat-IC" = "Fababean_Wheat_IC_2007"
  )

# workspace_usms_cor is a list that links which intercrop workspaces correspong to 
# which sole crop workspaces:
workspace_usms_cor =
  list(
    "Angers-IC-Pea_Barley" = c(p = "Angers-SC-Pea", a = "Angers-SC-Barley"),
    "Auzeville-IC" = c(p = "Auzeville-Wheat-SC", a = "Auzeville-Pea-SC"),
    "Auzeville-IC-2012" = c(p = "Auzeville-Wheat-2012-SC", a = "Auzeville-Pea-2012-SC"),
    "1Tprecoce2Stardif2012" = c(p = "tourPrecoce2012-SC", a = "sojaTardif2012-SC"),
    "Auzeville_wfb-Fababean-Wheat-IC" = c(p = "Auzeville_wfb-Fababean-SC", a = "Auzeville_wfb-Wheat-SC")
  )

# Activate Beer's law:
lapply(plant_files, function(x){
  set_param_xml(
    file = x,
    param = "codetransrad",
    values = 1,
    overwrite = TRUE
  )
})

# Extract the information about the parameters to calibrate:
parameters_vars = extract_parameters(df_optim)
# Careful, this is very long (2/3 days):
param_values = optimize_workspace(worspaces_path, workspace_usms, parameters_vars, javastics_path)

# Summarizing the optimization results ------------------------------------

workspaces_orig = normalizePath(file.path("0-data/usms",names(workspace_usms)), winslash = "/")
workspaces_opti = normalizePath(file.path(worspaces_path,names(workspace_usms)), winslash = "/")
parameters_vars = extract_parameters(df_optim)

df = summarize_optimization(workspaces_orig, workspaces_opti, parameters_vars)

write.csv(df, "2-outputs/optimization/optim_results_beer.csv", row.names = FALSE)

# Update the xml files for intercrops -------------------------------------

# Activate Beer's law:
lapply(list.files(file.path(workspaces_opti,"plant"), full.names = TRUE), function(x){
  set_param_xml(
    file = x,
    param = "codetransrad",
    values = 1,
    overwrite = TRUE
  )
})

mapply(
  function(x,y){
    mapply(function(z, dominance){
      plant_optimized = list.files(file.path(worspaces_path, z, "plant"), full.names = TRUE)
      ic_plant = file.path(worspaces_path, x, "plant", basename(plant_optimized))
      file.copy(from = plant_optimized,
                to = ic_plant,
                overwrite = TRUE)
    }, y, names(y))
  },
  names(workspace_usms_cor),
  workspace_usms_cor
)

# Comparison before and after ---------------------------------------------

# sim_variables = c("lai(n)","QNplante","Qfix","masec(n)","hauteur","CNgrain","mafruit","chargefruit")
sim_variables = unique(unlist(lapply(parameters_vars, function(x) x$vars)))
sim_variables = c(sim_variables, "hauteur", "largeur")

# Run the simulations -----------------------------------------------------

res_orig_beer = run_simulation(
  workspaces = normalizePath(file.path("0-data/usms",names(workspace_usms_IC)), winslash = "/"),
  variables = sim_variables,
  javastics = javastics_path,
  usms = workspace_usms_IC
)
res_opti_beer = run_simulation(
  workspaces = normalizePath(file.path(worspaces_path,names(workspace_usms_IC)), winslash = "/"),
  variables = sim_variables,
  javastics = javastics_path,
  usms = workspace_usms_IC
)
# If you already made the simulations you can import them (faster):
# res_orig_beer = import_simulations(workspaces = workspaces_orig, variables = sim_variables)
# res_opti_beer = import_simulations(workspaces = workspaces_opti, variables = sim_variables)

# Make the plots ----------------------------------------------------------

plotting_var = sim_variables
# plotting_var = c("fapar","lai_n", "hauteur", "QNplante")
# plotting_var = c("hauteur", "QNplante", "lai_n")

# Plots per workspace:
dynamic_plots =
  mapply(
    function(x,y){
      plot(orig_radiative = x$sim, beer_optim = y$sim, obs = x$obs, type = "dynamic", verbose = FALSE,
           var = SticsRFiles:::var_to_col_names(plotting_var))
    },
    res_orig_beer,
    res_opti_beer)

dynamic_plots$`Auzeville-IC-2012.IC_Wheat_Pea_2012-2013_N1`

# Save the plots to disk:
mapply(function(x,y){
  ggplot2::ggsave(
    filename = paste0(x,".png"),
    path =  "2-outputs/optimization/plots-beer",
    plot = y,
    width = 16,
    height = 16.5,
    units = "cm"
  )
},names(dynamic_plots), dynamic_plots)
