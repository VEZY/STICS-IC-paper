# Purpose: automatically optimize the main parameter values of the model for
# contrasted situations with the radiative transfer computation.
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

# Copy all usms from 0-data/usms-optim-beer into 0-data/usms-optimized.
# The usms are first optimized for the Beer-lambert law of extinction because
# simulations with the radiative transfert option may fall back to the Beer
# computation for some days if the difference in height between the crops is low
# i.e. if it is < hauteur_threshold.

# Set-up the optimization process:
df_optim <- read.csv("0-data/calibration.csv", sep = ";")
javastics_path <- normalizePath("0-javastics", winslash = "/")
worspaces_path <- normalizePath("0-data/usms-optim-radiative", winslash = "/")

# workspace_usms is a list of workspace-name -> usm names for all sole crop USMs
workspace_usms <-
  list(
    "Angers-SC-Barley" = "SC_Barley_Angers_2003_N0",
    "Angers-SC-Pea" = "SC_Pea_Angers_2003_N0",
    "Auzeville-Pea-SC" = "SC_Pea_2005-2006_N0",
    "Auzeville-Wheat-SC" = "SC_Wheat_2005-2006_N0",
    "Auzeville-Pea-2012-SC" = "SC_Pea_2012-2013_N1",
    "Auzeville-Wheat-2012-SC" = "SC_Wheat_2012-2013_N1",
    "sojaTardif2012-SC" = "SojaTardif-SC2012",
    "tourPrecoce2012-SC" = "TournPrecoce-SC2012",
    "Auzeville_wfb-Fababean-SC" = "Fababean_SC_2007",
    "Auzeville_wfb-Wheat-SC" = "Wheat_SC_2007"
  )

plant_files <- normalizePath(list.files(file.path(worspaces_path, names(workspace_usms), "plant"), full.names = TRUE))
plant_files_beer <- normalizePath(list.files(file.path("0-data/usms-optim-beer", names(workspace_usms), "plant"), full.names = TRUE))

# workspace_usms_IC is a list of workspace-name -> usm names for all intercrop USMs
workspace_usms_IC <-
  list(
    "Angers-IC-Pea_Barley" = c(p = "Angers-SC-Pea", a = "Angers-SC-Barley"),
    "Auzeville-IC" = c(p = "Auzeville-Wheat-SC", a = "Auzeville-Pea-SC"),
    "Auzeville-IC-2012" = c(p = "Auzeville-Wheat-2012-SC", a = "Auzeville-Pea-2012-SC"),
    "1Tprecoce2Stardif2012" = c(p = "tourPrecoce2012-SC", a = "sojaTardif2012-SC"),
    "Auzeville_wfb-Fababean-Wheat-IC" = c(p = "Auzeville_wfb-Fababean-SC", a = "Auzeville_wfb-Wheat-SC")
  )

# Activate Radiative transfer, and put the optimized extin value from beer optim:
for (i in 1:length(plant_files)) {
  extin <- unlist(get_param_xml(
    file = file.path(
      "0-data/usms-optim-beer",
      basename(dirname(dirname(plant_files[i]))),
      "plant",
      basename(plant_files[i])
    ),
    param = "extin"
  ))

  set_param_xml(
    file = plant_files[i],
    param = "extin",
    values = extin,
    overwrite = TRUE
  )

  set_param_xml(
    file = plant_files[i],
    param = "codetransrad",
    values = 2,
    overwrite = TRUE
  )
}

# /!\ Careful, this is very long (2/3 days): /!\
parameters_vars <- extract_parameters(df_optim)
param_values <- optimize_workspace(
  worspaces_path,
  workspace_usms,
  parameters_vars,
  javastics_path,
  stics_version = "Stics_IC_v24-05-2022_mac"
)

# Summarizing the optimization results ------------------------------------

workspaces_orig <- normalizePath(file.path("0-data/usms", names(workspace_usms)), winslash = "/")
workspaces_opti <- normalizePath(file.path(worspaces_path, names(workspace_usms)), winslash = "/")
parameters_vars <- extract_parameters(df_optim)

df <- summarize_optimization(workspaces_orig, workspaces_opti, parameters_vars)

write.csv(df, "2-outputs/optimization/optim_results.csv", row.names = FALSE)

# Comparison before and after ---------------------------------------------

# sim_variables = c("lai(n)","QNplante","Qfix","masec(n)","hauteur","CNgrain","mafruit","chargefruit")
sim_variables <- unique(unlist(lapply(parameters_vars, function(x) x$vars)))
sim_variables <- c(sim_variables, "hauteur")

# Run the simulations -----------------------------------------------------

res_orig <- run_simulation(
  workspaces = workspaces_orig,
  variables = sim_variables,
  javastics = javastics_path,
  usms = workspace_usms
)
res_opti <- run_simulation(
  workspaces = workspaces_opti,
  variables = sim_variables,
  javastics = javastics_path,
  usms = workspace_usms
)
# If you already made the simulations you can import them (faster):
# res_orig = import_simulations(workspaces = workspaces_orig, variables = sim_variables)
# res_opti = import_simulations(workspaces = workspaces_opti, variables = sim_variables)

# Make the plots ----------------------------------------------------------

plotting_var <- sim_variables

# Plots per workspace:
dynamic_plots <-
  mapply(
    function(x, y) {
      plot(
        orig = x$sim, optim = y$sim, obs = x$obs, type = "dynamic", verbose = FALSE,
        var = SticsRFiles:::var_to_col_names(plotting_var)
      )
    },
    res_orig,
    res_opti
  )

dynamic_plots$`Auzeville-Pea-2012-SC.SC_Pea_2012-2013_N1`
dynamic_plots$`Auzeville-Wheat-2012-SC.SC_Wheat_2012-2013_N1`
dynamic_plots$`Angers-SC-Pea.SC_Pea_Angers_2003_N0`
dynamic_plots$`Angers-SC-Barley.SC_Barley_Angers_2003_N1`
dynamic_plots$`Auzeville_wfb-Fababean-SC.Fababean_SC_2007`
dynamic_plots$`Angers-SC-Barley.SC_Barley_Angers_2003_N0`

# Update the xml files for intercrops -------------------------------------

mapply(
  function(x, y) {
    mapply(function(z, dominance) {
      plant_optimized <- list.files(file.path(worspaces_path, z, "plant"), full.names = TRUE)
      ic_plant <- file.path(worspaces_path, x, "plant", basename(plant_optimized))
      file.copy(
        from = plant_optimized,
        to = ic_plant,
        overwrite = TRUE
      )

      # Update haut_dev_x01 and haut_dev_x02 in param_newform:
      param_newform_optim <- file.path(worspaces_path, z, "param_newform.xml")
      param_newform_ic <- file.path(worspaces_path, x, "param_newform.xml")

      set_param_xml(
        param_newform_ic,
        param = ifelse(dominance == "p", "haut_dev_x01", "haut_dev_x02"),
        values = unlist(get_param_xml(param_newform_optim, param_name = "haut_dev_x01")),
        overwrite = TRUE
      )
      set_param_xml(
        param_newform_ic,
        param = ifelse(dominance == "p", "haut_dev_k1", "haut_dev_k2"),
        values = unlist(get_param_xml(param_newform_optim, param_name = "haut_dev_k1")),
        overwrite = TRUE
      )
    }, y, names(y))
  },
  names(workspace_usms_IC),
  workspace_usms_IC
)
