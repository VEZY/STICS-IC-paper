# Purpose: simulate inter-crops with marked dominance either using the beer law
# for mixed crops or the radiative transert
# Date: 16/04/2021

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
# workspace_pea_barley = "0-data/usms-optim-radiative/Angers-SC-Pea"
# workspace_pea_barley = "0-data/usms-optim-radiative/Angers-SC-Barley"
# workspace_pea_barley = "0-data/usms-optim-radiative/Angers-IC-Pea_Barley"
# workspace_pea_barley = "0-data/usms-optim-radiative/Auzeville_wfb-Wheat-SC"
# workspace_pea_barley = "0-data/usms-optim-radiative/Auzeville_wfb-Fababean-SC"
# workspace_pea_barley = "0-data/usms-optim-radiative/Auzeville_wfb-Fababean-Wheat-IC"
workspace_pea_barley = normalizePath("0-data/usms-optim-radiative/Auzeville-IC", winslash = "/")
# workspace_pea_barley = "0-data/usms-optim-radiative/1Tprecoce2Stardif2012"

# Define the variables to simulate ----------------------------------------

# sim_variables = c("lai(n)","masec(n)","QNplante","mafruit","Qfix","profexteau","profextN","hauteur")
sim_variables = c("lai(n)","masec(n)","QNplante","Qfix","mafruit","fapar","ilaxs","idrps","imats","tustress","hauteur","chargefruit")
# sim_variables = c("lai(n)","masec(n)","QNplante","Qfix","mafruit","fapar","tustress","ulai(n)","efdensite","iamfs","hauteur","chargefruit")

# SticsRFiles::get_var_info("ulai(n)")

# Run the simulations -----------------------------------------------------

SticsRFiles::get_usms_list(file = file.path(workspace_pea_barley,"usms.xml"))
usms = "IC_Wheat_Pea_2012-2013_N1"
# usms = "SC_Wheat_2012-2013_N0"
# usms= "Fababean_Wheat_IC_2007"
# usms = "Wheat_SC_2007"
# usms = "Fababean_SC_2007"
# usms = "TournPrecoce-SC2012"
# usms = "SojaTardif-SC2012"
# usms = "1Tprecoce2Stardif2012"
# usms = c("SC_Barley_Angers_2003_N0","SC_Barley_Angers_2003_N1")
# usms = c("SC_Barley_Angers_2003_N0")
# usms = "SC_Wheat_2005-2006_N1"
# usms = "IC_PeaBarley_Angers_2003_N0_D50-50"
# usms = "IC_Wheat_Pea_2005-2006_N0"
# usms = "SC_Pea_Angers_2003_N1"
# usms = "SC_Barley_Angers_2003_N0"

# Get the observations

obs = get_obs(
  workspace = workspace_pea_barley,
  usm = usms,
  usms_file = file.path(workspace_pea_barley,"usms.xml")
)

SticsRFiles::gen_varmod(workspace_pea_barley, sim_variables)

# set_param_xml(file.path(workspace_pea_barley,"plant","pea-sebastian_plt.xml"),
#               # param_name = "inngrain2", param_value = 1.00116558239944, overwrite = TRUE)
#               param_name = "stdnofno", param_value = 1000, overwrite = TRUE)
SticsOnR::run_javastics(
  javastics = javastics,
  workspace = workspace_pea_barley,
  # stics_exe = "Stics_IC_v07-01-2022.exe",
  stics_exe = "Stics_IC_v13-05-2022.exe",
  # stics_exe = "D:/OneDrive - cirad.fr/Travail_Postdoc/STICS/Stics_v850_r1528_branche_Intercrop/stics/Debug/Stics.exe",
  usms_list = usms
)

sim_regular = get_sim(workspace = workspace_pea_barley,
                      usm = usms,
                      usms_file = file.path(workspace_pea_barley,"usms.xml"))

plot(regular = sim_regular, obs = obs)

SticsOnR::run_javastics(javastics_path = javastics, workspace_path = workspace_pea_barley,
                        # stics_exe = "Stics_IC_v07-01-2022.exe",
                        stics_exe = "Stics_IC_v13-05-2022.exe",
                        # stics_exe = "D:/OneDrive - cirad.fr/Travail_Postdoc/STICS/Stics_v850_r1528_branche_Intercrop/stics/Debug/Stics.exe",
                        usms_list = usms)

sim_new = get_sim(workspace = workspace_pea_barley,
                      usm_name = usms,
                      usms_filepath = file.path(workspace_pea_barley,"usms.xml"))
# names(sim_new) = names(sim_regular)
plot(regular = sim_regular, sim_new = sim_new, obs = obs)

# set_param_xml(file.path(workspace_pea_barley,"plant","pea-sebastian_plt.xml"),
#               # param_name = "inngrain2", param_value = 1.5, overwrite = TRUE)
#               param_name = "stdnofno", param_value = 1500, overwrite = TRUE)
# SticsOnR::run_javastics(javastics_path = javastics, workspace_path = workspace_pea_barley,
#                         # stics_exe = "Stics_IC_v13-05-2022.exe",
#                         stics_exe = "D:/OneDrive - cirad.fr/Travail_Postdoc/STICS/Stics_v850_r1528_branche_Intercrop/stics/Debug/Stics.exe",
#                         usms_list = usms)
# sim_new = get_sim(workspace = workspace_pea_barley,
#                     usm_name = usms,
#                     usms_filepath = file.path(workspace_pea_barley,"usms.xml"))
# names(sim_new) = usms
# attr(sim_new, "class") = "cropr_simulation"
# #
# plot(regular = sim_regular, sim_new = sim_new, obs = obs)




# SticsOnR::run_javastics(javastics_path = javastics, workspace_path = workspace_pea_barley,
#                         stics_exe = "D:/OneDrive - cirad.fr/Travail_Postdoc/STICS/Stics_v850_r1528_branche_Intercrop/stics/Debug/Stics.exe",
#                         usms_list = usms)
# sim_strip_new = get_sim(workspace = workspace_pea_barley,
#                         usm_name = usms,
#                         usms_filepath = file.path(workspace_pea_barley,"usms.xml"))
# names(sim_strip_new) = names(sim_regular) = names(sim_strip) = usms
# attr(sim_strip_new, "class") = attr(sim_regular, "class") = attr(sim_strip, "class") = "cropr_simulation"


# Make the plots:
# plots = plot(regular = sim_regular, strip = sim_strip, strip_new = sim_strip_new, obs = obs)
# plots = plot(regular = sim_regular, strip_new = sim_strip_new, obs = obs)
# plots$`1Tprecoce2Stardif2012`
