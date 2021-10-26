
library(tidyverse)
library(SticsRPacks)
source("functions.R")

# STICS version -----------------------------------------------------------

#Filepath for JavaSTICS version 8.5
javastics= normalizePath("0-javastics", winslash = "/")

workspace = normalizePath("0-data/usms/Angers-SC-Pea", winslash =  "/")

# Run the simulation ------------------------------------------------------

# Simulating the intercrop: 

usms= SticsRFiles::get_usms_list(usm_path = file.path(workspace,"usms.xml"))

# USMs we want to simulate: 
usms_to_sim= usms # all usms here

# Choose the variables we want in output: 
sim_variables= c("hauteur","somupvtsem","masec(n)","lai(n)")
# NB, use get_var_info() to get the variables by names or keywords if you don't remember, e.g.: get_var_info('laisen')
# get_var_info("lai")

# Generate the var.mod file:
SticsRFiles::gen_varmod(workspace, sim_variables)

run_javastics(javastics_path = javastics, workspace_path = workspace, 
              stics_exe = "Stics_IC_v29-03-2021.exe",
              usms_list = usms_to_sim)


# Get the results ---------------------------------------------------------

sim= get_daily_results(workspace = workspace, usm_name = usms_to_sim)

obs= get_obs(workspace =  workspace, usm_name = usms_to_sim, usms_filename = "usms.xml")


# use last version of the model
# B1_backup = get_param_xml(file.path(workspace,"param_newform.xml"),"haut_dev_B1")
# A1_backup = get_param_xml(file.path(workspace,"param_newform.xml"),"haut_dev_A1")
# C1_backup = get_param_xml(file.path(workspace,"param_newform.xml"),"haut_dev_C1")

# Get the observations ----------------------------------------------------

# set_param_xml(file.path(workspace,"param_newform.xml"),"haut_dev_B1", 865, overwrite = TRUE)
# set_param_xml(file.path(workspace,"param_newform.xml"),"haut_dev_B1", 854.2, overwrite = TRUE)
# set_param_xml(file.path(workspace,"param_newform.xml"),"haut_dev_A1", 0.8944, overwrite = TRUE)
# set_param_xml(file.path(workspace,"param_newform.xml"),"haut_dev_C1", 0.005801, overwrite = TRUE)
# <param format="real" max="2.0" min="0.0" nom="haut_dev_A1">0.85</param>
#   <param format="real" max="5000.0" min="0.0" nom="haut_dev_B1">796</param>
#   <param format="real" max="0.05" min="0.0" nom="haut_dev_C1">0.00691</param>
# run_javastics(javastics_path = javastics, workspace_path = workspace, 
#               stics_exe = "Stics_IC_v29-03-2021.exe",
#               usms_list = usms_to_sim)
# set_param_xml(file.path(workspace,"param_newform.xml"),"haut_dev_B1", B1_backup, overwrite = TRUE)
# set_param_xml(file.path(workspace,"param_newform.xml"),"haut_dev_A1", A1_backup, overwrite = TRUE)
# set_param_xml(file.path(workspace,"param_newform.xml"),"haut_dev_C1", C1_backup, overwrite = TRUE)

# sim_new = get_daily_results(workspace = workspace, usm_name = usms_to_sim)

# Plotting ----------------------------------------------------------------

# Plot in the plot panel in RStudio
# plot(sim=sim,sim_new=sim_new,obs=obs,type="scatter",verbose=FALSE)
# # plot(sim=sim,sim_new=sim_new,obs=obs,type="dynamic",verbose=FALSE)
# summary(sim=sim,sim_new=sim_new,obs=obs, stat=c("R2","nRMSE"))


obs_merged = dplyr::bind_rows(obs, .id = "usm")

sim_merged = sim_new
class(sim_merged) = "list"
sim_merged = dplyr::bind_rows(sim_merged, .id = "usm")

res_all = left_join(obs_merged,sim_merged, by = c("usm","Date"))

plot(res_all$somupvtsem, res_all$hauteur.x)


P_haut_dev_A = get_param_xml(file.path(workspace,"param_newform.xml"),"haut_dev_A1")[[1]][[1]]
P_haut_dev_C = get_param_xml(file.path(workspace,"param_newform.xml"),"haut_dev_C1")[[1]][[1]]

nls(hauteur.x ~ P_haut_dev_A / (1+exp(-P_haut_dev_C*(somupvtsem-P_haut_dev_B))), data = res_all, 
    start = list(P_haut_dev_B = 1040,P_haut_dev_A = 2.264,P_haut_dev_C = 0.004769))

