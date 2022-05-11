# Purpose: manually optimize the main parameter values for which the automatic
# optimization did not work properly. This is done only on parameters of the last
# steps of the optimization, meaning the ones not correlated to others. This is
# just some tweaking because I am a little bit perfectionist.
# Date: 12/11/2021
# Author: R. Vezy

library(SticsRPacks)

javastics_path = normalizePath("0-javastics", winslash = "/")
workspace_usms =
  list(
    "Angers-SC-Barley" = "SC_Barley_Angers_2003_N0", # replace by N1?
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

i = 2
javastics_workspace_path = normalizePath(file.path("0-data/usms-optim-radiative",names(workspace_usms)[i]), winslash = "/")
stics_inputs_path = file.path(javastics_workspace_path, "manual_optimization")
usms = workspace_usms[[i]]

# var_name = "lai_n"
var_name = c("lai_n", "masec_n", "mafruit", "CNgrain", "QNplante")
# var_name = c("vitircarbT", "lai_n")
# var_name = c("mafruit")
obs_list = get_obs(javastics_workspace_path, usm = usms)
obs_list = filter_obs(obs_list, var= var_name, include=TRUE)

dir.create(stics_inputs_path, showWarnings = FALSE)

gen_usms_xml2txt(
  javastics = javastics_path,
  workspace  = javastics_workspace_path,
  out_dir = stics_inputs_path,
  usm = usms,
  verbose = TRUE
)

model_options =
  stics_wrapper_options(
    javastics = javastics_path,
    workspace = stics_inputs_path,
    parallel = FALSE, # Because we have only one usm per workspace so no need
    stics_exe = "Stics_IC_v13-01-2022.exe"
  )

get_param_txt(file.path(stics_inputs_path,usms), "cgrain") # 5e-04

res_orig = stics_wrapper(
  model_options = model_options,
  situation = usms,
  var = var_name,
)

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
    # "nbgrmin" = 1540, "cgrain" = 0.138, "croirac" = 0.2,
    # "nbgrmin" = 2000,
    "Vmax2" = 0.0039, "inngrain2" = 1.3,
    "vitircarbT"= 0.00082
    # , "innsen" = 0.2
  ),
  situation = usms,
  var = var_name,
)

plot(orig = res_orig$sim_list, optim = res_opti$sim_list, obs = obs_list)

res_orig$sim_list$`SC_Pea_2012-2013_N1`$imats
res_opti$sim_list$`SC_Pea_2012-2013_N1`$imats
obs_list$`SC_Pea_2012-2013_N1`$imats
# fixmaxveg, fixmaxgr

# c("vitircarbT" = 0.000365), for sunflower
# c("fixmaxveg" = 32.5) for pea Angers
# c("dlaimaxbrut" = 0.00031, "stdrpmat" = 731,"vitircarbT" = 0.00095) for Pea Auzeville 2012

workspace_IC = list("Auzeville-IC-2012" = "IC_Wheat_Pea_2012-2013_N1")
var_name2 = c("lai_n", "masec_n", "mafruit", "QNplante", "hauteur", "fapar", "demande")

# res_beer = run_simulation(
#   workspaces = normalizePath(file.path("0-data/usms-optim-beer",names(workspace_IC)), winslash = "/"),
#   variables = var_name2,
#   javastics = javastics_path,
#   usms = workspace_IC
# )

res_beer = run_simulation(
  workspaces = normalizePath(file.path("0-data/usms-optim-radiative",names(workspace_IC)), winslash = "/"),
  variables = var_name2,
  javastics = javastics_path,
  usms = workspace_IC
)

res_rad = run_simulation(
  workspaces = normalizePath(file.path("0-data/usms-optim-radiative",names(workspace_IC)), winslash = "/"),
  variables = var_name2,
  javastics = javastics_path,
  usms = workspace_IC
)

res_rad_new = run_simulation(
  workspaces = normalizePath(file.path("0-data/usms-optim-radiative",names(workspace_IC)), winslash = "/"),
  variables = var_name2,
  javastics = javastics_path,
  usms = workspace_IC
)


plot(
  # beer = res_beer$`Auzeville-IC-2012`$sim, 
  rad = res_rad$`Auzeville-IC-2012`$sim, 
  rad_new = res_rad_new$`Auzeville-IC-2012`$sim, 
  obs = res_opti_beer$`Auzeville-IC-2012`$obs, 
  type = "dynamic", verbose = FALSE,
  var = SticsRFiles:::var_to_col_names(var_name2)
)




