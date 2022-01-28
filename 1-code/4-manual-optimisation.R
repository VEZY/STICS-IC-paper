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
# var_name = c("vitircarbT", "lai_n")
var_name = c("mafruit")
obs_list = get_obs(javastics_workspace_path, usm_name = usms)
obs_list = filter_obs(obs_list, var_names= var_name, include=TRUE)

dir.create(stics_inputs_path, showWarnings = FALSE)

gen_usms_xml2txt(
  javastics_path = javastics_path,
  workspace_path = javastics_workspace_path,
  target_path = stics_inputs_path,
  usms_list = usms,
  verbose = TRUE
)

model_options =
  stics_wrapper_options(
    javastics_path = javastics_path,
    data_dir = stics_inputs_path,
    parallel = FALSE, # Because we have only one usm per workspace so no need
    stics_exe = "Stics_IC_v13-01-2022.exe"
  )

get_param_txt(file.path(stics_inputs_path,usms), "stdrpmat") # 5e-04

res_orig = stics_wrapper(
  model_options = model_options,
  sit_names = usms,
  var_names = var_name,
)

res_opti = stics_wrapper(
  model_options = model_options,
  param_values = c("stdrpmat" = 731.3547137,
                   "vitircarbT" = 0.001),
  # param_values = c("fixmaxveg" = 35, "Vmax2" = 0.01),
  sit_names = usms,
  var_names = var_name,
)

plot(orig = res_orig$sim_list, optim = res_opti$sim_list, obs = obs_list)

# fixmaxveg, fixmaxgr

# c("vitircarbT" = 0.000365), for sunflower
# c("fixmaxveg" = 32.5) for pea Angers
