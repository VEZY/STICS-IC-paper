
# devtools::install_github("SticsRPacks/SticsRPacks")
library(SticsRPacks)
library(foreach)
library(doParallel)
# First step: copy all usms into a new folder were we will change the parameter values (and keep the original files).
# The original folder is 0-data/usms, the destination folder is 0-data/usms-optimized 

# Set-up the optimization process:
df_optim = read.csv("0-data/calibration.csv", sep = ";")
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
# workspace_usms is a list of workspace-name -> usm names

parameters_vars = list()
for(i in 1:nrow(df_optim)){
  parameters_vars[[i]] = list(
    params = trimws(unlist(strsplit(df_optim$Parameter[i], ","))),
    params_lb = as.numeric(trimws(unlist(strsplit(df_optim$low_boundary[i], ",")))),
    params_ub = as.numeric(trimws(unlist(strsplit(df_optim$high_boundary[i], ",")))),
    vars = trimws(unlist(strsplit(df_optim$Variable[i], ",")))
  )
}

param_values = list()


# setup parallel backend to use many processors
# cl = makeCluster(length(workspace_usms)+1) #not to overload your computer
# registerDoParallel(cl)
# 
# param_values = foreach(i = 1:length(workspace_usms), .packages = c("SticsRPacks")) %dopar% {
#   param_workspace_vals = c()
#   
#   for (j in 1:length(parameters_vars)){
#     
#     javastics_workspace_path = normalizePath(file.path("0-data/usms-optimized",names(workspace_usms)[i]), winslash = "/")
#     stics_inputs_path = file.path(javastics_workspace_path,paste(parameters_vars[[j]]$params, collapse = "_"))
#     dir.create(stics_inputs_path, showWarnings = FALSE)
#     usms = workspace_usms[[i]]
#     
#     gen_usms_xml2txt(
#       javastics_path = javastics_path,
#       workspace_path = javastics_workspace_path,
#       target_path = stics_inputs_path,
#       usms_list = usms,
#       verbose = TRUE
#     )
#     
#     # Set the model options (see '? stics_wrapper_options' for details)
#     model_options =
#       stics_wrapper_options(
#         javastics_path = javastics_path,
#         data_dir = stics_inputs_path,
#         parallel=FALSE, # Because we have only one usm per workspace so no need
#         stics_exe = "Stics_IC_v18-10-2021.exe"
#       )
#     
#     var_name = parameters_vars[[j]]$vars
#     obs_list = get_obs(javastics_workspace_path, usm_name = usms)
#     obs_list = filter_obs(obs_list, var_names= var_name, include=TRUE)
#     if(ncol(obs_list[[1]]) < length(var_name) + 2 ){
#       warning("Skipping optimisation of [", paste(parameters_vars[[j]]$params, collapse = ", "),
#               "] for workspace ", workspace_usms[[i]], ". No obs found for [",
#               paste(var_name, collapse = ", "), "].")
#       next
#     }
#     lb = parameters_vars[[j]]$params_lb
#     ub = parameters_vars[[j]]$params_ub
#     names(ub) = names(lb) = parameters_vars[[j]]$params
#     
#     param_info = list(lb = lb, ub = ub)
#     
#     optim_options = list()
#     optim_options$nb_rep = 7
#     optim_options$maxeval = 500 # Maximum number of evaluations of the minimized criteria
#     optim_options$xtol_rel = 1e-03 # Tolerance criterion between two iterations
#     # (threshold for the relative difference of parameter values between the 2 previous
#     # iterations)
#     dir_estim_results = stics_inputs_path
#     optim_options$path_results = dir_estim_results # path where to store the results (graph and Rdata)
#     optim_options$ranseed = 1 # set random seed so that each execution give the same results
#     # If you want randomization, don't set it.
#     
#     res =
#       estim_param(
#         obs_list = obs_list,
#         model_function = stics_wrapper,
#         model_options = model_options,
#         optim_options = optim_options,
#         param_info = param_info
#       )
#     
#     param_workspace_vals = c(param_workspace_vals, res$final_values)
#     
#     plant_file = list.files(file.path(javastics_workspace_path,"plant"), full.names = TRUE)
#     
#     if(length(plant_file) > 1){
#       stop("There must be only one file in the plant folder: ",
#            file.path(javastics_workspace_path,"plant"))
#     }
#     
#     for(params in parameters_vars[[j]]$params){
#       if(params == "haut_dev_x01" | params == "haut_dev_k1"){
#         xml_file = file.path(javastics_workspace_path,"param_newform.xml")
#       }else{
#         xml_file = plant_file
#       }
#       set_param_xml(
#         xml_file = xml_file,
#         param_name = params,
#         param_value = res$final_values[params],
#         overwrite = TRUE
#       )
#     }
#   }
#   param_workspace_vals
# }
# stopCluster(cl)

for(i in 1:length(workspace_usms)){
  param_workspace_vals = c()

  for (j in 1:length(parameters_vars)){

    javastics_workspace_path = normalizePath(file.path("0-data/usms-optimized",names(workspace_usms)[i]), winslash = "/")
    stics_inputs_path = file.path(javastics_workspace_path,paste(parameters_vars[[j]]$params, collapse = "_"))
    dir.create(stics_inputs_path, showWarnings = FALSE)
    usms = workspace_usms[[i]]

    gen_usms_xml2txt(
      javastics_path = javastics_path,
      workspace_path = javastics_workspace_path,
      target_path = stics_inputs_path,
      usms_list = usms,
      verbose = TRUE
    )

    # Set the model options (see '? stics_wrapper_options' for details)
    model_options =
      stics_wrapper_options(
        javastics_path = javastics_path,
        data_dir = stics_inputs_path,
        parallel = FALSE, # Because we have only one usm per workspace so no need
        stics_exe = "Stics_IC_v18-10-2021.exe"
      )

    var_name = parameters_vars[[j]]$vars
    obs_list = get_obs(javastics_workspace_path, usm_name = usms)
    obs_list = filter_obs(obs_list, var_names= var_name, include=TRUE)
    if(ncol(obs_list[[1]]) < length(var_name) + 2 ){
      warning("Skipping optimisation of [", paste(parameters_vars[[j]]$params, collapse = ", "),
              "] for workspace ", workspace_usms[[i]], ". No obs found for [",
              paste(var_name, collapse = ", "), "].")
      next
    }
    lb = parameters_vars[[j]]$params_lb
    ub = parameters_vars[[j]]$params_ub
    names(ub) = names(lb) = parameters_vars[[j]]$params

    param_info = list(lb = lb, ub = ub)

    optim_options = list()
    optim_options$nb_rep = 7
    optim_options$maxeval = 500 # Maximum number of evaluations of the minimized criteria
    optim_options$xtol_rel = 1e-03 # Tolerance criterion between two iterations
    # (threshold for the relative difference of parameter values between the 2 previous
    # iterations)
    dir_estim_results = stics_inputs_path
    optim_options$path_results = dir_estim_results # path where to store the results (graph and Rdata)
    optim_options$ranseed = 1 # set random seed so that each execution give the same results
    # If you want randomization, don't set it.

    res =
      estim_param(
        obs_list = obs_list,
        model_function = stics_wrapper,
        model_options = model_options,
        optim_options = optim_options,
        param_info = param_info
      )

    param_workspace_vals = c(param_workspace_vals, res$final_values)

    plant_file = list.files(file.path(javastics_workspace_path,"plant"), full.names = TRUE)

    if(length(plant_file) > 1){
      stop("There must be only one file in the plant folder: ",
           file.path(javastics_workspace_path,"plant"))
    }

    for(params in parameters_vars[[j]]$params){
      if(params == "haut_dev_x01" | params == "haut_dev_k1"){
        xml_file = file.path(javastics_workspace_path,"param_newform.xml")
      }else{
        xml_file = plant_file
      }
      set_param_xml(
        xml_file = xml_file,
        param_name = params,
        param_value = res$final_values[params],
        overwrite = TRUE
      )
    }
  }
  param_values[[i]] = param_workspace_vals
}

names(param_values) = names(workspace_usms)


# > param_values
# $SC_Barley_Angers_2003_N0
# stdrpmat  haut_dev_x01   haut_dev_k1   dlaimaxbrut       durvieF       laicomp 
# 8.500000e+02  3.581765e+02  1.981527e-02  1.000000e-04  1.000000e+02  1.889335e-01 
# ktrou         Vmax2     inngrain2    innturgmin        innsen         rsmin 
# 7.482475e-01  4.000000e-03  1.598361e+00 -4.758850e-01  6.218518e-01  2.936682e+02 
# efcroijuv     efcroiveg   efcroirepro        cgrain       nbgrmin    vitircarbT 
# 1.000000e+00  4.000000e+00  4.000000e+00  1.000000e-01  3.000000e+03  7.846749e-04 
# vitirazo 
# 2.372658e-02 
# 
# $SC_Pea_Angers_2003_N0
# stamflax      stdrpmat  haut_dev_x01   haut_dev_k1   dlaimaxbrut       durvieF 
# 5.296079e+02  8.500000e+02  3.581765e+02  1.981527e-02  4.570643e-04  1.340502e+02 
# laicomp         ktrou         Vmax2     inngrain2    innturgmin        innsen 
# 1.132736e-01  7.482475e-01  4.000000e-03  1.600000e+00 -7.500000e-01  7.991490e-01 
# rsmin     efcroijuv     efcroiveg   efcroirepro        cgrain       nbgrmin 
# 2.039846e+02  1.923481e+00  2.589805e+00  2.945986e+00  2.010912e-02  1.879790e+03 
# vitircarbT      vitirazo 
# 7.846749e-04  3.149617e-02 
# 
# $`SC_Pea_2005-2006_N0`
# stlevamf     stlevdrp     stamflax     stdrpmat     stdrpdes haut_dev_x01  haut_dev_k1 
# 1.966248e+02 8.438104e+02 4.976288e+02 4.172887e+02 2.971100e+02 3.581765e+02 1.981527e-02 
# dlaimaxbrut      durvieF      laicomp        ktrou        Vmax2    inngrain2   innturgmin 
# 1.000000e-04 1.619985e+02 9.998138e-01 8.197008e-01 4.000000e-03 1.000000e+00 1.000000e+00 
# innsen        rsmin    efcroijuv    efcroiveg  efcroirepro       cgrain      nbgrmin 
# 8.000000e-01 3.000000e+02 1.000000e+00 3.338753e+00 1.489884e+00 2.161569e-02 1.838669e+03 
# vitircarbT     vitirazo 
# 1.566247e-03 3.263875e-02 
# 
# $`SC_Wheat_2005-2006_N0`
# stlevamf     stlevdrp     stamflax     stdrpmat     stdrpdes haut_dev_x01  haut_dev_k1 
# 1.850777e+02 9.297647e+02 6.000000e+02 5.084291e+02 5.484956e+02 3.581765e+02 1.981527e-02 
# dlaimaxbrut      durvieF      laicomp        ktrou        Vmax2    inngrain2   innturgmin 
# 2.914631e-04 1.942329e+02 1.000000e-01 6.932501e-01 4.000000e-03 1.598361e+00 3.619434e-01 
# innsen        rsmin    efcroijuv    efcroiveg  efcroirepro       cgrain      nbgrmin 
# 1.965331e-01 3.000000e+02 4.000000e+00 4.000000e+00 4.000000e+00 4.325825e-02 2.989336e+03 
# vitircarbT     vitirazo 
# 1.029098e-03 2.568530e-02 
# 
# $`SojaTardif-SC2012`
# stamflax      stdrpmat  haut_dev_x01   haut_dev_k1   dlaimaxbrut       durvieF 
# 6.000000e+02  6.616289e+02  3.581765e+02  1.981527e-02  7.219266e-04  1.746239e+02 
# laicomp         ktrou    innturgmin        innsen         rsmin     efcroijuv 
# 9.986592e-01  1.900000e+00 -7.500000e-01  7.021630e-01  2.691988e+02  1.294338e+00 
# efcroiveg   efcroirepro    vitircarbT 
# 3.178508e+00  4.000000e+00  1.165676e-03 
# 
# $`TournPrecoce-SC2012`
# stamflax      stdrpmat  haut_dev_x01   haut_dev_k1   dlaimaxbrut       durvieF 
# 226.306279648 680.816335148 358.176470585   0.019815268   0.001009097 188.832342169 
# laicomp         ktrou    innturgmin        innsen         rsmin     efcroijuv 
# 0.999504728   1.900000000  -0.145770080   0.100000000 248.837453771   3.987890814 
# efcroiveg   efcroirepro    vitircarbT 
# 4.000000000   2.124353112   0.000500000

# > warnings()
# 2: Skipping optimisation of [stlevamf, stlevdrp] for workspace SC_Barley_Angers_2003_N0. No obs found for [iamfs, idrps].
# 4: Skipping optimisation of [stamflax] for workspace SC_Barley_Angers_2003_N0. No obs found for [ilaxs].
# 6: Skipping optimisation of [stdrpdes] for workspace SC_Barley_Angers_2003_N0. No obs found for [irecs].
# 8: Skipping optimisation of [stlevamf, stlevdrp] for workspace SC_Pea_Angers_2003_N0. No obs found for [iamfs, idrps].
# 11: Skipping optimisation of [stdrpdes] for workspace SC_Pea_Angers_2003_N0. No obs found for [irecs].
# 20: Skipping optimisation of [stlevamf, stlevdrp] for workspace SojaTardif-SC2012. No obs found for [iamfs, idrps].
# 23: Skipping optimisation of [stdrpdes] for workspace SojaTardif-SC2012. No obs found for [irecs].
# 25: Skipping optimisation of [Vmax2, inngrain2] for workspace SojaTardif-SC2012. No obs found for [QNplante].
# 27: Skipping optimisation of [cgrain, nbgrmin] for workspace SojaTardif-SC2012. No obs found for [chargefruit].
# 29: Skipping optimisation of [vitirazo] for workspace SojaTardif-SC2012. No obs found for [CNgrain].
# 31: Skipping optimisation of [stlevamf, stlevdrp] for workspace TournPrecoce-SC2012. No obs found for [iamfs, idrps].
# 35: Skipping optimisation of [stdrpdes] for workspace TournPrecoce-SC2012. No obs found for [irecs].
# 37: Skipping optimisation of [Vmax2, inngrain2] for workspace TournPrecoce-SC2012. No obs found for [QNplante].
# 39: Skipping optimisation of [cgrain, nbgrmin] for workspace TournPrecoce-SC2012. No obs found for [chargefruit].
# 41: Skipping optimisation of [vitirazo] for workspace TournPrecoce-SC2012. No obs found for [CNgrain].

# Summarizing the optimization results ------------------------------------

workspaces_orig = normalizePath(file.path("0-data/usms",names(workspace_usms)), winslash = "/")
workspaces_opti = normalizePath(file.path("0-data/usms-optimized",names(workspace_usms)), winslash = "/")

params = unique(unlist(lapply(parameters_vars, function(x) x$params)))
plant_file_orig = list.files(file.path(workspaces_orig,"plant"), full.names = TRUE)
plant_file_optim = list.files(file.path(workspaces_opti,"plant"), full.names = TRUE)
param_orig = get_param_xml(xml_file = plant_file_orig, param_name = params)
param_optim = get_param_xml(xml_file = plant_file_optim, param_name = params)

param_orig2 = get_param_xml(xml_file = file.path(workspaces_orig,"param_newform.xml"), 
                            param_name = params)
param_optim2 = get_param_xml(xml_file = file.path(workspaces_opti,"param_newform.xml"), 
                             param_name = params)
names(param_orig2) = names(param_optim2) = names(param_orig)

df = data.frame(plant = NA, parameter = params, original_value = NA, optimized_value = NA)
df = df[rep(1:nrow(df), length(names(param_orig))),]
df$plant = rep(names(param_orig), each = length(params))
for(i in seq_along(param_orig)){
  plant_i = names(param_orig)[i]
  for(j in seq_along(param_orig[[i]])){
    df[df$parameter == names(param_orig[[i]][j]) & df$plant == plant_i,3] = 
      paste(param_orig[[i]][j], collapse = ", ")
    df[df$parameter == names(param_optim[[i]][j])  & df$plant == plant_i,4] = 
      paste(param_optim[[i]][j], collapse = ", ")
  }
  
  for(j in seq_along(param_orig2[[i]])){
    df[df$parameter == names(param_orig2[[i]][j]) & df$plant == plant_i,3] = 
      paste(param_orig2[[i]][j], collapse = ", ")
    df[df$parameter == names(param_optim2[[i]][j])  & df$plant == plant_i,4] = 
      paste(param_optim2[[i]][j], collapse = ", ")
  }
}

write.csv(df, "2-outputs/optimization/optim_results.csv", row.names = FALSE)
# write.csv(dplyr::arrange(df, parameter), "2-outputs/optimization/optim_results2.csv", row.names = FALSE)

# Comparison before and after ---------------------------------------------

source("1-code/functions.R")

# sim_variables = c("lai(n)","QNplante","Qfix","masec(n)","hauteur","CNgrain","mafruit","chargefruit")
sim_variables = unique(unlist(lapply(parameters_vars, function(x) x$vars)))

# Run the simulations -----------------------------------------------------

res_orig = run_simulation(workspaces = workspaces_orig,
                          variables = sim_variables,
                          javastics = javastics_path,
                          usms = workspace_usms
                          )
res_opti = run_simulation(workspaces = workspaces_opti,
                          variables = sim_variables,
                          javastics = javastics_path,
                          usms = workspace_usms
                          )
# res_orig = import_simulations(workspaces = workspaces_orig, variables = sim_variables)
# res_opti = import_simulations(workspaces = workspaces_opti, variables = sim_variables)

# Make the plots ----------------------------------------------------------

plotting_var = sim_variables

# Plots per workspace:
dynamic_plots = 
  mapply(
    function(x,y){
      plot(orig = x$sim, optim = y$sim, obs = x$obs, type = "dynamic", verbose = FALSE, 
           var = SticsRFiles:::var_to_col_names(plotting_var))
    },
    res_orig,
    res_opti)

dynamic_plots


# Update the xml files for intercrops -------------------------------------

workspace_usms_IC = 
  list(
    "Angers-IC-Pea_Barley" = c(p = "Angers-SC-Pea", a = "Angers-SC-Barley"),
    "Auzeville-IC" = c(p = "Auzeville-Wheat-SC", a = "Auzeville-Pea-SC"),
    "1Tprecoce2Stardif2012" = c(p = "tourPrecoce2012-SC", a = "sojaTardif2012-SC"),
    "Auzeville_wfb-Fababean-Wheat-IC" = c(p = "Auzeville_wfb-Fababean-SC", a = "Auzeville_wfb-Wheat-SC")
  )


mapply(
  function(x,y){
    mapply(function(z, dominance){
      plant_optimized = list.files(file.path("0-data/usms-optimized", z, "plant"), full.names = TRUE)
      ic_plant = file.path("0-data/usms-optimized", x, "plant", basename(plant_optimized))
      file.copy(from = plant_optimized,
                to = ic_plant,
                overwrite = TRUE)
      
      # Update haut_dev_x01 and haut_dev_x02 in param_newform:
      param_newform_optim = file.path("0-data/usms-optimized", z, "param_newform.xml")
      param_newform_ic = file.path("0-data/usms-optimized", x, "param_newform.xml")
      
      set_param_xml(
        param_newform_ic, 
        param_name = ifelse(dominance == "p", "haut_dev_x01", "haut_dev_x02"),
        param_value = unlist(get_param_xml(param_newform_optim, param_name = "haut_dev_x01")),
        overwrite = TRUE
      )
      set_param_xml(
        param_newform_ic, 
        param_name = ifelse(dominance == "p", "haut_dev_k1", "haut_dev_k2"),
        param_value = unlist(get_param_xml(param_newform_optim, param_name = "haut_dev_k1")),
        overwrite = TRUE
      )
    }, y, names(y))
  },
  names(workspace_usms_IC),
  workspace_usms_IC
)
