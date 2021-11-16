# Purpose: automatically optimize the main parameter values of the model for 
# contrasted situations.
# Date: 12/11/2021
# Author: R. Vezy

# devtools::install_github("SticsRPacks/SticsRPacks")
library(SticsRPacks)
library(foreach)
library(doParallel)
library(dplyr)
# First step: copy all usms into a new folder were we will change the parameter 
# values (and keep the original files).
# The original folder is 0-data/usms, the destination folder is 
# 0-data/usms-optimized 

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

plant_files = normalizePath(list.files(file.path("0-data/usms-optimized",names(workspace_usms), "plant"), full.names = TRUE))
plant_files_beer = normalizePath(list.files(file.path("0-data/usms-optim-beer",names(workspace_usms), "plant"), full.names = TRUE))

# Activate Radiative transfer, and put the optimized extin value from beer optim:
for(i in 1:length(plant_files)){
  extin = unlist(get_param_xml(
    xml_file = file.path("0-data/usms-optim-beer", names(workspace_usms)[i], basename(plant_files[i])),
    param_name = "extin"
  ))
  
  set_param_xml(
    xml_file = plant_files[i],
    param_name = "extin",
    param_value = extin,
    overwrite = TRUE
  )
  
  set_param_xml(
    xml_file = plant_files[i],
    param_name = "codetransrad",
    param_value = 2,
    overwrite = TRUE
  )
}

parameters_vars = list()
for(i in 1:nrow(df_optim)){
  parameters_vars[[i]] = list(
    params = trimws(unlist(strsplit(df_optim$Parameter[i], ","))),
    params_lb = as.numeric(trimws(unlist(strsplit(df_optim$low_boundary[i], ",")))),
    params_ub = as.numeric(trimws(unlist(strsplit(df_optim$high_boundary[i], ",")))),
    vars = trimws(unlist(strsplit(df_optim$Variable[i], ","))),
    process = df_optim$Process[i]
  )
}

param_values = list()

# setup parallel backend to use many processors
# cl = makeCluster(length(workspace_usms)+1) #not to overload your computer
# registerDoParallel(cl)
# 
# param_values = foreach(i = 1:length(workspace_usms), .packages = c("SticsRPacks")) %dopar% {

# This step is done twice to re-optimize some of the first variables based on the  
# values of the last parameters (some have interactions)
for(i in 1:length(workspace_usms)){
  param_workspace_vals = c()
  
  for (j in 1:length(parameters_vars)){
    
    javastics_workspace_path = normalizePath(file.path("0-data/usms-optimized",names(workspace_usms)[i]), winslash = "/")
    stics_inputs_path = file.path(javastics_workspace_path,paste(parameters_vars[[j]]$params, collapse = "_"))
    usms = workspace_usms[[i]]
    
    var_name = parameters_vars[[j]]$vars
    obs_list = get_obs(javastics_workspace_path, usm_name = usms)
    obs_list = filter_obs(obs_list, var_names= var_name, include=TRUE)
    if(ncol(obs_list[[1]]) < length(var_name) + 2 ){
      warning("Skipping optimisation of [", paste(parameters_vars[[j]]$params, collapse = ", "),
              "] for workspace ", usms, ". No obs found for [",
              paste(var_name, collapse = ", "), "].")
      next
    }
    
    dir.create(stics_inputs_path, showWarnings = FALSE)
    
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
        param_info = param_info,
        transform_sim = if(parameters_vars[[j]]$process == "Phenology"){
          function(model_results,obs_list,param_values,model_options){
            res =
              lapply(model_results$sim_list, function(x){
                mutate(x,across(where(is.numeric), ~ tail(unique(.x),1)))
              })
            list(error = model_results$error, sim_list = res)
          }
        }else if(parameters_vars[[j]]$process == "Yield"){
          function(model_results,obs_list,param_values,model_options){
            res =
              mapply(function(x,y){
                # dates_obs = filter(x, !is.na(mafruit))$Date
                # filter(y, Date %in% dates_obs)
                sim_yield = filter(y, .data$mafruit == max(.data$mafruit))
                sim_yield = tail(sim_yield,1)
                sim_yield$Date = tail(filter(x, !is.na(.data$mafruit))$Date, 1)
                sim_yield
              }, obs_list, model_results$sim_list, SIMPLIFY = FALSE)
            
            list(error = model_results$error, sim_list = res)
          }
        }else{
          NULL
        },
        transform_obs = if(parameters_vars[[j]]$process == "Phenology"){
          function(model_results,obs_list,param_values,model_options){
            lapply(obs_list, function(x){
              dplyr::mutate(x,across(where(is.numeric), ~ tail(sort(.x, na.last = FALSE),1)))
            })
          }
        }else if(parameters_vars[[j]]$process == "Yield"){
          function(model_results,obs_list,param_values,model_options){
            lapply(obs_list, function(x){
              tail(filter(x, !is.na(mafruit)), 1)
            })
          }
        }else{
          NULL
        }
      )
    # NB: the functions given to transform_sim and transform_obs helps us use
    # one value only for the phenology stages. This is applied to be independent
    # of the day it arrives, and only consider the day as the value
    
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
  # param_workspace_vals # Make it parallel (comment line above too)
}
# stopCluster(cl)

names(param_values) = names(workspace_usms)

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

df$is_optimized = df$original_value != df$optimized_value

write.csv(df, "2-outputs/optimization/optim_results.csv", row.names = FALSE)

# Activate Radiative transfer:
lapply(plant_file_optim, function(x){
  set_param_xml(
    xml_file = x,
    param_name = "codetransrad",
    param_value = 2,
    overwrite = TRUE
  )
})

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


