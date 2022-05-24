
run_simulation = function(workspaces,variables,javastics,usms=NULL){
  res =
    lapply(workspaces, function(i){
      if(!is.null(usms)){
        usms_i = usms[[basename(i)]]
      }else{
        usms_i = SticsRFiles::get_usms_list(usm_path = file.path(i,"usms.xml"))
      }
      # Generate the var.mod file:
      SticsRFiles::gen_varmod(i, variables)
      # Run the simulations:
      SticsOnR::run_javastics(javastics = javastics, workspace = i,
                              stics_exe = "Stics_IC_v24-05-2022.exe",
                              usm = usms_i)
      # Get the results
      sim = get_sim(workspace = i, usm = usms_i, usms_file = file.path(i,"usms.xml"))

      # Get the observations
      obs = get_obs(workspace =  i, usm = usms_i, usms_file = file.path(i,"usms.xml"))

      sim = lapply(sim, rename_plant, "poi" = "pea", "ble" = "wheat","esc" = "barley")
      class(sim) = append(class(sim),"cropr_simulation")
      obs = lapply(obs, rename_plant, "poi" = "pea", "ble" = "wheat","esc" = "barley")
      list(sim = sim, obs = obs)
    })
  names(res) = basename(workspaces)
  res
}


# usms is a list of vector of usms for each workspace, names by the folder name
import_simulations = function(workspaces, variables, usms = NULL){
  res =
    lapply(workspaces, function(i){
      if(!is.null(usms)){
        usms_i = usms[[basename(i)]]
      }else{
        usms_i = SticsRFiles::get_usms_list(usm_path = file.path(i,"usms.xml"))
      }

      # Get the results
      sim = get_sim(workspace = i, usm_name = usms, usms_filepath = file.path(i,"usms.xml"))
      # Get the observations
      obs = get_obs(workspace =  i, usm_name = usms, usms_filepath = file.path(i,"usms.xml"))
      sim = lapply(sim, rename_plant, "poi" = "pea", "ble" = "wheat","esc" = "barley")
      class(sim) = append(class(sim),"cropr_simulation")
      obs = lapply(obs, rename_plant, "poi" = "pea", "ble" = "wheat","esc" = "barley")
      list(sim = sim, obs = obs)
    })
  names(res) = basename(workspaces)
  res
}

rename_plant = function(data,...){
  dot_args = list(...)
  if(!is.null(data$Plant)){
    for (i in seq_along(dot_args)) {
      data$Plant[data$Plant== names(dot_args)[i]] = dot_args[[i]]
    }
  }
  data
}


merge_sim = function(sim) {

  new_sim = vector("list")
  new_obs = vector("list")

  for (i in seq_along(sim)) {
    new_sim = c(new_sim,sim[[i]]$sim)
    new_obs = c(new_obs,sim[[i]]$obs)
  }
  class(new_sim) = append(class(new_sim),"cropr_simulation")

  list(sim = new_sim, obs = new_obs)
}



optim_height = function(workspace, usms){

  # Choose the variables we want in output:
  sim_variables= c("hauteur","somupvtsem","masec(n)","lai(n)")
  # NB, use get_var_info() to get the variables by names or keywords if you don't remember, e.g.: get_var_info('laisen')
  # get_var_info("lai")

  # Generate the var.mod file:
  SticsRFiles::gen_varmod(workspace, sim_variables)

  run_javastics(javastics_path = javastics, workspace_path = workspace,
                stics_exe = "Stics_IC_v24-05-2022.exe",
                usms_list = usms)


  # Get the results ---------------------------------------------------------

  sim= get_sim(workspace = workspace, usm_name = usms)
  obs= get_obs(workspace =  workspace, usm_name = usms, usms_filepath = file.path(workspace,"usms.xml"))

  x0 = get_param_xml(file.path(workspace,"param_newform.xml"),"haut_dev_x01")[[1]][[1]]
  k = get_param_xml(file.path(workspace,"param_newform.xml"),"haut_dev_k1")[[1]][[1]]
  plant_file = list.files(file.path(workspace,"plant"), full.names = TRUE)
  hautmax_ref = get_param_xml(file = plant_file, param = "hautmax")[[1]][[1]]

  obs_merged = dplyr::bind_rows(obs, .id = "situation")

  sim_merged = CroPlotR::bind_rows(sim)

  res_all = left_join(obs_merged,sim_merged%>%select("situation","Date", "somupvtsem"),
                      by = c("situation","Date"))

  df_optim =
    res_all%>%
    select(hauteur, somupvtsem)%>%
    na.omit()

  hautmax = max(df_optim$hauteur)
  optim_res =
    nls(
      hauteur ~ hautmax / (1 + exp(-k_mod * (somupvtsem - x0_mod))),
      data = df_optim,
      start = list(x0_mod = x0, k_mod = k)
    )

  x0_mod = coef(optim_res)[["x0_mod"]]
  k_mod = coef(optim_res)[["k_mod"]]

  set_param_xml(
    file.path(workspace,"param_newform.xml"),
    "haut_dev_x01",
    param_value = x0_mod,
    overwrite = TRUE
  )

  set_param_xml(
    file.path(workspace,"param_newform.xml"),
    "haut_dev_k1",
    param_value = k_mod,
    overwrite = TRUE
  )

  set_param_xml(
    plant_file,
    "hautmax",
    param_value = hautmax,
    overwrite = TRUE
  )

  hauteur_sim = hautmax / (1 + exp(-k_mod * (df_optim$somupvtsem - x0_mod)))
  plot(
    x = df_optim$somupvtsem,
    y = df_optim$hauteur,
    xlab = "somupvtsem",
    ylab = "height (m)",
    main = basename(plant_file),
  )

  lines(df_optim$somupvtsem, hauteur_sim)

  list(hautmax = hautmax, x0 = x0_mod, k = k_mod)
}


optimize_workspace = function(worspaces_path, workspace_usms,parameters_vars,javastics_path,stics_version = "Stics_IC_v24-05-2022.exe"){

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

      javastics_workspace_path = normalizePath(file.path(worspaces_path,names(workspace_usms)[i]), winslash = "/")
      stics_inputs_path = file.path(javastics_workspace_path,paste(parameters_vars[[j]]$params, collapse = "_"))
      usms = workspace_usms[[i]]

      var_name = parameters_vars[[j]]$vars
      obs_list = get_obs(javastics_workspace_path, usm = usms)
      obs_list = filter_obs(obs_list, var= var_name, include=TRUE)

      # Remove the usms with no observations:
      obs_list = lapply(obs_list, function(x){
        if(length(colnames(x)) == 2 && colnames(x) == c("Date","Plant")){
          return()
        }else{
          return(x)
        }
      })
      obs_list = obs_list[unlist(lapply(obs_list, function(x) !is.null(x)))]

      if(length(obs_list) == 0){
        warning("Skipping optimisation of [", paste(parameters_vars[[j]]$params, collapse = ", "),
                "] for workspace ", usms, ". No obs found for [",
                paste(var_name, collapse = ", "), "].")
        next
      }

      dir.create(stics_inputs_path, showWarnings = FALSE)

      gen_usms_xml2txt(
        javastics = javastics_path,
        workspace = javastics_workspace_path,
        out_dir = stics_inputs_path,
        usm = usms,
        verbose = TRUE
      )

      # Set the model options (see '? stics_wrapper_options' for details)
      model_options =
        stics_wrapper_options(
          javastics_path = javastics_path,
          data_dir = stics_inputs_path,
          parallel = FALSE, # Because we have only one usm per workspace so no need
          stics_exe = stics_version
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
          param = params,
          values = res$final_values[params],
          overwrite = TRUE
        )
      }
    }
    param_values[[i]] = param_workspace_vals
    # param_workspace_vals # Make it parallel (comment line above too)
  }
  # stopCluster(cl)

  names(param_values) = names(workspace_usms)
  return(param_values)
}


#' Exctract paramaters from a pre-formated data.frame optimisation file
#'
#'
#' @param df_optim The input data.frame of the same format as 0-data/calibration.csv
#'
#' @return A list of parameters to optimise along with informations (e.g. boundary values)
#' @export
#'
extract_parameters = function(df_optim){
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
  return(parameters_vars)
}


#' Summarize an optimisation process
#'
#' Compare the values of the parameters in two workspaces, and tells if they
#' were optimized or not, and return their values.
#'
#' @param workspaces_orig The workspace of origin
#' @param workspaces_opti The optimized workspace
#' @param parameters_vars A list of parameter names + info (see `extract_parameters`)
#'
#' @return A data.frame with parameter values and wether they were optimized or not
#' @export
#'
summarize_optimization = function(workspaces_orig, workspaces_opti,parameters_vars){

  params = unique(unlist(lapply(parameters_vars, function(x) x$params)))
  plant_file_orig = list.files(file.path(workspaces_orig,"plant"), full.names = TRUE)
  plant_file_optim = list.files(file.path(workspaces_opti,"plant"), full.names = TRUE)
  param_orig = get_param_xml(file = plant_file_orig, param = params)
  param_optim = get_param_xml(file = plant_file_optim, param = params)

  param_orig2 = get_param_xml(file = file.path(workspaces_orig,"param_newform.xml"),
                              param = params)
  param_optim2 = get_param_xml(file = file.path(workspaces_opti,"param_newform.xml"),
                               param = params)
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

  return(df)
}
