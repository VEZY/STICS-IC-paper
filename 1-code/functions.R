
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
      SticsOnR::run_javastics(javastics_path = javastics, workspace_path = i, 
                              stics_exe = "Stics_IC_v18-10-2021.exe",
                              usms_list = usms_i)
      # Get the results
      sim = get_sim(workspace = i, usm_name = usms_i, usms_filepath = file.path(i,"usms.xml"))
      
      # Get the observations
      obs = get_obs(workspace =  i, usm_name = usms_i, usms_filepath = file.path(i,"usms.xml"))
      
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
                stics_exe = "Stics_IC_v18-10-2021.exe",
                usms_list = usms)
  
  
  # Get the results ---------------------------------------------------------
  
  sim= get_sim(workspace = workspace, usm_name = usms)
  obs= get_obs(workspace =  workspace, usm_name = usms, usms_filepath = file.path(workspace,"usms.xml"))
  
  x0 = get_param_xml(file.path(workspace,"param_newform.xml"),"haut_dev_x01")[[1]][[1]]
  k = get_param_xml(file.path(workspace,"param_newform.xml"),"haut_dev_k1")[[1]][[1]]
  plant_file = list.files(file.path(workspace,"plant"), full.names = TRUE)
  hautmax_ref = get_param_xml(xml_file = plant_file, param_name = "hautmax")[[1]][[1]]
  
  obs_merged = dplyr::bind_rows(obs, .id = "situation")
  
  sim_merged = CroPlotR::bind_rows_sim(sim)
  
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


