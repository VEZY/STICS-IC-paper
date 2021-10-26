
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
                              stics_exe = "Stics_IC_v29-03-2021.exe",
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

