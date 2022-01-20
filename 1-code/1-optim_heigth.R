# Purpose: optimize the parameters of crop height outside of STICS (much faster)

library(tidyverse)
library(SticsRPacks)
source("1-code/functions.R")

# STICS version -----------------------------------------------------------

#Filepath for JavaSTICS version 8.5
javastics= normalizePath("0-javastics", winslash = "/")
workspaces = "0-data/usms"
  
workspace_usms = 
  list(
    "Angers-SC-Barley" = "SC_Barley_Angers_2003_N0", # replace by N1?
    "Angers-SC-Pea" = "SC_Pea_Angers_2003_N0",
    "Auzeville-Pea-SC" = "SC_Pea_2005-2006_N0",
    "Auzeville-Wheat-SC" = "SC_Wheat_2005-2006_N0",
    "sojaTardif2012-SC" = "SojaTardif-SC2012",
    "tourPrecoce2012-SC" = "TournPrecoce-SC2012",
    "Auzeville_wfb-Fababean-SC" = "Fababean_SC_2007",
    "Auzeville_wfb-Wheat-SC" = "Wheat_SC_2007"
  )

# Run the optimization ------------------------------------------------------

mapply(function(x,y){
  optim_height(workspace = normalizePath(file.path(workspaces,x), winslash =  "/"),
               usms = y)
}, names(workspace_usms), workspace_usms)


# Update the IC files too -------------------------------------------------

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
      plant_optimized = list.files(file.path(workspaces, z, "plant"), full.names = TRUE)
      ic_plant = file.path(workspaces, x, "plant", basename(plant_optimized))
      file.copy(from = plant_optimized,
                to = ic_plant,
                overwrite = TRUE)
      
      # Update haut_dev_x01 and haut_dev_x02 in param_newform:
      param_newform_optim = file.path(workspaces, z, "param_newform.xml")
      param_newform_ic = file.path(workspaces, x, "param_newform.xml")
      
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


