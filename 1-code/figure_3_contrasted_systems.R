# Purpose: simulate contrasted intercrop systems to show the vaildity domain of 
# the model.
# Date: 27/10/2021

# Install the packages (to do only once) ----------------------------------

# remotes::install_github("SticsRPacks/SticsRPacks")

# Import the packages -----------------------------------------------------

library(SticsRPacks)
library(tidyverse)
source("1-code/functions.R")

# Define javaStics installation -------------------------------------------

# STICS version -----------------------------------------------------------

javastics = normalizePath("0-javastics", winslash = "/")

# Define the workspaces ---------------------------------------------------

workspace_usms = 
  list(
    "Angers-IC-Pea_Barley" = "IC_PeaBarley_Angers_2003_N0_D50-50", # replace by N1?
    "Auzeville-IC" = "IC_Wheat_Pea_2005-2006_N0",
    "1Tprecoce2Stardif2012" = "1Tprecoce2Stardif2012",
    "Auzeville_wfb-Fababean-Wheat-IC" = "Fababean_Wheat_IC_2011"
  )

worskpaces_paths = file.path("0-data/usms-optimized", names(workspace_usms)) 

# SticsRFiles::get_usms_list("0-data/usms/Auzeville_wfb-Fababean-Wheat-IC/usms.xml")

# Define the variables to simulate ----------------------------------------

sim_variables = c("hauteur","lai(n)","masec(n)","QNplante","mafruit","Qfix",
                  # "ilevs", 
                  "iflos", 
                  "imats"
                  # "iamfs",
                  # "ilaxs"
                  )

# SticsRFiles::get_var_info("Qfix")

# Run the simulations -----------------------------------------------------

# usms = SticsRFiles::get_usms_list(usm_path = file.path(workspace_wheat,"usms.xml"))

mapply(
  function(x,y){
    SticsRFiles::gen_varmod(x, sim_variables)
    SticsOnR::run_javastics(javastics_path = javastics, 
                            workspace_path = x,
                            stics_exe = "Stics_IC_v18-10-2021.exe",
                            usms_list = y)
  },
  worskpaces_paths, 
  workspace_usms
)

sim = mapply(function(x,y){
  get_sim(workspace = x, 
          usm_name = y, 
          usms_filepath = file.path(x, "usms.xml"))
},worskpaces_paths, workspace_usms)

# Add NDFA to sim + put all stages at same value along the whole crop for plotting:
sim = lapply(sim, function(x){
  x = 
    x%>%
    group_by(Plant)%>%
    mutate(
      iflos = unique(as.integer(iflos))[2],
      # iamfs = unique(as.integer(iamfs))[2],
      # ilevs = unique(as.integer(ilevs))[2],
      imats = unique(as.integer(imats))[2]
      # ilaxs = unique(as.integer(ilaxs))[2]
      )
  
  if(!is.null(x$Qfix)){
    return(x%>%mutate(NDFA = Qfix / QNplante))
  }else{
    x  
  }
})

names(sim) = unlist(workspace_usms)
attr(sim, "class") = "cropr_simulation"

# Get the observations

obs = mapply(function(x,y){
  get_obs(workspace = x, usm_name = y, usms_filepath = file.path(x, "usms.xml"))
},worskpaces_paths,workspace_usms)
names(obs) = unlist(workspace_usms)

# Add NDFA to obs:
obs = lapply(obs, function(x){
  if(!is.null(x$Qfix)){
    return(x%>%mutate(NDFA = Qfix / QNplante))
  }else{
    x  
  }
})

 

# Make the plots:
plots = plot(sim,obs=obs, type = "scatter")

write.csv(
  summary(sim, obs = obs, stat = c("R2", "EF", "RMSE", "nRMSE", "Bias")), 
  "2-outputs/stats/constrasted_systems.csv"
)



# df_ic = 
plots$all_situations$data%>%
  mutate(
    # Dominance = ifelse(Dominance == "Principal", "Prin.", "Asso."),
    Plant = recode(Plant,
                   "poi" = "Pea", 
                   "ble" = "Wheat",
                   "soj" = "Soybean",
                   "faba" = "Fababean",
                   "tou" = "Sunflower",
                   "esc" = "Barley"
                   ),
  variable = recode(variable,
                    "lai_n" = "LAI~(m2~m^{-2})",
                    "masec_n" = "Agb~(t~ha^{-1})",
                    "mafruit" = "Gr.~yield~(t~ha^{-1})",
                    "Qfix" = "N~Fix.~(kg~ha^{-1})",
                    "QNplante" = "N~acc.~(kg~ha^{-1})",
                    "imats" = "Matur.~(julian~day)",
                    "iflos" = "Flowe.~(julian~day)",
                    "iamfs" = "Juven.~(julian~day)",
                    "ilaxs" = "Max.L.~(julian~day)",
                    "ilevs" = "Emerg.~(julian~day)",
                    "hauteur" = "Height~(m)",
                    "NDFA" = "NDFA~('%')"
                    )
  )%>%
  ggplot(aes(x = Observed, color = Plant, fill = Plant, shape = Dominance))+
  # geom_point(aes(y = Simulated), size = 1.5, shape = 21, stroke = 1.5)+
  geom_point(aes(y = Simulated), size = 1.5, fill = "transparent", stroke = 1.5)+
  geom_point(aes(y = Simulated, fill = Plant), size = 1.5, alpha = 0.5, stroke = 1)+
  geom_point(aes(y = Observed), color = "transparent", fill = "transparent")+ # Just to get y=x scales
  facet_wrap(variable~., 
             scales = "free", 
             labeller = label_parsed, 
             shrink = TRUE)+
  # labs(colour = "Cropping system:")+
  theme_minimal()+
  # labs(y = NULL)+
  geom_abline()+
  scale_shape_manual(values = c("Principal" = 21, "Associated" = 24))+
  # scale_colour_manual(values= c("Intercrop" = "#6EC0C0", "Sole crop" = "#746EC2"))+
  # scale_fill_manual(values= c("Intercrop" = "#6EC0C04C", "Sole crop" = "#746EC24C")) +
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  # scale_color_brewer(palette = "RdBu")+
  # scale_fill_brewer(palette = "RdBu")+
  theme(
    legend.direction = "horizontal",
    legend.position = 'bottom',
    strip.text.y = element_text(size = 8)
  )

ggsave(filename = "contrasted_systems.png", path = "2-outputs/plots",
       width = 16, height = 16.5, units = "cm")




