# Purpose: simulate two crops in sole and intercrop to show we grasp the main
# interactions at play in the system.
# Date: 12/04/2021

# Install the packages (to do only once) ----------------------------------

# remotes::install_github("SticsRPacks/SticsRPacks")

# Import the packages -----------------------------------------------------

library(SticsRPacks)
library(tidyverse)
source("1-code/functions.R")
Sys.setlocale("LC_TIME", "C") # To get the dates in english

# Define javaStics installation -------------------------------------------

# STICS version -----------------------------------------------------------

javastics = normalizePath("0-javastics", winslash = "/")

# Define the workspaces ---------------------------------------------------

workspace_wheat = "0-data/usms-optim-radiative/Auzeville-Wheat-SC"
workspace_pea = "0-data/usms-optim-radiative/Auzeville-Pea-SC"
workspace_wheat_pea = "0-data/usms-optim-radiative/Auzeville-IC"
workspaces = list(wheat = workspace_wheat, pea = workspace_pea, wheat_pea = workspace_wheat_pea)

# Define the variables to simulate ----------------------------------------

# sim_variables = c("lai(n)","masec(n)","QNplante","mafruit","Qfix","profexteau","profextN","hauteur")
sim_variables = c("lai(n)","masec(n)","QNplante","mafruit","Qfix")

# SticsRFiles::get_var_info("Qfix")

# Run the simulations -----------------------------------------------------

# usms = SticsRFiles::get_usms_list(usm_path = file.path(workspace_wheat,"usms.xml"))


usms_wheat = "SC_Wheat_2005-2006_N0"
usms_pea = "SC_Pea_2005-2006_N0"
usms_wheat_pea = "IC_Wheat_Pea_2005-2006_N0"
usms = list(wheat = usms_wheat, pea = usms_pea, wheat_pea = usms_wheat_pea)

lapply(workspaces, function(x) SticsRFiles::gen_varmod(x, sim_variables))


mapply(function(x,y){
  SticsOnR::run_javastics(javastics_path = javastics, workspace_path = x,
                          stics_exe = "Stics_IC_v13-01-2022.exe",
                          usms_list = y)
},workspaces,usms)

sim = mapply(function(x,y){
  get_sim(workspace = x, usm_name = y, usms_filepath = file.path(x, "usms.xml"))
},workspaces,usms)

# Add NDFA to sim:
sim = lapply(sim, function(x){
  if(!is.null(x$Qfix)){
    return(x%>%mutate(NDFA = Qfix / QNplante))
  }else{
    x
  }
})

names(sim) = usms
attr(sim, "class") = "cropr_simulation"

# Get the observations

obs = mapply(function(x,y){
  get_obs(workspace = x, usm_name = y, usms_filepath = file.path(x, "usms.xml"))
},workspaces,usms)
names(obs) = usms

# Add NDFA to obs:
obs = lapply(obs, function(x){
  if(!is.null(x$Qfix)){
    return(x%>%mutate(NDFA = Qfix / QNplante))
  }else{
    x
  }
})

# Make the plots:
plots = plot(sim, obs = obs)

# Computing per ha for both sole crops and intercrops:

# Which variables need to be summed for plot scale results (instead of averaged)
to_sum = c("lai_n","masec_n","abso_n","mafruit","QNplante","demande","dltams_n",
           "msrac_n","cumraint","raint","dltamsen","deltai_n","laisen_n","rlj","Qfix")

IC_sum =
  plots$`IC_Wheat_Pea_2005-2006_N0`$data%>%
  group_by(Date,variable,Plant)%>%
  summarise(Simulated = ifelse(variable %in% to_sum,
                               Simulated * 2, # To give a full 1ha surface of production
                               Simulated),
            Observed = ifelse(variable %in% to_sum,
                              Observed * 2,
                              Observed)
            )%>%
  mutate(System = "Intercrop", Plant = ifelse(Plant == "poi", "Pea", "Wheat"))

SC_sum =
  dplyr::bind_rows(plots$`SC_Wheat_2005-2006_N0`$data%>%mutate(Plant = "Wheat"),
                   plots$`SC_Pea_2005-2006_N0`$data%>%mutate(Plant = "Pea"))%>%
  mutate(System = "Sole crop")

bind_rows(IC_sum,SC_sum)%>%
  mutate(variable = recode(variable,"lai_n" = "LAI~(m2~m^{-2})",
                           "masec_n" = "Agb~(t~ha^{-1})",
                           "fapar" = "FaPAR~('%')",
                           "mafruit" = "Gr.~yield~(t~ha^{-1})",
                           "Qfix" = "N~Fix.~(kg~ha^{-1})",
                           "QNplante"= "N~cont.~(kg~ha^{-1})",
                           "NDFA" = "NDFA~('%')"
                           )
         )%>%
  ggplot(aes(x = Date, color = System, fill=System))+
  geom_line(aes(y = Simulated), lwd = 1.3)+
  geom_point(aes(y = Observed), show.legend = FALSE, size = 1.5, shape = 21, stroke = 1.5)+
  # geom_point(aes(y = Observed), show.legend = FALSE, size = 2, shape = 21)+
  facet_grid(scales = "free_y", cols = vars(Plant), rows = vars(variable), labeller = label_parsed)+
  labs(colour = "Cropping system:")+
  theme_minimal()+
  labs(y = NULL)+
  scale_colour_manual(values= c("Intercrop" = "#6EC0C0", "Sole crop" = "#746EC2"))+
  scale_fill_manual(values= c("Intercrop" = "#6EC0C04C", "Sole crop" = "#746EC24C")) +
  theme(legend.direction = "horizontal",
        legend.position = 'bottom',
        strip.text.y = element_text(size = 8))
  # scale_colour_manual(values= c("Intercrop" = "#6EC0C0", "Sole crop" = "#746EC2"))+
  # scale_fill_manual(values= c("Intercrop" = "#6EC0C080", "Sole crop" = "#746EC280"))

ggsave(filename = "sole_vs_intercrop.png", path = "2-outputs/plots",
       width = 16, height = 16.5, units = "cm")
