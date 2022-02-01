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
sim_variables = c("lai(n)","masec(n)","QNplante","mafruit","Qfix", "inns", "fapar")
# SticsRFiles::get_var_info("inn")

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
    x = x%>%mutate(NDFA = Qfix / QNplante)
  }
  
  if(!is.null(x$inns)){
    x = x%>%select(-inns)
  }
  return(x)
})

# Make the plots:
plots = plot(sim, obs = obs)

# Computing per ha for both sole crops and intercrops:

# Which variables need to be summed for plot scale results (instead of averaged)
to_sum_obs = c("lai_n","masec_n","mafruit","QNplante", "Qfix")
to_sum_sim = c(to_sum_obs, "fapar")


IC_sum =
  plots$`IC_Wheat_Pea_2005-2006_N0`$data%>%
  group_by(Date,variable,Plant)%>%
  summarise(Simulated = ifelse(variable %in% to_sum_sim,
                               Simulated * 2, # To give a full 1ha surface of production
                               Simulated),
            Observed = ifelse(variable %in% to_sum_obs,
                              Observed * 2,
                              Observed)
  )%>%
  mutate(System = "Intercrop", Plant = ifelse(Plant == "poi", "bold(Pea)", "bold(Wheat)"))

SC_sum =
  dplyr::bind_rows(plots$`SC_Wheat_2005-2006_N0`$data%>%mutate(Plant = "bold(Wheat)"),
                   plots$`SC_Pea_2005-2006_N0`$data%>%mutate(Plant = "bold(Pea)"))%>%
  mutate(System = "Sole crop")

df = bind_rows(IC_sum,SC_sum)%>%filter(variable != "Qfix" && variable != "fapar")

stats = 
  df%>%
  group_by(variable)%>%
  summarize(Plant = unique(Plant), y = max(Simulated))%>%
  group_by(Plant)%>%
  mutate(variable = 
           recode(
             .data$variable,
             "lai_n" = "bold(LAI~(m^{2}~m^{-2}))",
             "masec_n" = "bold(Agb~(t~ha^{-1}))",
             "fapar" = "bold(FaPAR~('%'))",
             "mafruit" = "bold(Gr.~yield~(t~ha^{-1}))",
             "Qfix" = "bold(N~Fix.~(kg~ha^{-1}))",
             "QNplante"= "bold(N~acc.~(kg~ha^{-1}))",
             "NDFA" = "bold(NDFA~('%'))",
             "inns" = "bold(N~stress~eff.)"
           )
  )%>%
  arrange(variable)%>%
  mutate(
    plot_index = order(variable),
    plot_nb = ifelse(
      Plant == "bold(Pea)", 
      paste(plot_index, "a", sep = "."),
      paste(plot_index, "b", sep = ".")
    )
  )

df%>%
  mutate(variable = 
           recode(
             variable,
             "lai_n" = "bold(LAI~(m^{2}~m^{-2}))",
             "masec_n" = "bold(Agb~(t~ha^{-1}))",
             "fapar" = "bold(FaPAR~('%'))",
             "mafruit" = "bold(Gr.~yield~(t~ha^{-1}))",
             "Qfix" = "bold(N~Fix.~(kg~ha^{-1}))",
             "QNplante"= "bold(N~acc.~(kg~ha^{-1}))",
             "NDFA" = "bold(NDFA~('%'))",
             "inns" = "bold(N~stress~eff.)"
           )
  )%>%
  ggplot(aes(x = Date, color = System, fill=System))+
  geom_line(aes(y = Simulated), lwd = 1.3)+
  geom_point(aes(y = Observed), show.legend = FALSE, size = 1.5, shape = 21, stroke = 1.5)+
  facet_grid(
    scales = "free_y", cols = vars(Plant), rows = vars(variable),
    labeller = label_parsed,
    switch = "y"
  )+
  geom_label(
    x = as.POSIXct("2005-09-26 UTC", tz = "UTC"),
    aes(y = y, label = plot_nb),inherit.aes = FALSE,
    data = stats, hjust=0, size = 3.1,
    label.size = NA, fontface = "bold",
    parse = FALSE
  )+
  labs(colour = "Cropping system:")+
  theme_minimal()+
  labs(y = NULL)+
  scale_colour_manual(values= c("Intercrop" = "#6EC0C0", "Sole crop" = "#746EC2"))+
  scale_fill_manual(values= c("Intercrop" = "#6EC0C04C", "Sole crop" = "#746EC24C")) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1)))+ # expand the limits of the plots
  theme(
    strip.text.x = element_text(size = 12, face = "bold.italic"),
    strip.text.y = element_text(size = 8),
    legend.direction = "horizontal",
    legend.position = 'bottom',
    strip.placement.y = "outside"
  )

ggsave(filename = "sole_vs_intercrop.png", path = "2-outputs/plots",
       width = 16, height = 18, units = "cm")

