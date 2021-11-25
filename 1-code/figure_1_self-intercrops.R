# Purpose: compare the model outputs for sole crop and sole crop simulated as an
# intercrop (self-intercrop).
# Careful, the simulations are made using Beer's law because in intercrops when 
# two crops have ~same height we compute using Beer's law whatever the parameter.
# Date: 05/11/2020

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

workspaces = list.dirs("0-data/usms-optimized", full.names = TRUE, recursive = FALSE)

workspace = "0-data/usms-optim-beer-backup/Auzeville-Wheat-SC"

# Setting the light interception to 1 so both sole and inter-crop share the same
# computation (self-intercrop will have the same height, so the computation is always
# the Beer's law)
set_param_xml(file.path(workspace,"plant/Wheat_Eric_plt.xml"),
              "codetransrad", 1, overwrite = TRUE)


# Define the variables to simulate ----------------------------------------

sim_variables = c("lai(n)","masec(n)","QNplante","mafruit")
# Investigating lai dev:
# sim_variables = c("lai(n)","deltai(n)","laisen(n)","isens","turfac","innlai")
# sim_variables = c("efdensite_rac","masec(n)","CNplante","abso(n)","rlj","msrac(n)",
#                   "dltams(n)","QNplante","demande")
# Investigating root growth:
# sim_variables = c("efdensite_rac","efdensite_rac","dtj(n)","abso(n)","rlj","msrac(n)",
#                   "dltams(n)","urac","efdensite","densite")
# sim_variables = c("QNplante","cdemande","demande","flusol","flurac",
#                   "CNgrain","abso(n)","CNplante","inn")

# Which variables need to be summed for plot scale results (instead of averaged)
to_sum = c("lai_n","masec_n","abso_n","mafruit","QNplante","demande","dltams_n",
           "msrac_n","cumraint","raint","dltamsen","deltai_n","laisen_n","rlj")


SticsRFiles::get_var_info(keyword = "demande")

# get_param_xml("0-data/usms/Auzeville-Wheat-SC/plant/Wheat_Eric_plt.xml",
#               "codetransrad")

# get_param_info(parameter = "lvfront")

# SticsRFiles::get_var_info(var = "leaf area")
# Run the simulations -----------------------------------------------------

usms = SticsRFiles::get_usms_list(usm_path = file.path(workspace,"usms.xml"))
usms = c("SC_Wheat_2005-2006_N0", "IC_Wheat_Wheat_2005-2006_N0")

SticsRFiles::gen_varmod(workspace, sim_variables)

SticsOnR::run_javastics(javastics_path = javastics, workspace_path = workspace,
                        stics_exe = "Stics_IC_v18-10-2021.exe",
                        usms_list = usms)

# Get the results
sim = get_sim(workspace = workspace, usm_name = usms)

# Get the observations
obs = get_obs(workspace =  workspace, usm_name = usms)

# Setting codetransrad back to radiative transfer: 
set_param_xml("0-data/usms/Auzeville-Wheat-SC/plant/Wheat_Eric_plt.xml",
              "codetransrad", 2, overwrite = TRUE)


plots = plot(sim,obs=obs)

IC_sum = 
  plots$`IC_Wheat_Wheat_2005-2006_N0`$data%>%
  group_by(Date,variable)%>%
  summarise(Simulated = ifelse(variable %in% to_sum,
                               sum(Simulated),
                               mean(Simulated)))%>%
  mutate(System = "Self Intercrop")

# plots$`SC_Wheat_2005-2006_N0` +
#   geom_point(aes(y = Observed), show.legend = FALSE, size = 1.5, shape = 21, stroke = 1.5)+
#   geom_line(aes(color = "Sole crop"), lwd = 1.3) +
#   geom_line(data = IC_sum, lty = 2, aes(color = "Self intercrop"), lwd = 1.3) +
#   ggtitle(NULL) +
#   labs(y = NULL) +
#   theme_minimal()+
#   scale_colour_manual(values= c("Self intercrop" = "#6EC0C0", "Sole crop" = "#746EC2"))+
#   theme(legend.direction = "horizontal", legend.position = 'bottom')


# Plot for the paper: -----------------------------------------------------

plots$`SC_Wheat_2005-2006_N0`$data%>%
  mutate(System = "Sole crop")%>%
  bind_rows(.,IC_sum)%>%
  mutate(variable = recode(.data$variable,
                           "lai_n" = "LAI~(m2~m^{-2})",
                           "masec_n" = "Biomass~(t~ha^{-1})",
                           "mafruit" = "Fruits~(t~ha^{-1})",
                           "QNplante"= "Plant~N~(t~ha^{-1})"))%>%
  ggplot(aes(x = Date, color = System, fill=System))+
  facet_wrap(variable~., scales = "free_y", labeller = label_parsed)+
  geom_point(aes(y = Observed), show.legend = FALSE, size = 1.5, 
             shape = 21, stroke = 1.5, colour = "#F49690", fill = "#F496904C")+
  geom_line(aes(y = Simulated, lty = System), lwd = 1.3) + 
  ggtitle(NULL) +
  labs(y = NULL) +
  theme_minimal()+
  scale_colour_manual(values= c("Self Intercrop" = "#6EC0C0", "Sole crop" = "#746EC2"))+
  theme(legend.direction = "horizontal", legend.position = 'bottom')

ggsave(filename = "self-intercrop.png", path = "2-outputs/plots",
       width = 16, height = 15, units = "cm")

# Same plot but each intercrop is separated and some outputs are x2 to compare
# with sole crop:
IC_2 = 
  plots$`IC_Wheat_Wheat_2005-2006_N0`$data%>%
  group_by(Dominance,Date,variable)%>%
  summarise(Simulated = ifelse(variable %in% to_sum,
                               Simulated*2,Simulated))

plots$`SC_Wheat_2005-2006_N0` +
  geom_line(aes(color = "Sole crop")) + 
  geom_line(data = IC_2, lty = 2, aes(color = Dominance)) + 
  ggtitle(NULL) +
  labs(y = NULL) +
  theme(legend.direction = "horizontal", legend.position = 'bottom')


