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

workspace = "0-data/usms-optim-beer/Auzeville-Pea-SC"

# Define the variables to simulate ----------------------------------------

sim_variables = c("lai(n)","masec(n)","QNplante","mafruit")

# Which variables need to be summed for plot scale results (instead of averaged)
to_sum = c("lai_n","masec_n","abso_n","mafruit","QNplante","demande","dltams_n",
           "msrac_n","cumraint","raint","dltamsen","deltai_n","laisen_n","rlj")
# SticsRFiles::get_var_info(keyword = "demande")

# Run the simulations -----------------------------------------------------

usms = SticsRFiles::get_usms_list(usm_path = file.path(workspace,"usms.xml"))
usms = c("SC_Pea_2005-2006_N0", "IC_Pea_Pea_2005-2006_N0")

SticsRFiles::gen_varmod(workspace, sim_variables)

SticsOnR::run_javastics(javastics_path = javastics, workspace_path = workspace,
                        stics_exe = "Stics_IC_v13-01-2022.exe",
                        usms_list = usms)

# Get the results
sim = get_sim(workspace = workspace, usm = usms)

# Get the observations
obs = get_obs(workspace =  workspace, usm = usms)

# Pre-make the plot with {CroPlotR}
plots = plot(sim,obs=obs)

# Compute the sum (or average) of variables for both intercrops to match the sole crop  
IC_sum =
  plots$`IC_Pea_Pea_2005-2006_N0`$data%>%
  group_by(.data$Date,.data$variable)%>%
  summarise(Simulated_sum = sum(.data$Simulated),
            Simulated_mean = mean(.data$Simulated))%>%
  mutate(
    Simulated = ifelse(.data$variable %in% to_sum, Simulated_sum, Simulated_mean),
    System = "Self Intercrop"
  )

# Statistics --------------------------------------------------------------

stats = 
  left_join(
    plots$`SC_Pea_2005-2006_N0`$data, 
    IC_sum, 
    by = c("Date","variable"), 
    suffix = c("_sc", "_ic")
  )%>%
  group_by(variable)%>%
  filter(Simulated_sc == max(Simulated_sc))%>%
  mutate(
    error = Simulated_ic - Simulated_sc
  )%>%
  filter(error == max(error))%>%
  summarise(
    y = max(Simulated_ic, Simulated_sc),
    Simulated_ic = head(Simulated_ic, 1),
    Simulated_sc = head(Simulated_sc, 1),
    Date = mean(Date),
    bias = Bias(sim = Simulated_ic, obs = Simulated_sc),
    rel_max_error = unique(error / Simulated_sc * 100))%>%
  mutate(variable = 
           recode(
             .data$variable,
             "lai_n" = "bold(LAI~(m^{2}~m^{-2}))",
             "masec_n" = "bold(Agb~(t~ha^{-1}))",
             "mafruit" = "bold(Grain~(t~ha^{-1}))",
             "QNplante"= "bold(N~acc.~(kg~ha^{-1}))"
           ),
         plot_index = order(variable))
stats

# Plot for the paper: -----------------------------------------------------

plots$`SC_Pea_2005-2006_N0`$data%>%
  mutate(System = "Sole crop")%>%
  bind_rows(.,IC_sum)%>%
  mutate(variable = 
           recode(
             .data$variable,
             "lai_n" = "bold(LAI~(m^{2}~m^{-2}))",
             "masec_n" = "bold(Agb~(t~ha^{-1}))",
             "mafruit" = "bold(Grain~(t~ha^{-1}))",
             "QNplante"= "bold(N~acc.~(kg~ha^{-1}))"
           )
  )%>%
  ggplot(aes(x = Date))+
  facet_wrap(variable~., scales = "free_y", labeller = label_parsed, strip.position = "left")+
  geom_point(
    aes(y = Observed, color = System, fill = System), show.legend = FALSE, size = 1.5,
    shape = 21, stroke = 1.5, colour = "#F49690", fill = "#F496904C"
  )+
  geom_line(aes(y = Simulated, lty = System, color = System, fill = System), lwd = 1.3) +
  geom_label(
    x = as.POSIXct("2005-09-26 UTC", tz = "UTC"),
    aes(y = y, label = paste0(plot_index,".")),
    data = stats, hjust=0, size = 3.1,
    label.size = NA, fontface = "bold",
    parse = FALSE
  )+
  # geom_text(
  #   x = as.POSIXct("2005-09-26 UTC", tz = "UTC"),
  #   aes(y = y, label = paste("Bias: ",format(bias, scientific = TRUE, digits = 2))),
  #   data = stats, hjust=0,
  #   parse = FALSE)+
  geom_label(
    x = as.POSIXct("2005-10-15 UTC", tz = "UTC"),
    aes(y = y, label = paste("Max. diff. (%):",format(rel_max_error, scientific = FALSE, digits = 2))),
    data = stats, hjust=0,
    label.size = NA, size = 3.1,
    parse = FALSE
  )+
  geom_linerange(
    aes(x = Date, ymin = Simulated_ic, ymax = Simulated_sc), 
    data = stats, lwd = 1.3, color = 2, inherit.aes = FALSE
  ) +
  ggtitle(NULL) +
  labs(y = NULL) +
  theme_minimal() +
  labs(colour = "Simulation design:",
       lty = "Simulation design:")+
  scale_colour_manual(values= c("Self Intercrop" = "#6EC0C0", "Sole crop" = "#746EC2"))+
  theme(legend.direction = "horizontal", legend.position = 'bottom', strip.placement.y = "outside")

ggsave(filename = "Fig.1_self-intercrop.png", path = "2-outputs/plots",
       width = 16, height = 10, units = "cm")


# Same plot but each intercrop is separated and some outputs are x2 to compare
# with sole crop:
IC_2 =
  plots$`IC_Pea_Pea_2005-2006_N0`$data%>%
  group_by(Dominance,Date,variable)%>%
  summarise(Simulated = ifelse(variable %in% to_sum,
                               Simulated*2,Simulated))

plots$`SC_Pea_2005-2006_N0` +
  geom_line(aes(color = "Sole crop")) +
  geom_line(data = IC_2, lty = 2, aes(color = Dominance)) +
  ggtitle(NULL) +
  labs(y = NULL) +
  theme(legend.direction = "horizontal", legend.position = 'bottom')
