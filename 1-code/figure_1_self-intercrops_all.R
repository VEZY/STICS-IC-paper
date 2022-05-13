# Purpose: compare the model outputs for sole crop and sole crop simulated as an
# intercrop (self-intercrop).
# Careful, the simulations are made using Beer's law because in intercrops when
# two crops have ~same height we compute using Beer's law whatever the parameter.
# Date: 05/11/2020

# Install the packages (to do only once) ----------------------------------

# remotes::install_github("SticsRPacks/SticsRPacks")

# Import the packages -----------------------------------------------------

library(tidyverse)
library(SticsRPacks)
source("1-code/functions.R")

Sys.setlocale("LC_TIME", "C") # To get the dates in english

# Define javaStics installation -------------------------------------------

# STICS version -----------------------------------------------------------

javastics = normalizePath("0-javastics", winslash = "/")

# Define the workspaces ---------------------------------------------------

main_dir = normalizePath("0-data/usms-optim-radiative", winslash = "/")

# Define the variables to simulate ----------------------------------------

sim_variables = c("lai(n)","masec(n)","QNplante","mafruit", "hauteur", "fapar")

# Which variables need to be summed for plot scale results (instead of averaged)
to_sum = c("lai_n","masec_n","abso_n","mafruit","QNplante", "fapar")
# SticsRFiles::get_var_info(keyword = "res")

# Run the simulations -----------------------------------------------------

# usms = SticsRFiles::get_usms_list(file = file.path(workspace,"usms.xml"))
usms =
  list(
    "Auzeville-Pea-SC" = c(sc = "SC_Pea_2005-2006_N0", ic = "IC_Pea_Pea_2005-2006_N0"),
    "Auzeville-Wheat-SC" = c(sc = "SC_Wheat_2005-2006_N0", ic = "IC_Wheat_Wheat_2005-2006_N0"),
    "Angers-SC-Barley" = c(sc = "SC_Barley_Angers_2003_N0", ic = "IC_Barley_Barley_Angers_2003_N0"),
    "Auzeville_wfb-Fababean-SC" = c(sc = "Fababean_SC_2007", ic = "Fababean_IC_2007"),
    "sojaTardif2012-SC" = c(sc = "SojaTardif-SC2012", ic = "SojaTardif-IC2012"),
    "tourPrecoce2012-SC" = c(sc = "TournPrecoce-SC2012", ic = "TournPrecoce-IC2012")
  )

# SticsRFiles::get_usms_list("0-data/usms-optim-radiative/tourPrecoce2012-SC/usms.xml")

presentation = FALSE # FALSE for the paper (white background), TRUE for the presentation

if(presentation){
  text_color = "white"
}else{
  text_color = "black"
}

res_scat = mapply(function(x,y){
  # x = usms[[3]]
  # y = names(usms)[[3]]
  workspace = file.path(main_dir, y)
  SticsRFiles::gen_varmod(workspace, sim_variables)
  SticsOnR::run_javastics(
    javastics = javastics, workspace = workspace,
    stics_exe = "Stics_IC_v13-05-2022.exe",
    usms_list = x
  )
  # Get the results
  sim = get_sim(workspace = workspace, usm = x)

  # Get the observations
  obs = get_obs(workspace =  workspace, usm = x)
  # Pre-make the plot with {CroPlotR}
  plots = plot(sim,obs=obs)
  # Compute the sum (or average) of variables for both intercrops to match the sole crop

  df_ic =
    plots[[grep("(IC_|-IC)", names(plots))]]$data%>%
    group_by(.data$Date,.data$variable)%>%
    summarise(Simulated_sum = sum(.data$Simulated),
              Simulated_mean = mean(.data$Simulated))%>%
    mutate(
      Simulated = ifelse(.data$variable %in% to_sum, Simulated_sum, Simulated_mean)
    )%>%
    select(-Simulated_sum,-Simulated_mean)

  df_sc = plots[[grep("(SC_|-SC)", names(plots))]]$data%>%select(-Sit_Name)

  df =
    left_join(
      df_sc,
      df_ic,
      by = c("Date","variable"),
      suffix = c("_sc", "_ic")
    )

  return(df)
}, usms, names(usms), SIMPLIFY = FALSE)

df = bind_rows(res_scat, .id = "Crop")

# Scatter plot:
ggplot(df, aes(x = Simulated_sc , y = Simulated_ic, color = Crop))+
  geom_abline()+
  geom_point()+
  facet_wrap(
    variable~.,
    scales = "free",
    labeller = label_parsed,
    strip.position = "left",
    ncol = 2
  )+
  ggtitle(NULL) +
  labs(y = NULL) +
  theme_minimal() +
  # scale_colour_manual(values= c("Self Intercrop" = "#6EC0C0", "Sole crop" = "#746EC2"))+
  theme(
    legend.direction = "horizontal",
    legend.position = 'bottom',
    strip.placement.y = "outside"
  )

# Line plot:
p =
  df%>%
  mutate(
    Crop = recode(
      Crop,
      "Auzeville-Pea-SC" = "bold(Pea)",
      "Auzeville-Wheat-SC" = "bold(Wheat)",
      "Angers-SC-Barley" = "bold(Barley)",
      "Auzeville_wfb-Fababean-SC" = "bold(Fababean)",
      "sojaTardif2012-SC" = "bold(Soybean)",
      "tourPrecoce2012-SC" = "bold(Sunflower)"
    ),
    variable = recode(
      .data$variable,
      "lai_n" = "bold(LAI~(m^{2}~m^{-2}))",
      "masec_n" = "bold(Agb~(t~ha^{-1}))",
      "mafruit" = "bold(Grain~(t~ha^{-1}))",
      "QNplante"= "bold(N~acc.~(kg~ha^{-1}))",
      "fapar"= "bold(faPAR)",
      "hauteur"= "bold(Height~(m))"
    )
  )%>%
  ggplot(aes(x = Date))+
  geom_point(
    aes(y = Observed), show.legend = FALSE, size = 1.5,
    shape = 21, stroke = 1.5, colour = "#F49690", fill = "#F496904C",
    na.rm = TRUE
  )+
  geom_line(aes(y = Simulated_ic, lty = "Self Intercrop", color = "Self Intercrop"), lwd = 1.3)+
  geom_line(aes(y = Simulated_sc, lty = "Sole crop", color = "Sole crop"), lwd = 1.3)+
  facet_grid(
    vars(variable),
    vars(Crop),
    scales = "free",
    labeller = label_parsed
  )+
  scale_y_continuous(n.breaks = 5, minor_breaks = NULL)+
  scale_x_datetime(minor_breaks = NULL)+
  ggtitle(NULL) +
  labs(y = NULL) +
  theme_minimal() +
  scale_colour_manual(values= c("Self Intercrop" = "#6EC0C0", "Sole crop" = "#746EC2"))+
  labs(
    colour = "Simulation:",
    lty = "Simulation:"
  )+
  theme(
    legend.position = 'bottom',
    legend.box = 'vertical',
    axis.title.x = element_text(face = "bold", size = 12),
  )+
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

p

ggsave(p, filename = "Fig.1_self-intercrop_all.png", path = "2-outputs/plots",
       width = 18, height = 18, units = "cm")

# Statistics for each variable for all crops:
df%>%
  group_by(variable)%>%
  summarise(
    RMSE = RMSE(Simulated_ic, Simulated_sc),
    EF = EF(Simulated_ic, Simulated_sc),
    Bias = Bias(Simulated_ic, Simulated_sc)
  )

df%>%
  group_by(variable, Crop)%>%
  summarise(
    max_ic = max(Simulated_ic),
    max_sc = max(Simulated_sc)
    )%>%
  ungroup()%>%
  group_by(variable)%>%
  summarise(diff = mean(max_ic - max_sc))
