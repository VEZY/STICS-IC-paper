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

res = mapply(function(x,y){
  # x = usms[[3]]
  # y = names(usms)[[3]]
  workspace = file.path(main_dir, y)
  SticsRFiles::gen_varmod(workspace, sim_variables)
  SticsOnR::run_javastics(
    javastics = javastics, workspace = workspace,
    stics_exe = "Stics_IC_v13-01-2022.exe",
    usms_list = x
  )
  # Get the results
  sim = get_sim(workspace = workspace, usm = x)
  
  # Get the observations
  obs = get_obs(workspace =  workspace, usm = x)
  # Pre-make the plot with {CroPlotR}
  plots = plot(sim,obs=obs)
  # Compute the sum (or average) of variables for both intercrops to match the sole crop  
  
  
  IC_sum =
    plots[[grep("(IC_|-IC)", names(plots))]]$data%>%
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
      plots[[grep("(SC_|-SC)", names(plots))]]$data,
      IC_sum, 
      by = c("Date","variable"), 
      suffix = c("_sc", "_ic")
    )%>%
    group_by(variable)%>%
    filter(Simulated_sc == max(Simulated_sc) | variable %in% c("resmes"))%>%
    mutate(
      y = max(Simulated_ic, Simulated_sc),
      error = Simulated_ic - Simulated_sc
    )%>%
    filter(error == max(error))%>%
    summarise(
      y = unique(y)*1.05,
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
               "QNplante"= "bold(N~acc.~(kg~ha^{-1}))",
               "fapar"= "bold(faPAR)",
               "resmes"= "bold(SWC~(mm))"
             )
    )%>%
    arrange(variable)%>%
    mutate(plot_index = order(variable))
  
  p = 
    plots[[grep("(SC_|-SC)", names(plots))]]$data%>%
    mutate(System = "Sole crop")%>%
    bind_rows(.,IC_sum)%>%
    mutate(variable = 
             recode(
               .data$variable,
               "lai_n" = "bold(LAI~(m^{2}~m^{-2}))",
               "masec_n" = "bold(Agb~(t~ha^{-1}))",
               "mafruit" = "bold(Grain~(t~ha^{-1}))",
               "QNplante"= "bold(N~acc.~(kg~ha^{-1}))",
               "fapar"= "bold(faPAR)",
               "resmes"= "bold(SWC~(mm))"
             )
    )%>%
    arrange(variable)%>%
    ggplot(aes(x = Date))+
    facet_wrap(variable~., scales = "free_y", labeller = label_parsed, strip.position = "left", ncol = 2)+
    geom_point(
      aes(y = Observed, color = System, fill = System), show.legend = FALSE, size = 1.5,
      shape = 21, stroke = 1.5, colour = "#F49690", fill = "#F496904C"
    )+
    geom_line(aes(y = Simulated, lty = System, color = System, fill = System), lwd = 1.3) +
    geom_label(
      x = as.POSIXct("2005-09-26 UTC", tz = "UTC"),
      aes(y = y, label = paste0(plot_index,".")),
      data = stats, hjust=0, size = 3.1,
      fill = if(presentation){"#2F2F31"}else{"white"},
      label.size = NA, fontface = "bold",
      parse = FALSE, color = text_color
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
      fill = if(presentation){"#2F2F31"}else{"transparent"},
      label.size = NA, size = 3.1,
      parse = FALSE, color = text_color
    )+
    geom_linerange(
      aes(x = Date, ymin = Simulated_ic, ymax = Simulated_sc), 
      data = stats, lwd = 1.3, color = 2, inherit.aes = FALSE
    ) +
    ggtitle(NULL) +
    labs(y = NULL) +
    theme_minimal() +
    labs(colour = "Simulation design:", lty = "Simulation design:")+
    # scale_y_continuous(expand = expansion(mult = c(0.1, 0.1)))+ # expand the limits of the plots
    scale_colour_manual(values= c("Self Intercrop" = "#6EC0C0", "Sole crop" = "#746EC2"))+
    theme(
      legend.direction = "horizontal", 
      legend.position = 'bottom', 
      strip.placement.y = "outside"
    )
  
  if(presentation){
    p = 
      p + 
      theme(
        axis.title = element_text(color="white"),
        axis.text = element_text(color="white"),
        legend.title = element_text(color="white"),
        legend.text = element_text(color="white"),
        strip.text = element_text(color="white"),
        panel.grid = element_line(color = '#696969', linetype = "dotted")
      )
  }
  
  # if(!presentation){
  #   ggsave(p, filename = "Fig.1_self-intercrop.png", path = "2-outputs/plots",
  #          width = 16, height = 13, units = "cm")
  # }else{
  #   ggsave(filename = "Fig.1_self-intercrop.png", path = "2-outputs/plots/presentation",
  #          width = 20, height = 10, units = "cm")
  # }
  
  return(list(plot = p, stats = stats))
}, usms, names(usms), SIMPLIFY = FALSE)

res$`Auzeville-Pea-SC`$plot
res$`Auzeville-Wheat-SC`$plot
res$`Angers-SC-Barley`$plot
res$`sojaTardif2012-SC`$plot
res$`tourPrecoce2012-SC`$plot
res$`Auzeville_wfb-Fababean-SC`$plot


res_scat = mapply(function(x,y){
  # x = usms[[3]]
  # y = names(usms)[[3]]
  workspace = file.path(main_dir, y)
  SticsRFiles::gen_varmod(workspace, sim_variables)
  SticsOnR::run_javastics(
    javastics = javastics, workspace = workspace,
    stics_exe = "Stics_IC_v13-01-2022.exe",
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
  group_by(variable,Crop)%>%
  summarise(
    nRMSE = nRMSE(Simulated_ic, Simulated_sc),
    EF = EF(Simulated_ic, Simulated_sc),
    Bias = Bias(Simulated_ic, Simulated_sc)
  )

