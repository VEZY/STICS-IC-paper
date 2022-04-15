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
main_dir = normalizePath("0-data/usms-optim-radiative", winslash = "/")

# Define the workspaces ---------------------------------------------------

workspace_usms =
  list(
    "Angers-IC-Pea_Barley" = "IC_PeaBarley_Angers_2003_N0_D50-50", # replace by N1?
    "Auzeville-IC" = "IC_Wheat_Pea_2005-2006_N0",
    "1Tprecoce2Stardif2012" = "1Tprecoce2Stardif2012",
    "Auzeville_wfb-Fababean-Wheat-IC" = "Fababean_Wheat_IC_2007"
  )

workspace_usms_sc =
  list(
    "Angers-SC-Barley" = "SC_Barley_Angers_2003_N0",
    "Angers-SC-Pea" = "SC_Pea_Angers_2003_N0",
    "Auzeville-Pea-SC" = "SC_Pea_2005-2006_N0",
    "Auzeville-Wheat-SC" = "SC_Wheat_2005-2006_N0",
    "sojaTardif2012-SC" = "SojaTardif-SC2012",
    "tourPrecoce2012-SC" = "TournPrecoce-SC2012",
    "Auzeville_wfb-Fababean-SC" = "Fababean_SC_2007",
    "Auzeville_wfb-Wheat-SC" = "Wheat_SC_2007"
  )

worskpaces_paths = file.path(main_dir, names(workspace_usms))
worskpaces_paths_sc = file.path(main_dir, names(workspace_usms_sc))

# Which sole crop usm correspond to a given intercrop system:
link_IC_SC =
  list(
    "IC_PeaBarley_Angers_2003_N0_D50-50" = c(p = "SC_Pea_Angers_2003_N0", a = "SC_Barley_Angers_2003_N0"),
    "IC_Wheat_Pea_2005-2006_N0" = c(p = "SC_Wheat_2005-2006_N0", a = "SC_Pea_2005-2006_N0"),
    "1Tprecoce2Stardif2012" = c(p = "TournPrecoce-SC2012", a = "SojaTardif-SC2012"),
    "Fababean_Wheat_IC_2007" = c(p = "Fababean_SC_2007", a = "Wheat_SC_2007")
  )

# SticsRFiles::get_usms_list("0-data/usms/Auzeville_wfb-Fababean-Wheat-IC/usms.xml")

# Define the variables to simulate ----------------------------------------

sim_variables = c("hauteur","lai(n)","masec(n)","QNplante","mafruit","Qfix",
                  # "ilevs",
                  "iflos",
                  "imats"
                  # "fapar"
                  # "ilaxs"
)

# SticsRFiles::get_var_info("Qfix")

# Run the IC simulations -----------------------------------------------------

# usms = SticsRFiles::get_usms_list(usm_path = file.path(workspace_wheat,"usms.xml"))

mapply(
  function(x,y){
    SticsRFiles::gen_varmod(x, sim_variables)
    SticsOnR::run_javastics(javastics_path = javastics,
                            workspace_path = x,
                            stics_exe = "Stics_IC_v13-01-2022.exe",
                            usms_list = y)
  },
  worskpaces_paths,
  workspace_usms
)

sim = mapply(function(x,y){
  get_sim(workspace = x,
          usm = y,
          usms_file = file.path(x, "usms.xml"))
},worskpaces_paths, workspace_usms)
names(sim) = unlist(workspace_usms)

# Run the sole crop simulations -------------------------------------------

mapply(
  function(x,y){
    SticsRFiles::gen_varmod(x, sim_variables)
    SticsOnR::run_javastics(javastics_path = javastics,
                            workspace_path = x,
                            stics_exe = "Stics_IC_v13-01-2022.exe",
                            usms_list = y)
  },
  worskpaces_paths_sc,
  workspace_usms_sc
)

sim_sc = mapply(function(x,y){
  get_sim(workspace = x,
          usm = y,
          usms_file = file.path(x, "usms.xml"))
},worskpaces_paths_sc, workspace_usms_sc)
names(sim_sc) = unlist(workspace_usms_sc)

# Compute new variables ---------------------------------------------------

# Add NDFA to sim + put all stages at same value along the whole crop for plotting:
sim = mapply(function(x,usms_sc){
  df_sc = bind_rows(sim_sc[usms_sc[["p"]]][[1]], sim_sc[usms_sc[["a"]]][[1]])
  
  x =
    x%>%
    group_by(Plant)%>%
    mutate(
      iflos = unique(as.integer(.data$iflos))[2],
      # iamfs = unique(as.integer(iamfs))[2],
      # ilevs = unique(as.integer(ilevs))[2],
      imats = unique(as.integer(.data$imats))[2],
      # ilaxs = unique(as.integer(ilaxs))[2]
      LER = ifelse(.data$mafruit == max(.data$mafruit), max(.data$mafruit) / max(df_sc$mafruit[df_sc$Plant == unique(.data$Plant)]),NA)
    )
  if(!is.null(x$Qfix)){
    return(x%>%mutate(NDFA = Qfix / QNplante))
  }else{
    x
  }
}, sim, link_IC_SC[names(sim)], SIMPLIFY = FALSE)

names(sim) = unlist(workspace_usms)
attr(sim, "class") = "cropr_simulation"

# Get the observations ----------------------------------------------------

obs = mapply(function(x,y){
  get_obs(workspace = x, usm = y, usms_file = file.path(x, "usms.xml"))
},worskpaces_paths,workspace_usms)
names(obs) = unlist(workspace_usms)

obs_sc = mapply(function(x,y){
  get_obs(workspace = x, usm = y, usms_file = file.path(x, "usms.xml"))
},worskpaces_paths_sc,workspace_usms_sc)
names(obs_sc) = unlist(workspace_usms_sc)

# Add NDFA to obs:
obs = mapply(function(x,usms_sc){
  df_sc = bind_rows(sim_sc[usms_sc[["p"]]][[1]], sim_sc[usms_sc[["a"]]][[1]])
  
  x =
    x%>%
    group_by(Plant)%>%
    mutate(
      LER = 
        max(.data$mafruit, na.rm = TRUE) / 
        max(df_sc$mafruit[df_sc$Plant == unique(.data$Plant)],na.rm = TRUE)
    )
  
  if(!is.null(x$Qfix)){
    return(x%>%mutate(NDFA = Qfix / QNplante))
  }else{
    x
  }
}, obs, link_IC_SC[names(obs)], SIMPLIFY = FALSE)

# Make the plots:
plots = plot(sim, obs = obs, type = "scatter", shape_sit = "txt")
# plots = plot(sim,obs=obs) # dynamic plots just in case

# Statistics --------------------------------------------------------------

stats = 
  summary(sim, obs = obs, stat = c("MAPE", "EF", "RMSE", "nRMSE", "Bias"))%>%
  select(-group, -situation)%>%
  filter(variable != "Qfix")%>%
  mutate(across(is.numeric, ~round(.x, 2)))%>%
  mutate(
    variable = 
      recode(
        variable,
        "lai_n" = "LAI~(m2~m^{-2})",
        "masec_n" = "Agb~(t~ha^{-1})",
        "mafruit" = "Gr.~yield~(t~ha^{-1})",
        # "Qfix" = "N~Fix.~(kg~ha^{-1})",
        "QNplante" = "N~acc.~(kg~ha^{-1})",
        "imats" = "Matur.~(julian~day)",
        "iflos" = "Flowe.~(julian~day)",
        "iamfs" = "Juven.~(julian~day)",
        "ilaxs" = "Max.L.~(julian~day)",
        "ilevs" = "Emerg.~(julian~day)",
        "hauteur" = "Height~(m)",
        "NDFA" = "NDFA~('%')",
        "LER" = "Partial~LER"
      )
  )%>%
  arrange(variable)
stats

write.csv(stats, "2-outputs/stats/stats_constrasted_systems.csv")

# Plot --------------------------------------------------------------------

df_ic =
  plots$all_situations$data%>%
  filter(variable != "Qfix")%>%
  mutate(
    Plant = recode(Plant,
                   "poi" = "Pea",
                   "ble" = "Wheat",
                   "soj" = "Soybean",
                   "faba" = "Fababean",
                   "tou" = "Sunflower",
                   "esc" = "Barley"
    ),
    Association = recode(Sit_Name,
                         "IC_PeaBarley_Angers_2003_N0_D50-50" = "Pea-Barley",
                         "IC_Wheat_Pea_2005-2006_N0"  = "Wheat-Pea",
                         "1Tprecoce2Stardif2012" = "Sunflower-Soybean",
                         "Fababean_Wheat_IC_2007" = "Fababean-Wheat"
    ),
    variable = recode(variable,
                      "lai_n" = "LAI~(m2~m^{-2})",
                      "masec_n" = "Agb~(t~ha^{-1})",
                      "mafruit" = "Gr.~yield~(t~ha^{-1})",
                      # "Qfix" = "N~Fix.~(kg~ha^{-1})",
                      "QNplante" = "N~acc.~(kg~ha^{-1})",
                      "imats" = "Matur.~(julian~day)",
                      "iflos" = "Flowe.~(julian~day)",
                      "iamfs" = "Juven.~(julian~day)",
                      "ilaxs" = "Max.L.~(julian~day)",
                      "ilevs" = "Emerg.~(julian~day)",
                      "hauteur" = "Height~(m)",
                      "NDFA" = "NDFA~('%')",
                      "LER" = "Partial~LER"
    )
  )%>%
  arrange(variable)

fig_num = 
  df_ic%>%
  group_by(variable)%>%
  summarise(
    y = max(Simulated,Observed),
    ymin = min(Simulated,Observed),
  )%>%
  mutate(plot_index = paste0(order(variable),"."))%>%
  arrange(variable)
fig_num


# Plotting

presentation = TRUE # FALSE for the paper (white background), TRUE for the presentation

if(presentation){
  text_color = "white"
}else{
  text_color = "black"
}

p = 
  ggplot(df_ic, aes(x = Observed, color = Plant, fill = Plant, shape = Association))+
  geom_point(aes(y = Simulated), size = 1.5, fill = "transparent", stroke = 1.5)+
  geom_point(aes(y = Simulated, fill = Plant), size = 1.5, alpha = 0.5, stroke = 1)+
  geom_point(aes(y = Observed), color = "transparent", fill = "transparent")+ # Just to get y=x scales
  geom_label(
    x = -Inf,
    aes(y = y, label = plot_index), inherit.aes = FALSE,
    data = fig_num, hjust = 0, size = 3.1,
    label.size = NA, fontface = "bold",
    parse = FALSE, color = text_color,
    fill = if(presentation){"#2F2F31"}else{"white"}
  )+
  geom_label(
    x = -Inf,
    aes(
      y = y - (y - ymin) * 0.31, 
      label = paste0("EF:",EF,"\nnRMSE:",nRMSE,"\nRMSE:",RMSE,"\nBias:",Bias)
    ),
    data = stats%>%mutate(y = fig_num$y, ymin = fig_num$ymin), hjust=0, size = 2.6,
    label.size = NA, inherit.aes = FALSE,
    parse = FALSE, color = text_color,
    fill = if(presentation){"#2F2F31"}else{"transparent"}
  )+
  facet_wrap(variable~.,
             scales = "free",
             ncol = if(presentation){5}else{NULL},
             labeller = label_parsed,
             shrink = TRUE)+
  labs(colour = "Plant species:",
       shape = "Association:",
       fill = "Plant species:")+
  theme_minimal()+
  geom_abline()+
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  scale_shape_manual(
    values = c(
      "Pea-Barley" = 21,
      "Wheat-Pea" = 22,
      "Sunflower-Soybean" = 23,
      "Fababean-Wheat" = 24
    )
  )+
  theme(
    legend.position = 'bottom',
    legend.box = 'vertical',
    strip.text.y = element_text(size = 8),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12)
  )+
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

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

if(!presentation){
  ggsave(filename = "Fig.3_contrasted_systems.png", path = "2-outputs/plots",
         width = 16, height = 18, units = "cm")
}else{
  ggsave(filename = "Fig.3_contrasted_systems.png", path = "2-outputs/plots/presentation",
         width = 24, height = 14, units = "cm")
}
