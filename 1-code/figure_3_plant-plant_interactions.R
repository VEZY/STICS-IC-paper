# Purpose: simulate two crops in sole and intercrop to show we grasp the main
# interactions at play in the system.
# Date: 12/04/2021

# Install the packages (to do only once) ----------------------------------

# remotes::install_github("SticsRPacks/SticsRPacks")

# Import the packages -----------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyr)
library(SticsRPacks)
source("1-code/functions.R")
Sys.setlocale("LC_TIME", "C") # To get the dates in english

# Define javaStics installation -------------------------------------------

# STICS version -----------------------------------------------------------

javastics <- normalizePath("0-javastics", winslash = "/")

# Define the workspaces ---------------------------------------------------

workspace_wheat <- "0-data/usms-optim-radiative/Auzeville-Wheat-SC"
workspace_pea <- "0-data/usms-optim-radiative/Auzeville-Pea-SC"
workspace_wheat_pea <- "0-data/usms-optim-radiative/Auzeville-IC"
workspaces <- list(wheat = workspace_wheat, pea = workspace_pea, wheat_pea = workspace_wheat_pea)

# Define the variables to simulate ----------------------------------------

# sim_variables = c("lai(n)","masec(n)","QNplante","mafruit","Qfix","profexteau","profextN","hauteur")
sim_variables <- c("lai(n)", "masec(n)", "QNplante", "mafruit", "Qfix", "fapar", "hauteur", "irecs")
# SticsRFiles::get_var_info(keyword = "transpi")

# Run the simulations -----------------------------------------------------

# usms = SticsRFiles::get_usms_list(usm_path = file.path(workspace_wheat,"usms.xml"))
usms_wheat <- "SC_Wheat_2005-2006_N0"
usms_pea <- "SC_Pea_2005-2006_N0"
usms_wheat_pea <- "IC_Wheat_Pea_2005-2006_N0"
usms <- list(wheat = usms_wheat, pea = usms_pea, wheat_pea = usms_wheat_pea)

lapply(workspaces, function(x) SticsRFiles::gen_varmod(x, sim_variables))


mapply(function(x, y) {
  SticsOnR::run_javastics(
    javastics = javastics,
    workspace = normalizePath(x, winslash = "/"),
    stics_exe = "Stics_IC_v24-05-2022_mac",
    usm = y
  )
}, workspaces, usms)

sim <- mapply(function(x, y) {
  get_sim(workspace = x, usm = y, usms_file = file.path(x, "usms.xml"))
}, workspaces, usms)

# Add NDFA to sim:
sim <- lapply(sim, function(x) {
  # x = sim[[1]]
  if (!is.null(x$Qfix)) {
    x <- x %>% mutate(NDFA = Qfix / QNplante)
  }

  if (!is.null(x$ep) & !is.null(x$dltams_n)) {
    # ep in mm day-1
    x <- x %>%
      mutate(wue = dltams_n / ep) %>%
      select(-ep, -dltams_n)
    x$wue[x$lai <= 0.01] <- NA
  }

  plants <- unique(x$Plant)
  for (i in 1:length(plants)) {
    jul_rec <- unique(x$irecs[x$Plant == plants[i]])[2]
    x$date_harvest[x$Plant == plants[i]] <- x$Date[x$Plant == plants[i] & x$irecs == jul_rec][1]
  }

  x$date_harvest <- as.POSIXct(x$date_harvest, tz = "UTC")
  return(x)
})
names(sim) <- usms
attr(sim, "class") <- "cropr_simulation"

# Get the observations

obs <- mapply(function(x, y) {
  get_obs(workspace = x, usm = y, usms_file = file.path(x, "usms.xml"))
}, workspaces, usms)
names(obs) <- usms

# Add NDFA to obs:
obs <- lapply(obs, function(x) {
  if (!is.null(x$Qfix)) {
    x <- x %>% mutate(NDFA = Qfix / QNplante)
  }

  if (!is.null(x$inns)) {
    x <- x %>% select(-inns)
  }
  vars <- unlist(lapply(c("Date", "Plant", "NDFA", sim_variables), function(y) grep(pattern = SticsRFiles:::var_to_col_names(y), x = colnames(x))))
  return(x[, vars])
})


obs_sd <- lapply(obs, function(x) {
  sd_vars <- unlist(lapply(sim_variables, function(y) grep(pattern = paste0(SticsRFiles:::var_to_col_names(y), "_sd"), x = colnames(x))))

  if (length(sd_vars) > 1) {
    sd_df <- x[c(1, sd_vars)]
    sd_df$Plant <- x$Plant
    colnames(sd_df) <- gsub("_sd", "", colnames(sd_df))
  } else {
    sd_df <- NULL
  }
  return(sd_df)
})

obs_sd <- lapply(obs_sd, function(x) {
  x$NDFA <- NA
  x$NDFA[1] <- 0
  vars <- unlist(lapply(c("Date", "Plant", "NDFA", sim_variables), function(y) which(SticsRFiles:::var_to_col_names(y) == colnames(x))))
  return(x[, vars])
})

# obs_sd_test = lapply(obs_sd, function(x){
#   select(x, Date, Plant, lai_n, masec_n, QNplante, mafruit, hauteur)
# })

obs <- lapply(obs, function(x) {
  vars <- unlist(lapply(c("Date", "Plant", "NDFA", sim_variables), function(y) which(SticsRFiles:::var_to_col_names(y) == colnames(x))))
  return(x[, vars])
})

# Make the plots:
plots <- plot(sim, obs = obs, obs_sd = obs_sd)
# plots_sd <- plot(sim, obs = obs, obs_sd = obs_sd)

# Computing per ha for both sole crops and intercrops:

# Which variables need to be summed for plot scale results (instead of averaged)
to_sum_obs <- c("lai_n", "masec_n", "mafruit", "QNplante", "Qfix", "cep")
to_sum_sim <- c(to_sum_obs, "fapar")

IC_sum <-
  plots$`IC_Wheat_Pea_2005-2006_N0`$data %>%
  group_by(Date, variable, Plant) %>%
  summarise(
    Simulated = ifelse(variable %in% to_sum_sim,
      Simulated * 2, # To give a full 1ha surface of production
      Simulated
    ),
    Observed = ifelse(variable %in% to_sum_obs,
      Observed * 2,
      Observed
    ),
    Obs_SD = ifelse(variable %in% to_sum_obs,
      Obs_SD * 2,
      Obs_SD
    )
  ) %>%
  mutate(
    System = "Intercrop",
    Plant = ifelse(Plant == "poi", "bold(Pea)", "bold(Wheat)"),
    Date_harvest = unique(sim$`IC_Wheat_Pea_2005-2006_N0`$date_harvest)
  )

SC_sum <-
  dplyr::bind_rows(
    plots$`SC_Wheat_2005-2006_N0`$data %>%
      mutate(Plant = "bold(Wheat)", Date_harvest = unique(sim$`SC_Wheat_2005-2006_N0`$date_harvest)),
    plots$`SC_Pea_2005-2006_N0`$data %>%
      mutate(Plant = "bold(Pea)", Date_harvest = unique(sim$`SC_Pea_2005-2006_N0`$date_harvest))
  ) %>%
  mutate(System = "Sole crop")

df <- bind_rows(IC_sum, SC_sum) %>% filter(variable != "Qfix" & variable != "fapar")

# Statistics --------------------------------------------------------------

filter(df, variable == "lai_n" & !is.na(Observed) & System == "Intercrop")

stats <-
  df %>%
  group_by(Plant, System, variable) %>%
  # mutate(diff = Simulated - )%>%
  summarize(
    sim_max = ifelse(all(variable != "inns"), max(Simulated), min(Simulated)),
    obs_max = max(Observed, na.rm = TRUE)
  ) %>%
  ungroup()

stats$diff_sim[stats$System == "Intercrop"] <- (stats$sim_max[stats$System == "Intercrop"] - stats$sim_max[stats$System == "Sole crop"]) / stats$sim_max[stats$System == "Sole crop"] * 100
stats$diff_obs[stats$System == "Intercrop"] <- (stats$obs_max[stats$System == "Intercrop"] - stats$obs_max[stats$System == "Sole crop"]) / stats$obs_max[stats$System == "Sole crop"] * 100

stats <-
  stats %>%
  filter(System == "Intercrop") %>%
  select(-System, -sim_max, -obs_max) %>%
  mutate(Plant = ifelse(Plant == "bold(Pea)", "Pea", "Wheat"))

stat_diff <-
  pivot_wider(stats, names_from = Plant, values_from = c(diff_sim, diff_obs)) %>%
  select(variable, diff_obs_Pea, diff_sim_Pea, diff_obs_Wheat, diff_sim_Wheat) %>%
  mutate(across(is.numeric, ~ round(.x, 0))) %>%
  mutate(
    variable =
      recode(
        variable,
        "lai_n" = "LAI (m2 m-2)",
        "masec_n" = "Agb (t ha-1)",
        "fapar" = "FaPAR (%)",
        "mafruit" = "Gr. yield (t ha-1)",
        "Qfix" = "N Fix. (kg ha-1)",
        "QNplante" = "N acc. (kg ha-1)",
        "NDFA" = "NDFA (%)",
        "inns" = "N stress eff."
      )
  ) %>%
  arrange(variable)


write.csv(stat_diff, file = "2-outputs/stats/Table.1_Plant_interactions.csv", row.names = FALSE)

# Plotting

numbering <-
  df %>%
  group_by(variable) %>%
  summarize(Plant = unique(Plant), y = max(Simulated)) %>%
  group_by(Plant) %>%
  mutate(
    variable =
      recode(
        .data$variable,
        "lai_n" = "bold(LAI~(m^{2}~m^{-2}))",
        "masec_n" = "bold(Biomass~(t~ha^{-1}))",
        "fapar" = "bold(FaPAR~('%'))",
        "mafruit" = "bold(Grain~(t~ha^{-1}))",
        "Qfix" = "bold(N~Fix.~(kg[N]~ha^{-1}))",
        "QNplante" = "bold(N~acq.~(kg~N~ha^{-1}))",
        "NDFA" = "bold(NDFA~('%'))",
        "inns" = "bold(N~stress~eff.)",
        "hauteur" = "bold(Height~(m))"
      )
  ) %>%
  arrange(variable) %>%
  mutate(
    plot_index = order(variable),
    plot_nb = ifelse(
      Plant == "bold(Pea)",
      paste(plot_index, "a", sep = "."),
      paste(plot_index, "b", sep = ".")
    )
  )

presentation <- FALSE # FALSE for the paper (white background), TRUE for the presentation

if (presentation) {
  text_color <- "white"
} else {
  text_color <- "black"
}

df_harvest <- df
df_harvest$Simulated[df_harvest$Date >= df_harvest$Date_harvest] <- NA
max_harvest_date <- max(df_harvest$Date_harvest)
p <-
  df_harvest %>%
  mutate(
    variable =
      recode(
        variable,
        "lai_n" = "bold(LAI~(m^{2}~m^{-2}))",
        "masec_n" = "bold(Biomass~(t~ha^{-1}))",
        "fapar" = "bold(FaPAR~('%'))",
        "mafruit" = "bold(Grain~(t~ha^{-1}))",
        "Qfix" = "bold(N~Fix.~(kg[N]~ha^{-1}))",
        "QNplante" = "bold(N~acq.~(kg~N~ha^{-1}))",
        "NDFA" = "bold(NDFA~('%'))",
        "inns" = "bold(N~stress~eff.)",
        "hauteur" = "bold(Height~(m))"
      )
  ) %>%
  filter(
    Date >= as.POSIXct("2005-11-30 UTC", tz = "UTC") &
      Date <= max_harvest_date
  ) %>%
  ggplot(aes(x = Date, color = System, fill = System)) +
  geom_line(aes(y = Simulated, lty = System), lwd = 1.3) +
  geom_errorbar(
    aes(y = Observed, ymin = Observed - Obs_SD, ymax = Observed + Obs_SD),
    show.legend = FALSE, lwd = 0.8,
    # shape = 21, stroke = 1.5, colour = "#F49690", size = 1.5,
    na.rm = TRUE
  ) +
  geom_point(aes(y = Observed, shape = System), size = 1.5, stroke = 1.5) +
  facet_grid(
    scales = "free_y", cols = vars(Plant), rows = vars(variable),
    labeller = label_parsed,
    switch = "y"
  ) +
  geom_label(
    x = as.POSIXct("2005-11-30 UTC", tz = "UTC"),
    aes(y = y, label = plot_nb), inherit.aes = FALSE,
    data = numbering, hjust = 0, size = 3.1,
    label.size = NA, fontface = "bold",
    parse = FALSE, color = text_color,
    fill = if (presentation) {
      "#2F2F31"
    } else {
      "white"
    }
  ) +
  theme_minimal() +
  labs(y = NULL) +
  scale_colour_manual(values = c("Intercrop" = "#6EC0C0", "Sole crop" = "#746EC2")) +
  scale_fill_manual(values = c("Intercrop" = "#6EC0C04C", "Sole crop" = "#746EC24C")) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) + # expand the limits of the plots
  scale_shape_manual(values = c("Intercrop" = 21, "Sole crop" = 23)) +
  labs(colour = "Cropping system:", lty = "Cropping system:", shape = "Cropping system:", fill = "Cropping system:") +
  theme(
    strip.text.x = element_text(size = 12, face = "bold"),
    strip.text.y = element_text(size = 8),
    legend.direction = "horizontal",
    legend.position = "bottom",
    strip.placement.y = "outside"
  )

p

if (presentation) {
  p <-
    p +
    theme(
      axis.title = element_text(color = "white"),
      axis.text = element_text(color = "white"),
      legend.title = element_text(color = "white"),
      legend.text = element_text(color = "white"),
      strip.text = element_text(color = "white"),
      panel.grid = element_line(color = "#696969", linetype = "dotted")
    )
}


if (!presentation) {
  ggsave(p,
    filename = "Fig.3_sole_vs_intercrop.png", path = "2-outputs/plots",
    width = 16, height = 18, units = "cm"
  )
} else {
  ggsave(p,
    filename = "Fig.3_sole_vs_intercrop.png", path = "2-outputs/plots/presentation",
    width = 22, height = 17.5, units = "cm"
  )
}
