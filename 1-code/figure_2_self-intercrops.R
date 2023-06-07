# Purpose: compare the model outputs for sole crop and sole crop simulated as an
# intercrop (self-intercrop).
# Careful, the simulations are made using Beer's law because in intercrops when
# two crops have ~same height we compute using Beer's law whatever the parameter.
# Date: 05/11/2020

# system("java -version")
# R CMD javareconf JAVA_HOME=`/usr/libexec/java_home -v 1.8`
# Sys.setenv(JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.8.0_202.jdk/Contents/Home")

# Install the packages (to do only once) ----------------------------------

# remotes::install_github("SticsRPacks/SticsRPacks")

# Import the packages -----------------------------------------------------

library(ggplot2)
library(dplyr)
library(SticsRPacks)
source("1-code/functions.R")

Sys.setlocale("LC_TIME", "C") # To get the dates in english

# Define javaStics installation -------------------------------------------

# STICS version -----------------------------------------------------------

javastics <- normalizePath("0-javastics", winslash = "/")

# Define the workspaces ---------------------------------------------------

main_dir <- normalizePath("0-data/usms-optim-radiative", winslash = "/")

# Define the variables to simulate ----------------------------------------

sim_variables <- c("lai(n)", "masec(n)", "QNplante", "mafruit", "hauteur", "fapar", "irecs")

# Which variables need to be summed for plot scale results (instead of averaged)
to_sum <- c("lai_n", "masec_n", "abso_n", "mafruit", "QNplante", "fapar")
# SticsRFiles::get_var_info(keyword = "resmes")

# Run the simulations -----------------------------------------------------

# usms = SticsRFiles::get_usms_list(file = file.path(workspace,"usms.xml"))
usms <-
  list(
    "Auzeville-Pea-SC" = c(sc = "SC_Pea_2005-2006_N0", ic = "IC_Pea_Pea_2005-2006_N0"),
    "Auzeville-Wheat-SC" = c(sc = "SC_Wheat_2005-2006_N0", ic = "IC_Wheat_Wheat_2005-2006_N0"),
    "Angers-SC-Barley" = c(sc = "SC_Barley_Angers_2003_N0", ic = "IC_Barley_Barley_Angers_2003_N0"),
    "Auzeville_wfb-Fababean-SC" = c(sc = "Fababean_SC_2007", ic = "Fababean_IC_2007"),
    "sojaTardif2012-SC" = c(sc = "SojaTardif-SC2012", ic = "SojaTardif-IC2012"),
    "tourPrecoce2012-SC" = c(sc = "TournPrecoce-SC2012", ic = "TournPrecoce-IC2012")
  )

# SticsRFiles::get_usms_list("0-data/usms-optim-radiative/tourPrecoce2012-SC/usms.xml")

presentation <- FALSE # FALSE for the paper (white background), TRUE for the presentation

if (presentation) {
  text_color <- "white"
} else {
  text_color <- "black"
}

res_scat <- mapply(function(x, y) {
  # x = usms[[1]]
  # y = names(usms)[[1]]
  workspace <- file.path(main_dir, y)
  SticsRFiles::gen_varmod(workspace, sim_variables)
  SticsOnR::run_javastics(
    javastics = javastics, workspace = workspace,
    stics_exe = "Stics_IC_v24-05-2022_mac",
    usm = x
  )
  # Get the results
  sim <- get_sim(workspace = workspace, usm = x)

  # Add the date of harvest:
  sim <- lapply(sim, function(u) {
    # u = sim[["IC_Barley_Barley_Angers_2003_N0"]]
    plants <- unique(u$Plant)
    for (i in 1:length(plants)) {
      jul_rec <- unique(u$irecs[u$Plant == plants[i]])[2]
      u$date_harvest[u$Plant == plants[i]] <- u$Date[u$Plant == plants[i] & u$irecs == jul_rec][1]
    }
    u$date_harvest <- as.POSIXct(u$date_harvest, tz = "UTC")
    u <- mutate(u, is_harvested = ifelse(Date > date_harvest, TRUE, FALSE))
    # u <- filter(u, Date < date_harvest)
    return(u)
  })

  # Get the observations
  obs <- get_obs(workspace = workspace, usm = x)

  sd_vars <- unlist(lapply(sim_variables, function(x) grep(pattern = paste0(SticsRFiles:::var_to_col_names(x), "_sd"), x = colnames(obs[[1]]))))

  if (length(sd_vars) > 1) {
    sd_df <- obs[[1]][c(1, sd_vars)]
    sd_df$Plant <- obs[[1]]$Plant
    colnames(sd_df) <- gsub("_sd", "", colnames(sd_df))
    sd_list <- list(sd_df)
    names(sd_list) <- names(obs)[1]
  } else {
    sd_list <- NULL
  }

  dates_harvest <- lapply(sim, function(x) x$date_harvest[1])

  sim_plot <- lapply(sim, function(u) {
    select(u, -c(date_harvest, irecs, is_harvested))
  })
  attr(sim_plot, "class") <- "cropr_simulation"

  # Pre-make the plot with {CroPlotR}
  plots <- plot(sim_plot, obs = obs, obs_sd = sd_list)

  # Compute the sum (or average) of variables for both intercrops to match the sole crop

  df_ic <-
    left_join(
      plots[[grep("(IC_|-IC)", names(plots))]]$data,
      select(bind_rows(sim[grep("(IC_|-IC)", names(plots))], .id = "Sit_Name"), Sit_Name, Date, Plant, is_harvested),
      by = join_by(Date, Plant, Sit_Name)
    ) %>%
    group_by(.data$Date, .data$variable) %>%
    summarise(
      Simulated_sum = sum(.data$Simulated),
      Simulated_mean = mean(.data$Simulated),
      is_harvested = ifelse(any(.data$is_harvested), TRUE, FALSE)
    ) %>%
    mutate(
      Simulated = ifelse(.data$variable %in% to_sum, Simulated_sum, Simulated_mean)
      # Date_harvest = dates_harvest[[grep("(IC_|-IC)", names(dates_harvest))]],
    ) %>%
    select(-Simulated_sum, -Simulated_mean)

  df_sc <-
    left_join(
      plots[[grep("(SC_|-SC)", names(plots))]]$data,
      select(bind_rows(sim[grep("(SC_|-SC)", names(plots))], .id = "Sit_Name"), Sit_Name, Date, is_harvested),
      by = join_by(Date, Sit_Name)
    )
  # mutate(
  #   Date_harvest = dates_harvest[[grep("(SC_|-SC)", names(dates_harvest))]]
  # )

  df <-
    left_join(
      df_sc,
      df_ic,
      by = c("Date", "variable"),
      suffix = c("_sc", "_ic")
    )

  return(df)
}, usms, names(usms), SIMPLIFY = FALSE)

df <- bind_rows(res_scat, .id = "Crop")

df$Simulated_ic[df$is_harvested_ic] <- NA
df$Simulated_sc[df$is_harvested_sc] <- NA

# Scatter plot:
df %>%
  ggplot(aes(x = Simulated_sc, y = Simulated_ic, color = Crop)) +
  geom_abline() +
  geom_point() +
  facet_wrap(
    variable ~ .,
    scales = "free",
    labeller = label_parsed,
    strip.position = "left",
    ncol = 2
  ) +
  ggtitle(NULL) +
  labs(y = NULL) +
  theme_minimal() +
  # scale_colour_manual(values= c("Self Intercrop" = "#6EC0C0", "Sole crop" = "#746EC2"))+
  theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    strip.placement.y = "outside"
  )

# Line plot:
p <-
  df %>%
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
      "masec_n" = "bold(Biomass~(t~ha^{-1}))",
      "mafruit" = "bold(Grain~(t~ha^{-1}))",
      "QNplante" = "bold(N~acq.~(kg~N~ha^{-1}))",
      "fapar" = "bold(faPAR)",
      "hauteur" = "bold(Height~(m))"
    )
  ) %>%
  # filter(
  #   # is_harvested_sc == FALSE | is_harvested_sc == FALSE
  #   Date <= as.POSIXct("2005-11-30 UTC", tz = "UTC")
  # ) %>%
  ggplot(aes(x = Date)) +
  geom_errorbar(
    aes(y = Observed, ymin = Observed - Obs_SD, ymax = Observed + Obs_SD),
    show.legend = FALSE, lwd = 0.8,
    # shape = 21, stroke = 1.5,
    colour = "#F49690",
    na.rm = TRUE
  ) +
  geom_point(
    aes(y = Observed),
    show.legend = FALSE, size = 1.5,
    shape = 21, stroke = 1.5, colour = "#F49690", fill = "#F496904C",
    na.rm = TRUE
  ) +
  geom_line(aes(y = Simulated_ic, lty = "Self Intercrop", color = "Self Intercrop"), lwd = 1.3) +
  geom_line(aes(y = Simulated_sc, lty = "Sole crop", color = "Sole crop"), lwd = 1.3) +
  facet_grid(
    vars(variable),
    vars(Crop),
    scales = "free",
    labeller = label_parsed
  ) +
  scale_y_continuous(n.breaks = 5, minor_breaks = NULL) +
  scale_x_datetime(minor_breaks = NULL, date_breaks = "2 month", date_labels = "%b", guide = guide_axis(angle = 45)) +
  ggtitle(NULL) +
  labs(y = NULL) +
  theme_minimal() +
  scale_colour_manual(values = c("Self Intercrop" = "#6EC0C0", "Sole crop" = "#746EC2")) +
  labs(
    colour = "Simulation:",
    lty = "Simulation:"
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    axis.title.x = element_text(face = "bold", size = 12),
    # strip.text.x = element_text(angle = 45),
  ) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

p

ggsave(p,
  filename = "Fig.2_self-intercrop_all.png", path = "2-outputs/plots",
  width = 18, height = 20, units = "cm"
)

# write.csv(df, "2-outputs/stats/table_figure_2.csv", row.names = FALSE)
df <- read.csv("2-outputs/stats/table_figure_2.csv")

# Statistics for each variable for all crops:
df %>%
  group_by(variable) %>%
  summarise(
    RMSE = RMSE(Simulated_ic, Simulated_sc),
    EF = EF(Simulated_ic, Simulated_sc),
    Bias = Bias(Simulated_ic, Simulated_sc),
    R2 = R2(Simulated_ic, Simulated_sc),
  )

df %>%
  group_by(variable, Crop) %>%
  summarise(
    max_ic = max(Simulated_ic, na.rm = TRUE),
    max_sc = max(Simulated_sc, na.rm = TRUE)
  ) %>%
  mutate(
    diff = max_ic - max_sc,
    diff_perc = (max_ic - max_sc) / max_ic
  ) %>%
  ungroup() %>%
  group_by(variable) %>%
  filter(diff_perc == max(diff_perc))

df %>%
  group_by(variable, Crop) %>%
  summarise(
    max_ic = max(Simulated_ic),
    max_sc = max(Simulated_sc)
  ) %>%
  ungroup() %>%
  group_by(variable) %>%
  summarise(diff = mean(max_ic - max_sc), mean((max_ic - max_sc) / max_ic))
