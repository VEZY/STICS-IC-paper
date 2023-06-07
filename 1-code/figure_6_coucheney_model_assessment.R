# Purpose: assess the model simulations by reproducing the figure of Coucheney et al. (2015)
# the model.
# Date: 06/06/2023

# Import the packages -----------------------------------------------------
library(ggplot2)
library(dplyr)
library(ggrepel)
source("1-code/functions.R")

# Define the criteria for the model assessment ----------------------------

criteria_df <- data.frame(
    x = c(0.0, 0.0, 0.0, 0.0),
    y = c(0.45, 0.55, 0.65, 0.75),
    criteria = c("very good", "good", "satisfactory", "unsatisfactory")
)

# Import the data ---------------------------------------------------------

df_stages <- read.csv("2-outputs/stats/stats_constrasted_systems.csv")
df_all <- read.csv("2-outputs/stats/stats_constrasted_systems_all.csv")

# Bind the data -----------------------------------------------------------

df <- rbind(
    select(
        mutate(
            df_stages,
            origin = "Critical stages",
            variable = recode(variable, "N~acq.~(kg~N~ha^{-1})" = "Harvested N acq."),
        ),
        variable, origin, RMSEu_stdobs, RMSEs_stdobs
    ),
    select(
        mutate(
            df_all,
            variable,
            origin = "Full growth cycle",
            variable = recode(variable, "N~acq.~(kg~N~ha^{-1})" = "N acq."),
        ),
        variable, origin, RMSEu_stdobs, RMSEs_stdobs
    )
)

# Plot the data -----------------------------------------------------------

df %>%
    mutate(
        variable =
            recode(
                variable,
                "Flowering~(Julian~day)" = "Flowering",
                "Maturity~(Julian~day)" = "Maturity",
                "Max.~height~(m)" = "Max. height",
                "Max.~LAI~(m^{2}~m^{-2})" = "Max. LAI",
                "Harvested~Biomass~(t~ha^{-1})" = "Harvested Biomass",
                "Grain~(t~ha^{-1})" = "Grain",
                "N~grain~('%')" = "N grain",
                "NDFA~('%')" = "NDFA",
                "Partial~LER" = "Partial LER",
                "LAI~(m^{2}~m^{-2})" = "LAI",
                "Biomass~(t~ha^{-1})" = "Biomass",
                "N~Fix.~(kg~N~ha^{-1})" = "N Fix.",
                "Height~(m)" = "Height",
            )
    ) %>%
    ggplot() +
    geom_point(aes(x = RMSEu_stdobs, y = RMSEs_stdobs, shape = origin), size = 4) +
    geom_label_repel(aes(x = RMSEu_stdobs, y = RMSEs_stdobs, label = variable), size = 3, nudge_y = 0.03, nudge_x = 0.05) +
    geom_text(data = criteria_df, aes(x = x, y = y, label = criteria), size = 5, hjust = "left", nudge_x = -0.03) +
    geom_abline(intercept = 0, slope = 1, alpha = 0.3) +
    gg_circle(r = 0.5, xc = 0.0, yc = 0.0, color = "transparent", fill = "black", alpha = 0.2) +
    gg_circle(r = 0.6, xc = 0.0, yc = 0.0, color = "transparent", fill = "black", alpha = 0.2) +
    gg_circle(r = 0.7, xc = 0.0, yc = 0.0, color = "transparent", fill = "black", alpha = 0.2) +
    coord_cartesian(ylim = c(0.0, 0.8), xlim = c(0.0, 0.8)) +
    ylab("RMSEs / std-obs") +
    xlab("RMSEu / std-obs") +
    theme(legend.position = "bottom")

ggsave(
    filename = "Fig.6_coucheney_et_al_2015_assessment.png", path = "2-outputs/plots",
    width = 16, height = 16, units = "cm"
)
