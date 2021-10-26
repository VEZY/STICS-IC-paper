# Purpose: run all the simulation of the project (Auzeville + Angers)
# Date: 05/11/2020



# Install the packages (to do only once) ----------------------------------

# remotes::install_github("SticsRPacks/SticsRPacks")
# SticsRPacks::SticsRPacks_update()

# Import the packages -----------------------------------------------------

library(SticsRPacks)
source("1-code/functions.R")

# Define javaStics installation -------------------------------------------

# STICS version -----------------------------------------------------------

javastics = normalizePath("0-javastics", winslash = "/")

# Define the workspaces ---------------------------------------------------

workspaces = list.dirs("0-data/usms", full.names = TRUE, recursive = FALSE)

# Define the variables to simulate ----------------------------------------

sim_variables = c("lai(n)","QNplante","Qfix","masec(n)","hauteur","CNgrain","mafruit","chargefruit")

# Run the simulations -----------------------------------------------------

# res = run_simulation(workspaces = workspaces, variables = sim_variables, javastics = javastics)
res = import_simulations(workspaces = workspaces, variables = sim_variables)

# Make the plots ----------------------------------------------------------

plots = plot(sim = res$`Auzeville-Pea-SC`$sim, obs = res$`Auzeville-Pea-SC`$obs, type="dynamic",verbose=FALSE)

# It is possible to subset the variables we actually want to plot:
# plotting_var = "lai_n"
plotting_var = sim_variables

# Plots per workspace:
dynamic_plots = 
  lapply(res, function(x){
    plot(sim = x$sim, obs = x$obs, type = "dynamic", verbose = FALSE, var = SticsRFiles:::var_to_col_names(plotting_var))
  })

scatter_plots = 
  lapply(res, function(x){
    plot(sim = x$sim, obs = x$obs, type = "scatter", verbose = FALSE, var = SticsRFiles:::var_to_col_names(plotting_var))
  })

# Plots merged by location:

res_Auzeville = merge_sim(res[grep("Auzeville",names(res))])
res_Angers = merge_sim(res[grep("Angers",names(res))])
res_location = list(Angers = res_Angers, Auzeville = res_Auzeville)

scatter_plots_location = 
  lapply(seq_along(res_location), function(x){
    plot(sim = res_location[[x]]$sim, obs = res_location[[x]]$obs, type = "scatter", verbose = FALSE, 
         var = SticsRFiles:::var_to_col_names(plotting_var))$all_situations+
      ggplot2::ggtitle(names(res_location)[[x]])
  })
names(scatter_plots_location) = names(res_location)

# saving plots to pdfs ----------------------------------------------------

# Scatter plot for each location:
lapply(seq_along(scatter_plots_location), function(x){
  save_plot_pdf(plot = scatter_plots_location[x], filename = paste0(names(scatter_plots_location)[x],"_scatter"),
            path = "2-outputs/paper_optim/plots", file_per_var = FALSE)
})

# Scatter plot for each USM:
lapply(seq_along(scatter_plots), function(x){
  save_plot_pdf(plot = scatter_plots[[x]], filename = paste0(names(scatter_plots)[x],"_scatter"),
            path = "2-outputs/paper_optim/plots", file_per_var = FALSE)
})

# Dynamic plot for each USM:
lapply(seq_along(dynamic_plots), function(x){
  save_plot_pdf(plot = dynamic_plots[[x]], filename = paste0(names(dynamic_plots)[x],"_dynamic"),
            path = "2-outputs/paper_optim/plots", file_per_var = FALSE)
})


# computing and saving statistics -----------------------------------------

# Scatter plot for each location:

stats_location= 
  lapply(seq_along(res_location), function(x){
    stats = summary(sim = res_location[[x]]$sim, obs = res_location[[x]]$obs, all_situations = TRUE)
    data.table::fwrite(stats,file = file.path("2-outputs/paper_optim/stats",paste0(names(res_location)[x],"_stats.csv")),sep = ";")
    stats
  })


stats= 
  lapply(seq_along(res), function(x){
    stats = summary(sim = res[[x]]$sim, obs = res[[x]]$obs, all_situations = TRUE)
    stats_each = summary(sim = res[[x]]$sim, obs = res[[x]]$obs, all_situations = FALSE)
    data.table::fwrite(stats,file = file.path("2-outputs/paper_optim/stats",paste0(names(res)[x],"_stats_all.csv")),sep = ";")
    data.table::fwrite(stats_each,file = file.path("2-outputs/paper_optim/stats",paste0(names(res)[x],"_stats_each.csv")),sep = ";")
    list(total = stats, by_usm = stats_each)
  })
