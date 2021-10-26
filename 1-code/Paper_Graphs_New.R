#Create plots for STICS paper
#Date: 10/11/2020

# Import the packages -----------------------------------------------------

library(SticsRPacks)
source("1-code/functions.R")

# Define the plots directory

plotdir = "2-outputs/paper_optim/plots"

# Define javaStics installation -------------------------------------------

# STICS version -----------------------------------------------------------

javastics = normalizePath("0-javastics", winslash = "/")

# Define the workspaces ---------------------------------------------------

workspaces = list.dirs("0-data/usms", full.names = TRUE, recursive = FALSE)

# Define the variables to simulate ----------------------------------------

sim_variables = c("lai(n)","QNplante","Qfix","masec(n)","hauteur","CNgrain","mafruit","chargefruit","iflos","imats")

# Run the simulations -----------------------------------------------------

res = run_simulation(workspaces = workspaces, variables = sim_variables, javastics = javastics)

#How to call one sim treatment
#res$`Auzeville-Wheat-SC`$sim$`SC_Wheat_2012-2013_N1`
#How to call one obs treatment
#res$`Auzeville-Wheat-SC`$obs$`SC_Wheat_2012-2013_N1`
# It is possible to subset the variables we actually want to plot:
# plotting_var = "lai_n"

#Figure 2 SC sim v obs LAI, masec, QNplante,mafruit Wheat, Pea, Barley, Spring Pea
#a) SC LAI
#Select plotting variable
plotting_var_lai = "lai_n"

# Sort out SC simulations
res_SC= res[grep("SC",names(res))]
res_SC$`Auzeville-Wheat-SC`$sim$`IC_Wheat_Wheat_2005-2006_N0` = NULL
res_SC$`Auzeville-Wheat-SC`$obs$`IC_Wheat_Wheat_2005-2006_N0` = NULL

scatter_plots_SC_LAI = 
  lapply(seq_along(res_SC), function(x){
    plot(sim = res_SC[[x]]$sim, obs = res_SC[[x]]$obs, 
         type = "scatter", verbose = FALSE, 
         var = SticsRFiles:::var_to_col_names(plotting_var_lai))$all_situations+
      ggplot2::ggtitle(names(res_SC)[[x]])
  })
names(scatter_plots_SC_LAI) = paste0(names(res_SC), "_", plotting_var_lai)


#b) SC Masec
plotting_var_masec = "masec_n"

scatter_plots_SC_masec = 
  lapply(seq_along(res_SC), function(x){
    plot(sim = res_SC[[x]]$sim, obs = res_SC[[x]]$obs, 
         type = "scatter", verbose = FALSE, 
         var = SticsRFiles:::var_to_col_names(plotting_var_masec))$all_situations+
      ggplot2::ggtitle(names(res_SC)[[x]])
  })
names(scatter_plots_SC_masec) = paste0(names(res_SC), "_", plotting_var_masec)

# CroPlotR::plot_save(scatter_plots_SC_masec, path = plotdir)

#c) SC QNplante
plotting_var_QNplante = "QNplante"

scatter_plots_SC_QNplante = 
  lapply(seq_along(res_SC), function(x){
    plot(sim = res_SC[[x]]$sim, obs = res_SC[[x]]$obs, 
         type = "scatter", verbose = FALSE, 
         var = SticsRFiles:::var_to_col_names(plotting_var_QNplante))$all_situations+
      ggplot2::ggtitle(names(res_SC)[[x]])
  })
names(scatter_plots_SC_QNplante) = paste0(names(res_SC), "_", plotting_var_QNplante)

#d) SC mafruit
plotting_var_mafruit = "mafruit"

scatter_plots_SC_mafruit = 
  lapply(seq_along(res_SC), function(x){
    plot(sim = res_SC[[x]]$sim, obs = res_SC[[x]]$obs, 
         type = "scatter", verbose = FALSE, 
         var = SticsRFiles:::var_to_col_names(plotting_var_mafruit))$all_situations+
      ggplot2::ggtitle(names(res_SC)[[x]])
  })
names(scatter_plots_SC_mafruit) = paste0(names(res_SC), "_", plotting_var_mafruit)

#Figure 3 Simulated versus observed developmental stages along the 1:1 line for 
#IC Wheat-Pea and IC Pea-Barley
plotting_var_pheno=c("iflos","imats")

#Filter out IC simulations
res_IC_Auz= res[grep("Auzeville-IC",names(res))]
res_IC_Ang= res[grep("Angers-IC",names(res))]
res_IC= res[grep("IC",names(res))]

scatter_plots_IC_pheno = 
  lapply(seq_along(res_IC), function(x){
    plot(sim = res_IC[[x]]$sim, obs = res_IC[[x]]$obs, 
         type = "scatter", verbose = FALSE, 
         var = SticsRFiles:::var_to_col_names(plotting_var_pheno))$all_situations+
      ggplot2::ggtitle(names(res_SC)[[x]])
  })
names(scatter_plots_IC_pheno) = paste0(names(res_IC), "_", paste(plotting_var_pheno, collapse = "+"))

#Figure 4:Simulated and observed leaf area index (LAI) over time for a) the 
#winter wheat and winter pea treatments at Auzeville, and (b) the spring pea
#spring barley treatments at Angers.

#Filter out IC simulations for 2007
res_IC_Auz_2007= res_IC_Auz[grep("Auzeville-IC",names(res_IC_Auz))]
#Filter out IC simulations for 2003
res_IC_Ang_2003= res_IC_Ang[grep("Angers-IC-Pea_Barley",names(res_IC_Ang))]
#a)
dynamic_plots_Auz_lai = 
  lapply(seq_along(res_IC_Auz_2007), function(x){
    plot(sim = res_IC_Auz_2007[[x]]$sim, obs = res_IC_Auz_2007[[x]]$obs, 
         type = "dynamic", verbose = FALSE, 
         var = SticsRFiles:::var_to_col_names(plotting_var_lai))
  })[[1]]
names(dynamic_plots_Auz_lai) = paste("Auzeville", names(dynamic_plots_Auz_lai),plotting_var_lai, sep="_")

#b)
dynamic_plots_Ang_lai = 
  lapply(seq_along(res_IC_Ang_2003), function(x){
    plot(sim = res_IC_Ang_2003[[x]]$sim, obs = res_IC_Ang_2003[[x]]$obs, 
         type = "dynamic", verbose = FALSE, 
         var = SticsRFiles:::var_to_col_names(plotting_var_lai))
  })[[1]]
names(dynamic_plots_Ang_lai) = paste("Angers", names(dynamic_plots_Ang_lai),plotting_var_lai, sep="_")

#Figure 5:Simulated and observed crop height (hauteur) over time for a) the 
#winter wheat and winter pea treatments at Auzeville, and (b) the spring pea
#spring barley treatments at Angers.
plotting_var_hauteur="hauteur"
#a)
dynamic_plots_Auz_hauteur = 
  lapply(seq_along(res_IC_Auz_2007), function(x){
    plot(sim = res_IC_Auz_2007[[x]]$sim, obs = res_IC_Auz_2007[[x]]$obs, 
         type = "dynamic", verbose = FALSE, 
         var = SticsRFiles:::var_to_col_names(plotting_var_hauteur))
  })[[1]]
names(dynamic_plots_Auz_hauteur) = paste("Auzeville", names(dynamic_plots_Auz_hauteur),plotting_var_hauteur, sep="_")

#b)
dynamic_plots_Ang_hauteur = 
  lapply(seq_along(res_IC_Ang_2003), function(x){
    plot(sim = res_IC_Ang_2003[[x]]$sim, obs = res_IC_Ang_2003[[x]]$obs, 
         type = "dynamic", verbose = FALSE, 
         var = SticsRFiles:::var_to_col_names(plotting_var_hauteur))
  })[[1]]
names(dynamic_plots_Ang_hauteur) = paste("Angers", names(dynamic_plots_Ang_hauteur),plotting_var_hauteur, sep="_")

#SKIP FIGURE 6 FOR NOW
#Figure 6:Simulated and observed N uptake (QNplante) over time for a) the 
#winter wheat and winter pea treatments at Auzeville, and (b) the spring pea
#spring barley treatments at Angers.

#Figure 7:Simulated versus observed values for intercropped wheat-pea and pea-barley
#for a) in season total above ground biomass, b) end of season grain yield and 
#c) grain N concentration along the 1:1 line (black line). 

#a)IC masec

scatter_plots_IC_masec = 
  lapply(seq_along(res_IC), function(x){
    plot(sim = res_IC[[x]]$sim, obs = res_IC[[x]]$obs, 
         type = "scatter", verbose = FALSE, 
         var = SticsRFiles:::var_to_col_names(plotting_var_masec))$all_situations+
      ggplot2::ggtitle(names(res_IC)[[x]])
  })
names(scatter_plots_IC_masec) = names(res_IC)

#b) IC mafruit

scatter_plots_IC_mafruit = 
  lapply(seq_along(res_IC), function(x){
    plot(sim = res_IC[[x]]$sim, obs = res_IC[[x]]$obs, 
         type = "scatter", verbose = FALSE, 
         var = SticsRFiles:::var_to_col_names(plotting_var_mafruit))$all_situations+
      ggplot2::ggtitle(names(res_IC)[[x]])
  })
names(scatter_plots_IC_mafruit) = paste0(names(res_IC), "_", plotting_var_mafruit)

#c) IC Grain N concentration (CNgrain)
plotting_var_CNgrain = "CNgrain"

scatter_plots_IC_CNgrain = 
  lapply(seq_along(res_IC), function(x){
    plot(sim = res_IC[[x]]$sim, obs = res_IC[[x]]$obs, 
         type = "scatter", verbose = FALSE, 
         var = SticsRFiles:::var_to_col_names(plotting_var_CNgrain))$all_situations+
      ggplot2::ggtitle(names(res_IC)[[x]])
  })
names(scatter_plots_IC_CNgrain) = paste0(names(res_IC), "_", plotting_var_CNgrain)

#Figure 8: Simulated versus observed a) in season total aboveground biomass 
#partial land equivalent ratio (LER), b) total N uptake partial LER, and 
#c) grain yield partial LER along the 1:1 line for intercropped spring barley 
#and spring pea and intercropped winter wheat (purple triangle) and winter pea. 

#REMI, I am not sure if your code includes a shortcut for calculating LER. I
#didn't see one in main.R and the stats function does not seem to output LER.
#Am I missing something? Thanks.

# Saving the plots:
plots_all = 
  c(scatter_plots_SC_LAI,scatter_plots_SC_masec,scatter_plots_SC_QNplante,scatter_plots_SC_mafruit,
    scatter_plots_IC_pheno,dynamic_plots_Auz_lai,dynamic_plots_Ang_lai,dynamic_plots_Auz_hauteur,
    dynamic_plots_Ang_hauteur,scatter_plots_IC_masec,scatter_plots_IC_mafruit,scatter_plots_IC_CNgrain)

CroPlotR::plot_save(plots_all, path = plotdir)

CroPlotR::plot_save(dynamic_plots_Ang_lai, path = plotdir)
names(dynamic_plots_Ang_hauteur)

