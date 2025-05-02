# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
extrafont::loadfonts()
library(tidyverse)
library(here)
library(ggraph)
library(patchwork)

# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
    packages = desc::desc_get_deps()$package[-1],
    format = "rds"
)

options(clustermq.scheduler = "multiprocess")

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  tar_target(name = gonetwork,
             command = prepare_network()
  ),
  tar_target(name = go_data_list_1,
             command = data_list(here::here("data-raw/"),1, gonetwork)),
  tar_target(name = subgraphs_1,
             command = create_subgraphs(go_data_list_1,
                                        gonetwork))
   ,
   tar_target(name = clusteringResults_1,
              command = cluster_go_terms(subgraphs_1,
                                         gonetwork,
                                         go_data_list_1,
                                         .6))
   ,
   tar_target(name = annotated_subgraphs_1,
              command =prepare_plot(clusteringResults_1,
                                    subgraphs_1)),
  tar_target(name = plots_1,
             command = make_plots(clusteringResults_1,
                                  annotated_subgraphs_1))
  ,
  tar_target(name = go_data_list_2,
             command = data_list(here::here("data-raw/"),2, gonetwork)),
  tar_target(name = subgraphs_2,
             command = create_subgraphs(go_data_list_2,
                                        gonetwork))
  ,
  tar_target(name = clusteringResults_2,
             command = cluster_go_terms(subgraphs_2,
                                        gonetwork,
                                        go_data_list_2,
                                        .6))
  ,
  tar_target(name = annotated_subgraphs_2,
             command =prepare_plot(clusteringResults_2,
                                   subgraphs_2)),
  tar_target(name = plots_2,
             command = make_plots(clusteringResults_2,
                                  annotated_subgraphs_2)),
  tar_target(name = go_data_list_3,
             command = data_list(here::here("data-raw/"),3, gonetwork)),
  tar_target(name = subgraphs_3,
             command = create_subgraphs(go_data_list_3,
                                        gonetwork))
  ,
  tar_target(name = clusteringResults_3,
             command = cluster_go_terms(subgraphs_3,
                                        gonetwork,
                                        go_data_list_3,
                                        .6))
  ,
  tar_target(name = annotated_subgraphs_3,
             command =prepare_plot(clusteringResults_3,
                                   subgraphs_3)),
  tar_target(name = plots_3,
             command = make_plots(clusteringResults_3,
                                  annotated_subgraphs_3)),
  tar_target(name = print_plot_1,
             command = print_figures(plots_1,
                                     "Cold_vs_TN")),
  tar_target(name = print_plot_2,
             command = print_figures(plots_2,
                                     "Acute_cold_TN")),
  tar_target(name = print_plot_3,
             command = print_figures(plots_3,
                                     "Cold_TN_vs_TN")),
  tar_target(name = testes_data,
             command = generate_testes_data("data-raw/testes/Biological_Process_testes.xlsx",
                                            gonetwork)),
  tar_target(name = testes_subgraph,
             command = make_testes_subgraph(testes_data,
                                       gonetwork)),
  tar_target(name = testes_clustering,
             command= testes_cluster(testes_subgraph,
                                        gonetwork,
                                        testes_data)),
  tar_target(name = testes_annotated,
             command = prepare_testes_plot(testes_clustering,
                                           testes_subgraph)),
  tar_target(name = testes_plot,
             command = make_testes_plot(testes_clustering,
                                        testes_annotated,
                                        "Testes")),
  tar_target(name = clusteringResults_1_high_res,
             command = cluster_go_terms(subgraphs_1,
                                        gonetwork,
                                        go_data_list_1,
                                        1))
  ,
  tar_target(name = annotated_subgraphs_1_high_res,
             command =prepare_plot(clusteringResults_1_high_res,
                                   subgraphs_1)),
  tar_target(name = plots_1_high_res,
             command = make_plots(clusteringResults_1_high_res,
                                  annotated_subgraphs_1_high_res)),
  tar_target(name = print_plot_1_high_res,
             command = print_figures(plots_1_high_res,
                                     "Cold_vs_TN high res")),
  tar_target(name = clusteringResults_2_high_res,
             command = cluster_go_terms(subgraphs_2,
                                        gonetwork,
                                        go_data_list_2,
                                        1))
  ,
  tar_target(name = annotated_subgraphs_2_high_res,
             command =prepare_plot(clusteringResults_2_high_res,
                                   subgraphs_2)),
  tar_target(name = plots_2_high_res,
             command = make_plots(clusteringResults_2_high_res,
                                  annotated_subgraphs_2_high_res)),
  tar_target(name = print_plot_2_high_res,
             command = print_figures(plots_2_high_res,
                                     "Acute_cold_TN high res"))


)
