plots_1 <- targets::tar_read(print_plot_1)
ggsave(here::here("data/cold_vs_TN_with_dir.pdf"), plots_1, scale = 1.1,width = 20, height = 20)
plots_2 <- targets::tar_read(print_plot_2)
ggsave(here::here("data/acute_cold_TN_with_dir.pdf"), plots_2, scale = 1.1,width = 20, height = 20)
plots_3 <- targets::tar_read(print_plot_3)
ggsave(here::here("data/cold_tn_tn_with_dir.pdf"), plots_3, scale = 1.1,width = 20, height = 20)


#print plots
data_list_1 <- targets::tar_read(clusteringResults_1)
writexl::write_xlsx(data_list_1, here::here("data/cold_vs_TN.xlsx"))


data_list_2 <- targets::tar_read(clusteringResults_2)
writexl::write_xlsx(data_list_2, here::here("data/acute_vs_TN.xlsx"))


data_list_3 <- targets::tar_read(clusteringResults_3)
writexl::write_xlsx(data_list_3, here::here("data/cold_tn_tn.xlsx"))

testes_plot <- targets::tar_read(testes_plot)
ggsave(here::here("data/testes_plot.pdf"), testes_plot, scale = 2, width = 5, height = 5)

data_testes <- targets::tar_read(testes_clustering)
writexl::write_xlsx(data_testes, here::here("data/testes.xlsx"))

plots_1_high_res <- targets::tar_read(print_plot_1_high_res)
ggsave(here::here("data/cold_vs_TN_with_dir_high_res_1.pdf"), plots_1_high_res, scale = 1.1,width = 20, height = 20)
plots_2_high_res <- targets::tar_read(print_plot_2_high_res)
ggsave(here::here("data/acute_cold_TN_with_dir_high_res_1.pdf"), plots_2_high_res, scale = 1.1,width = 20, height = 20)

data_list_1_high_res <- targets::tar_read(clusteringResults_1_high_res)
writexl::write_xlsx(data_list_1_high_res, here::here("data/cold_vs_TN_high_res_1.xlsx"))


data_list_2_high_res <- targets::tar_read(clusteringResults_2_high_res)
writexl::write_xlsx(data_list_2_high_res, here::here("data/acute_vs_TN_high_res_1.xlsx"))
