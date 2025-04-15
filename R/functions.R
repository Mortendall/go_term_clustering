#' Prepare GO network
#'
#' @return an igraph reference network

prepare_network <- function(){
    #prepare network
    flatNet <- as.list(GO.db::GOBPCHILDREN)
    allNodes <- unique(c(names(flatNet), unlist(flatNet)))
    flatNet <- flatNet[!is.na(flatNet)]
    onto2eg <- as.list(org.Mm.eg.db::org.Mm.egGO2ALLEGS)
    allNodes <- allNodes[allNodes %in% names(onto2eg)]
    flatNet <- flatNet[names(flatNet) %in% allNodes]
    flatNet <- reshape2::melt(flatNet) |>
        dplyr::rename(from = value,
                      to = L1) |>
        dplyr::mutate(from = as.character(from),
                      to = as.character(to))
    graphNet <- igraph::graph_from_data_frame(flatNet)
    ontoNames <- as.list(GO.db::GOTERM)
    ontoNames <- sapply(ontoNames, AnnotationDbi::Term)
    igraph::V(graphNet)$ontoTerm <- ontoNames[igraph::V(graphNet)$name]
    return(graphNet)
}


#' Generate file list for analysis
#'
#' @param folder destination folder for xlsx files
#' @param sheet_number sheet to read in excel file
#'
#' @return a list of files to read

data_list <- function(folder,sheet_number, graphNet){
    file_list <- fs::dir_ls(folder,
                            regexp = ".xlsx$")

    data_list <- vector(mode = "list", length = length(file_list))

    data_list <- purrr::map(file_list,
                            ~readxl::read_xlsx(.x,sheet = sheet_number))

    names(data_list) <- stringr::str_extract(file_list,
                                             pattern = "(?<=_)[:alpha:]+(?=.xlsx)")

    data_list <- purrr::map(data_list,
                            ~dplyr::filter(.x,
                                           FDR<0.05) |>
                                dplyr::filter(ID %in% V(graphNet)$name))
    return(data_list)



}

#' Cluster GO terms'
#' @param file_list a list of files to read
#' @param graphNet an igraph network of GO terms
#'
#' @return an igraph subgraph

create_subgraphs <- function(data_list,graphNet){


    connectedSubgraphs <- purrr::map(data_list,
                                     ~igraph::all_shortest_paths(
                                         graphNet,
                                         from = .x$ID,
                                         to = .x$ID,
                                         mode = "all"))

    connectedSubgraphs <- purrr::map(connectedSubgraphs,~.x[["res"]])
    connectedSubgraphs<- purrr::map(connectedSubgraphs,
                                    ~unique(names(unlist(.x))))

        ontoNetSubgraph <- purrr::map(connectedSubgraphs,
                                      ~igraph::induced_subgraph(graphNet, .x))
        return(ontoNetSubgraph)


}

#' Add direction to clustering results
#'
#' @param clustering_result
#' @param data_list
#'
#' @return a clustering list with added direction

add_direction <- function(clustering_result, data_list){
    direction <- purrr::map(data_list,~dplyr::select(.x, ID,
                                                     Direction))

    clustering_result <- purrr::map(clustering_result,
                        ~dplyr::mutate(.x, order = dplyr::row_number()))

    joined_clustering <- purrr::map2(clustering_result,
                                     direction,
                                     ~dplyr::left_join(.x, .y, by = c("ontoID"="ID"))
    )
    joined_clustering <- purrr::map(joined_clustering,
                                    ~dplyr::mutate(.x, Direction =
                                                       dplyr::case_when(
                                                           is.na(Direction)~"non-significant",
                                                           .default = Direction
                                                       )) |>
                                        dplyr::arrange(order) |>
                                        dplyr::select(-order))
    return(joined_clustering)

}

#' Cluster GO terms
#'
#' @param ontoNetSubgraph an igraph subgraph
#' @param graphNet a reference network
#'
#' @return a list with results

cluster_go_terms <- function(ontoNetSubgraph, graphNet, significant_go_terms){
    set.seed(429)
    ontoClustCommunity <- purrr::map(ontoNetSubgraph,
                                     ~ leidenAlg::find_partition(igraph::as_undirected(.x),
                                                                 edge_weights = rep(1, ecount(.x)),
                                                                 resolution = .6) +1)


    ontoClust <- purrr::map2(ontoClustCommunity,
                             ontoNetSubgraph,
                             ~tibble::tibble(membership = .x,
                                             names = igraph::V(.y)$name))

    clusterTerm <- purrr::map(ontoClust,
                              ~sapply(unique(.x$membership), function(y){

                                  y <- .x$names[.x$membership == y]

                                  ySub <- igraph::induced_subgraph(graphNet, y)

                                  set.seed(429)
                                  yMax <- igraph::centr_eigen(ySub)$vector
                                  yTerm <- igraph::V(graphNet)$ontoTerm[match(y[which.max(yMax)],
                                                                              igraph::V(graphNet)$name)]


                                  return(yTerm)

                              }))

    clusterTerm <- purrr::map2(clusterTerm,
                               ontoClust,
                               ~purrr::set_names(.x, unique(.y$membership)))


    names(clusterTerm) <- unique(ontoClust$membership)

    ontoClust <- purrr::map2(ontoClust,
                                    clusterTerm,
                                    ~data.frame(clusterNumber = .x$membership,
                                                clusterTerm = .y[as.character(.x$membership)],
                                                ontoID = .x$names,
                                                ontoTerm = igraph::V(graphNet)$ontoTerm[match(.x$names,
                                                                                              igraph::V(graphNet)$name)]))

    cols <- purrr::map(ontoClust,
                       ~ggsci::pal_igv()(max(.x$clusterNumber)))
    set.seed(429)
    cols <- purrr::map2(ontoClust,
                        cols,
                        ~sample(.y, max(.x$clusterNumber), replace = F))
    cols <- purrr::map2(ontoClust,
                        cols,
                        ~.y[.x$clusterNumber])
    ontoClust <- purrr::map2(ontoClust,
                             cols,
                             ~dplyr::mutate(.x, color = .y)
    )
    ontoClust <- purrr::map2(ontoClust,
                             significant_go_terms,
                             ~dplyr::mutate(.x, color = dplyr::case_when(
                                 ontoID %in% .y$ID ~ color,
                                 .default = "#808080"
                             )))

     ontoClust <- add_direction(ontoClust,
                                significant_go_terms)


    return(ontoClust)

}

#' prepare_plots
#'
#' @param ontoClust clustering result
#' @param ontoNetSubgraph subgraphs
#' @param significant_go_terms data frames with significant go terms
#'
#' @return subgraphs with updated vertex attributes

prepare_plot <- function(ontoClust, ontoNetSubgraph){




    add_vertex_attributes <- function(subgraph, clustering_result){
        igraph::vertex_attr(subgraph) <- list(name = clustering_result$ontoID,
                                              color = clustering_result$color,
                                              clusterTerm = clustering_result$clusterTerm,
                                              label.color = clustering_result$color,
                                              ontoTerm = clustering_result$ontoTerm)
        return(subgraph)
    }

    ontoNetSubgraph <- purrr::map2(ontoNetSubgraph,
                                   ontoClust,
                                   ~add_vertex_attributes(.x,.y))

    return(ontoNetSubgraph)
}

#' Make ggraph graphs
#'
#' @param ontoClust clustering result
#' @param ontoNetSubgraph subgraph
#'
#' @return a list of ggraph graphs

make_plots <- function(ontoClust, ontoNetSubgraph){
    set.seed(429)
    color_guides <- purrr::map(ontoClust,
                               ~dplyr::filter(.x,
                                              color != "#808080") |>
                                   dplyr::distinct(color, .keep_all = T) |>
                                   dplyr::select(color, clusterTerm))

    #some clusters may have no significant targets. They get annotation too


    plot_list <- purrr::map2(ontoNetSubgraph,
                             ontoClust,
                             ~ ggraph::ggraph(.x, layout = "nicely")+
                                 ggraph::geom_edge_link0(width = 0.2, colour = "grey") +
                                 ggraph::geom_node_point(col = .y$color,
                                                         aes(fill = .y$clusterTerm),
                                                         size = ifelse(.y$color=="#808080", 0,5)
                                                          # ,
                                                          # shape = dplyr::case_when(
                                                          #     .y$Direction == "Up"~ 24,
                                                          #     .y$Direction == "Down"~25,
                                                          #     .default = 21
                                                          # )
                                                         ))

    color_mismatch <- purrr::map2(color_guides,
                            ontoClust,~dplyr::filter(.y,
                                                     !clusterTerm%in% .x$clusterTerm) |>
                                dplyr::distinct(clusterTerm, .keep_all = T) |>
                                dplyr::select(color, clusterTerm))
    color_guides <- purrr::map2(color_guides,
                                color_mismatch,
                                ~if(!is.null(.y)){(dplyr::mutate(.y, color = "#808080") |>
                                    dplyr::rows_append(.x))})


    plot_list <- purrr::map2(plot_list,
                             color_guides,
                             ~.x +
                                 scale_fill_manual(
                                     values = .y$color,
                                     labels = .y$clusterTerm,
                                     name = "cluster",
                                     guide = "legend"
                                 )+
                                 guides(fill = guide_legend(override.aes = list(size = 5,
                                                                                shape = 21)))+
                                 #ggraph::theme_graph(base_family = "Arial") +
                                 theme(legend.text = element_text(size = 12),
                                       panel.background = element_rect(fill='transparent'),
                                       panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank(),
                                       plot.background = element_rect(fill='transparent', color=NA),
                                       legend.background = element_rect(fill='transparent')
                                       ))
    plot_names <- names(plot_list)
    plot_list <- purrr::map2(plot_list,
                             plot_names,
                             ~.x+ggplot2::ggtitle(.y)+ggplot2::theme(title = ggplot2::element_text(size  =14)))

    return(plot_list)
}

#' save object as PDF
#'
#' @param plot_object targets list of plots
#' @param plot_title title of plot
#'
#' @return a PDF

print_figures <- function(plot_file, figure_title){

    design_layout <- "
111222
333444
555666
777###
"
    patchworktest <-
        patchwork::wrap_plots(plot_file[[1]]) +
        plot_file[[2]] +
        plot_file[[3]] +
        plot_file[[4]] +
        plot_file[[5]] +
        plot_file[[6]] +
        plot_file[[7]]+
        patchwork::plot_layout(design = design_layout)+
        patchwork::plot_annotation(tag_levels = "A")&
        ggplot2::theme(plot.tag = ggplot2::element_text(size = 16))

    patchworktest <- patchworktest + patchwork::plot_annotation(title = figure_title,
                                                                theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 16)))
    return(patchworktest)
    # grDevices::pdf(here::here(paste("data/",figure_title,".pdf")), height = 20, width = 20)
    # patchworktest
    # dev.off()

}

