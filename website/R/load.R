get_datasets <- function() {
    labels <- get_labels()
    metrics <- get_metrics(here::here("..", "data", "metrics.csv"), labels)
    sort(unique(as.character(metrics$dataset)))
}

get_methods <- function() {
    labels <- get_labels()
    metrics <- get_metrics(here::here("..", "data", "metrics.csv"), labels)
    sort(unique(as.character(metrics$method)))
}

get_metrics <- function(metrics_file, labels) {

    `%>%` <- magrittr::`%>%`

    # Read in the file with specified column names/types
    readr::read_csv(
        metrics_file,
        col_names = c(
            "path",
            "NMI_cluster_label",
            "ARI_cluster_label",
            "ASW_label",
            "ASW_label_batch",
            "PCR_batch",
            "cell_cycle",
            "isolated_label_F1",
            "isolated_label_silhouette",
            "graph_connectivity",
            "kBET",
            "iLISI",
            "cLISI",
            "HVG_overlap",
            "trajectory"
        ),
        col_types = readr::cols(
            path                      = readr::col_character(),
            NMI_cluster_label         = readr::col_double(),
            ARI_cluster_label         = readr::col_double(),
            ASW_label                 = readr::col_double(),
            ASW_label_batch           = readr::col_double(),
            PCR_batch                 = readr::col_double(),
            cell_cycle                = readr::col_double(),
            isolated_label_F1         = readr::col_double(),
            isolated_label_silhouette = readr::col_double(),
            graph_connectivity        = readr::col_double(),
            kBET                      = readr::col_double(),
            iLISI                     = readr::col_double(),
            cLISI                     = readr::col_double(),
            HVG_overlap               = readr::col_double(),
            trajectory                = readr::col_double()
        ),
        skip = 1
    ) %>%
        # Split the path into the different parts of the scenario
        tidyr::separate(
            path,
            into = c("dataset", NA, "scaling", "features", "method"),
            sep = "/"
        ) %>%
        # Split method into method and output
        tidyr::separate(method, into = c("method", "output"), sep = "_") %>%
        # Set factors with pretty labels
        dplyr::mutate(
            dataset = factor(dataset),
            scaling = factor(
                scaling,
                levels = c("scaled", "unscaled"),
                labels = c("Scaled", "Unscaled")
            ),
            features = factor(
                features,
                levels = c("full_feature", "hvg"),
                labels = c("Full", "HVG")
            ),
            method = factor(
                method,
                levels = labels$methods,
                labels = names(labels$methods)
            ),
            output = factor(
                output,
                levels = c("full", "embed", "knn"),
                labels = c("Features", "Embedding", "Graph")
            )
        ) %>%
        # Create additional useful columns
        dplyr::mutate(
            input       = paste(dataset, scaling, features, sep = "-"),
            full_method = paste(method, output, sep = "-"),
            scenario    = paste(input, full_method, sep = "|")
        ) %>%
        # Rescale scores by dataset
        dplyr::group_by(dataset) %>%
        dplyr::mutate(
            dplyr::across(
                where(is.numeric),
                function (x) {
                    if (all(is.na(x))) {
                        return(x)
                    }
                    scales::rescale(x, to = c(0, 1))
                }
            )
        ) %>%
        dplyr::ungroup() %>%
        # Calculate overall scores
        dplyr::rowwise() %>%
        dplyr::mutate(
            batch_correction = mean(
                c(PCR_batch, ASW_label_batch, iLISI, graph_connectivity, kBET),
                na.rm = TRUE
            ),
            bio_conservation = mean(
                c(
                    NMI_cluster_label, ARI_cluster_label, ASW_label,
                    isolated_label_F1, isolated_label_silhouette, cLISI,
                    HVG_overlap, cell_cycle, trajectory
                ),
                na.rm = TRUE
            )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            overall = 0.4 * batch_correction + 0.6 * bio_conservation
        ) %>%
        # Reorder columns
        dplyr::relocate(
            scenario, input, dataset, scaling, features, full_method, method,
            output, overall, batch_correction, PCR_batch, ASW_label_batch, iLISI,
            graph_connectivity, kBET, bio_conservation, NMI_cluster_label,
            ARI_cluster_label, ASW_label, isolated_label_F1,
            isolated_label_silhouette, cLISI, HVG_overlap, cell_cycle, trajectory
        )
}

get_labels <- function() {
    list(
        methods = c(
            "MNN"          = "mnn",
            "Scanorama"    = "scanorama",
            "Seurat CCA"   = "seurat",
            "Harmony"      = "harmony",
            "BBKNN"        = "bbknn",
            "SAUCIE"       = "saucie",
            "trVAE"        = "trvae",
            "scVI"         = "scvi",
            "CONOS"        = "conos",
            "ComBat"       = "combat",
            "LIGER"        = "liger",
            "scGen"        = "scgen",
            "scANVI"       = "scanvi",
            "FastMNN"      = "fastmnn",
            "DESC"         = "desc",
            "Unintegrated" = "unintegrated"
        ),
        metrics = list(
            batch = c(
                "Batch correction"   = "batch_correction",
                "PCR batch"          = "PCR_batch",
                "Batch ASW"          = "ASW_label_batch",
                "Graph iLISI"        = "iLISI",
                "Graph connectivity" = "graph_connectivity",
                "kBET"               = "kBET"
            ),
            bio = c(
                "Bio conservation"          = "bio_conservation",
                "NMI cluster/label"         = "NMI_cluster_label",
                "ARI cluster/label"         = "ARI_cluster_label",
                "Label ASW"                 = "ASW_label",
                "Isolated label F1"         = "isolated_label_F1",
                "Isolated label silhouette" = "isolated_label_silhouette",
                "Graph cLISI"               = "cLISI",
                "HVG conservation"          = "HVG_overlap",
                "Cell cycle conservation"   = "cell_cycle",
                "Trajectory conservation"   = "trajectory"
            )
        )
    )
}

get_benchmarks <- function(benchmarks_file, labels) {

    `%>%` <- magrittr::`%>%`

    readr::read_csv(
        benchmarks_file,
        col_types = readr::cols(
            .default = readr::col_double(),
            scenario = readr::col_character(),
            h_m_s    = readr::col_time(format = "")
        ),
        na = "-"
    ) %>%
        dplyr::mutate(
            scenario = stringr::str_remove(scenario, "/R"),
            scenario = stringr::str_remove(scenario, ".h5ad"),
            scenario = stringr::str_remove(scenario, ".RDS")
        ) %>%
        # Split the path into the different parts of the scenario
        tidyr::separate(
            scenario,
            into = c("dataset", NA, "scaling", "features", "method"),
            sep = "/"
        ) %>%
        # Set factors with pretty labels
        dplyr::mutate(
            dataset = factor(dataset),
            scaling = factor(
                scaling,
                levels = c("scaled", "unscaled"),
                labels = c("Scaled", "Unscaled")
            ),
            features = factor(
                features,
                levels = c("full_feature", "hvg"),
                labels = c("Full", "HVG")
            ),
            method = factor(
                method,
                levels = labels$methods,
                labels = names(labels$methods)
            )
        )
}
