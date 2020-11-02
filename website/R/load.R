#' Get datasets
#'
#' Get a vector of dataset names for use by Drake
#'
#' @return Vector of dataset names
get_datasets <- function() {
    labels <- get_labels()
    metrics <- get_metrics(here::here("..", "data", "metrics.csv"), labels)
    sort(unique(as.character(metrics$dataset)))
}

#' Get methods
#'
#' Get a vector of method names for use by Drake
#'
#' @return Vector of method names
get_methods <- function() {
    labels <- get_labels()
    metrics <- get_metrics(here::here("..", "data", "metrics.csv"), labels)
    sort(unique(as.character(metrics$method)))
}

#' Get metrics
#'
#' Read the combined benchmarking metrics file. Also scales scores between
#' zero and one for each dataset and calculates overall scores for each dataset.
#'
#' @param metrics_file Path to the metrics CSV file
#' @param labels List containing standard labels
#'
#' @return tibble containing tidied metrics
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
        # Remove empty rows from failed runs
        dplyr::filter(!is.na(NMI_cluster_label)) %>%
        # Remove leading / if present
        dplyr::mutate(path = stringr::str_remove(path, "^/")) %>%
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
            isolated_label_silhouette, cLISI, HVG_overlap, cell_cycle,
            trajectory
        )
}

#' Get labels
#'
#' Get a list of standard labels to use in other functions
#'
#' @return list of standard labels
get_labels <- function() {
    list(
        methods = c(
            "BBKNN"          = "bbknn",
            "Conos"          = "conos",
            "trVAE"          = "trvae",
            "scVI"           = "scvi",
            "ComBat"         = "combat",
            "Harmony"        = "harmony",
            "LIGER"          = "liger",
            "Scanorama"      = "scanorama",
            "Seurat v3 CCA"  = "seurat",
            "Seurat v3 RPCA" = "seuratrpca",
            "MNN"            = "mnn",
            "FastMNN"        = "fastmnn",
            "scGen"          = "scgen",
            "scANVI"         = "scanvi",
            "DESC"           = "desc",
            "SAUCIE"         = "saucie",
            "Unintegrated"   = "unintegrated"
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

#' Get benchmarks
#'
#' Read the combined scalability benchmarks file.
#'
#' @param benchmarks_file Path to benchmarks CSV file
#' @param labels List containing standard labels
#'
#' @return tibble of tidied scalability benchmarks
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
        # Remove leading / if present
        dplyr::mutate(scenario = stringr::str_remove(scenario, "^/")) %>%
        # Split the path into the different parts of the scenario
        tidyr::separate(
            scenario,
            into = c("dataset", NA, "scaling", "features", "method"),
            sep = "/"
        ) %>%
        # Filter ATAC datasets
        dplyr::filter(!stringr::str_detect(dataset, "atac")) %>%
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

#' Get datasets metadata
#'
#' Read the datasets metadata file
#'
#' @param datasets_meta_file Path to datasets metadata TSV file
#'
#' @return tibble of datasets metadata
get_datasets_meta <- function(datasets_meta_file) {

    readr::read_tsv(
        datasets_meta_file,
        col_types = readr::cols(
            Name        = readr::col_character(),
            Description = readr::col_character(),
            Type        = readr::col_character(),
            Cells       = readr::col_double(),
            Batches     = readr::col_double(),
            Labels      = readr::col_double()
        )
    )
}

#' Get methods metadata
#'
#' Read the methods metadata file
#'
#' @param methods_meta_file Path to methods metadata TSV file
#'
#' @return tibble of methods metadata
get_methods_meta <- function(methods_meta_file) {

    readr::read_tsv(
        methods_meta_file,
        col_types       = readr::cols(
            Name        = readr::col_character(),
            Description = readr::col_character(),
            GitHub      = readr::col_character(),
            DOI         = readr::col_character(),
            Labels      = readr::col_character()
        )
    )
}

#' Get usability
#'
#' Read the method usability scores
#'
#' @param usability_papers_file Path to papers usability TSV file
#' @param usability_packages_file Path to packages usability TSV file
#' @param gh_stats_file Path to GitHub activity statistics TSV file
#'
#' @return list of tibbles with papers and packages usability scores
get_usability <- function(usability_papers_file, usability_packages_file,
                          gh_stats_file) {

    `%>%` <- magrittr::`%>%`

    usability_papers <- readr::read_tsv(
        usability_papers_file,
        col_types = readr::cols(
            .default = readr::col_double(),
            Method   = readr::col_character()
        )
    )

    gh_stats <- readr::read_tsv(
        gh_stats_file,
        col_types = readr::cols(
            .default = readr::col_double(),
            Repo     = readr::col_character(),
            Tool     = readr::col_character(),
            Created  = readr::col_datetime(format = ""),
            Updated  = readr::col_datetime(format = "")
        )
    ) %>%
        dplyr::select(Repo, IssueActivityScore, IssueResponseScore)

    usability_packages <- readr::read_tsv(
        usability_packages_file,
        col_types = readr::cols(
            .default = readr::col_double(),
            Package  = readr::col_character(),
            Method   = readr::col_character(),
            Repo     = readr::col_character()
        )
    ) %>%
        dplyr::left_join(gh_stats, by = "Repo")

    list(papers = usability_papers, packages = usability_packages)
}
