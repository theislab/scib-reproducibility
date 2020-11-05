#==============================================================================#
# ---- LIBRARIES ----
#==============================================================================#

library("drake")
library("here")

#==============================================================================#
# ---- FUNCTIONS ----
#==============================================================================#

source(here("R", "render.R"))
source(here("R", "load.R"))
source(here("R", "plotting.R"))

#==============================================================================#
# ---- PLAN ----
#==============================================================================#

DATASETS <- get_datasets(here::here("..", "data", "metrics.csv"))
METHODS  <- get_methods(here::here("..", "data", "metrics.csv"))
DATASETS_ATAC <- get_datasets(here::here("..", "data", "metrics_atac.csv"))
DATASETS_ATAC <- DATASETS_ATAC[!grepl("3batches", DATASETS_ATAC)]
METHODS_ATAC <- get_methods(here::here("..", "data", "metrics_atac.csv"))

`%>%` <- magrittr::`%>%`

# Set TRUE for testing
if (FALSE) {
    DATASETS      <- "pancreas"
    METHODS       <- "scGen*"
    DATASETS_ATAC <- "mouse_brain_atac_genes_small"
    METHODS_ATAC  <- "trVAE"
}

plan <- drake_plan(
    navbar_content = target(
        make_navbar_html(
            DATASETS,
            DATASETS_ATAC,
            METHODS,
            METHODS_ATAC,
            here(file_out("../docs/navbar-content.html"))
        ),
        trigger = trigger(change = list(DATASETS, METHODS)),
    ),
    navbar = readr::read_lines(here(file_in("pages/_navbar.html"))),
    setup = readr::read_lines(here(file_in("R/document_setup.R"))),
    configs = list(setup, navbar),
    css = fs::file_copy(
        here(file_in("pages/style.css")),
        here(file_out("../docs/style.css")),
        overwrite = TRUE
    ),
    labels = get_labels(),
    metrics = get_metrics(here(file_in("../data/metrics.csv")), labels),
    metrics_atac = get_metrics(
        here(file_in("../data/metrics_atac.csv")),
        labels
    ) %>%
        dplyr::filter(
            stringr::str_detect(dataset, "3batches", negate = TRUE)
        ) %>%
        dplyr::mutate(dataset = factor(dataset)),
    benchmarks = get_benchmarks(here(file_in("../data/benchmarks.csv")), labels),
    benchmarks_atac = get_benchmarks(
        here(file_in("../data/benchmarks_atac.csv")), labels
    ),
    datasets_meta = get_datasets_meta(here(file_in("../data/datasets_meta.tsv"))),
    datasets_meta_atac = get_datasets_meta(
        here(file_in("../data/datasets_meta_atac.tsv"))
    ),
    methods_meta = get_methods_meta(here(file_in("../data/methods_meta.tsv"))),
    usability = get_usability(
        here(file_in("../data/usability_papers.tsv")),
        here(file_in("../data/usability_packages.tsv")),
        here(file_in("../data/gh_repo_summary.tsv"))
    ),
    rmd_index = target(
        callr_render(
            here(knitr_in("pages/index.Rmd")),
            here("..", "docs", "index.html"),
            list(
                datasets      = DATASETS,
                methods       = METHODS,
                datasets_atac = DATASETS_ATAC,
                methods_atac  = METHODS_ATAC
            )
        ),
        trigger = trigger(change = configs)
    ),
    rmd_dataset = target(
        callr_render(
            here(knitr_in("pages/dataset.Rmd")),
            here("..", "docs", paste0("dataset_", dataset, ".html")),
            list(
                dataset = dataset,
                fig_dir = here("..", "docs", "figures",
                               paste0("dataset_", dataset))
            )
        ),
        transform = map(dataset = !!DATASETS),
        trigger = trigger(change = configs)
    ),
    rmd_dataset_atac = target(
        callr_render(
            here(knitr_in("pages/dataset_atac.Rmd")),
            here("..", "docs", paste0("dataset_atac_", dataset, ".html")),
            list(
                dataset = dataset,
                fig_dir = here("..", "docs", "figures",
                               paste0("dataset_atac_", dataset))
            )
        ),
        transform = map(dataset = !!DATASETS_ATAC),
        trigger = trigger(change = configs)
    ),
    rmd_method = target(
        callr_render(
            here(knitr_in("pages/method.Rmd")),
            here("..", "docs", paste0("method_", method, ".html")),
            list(
                method = method,
                fig_dir = here("..", "docs", "figures",
                               paste0("method_", method))
            )
        ),
        transform = map(method = !!METHODS),
        trigger = trigger(change = configs)
    ),
    rmd_method_atac = target(
        callr_render(
            here(knitr_in("pages/method_atac.Rmd")),
            here("..", "docs", paste0("method_atac_", method, ".html")),
            list(
                method = method,
                fig_dir = here("..", "docs", "figures",
                               paste0("method_atac_", method))
            )
        ),
        transform = map(method = !!METHODS_ATAC),
        trigger = trigger(change = configs)
    ),
    rmd_usability = target(
        callr_render(
            here(knitr_in("pages/usability.Rmd")),
            here("..", "docs", "usability.html"),
        ),
        trigger = trigger(change = configs)
    ),
)

#==============================================================================#
# ---- CONFIG ----
#==============================================================================#

drake_config(plan, verbose = 1)
