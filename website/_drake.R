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

DATASETS <- get_datasets()
METHODS  <- get_methods()

plan <- drake_plan(
    navbar_content = target(
        make_navbar_html(
            DATASETS,
            METHODS,
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
    benchmarks = get_benchmarks(here(file_in("../data/benchmarks.csv")), labels),
    datasets_meta = get_datasets_meta(here(file_in("../data/datasets_meta.tsv"))),
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
                datasets = DATASETS,
                methods  = METHODS
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
