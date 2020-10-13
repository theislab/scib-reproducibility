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
    site = target(
        make_site_yaml(here(file_out("pages/_site.yml")), DATASETS, METHODS),
        trigger = trigger(change = list(DATASETS, METHODS)),
    ),
    setup = list(
        setup = readr::read_lines(here(file_in("R/document_setup.R")))
    ),
    configs = list(site, setup),
    css = fs::file_copy(
        here(file_in("pages/style.css")),
        here(file_out("../docs/style.css")),
        overwrite = TRUE
    ),
    labels = get_labels(),
    metrics = get_metrics(here(file_in("../data/metrics.csv")), labels),
    benchmarks = get_benchmarks(here(file_in("../data/benchmarks.csv")), labels),
    datasets_meta = get_datasets_meta(here(file_in("../data/datasets_meta.tsv"))),
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
                fig_dir = here("..", "docs", "figures", paste0("dataset_", dataset))
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
                fig_dir = here("..", "docs", "figures", paste0("method_", method))
            )
        ),
        transform = map(method = !!METHODS),
        trigger = trigger(change = configs)
    )
)

#==============================================================================#
# ---- CONFIG ----
#==============================================================================#

drake_config(plan, verbose = 1)
