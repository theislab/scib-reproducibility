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
    configs = list(
        site = readr::read_lines(here(file_in("pages/_site.yml"))),
        setup = readr::read_lines(here(file_in("R/document_setup.R")))
    ),
    labels = get_labels(),
    metrics = get_metrics(here(file_in("data/metrics.csv")), labels),
    rmd_dataset = target(
        callr_render(
            here(knitr_in("pages/dataset.Rmd")),
            here("docs", paste0("dataset_", dataset, ".html")),
            list(dataset = dataset)
        ),
        transform = map(dataset = !!DATASETS),
        trigger = trigger(change = configs)
    ),
    rmd_method = target(
        callr_render(
            here(knitr_in("pages/method.Rmd")),
            here("docs", paste0("method_", method, ".html")),
            list(method = method)
        ),
        transform = map(method = !!METHODS),
        trigger = trigger(change = configs)
    )
)

#==============================================================================#
# ---- CONFIG ----
#==============================================================================#

drake_config(plan, verbose = 1)
