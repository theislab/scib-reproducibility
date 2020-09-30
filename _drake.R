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

plan <- drake_plan(
    configs = list(
        site = readr::read_lines(here(file_in("pages/_site.yml"))),
        setup = readr::read_lines(here(file_in("R/document_setup.R")))
    ),
    labels = get_labels(),
    metrics = get_metrics(here(file_in("data/metrics.csv")), labels),
    dataset = target(
        callr_render(
            here(knitr_in("pages/dataset.Rmd")),
            here(file_out("docs/dataset.html"))
        ),
        trigger = trigger(change = configs)
    )
)

#==============================================================================#
# ---- CONFIG ----
#==============================================================================#

drake_config(plan, verbose = 2)
