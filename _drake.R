#==============================================================================#
# ---- LIBRARIES ----
#==============================================================================#

library("drake")
library("here")
library("tidyverse")

library("htmltools")

#==============================================================================#
# ---- FUNCTIONS ----
#==============================================================================#

# See https://github.com/rstudio/gt/issues/297#issuecomment-497778735
callr_render <- function(input, output_file) {
    callr::r(
        function(...) rmarkdown::render(...),
        args = list(input = input, output_file = output_file)
    )
}

#==============================================================================#
# ---- PLAN ----
#==============================================================================#

plan <- drake_plan(
    configs = list(
        site = read_lines(here(file_in("pages/_site.yml")))
    ),
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
