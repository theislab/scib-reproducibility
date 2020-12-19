#==============================================================================#
# ---- LIBRARIES ----
#==============================================================================#

suppressPackageStartupMessages({
    # Package conflicts
    library("conflicted")
    # File paths
    library("fs")
    library("here")
    # Presentation
    library("knitr")
    library("patchwork")
    library("gt")
    library("glue")
    # Tidyverse
    library("tidyverse")
})

#==============================================================================#
# ---- CONFLICTS ----
#==============================================================================#

suppressMessages({
    conflict_prefer("filter", "dplyr")
})

#==============================================================================#
# ---- KNITR ----
#==============================================================================#

DOCNAME <- knitr::current_input()

knitr::knit_hooks$set(pngquant = knitr::hook_pngquant)

knitr::opts_chunk$set(
    autodep        = TRUE,
    cache          = FALSE,
    cache.comments = FALSE,
    echo           = FALSE,
    error          = FALSE,
    dev            = "ragg_png",
    fig.path       = paste0("figures/", DOCNAME, "/"),
    fig.align      = "center",
    fig.width      = 10,
    fig.height     = 8,
    dpi            = 120,
    message        = FALSE,
    warning        = FALSE,
    pngquant       = "--speed=1 --quality=0-50"
)

#==============================================================================#
# ---- ENVIRONMENT VARIABLES ----
#==============================================================================#

#==============================================================================#
# ---- FUNCTIONS ----
#==============================================================================#

#==============================================================================#
# ---- THEME ----
#==============================================================================#

theme_set(
    theme_minimal() +
        theme(
            axis.text = element_text(size = 10)
        )
)

#==============================================================================#
# ---- PATHS ----
#==============================================================================#

PATHS <- list(

)
