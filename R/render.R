#' callr render
#'
#' Call `rmarkdown::render()` inside a new **callr** environment
#'
#' @param input input to render
#' @param output_file patr to output file
#'
#' @details
#' Some packages (including **htmltools**) mess with the random seed which
#' **drake** doesn't like. Wrapping them with **callr** is one solution. See
#' https://github.com/rstudio/gt/issues/297#issuecomment-497778735.
#'
#' @return path to the output file
callr_render <- function(input, output_file, params = NULL) {
    callr::r(
        function(...) {rmarkdown::render(...)},
        args = list(input = input, output_file = output_file, params = params)
    )
}

make_site_yaml <- function(outpath, datasets, methods) {

    `%>%` <- magrittr::`%>%`

    datasets_list <- purrr::map(datasets, function(.dataset) {
        ymlthis::navbar_page(.dataset, paste0("dataset_", .dataset, ".html"))
    })

    methods_list <- purrr::map(methods, function(.method) {
        ymlthis::navbar_page(.method, paste0("method_", .method, ".html"))
    })

    site_yml <- ymlthis::yml_empty() %>%
        ymlthis::yml_site_opts(
            name       = "scIB-results",
            output_dir = "../docs"
        ) %>%
        ymlthis::yml_navbar(
            title = "scIB Results",
            type = "default",
            left = list(
                ymlthis::navbar_page("Home", "index.html"),
                ymlthis::navbar_page("Datasets", menu = datasets_list),
                ymlthis::navbar_page("Methods", menu = methods_list)
            ),
            right = list(
                # ymlthis::navbar_page("About", "about.html"),
                ymlthis::navbar_page(
                    icon = "fa-github fa-lg",
                    href = "https://github.com/theislab/scIB-results"
                )
            )
        ) %>%
        ymlthis::yml_output(
            rmarkdown::html_document(
                theme           = "yeti",
                highlight       = "textmate",
                number_sections = TRUE,
                toc             = TRUE,
                toc_float       = TRUE,
                css             = "style.css",
                self_contained  = FALSE,
                lib_dir         = "../docs/site_libs"
            )
        )

    if (fs::file_exists(outpath)) {
        fs::file_delete(outpath)
    }

    ymlthis::use_site_yml(site_yml, fs::path_dir(outpath))

    invisible(site_yml)
}
