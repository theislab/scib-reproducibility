#' callr render
#'
#' Call `rmarkdown::render()` inside a new **callr** environment
#'
#' @param input Input to render
#' @param output_file Path to output file
#'
#' @details
#' Some packages (including **htmltools**) mess with the random seed which
#' **drake** doesn't like. Wrapping them with **callr** is one solution. See
#' https://github.com/rstudio/gt/issues/297#issuecomment-497778735.
#'
#' @return Path to the output file
callr_render <- function(input, output_file, params = NULL) {
    callr::r(
        function(...) {rmarkdown::render(...)},
        args = list(input = input, output_file = output_file, params = params)
    )
}

#' Make site YAML
#'
#' Creat the `_site.yml` file describing the website.
#'
#' @param outpath Path to the file `_site.yml` file
#' @param datasets Vector of datasets
#' @param methods Vector of methods
#'
#' @details
#' Loops over the methods and datasets and add them to the `_site.yml`. This
#' allows the navbar to be automatically populated when new methods/datasets are
#' added. Note that the navbar is still embedded in the file HTML files so every
#' file still needs to be rebuilt for the navbar to be updated.
#'
#' @return Writes the YAML to `outpath` but also returns it invisibly
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
            output_dir = "../../docs"
        ) %>%
        ymlthis::yml_navbar(
            title = "scIB Results",
            type = "default",
            left = list(
                ymlthis::navbar_page("Home", "index.html"),
                ymlthis::navbar_page("Datasets", menu = datasets_list),
                ymlthis::navbar_page("Methods", menu = methods_list),
                ymlthis::navbar_page("Usability", "usability.html")
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
                lib_dir         = "../../docs/site_libs"
            )
        )

    if (fs::file_exists(outpath)) {
        fs::file_delete(outpath)
    }

    ymlthis::use_site_yml(site_yml, fs::path_dir(outpath))

    invisible(site_yml)
}
