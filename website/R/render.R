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

#' Make navbar HTML
#'
#' Generate the HTML content for the site navigation bar
#'
#' @param datasets Vector of datasets
#' @param methods Vector of methods
#' @param outpath Path to output HTML files
#'
#' @return Writes the HTML to `outpath` but also return is invisibly
make_navbar_html <- function(datasets, methods, outpath) {

    tags <- htmltools::tags

    datasets_list <- purrr::map(datasets, function(.dataset) {
        tags$li(
            tags$a(
                href = paste0("dataset_", .dataset, ".html"),
                .dataset
            )
        )
    })

    methods_list <- purrr::map(methods, function(.method) {
        tags$li(
            tags$a(
                href = paste0("method_", .method, ".html"),
                .method
            )
        )
    })

    html_tags <- tags$div(
        class = "navbar navbar-default navbar-fixed-top",
        role  = "navigation",
        tags$div(
            class = "container",
            tags$div(
                class = "navbar-header",
                tags$button(
                    type          = "button",
                    class         ="navbar-toggle collapsed",
                    `data-toggle` = "collapse",
                    `data-target` = "#navbar",
                    tags$span(class = "icon-bar"),
                    tags$span(class = "icon-bar"),
                    tags$span(class = "icon-bar")
                ),
                tags$a(
                    class = "navbar-brand",
                    href  = "index.html",
                    "scIB Results")
            ),
            tags$div(
                id    = "navbar",
                class = "navbar-collapse collapse",
                tags$ul(
                    class = "nav navbar-nav",
                    tags$li(tags$a(href = "index.html", "Home")),
                    tags$li(
                        class = "dropdown",
                        tags$a(
                            href            = "#",
                            class           = "dropdown-toggle",
                            `data-toggle`   = "dropdown",
                            role            = "button",
                            `aria-expanded` = "false",
                            "Datasets",
                            tags$span(class = "caret")
                        ),
                        tags$ul(
                            class = "dropdown-menu",
                            role  = "menu",
                            datasets_list
                        )
                    ),
                    tags$li(
                        class = "dropdown",
                        tags$a(
                            href            = "#",
                            class           = "dropdown-toggle",
                            `data-toggle`   = "dropdown",
                            role            = "button",
                            `aria-expanded` = "false",
                            "Methods",
                            tags$span(class = "caret")
                        ),
                        tags$ul(
                            class = "dropdown-menu",
                            role  = "menu",
                            methods_list
                        )
                    ),
                    tags$li(tags$a(href = "usability.html", "Usability")),
                ),
                tags$ul(
                    class = "nav navbar-nav navbar-right",
                    tags$li(
                        tags$a(
                            href = "https://github.com/theislab/scIB-reproducibility",
                            tags$span(class = "fa fa-github fa-lg")
                        )
                    )
                )
            )
        )
    )

    html_rendered <- htmltools::renderTags(html_tags)$html

    writeLines(html_rendered, outpath)

    invisible(html_rendered)
}
