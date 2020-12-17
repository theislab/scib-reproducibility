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
#' @param datasets_atac Vector of ATAC datasets
#' @param methods Vector of methods
#' @param methods_atac Vector of ATAC methods
#' @param outpath Path to output HTML files
#'
#' @return Writes the HTML to `outpath` but also return is invisibly
make_navbar_html <- function(datasets, datasets_atac, methods, methods_atac,
                             outpath) {

    tags <- htmltools::tags

    datasets_list <- purrr::map(datasets, function(.dataset) {
        tags$li(
            tags$a(
                href = paste0("dataset_", .dataset, ".html"),
                .dataset
            )
        )
    })

    datasets_atac_list <- purrr::map(datasets_atac, function(.dataset) {
        tags$li(
            tags$a(
                href = paste0("dataset_atac_", .dataset, ".html"),
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

    methods_atac_list <- purrr::map(methods_atac, function(.method) {
        tags$li(
            tags$a(
                href = paste0("method_atac_", .method, ".html"),
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
                    tags$li(tags$a(href = "overview.html", "Overview")),
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
                    tags$li(
                        class = "dropdown",
                        tags$a(
                            href            = "#",
                            class           = "dropdown-toggle",
                            `data-toggle`   = "dropdown",
                            role            = "button",
                            `aria-expanded` = "false",
                            "ATAC datasets",
                            tags$span(class = "caret")
                        ),
                        tags$ul(
                            class = "dropdown-menu",
                            role  = "menu",
                            datasets_atac_list
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
                            "ATAC methods",
                            tags$span(class = "caret")
                        ),
                        tags$ul(
                            class = "dropdown-menu",
                            role  = "menu",
                            methods_atac_list
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

    html_tags <- list(
        tags$link(
            href = "site_libs/font-awesome-5.1.0/css/all.css",
            rel  = "stylesheet"
        ),
        tags$link(
            href = "site_libs/font-awesome-5.1.0/css/v4-shims.css",
            rel  = "stylesheet"
        ),
        html_tags
    )

    html_rendered <- htmltools::renderTags(html_tags)$html

    writeLines(html_rendered, outpath)

    invisible(html_rendered)
}

#' Mark dataset embeddings markdown
#'
#' Produce markdown for displaying embedding plots for a dataset
#'
#' @param metrics tibble containing metrics for a single dataset
#' @param dataset String giving the name of the dataset
#' @param labels List containing standard labels
#'
#' @return List containing markdown strings
make_dataset_embeddings_md <- function(metrics, dataset, labels) {

    metrics <- dplyr::arrange(metrics, dplyr::desc(overall))

    full_src <- list()

    for (full_method in unique(metrics$full_method)) {
        split_method <- stringr::str_split(full_method, pattern = "-")
        method <- split_method[[1]][1]
        output <- split_method[[1]][2]
        full_src <- c(
            full_src,
            glue::glue(
                "## {method} ({output}) {{.unnumbered .tabset .tabset-pills ",
                ".tabset-fade}}"
            ),
            "**Input features (Scaling)**",
            ""
        )

        metrics_method <- dplyr::filter(metrics, full_method == !!full_method)
        full_src <- c(
            full_src,
            make_inner_embedding_md(metrics_method, dataset, method, output,
                                    labels, type = "dataset")
        )
    }

    return(full_src)
}

#' Mark method embeddings markdown
#'
#' Produce markdown for displaying embedding plots for a method
#'
#' @param metrics tibble containing metrics for a single method
#' @param method String giving the name of the method
#' @param labels List containing standard labels
#'
#' @return List containing markdown strings
make_method_embeddings_md <- function(metrics, method, labels) {

    metrics <- dplyr::arrange(metrics, dplyr::desc(overall))
    metrics$dataset_output <- paste(metrics$dataset, metrics$output, sep = "-")

    full_src <- list()

    for (dataset_output in unique(metrics$dataset_output)) {
        split_dataset <- stringr::str_split(dataset_output, pattern = "-")
        dataset <- split_dataset[[1]][1]
        output <- split_dataset[[1]][2]
        full_src <- c(
            full_src,
            glue::glue(
                "## {dataset} ({output}) {{.unnumbered .tabset .tabset-pills ",
                ".tabset-fade}}"
            ),
            "**Input features (Scaling)**",
            ""
        )

        metrics_dataset <- dplyr::filter(
            metrics, dataset_output == !!dataset_output
        )
        full_src <- c(
            full_src,
            make_inner_embedding_md(metrics_dataset, dataset, method, output,
                                    labels, type = "method")
        )
    }

    return(full_src)
}

#' Mark inner embeddings markdown
#'
#' Produce markdown for displaying embedding plots by looping over input
#' features and scaling
#'
#' @param metrics tibble containing selected metrics
#' @param dataset String giving the name of the dataset
#' @param method String giving the name of the method
#' @param output String giving the method output type
#' @param labels List containing standard labels
#' @param type Whether the embeddings are for a "dataset", or a "method"
#'
#' @return List containing markdown strings
make_inner_embedding_md <- function(metrics, dataset, method, output, labels,
                                    type = c("dataset", "method")) {

    type <- match.arg(type)

    top_label <- switch (type,
        dataset = method,
        method  = dataset
    )

    full_src <- list()
    for (features in sort(unique(metrics$features))) {

        metrics_sel <- dplyr::filter(metrics, features == !!features)
        src_list <- purrr::map_chr(
            sort(unique(metrics_sel$scaling)),
            function(.scaling) {
                inner_label <- glue::glue(
                    "embedding-{top_label}-{output}-{features}-{.scaling}"
                )
                height <- get_embedding_rows(dataset) * 7
                src <- c(
                    "### <<features>> (<<.scaling>>) {.unnumbered}",
                    "```{r <<inner_label>>, fig.height = <<height>>}",
                    "plots <- plot_embedding_coords('<<dataset>>',",
                    "'<<.scaling>>', '<<features>>', '<<method>>',",
                    "'<<output>>', labels)",
                    "wrap_plots(plots, ncol = 2)",
                    "```",
                    ""
                )
                knitr::knit_expand(text = src, delim = c("<<", ">>"))
            }
        )
        full_src <- c(full_src, src_list)
    }

    return(full_src)
}

#' Get embedding rows
#'
#' Get the number of rows for an embedding plot (two plots per row)
#'
#' @param dataset Name of the dataset to check for.
#'
#' @details
#' Checks if there is an additional annotation file for the dataset.
#' If so head a sample of the file and use the number of annotation columns to calculate the number of rows.
#'
#' @return Number of rows
get_embedding_rows <- function(dataset) {

    annot_path <- here::here(
        "..",
        "data",
        "annotations",
        paste0(dataset, ".csv")
    )

    if (!fs::file_exists(annot_path)) {
        return(1)
    }

    annot <- suppressMessages(suppressWarnings(
        readr::read_csv(annot_path, n_max = 100)
    ))

    return(ceiling((ncol(annot) + 1) / 2))
}
