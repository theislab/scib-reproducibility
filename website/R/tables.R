#' Make overall table
#'
#' Make a an interactive HTML table displaying all the metric scores for a
#' dataset or method.
#'
#' @param metrics tibble containing metrics to show
#' @param labels List of standard labels
#' @param type Whether the table is showing a `"dataset"` or `"method"`
#'
#' @return List containing HTML
make_overall_table <- function(metrics, labels, type = c("dataset", "method")) {

    `%>%` <- magrittr::`%>%`

    type <- match.arg(type)

    overall_cell <- make_score_cell_func(
        c("#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0",
          "#225ea8")
    )
    batch_cell <- make_score_cell_func(
        c("#fff7f3", "#fde0dd", "#fcc5c0", "#fa9fb5", "#f768a1", "#dd3497",
          "#ae017e")
    )
    bio_cell <- make_score_cell_func(
        c("#fff7ec", "#fee8c8", "#fdd49e", "#fdbb84", "#fc8d59", "#ef6548",
          "#d7301f")
    )

    # Replace method with dataset if we are making a method table
    if (type == "method") {
        metrics <- dplyr::mutate(metrics, method = dataset)
    }

    tbl <- metrics %>%
        dplyr::select(-scenario, -dataset, -input, -full_method) %>%
        dplyr::relocate(method, output, features, scaling) %>%
        reactable::reactable(
            pagination = FALSE,
            defaultSorted = "overall",
            defaultSortOrder = "desc",
            defaultColDef = reactable::colDef(
                class       = "cell",
                headerClass = "header"
            ),
            defaultColGroup = reactable::colGroup(headerClass = "group-header"),
            columnGroups = list(
                reactable::colGroup(
                    name    = "Batch correction",
                    columns = unname(labels$metrics$batch)
                ),
                reactable::colGroup(
                    name    = "Bio conservation",
                    columns = unname(labels$metrics$bio)
                )
            ),
            columns = list(
                method = reactable::colDef(
                    name = dplyr::if_else(
                        type == "dataset",
                        "Method",
                        "Dataset"
                    ),
                    width       = 100,
                    class       = "sticky",
                    headerClass = "sticky header",
                    style       = list(left = 0),
                    headerStyle = list(left = 0, fontWeight = 700),
                    filterable  = TRUE
                ),
                output = reactable::colDef(
                    name        = "Output",
                    width       = 100,
                    class       = "sticky",
                    headerClass = "sticky header",
                    style       = list(left = 100),
                    headerStyle = list(left = 100, fontWeight = 700),
                    filterable  = TRUE
                ),
                features = reactable::colDef(
                    name        = "Features",
                    width       = 80,
                    class       = "sticky",
                    headerClass = "sticky header",
                    style       = list(left = 200),
                    headerStyle = list(left = 200, fontWeight = 700),
                    filterable  = TRUE
                ),
                scaling = reactable::colDef(
                    name        = "Scaling",
                    width       = 80,
                    class       = "sticky border-right",
                    headerClass = "sticky header border-right",
                    style       = list(left = 280),
                    headerStyle = list(left = 280, fontWeight = 700),
                    filterable  = TRUE
                ),
                overall = score_column(
                    name        = "Overall score",
                    headerStyle = list(fontWeight = 700),
                    cell        = overall_cell,
                    class       = "cell number border-left"
                ),
                batch_correction = score_column(
                    name  = "Overall",
                    cell  = batch_cell,
                    class = "cell number border-left"
                ),
                PCR_batch = score_column(
                    name = "PCR batch",
                    cell = batch_cell
                ),
                ASW_label_batch = score_column(
                    name = "Batch ASW",
                    cell = batch_cell
                ),
                iLISI = score_column(
                    name = "Graph\niLISI",
                    cell = batch_cell,
                ),
                graph_connectivity = score_column(
                    name = "Graph connectivity",
                    cell = batch_cell,
                ),
                kBET = score_column(
                    name = "kBET",
                    cell = batch_cell
                ),
                bio_conservation = score_column(
                    name  = "Overall",
                    cell  = bio_cell,
                    class = "cell number border-left",
                ),
                NMI_cluster_label = score_column(
                    name = "NMI cluster/label",
                    cell = bio_cell,
                ),
                ARI_cluster_label = score_column(
                    name = "ARI cluster/label",
                    cell = bio_cell
                ),
                ASW_label = score_column(
                    name = "Label ASW",
                    cell = bio_cell
                ),
                isolated_label_F1 = score_column(
                    name = "Isolated label F1",
                    cell = bio_cell
                ),
                isolated_label_silhouette = score_column(
                    name = "Isolated label silhouette",
                    cell = bio_cell
                ),
                cLISI = score_column(
                    name = "Graph cLISI",
                    cell = bio_cell
                ),
                HVG_overlap = score_column(
                    name = "HVG conservation",
                    cell = bio_cell
                ),
                cell_cycle = score_column(
                    name = "Cell cycle conservation",
                    cell = bio_cell
                ),
                trajectory = score_column(
                    name = "Trajectory conservation",
                    cell = bio_cell
                )
            ),
            compact      = TRUE,
            highlight    = TRUE,
            showSortIcon = FALSE,
            borderless   = TRUE,
            class        = "metrics-table"
        )

    htmltools::div(class = "metrics", tbl)
}

#' Make papers table
#'
#' Make an interactive HTML table showing the usability scores for papers.
#'
#' @param usability_papers tibble of paper usability scores
#'
#' @return List of HTML
make_papers_table <- function(usability_papers) {

    `%>%` <- magrittr::`%>%`

    papers_cell <- make_score_cell_func(
        c("#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0",
          "#225ea8")
    )

    tbl <- usability_papers %>%
        reactable::reactable(
            pagination = FALSE,
            defaultSorted = "Overall",
            defaultSortOrder = "desc",
            defaultColDef = reactable::colDef(
                class       = "cell",
                headerClass = "header"
            ),
            defaultColGroup = reactable::colGroup(headerClass = "group-header"),
            columnGroups = list(
                reactable::colGroup(
                    "Evaluation of accuracy",
                    columns = c("AccuracyDatasets", "AccuracySimulation")
                )
            ),
            columns = list(
                Method = reactable::colDef(
                    name        = "Method",
                    class       = "sticky border-right",
                    headerClass = "sticky header border-right",
                    style       = list(left = 0),
                    headerStyle = list(left = 0, fontWeight = 700)
                ),
                DOI = reactable::colDef(
                    name        = "DOI",
                    headerStyle = list(fontWeight = 700),
                    width       = 210
                ),
                Overall = score_column(
                    name        = "Overall",
                    headerStyle = list(fontWeight = 700),
                    cell        = papers_cell,
                    class       = "cell number border-left",
                    width       = 90
                ),
                PeerReview = score_column(
                    name        = "Peer-reviewed",
                    headerStyle = list(fontWeight = 700),
                    cell        = papers_cell,
                    class       = "cell number border-left",
                    width       = 90
                ),
                AccuracyDatasets = score_column(
                    name  = "Datasets",
                    cell  = papers_cell,
                    class = "cell number border-left",
                    width = 80
                ),
                AccuracySimulation = score_column(
                    name  = "Simulations",
                    cell  = papers_cell,
                    width = 110
                ),
                Robustness = score_column(
                    name        = "Robustness",
                    headerStyle = list(fontWeight = 700),
                    cell        = papers_cell,
                    class       = "cell number border-left",
                    width       = 120
                ),
                Benchmarking = score_column(
                    name        = "Benchmarking",
                    headerStyle = list(fontWeight = 700),
                    cell        = papers_cell,
                    class       = "cell number border-left",
                    width       = 130
                )
            ),
            compact      = TRUE,
            highlight    = TRUE,
            showSortIcon = FALSE,
            borderless   = TRUE,
            class        = "metrics-table"
        )

    htmltools::div(class = "metrics", tbl)
}

#' Make packages
#'
#' Make an interactive HTML showing usability scores for packages.
#'
#' @param usability_packages tibble containing package usability scores
#'
#' @return List of HTML
make_packages_table <- function(usability_packages) {

    `%>%` <- magrittr::`%>%`

    packages_cell <- make_score_cell_func(
        c("#fff7f3", "#fde0dd", "#fcc5c0", "#fa9fb5", "#f768a1", "#dd3497",
          "#ae017e")
    )

    tbl <- usability_packages %>%
        reactable::reactable(
            pagination = FALSE,
            defaultSorted = "Overall",
            defaultSortOrder = "desc",
            defaultColDef = reactable::colDef(
                class       = "cell",
                headerClass = "header"
            ),
            defaultColGroup = reactable::colGroup(headerClass = "group-header"),
            columnGroups = list(
                reactable::colGroup(
                    "Open source",
                    columns = c("OpenCode", "OpenPlatform")
                ),
                reactable::colGroup(
                    "Tutorials",
                    columns = c("HasTutorial", "TutorialErrors",
                                "TutorialScenarios", "TutorialNonNative")
                ),
                reactable::colGroup(
                    "Function documentation",
                    columns = c("FunctionPurpose", "FunctionParameters",
                                "FunctionOutput")
                ),
                reactable::colGroup(
                    "GitHub issues",
                    columns = c("IssueActivityScore", "IssueResponseScore")
                )
            ),
            columns = list(
                Package = reactable::colDef(
                    name        = "Package",
                    width       = 100,
                    class       = "sticky",
                    headerClass = "sticky header",
                    style       = list(left = 0),
                    headerStyle = list(left = 0, fontWeight = 700),
                ),
                Method = reactable::colDef(
                    name        = "Method",
                    class       = "sticky border-right",
                    headerClass = "sticky header border-right",
                    style       = list(left = 100),
                    headerStyle = list(left = 100, fontWeight = 700),
                ),
                Repo = reactable::colDef(
                    name        = "Repository",
                    headerStyle = list(fontWeight = 700),
                    width       = 190
                ),
                Overall = score_column(
                    name        = "Overall",
                    headerStyle = list(fontWeight = 700),
                    cell        = packages_cell,
                    class       = "cell number border-left",
                    width       = 80,
                ),
                OpenCode = score_column(
                    name  = "Code",
                    cell  = packages_cell,
                    class = "cell number border-left",
                    width = 60
                ),
                OpenPlatform = score_column(
                    name  = "Platform",
                    cell  = packages_cell,
                    width = 80
                ),
                VersionControl = score_column(
                    name        = "Version control",
                    headerStyle = list(fontWeight = 700),
                    cell        = packages_cell,
                    class       = "cell number border-left",
                    width       = 80,
                ),
                UnitTests = score_column(
                    name        = "Unit tests",
                    headerStyle = list(fontWeight = 700),
                    cell        = packages_cell,
                    class       = "cell number border-left",
                    width       = 90,
                ),
                HasTutorial = score_column(
                    name  = "Has tutorial",
                    cell  = packages_cell,
                    class = "cell number border-left",
                    width = 80
                ),
                TutorialErrors = score_column(
                    name  = "Errors",
                    cell  = packages_cell,
                    width = 70
                ),
                TutorialScenarios = score_column(
                    name  = "Scenarios",
                    cell  = packages_cell,
                    width = 90
                ),
                TutorialNonNative = score_column(
                    name  = "Non-native",
                    cell  = packages_cell,
                    width = 90
                ),
                FunctionPurpose = score_column(
                    name  = "Purpose",
                    cell  = packages_cell,
                    class = "cell number border-left",
                    width = 80
                ),
                FunctionParameters = score_column(
                    name  = "Parameters",
                    cell  = packages_cell,
                    width = 100
                ),
                FunctionOutput = score_column(
                    name  = "Output",
                    cell  = packages_cell,
                    width = 70
                ),
                IssueActivityScore = score_column(
                    name  = "Activity",
                    cell  = packages_cell,
                    class = "cell number border-left",
                    width = 80
                ),
                IssueResponseScore = score_column(
                    name  = "Response",
                    cell  = packages_cell,
                    width = 90
                )
            ),
            compact      = TRUE,
            highlight    = TRUE,
            showSortIcon = FALSE,
            borderless   = TRUE,
            class        = "metrics-table"
        )

    htmltools::div(class = "metrics", tbl)
}

#' Make colour palette
#'
#' Create a function that converts values to colours
#'
#' @param colours Vector of colours to form the palette
#' @param bias Controls the separation of colours, see [`colorRamp()`]
#'
#' @return Function that takes a value and returns a colour
make_colour_pal <- function(colours, bias = 1) {

    get_color <- colorRamp(colours, bias = bias)

    function(x) {
        ifelse(
            is.na(x),
            "white",
            rgb(get_color(x), maxColorValue = 255)
        )
    }
}

#' Make score cell function
#'
#' Create a function for specifying a coloured table cell
#'
#' @param colours Vector of colours to use
#'
#' @return Function that takes a value and returns a HTML div with a specific
#' background colour
make_score_cell_func <- function(colours) {

    colour_pal <- make_colour_pal(colours)

    function(value) {
        color <- colour_pal(value)
        value <- format(round(value, 2), nsmall = 2)
        htmltools::div(
            class = "score",
            style = list(background = color),
            value
        )
    }
}

#' Score column
#'
#' Wrapper around [`reactable::colDef()`] that sets some default options.
#'
#' @param maxWidth Maximum width of the column
#' @param class Class for the column div
#' @param ... Additional arguments passed to [`reactable::colDef()`]
#'
#' @return A **reactable** column definition object
score_column <- function(maxWidth = 80, class = "cell number", ...) {
    reactable::colDef(maxWidth = maxWidth, align = "center", class = class, ...)
}
