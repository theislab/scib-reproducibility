make_dataset_table <- function(metrics, labels) {

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

    score_column <- function(maxWidth = 80, class = "cell number", ...) {
        colDef(maxWidth = maxWidth, align = "center", class = class, ...)
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
                    name        = "Method",
                    headerStyle = list(fontWeight = 700),
                    filterable  = TRUE
                ),
                output = reactable::colDef(
                    name        = "Output",
                    headerStyle = list(fontWeight = 700),
                    filterable  = TRUE
                ),
                scaling = reactable::colDef(
                    name        = "Scaling",
                    headerStyle = list(fontWeight = 700),
                    filterable  = TRUE
                ),
                features = reactable::colDef(
                    name        = "Features",
                    headerStyle = list(fontWeight = 700),
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

    htmltools::div(class = "metrics",
        tbl
    )
}

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
