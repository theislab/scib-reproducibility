#' Method palette
#'
#' Colour palette for methods
#'
#' @return Name vector of method colours
method_pal <- function() {
    c(
        "BBKNN"          = "#5A5156",
        "Conos"          = "#E4E1E3",
        "trVAE"          = "#F6222E",
        "scVI"           = "#FE00FA",
        "ComBat"         = "#16FF32",
        "Harmony"        = "#3283FE",
        "LIGER"          = "#FEAF16",
        "Scanorama"      = "#B00068",
        "Seurat v3 CCA"  = "#1CFFCE",
        "Seurat v3 RPCA" = "#90AD1C",
        "MNN"            = "#2ED9FF",
        "FastMNN"        = "#DEA0FD",
        "scGen*"         = "#AA0DFE",
        "scANVI*"        = "#F8A19F",
        "DESC"           = "#325A9B",
        "SAUCIE"         = "#C4451C",
        "Unintegrated"   = "#66B0FF"
    )
}

#' Dataset palette
#'
#' Colour palette for datasets
#'
#' @return Named vector of dataset colours
dataset_pal <- function() {
    c(
        "immune_cell_hum"                = "#e41a1c",
        "immune_cell_hum_mou"            = "#377eb8",
        "lung_atlas"                     = "#4daf4a",
        "mouse_brain"                    = "#984ea3",
        "pancreas"                       = "#ff7f00",
        "simulations_1_1"                = "#f781bf",
        "simulations_2"                  = "#a65628",
        "mini_sim"                       = "#f781bf",
        "mini_pancreas"                  = "#999999",
        "mouse_brain_atac_genes_large"   = "#1f78b4",
        "mouse_brain_atac_genes_small"   = "#a6cee3",
        "mouse_brain_atac_peaks_large"   = "#33a02c",
        "mouse_brain_atac_peaks_small"   = "#b2df8a",
        'mouse_brain_atac_windows_large' = "#ff7f00",
        "mouse_brain_atac_windows_small" = "#fdbf6f"
    )
}

#' Plot dataset overall
#'
#' Plot an overall summary scatter plot for a dataset
#'
#' @param metrics tibble containing metrics for a single dataset
#'
#' @return ggplot object
plot_dataset_overall <- function(metrics) {

    plot_data <- metrics %>%
        dplyr::filter(method != "Unintegrated")

    medians <- metrics %>%
        dplyr::summarise(
            batch_correction = median(batch_correction),
            bio_conservation = median(bio_conservation)
        ) %>%
        dplyr::mutate(type = "Median")

    ref_lines <- metrics %>%
        dplyr::filter(method == "Unintegrated") %>%
        dplyr::mutate(type = "Unintegrated") %>%
        dplyr::select(type, batch_correction, bio_conservation) %>%
        dplyr::bind_rows(medians)

    plot_overall(plot_data, ref_lines, method, method_pal())
}

#' Plot method overall
#'
#' Plot an overall summary scatter plot for a method
#'
#' @param metrics tibble containing metrics for a single metrics
#'
#' @return ggplot object
plot_method_overall <- function(metrics) {

    ref_lines <- metrics %>%
        dplyr::summarise(
            batch_correction = median(batch_correction),
            bio_conservation = median(bio_conservation)
        ) %>%
        dplyr::mutate(type = "Median")

    plot_overall(metrics, ref_lines, dataset, dataset_pal())
}

#' Plot overall
#'
#' Plot an overall scatter plot
#'
#' @param metrics tibble containing metrics for scenarios to be plotted
#' @param ref_lines tibble containing reference lines add
#' @param colour_by Name of the metrics column to colour points by
#' @param palette Named vector containing colours for the items in the colour
#' column
#'
#' @return ggplot object
plot_overall <- function(metrics, ref_lines, colour_by, palette) {

    plot_data <- metrics %>%
        dplyr::mutate(output_features = paste(output, features, sep = "-"))

    plot <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(
            x      = batch_correction,
            y      = bio_conservation,
            colour = {{ colour_by }},
            size   = overall,
            shape  = output_features,
        )
    )

    ref_lines <- ref_lines %>%
        dplyr::arrange(type) %>%
        dplyr::mutate(
            colour = dplyr::case_when(
                type == "Unintegrated" ~ "red",
                type == "Median"       ~ "blue",
                TRUE                   ~ "black"
            )
        )

    ref_layers <- purrr::pmap(ref_lines, function(...) {
            current <- tibble(...)

            hline <- ggplot2::geom_hline(
                    data = current,
                    ggplot2::aes(yintercept = bio_conservation,
                                 linetype = type),
                    colour = current$colour
            )
            vline <- ggplot2::geom_vline(
                data = current,
                ggplot2::aes(xintercept = batch_correction, linetype = type),
                colour = current$colour
            )

            return(list(hline = hline, vline = vline))
    })

    for (layer in ref_layers) {
        plot <- plot + layer$hline + layer$vline
    }

    plot <- plot +
        ggplot2::geom_point(stroke = 1, fill = "white") +
        ggplot2::geom_point(
            data = dplyr::filter(metrics, features == "Full"),
            ggplot2::aes(alpha = scaling),
            shape = 4, size = 1.5, colour = "white"
        ) +
        ggplot2::geom_point(
            data = dplyr::filter(metrics, features == "HVG"),
            ggplot2::aes(alpha = scaling),
            shape = 4, size = 1.5
        ) +
        ggplot2::scale_x_continuous(limits = c(0, 1)) +
        ggplot2::scale_y_continuous(limits = c(0, 1)) +
        ggplot2::scale_colour_manual(values = palette) +
        ggplot2::scale_size_continuous(range = c(0.5, 5), limits = c(0, 1),
                                       breaks = seq(0, 1, 0.2)) +
        ggplot2::scale_shape_manual(
            values = c(16, 21, 15, 22, 17, 24),
            labels = c("Embedding (Full)", "Embedding (HVG)", "Features (Full)",
                       "Features (HVG)", "Graph (Full)", "Graph (HVG)"),
            breaks = c("Embedding-Full", "Embedding-HVG", "Features-Full",
                       "Features-HVG", "Graph-Full", "Graph-HVG"),
            drop = FALSE
        ) +
        ggplot2::scale_alpha_manual(values = c(1, 0), drop = FALSE) +
        ggplot2::scale_linetype_manual(values = c(1, 5)) +
        ggplot2::coord_fixed() +
        ggplot2::labs(
            x = "Batch correction",
            y = "Bio conservation"
        ) +
        ggplot2::guides(
            colour = ggplot2::guide_legend(
                title          = stringr::str_to_sentence(
                    rlang::as_label(rlang::enquo(colour_by))
                ),
                title.position = "top",
                ncol           = 2,
                order          = 10
            ),
            shape = ggplot2::guide_legend(
                title          = "Output (features)",
                title.position = "top",
                ncol           = 2,
                byrow          = TRUE,
                order          = 20
            ),
            alpha = ggplot2::guide_legend(
                title          = "Scaling",
                title.position = "top",
                ncol           = 2,
                order          = 30
            ),
            size = ggplot2::guide_legend(
                title          = "Overall score",
                title.position = "top",
                direction      = "horizontal",
                nrow           = 1,
                label.position = "bottom",
                order          = 40
            ),
            linetype = ggplot2::guide_legend(
                title          = "",
                title.position = "top",
                ncol           = 1,
                override.aes   = list(colour = unique(ref_lines$colour)),
                order          = 90
            )
        ) +
        ggplot2::theme(
            legend.position  = "right",
            axis.text        = ggplot2::element_text(size = 7),
            panel.border     = ggplot2::element_rect(fill = NA)
        )

    return(plot)
}

#' Metric dataset barplot
#'
#' Plot a metrics barplot for a dataset
#'
#' @param metrics tibble of metrics for a single dataset
#' @param metric Name of the metric column to plot
#' @param label Label for the metric to plot
#'
#' @return ggplot object
metric_dataset_barplot <- function(metrics, metric, label) {

    ref_lines <- tibble::tribble(
                                                          ~Value,         ~Type,
        median(dplyr::pull(metrics, {{ metric }}), na.rm = TRUE),      "Median",
        dplyr::filter(metrics, method == "Unintegrated") %>%
            dplyr::pull({{ metric }}),                            "Unintegrated"
    )

    metric_barplot(metrics, {{ metric }}, full_method, method, ref_lines, label,
                   method_pal())
}

#' Metric method barplot
#'
#' Plot a metrics barplot for a method
#'
#' @param metrics tibble of metrics for a single method
#' @param metric Name of the metric column to plot
#' @param label Label for the metric to plot
#'
#' @return ggplot object
metric_method_barplot <- function(metrics, metric, label) {

    ref_lines <- tibble::tribble(
                                           ~Value,                   ~Type,
        median(dplyr::pull(metrics, {{ metric }}), na.rm = TRUE), "Median",
    )

    metrics <- dplyr::mutate(
        metrics,
        dataset_output = paste0(dataset, " (", output, ")")
    )

    metric_barplot(metrics, {{ metric }}, dataset_output, dataset, ref_lines,
                   label, dataset_pal())
}

#' Metric barplot
#'
#' Plot a barplot showing performance on a single metric
#'
#' @param metrics tibble containing metrics to plot
#' @param metric Name of the metric column to plot
#' @param group Name of the column containing groups for each bar
#' @param colour Name of the column to colour bars by
#' @param ref_lines tibble containing reference lines to plot
#' @param label Label for the metric column
#' @param palette Named vector of colours matching the items in `colour`
#'
#' @return ggplot object
metric_barplot <- function(metrics, metric, group, colour, ref_lines, label,
                           palette) {

    metric_str <- rlang::as_label(rlang::enquo(metric))
    colour_str <- rlang::as_label(rlang::enquo(colour))

    ref_lines <- ref_lines %>%
        dplyr::arrange(Type) %>%
        dplyr::mutate(
            colour = dplyr::case_when(
                Type == "Unintegrated" ~ "red",
                Type == "Median"       ~ "blue",
                TRUE                   ~ "black"
            )
        )

    plot <- ggplot2::ggplot(
        metrics,
        ggplot2::aes(
            x    = forcats::fct_reorder({{ group }}, {{ metric }}, .fun = max),
            y    = {{ metric }},
            fill = {{ colour }}
        )
    ) +
        ggplot2::geom_hline(
            data = ref_lines,
            ggplot2::aes(yintercept = Value, colour = Type, linetype = Type)
        ) +
        ggplot2::geom_col() +
        ggplot2::geom_point(
            ggplot2::aes(shape = output),
            size = 3.5, colour = "white",
            show.legend = c(fill = FALSE, shape = TRUE)
        ) +
        ggplot2::geom_text(
            data = dplyr::filter(metrics, is.na({{ metric }})),
            y = 0.5, label = "Not computed"
        ) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(values = palette, drop = FALSE) +
        ggplot2::scale_linetype_manual(values = c(1, 5)) +
        ggplot2::scale_colour_manual(values = c("blue", "red"), guide = FALSE) +
        ggplot2::scale_shape_manual(values = c(21, 22, 23), drop = FALSE) +
        ggplot2::labs(y = label) +
        ggplot2::guides(
            fill = ggplot2::guide_legend(
                title          = stringr::str_to_title(colour_str),
                title.position = "top",
                ncol           = 2,
                order          = 10
            ),
            shape = ggplot2::guide_legend(
                title          = "Output",
                title.position = "top",
                ncol           = 2,
                byrow          = TRUE,
                override.aes   = list(colour = "grey60", stroke = 1),
                order          = 20
            ),
            linetype = ggplot2::guide_legend(
                title          = "",
                title.position = "top",
                ncol           = 1,
                override.aes   = list(colour = unique(ref_lines$colour)),
                order          = 90
            )
        ) +
        ggplot2::theme(
            legend.position  = "right",
            axis.text        = ggplot2::element_text(size = 7),
            axis.title.y     = ggplot2::element_blank(),
            panel.border     = ggplot2::element_rect(fill = NA),
            strip.background = ggplot2::element_rect(fill = "black"),
            strip.text       = ggplot2::element_text(
                size = 10,
                colour = "white"
            )
        )

    if (any(!is.na(metrics[[metric_str]]))) {
        plot <- plot +
            ggplot2::scale_y_continuous(limits = c(0, 1)) +
            ggplot2::facet_grid(features ~ scaling)
    }

    plot
}

#' Plot embedding coordinates
#'
#' Plot an embedding scatter plot for a specific combination of dataset, input,
#' method and output.
#'
#' @param dataset String giving the name of the dataset to plot
#' @param scaling String giving the scaling to plot, either "Scaled" or
#' "Unscaled"
#' @param features String giving the features to plot, either "Full" or "HVG"
#' @param method String giving the method to plot
#' @param output String giving the output to plot, either "Features",
#' "Embedding" or "Graph"
#' @param labels List containing standard labels
#'
#' @details
#' Note that paths to CSV files containing embedding coordinates are hardcoded
#' through `get_coords_path()`
#'
#' @return List of ggplot objects, including "Group", "Batch" and any additional annotations
plot_embedding_coords <- function(dataset, scaling, features, method, output,
                                  labels) {

    `%>%` <- magrittr::`%>%`

    coords_path <- get_coords_path(dataset, scaling, features, method, output,
                                   labels)

    coords <- suppressMessages(suppressWarnings(
        readr::read_csv(coords_path)
    ))

    annot_path <- here::here(
        "..",
        "data",
        "annotations",
        paste0(dataset, ".csv")
    )

    if (fs::file_exists(annot_path)) {
        annot <- suppressMessages(suppressWarnings(
            readr::read_csv(annot_path)
        ))
        coords <- dplyr::left_join(coords, annot, by = "CellID")
        annot_cols <- colnames(annot)[-1]
    } else {
        annot_cols <- NULL
    }

    # Shuffle cells so the plot order is random
    withr::with_seed(1, {
        coords <- coords[sample(nrow(coords)), ]
    })

    group_name <- colnames(coords)[2]
    batch_name <- colnames(coords)[3]
    dim1_name  <- colnames(coords)[4]
    dim2_name  <- colnames(coords)[5]

    base_plot <- ggplot2::ggplot(
        coords,
        ggplot2::aes(x = .data[[dim1_name]], y = .data[[dim2_name]])
    ) +
        ggplot2::guides(
            colour = ggplot2::guide_legend(
                title        = NULL,
                ncol         = 3,
                override.aes = list(size = 2, alpha = 1)
            )
        ) +
        ggplot2::theme(
            plot.title      = ggplot2::element_text(hjust = 0.5, size = 20),
            legend.position = "bottom",
            legend.text     = ggplot2::element_text(lineheight = 0.75),
            panel.border    = ggplot2::element_rect(fill = NA)
        )

    wrap20 <- function(x) {
        stringr::str_wrap(x, width = 20)
    }

    group_plot <- base_plot +
        scattermore::geom_scattermore(
            ggplot2::aes(colour = factor(.data[[group_name]])),
            pointsize = 3,
            alpha     = 0.5,
            pixels    = c(1200, 1200)
        ) +
        ggplot2::labs(title = group_name)  +
        ggsci::scale_colour_d3("category20", labels = wrap20)

    batch_plot <- base_plot +
        scattermore::geom_scattermore(
            ggplot2::aes(colour = factor(.data[[batch_name]])),
            pointsize = 3,
            alpha     = 0.5,
            pixels    = c(1200, 1200)
        ) +
        ggplot2::labs(title = batch_name) +
        ggsci::scale_colour_ucscgb(labels = wrap20)

    annot_plots <- purrr::map(annot_cols, function(.annot) {
        base_plot +
            scattermore::geom_scattermore(
                ggplot2::aes(colour = factor(.data[[.annot]])),
                pointsize = 3,
                alpha     = 0.5,
                pixels    = c(1200, 1200)
            ) +
            ggplot2::labs(title = .annot) +
            ggsci::scale_colour_igv(labels = wrap20)
    })
    names(annot_plots) <- annot_cols

    c(list(Group = group_plot, Batch = batch_plot), annot_plots)
}

#' Get coords path
#'
#' Get the path to a specific embedding coordinated CSV file
#'
#' @param dataset String giving the name of the dataset to plot
#' @param scaling String giving the scaling to plot, either "Scaled" or
#' "Unscaled"
#' @param features String giving the features to plot, either "Full" or "HVG"
#' @param method String giving the method to plot
#' @param output String giving the output to plot, either "Features",
#' "Embedding" or "Graph"
#' @param labels List containing standard labels
#'
#' @return Path to embedding CSV file
get_coords_path <- function(dataset, scaling, features, method, output,
                            labels) {
    scaling <- stringr::str_to_lower(scaling)
    features <- dplyr::if_else(features == "Full", "full_feature", "hvg")
    method <- labels$methods[method]
    output <- dplyr::case_when(
        output == "Features"  ~ "full",
        output == "Embedding" ~ "embed",
        output == "Graph"     ~ "knn"
    )

    here::here(
        "..",
        "data",
        "embeddings",
        dataset,
        scaling,
        features,
        paste0(method, "_", output, ".csv")
    )
}

#' Benchmark barplot
#'
#' Plot a barplot for a scalability benchmark
#'
#' @param benchmarks tibble containing scalability benchmarks to plot
#' @param metric Name of the scalability metric to plot
#' @param group Name of the column containing groups for each bar
#' @param label Label for the plotted metric column
#' @param palette Named vector of colours matching the items in `group`
#'
#' @return ggplot object
benchmark_barplot <- function(benchmarks, metric, group, label, palette) {

    `%>%` <- magrittr::`%>%`

    group_str <- rlang::as_label(rlang::enquo(group))

    medians <- benchmarks %>%
        dplyr::group_by(features, scaling) %>%
        dplyr::summarise(median = median({{ metric }}), .groups = "drop")

    plot <- ggplot2::ggplot(
        benchmarks,
        ggplot2::aes(
            x    = forcats::fct_reorder({{ group }}, {{ metric }}, .fun = max),
            y    = {{ metric }},
            fill = {{ group }}
        )
    ) +
        ggplot2::geom_hline(
            data = medians,
            ggplot2::aes(yintercept = median, linetype = "Median"),
            colour = "blue"
        ) +
        ggplot2::geom_col() +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(values = palette, drop = FALSE) +
        ggplot2::labs(y = label) +
        ggplot2::guides(
            fill = ggplot2::guide_legend(
                title          = stringr::str_to_title(group_str),
                title.position = "top",
                ncol           = 2,
                order          = 10
            ),
            linetype = ggplot2::guide_legend(
                title          = "",
                title.position = "top",
                ncol           = 1,
                override.aes   = list(colour = "blue"),
                order          = 90
            )
        ) +
        ggplot2::theme(
            legend.position  = "right",
            axis.text        = ggplot2::element_text(size = 7),
            axis.title.y     = ggplot2::element_blank(),
            panel.border     = ggplot2::element_rect(fill = NA),
            strip.background = ggplot2::element_rect(fill = "black"),
            strip.text       = ggplot2::element_text(
                size = 10,
                colour = "white"
            )
        )

    if (any(!is.na(dplyr::pull(benchmarks, {{ metric }})))) {
        plot <- plot + ggplot2::facet_grid(features ~ scaling)
    }

    return(plot)
}
