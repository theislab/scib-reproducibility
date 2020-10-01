plot_dataset_overall <- function(metrics) {

    plot_data <- metrics %>%
        dplyr::filter(method != "Unintegrated") %>%
        dplyr::mutate(output_features = paste(output, features, sep = "-"))

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

    plot_overall(plot_data, ref_lines, method)
}

plot_method_overall <- function(metrics) {

    plot_data <- metrics %>%
        dplyr::mutate(output_features = paste(output, features, sep = "-"))

    ref_lines <- metrics %>%
        dplyr::summarise(
            batch_correction = median(batch_correction),
            bio_conservation = median(bio_conservation)
        ) %>%
        dplyr::mutate(type = "Median")

    plot_overall(plot_data, ref_lines, dataset)
}

plot_overall <- function(metrics, ref_lines, colour_by) {

    plot <- ggplot2::ggplot(
        metrics,
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
                    aes(yintercept = bio_conservation, linetype = type),
                    colour = current$colour
            )
            vline <- ggplot2::geom_vline(
                data = current,
                aes(xintercept = batch_correction, linetype = type),
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
            aes(alpha = scaling),
            shape = 4, size = 1.5, colour = "white"
        ) +
        ggplot2::geom_point(
            data = dplyr::filter(metrics, features == "HVG"),
            aes(alpha = scaling),
            shape = 4, size = 1.5
        ) +
        ggplot2::scale_x_continuous(limits = c(0, 1)) +
        ggplot2::scale_y_continuous(limits = c(0, 1)) +
        ggplot2::scale_colour_brewer(palette = "Paired") +
        ggplot2::scale_size_continuous(range = c(0.5, 5), limits = c(0, 1),
                                       breaks = seq(0, 1, 0.2)) +
        ggplot2::scale_shape_manual(
            values = c(16, 21, 15, 22, 17, 24),
            labels = c("Embedding (Full)", "Embedding (HVG)", "Features (Full)",
                       "Features (HVG)", "Graph (Full)", "Graph (HVG)")
        ) +
        ggplot2::scale_alpha_manual(values = c(1, 0)) +
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

metric_barplot <- function(metrics, metric, label) {
    metric_str <- rlang::as_label(rlang::enquo(metric))
    metric_vec <- metrics[[metric_str]]

    ref_lines <- tibble::tribble(
                                              ~Value,          ~Type,
                    median(metric_vec, na.rm = TRUE),       "Median",
        metric_vec[metrics$method == "Unintegrated"], "Unintegrated"
    )

    plot <- ggplot2::ggplot(
        metrics,
        ggplot2::aes(
            x = forcats::fct_reorder(full_method, {{ metric }}, .fun = max),
            y = {{ metric }},
            fill = method
        )
    ) +
        ggplot2::geom_hline(
            data = ref_lines,
            ggplot2::aes(yintercept = Value, colour = Type, linetype = Type)
        ) +
        ggplot2::geom_col() +
        ggplot2::geom_point(
            ggplot2::aes(shape = output),
            size = 4, colour = "white",
            show.legend = c(fill = FALSE, shape = TRUE)
        ) +
        ggplot2::geom_text(
            data = dplyr::filter(metrics, is.na({{ metric }})),
            y = 0.5, label = "Not computed"
        ) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_brewer(palette = "Paired") +
        ggplot2::scale_linetype_manual(values = c(1, 5)) +
        ggplot2::scale_colour_manual(values = c("blue", "red"), guide = FALSE) +
        ggplot2::scale_shape_manual(values = c(21, 22, 23)) +
        ggplot2::labs(y = label) +
        ggplot2::guides(
            fill = ggplot2::guide_legend(
                title          = "Method",
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
                override.aes   = list(colour = c("blue", "red")),
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

    if (any(!is.na(metric_vec))) {
        plot <- plot +
            ggplot2::scale_y_continuous(limits = c(0, 1)) +
            ggplot2::facet_grid(features ~ scaling)
    }

    plot
}

plot_embedding_coords <- function(dataset, scaling, features, method, output,
                                  labels) {

    `%>%` <- magrittr::`%>%`

    coords_path <- get_coords_path(dataset, scaling, features, method, output,
                                   labels)

    coords <- suppressMessages(suppressWarnings(
        readr::read_csv(coords_path
    )))

    group_name <- colnames(coords)[2]
    batch_name <- colnames(coords)[3]
    dim1_name  <- colnames(coords)[4]
    dim2_name  <- colnames(coords)[5]

    base_plot <- ggplot2::ggplot(
        coords,
        ggplot2::aes(x = .data[[dim1_name]], y = .data[[dim2_name]])
    ) +
        ggplot2::theme(
            plot.title      = ggplot2::element_text(hjust = 0.5, size = 20),
            legend.position = "bottom",
            panel.border    = ggplot2::element_rect(fill = NA)
        )

    group_plot <- base_plot +
        ggplot2::geom_point(ggplot2::aes(colour = .data[[group_name]])) +
        ggplot2::labs(title = group_name)  +
        ggplot2::scale_colour_hue(h = c(10, 170))

    batch_plot <- base_plot +
        ggplot2::geom_point(ggplot2::aes(colour = .data[[batch_name]])) +
        ggplot2::labs(title = batch_name) +
        ggplot2::scale_colour_hue(h = c(190, 350))

    group_plot + batch_plot
}

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
        "data",
        dataset,
        scaling,
        features,
        "figures",
        paste(method, output, "coords.csv", sep = "_")
    )
}
