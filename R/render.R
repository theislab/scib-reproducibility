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
callr_render <- function(input, output_file) {
    callr::r(
        function(...) {rmarkdown::render(...)},
        args = list(input = input, output_file = output_file)
    )
}
