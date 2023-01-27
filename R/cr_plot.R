# library(tidyverse)
# library(ggmap)


# plot map refers to a dataframe containing the actual plot map
# plot df contains the tidy table of all experiment information



cr_plot <- function(plot_df, plot_map){

  # generate treatments
  reps <- ncol(plot_map)
  treatments <- factor(plot_df$treatments)
  treat_n <- length(levels(treatments))
  # generate plot data frame treatments
  x1 <- rep(1:(reps), each = treat_n)
  x2 <- x1 + 1

  y1 <- rep(1:treat_n, reps)
  y2 <- y1 + 1
  df <- tibble::tibble(x1,x2,y1,y2,treatments)

  # generate block sequences
  blocks <- seq(1:(reps))
  block_regions_x <- NULL
  block_regions_y <- NULL
  block_names <- NULL
  for (i in blocks) {
    block_regions_x <- append(block_regions_x,i+.5)
    block_regions_y <- append(block_regions_y, reps+1.5)
    block_names <- append(block_names, paste("Column ",i, sep = ""))
  }

  #get block annotation
  blocks_ann <- tibble::tibble(x1 = block_regions_x, y1 =block_regions_y, block_names)

  # generate plot for design
  plot_design <- df |>
    dplyr::mutate(plot = as.vector(unlist(as.vector(plot_map)))) |>
    dplyr::left_join(plot_df |> dplyr::select(plot,row), by = "plot") |>
    dplyr::full_join(blocks_ann, by = c("x1", "y1")) |>
    dplyr::mutate(treatments = factor(treatments)) |>
    ggplot2::ggplot(ggplot2::aes(.data$x1,.data$y1)) +
    ggplot2::geom_rect(mapping=ggplot2::aes(xmin=.data$x1, xmax=.data$x2,
                                   ymin= .data$y1, ymax=.data$y2,
                                   fill = .data$treatments),
                       alpha = 0.2, color = "black",
              na.rm = T)  +
    ggplot2::ylim(1,max(df$y1)+1) +
    ggplot2::geom_text(ggplot2::aes(x=.data$x1+(.data$x2-.data$x1)/2,
                                    y=.data$y1+(.data$y2-.data$y1)/2, label= .data$plot), size=4,
              na.rm = T) +
    ggplot2::scale_fill_discrete(na.translate = F) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))


  return(plot_design)
}



