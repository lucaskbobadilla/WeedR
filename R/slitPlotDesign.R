# Splitplot design
#library(tidyverse)

# Split Ploy DESIGN FUNCTION ----------------------------------------------
# Written by Lucas K. Bobadilla
# Written on May 25 2022
# Description: Create a split plotdesign object in the form of a dataframe

# plot function
splitPlot_plot <- function(df_design, plot_map){

  # generate treatments
  reps <- ncol(plot_map)
  treatments <- factor(as.vector(df_design |> dplyr::select(everything())  |> apply(2, rev)))
  treat_n <- length(levels(treatments))
  # generate plot data frame treatments
  x1 <- rep(1:(reps), each = treat_n)
  x2 <- x1 + 1

  y1 <- rep(1:treat_n, reps)
  y2 <- y1 + 1
  df <- tibble::tibble(x1,x2,y1,y2,treatments)



  sub_plot_n <- length(unique(unlist(as.vector(df_design))))
  # generate block sequences
  main_plot_regions_x <- NULL
  main_plot_regions_y <- NULL
  main_rep_names <- colnames(df_design)
  for (i in 1:reps) {
    main_plot_regions_x <- append(main_plot_regions_x,i+.5)
    main_plot_regions_y <- append(main_plot_regions_y, sub_plot_n+1.5)
  }



  #get block annotation
  blocks_ann <- tibble::tibble(x1 = main_plot_regions_x, y1 =main_plot_regions_y, main_rep_names)



  # generate plot for design
  plot_design <- df |>
    dplyr::mutate(plot =  as.vector(plot_map  |> apply(2, rev))) |>
    dplyr::full_join(blocks_ann, by = c("x1", "y1")) |>
    dplyr::mutate(treatments = factor(treatments)) |>
    ggplot2::ggplot(ggplot2::aes(.data$x1,.data$y1)) +
    ggplot2::geom_rect(mapping=ggplot2::aes(xmin=.data$x1, xmax=.data$x2,
                                            ymin=.data$y1, ymax=.data$y2,
                                            fill = .data$treatments),
                       alpha = 0.2, color = "black",
              na.rm = T)  +
    ggplot2::geom_text(ggplot2::aes(x=.data$x1+(.data$x2-.data$x1)/2,
                                    y=.data$y1+(.data$y2-.data$y1)/2,
                                    label= .data$plot), size=4,
              na.rm = T) +
    ggplot2::scale_fill_discrete(na.translate = F) +
    ggplot2::theme_void() +
    ggplot2::geom_text(ggplot2::aes(x = .data$x1,
                           y = .data$y1,
                           label = .data$main_rep_names), na.rm = TRUE) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))


  return(plot_design)
}

# main function
splitPlotDesign <- function(subplot, main_plot,
                            main_plot_reps = 2, seed = NA,
                            subplot_blocks = NA,
                            random_main_plot = FALSE){


  # Save the old random seed and use the new one, if present
  if (!is.na(seed)) {
    if (exists(".Random.seed"))  { saved.seed <- .Random.seed }
    else                         { saved.seed <- NA }
    set.seed(seed)
  }

  # get number of reps
  main_n <- ifelse(length(main_plot) == 1, main_plot,length(main_plot))
  blocks_n <- main_plot_reps*main_n

  # create treatment lists if numeric value is provided
  if (length(main_plot) == 1 && is.numeric(main_plot)) {
    main_plot <- 1:main_plot
  }

  if (length(subplot) == 1 && is.numeric(subplot)) {
    subplot <- 1:subplot
  }


  # build design
  sb_df <- replicate(blocks_n, sample(subplot))
  main_df <- c(replicate(main_plot_reps, sample(main_plot)))

  if (random_main_plot == FALSE) {
    main_df <- c(t(replicate(main_plot_reps, main_plot)))
  }

  df_design <- data.frame(sb_df)
  colnames_df <- paste(main_df, paste0("rep", 1:(blocks_n/main_n)),sep = "_")
  colnames(df_design) <- colnames_df
  df_design <- tibble::tibble(df_design)

  # build experimental map
  plot_map <- matrix(101:(100+length(subplot)), nrow = length(subplot), ncol = blocks_n)
  for (i in 1:dim(plot_map)[2]) {
    if (i > 1) {
      plot_map[,i] <- plot_map[,i]+(100*(i-1))
    }
  }
  colnames(plot_map) <- colnames_df
  plot_map <- tibble::tibble(data.frame(plot_map))
  # create data frame with plot information
  a <- tidyr::pivot_longer(plot_map, cols = tidyselect::everything(), names_to = "Main_plot",
                    values_to = "plot")

  b <- df_design |>
    tidyr::pivot_longer(cols = tidyselect::everything(),
                        names_to = "Main_plot", values_to = "Sub_plot")


  # get tidy table
  plot_info_df <- a |> dplyr::mutate(Sub_plot = b$Sub_plot)

  # get plot
  plot_design <- splitPlot_plot(df_design, plot_map)

  plan_list <- list(design = df_design, plot_map = plot_map, experiment_book = plot_info_df, plot = plot_design)


  return(plan_list)

  # Restore the old random seed, if present
  if (!is.na(seed) && !is.na(saved.seed)) { .Random.seed <- saved.seed }
}



