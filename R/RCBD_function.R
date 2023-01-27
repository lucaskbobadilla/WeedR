
#' @importFrom rlang .data

# RCBD design function ------------------------

RCBD_design <- function(blocks_n, treat, plot_label = c("treatments", "plots"), seed = NA){

  # set number of replications and treatments
  plot_label <- plot_label

  # error messages
  if(length(treat) == 1 && treat == 0){
    stop("Treatment numbers must be different from 0.")
  }


  # Save the old random seed and use the new one, if present
  if (!is.na(seed)) {
    if (exists(".Random.seed"))  { saved.seed <- .Random.seed }
    else                         { saved.seed <- NA }
    set.seed(seed)
  }


  #generate treatments
  if(length(treat) > 1) {
    treat_n <- length(treat)
    treatments <- c()
    for (t in 1:blocks_n) {
      block <- sample(rep(1:treat_n))
      treatments <- append(treatments, block)
    }
    treatments <- factor(treatments)
    levels(treatments) <- treat
  } else {
    treat_n <- treat
    treatments <- c()
    for (t in 1:blocks_n) {
      block <- sample(rep(1:treat_n))
      treatments <- append(treatments, block)
      treatments <- factor(treatments)
    }
  }



  # generate plot data frame treatments
  x1 <- rep(1:blocks_n, each = treat_n)
  x2 <- x1 + 1

  y1 <- rep(1:treat_n, blocks_n)
  y2 <- y1 + 1
  df <- tibble::tibble(x1,x2,y1,y2,treatments)

  # generate plot number sequences
  seq_test <- seq(from = 100, to = 100*blocks_n, by = 100)
  plots <- NULL
  for (i in 1:treat_n){
    for (block in seq_test){
      plots <- append(plots, block+i)
    }
    plots <- sort(plots)
  }

  # create experimental tables
  experiment_table <- df  |>
    dplyr::top_n(n= treat_n*blocks_n, wt = treatments) |>
    dplyr::mutate(plots = plots,
           ID = seq(from = 1, to = treat_n*blocks_n, by = 1)) |>
    dplyr::select(ID,blocks = x1, plots, treatments)

  #save table
  experiment_plan <- experiment_table

  # generate block sequences
  blocks <- seq(1:blocks_n)
  block_regions_x <- NULL
  block_regions_y <- NULL
  block_names <- NULL
  for (i in blocks) {
    block_regions_x <- append(block_regions_x,i+.5)
    block_regions_y <- append(block_regions_y, treat_n+1.5)
    block_names <- append(block_names, paste("Block ",i, sep = ""))
  }

  #get block annotation
  blocks_ann <- tibble::tibble(x1 = block_regions_x, y1 =block_regions_y, block_names)


  # generate plot for design
  if(plot_label == "treatments") {
    df <- df |> dplyr::mutate(labels = treatments)
  } else {
    df <- df |> dplyr::mutate(labels = c(experiment_table$plots, rep(NA,nrow(df) - nrow(experiment_table))))
  }

  plot_design <- df |>
    dplyr::mutate(plots = c(experiment_table$plots, rep(NA,nrow(df) - nrow(experiment_table)))) |>
    dplyr::full_join(blocks_ann, by = c("x1", "y1")) |>
    ggplot2::ggplot(ggplot2::aes(.data$x1,.data$y1)) +
    ggplot2::geom_rect(mapping=ggplot2::aes(xmin=.data$x1, xmax=.data$x2,
                                     ymin=.data$y1, ymax=.data$y2,
                                     fill = .data$treatments), alpha = 0.2,
                         color = "black", na.rm = T) +
    ggplot2::geom_text(ggplot2::aes(x=.data$x1+(.data$x2-.data$x1)/2,
                           y=.data$y1+(.data$y2-.data$y1)/2,
                           label= .data$labels ), size=4,
              na.rm = T) +
    ggplot2::scale_fill_discrete(na.translate = F) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none",
          axis.title = ggplot2::element_blank()) +
    ggplot2::geom_text(ggplot2::aes(x = .data$x1, y = .data$y1, label = .data$block_names), na.rm = TRUE)  +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))

  # save plot
  plot_design <- plot_design

  # get final results
  experimental_design <- list(experiment_book = experiment_plan, block_ann = blocks_ann,
                              plot = plot_design)

  return(experimental_design)

  # Restore the old random seed, if present
  if (!is.na(seed) && !is.na(saved.seed)) { .Random.seed <- saved.seed }

}
