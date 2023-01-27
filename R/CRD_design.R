# CRD DESIGN FUNCTION ----------------------------------------------
# Written by Lucas K. Bobadilla
# Written on May 25 2022
# Description: Create a CRD design object in the form of a list


CRD_design <- function(reps_n, treat, reps_as_col = TRUE,
                       plot_label = c(treatments, plots),
                       seed = NA, byID = TRUE) {

  # Save the old random seed and use the new one, if present
  if (!is.na(seed)) {
    if (exists(".Random.seed"))  { saved.seed <- .Random.seed }
    else                         { saved.seed <- NA }
    set.seed(seed)
  }

  # check if a number was provided as value for treat
  if (typeof(treat) == "character") {

    # generate randomization
    treatments <- rep(1:length(treat), reps_n)
    treatments <- factor(treatments)
    treat_length <- length(treat)

    # if user wants to specify by ID
    if (byID) {
      levels(treatments) <- treat
    }
  }

  if (is.numeric(treat) && length(treat) == 1) {
    treatments <- rep(1:treat, reps_n)
    treatments <- factor(treatments)
    treat_length <- treat
  }

  # create treatment matrix
  crd <- matrix(sample(treatments), nrow = treat_length, ncol = reps_n)

  #plot_map
  seq_test <- seq(from = 100, to = 100*reps_n, by = 100)
  plot <- NULL
  for (i in 1:treat_length){
    for (rep in seq_test){
      plot <- append(plot, rep+i)
    }
    plot <- sort(plot)
  }
  plot <- matrix(plot, nrow = treat_length, ncol = reps_n)



  #if reps as row
  if (reps_as_col == FALSE) {
    crd <-  matrix(sample(treatments), nrow = treat_length, ncol = reps_n, byrow = T)
    plot <- matrix(plot, nrow = treat_length, ncol = reps_n, byrow = T)

    #plot_map
    seq_test <- seq(from = 100, to = 100*treat_length, by = 100)
    plot <- NULL
    for (i in 1:reps_n){
      for (rep in seq_test){
        plot <- append(plot, rep+i)
      }
      plot <- sort(plot)
    }
    plot <- matrix(plot, nrow = treat_length, ncol = reps_n, byrow = T)
  }

  # convert to tibble
  crd <- tibble::tibble(data.frame(crd))
  plot <- tibble::tibble(data.frame(plot))

  names(crd) <- gsub(x = names(crd), pattern = "X", replacement = "Col_")
  names(plot) <- gsub(x = names(plot), pattern = "X", replacement = "Col_")

  # create experimental plan

  experiment_info <- crd |>
    dplyr::mutate(row = 1:treat_length) |>
    tidyr::pivot_longer(-row,names_to = "col",
                 values_to = "treatments") |>
    dplyr::full_join(plot |>
                       dplyr::mutate(row = 1:treat_length) |>
                       tidyr::pivot_longer(-row,names_to = "col", values_to = "plot"),
              by = c("row", "col"))

  plot_crd <- cr_plot(experiment_info, plot)

  # create final list
  crd_obj <- list(crd_treat = crd,
                  crd_map = plot ,
                  experiment_book = experiment_info,plot = plot_crd)



  #return matrix
  return(crd_obj)

  # Restore the old random seed, if present
  if (!is.na(seed) && !is.na(saved.seed)) { .Random.seed <- saved.seed }

}



