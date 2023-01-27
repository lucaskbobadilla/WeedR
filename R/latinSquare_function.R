### Latin Square function

#treatments <- c("A","B","C","D","E")
# # reps = 4
# # returnstrings <- T
# latinSquare(treatments = treatments, reps, returnstrings = F)
#latinSquare_design(treatments = 5, reps = 4)


latinSquare <- function(treatments, reps=1, seed=NA, returnstrings=FALSE) {

  # Save the old random seed and use the new one, if present
  if (!is.na(seed)) {
    if (exists(".Random.seed"))  { saved.seed <- .Random.seed }
    else                         { saved.seed <- NA }
    set.seed(seed)
  }

  # check if a number was provided as value for treatments
  if (typeof(treatments) == "character" && length(treatments) > 1) {

    # get the length of treatments
    len <- length(treatments)
  }

  if (is.numeric(treatments) && length(treatments) == 1) {
    treatments <- rep(1:treatments)
    treatments <- factor(treatments)
    len <- length(treatments)
  }




  # This matrix will contain all the individual squares
  allsq <- matrix(nrow=reps*len, ncol=len)

  # Store a string id of each square if requested
  if (returnstrings) {  squareid <- vector(mode = "character", length = reps) }

  # Get a random element from a vector (the built-in sample function annoyingly
  #   has different behavior if there's only one element in x)
  sample1 <- function(x) {
    if (length(x)==1) { return(x) }
    else              { return(sample(x,1)) }
  }

  # Generate each of n individual squares
  for (n in 1:reps) {

    # Generate an empty square
    sq <- matrix(nrow=len, ncol=len)

    # If we fill the square sequentially from top left, some latin squares
    # are more probable than others.  So we have to do it random order,
    # all over the square.
    # The rough procedure is:
    # - randomly select a cell that is currently NA (call it the target cell)
    # - find all the NA cells sharing the same row or column as the target
    # - fill the target cell
    # - fill the other cells sharing the row/col
    # - If it ever is impossible to fill a cell because all the numbers
    #    are already used, then quit and start over with a new square.
    # In short, it picks a random empty cell, fills it, then fills in the
    # other empty cells in the "cross" in random order. If we went totally randomly
    # (without the cross), the failure rate is much higher.
    while (any(is.na(sq))) {

      # Pick a random cell which is currently NA
      k <- sample1(which(is.na(sq)))

      i <- (k-1) %% len +1       # Get the row num
      j <- floor((k-1) / len) +1 # Get the col num

      # Find the other NA cells in the "cross" centered at i,j
      sqrow <- sq[i,]
      sqcol <- sq[,j]

      # A matrix of coordinates of all the NA cells in the cross
      openCell <-rbind( cbind(which(is.na(sqcol)), j),
                        cbind(i, which(is.na(sqrow))))
      # Randomize fill order
      openCell <- openCell[sample(nrow(openCell)),]

      # Put center cell at top of list, so that it gets filled first
      openCell <- rbind(c(i,j), openCell)
      # There will now be three entries for the center cell, so remove duplicated entries
      # Need to make sure it's a matrix -- otherwise, if there's just
      # one row, it turns into a vector, which causes problems
      openCell <- matrix(openCell[!duplicated(openCell),], ncol=2)

      # Fill in the center of the cross, then the other open spaces in the cross
      for (c in 1:nrow(openCell)) {
        # The current cell to fill
        ci <- openCell[c,1]
        cj <- openCell[c,2]
        # Get the numbers that are unused in the "cross" centered on i,j
        freeNum <- which(!(1:len %in% c(sq[ci,], sq[,cj])))

        # Fill in this location on the square
        if (length(freeNum)>0) { sq[ci,cj] <- sample1(freeNum) }
        else  {
          # Failed attempt - no available numbers
          # Re-generate empty square
          sq <- matrix(nrow=len, ncol=len)

          # Break out of loop
          break;
        }
      }
    }

    # Store the individual square into the matrix containing all squares
    allsqrows <- ((n-1)*len) + 1:len
    allsq[allsqrows,] <- sq

    # place treatment values to matrix
    for (i in 1:length(treatments)) {
      allsq[allsq == i] <- treatments[i]
      sq[sq == i] <- treatments[i]
    }

    # Store a string representation of the square if requested. Each unique
    # square has a unique string.
    if (returnstrings) { squareid[n] <- paste(sq, collapse="") }

  }




  # Restore the old random seed, if present
  if (!is.na(seed) && !is.na(saved.seed)) { .Random.seed <- saved.seed }

  if (returnstrings) { return(squareid) }
  else               { return(allsq) }}

latin_plot <- function(plot_df, plot_map){

  # generate treatments
  reps <- length(unique(plot_df$reps))
  treatments <- factor(plot_df$treatments)
  treat_n <- length(levels(treatments))*reps
  # generate plot data frame treatments
  x1 <- rep(1:(treat_n/reps), each = treat_n)
  x2 <- x1 + 1

  y1 <- rep(1:treat_n, treat_n/reps)
  y2 <- y1 + 1
  df <- tibble::tibble(x1,x2,y1,y2,treatments)

  # generate block sequences
  blocks <- seq(1:(treat_n/reps))
  block_regions_x <- NULL
  block_regions_y <- NULL
  block_names <- NULL
  for (i in blocks) {
    block_regions_x <- append(block_regions_x,i+.5)
    block_regions_y <- append(block_regions_y, treat_n+1.5)
    block_names <- append(block_names, paste("Column ",i, sep = ""))
  }

  #get block annotation
  blocks_ann <- tibble::tibble(x1 = block_regions_x, y1 =block_regions_y, block_names)



  # generate plot for design
  plot_design <- df |>
    dplyr::mutate(plots = as.vector(plot_map |>
                               dplyr::select(starts_with("Col")) |>
                               apply(2, rev))) |>
    dplyr::left_join(plot_df |>
                       dplyr::select(plots,reps), by = "plots") |>
    dplyr::full_join(blocks_ann, by = c("x1", "y1")) |>
    ggplot2::ggplot(ggplot2::aes(.data$x1,.data$y1)) +
    ggplot2::geom_rect(mapping=ggplot2::aes(xmin=.data$x1, xmax=.data$x2,
                                            ymin=.data$y1, ymax=.data$y2,
                                            fill = factor(.data$reps)),
                       alpha = 0.2, color = "black", na.rm = T) +
    ggplot2::geom_text(ggplot2::aes(x=.data$x1+(.data$x2-.data$x1)/2,
                           y=.data$y1+(.data$y2-.data$y1)/2,
                           label= .data$plots), size=4,
              na.rm = T) +
    ggplot2::theme(legend.position = "none",
          axis.title = ggplot2::element_blank()) +
    ggplot2::geom_text(ggplot2::aes(x = .data$x1,
                                    y = .data$y1,
                                    label = .data$block_names), na.rm = TRUE) +
    ggplot2::scale_fill_discrete(na.translate = F) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1,title = "Reps"))

  return(plot_design)
}

latinSquare_design <- function(treatments, reps=1, seed=NA, returnstrings=F) {


  # create the square
  square <- latinSquare(treatments, reps)
  square_dim <- dim(square)
  square_size <- prod(dim(square))/2

  # build tidy square design
  if (returnstrings == TRUE) {
    return(square)
  } else {
    new_square <- matrix(nrow = nrow(square), ncol = ncol(square))
    for (row in 1:nrow(square)) {
      for (col in 1:ncol(square)) {
        value <- square[row,col]
        new_square[row,col] <- value
      }
    }
    new_square <- tibble::as_tibble(new_square)
    names(new_square) <- gsub(x = names(new_square), pattern = "V", replacement = "Col_")
    new_square <- new_square  |>
      dplyr::mutate(row = rep(1:ifelse(length(treatments) == 1,treatments,length(treatments)), reps)) |>
      dplyr::select(row, everything())
    if (reps > 1) {
      new_square <- new_square |>
        dplyr::mutate(reps = rep(1:reps, each = ifelse(length(treatments) == 1,treatments,length(treatments))))
    }


    # create a plot number map
    seq_test <- seq(from = 100, to = 100*reps, by = 100)
    exp_map <- NULL
    len <- ifelse(length(treatments) == 1,treatments,length(treatments))
    for (i in 1:(len*len)){
      for (rep in seq_test){
        exp_map <- append(exp_map, rep+i)
      }
      exp_map <- sort(exp_map)
    }



    # convert to dataframe
    exp_map <- data.frame(matrix(exp_map,nrow = square_dim[1],ncol = square_dim[2], byrow = T))

    exp_map <- tibble::tibble(exp_map) |> dplyr::mutate(row = rep(1:len, reps)) |>
      dplyr::select(row, everything()) |>
      dplyr::mutate(reps = rep(1:reps, each = len))
    names(exp_map) <- gsub(x = names(exp_map), pattern = "X", replacement = "Col_")


    # create table with all information
    exp_information <- new_square |>
      tidyr::pivot_longer(cols = starts_with("Col_"), names_to = "Column",
                   values_to = "treatments") |>
      dplyr::left_join(exp_map |>
                  tidyr::pivot_longer(cols = tidyselect::starts_with("Col_"), names_to = "Column",
                               values_to = "plots"), by = c("row", "reps", "Column"))

    # create plot
    plot <-latin_plot(exp_information, exp_map)

    # return all tables
    return(list(experiment_plan = new_square,
                plot_map = exp_map,
                experiment_book = exp_information,
                plot = plot))

    # Restore the old random seed, if present
    if (!is.na(seed) && !is.na(saved.seed)) { .Random.seed <- saved.seed }

  }
}
