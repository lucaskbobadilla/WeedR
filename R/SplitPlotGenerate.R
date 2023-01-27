# Splitplot factors generation function ---------------------
# Written by Lucas K. Bobadilla
# Written on May 25 2022
# Description: Create a list of two vectors containing the main and subplots for

SplitPlotGenerate <- function(Sub, Main, SubPrefix = "subplot",
                              MainPrefix = "mainplot"){


  # if variables are vectors containing treatment names
  sub_plot <- Sub
  main_plot <- Main

  # if variables are integers
  if (typeof(Sub) == "double" && length(Sub) == 1) {
    sub_plot <- paste(SubPrefix, 1:Sub, sep = "_")
    sub_n <- Sub
  }

  if (typeof(Main) == "double" && length(Main) == 1) {
    main_plot <- paste(MainPrefix, 1:Main, sep = "_")
    main_n <- Main
  }



  return(list(subplot = sub_plot, main_plot = main_plot))
}


