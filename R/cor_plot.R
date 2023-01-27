# create a correlation plot (add it direct to shiny)

# pass the loaded DF to the reactive value
numeric_corr_plot <- function(dataframe) {

  num_only <- dataframe |>
    dplyr::select_if(is.numeric)

  testRes = corrplot::cor.mtest(num_only, conf.level = 0.95)

  corrplot::corrplot(cor(num_only), p.mat = testRes$p,
                     method = 'circle', type = 'lower', insig='blank',
                     order = 'AOE', diag = FALSE,)$corrPos -> corr_val
  text(corr_val$x, corr_val$y, round(corr_val$corr, 2))

}


