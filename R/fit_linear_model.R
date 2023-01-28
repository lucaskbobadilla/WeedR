#### linear model fit function ############

# fit function

# f <- "Petal.Length ~ Species + Sepal.Length"
#
#  t <- fit_linear_model(dataframe = iris,
#                    formula = f,
#                    postHoc_variable = "Species",
#                    postHoc_test = "tukey", outliers_check = F)
# plot(t$model)
# ggplot2::ggsave("~/Downloads/test.png",t$fit_plot)

fit_linear_model <- function(dataframe, formula,
                             postHoc_variable = NULL,
                             postHoc_test = c("tukey", "LSD"),
                             outliers_check = FALSE,
                             xlab= "Response", ylab="Variable") {

  # fit model and null model
  vars <- gsub(" ","", unlist(strsplit(formula, "~")))
  response_var <- vars[1]

  f_lm <- lm(as.formula(formula), data = dataframe)
  f_lm_null <- lm(as.formula(paste(response_var, "1", sep = "~")), data = dataframe)

  if (outliers_check) {

    new_df <- remove_LM_outliers(f_lm)

    if(is.data.frame(new_df)){

      f_lm <- lm(as.formula(formula), data = new_df)
      f_lm_null <- lm(as.formula(paste(response_var, "1", sep = "~")), data = new_df)

    }

  }

  # get anova comparing null and full

  anov_null <- broom::tidy(anova(f_lm, f_lm_null))

  # get model summary

  mod_summary <- broom::tidy(f_lm)

  # get model fit plots

  mod_fit <- performance::check_model(f_lm)#ggfortify:::autoplot.lm(f_lm)

  # get model anova

  mod_anova <- broom::tidy(anova(f_lm))


  if(postHoc_test == "tukey"){
    posthoc_var <- postHoc_variable
    postHoc <- agricolae::HSD.test(f_lm, posthoc_var)
    postHoc_table <- postHoc$means |>
      tibble::rownames_to_column("Factor") |>
      dplyr::full_join(
        postHoc$groups |>
          tibble::rownames_to_column("Factor")
      ) |>
      dplyr::mutate_if(is.numeric, round, 2) |>
      tibble::tibble()

    colnames(postHoc_table) <- c("response", "variable", colnames(postHoc_table)[-c(1,2)])
  }

  if(postHoc_test == "LSD") {
    posthoc_var <- postHoc_variable
    postHoc <- agricolae::LSD.test(f_lm, posthoc_var)
    postHoc_table <- postHoc$means |>
      tibble::rownames_to_column("Factor") |>
      dplyr::full_join(
        postHoc$groups |>
          tibble::rownames_to_column("Factor")
      ) |>
      dplyr::mutate_if(is.numeric, round, 2) |>
      dplyr::mutate(Factor = factor(Factor)) |>
      tibble::tibble()

    colnames(postHoc_table) <- c("response", "variable", colnames(postHoc_table)[-c(1,2)])

  }
    # create plot

  # plot colors
  cols <- RColorBrewer::brewer.pal(length(unique(postHoc_table$response)),"Dark2")

    if(!is.null(postHoc_test)) {

      mean_plot <- postHoc_table |>
        ggplot2::ggplot(ggplot2::aes(x = response,
                                     y = variable,
                                     fill = response)) +
        ggplot2::geom_col(colour = "black",width = .4)  +
        ggplot2::theme_classic() +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::labs(x=xlab, y=ylab) + # add a variable for the function here
        ggplot2::theme(panel.spacing = grid::unit(.5, "lines"),
                       legend.text=ggplot2::element_text(size=12),
                       legend.position = "bottom") +
        ggplot2::theme(text = ggplot2::element_text(size=12, face = "bold"),
                       axis.text = ggplot2::element_text(color = "black"))  +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = Q25,
                                            ymax = Q75),
                               width=0.2, linewidth = .5) +
        ggplot2::scale_fill_manual(values = cols) +
        ggplot2::geom_text(ggplot2::aes(label= groups, vjust=-.4,
                                        y = Q75), size = 6) +
        ggplot2::ylim(0,max(postHoc_table$Max) + max(postHoc_table$Max)*0.1)

      results <- list(model = f_lm, anova_null = anov_null, model_summary = mod_summary,
                      anova_model = mod_anova, postHoc_table = postHoc_table,
                      bar_plot = mean_plot, fit_plot = mod_fit)
    } else {
      results <- list(model = f_lm, anova_null = anov_null, model_summary = mod_summary,
                      anova_model = mod_anova, fit_plot = mod_fit)
    }

  return(results)

}


