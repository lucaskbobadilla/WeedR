
# #
# #
# test <- FitDoseResponse(df = drc::S.alba, dose = "Dose", var_name = "DryMatter",
#                group = "Herbicide", fit_weibull = T, check_outliers = TRUE)
# #


## DOSE RESPONSE MAIN FUNCTION -------------

FitDoseResponse <- function(dose, var_name, group, df,
                            fit_weibull = FALSE,
                            xlab = "Dose (g a.i /ha)",  ylab = "Biomass", legend_tittle = "Group",
                            print_summary = FALSE, check_outliers = FALSE) {


  # extract variables
  df <- data.frame(df)
  dose <- df[,dose]
  var_name <- df[,var_name]
  group <- df[,group]

  # fit models
  if (fit_weibull == FALSE) {
    # fit log logistics models

    #builds a model with three parameters
    ll3_model <- drc::drm(var_name ~ dose, group, data= df,
                     fct=drc::LL.3())

    #builds a model with four parameters
    ll4_model <- drc::drm(var_name ~ dose, group, data= df,
                     fct=drc::LL.4())

    # builds a model with five parameters
    ll5_model <- drc::drm(var_name ~ dose, group, data= df,
                     fct=drc::LL.5())

    # check for outliers
    if (check_outliers) {

      df_l3_clean <- ouliersRemoval(ll3_model)
      df_l4_clean <- ouliersRemoval(ll4_model)
      df_l5_clean <- ouliersRemoval(ll5_model)

      # refit without outliers
      if (is.data.frame(df_l3_clean)) {
        ll3_model <- drc::drm(var_name ~ dose, group, data= df_l3_clean,
                              fct=drc::LL.3())
        }

      if (is.data.frame(df_l4_clean)) {
        ll4_model <- drc::drm(var_name ~ dose, group, data= df,
                              fct=drc::LL.4())
        }

      if (is.data.frame(df_l5_clean)) {
        ll5_model <- drc::drm(var_name ~ dose, group, data= df,
                              fct=drc::LL.5())
        }
      }


    # join models into list
    models_list <- list(ll3_model = ll3_model,
                        ll4_model = ll4_model,
                        ll5_model = ll5_model)
  } else{
    # fit log logistics models
    # three parameters
    ll3_model <- drc::drm(var_name ~ dose, group, data= df,
                     fct=drc::LL.3())
    # four parameters
    ll4_model <- drc::drm(var_name ~ dose, group, data= df,
                     fct=drc::LL.4())
    # five parameters
    ll5_model <- drc::drm(var_name ~ dose, group, data= df,
                     fct=drc::LL.5())


    # build model with weillbul curves
    #W1
    W13_model <- drc::drm(var_name ~ dose, group, data = df,
                     fct = drc::W1.3())
    W14_model <- drc::drm(var_name ~ dose, group, data = df,
                     fct = drc::W1.4())
    #W2
    W23_model <- drc::drm(var_name ~ dose, group, data = df,
                     fct = drc::W2.3())
    W24_model <- drc::drm(var_name ~ dose, group, data = df,
                     fct = drc::W2.4())


    # check for outliers
    if (check_outliers) {

      df_l3_clean <- ouliersRemoval(ll3_model)
      df_l4_clean <- ouliersRemoval(ll4_model)
      df_l5_clean <- ouliersRemoval(ll5_model)
      df_W13_clean <- ouliersRemoval(W13_model)
      df_W14_clean <- ouliersRemoval(W14_model)
      df_W23_clean <- ouliersRemoval(W23_model)
      df_W24_clean <- ouliersRemoval(W24_model)


      # refit without outliers
      if (is.data.frame(df_l3_clean)) {
        ll3_model <- drc::drm(var_name ~ dose, group, data= df_l3_clean,
                              fct=drc::LL.3())
      }

      if (is.data.frame(df_l4_clean)) {
        ll4_model <- drc::drm(var_name ~ dose, group, data= df,
                              fct=drc::LL.4())
      }

      if (is.data.frame(df_l5_clean)) {
        ll5_model <- drc::drm(var_name ~ dose, group, data= df,
                              fct=drc::LL.5())
      }

      if (is.data.frame(df_W13_clean)) {
        W13_model <- drc::drm(var_name ~ dose, group, data= df,
                              fct=drc::W1.3())
      }

      if (is.data.frame(df_W14_clean)) {
        W14_model <- drc::drm(var_name ~ dose, group, data= df,
                              fct=drc::W1.4())
      }

      if (is.data.frame(df_W23_clean)) {
        W23_model <- drc::drm(var_name ~ dose, group, data= df,
                              fct=drc::W2.3())
      }

      if (is.data.frame(df_W24_clean)) {
        W24_model <- drc::drm(var_name ~ dose, group, data= df,
                              fct=drc::W2.4())
      }
    }


    # join models into a list

    models_logistic <- list(ll3_model = ll3_model,
                            ll4_model = ll4_model,
                            ll5_model = ll5_model)

    models_weibull <- list(W13_model = W13_model,
                           W14_model = W14_model,
                           W23_model = W23_model,
                           W24_model = W24_model)

    # create a list with all modules

    models_list <- c(models_logistic, models_weibull)
  }

  # get better model using AIC and BIC
  model_table <- models_list |>
    lapply(FUN=broom::glance) |>
    lapply(FUN=function(x) x[(names(x) %in% c("AIC", "BIC"))]) |>
    dplyr::bind_rows(.id = "id") |>
    dplyr::mutate(model = models_list,
           summary = purrr::map(model, broom::tidy)) |>
    dplyr::arrange(AIC, BIC)


  # select best model
  selected_model <- get(model_table[1,]$id)




  # print message with the selected model
  message <- paste("According to AIC and BIC parameters: The model", model_table[1,1]$id, "is the best fit for this data",
              "(AIC =", round(model_table[1,]$AIC,2), "BIC =", paste0(round(model_table[1,]$BIC,2), ")"))


  # create a AIC table
  AIC_table <- model_table |> dplyr::select(id:BIC)

  #create a summary table
  summ_table <- model_table[1,] |>
    tidyr::unnest(summary) |>
    dplyr::select(paramater = term, group = curve, estimate:p.value) |>
    dplyr::mutate(paramater = dplyr::case_when(paramater == "b" ~ "slope",
                                                paramater == "c" ~ "lower",
                                                paramater == "d" ~ "upper",
                                                paramater == "e" ~ "ED50",
                                                TRUE ~ as.character(paramater)))
  # PRINT SUMMARY
  if (print_summary == TRUE) {
    cat("Summary table:\n")
    print(summ_table)
  }


    # create assumption and curve plots

    QQplot <- normalQQ_plot(model = selected_model)
    residualPlot <- homogTest_plot(model = selected_model)
    curvePlot <- plot_DR(DR_model = selected_model, legend_tittle = legend_tittle, xlab = xlab,
                         ylab = ylab)
    # get final list
    results <- list(message = message, summary = summ_table, AIC_table = AIC_table, selected_model = selected_model,
                    model_table = model_table, QQplot = QQplot, variancePlot = residualPlot,
                    curvePlot = curvePlot)

    return(results)

  }




## function to check for normality ----------------
normalQQ_plot <- function (model) # argument: vector of numbers
{
  # following four lines from base R's qqline()
  vec <- residuals(model)
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]

  # data frame for residuals
  d <- data.frame(resids = vec)

  # calculate shapiro test
  Shap_test <- stats::shapiro.test(residuals(model))
  Shap_pval <- Shap_test$p.value

  # create annotation with the shapiro test
  grob <- grid::grobTree(grid::textGrob(paste("Shapiro-wilk test p-value:",
                                        round(Shap_pval,4)), x=0.1,  y=0.95, hjust=0,
                            gp=grid::gpar(col="red", fontsize=13, fontface="italic")))

  # create plot
  ggplot2::ggplot(d, ggplot2::aes(sample = resids)) +
    ggplot2::geom_qq_line(linewidth = 1.2) +
    ggplot2::stat_qq(color = "#FF6666", size = 2) +
    ggplot2::theme_light() +
    ggplot2::labs(title = "QQ-plot for normality assumption",
         x = "Theoretical Quantiles", y = "Sample Quantiles") +
    ggplot2::annotation_custom(grob)

}


## function for variance assumption ----------
homogTest_plot <- function(model){
  # run fligner test - homogeinety
  df <- tibble::tibble(model$data[c(1,2,4)])
  Group <-  rep("Lower",nrow(df)) #Creates a vector that repeats "Lower" n times
  Group[model$data$var_name > median(model$data$var_name)] <-  "Upper" #Changing the appropriate values to "Upper"
  Group <- as.factor(Group) #Changes it to a factor, which R recognizes as a grouping variable.
  df$Group <- Group
  the.FKtest <- stats::fligner.test(residuals(model), df$Group)
  FK_pval <- the.FKtest$p.value

  # create annotation with the fligner test
  grob <- grid::grobTree(grid::textGrob(paste("Fligner test p-value:", round(FK_pval,4)), x=0.1,  y=0.95, hjust=0,
                            gp=grid::gpar(col="red", fontsize=13, fontface="italic")))

  #  create plot
  ggplot2::ggplot(generics::augment(model, data = df),
                  ggplot2::aes(.fitted, .resid)) +
    ggplot2::geom_point(color = "#FF6666", size = 2) +
    ggplot2::stat_smooth(method="loess", formula = 'y ~ x') +
    ggplot2::geom_hline(yintercept=0, col="red", linetype="dashed") +
    ggplot2::labs(x = "Fitted values", y = "Residuals", title = "Residual vs Fitted") +
    ggplot2::theme_light() +
    ggplot2::annotation_custom(grob)
}


# function to apply correction if needed
# applyBoxCox <- function(model){
#   # calculate shapiro-wilk
#   Shap_test <- shapiro.test(residuals(model))
#   Shap_pval <- Shap_test$p.value
#
#   # run fligner test - homogeneity
#   df <- tibble(model$data[c(1,2,4)])
#   Group <-  rep("Lower",nrow(df))
#   Group[df$var_name > median(df$var_name)] <-  "Upper"
#   df$Group <- as.factor(Group)
#   the.FKtest <- fligner.test(residuals(model), df$Group)
#   FK_pval <- the.FKtest$p.value


  # apply correction
#   if (FK_pval <= 0.05 | Shap_pval <= 0.05) {
#     cat("Box-Cox correction applied using the Anova method. New model saved into global environment.")
#     corrected_model <<- boxcox(model$call,method="anova", plotit = F)
#   } else {
#     cat("No correction needed.")
#   }
# }



## function to look for outliers ------------
ouliersRemoval <- function(model) {

  # check for outliers
  df <- model$data[c(1,2,4)]
  ei.s <- residuals(model)/sqrt(sum(residuals(model)^2)/(nrow(df) - length(model$coefficients)))
  alpha <- 0.1 ; n = nrow(df); p = length(model$coefficients)
  cutoff <- qt(1-alpha/(2*n), n -p )
  cutoff.deleted <- qt(1-alpha/(2*n), n -p -1 )
  outliers <- which(abs(ei.s) > cutoff)

  # create new data without the outliers

  # return new data
  if (length(outliers) == 0) {
    return("No outliers detected.")
  } else{
    new.data <- df[-outliers,]
    return(new.data)
  }

}





## FUNCTION TO PLOT RESULTS ---------

plot_DR <- function(DR_model, xlab = "Dose (g a.i /ha)",  ylab = "Biomass", legend_tittle = "Group") {


  # get model used
  model_used <- as.character(DR_model$call)[5]
  DR_mod <- NULL

  if(model_used == "drc::LL.3()" | model_used == "drc::W1.3()" | model_used == "drc::W2.3()"){
    DR_mod <- drc::L.3()
  }else{
    if(model_used == "drc::LL.4()" | model_used == "drc::W1.4()" | model_used == "drc::W2.4()"){
      DR_mod <- drc::L.4()
    }else{
      if(model_used == "drc::LL.5()"){
        DR_mod <- drc::L.5()
      }
    }
  }

  # general sd function
  std_mean <- function(x) sd(x,na.rm=TRUE)/sqrt(length(x))

  # correct model dataframe and create a dataframe to plot
  DR_data <- DR_model$data
  colnames(DR_data) <- c("dose", "var_name", "variable1", "variable2", "weights")

  options(dplyr.summarise.inform = FALSE) # suppress messages

  df_plot <- tibble::tibble(DR_data) |>
    dplyr::group_by(dose, variable2) |>
    dplyr::summarize(var_value = mean(var_name, na.rm=TRUE),
              sd = std_mean(var_name)) |>
    dplyr::mutate(dose = dose + 0.01) |>
    dplyr::ungroup()


  # generate plot
  p1 <- suppressWarnings(
      ggplot2::ggplot(data = df_plot, ggplot2::aes(x = dose, y = var_value)) +
        ggplot2::geom_point(ggplot2::aes(color = variable2,
                       text = paste("Dose:", dose,
                                    "\nHerbicide:", variable2,
                                    "\nBiomass:", var_value))) +
        ggplot2::scale_x_log10() +
        ggplot2::geom_errorbar(mapping=ggplot2::aes(ymin=var_value-sd,
                                                    ymax=var_value+sd,
                                                    color = variable2),
                               width=0.2, alpha = .4) +
        ggplot2::geom_smooth(ggplot2::aes(color = variable2),
                    formula = 'y ~ x',
                    method = drc::drm,
                    method.args = list(fct = DR_mod), se = F) +
        ggplot2::theme_light() +
        ggplot2::labs(title= "", x = xlab,  y = ylab) +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::guides(color=ggplot2::guide_legend(title=legend_tittle)
    )
  )

  p1
}

