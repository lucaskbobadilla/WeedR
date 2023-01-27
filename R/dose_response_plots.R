# ###### Dose response plots #####
#
# library(drc)
# library(tidyverse)
# library(plotly)
# library(broom)
#
# # # Suppress summarise info
# # options(dplyr.summarise.inform = FALSE)
# #
# # # load data
# # DR.test.1 <- S.alba
# # usethis::use_data(DR.test.1)
#
# # write_csv(DR.test.1, "~/Downloads/s_alba.csv")
# # fit dose response
#
# # FitDoseResponse(dose = "Dose", "DryMatter", variable = "Herbicide",
# #                 df = df, fit_weibull = TRUE, get_AIC_table = TRUE, get_summary = TRUE,
# #                 remove_outliers = TRUE, applyBC = TRUE, plot_curve = TRUE)
#
#
#
#
#
# applyBoxCox(selected_model)
# t <- selected_model$call
# # plot 1 - DRC version
#
# plot(selected_model, bp=.2, bty="l",
#      ylab="Biomass reduction (%)",
#      xlab="Dicamba (g a.e /ha)",
#      main="Biomass reduction dose response",
#      xlim=c(0,100000),
#      col = T,
#      ylim = c(0,5),
#      broken = T,
#      pch = 1,
#      lwd = 2.5)
# arrows(.1, 50, 200, 50, code=0, lty=1, col="red")
# arrows(200, 50, 200, 0, code=0, lty=1, col="red")
#
#
# #plot_2
# options(scipen=10000)
# std_mean <- function(x) sd(x,na.rm=TRUE)/sqrt(length(x))
#
# df_1 <- tibble(df) %>%
#   group_by(Dose, Herbicide) %>%
#   summarize(var_value = mean(DryMatter, na.rm=TRUE),
#             sd = std_mean(DryMatter),
#             ) %>%
#   mutate(Dose = Dose + 0.1)
#
# field_rate <- 320
#
# drc_mods <-
#
# df_1 <- df_1 |>
#   mutate(var_value =
#            ifelse(var_value == 0,
#                   yes = (sort(df_1$var_value[which.min(sort(df_1$var_value)) + 1]/100)),
#                   no = var_value
#            ))
#
#
# model_used <- as.character(selected_model$call)[5]
# DR_mod <- NULL
#
# if(model_used == "drc::LL.3()" | model_used == "drc::W1.3()" | model_used == "drc::W2.3()"){
#   DR_mod <- drc::L.3()
# }else{
#   if(model_used == "drc::LL.4()" | model_used == "drc::W1.4()" | model_used == "drc::W2.4()"){
#     DR_mod <- drc::L.4()
#   }else{
#     if(model_used == "drc::LL.5()"){
#       DR_mod <- drc::L.5()
#     }
#   }
# }
#
#
#
# ggplot(data = df_1, aes(x = Dose, y = var_value)) +
#   geom_point(aes(color = Herbicide,
#                  text = paste("Dose:", Dose,
#                               "\nHerbicide:", Herbicide,
#                               "\nBiomass:", var_value))) +
#   scale_x_log10() +
#   geom_errorbar(mapping=aes(ymin=var_value-sd, ymax=var_value+sd,color = Herbicide), width=0.2, alpha = .4) +
#   geom_smooth(aes(color = Herbicide),method = "drm", method.args = list(fct = DR_mod), se = F) +
#   theme_light() +
#   labs(title= "", x = "Dose (g a.e /ha)",  y = "Biomass") +
#   theme(legend.position = "bottom") +
#   ylim(0, NA)
#
# t <- selected_model$data
# cols <- which(names(selected_model$data) == 'variable')
# names(t)[cols] <- paste0('variable', seq_along(cols))
# names(t)
#
# ## create function for the first plot ---------
# selected_model$data
#
# DR_model <- selected_model
# rm(DR_model)
# rm(DR_mod)
# plot_DR <- function(DR_model) {
#
#
#   # get model used
#   model_used <- as.character(DR_model$call)[5]
#   DR_mod <- NULL
#
#   if(model_used == "drc::LL.3()" | model_used == "drc::W1.3()" | model_used == "drc::W2.3()"){
#     DR_mod <- drc::L.3()
#   }else{
#     if(model_used == "drc::LL.4()" | model_used == "drc::W1.4()" | model_used == "drc::W2.4()"){
#       DR_mod <- drc::L.4()
#     }else{
#       if(model_used == "drc::LL.5()"){
#         DR_mod <- drc::L.5()
#       }
#     }
#   }
#
#   # general sd function
#   std_mean <- function(x) sd(x,na.rm=TRUE)/sqrt(length(x))
#
#   # correct model dataframe and create a dataframe to plot
#   DR_data <- DR_model$data
#   colnames(DR_data) <- c("dose", "var_name", "variable1", "variable2", "weights")
#   #cols <- which(names(DR_data) == 'variable')
#   #names(DR_data)[cols] <- paste0('variable', seq_along(cols))
#   df_plot <- tibble(DR_data) %>%
#     group_by(dose, variable2) %>%
#     summarize(var_value = mean(var_name, na.rm=TRUE),
#               sd = std_mean(var_name)) %>%
#     mutate(dose = dose + 0.01) |>
#     ungroup()
#
#   # get
#
#   # generate plot
#   p1 <- suppressWarnings(
#     print (
#       ggplot(data = df_plot, aes(x = dose, y = var_value)) +
#         geom_point(aes(color = variable2,
#                        text = paste("Dose:", dose,
#                                     "\nHerbicide:", variable2,
#                                     "\nBiomass:", var_value))) +
#         scale_x_log10() +
#         geom_errorbar(mapping=aes(ymin=var_value-sd, ymax=var_value+sd,color = variable2), width=0.2, alpha = .4) +
#         geom_smooth(aes(color = variable2),
#                     formula = 'y ~ x',
#                     method = "drm",
#                     method.args = list(fct = DR_mod), se = F) +
#         theme_light() +
#         labs(title= "", x = "Dose (g a.i /ha)",  y = "Biomass") +
#         theme(legend.position = "bottom") + guides(color=guide_legend(title="Group"))
#     )
#   )
#
#   return(p1)
# }
#
# plot_DR(selected_model, legend_tittle = "Herbicide: ")
