# SHINY WeedR

  # Packages library ------------------------------
  # library(shiny)
  # library(shinydashboard)
  # library(tidyverse)
  # library(DT)
  # library(kableExtra)
  # library(plotly)
  # library(rhandsontable) # user filled table
  # library(drc)
  # library(datamods)

  # available designs
  design_list <- c("RCBD", "CRD", "Latin-square", "Split-plot")


  # utils functions

  formatv <- function(x, ...) {
    mapply(format, x, scientific = abs(x) < 0.0001, ...)
  }

  # Tabs content -------------------------------------------

  # Experimental design tab =============================================
  design_tab <- shinydashboard::tabItem(
    tabName = "Design",
    h2("Experimental design"),
    shiny::fluidRow(
      ## Experimental design setting ----------------------
      shinydashboard::box(
        title = "1. Choose your experimental design:",
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,

        # provide your design of choice
        shiny::selectInput(
          inputId = "exp_design",
          label = "Design types:",
          choices = design_list
        ),

        # Provide treatment numbers
        shiny::conditionalPanel(
          condition = "input.type_treatments == false",
          shiny::numericInput(
            inputId = "Treat_n",
            label = "How many treatments?",
            value = 2, min = 2
          )
        ),

        # ask if want treatment numbers
        shiny::checkboxInput(
          inputId = "type_treatments",
          label = "Provide list of treatment names intead?",
          value = FALSE
        ),


        shiny::conditionalPanel(
          condition = "input.type_treatments == true",
          shiny::selectizeInput(
            inputId = "list_treats",
            label = "Provide the treatment list:",
            choices = NULL,
            multiple = TRUE,
            options = list(create = TRUE)
          )
        ),


        # if design is RCBD
        shiny::conditionalPanel(
          condition = "input.exp_design == 'RCBD'",
          shiny::numericInput(inputId = "RCBD_blocks",
                              label = "Provide your block number:", value = 4)
        ),

        # if design is CRD or Latin-square
        shiny::conditionalPanel(
          condition = "input.exp_design == 'CRD' || input.exp_design == 'Latin-square'",
          shiny::numericInput(inputId = "CRD_reps",
                              label = "Provide your replication number:", value = 4)
        ),

        # if design is Split_plot
        shiny::conditionalPanel(
          condition = "input.Main_plot_text_check == false && input.exp_design == 'Split-plot'",
          shiny::numericInput(inputId = "Main_plot_n",
                              label = "Provide number of treatments in your main plot:",
                              value = 2,
                              min = 2),

        ),


        shiny::conditionalPanel(
          condition = "input.exp_design == 'Split-plot'",

          shiny::checkboxInput(inputId = "Main_plot_text_check",
                               label = "Provide list of main plot treatment names intead?",
                               value = FALSE),

          shiny::numericInput(
            inputId = "main_plot_reps",
            label = "Provide your main-plot reps:",
            value = 2,
            min = 2
          ),

          # if main plots as text
          shiny::conditionalPanel(
            condition = "input.Main_plot_text_check == true && input.exp_design == 'Split-plot'",
            shiny::selectizeInput(
              inputId = "main_plots_list",
              label = "Provide the main-plot treatment list:",
              choices = NULL,
              multiple = TRUE,
              options = list(create = TRUE)

            )
          ),
        ),

        shiny::actionButton(
          inputId = "build_design",
          label = "Build experimental design",
          class = "btn-lg btn-success"
        )
      ),
      ## Plot design --------------------
      shinydashboard::box(
        title = "Experimental design map:",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,

        plotly::plotlyOutput(outputId = "experiment_plot"),

        shiny::downloadButton('download_design_plot','Download Plot')

      )

    ),

    # Experimental design table
    shiny::fluidRow(
      shinydashboard::box(
        title = "Experimental plan:",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,

        DT::dataTableOutput(outputId = "design_table")

      )
    )

  )




  # Report tab =======================================
  report_tab <- shinydashboard::tabItem(
    tabName = "Report",
    h2("This tab will have report options")
  )



  # DR analysis tab ======================================
  DR_analysis_tab <- shinydashboard::tabItem(
    tabName = "DR_analysis",
    shiny::h2("Dose response analysis - DRC package"),

    #load data box
    shiny::fluidRow(

      shinydashboard::box(

        title = "1. Load your data here",
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,

        shiny::checkboxGroupInput(
          inputId = "from_DR",
          label = "Choose your source:",
          choices = c("file", "copypaste", "googlesheets", "url"),
          selected = c("file")

        ),

        actionButton("launch_modal_DR", "Load your data", class = "btn-lg btn-success")



      ),


      # Data summary box
      shinydashboard::box(

        title = "Data summary",
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,

        tags$b("Data overview:"),
        shiny::verbatimTextOutput(outputId = "summary_DR"),
        tags$head(tags$style("#summary_DR{color:red; font-size:12px; font-style:italic;
overflow-y:scroll; max-height: 150px; background: ghostwhite;}"))

      )
    ),

    # Data Analysis parameter and data row
    shiny::fluidRow(

      shinydashboard::box(
        title = "2. Select your variables and run the analysis:",
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,

        # input response variable
        shiny::selectInput(inputId = "response_var_DR",
                           label = "Response variable", choices = ""),

        # input response variable
        shiny::selectInput(inputId = "dose_var_DR",
                           label = "Dose variable", choices = ""),

        # input response variable
        shiny::selectInput(inputId = "group_var_DR",
                           label = "Group variable", choices = ""),

        # check parameters for function

        # outliers check
        shiny::checkboxInput(inputId = "DR_outliers_check",
                             label = "Check for outliers?", value = FALSE),

        shiny::checkboxInput(inputId = "DR_weibull_check",
                             label = "Fit Weibull models?", value = TRUE),


        shiny::actionButton(inputId = "run_dose_response", "Run analysis",
                            class = "btn-lg btn-success")
      ),

      shinydashboard::box(
        title = "Data: ",
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,

        DT::dataTableOutput(outputId = "data_DR")
      )

    ),

    # Results first line
    shiny::fluidRow(

      shinydashboard::box(
        title = "3. Model summary",
        status = "success",
        solidHeader = TRUE,
        collapsed = TRUE,

        # print message
        tags$b("Model choice:"),
        shiny::verbatimTextOutput(outputId = "DR_model"),

        DT::dataTableOutput(outputId = "DR_summary")
      ),

      shinydashboard::box(
        title = "4. Model choice parameters",
        status = "success",
        solidHeader = TRUE,
        collapsed = TRUE,

        DT::dataTableOutput(outputId = "DR_AIC")
      )

    ),

    # row for assumption plots
    shiny::fluidRow(

      # normality plot
      shinydashboard::box(
        title = "5. Normality assumption",
        status = "success",
        solidHeader = TRUE,
        collapsed = TRUE,

        plotly::plotlyOutput(outputId = "DR_normal_plot"),

        shiny::downloadButton('download_normal_DR_plot','Download Plot')

      ),

      # Variance plot
      shinydashboard::box(
        title = "6. Heterogeineity assumption",
        status = "success",
        solidHeader = TRUE,
        collapsed = TRUE,

        plotly::plotlyOutput(outputId = "homog_DR_plot"),

        shiny::downloadButton('download_homog_DR_plot','Download Plot')


      )

    ),

    # Dose response curve plot
    shiny::fluidRow(
      shinydashboard::box(
        title = "7. Dose response curve plot",
        status = "success",
        solidHeader = TRUE,
        collapsed = TRUE,
        width = 12,

        plotly::plotlyOutput(outputId = "DR_curve_plot"),

        shiny::downloadButton('download_DR_curve_plot','Download Plot')


      )
    )


  )

  # linear analysis tab ======================================
  linear_reg_tab <- shinydashboard::tabItem(

    tabName = "linear_analysis",

    shiny::h2("Linear regression analysis"),

    shiny::fluidRow(

      shinydashboard::box(

        title = "1. Load your data here",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,

        shiny::checkboxGroupInput(
          inputId = "from_LM",
          label = "Choose your source:",
          choices = c("file", "copypaste", "googlesheets", "url"),
          selected = c("file")

        ),

        shiny::actionButton("launch_modal_LM", "Load your data", class = "btn-lg btn-success")
      ),

      # Data summary box
      shinydashboard::box(

        title = "Data summary",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,

        tags$b("Data overview:"),
        shiny::verbatimTextOutput(outputId = "summary_LM"),
        tags$head(tags$style("#summary_LM{color:red; font-size:12px; font-style:italic;
overflow-y:scroll; background: ghostwhite;}"))

      )


    ),

    # get variables and build model
    shiny::fluidRow(

      shinydashboard::box(

        title = "2. Choose your variables and fit your model:",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,

        # input response variable
        shiny::selectInput(inputId = "depend_var_LM",
                           label = "Dependent variable", choices = ""),

        # input independent variable
        shiny::selectInput(inputId = "independ_var_LM",
                           label = "Dose variables", choices = "", multiple = TRUE),

        # input Interaction variable
        shiny::textInput(
          inputId = "interactions_LM",
          label = "Add your interaction terms following this format (col1 * col2 + col3 * col4):"
        ),

        # input posthoc variable
        shiny::textInput(
          inputId = "posthoc_LM",
          label = "Write your variable name for the Post-hoc analysis:"
        ),

        # choose posthoc anaylsis

        shiny::selectInput(inputId = "posthoc_type",
                           label = "What post-hoc analysis you want to do?",
                           choices = c("tukey", "LSD")),

        # outliers check
        shiny::checkboxInput(inputId = "LM_outliers_check",
                             label = "Check for outliers?", value = FALSE),


        #input formula
        tags$b("Formula:"),
        shiny::textOutput(outputId = "formula_LM"),

        shiny::actionButton(inputId = "run_linear_model", "Run analysis",
                            class = "btn-lg btn-success")


      ),

      shinydashboard::box(
        title = "Data: ",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        style = "overflow-x: scroll;",
        DT::dataTableOutput(outputId = "data_LM")
      )

    ),

    shiny::fluidRow(

      shinydashboard::box(
        title = "3. Model summary:",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        # print message
        shiny::verbatimTextOutput(outputId = "LM_model"),

        DT::dataTableOutput(outputId = "LM_summary")

      ),

      # print anova null table
      shinydashboard::box(
        title = "4. ANOVA against the null model:",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,

        DT::dataTableOutput(outputId = "LM_anova_null")

      )



    ),

    shiny::fluidRow(
      # create assumption plots
      shinydashboard::box(
        title = "5. Check your assumptions",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width=12,

        shiny::plotOutput(outputId = "LM_fit_plot"),

        # shiny::downloadButton(outputId = 'download_LM_fit_plot',
        #                       label = 'Download Plot')


      )

    ),

    shiny::fluidRow(
      # ANOVA table
      shinydashboard::box(
        title = "6. ANOVA table:",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width=12,

        DT::dataTableOutput(outputId = "anova_table_LM")


      )

    ),

    shiny::fluidRow(
      # Post-hoc analysis
      shinydashboard::box(
        title = "6. Post-hoc results:",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,

        DT::dataTableOutput(outputId = "LM_posthoc_table")


      ),

      shinydashboard::box(
        title = "Means plot:",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,

        shiny::plotOutput(outputId = "posthoc_plot_LM"),

        shiny::downloadButton(outputId = 'posthoc_plot_LM_download',
                              label = 'Download Plot')


      )



    )



  )

  # segregation analysis tab ======================================
  genetic_seg_tab <- shinydashboard::tabItem(
    tabName = "segregation_analysis",
    shiny::h2("Genetic segregation analysis"),


    shiny::fluidRow(

      shinydashboard::box(

        # Load data
        title = "1. Load your data here",
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,

        shiny::checkboxGroupInput(
          inputId = "from_gene_seg",
          label = "Choose your source:",
          choices = c("file", "copypaste", "googlesheets", "url"),
          selected = c("file")
        ),

        shiny::actionButton("launch_modal_gene_seg", "Load your data", class = "btn-lg btn-warning")
      ),


      # Data summary box
      shinydashboard::box(

        title = "Data summary",
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,

        tags$b("Data overview:"),
        shiny::verbatimTextOutput(outputId = "summary_gene_seg"),
        tags$head(tags$style("#summary_gene_seg{color:red; font-size:12px; font-style:italic;
overflow-y:scroll; background: ghostwhite;}"))

      )

    ),

    shiny::fluidRow(

      shinydashboard::box(
        title = "2. Visualize your phenotype:",
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,

        # input phenotype variable
        shiny::selectInput(inputId = "phenotype_var_gene",
                           label = "Phenotype variable: ", choices = "", selected = NULL),

        #input population variable
        shiny::selectInput(inputId = "population_var_gene",
                           label = "Populations variable: ", choices = "", selected = NULL),

        # input population types
        shiny::selectInput(inputId = "F2_column",
                           label = "F2 population name: ", choices = "", selected = NULL),

        shiny::selectInput(inputId = "F1_column",
                           label = "F1 population name: ", choices = "", selected = NULL),

        shiny::selectInput(inputId = "S_column",
                           label = "Wild-type parent name: ", choices = "", selected = ),

        shiny::selectInput(inputId = "R_column",
                           label = "Variant-type parent name: ", choices = "", selected = ""),

        # Back-crosses

        shiny::checkboxInput(inputId = "Want_BC",
                             label = "Do you have back-cross lines?", value = FALSE),

        shiny::conditionalPanel(
          condition = "input.Want_BC == true",
          shiny::selectInput(inputId = "BC_column",
                             label = "Back-cross population name: ", choices = "",
                             selected = "")
        ),


      ),


      shinydashboard::box(
        title = "Plot parameters",
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,

        # select populations to plot
        shiny::selectInput(
          inputId = "pop_to_plot",
          label = "Provide populations to plot: ",
          multiple = TRUE,
          choices = "",
          selected = ""
          ),

        shiny::selectInput(inputId = "plot_type_gene","Plot Type:",
                    list("histogram",
                         "density",
                         "ridge histogram",
                         "ridge density")
        ),

        shiny::conditionalPanel(
          condition = "input.plot_type_gene == 'histogram'",
          shiny::sliderInput(inputId = "bins_hist",label = "Number of bins:", min = 1, max = 60, value = 30)
        ),

        shiny::conditionalPanel(
          condition = "input.plot_type_gene == 'ridge histogram'",
          shiny::sliderInput(inputId = "bins_hist",label = "Number of bins:", min = 1, max = 60, value = 30)
        )




      )

    )
  )

  # mixed effect analysis tab ======================================
  mixed_reg_tab <- shinydashboard::tabItem(
    tabName = "mixed_analysis",
    h2("Coming soon!"),
    #fluidRow(column(4,rHandsontableOutput("test_table"), actionButton("saveBtn", "Save"))) # test table need to work on it
  )


  # SideBar content ---------------------------------------------------------------------------

  sideBar_content <- shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Experimental Design", tabName = "Design", icon = icon("calendar")),
      shinydashboard::menuItem("Dose response analysis", tabName = "DR_analysis", icon = icon("tint", lib = "glyphicon")),
      shinydashboard::menuItem("Linear regression analysis", tabName = "linear_analysis", icon = icon("indent-right", lib = "glyphicon")),
      shinydashboard::menuItem("Gene segregation analysis", tabName = "segregation_analysis", icon = icon("leaf", lib = "glyphicon")),
      shinydashboard::menuItem("Mixed-effect model analysis", tabName = "mixed_analysis", icon = icon("signal", lib = "glyphicon")),
      shinydashboard::menuItem("Generate report", tabName = "Report", icon = icon("save"))
    )
  )

  # BODY content ------------------------------------------------------------------------------

  body_content <- shinydashboard::dashboardBody(

    shinydashboard::tabItems(
      # to make changes do it at the tab section above
      design_tab,
      report_tab,
      DR_analysis_tab,
      linear_reg_tab,
      mixed_reg_tab,
      genetic_seg_tab
    )
  )

  # UI ----------------------------------------------------------------------------------------

  ui <- shinydashboard::dashboardPage(

    ## header
    shinydashboard::dashboardHeader(title = "WeedR"),

    ## Sidebar content
    sideBar_content,

    ## Body content
    body_content,

    ## Aesthetic
    skin = "green"
  )

  # Server ------------------------------------------------------------------------------------

  server <- function(input, output, session) {

    # EXPERIMENTAL DESIGN OUTPUT ########################

    # this will get the chosen design
    exp_design <- eventReactive(input$build_design,
                                    {
                                      input$exp_design
                                    })


    # This will build the experimental design
    experiment_data <- shiny::eventReactive(input$build_design, {

      #RCBD
      if (exp_design() == "RCBD") {

        if (input$type_treatments == FALSE) {
          RCBD_design(blocks_n = input$RCBD_blocks, treat = input$Treat_n, plot_label = "plots")
        } else {
          RCBD_design(blocks_n = input$RCBD_blocks, treat = input$list_treats, plot_label = "plots")
        }

      } else {
      #CRD
        if (exp_design() == "CRD") {
          if (input$type_treatments == FALSE) {
            CRD_design(reps_n = input$CRD_reps, treat = input$Treat_n)
          } else {
            CRD_design(reps_n = input$CRD_reps, treat = input$list_treats)
          }

        } else {
      # Latin square
          if (exp_design() == "Latin-square") {
            if (input$type_treatments == FALSE) {
              latinSquare_design(treatments = input$Treat_n,reps = input$CRD_reps)
            } else {
              latinSquare_design(treatments = input$list_treats,reps = input$CRD_reps)
            }

          } else {
      # Split-plot
            if (exp_design() == "Split-plot") {
              if (input$type_treatments == FALSE && input$Main_plot_text_check == FALSE) {
                splitPlotDesign(subplot =  input$Treat_n,
                                main_plot = input$Main_plot_n,
                                main_plot_reps = input$main_plot_reps)
              } else {
                if (input$type_treatments == TRUE && input$Main_plot_text_check == FALSE) {
                splitPlotDesign(subplot =  input$list_treats,
                                main_plot = input$Main_plot_n,
                                main_plot_reps = input$main_plot_reps)
                  } else {
                    if (input$type_treatments == FALSE && input$Main_plot_text_check == TRUE) {
                splitPlotDesign(subplot = input$Treat_n,
                                main_plot = input$main_plots_list,
                                main_plot_reps = input$main_plot_reps)
                    } else {
                      splitPlotDesign(subplot = input$list_treats,
                                      main_plot = input$main_plots_list,
                                      main_plot_reps = input$main_plot_reps)
                      }
                  }
              }
            }
          }
        }
        }
      })


    output$experiment_plot <-  plotly::renderPlotly({

      plotly::ggplotly(experiment_data()$plot)

    })

    output$download_design_plot <- shiny::downloadHandler(
      filename = function(){paste("Design_map_WeedR",'.jpeg',sep='')},
      content = function(file){
        ggplot2::ggsave(file,plot=experiment_data()$plot, dpi = 300)
      }
    )

    # get experimental design book

    output$design_table <- DT::renderDataTable(server=FALSE, {
      DT::datatable(
        { experiment_data()$experiment_book },
        extensions = 'Buttons',

        options = list(
          paging = TRUE,
          searching = TRUE,
          fixedColumns = TRUE,
          autoWidth = TRUE,
          ordering = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),

        class = "display")
    })




    # LM OUTPUT ##############

    # Load LM data
    shiny::observeEvent(input$launch_modal_LM, {
      shiny::req(input$from_LM)
      datamods::import_modal(
        id = "LM_data",
        from = input$from_LM,
        title = "Import data to be used in application"
      )
    })


    # get the imported LM data
    LM_data_server <- datamods::import_server("LM_data", return_class = "tbl_df")

    # LM Data loaded summary
    output$summary_LM <- shiny::renderPrint({
      summary(LM_data_server$data())
    })

    shiny::observe({
      #Update select input for dependent variable
      shiny::updateSelectInput(session, inputId = 'depend_var_LM',
                               label = 'Select your dependent variable:',
                               choices  = colnames(LM_data_server$data()))
    })

    shiny::observe({
      #Update select input for variables no interaction
      shiny::updateSelectInput(session, inputId = 'independ_var_LM',
                               label = 'Select your independent variables:',
                               choices  = colnames(LM_data_server$data()))
    })



    # build formula with input
    output$formula_LM <- shiny::renderText({
      # get formula values
      vars_independent_LM <- paste(input$independ_var_LM, collapse = " + ")
      var_dependent_LM <- input$depend_var_LM
      vars_interaction_LM <- input$interactions_LM

      LM_formula_text <- paste(var_dependent_LM, vars_independent_LM, sep = " ~ ")

      if (input$interactions_LM != "") {
        vars_all <- paste(vars_independent_LM, vars_interaction_LM, sep = " + ")
        LM_formula_text <- paste(var_dependent_LM, vars_all, sep = " ~ ")
      }

      LM_formula_text

    })

    # Data loaded table
    output$data_LM <- DT::renderDataTable(server = FALSE, {
      DT::datatable(
        LM_data_server$data(),
        extensions=c("Buttons",'Scroller'),
        options = list(dom = 'Bfrtip',
                       scrollY = 500,
                       scroller = TRUE,
                       scrollX=TRUE,
                       pagelength=15,
                       buttons = c('copy', 'csv')
        )
      )
    })


    ###### run linear regression function ######

    # BUILD the formula
    LM_formula <- shiny::reactive({

      if (input$interactions_LM != "") {

        vars_interaction_LM <- input$interactions_LM
        vars_all <- paste(paste(input$independ_var_LM,
                                collapse = " + "),
                          vars_interaction_LM, sep = " + ")
        LM_formula_text <- paste(input$depend_var_LM, vars_all,
                                 sep = " ~ ")

        LM_formula_text

      } else {


        LM_formula_text <- paste(input$depend_var_LM,
                                 paste(input$independ_var_LM,
                                       collapse = " + "), sep = " ~ ")

        LM_formula_text
      }

    })

    LM_results <- shiny::eventReactive(input$run_linear_model, {


      # fit model
      fit_linear_model(
        dataframe = LM_data_server$data(),
        formula = LM_formula(),
        postHoc_variable = input$posthoc_LM,
        postHoc_test = input$posthoc_type,
        outliers_check = input$LM_outliers_check
      )
    })

    # get results for linear model

    # get model information
    output$LM_model <- shiny::renderPrint({
      LM_results()$model
    })

    #summary table
    output$LM_summary <- DT::renderDataTable(server = FALSE,{
      DT::datatable({
        LM_results()$model_summary |>
          dplyr::mutate(p.value = formatv(p.value, digits = 3)) |>
          dplyr::mutate_if(is.numeric, round, 2)
      },

      extensions = 'Buttons',

      options = list(
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),

      class = "display")
    })

    #ANOVA against null table
    output$LM_anova_null <- DT::renderDataTable(server = FALSE,{
      DT::datatable({
        LM_results()$anova_null |>
          dplyr::mutate(p.value = formatv(p.value, digits = 3),
                        p.value = dplyr::if_else(p.value == "NA", "", p.value)) |>
          dplyr::mutate_if(is.numeric, round, 2)
      },

      extensions = 'Buttons',

      options = list(
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),

      class = "display")
    })

    # get fit plots
    output$LM_fit_plot <- shiny::renderPlot({

      LM_results()$fit_plot

    }, res = 96)

    ## NEED TO FIX THIS ###
    output$download_LM_fit_plot <- shiny::downloadHandler(
      filename = function(){paste("fit_plots_LM_WeedR",'.png',sep='')},
      content = function(file){
        ggplot2::ggsave(file,grid::grid.draw(LM_results()$fit_plot), dpi = 300)
      }
    )
    ## NEED TO FIX THIS ### DOWNLOAD NOT WORKING


    # ANOVA table
    output$anova_table_LM <- DT::renderDataTable(server = FALSE,{
      DT::datatable({
        LM_results()$anova_model |>
          dplyr::mutate(p.value = formatv(p.value, digits = 3),
                        p.value = dplyr::if_else(p.value == "NA", "", p.value)) |>
          dplyr::mutate_if(is.numeric, round, 2)
      },

      extensions = 'Buttons',

      options = list(
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),

      class = "display")
    })

    # Posthoc table
    output$LM_posthoc_table <- DT::renderDataTable(server = FALSE,{
      DT::datatable({
        LM_results()$postHoc_table |>
          dplyr::mutate_if(is.numeric, round, 2)
      },

      extensions = 'Buttons',

      options = list(
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        scrollY = 200,
        scrollCollapse = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),

      class = "display")
    })

    #post-hoc plot
    output$posthoc_plot_LM <- shiny::renderPlot({

      LM_results()$bar_plot

    }, res = 96)

    output$posthoc_plot_LM_download <- shiny::downloadHandler(
      filename = function(){paste("posthoc_LM_WeedR",'.png',sep='')},
      content = function(file){
        ggplot2::ggsave(file,plot=LM_results()$bar_plot, dpi = 300)
      }
    )








    # DR OUTPUT ############

    shiny::observeEvent(input$launch_modal_DR, {
      shiny::req(input$from_DR)
      datamods::import_modal(
        id = "DR_data",
        from = input$from_DR,
        title = "Import data to be used in application"
      )
    })


    # get the imported data
    DR_data_server <- datamods::import_server("DR_data", return_class = "tbl_df")

    shiny::observe({
      #Update select input for response
      shiny::updateSelectInput(session, inputId = 'response_var_DR',
                               label = 'Select your response variable:',
                               choices  = colnames(DR_data_server$data()))
    })

    shiny::observe({
      #Update select input for dose
      shiny::updateSelectInput(session, inputId = 'dose_var_DR',
                               label = 'Select your dose variable:',
                               choices  = colnames(DR_data_server$data()))
    })

    shiny::observe({
      #Update select input for group
      shiny::updateSelectInput(session, inputId = 'group_var_DR',
                               label = 'Select your group/population variable:',
                               choices  = colnames(DR_data_server$data()))
    })


    # Data loaded summary
    output$summary_DR <- shiny::renderPrint({
      summary(DR_data_server$data())
    })

    # Data loaded table
    output$data_DR <- DT::renderDataTable({
      DT::datatable(
        {DR_data_server$data()},

        extensions = 'Buttons',

        options = list(
          paging = TRUE,
          searching = TRUE,
          fixedColumns = TRUE,
          autoWidth = TRUE,
          ordering = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),

        class = "display"
      )
    })

    ###### run dose response function ----------------------

    DR_results <- shiny::eventReactive(input$run_dose_response, {


      FitDoseResponse(dose = input$dose_var_DR ,
                      var_name = input$response_var_DR,
                      group = input$group_var_DR, df = DR_data_server$data(),
                      fit_weibull = input$DR_weibull_check,
                      check_outliers = input$DR_outliers_check,
                      print_summary = FALSE)

    })

    # get results for dose response

    # get model information
    output$DR_model <- shiny::renderPrint({
      DR_results()$message
    })

    #summary table
    output$DR_summary <- DT::renderDataTable(server = FALSE, {
      DT::datatable({
        DR_results()$summary |>
          dplyr::mutate(p.value = as.character(round(p.value,5))) |>
          dplyr::mutate_if(is.numeric, round, 2)
      },

      extensions = 'Buttons',

      options = list(
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),

      class = "display")
    })


    # MODEL AIC/BIC TABLE
    output$DR_AIC <- DT::renderDataTable( server = FALSE, {
      DT::datatable({
        DR_results()$AIC_table |> dplyr::mutate_if(is.numeric, round, 2)
      },

      extensions = 'Buttons',

      options = list(
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),

      class = "display")
    })

    # Normality plot

    output$DR_normal_plot <- plotly::renderPlotly({

      plotly::ggplotly(DR_results()$QQplot)

    })

    output$download_normal_DR_plot <- shiny::downloadHandler(
      filename = function(){paste("QQplot_DR_WeedR",'.png',sep='')},
      content = function(file){
        ggplot2::ggsave(file,plot=DR_results()$QQplot, dpi = 300)
      }
    )

    # Homogeneity plot

    output$homog_DR_plot <- plotly::renderPlotly({

      plotly::ggplotly(DR_results()$variancePlot)

    })

    output$download_homog_DR_plot <- shiny::downloadHandler(
      filename = function(){paste("VariancePlot_DR_WeedR",'.png',sep='')},
      content = function(file){
        ggplot2::ggsave(file,plot=DR_results()$variancePlot, dpi = 300)
      }
    )

    # add Dose response curve

    output$DR_curve_plot <- plotly::renderPlotly({

      plotly::ggplotly(DR_results()$curvePlot)

    })

    output$download_DR_curve_plot <- shiny::downloadHandler(
      filename = function(){paste("CurvePlot_DR_WeedR",'.png',sep='')},
      content = function(file){
        ggplot2::ggsave(file,plot=DR_results()$curvePlot, dpi = 300)
      }
    )


    # GENE SEGREGATION OUTPUT ############

    # Load Gene segregation data
    shiny::observeEvent(input$launch_modal_gene_seg, {
      shiny::req(input$from_gene_seg)
      datamods::import_modal(
        id = "gene_seg_data",
        from = input$from_LM,
        title = "Import data to be used in application"
      )
    })


    # get the imported LM data
    gene_seg_data_server <- datamods::import_server("gene_seg_data", return_class = "tbl_df")

    # LM Data loaded summary
    output$summary_gene_seg <- shiny::renderPrint({
      summary(gene_seg_data_server$data())
    })

    shiny::observe({
      #Update select input for phenotpye
      shiny::updateSelectInput(session, inputId = 'phenotype_var_gene',
                               label = 'Select your phenotype variable:',
                               choices  = colnames(gene_seg_data_server$data()))

    })

    shiny::observe({
      shiny::req(gene_seg_data_server$data())
      #Update select input for populations
      choice <- colnames(gene_seg_data_server$data() |> dplyr::select_if(purrr::negate(is.numeric)))

      shiny::updateSelectInput(session, inputId = 'population_var_gene',
                               label = 'Select your populations variable:',
                               choices  = choice)
    })

    # get F2 value
    shiny::observe({
      shiny::req(input$population_var_gene)
      DF_gene <- gene_seg_data_server$data() |> dplyr::select(input$population_var_gene)
      choice <- as.vector(DF_gene |> dplyr::distinct())
      #Update select input for populations
      shiny::updateSelectInput(session, inputId = 'F2_column',
                               label = 'F2 population name: ',
                               choices  = choice,
                               selected = ""
      )

    })

    # get F1 value
    shiny::observe({
      shiny::req(input$population_var_gene)
      DF_gene <- gene_seg_data_server$data() |> dplyr::select(input$population_var_gene)
      #Update select input for populations
      shiny::updateSelectInput(session, inputId = 'F1_column',
                               label = 'F1 population name: ',
                               choices  = as.vector(DF_gene |> dplyr::distinct()),
                               selected = ""
      )

    })

    # get wild type value
    shiny::observe({
      shiny::req(input$population_var_gene)

      DF_gene <- gene_seg_data_server$data() |> dplyr::select(input$population_var_gene)
      choice <- as.vector(DF_gene |> dplyr::distinct())

        #Update select input for populations
        shiny::updateSelectInput(session, inputId = 'S_column',
                                 label = 'Wild-type population name: ',
                                 choices  = choice,
                                 selected = "")


    })

    # get variant-type value
    shiny::observe({
      shiny::req(input$population_var_gene)
      DF_gene <- gene_seg_data_server$data() |> dplyr::select(input$population_var_gene)
      choice <- as.vector(DF_gene |> dplyr::distinct())
      #Update select input for populations
      shiny::updateSelectInput(session, inputId = 'R_column',
                               label = 'Variant-type population name: ',
                               choices  = choice,
                               selected = ""
      )

    })

    # get BC value
    shiny::observe({
      shiny::req(input$population_var_gene)
      DF_gene <- gene_seg_data_server$data() |> dplyr::select(input$population_var_gene)
      choice <- as.vector(DF_gene |> dplyr::distinct())
      #Update select input for populations
      shiny::updateSelectInput(session, inputId = 'BC_column',
                               label = 'Back-cross population name: ',
                               choices  = choice,
                               selected = ""
      )

    })


    # get list of populations to plot
    shiny::observe({
      shiny::req(input$population_var_gene)
      DF_gene <- gene_seg_data_server$data() |> dplyr::select(input$population_var_gene)
      choice <- as.vector(DF_gene |> dplyr::distinct())
      #Update select input for populations
      shiny::updateSelectInput(session, inputId = 'pop_to_plot',
                               label = 'Populations to plot: ',
                               choices  = choice,
                               selected = ""
      )

    })

  }


  # Run shiny app ---------------------------------------------------------------------------
  shiny::shinyApp(ui, server)

