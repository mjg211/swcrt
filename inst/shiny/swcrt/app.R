##### Load required packages ###################################################

library(knitr)
library(magrittr)
library(swcrt)
options(shiny.sanitize.errors = TRUE)

#sapply("opt_sw_norm_setting.Rmd", knit, quiet = TRUE)

##### UI #######################################################################
ui <- function(request) {
  shinydashboard::dashboardPage(
    ##### Dashboard: Header ####################################################
    shinydashboard::dashboardHeader(
      title      = "swcrt",
      titleWidth = 200
    ),
    ##### Dashboard: Sidebar ###################################################
    shinydashboard::dashboardSidebar(
      width = 200,
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          text    = "Home",
          tabName = "home",
          icon    = shiny::icon(name = "home")
        ),
        shinydashboard::menuItem(
          text    = HTML("Source code<sup><i class='fa fa-external-link' ",
                         "style='font-size:8px'></i></sup>"),
          icon    = shiny::icon(name = "file-code-o"),
          href    = "https://github.com/mjg211/swcrt/"
        ),
        id = "sidebar"
      )
    ),
    ##### Dashboard: Body ######################################################
    shinydashboard::dashboardBody(
      #tags$head(includeScript("google-analytics.js")),
      tags$script(
        HTML(
          "var openTab = function(tabName){
           $('a', $('.sidebar')).each(function() {
             if(this.getAttribute('data-value') == tabName) {
               this.click()
             };
           });
        }"
        )
      ),
      shinybusy::add_busy_bar(color = "black"),
      sever::use_sever(),
      shinydashboard::tabItems(
        ##### Tab: Home ########################################################
        shinydashboard::tabItem(
          tabName = "home",
          ##### Row 1: About ###################################################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "About",
              width       = 12,
              solidHeader = TRUE,
              status      = "primary",
              shiny::withMathJax(
                shiny::includeMarkdown("about.md")
              )
            )
          ),
          ##### Row 2: Design parameters & Design summary ######################
          shiny::fluidRow(
            shinydashboard::box(
              shiny::withMathJax(),
              shinyalert::useShinyalert(),
              shinyFeedback::useShinyFeedback(),
              shinyjs::useShinyjs(),
              id          = "opt_sw_norm_parameters",
              title       = "Design parameters",
              width       = 4,
              solidHeader = TRUE,
              status      = "primary",
              tags$style(type = "text/css",
                         ".irs-grid-pol.small {height: 0px;}"),
              shiny::sliderInput(
                inputId = "opt_sw_norm_C",
                label   = "C (number of cluters):",
                min     = 2,
                max     = 100,
                value   = 10,
                step    = 1
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_C",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::sliderInput(
                inputId = "opt_sw_norm_Ti",
                label   = "T (number of time periods):",
                min     = 3,
                max     = 20,
                value   = 10,
                step    = 1
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_Ti",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::numericInput(
                inputId = "opt_sw_norm_m",
                label   = "m (number of measurements per cluster-period):",
                value   = 10,
                min     = 0,
                max     = NA,
                step    = 10
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_m",
                  size    = "m",
                  colour  = "black"),
              shiny::numericInput(
                inputId  = "opt_sw_norm_rho0",
                label    = "rho0 (within-period intra-cluster correlation):",
                value   = 0.05,
                min     = 0,
                max     = 1,
                step    = 0.01
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_rho0",
                  size    = "m",
                  colour  = "black"),
              shiny::numericInput(
                inputId  = "opt_sw_norm_r0",
                label    = "r0 (autoregressive structure parameter):",
                value   = 1,
                min     = 0,
                max     = 1,
                step    = 0.01
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_r0",
                  size    = "m",
                  colour  = "black"),
              shiny::numericInput(
                inputId  = "opt_sw_norm_r",
                label    = "r (autoregressive structure parameter):",
                value   = 1,
                min     = 0,
                max     = 1,
                step    = 0.01
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_r",
                  size    = "m",
                  colour  = "black"),
              shiny::selectInput(
                inputId  = "opt_sw_norm_time_effect",
                label    = "Time modelled:",
                choices  =
                  list("Discretly"   =
                         list("Fixed effect per period" = "discrete"),
                       "Continuously" =
                         list("Linear function"    = "linear",
                              "Quadratic function" = "quadratic",
                              "Cubic function"     = "cubic",
                              "Quartic function"   = "quartic",
                              "Quintic function "  = "quintic")),
                selected = "discrete"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_time_effect",
                  size    = "m",
                  colour  = "black"
                ),
              shinyWidgets::prettySwitch(
                inputId = "opt_sw_norm_extreme_seq",
                label   =
                  "Allow sequences of all control/all intervention allocation",
                status  = "info",
                value   = FALSE,
                slim    = TRUE
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_extreme_seq",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::hr(),
              shiny::actionButton(
                inputId = "opt_sw_norm_reset",
                label   = "  Reset inputs  ",
                icon    = shiny::icon(name = "eraser"),
                width   = "100%"
              ),
              shiny::hr(),
              shiny::actionButton(
                inputId = "opt_sw_norm_update",
                label   = "  Update outputs  ",
                icon    = shiny::icon(name = "check-square-o"),
                width   = "100%"
              ),
              shiny::hr(),
              shiny::textInput(
                inputId = "opt_sw_norm_filename",
                label   = "Report filename:",
                value   = "optimal_swcrt_norm"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_filename",
                  size    = "m",
                  colour  = "black"
                ),
              tags$head(tags$style(".full_width{width:100%;}")),
              shiny::radioButtons(
                inputId  = "opt_sw_norm_format",
                label    = "Download format",
                choices  = c("PDF"  = "pdf",
                             "HTML" = "html",
                             "Word" = "word"),
                selected = "pdf",
                inline   = TRUE
              ),
              shiny::downloadButton(
                outputId = "opt_sw_norm_report",
                label    = "  Download report  ",
                class    = "full_width"
              )
            ),
            shinydashboard::box(
              title       = "Optimal design summary",
              width       = 8,
              solidHeader = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::withMathJax(
                  shiny::htmlOutput("opt_sw_norm_summary")
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          ##### Row 3: Design plots ############################################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Exact optimal design (equal sequence width)",
              width       = 4,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput("opt_sw_norm_exact_equal"),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            ),
            shinydashboard::box(
              title       = "Exact optimal design (weighted sequence width)",
              width       = 4,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput("opt_sw_norm_exact_weighted"),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            ),
            shinydashboard::box(
              title       = "Rounded optimal design",
              width       = 4,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput("opt_sw_norm_rounded"),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          ##### Row 4: Session information #####################################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Session Information",
              status      = "primary",
              solidHeader = TRUE,
              width       = 12,
              collapsible = TRUE,
              collapsed   = TRUE,
              shiny::verbatimTextOutput("opt_sw_norm_debug")
            )
          )
        )
      )
    )
  )
}

##### Server ###################################################################
server <- function(input, output, session) {
  ##### Initial set-up #########################################################

  shinyhelper::observe_helpers(withMathJax = TRUE)

  ##### shinyFeedback warning messages #########################################

  shiny::observeEvent(input$opt_sw_norm_m, {
    shinyFeedback::feedbackDanger(
      inputId = "opt_sw_norm_m",
      show    = (input$opt_sw_norm_m <= 0),
      text    = "Must be strictly positive")
  })

  shiny::observeEvent(input$opt_sw_norm_rho0, {
    shinyFeedback::feedbackDanger(
      inputId = "opt_sw_norm_rho0",
      show    = any(input$opt_sw_norm_rho0 < 0,
                    input$opt_sw_norm_rho0 > 1),
      text    = "Must belong to [0,1]")
  })

  shiny::observeEvent(input$opt_sw_norm_r0, {
    shinyFeedback::feedbackDanger(
      inputId = "opt_sw_norm_r0",
      show    = any(input$opt_sw_norm_r0 <= 0,
                    input$opt_sw_norm_r0 > 1),
      text    = "Must belong to (0,1]")
  })

  shiny::observeEvent(input$opt_sw_norm_r, {
    shinyFeedback::feedbackDanger(
      inputId = "opt_sw_norm_r",
      show    = any(input$opt_sw_norm_r <= 0,
                    input$opt_sw_norm_r > 1),
      text    = "Must belong to (0,1]")
  })

  shiny::observeEvent(input$opt_sw_norm_filename, {
    shinyFeedback::feedbackWarning(
      inputId = "opt_sw_norm_filename",
      show    = any(strsplit(input$opt_sw_norm_filename,
                             split = "")[[1]] %in%
                      c('/', '\\', '?', "%", "*", ":", "|", "<", ">")),
      text    = paste0('It is generally inadvisable to use the characters /',
                       ', \\, ?, %, *, :, |, ", <, and > in a filename'))
  })

  ##### int_opt_sw_norm() ######################################################

  int_opt_sw_norm <- shiny::eventReactive(input$opt_sw_norm_update, {
    progress           <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Identifying optimal design", value = 0)
    opt                <- opt_sw_norm(C           = input$opt_sw_norm_C,
                                      Ti          = input$opt_sw_norm_Ti,
                                      m           = input$opt_sw_norm_m,
                                      rho0        = input$opt_sw_norm_rho0,
                                      r0          = input$opt_sw_norm_r0,
                                      r           = input$opt_sw_norm_r,
                                      extreme_seq =
                                        input$opt_sw_norm_extreme_seq,
                                      time_effect =
                                        input$opt_sw_norm_time_effect,
                                      symmetric_w = FALSE)
    progress$inc(amount = 0.25, message = "Producing design summary")
    rmarkdown::render(
      input         = "opt_sw_norm_summary.Rmd",
      output_format = rmarkdown::html_document(),
      output_file   = file.path(tempdir(), "opt_sw_norm_summary.html"),
      params        = list(C                       = input$opt_sw_norm_C,
                           Ti                      = input$opt_sw_norm_Ti,
                           m                       = input$opt_sw_norm_m,
                           rho0                    = input$opt_sw_norm_rho0,
                           r0                      = input$opt_sw_norm_r0,
                           r                       = input$opt_sw_norm_r,
                           extreme_seq             =
                             input$opt_sw_norm_extreme_seq,
                           time_effect             =
                             input$opt_sw_norm_time_effect,
                           optimal_num_exact       = opt$optimal_num_exact,
                           optimal_num_rounded     = opt$optimal_num_rounded,
                           optimal_weights_exact   = opt$optimal_weights_exact,
                           optimal_weights_rounded =
                             opt$optimal_weights_rounded)
    )
    xml2::write_html(
      rvest::html_node(
        xml2::read_html(
          paste0(tempdir(), "/opt_sw_norm_summary.html")
        ),
        "body"
      ),
      file = paste0(tempdir(), "/opt_sw_norm_summary_modified.html")
    )
    progress$inc(amount = 0.25, message = "Producting plots")
    plots              <- plot(opt, output = TRUE, print_plots = FALSE)
    opt$rounded        <- plots$plots$rounded
    opt$exact_equal    <- plots$plots$exact_equal
    opt$exact_weighted <- plots$plots$exact_weighted
    progress$inc(amount = 0.25, message = "Outputting results")
    opt
  })

  ##### Summary ################################################################

  output$opt_sw_norm_summary <- shiny::renderUI({
    input$opt_sw_norm_update
    C <- int_opt_sw_norm()$inputs$C
    shiny::withMathJax(
      shiny::includeHTML(
        path = file.path(tempdir(),
                         "/opt_sw_norm_summary_modified.html")
      )
    )
  })

  ##### Plots ##################################################################

  output$opt_sw_norm_exact_equal    <- shiny::renderPlot({
    input$opt_sw_norm_update
    if (shiny::isolate(input$opt_sw_norm_C)) {
      int_opt_sw_norm()$exact_equal +
        ggplot2::theme(text = ggplot2::element_text(size = 16)) +
        ggplot2::scale_fill_brewer()
    }
  })

  output$opt_sw_norm_exact_weighted <- shiny::renderPlot({
    input$opt_sw_norm_update
    if (shiny::isolate(input$opt_sw_norm_C)) {
      int_opt_sw_norm()$exact_weighted +
        ggplot2::theme(text = ggplot2::element_text(size = 16)) +
        ggplot2::scale_fill_brewer()
    }
  })

  output$opt_sw_norm_rounded        <- shiny::renderPlot({
    input$opt_sw_norm_update
    if (shiny::isolate(input$opt_sw_norm_C)) {
      int_opt_sw_norm()$rounded +
        ggplot2::theme(text = ggplot2::element_text(size = 16)) +
        ggplot2::scale_fill_brewer()
    }
  })

  ##### Report #################################################################

  output$opt_sw_norm_report <- shiny::downloadHandler(
    filename = function() {
      paste(input$opt_sw_norm_filename, sep = '.',
            switch(input$opt_sw_norm_format,
                   pdf  = "pdf",
                   html = "html",
                   word = "docx"
            )
      )
    },
    content  = function(file) {
      tempReport <- file.path(tempdir(), "opt_sw_norm_report.Rmd")
      file.copy("opt_sw_norm_report.Rmd", tempReport, overwrite = TRUE)
      params     <- list(C                       = input$opt_sw_norm_C,
                         Ti                      = input$opt_sw_norm_Ti,
                         m                       = input$opt_sw_norm_m,
                         rho0                    = input$opt_sw_norm_rho0,
                         r0                      = input$opt_sw_norm_r0,
                         r                       = input$opt_sw_norm_r,
                         extreme_seq             =
                           input$opt_sw_norm_extreme_seq,
                         time_effect             =
                           input$opt_sw_norm_time_effect,
                         optimal_num_exact       =
                           int_opt_sw_norm()$optimal_num_exact,
                         optimal_num_rounded     =
                           int_opt_sw_norm()$optimal_num_rounded,
                         optimal_weights_exact   =
                           int_opt_sw_norm()$optimal_weights_exact,
                         optimal_weights_rounded =
                           int_opt_sw_norm()$optimal_weights_rounded,
                         exact_equal             =
                           int_opt_sw_norm()$exact_equal,
                         exact_weighted          =
                           int_opt_sw_norm()$exact_weighted,
                         rounded                 = int_opt_sw_norm()$rounded)
      rmarkdown::render(tempReport,
                        output_format = paste0(input$opt_sw_norm_format,
                                               "_document"),
                        output_file   = file,
                        params        = params,
                        envir         = new.env(parent = globalenv())
      )
    }
  )

  ##### Session Info ###########################################################

  output$opt_sw_norm_debug  <- shiny::renderPrint({
    utils::sessionInfo()
  })

  ##### Close set-up ###########################################################

  shiny::observe({
    shiny::reactiveValuesToList(input)
    session$doBookmark()
  })
  shiny::onBookmarked(updateQueryString)

  sever::sever(
    html = tagList(
      h1("Whoops...you have been disconnected"),
      p("There are several reasons this could happen. You can try reconnecting",
        "by clicking the button below. If this problem persists, please ",
        "email:"),
      HTML('<a href="mailto:michael.grayling@newcastle.ac.uk">michael.grayling@newcastle.ac.uk</a>'),
      p(),
      sever::reload_button("Reconnect", "default")
    ),
    bg_color = "rgba(0,0,0,.5)",
    box      = TRUE,
    color    = "black")

  session$onSessionEnded(stopApp)

}

shiny::shinyApp(ui, server, enableBookmarking = "url")
