# SA APP


library(shiny)
library(shinyjs)
library(DT)
library(rstudioapi) 
library(shinythemes)
library(ggplot2)
library(gridExtra)
library(grid)
library(markdown)
#rmarkdown::render("RMarkdownFile.Rmd")

# Define UI for dataset viewer app
ui <- navbarPage(title = "SalmonellaLAB",theme = shinytheme("flatly"),
                 navbarMenu("Models", icon = icon("fa-solid fa-chart-line",verify_fa = FALSE),
                            tabPanel("Run",
                                     fluidPage(
                                       # App title ----
                                       shinyjs::useShinyjs(),
                                       # Sidebar layout with input and output definitions ----
                                       sidebarLayout(
                                         
                                         # Sidebar panel for inputs ----
                                         sidebarPanel(style = "background-color: #79a4b6;",
                                                      # Inputs
                                                      wellPanel(style = "background-color: #79a4b6;",
                                                                selectInput("input_type", "Choose model",
                                                                            c("Import model" = 1,"Pikma model" = 2, "SA-model" = 3)),
                                                                # UI inputs
                                                                uiOutput("select"),
                                                                uiOutput("select2"),
                                                                uiOutput("select3"),
                                                                actionButton('help', 'Help')
                                                      ),
                                                      
                                                      wellPanel(style = "background-color: #79a4b6;",
                                                                numericInput("iterations", "Number of iterations", 50000, min = 1, max = 1000000),
                                                                numericInput("burn_in", "Number of burn-ins", 10000, min = 1, max = 500000),
                                                                actionButton("run","Run", 
                                                                             style="color: #080808; background-color: #fff; border-color: #fff;"),
                                                                uiOutput("save_results"),
                                                                uiOutput("select4")
                                                      )
                                         ),
                                         
                                         # Main panel for displaying results
                                         mainPanel(
                                           fluidRow(
                                             column(10,
                                                    verbatimTextOutput("text"),
                                                    tags$head(tags$style("#text{ overflow-y:scroll;max-height: 500px}"))
                                             )
                                           )
                                         )
                                       )
                                     )
                            ),
                            
                            # Model diagnostic
                            tabPanel("Diagnostic",
                                     fluidPage(
                                       shinyjs::useShinyjs(),
                                       # Sidebar layout with input and output definitions ----
                                       sidebarLayout(
                                         
                                         # Sidebar panel for inputs ----
                                         sidebarPanel(style = "background-color: #79a4b6;",
                                                      # Inputs
                                                      wellPanel(style = "background-color: #79a4b6;",
                                                                selectInput("selected_model_diagnostic", "Choose model",
                                                                            c("Import model" = 1,"Pikma model" = 2, "SA-model" = 3)),
                                                                uiOutput("select6"),
                                                                uiOutput("select7"),
                                                                uiOutput("select8"),
                                                                actionButton("show_diagnostic", "Show results")
                                                                
                                                      ),
                                                      
                                         ),
                                         
                                         # Main panel for displaying results
                                         mainPanel(
                                           plotOutput("result_view3")
                                         )
                                       )
                                     )
                            ),
                            
                            tabPanel("Summary",
                                     fluidPage(
                                       # App title ----
                                       #titlePanel(titlePanel(title=div(img(src="SalmonellaLab.png", width = 50)))),
                                       
                                       shinyjs::useShinyjs(),
                                       # Sidebar layout with input and output definitions ----
                                       sidebarLayout(
                                         
                                         # Sidebar panel for inputs ----
                                         sidebarPanel(style = "background-color: #79a4b6;",
                                                      # Inputs
                                                      wellPanel(style = "background-color: #79a4b6;",
                                                                selectInput("selected_model_summary", "Choose model",
                                                                            c("Import model" = 1,"Pikma model" = 2, "SA-model" = 3)),
                                                                uiOutput("select9"),
                                                                uiOutput("summary_type"),
                                                                uiOutput("extra_summary_parameter"),
                                                                uiOutput("summary_selection"),
                                                                actionButton('summary_help', 'Help'),
                                                                actionButton("show_summary", "Show results"),
                                                                actionButton("save_summary_plot","Save chart")
                                                      ),
                                                      
                                         ),
                                         # Main panel for displaying results
                                         mainPanel(
                                           fluidRow(
                                             column(12,
                                                    verbatimTextOutput("summary_text"),

                                                    uiOutput("summary_output", width = "100%"),
                                                    uiOutput("summary_output2")
                                             )
                                           )
                                         )
                                       )
                                     )
                            )
                 ),
                 
                 # Typing run page
                 tabPanel("Subtype",icon = icon("fa-regular fa-virus",verify_fa = FALSE),
                          fluidPage(
                            shinyjs::useShinyjs(),
                            # Sidebar layout with input and output definitions ----
                            sidebarLayout(
                              # Sidebar panel for inputs ----
                              sidebarPanel(style = "background-color: #79a4b6;",
                                           fileInput("human_data","Upload human data file",
                                                     accept = ".xlsx"),
                                           actionButton("initialize_untrimmed_file","Initialize"),
                                           fileInput("trim_human_data","Upload trimmed human data file",
                                                     accept = ".xlsx"),
                                           fileInput("trim_animal_data","Upload Source data file",
                                                     accept = ".xlsx"),
                                           fileInput("human_and_source_cases_earlier","Upload old human and source data file",
                                                     accept = ".xlsx"),
                                           fileInput("human_cases_earlier","Upload old only human data file",
                                                     accept = ".xlsx"),
                                           actionButton("initialize_sa_model","Initialize SA-model")
                                           
                              ),
                              mainPanel(
                                fluidRow(
                                  column(10,
                                         verbatimTextOutput("ty_text"),
                                         tags$head(tags$style("#ty_text{ overflow-y:scroll;max-height: 200px}"))
                                  )
                                ),
                                fluidRow(
                                  column(6,
                                         DTOutput('ty_table')
                                  )
                                )
                              )
                            )
                          )
                 ),
                 ## ULJAS PAGE
                 navbarMenu("Uljas",icon = icon("fa-solid fa-database",verify_fa = FALSE),
                            tabPanel("Search",
                                     fluidPage(
                                       shinyjs::useShinyjs(),
                                       sidebarLayout(
                                         # Sidebar panel for inputs ----
                                         sidebarPanel(style = "background-color: #79a4b6;",
                                                      fileInput("Source_ids","Upload Sources ids",
                                                                accept = ".xlsx"),
                                                      numericInput("uljas_year", "Select a year", 2020, min = 2002, max = 2100),
                                                      actionButton("uljas_initialize","Initialize"),
                                                      actionButton("uljas_search","Search")
                                         ),
                                         mainPanel(
                                           fluidRow(
                                             column(12,verbatimTextOutput("ul_text"),
                                                    tags$head(tags$style("#ul_text{ overflow-y:scroll;max-height: 200px}"))
                                             )
                                           ),
                                           fluidRow(
                                             column(12,DTOutput('ul_table')
                                             )
                                           )
                                         )
                                       )
                                     )         
                            ),
                            tabPanel("Uppgrade table",
                                     fluidPage(
                                       shinyjs::useShinyjs(),
                                       sidebarLayout(
                                         sidebarPanel(style = "background-color: #79a4b6;",
                                                      numericInput("id",
                                                                   "ID",
                                                                   min = 1,
                                                                   max = 100000000,
                                                                   value = 10),
                                                      selectInput("tuore",label = "Tuore", choices = list(TRUE,FALSE)),
                                                      selectInput("luullinen",label = "Luullinen", choices = list(TRUE,FALSE)),
                                                      selectInput("kategoria",label = "Kategoria", choices = list("Broileri","Kalkkuna","Nauta","Sika")),
                                                      actionButton("add_btn", "Add"),
                                                      actionButton("delete_btn", "Delete"),
                                                      actionButton("undo","Undo")
                                         ),
                                         mainPanel(
                                           DTOutput("id_inf_list")
                                         )
                                       )
                                     )
                            )
                 ),
                 tabPanel("About",icon = icon("fa-solid fa-file",verify_fa = FALSE),
                          fluidRow(htmltools::tags$iframe(src = "SalmonellaLAB_manual.html", width = '100%',  height = 1000,  style = "border:none;"))
                 )
)

server <- function(input, output) {
  line_break <- htmltools::HTML("<br><br>")
  
  
  # Set the working directory to current source file 
  setwd(dirname(getActiveDocumentContext()$path))
  

  source(paste(getwd(),'/import_run.R',sep = ''),encoding="UTF-8")
  source(paste(getwd(),'/pikma_run.R',sep = ''),encoding="UTF-8")
  source(paste(getwd(),'/typingInf_run.R',sep = ''),encoding="UTF-8")
  source(paste(getwd(),'/res_plot.R', sep = ''),encoding="UTF-8")
  source(paste(getwd(), '/diagnostic.R', sep = ''), encoding = "UTF-8")
  source(paste(getwd(), '/sourceFinder.R', sep = ''),encoding="UTF-8")
  source(paste(getwd(), '/sa_run.R', sep = ''), encoding = "UTF-8")
  
  
  stats <- NULL
  
 # Typing ----------------------------------------------------------------------
  
  trim_dataset <- reactiveValues(data = NULL)
  source_cases <- reactiveValues(data = NULL)
  old_human_and_source_cases <- reactiveValues(data = NULL)
  old_human_cases <- reactiveValues(data = NULL)
  ty_dataset <- reactiveValues(data = NULL)
  
  # Initialize typing information
  observeEvent(input$initialize_untrimmed_file,{
    shinyjs::html(id = "text", html = "")
    inf <- trim_hc(input$human_data$name)
    ty_dataset$data <- inf
  })
  
  observeEvent(input$trim_human_data,{
    shinyjs::html(id = "text", html = "")
    trim_dataset$data <- input$trim_human_data$name
  })
  
  observeEvent(input$trim_animal_data,{
    shinyjs::html(id = "text", html = "")
    source_cases$data <- input$trim_animal_data$name
  })
  
  observeEvent(input$human_and_source_cases_earlier,{
    shinyjs::html(id = "text", html = "")
    old_human_and_source_cases$data <- input$human_and_source_cases_earlier$name
  })
  
  observeEvent(input$human_cases_earlier,{
    shinyjs::html(id = "text", html = "")
    old_human_cases$data <- input$human_cases_earlier$name
  })
  
  observeEvent(input$initialize_sa_model,{
    shinyjs::html(id = "text", html = "")
    typingInf_run(trim_dataset$data,source_cases$data,old_human_and_source_cases$data,old_human_cases$data)
    
  })
  # Output text to typing inf page
  output$ty_text <- renderText(" ")
  # Initial dataset to typingIng page 

  # Rendertable for the typing inf results
  output$ty_table = renderDataTable(server = FALSE,
                                    ty_dataset$data,
                                    rownames = FALSE,
                                    extensions = 'Buttons',
                                    
                                    options = list(
                                      scrollY="300px",
                                      dom = 'Bt',
                                      buttons = list(
                                        list(extend = 'excel', title = NULL)
                                      )
                                    )
                    )
  


  
  observeEvent(input$help,{
    switch(input$input_type,
           # Import model
           "1" =      showModal(modalDialog(
             title = "Example of datasets needed for the Import model",
             "Prevalence results (Chicken)",
             line_break,
             htmltools::tags$iframe(src = "tuonti.png", width = '100%',  height = 350,  style = "border:none;"),
             tags$hr(),
             "Import volumes",
             line_break,
             htmltools::tags$iframe(src = "tuontimaarat.png", width = '100%',  height = 350,  style = "border:none;"),
             "Additional guarantees",
             line_break,
             htmltools::tags$iframe(src = "AG2.png", width = '100%',  height = 350,  style = "border:none;")
           )),
           
           # Pikma model
           "2" =      showModal(modalDialog(
             title = "Example of dataset needed for the pikma model (Chicken)",
             line_break,
             htmltools::tags$iframe(src = "kotimainen2.png", width = '100%',  height = 350,  style = "border:none;"),
             
           )),
           
           "3" = showModal(modalDialog(
             title = "Help!",
             "Information",
           )),
    )

  })
  
  
  
  
  
  # File upload ----------------------------------------------------------------
  
  
  # File loading tool for different models 1 
  output$select <- renderUI({
    if (is.null(input$input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type,
           # Import model
           "1" = fileInput("Typing_inf","Upload Source(s) prevalence results",
                           accept = ".xlsx", multiple = TRUE),
           
           # Pikma model
           "2" = fileInput("Domestic_data","Upload Source information",
                           accept = ".xlsx", multiple = TRUE)
    )
    
  })
  
  # File loading tool for different models 2
  output$select2 <- renderUI({
    if (is.null(input$input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type,
           
           # Import model
           "1" =  fileInput("Ags","Upload Additional guarantees information",
                            accept = ".xlsx")
    )
    
  })
  # File loading tool for different models 3
  output$select3 <- renderUI({
    if (is.null(input$input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type,
           
           # Import model
           "1" =  fileInput("Import_volume","Upload import volume information",
                            accept = ".xlsx")
    )
    
  })

  # Checkbox to add new year
  output$select4 <- renderUI({
    if (is.null(input$input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type,
           
           # Import model
           "1" =  checkboxInput("new_year","Add new year",FALSE),
           # pikma model
           "2" =  checkboxInput("new_year","Add new year",FALSE)
    )
    
  })
  
  # Diagnostic page ------------------------------------------------------------
  
  # Parameter selection for SA-model
  output$select7 <- renderUI({
    if (is.null(input$selected_model_diagnostic))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$selected_model_diagnostic,
           "1" = selectizeInput("source", "Select a source:",
                                choices = c("Chicken" = "Chicken","Turkey" = "Turkey", "Beef" = "Beef","Pork" = "Pork"),
                                multiple = TRUE
           ),
           "2" = selectizeInput("source", "Select a source:",
                                choices = c("Chicken" = "Chicken","Turkey" = "Turkey", "Beef" = "Beef","Pork" = "Pork"),
                                multiple = TRUE
           ),
           
           "3" = selectizeInput("source", "Select a source:",
                                choices = c("Chicken (domestic)" =  "Chicken(d)", "Turkey(domestic)" = "Turkey(d)","Beef (domestic)" = "Beef(d)",
                                            "Pork (domestic)" = "Pork(d)", "Chicken(import)" =  "Chicken(i)", "Turkey(import)" =  "Turkey(i)",
                                            "Beef(import)" =  "Beef(i)","Pork(import)" ="Pork(i)"),
                                multiple = TRUE)
    )
  })
  
  output$select8 <- renderUI({
    if(is.null(input$selected_model_diagnostic))
      return()
    switch (input$selected_model_diagnostic,
            "2" =   selectInput("select_year", label = "Select year(s)", 
                                choices = array_to_list(get_current_years()), 
                                multiple = TRUE),
            "3" =   selectInput("select_year", label = "Select year(s)", 
                                choices = array_to_list(get_current_years()), 
                                multiple = TRUE)
    )
  })
  
  output$select6 <- renderUI({
    if(is.null(input$selected_model_diagnostic))
      return()
    switch (input$selected_model_diagnostic,
            "1" =   selectInput("select_parameter", label = "Select variable", 
                                choices = list(" " = "empty", "Cont_DC_total" = "Cont_DC_total"), 
                                selected = 1),
            "2" =   selectInput("select_parameter", label = "Select variable", 
                                choices = list(" " = "empty", "Cont_DC_total" = "Cont_DC_total"), 
                                selected = 1),
            "3"=   selectInput("select_parameter", label = "Select variable", 
                               choices = c(" " = "empty" ,"Ep" = "Ep", "Ep2" = "Ep2", "Epp" = "Epp",
                                           "Epp2" = "Epp2"), 
                               selected = 1)
            
            
    )
  })
  
  # Summmary page --------------------------------------------------------------
  
  # Parameter selection for SA-model
  output$select9 <- renderUI({
    if (is.null(input$selected_model_summary))
      return()
    
    switch(input$selected_model_summary,
           "1" = selectizeInput("summary_parameter","Choose variable:",
                                choices = c("Contamined meat " = "cont.DC.total"
                                            )
                                ),
           "2" = selectizeInput("summary_parameter","Choose variable:",
                                choices = c("cont.DC.total" = "cont.DC.total",
                                            "slaughpreva" = "slaughpreva",
                                            "p.cc" = "p.cc",
                                            "p.cont.DC" = "p.cont.DC"
                                )
           ),
           "3" = selectizeInput("summary_parameter", "Choose variable:",
                                choices = c("Ep" = "Ep", "Ep2" = "Ep2", "Epp" = "Epp",
                                            "Epp2" = "Epp2", "MEp" = "MEp", "MEp2" = "MEp2", "MEpp" = "MEpp","MEpp2" = "MEpp2", "q" = "q", "type" = "type"))
    )
  })
  
  output$summary_text <- renderText(convert_parameter_name(input$summary_parameter))
  
  output$summary_type <- renderUI({
    if(is.null(input$summary_parameter))
      return()
    if(input$summary_parameter %in% c("Ep", "Ep2","Epp","Epp2","cont.DC.total","slaughpreva","p.cc","p.cont.DC")){
      selectizeInput("summary_graph","Select summary type",
                     choices = c("Line plot" = "line_plot_summary",
                                 "Box plot" = "boxplot_summary"))
    }
    else{
      selectizeInput("summary_graph","Select summary type",
                     choices = c("Box plot" = "boxplot_summary"))
                                 
    }

  })
  
  output$extra_summary_parameter <- renderUI({
    if(is.null(input$summary_parameter) | is.null(input$summary_graph))
      return()
    if(input$summary_parameter %in% c("Ep", "Ep2","Epp","Epp2","cont.DC.total","slaughpreva","p.cc","p.cont.DC")){
      if(input$summary_graph == "line_plot_summary"){
        selectizeInput("moving_average_type","Select smoothing type:",
                       choices = c("Moving average" = "ma",
                                   "Centered moving average" = "cma",
                                   "Exponential smoothing" = "es")
        )
      }
      else if(input$summary_graph == "boxplot_summary"){

        if(input$summary_parameter %in% c("Ep", "Ep2","Epp","Epp2")){
          radioButtons("selected_origin", "Select origin:",
                       c("Import" = "import",
                         "Domestic" = "domestic"))
        }
        else if(input$summary_parameter %in% c("cont.DC.total","slaughpreva","p.cc","p.cont.DC")){
          radioButtons("selected_origin", "Select origin:",
                       c("Domestic" = "domestic"))
        }
        else{return()}
        
      }else{
        return()
      }

    }
  })
  
  output$summary_selection <- renderUI({
    if(is.null(input$summary_parameter) | is.null(input$summary_graph) | is.null(input$moving_average_type) ){
      return()
    }
    if(input$summary_parameter %in% c("Ep", "Ep2","Epp","Epp2","cont.DC.total","slaughpreva","p.cc","p.cont.DC")){
      if(input$summary_graph == "line_plot_summary"){
        if(input$moving_average_type %in% c("ma","cma")){
          sliderInput("moving_average_value", "Select moving average value:",
                      min = 1, max = 6, value = 1)
        }
        else{
          sliderInput("moving_average_value", "Select moving average value:",
                      min = 0.01, max = 0.99, value = 0.01)
        }
      }
      else if(input$summary_graph == "boxplot_summary"){
        radioButtons("grouped_by", "Grouped by:",
                     c("Source" = "source",
                       "Year" = "year"))
      }else{
        return()
      }

    }



  })
  
  observeEvent(input$summary_help,{
    if (is.null(input$selected_model_summary))
      return()
    switch(input$selected_model_summary,
           # Import model
           "1" =  showModal(modalDialog(
             title = "Parameter definitions"

           )),
           
           # Pikma model
           "2" =  showModal(modalDialog(
             title = "Help2!",
             "Information",
           )),
           
           "3" = showModal(modalDialog(
             htmltools::tags$iframe(src = "sa_parameter_definitions.html", width = '100%',  height = 1000,  style = "border:none;")
           ))
    )
    
  })
  
  
  # 
  last_summary <- reactiveValues(plot = NULL)
  
  summary_parameter2 <- eventReactive(input$summary_parameter,{
    input$summary_parameter
  })
  
  # Summary button
  summary_parameter <- eventReactive(input$show_summary,{

    input$summary_parameter
  })
  
  # Save last summary plot
  observeEvent(input$save_summary_plot,{
    save_plot(summary_parameter(), last_summary$plot)
  })
  
  output$summary_output <- renderUI({

    if (is.null(input$selected_model_summary))
      return()

    if(summary_parameter() %in% c("Ep","Ep2","Epp","Epp2", "cont.DC.total","slaughpreva","p.cc", "p.cont.DC")){
      if(input$summary_graph == "line_plot_summary"){
          # Isolate moving average value so plot don't update without pressing the button

          last_summary$plot = proportion_plot(
            isolate(summary_parameter()),isolate(input$moving_average_type),
            isolate(input$moving_average_value))
      }
      if(input$summary_graph == "boxplot_summary"){
        last_summary$plot = sa_boxplot(
          isolate(summary_parameter()), isolate(input$selected_origin), isolate(input$grouped_by)
        )
      }
      

    }
    else if(summary_parameter() %in% c("MEp","MEp2","MEpp","MEpp2", "q")){
      last_summary$plot = mean_proportion_plot(isolate(summary_parameter()))
    }
    else if(summary_parameter() == "type"){
      last_summary$plot = infection_plot(isolate(summary_parameter()))
    }
    else{
      last_summary$plot = NULL
      return()
    }
    if( !is.null(last_summary$plot)){
      renderPlot(last_summary$plot, height = 450, width = 925)
    }

      
    
    
  })

  

  
  # Button disables
  observeEvent(input,{
    shinyjs::disable("save_results")
    
  })
  
  # Model run ------------------------------------------------------------------
  
  # Run button to run models
  observeEvent(input$run,{
    switch(input$input_type,
           # Run import model
           "1" = {
             shinyjs::html(id = "text", html = "")
             import_volume <- input$Import_volume$name
             AG_data <- input$Ags$name
             typing_data <- input$Typing_inf$name
             stats <<- import_run(input$iterations, input$burn_in, import_volume, AG_data, typing_data)
             if(!is.null(stats)){shinyjs::enable("uppgrade_tables")}
             
             
           },
           
           # Run Pikma model
           "2" = {
             shinyjs::html(id = "text", html = "")
             dom_data <- input$Domestic_data$name
             stats <<- pikma_run(input$iterations, input$burn_in,dom_data)
             # Save only when model is run successfully
             if(!is.null(stats)){shinyjs::enable("uppgrade_tables")}
             
             
           },
           
           # Run SA-model
           "3" = {
             shinyjs::html(id = "text", html = "")
             sa_initialize()
             stats <<- sa_run(input$iterations, input$burn_in)

           }
    )
  })
  
  # Save button
  observeEvent(input$uppgrade_tables,{
    switch(input$input_type,
           # Results for import model
           "1" = {
             if(! is.null(stats)){import_update(stats$Em, stats$Esd,
                                                input$new_year)}
             shinyjs::disable("uppgrade_tables")
             

           },
           # Results for pikma model
           "2" = {
             if(! is.null(stats)){ pikma_update(stats$Em, stats$Esd,
                                                input$new_year)}
             shinyjs::disable("uppgrade_tables")
           }
    )
  })
  
  output$save_results <- renderUI({
    if(is.null(input$input_type)){
      return()
    }
    if(input$input_type %in% c("1","2")){
      disabled(actionButton('uppgrade_tables', 'Uppgrade tables',style="color: #080808; background-color: #fff; border-color: #fff;"))
    }
    
  })
  
  diagnostic_years <-eventReactive(input$show_diagnostic,{
    as.integer(input$select_year)
  })
  diagnostic_sources <- eventReactive(input$show_diagnostic,{
    input$source
  })
  diagnostic_model <- eventReactive(input$show_diagnostic,{
    input$selected_model_diagnostic
  })
  diagnostic_parameter <- eventReactive(input$show_diagnostic,{
    input$select_parameter
  })
  
  # Model diagnostic -----------------------------------------------------------
  
  output$result_view3 <- renderPlot(model_diagnostic(diagnostic_years(), diagnostic_sources(),diagnostic_parameter(), diagnostic_model()))
  
  output$summary_output2 <- renderUI({
    if(is.null(input$selected_model_summary))
      return()
    switch(input$selected_model_summary,
           "3" = renderDataTable(server = TRUE,
                                 tab_plot(summary_parameter()),
                                 rownames = TRUE,
                                 options = list(
                                   paging = TRUE,
                                   searching = TRUE,
                                   fixedColumns = TRUE,
                                   autoWidth = TRUE,
                                   ordering = TRUE,
                                   dom = 'fip'
                                 ),
                                 class = "display"
           )
    )
  })
  
  observeEvent(input$input_type,{
    rv_text$data <- " "
  })
  
  
  # Text view
  rv_text <- reactiveValues(data = " ")
  output$text <- renderText(rv_text$data)
  

  
  # Uljas page -----------------------------------------------------------------
  
  output$ul_text <- renderText(" ")
  ul_dataset <- reactiveValues(data = NULL)
  
  # Initialize button for uljas data
  observeEvent(input$uljas_initialize,{
    sourceFinder_initialize(input$Source_ids$name)
  })
  
  # Search button for uljas data
  observeEvent(input$uljas_search,{
    ul_dataset$data <- sourceFinder(input$uljas_year,input$Source_ids$name)
  }
  )
  output$ul_table = renderDataTable(server = FALSE,
                                    ul_dataset$data,
                                    editable = TRUE, 
                                    rownames = FALSE,
                                    extensions = 'Buttons',
                                    
                                    options = list(
                                      scrollX = TRUE,
                                      paging = TRUE,
                                      searching = TRUE,
                                      fixedColumns = FALSE,
                                      autoWidth = FALSE,
                                      ordering = TRUE,
                                      dom = 'tBfipr',
                                      buttons = list(
                                        list(extend = 'excel', title = NULL)
                                      )
                                    ),
                                    class = "display"
  )
  
  # Uljas ID list -----------------------------------------------------------------------
  id_info_table <- reactiveVal(sourceFinder_info_table())
  id_info_table_copy <- sourceFinder_info_table()
  
  observeEvent(input$add_btn, {
    t = rbind(c(input$id, input$tuore, input$luullinen, input$kategoria), id_info_table())
    id_info_table(t)
  })
  
  observeEvent(input$delete_btn, {
    t = id_info_table()
    if (!is.null(input$id_inf_list_rows_selected)) {
      t <- t[-as.numeric(input$id_inf_list_rows_selected),]
    }
    id_info_table(t)
  })
  
  observeEvent(input$undo,{
    id_info_table(id_info_table_copy)
  })
  
  output$id_inf_list <- renderDT({
    datatable(id_info_table(),selection = 'single',editable = TRUE,rownames = TRUE,
              extensions = 'Buttons',
              options = list(
                scrollX = TRUE,
                paging = TRUE,
                searching = TRUE,
                fixedColumns = FALSE,
                autoWidth = FALSE,
                ordering = TRUE,
                dom = 'tBfipr',
                buttons = list(
                  list(extend = 'excel', title = NULL)
                )
              ),
              class = "display"
    )
  })
}

# Create Shiny app ----
shinyApp(ui, server)