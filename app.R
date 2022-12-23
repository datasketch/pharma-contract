library(airtabler)
library(dsapptools)
library(shiny)
library(shinypanels)


ui <- panelsPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app.min.css"),
    tags$script(src="app.min.js")
  ),
  panel(title = "Filters",
        id = "pharma-panel",
        can_collapse = TRUE,
        width = 350,
        body =  div(
          uiOutput("menu_buttons")
          #uiOutput("controls")
        )
  ),
  panel(title = "Visualization",
        id = "pharma-panel",
        can_collapse = FALSE,
        header_right = div(
          class = "head-viz",
          div(class = "viz-style",
              uiOutput("viz_icons")),
          uiOutput("downloads")
        ),
        body =  div(
          verbatimTextOutput("test")
          #uiOutput("viz_view")
        )
  ),
  panel(title = "Detail",
        id = "pharma-detail",
        can_collapse = TRUE,
        width = 300,
        color = "chardonnay",
        body =  div(
          shinycustomloader::withLoader(
            uiOutput("click_info"),
            type = "html", loader = "loader4"
          )
        ),
        footer =  div(class = "footer-logos",
                      tags$a(
                        href="https://www.datasketch.co", target="blank",
                        img(src= 'img/logos/logo_ds.svg',
                            align = "left", width = 130, height = 70)),
                      img(src= 'img/logos/logo_ins.svg',
                          width = 150, height = 150)
        )
  )
)


server <- function(input, output, session) {
  
  
  
  # controls ----------------------------------------------------------------
  
  list_menu <- reactive({
    dsapptools:::make_buttons(c("a", "b", "c"),
                              c("<div class = 'info-menu'> <div class = 'arrow-menu'>&#x2192;</div>Request</div>",
                                "<div class = 'info-menu'> <div class = 'arrow-menu'>&#x2192;</div>Appeals</div>",
                                "<div class = 'info-menu'> <div class = 'arrow-menu'>&#x2192;</div>Contracts details</div>"),
                              class_buttons = "menu-line")
  })
  
  
  chosen_menu <- reactiveValues(id = NULL)
  observeEvent(input$last_click,{
    chosen_menu$id <- input$last_click
  })
  
  
  observe({
    if(is.null(list_menu())) return()
    l <- list_menu()
    last_btn <- chosen_menu$id
    if (!is.null(last_btn)) {
      button_id <- which(c("a", "b", "c") %in% last_btn)
      print(button_id)
      l[[button_id]] <- gsub("needed", "needed basic_active", l[[button_id]])
      l[[button_id]] <- gsub("arrow-menu", "arrow-menu-active", l[[button_id]])
      l[[button_id]] <- HTML(paste0(paste(l[[button_id]], collapse = '')))
      if (last_btn == "a") {
        l[[button_id]] <- gsub("menu-line", "", l[[button_id]])
        req(data_load())
        df <- data_load()
        l[[button_id]] <- div(l[[button_id]],
                              selectizeInput("request_countries", 
                                             "Country",
                                             c("All", setdiff(unique(df$`Request Country`), NA)),
                                             multiple = TRUE,
                                             options = list(
                                               placeholder = "All",
                                               plugins = list(
                                                 "remove_button",
                                                 "drag_drop"))),
                              selectizeInput("request_status", 
                                             "Status",
                                             c("All", setdiff(unique(df$Status), NA)),
                                             multiple = TRUE,
                                             options = list(
                                               placeholder = "All",
                                               plugins = list(
                                                 "remove_button",
                                                 "drag_drop"))),
                              div(class = "menu-line")
        )
      }
      if (last_btn == "b") {
        l[[button_id]] <- gsub("menu-line", "", l[[button_id]])
        req(data_load())
        df <- data_load()
        l[[button_id]] <- div(l[[button_id]],
                              selectizeInput("appeals_countries", 
                                             "Country",
                                             c("All", setdiff(unique(df$Country), NA)),
                                             multiple = TRUE,
                                             options = list(
                                               placeholder = "All",
                                               plugins = list(
                                                 "remove_button",
                                                 "drag_drop"))),
                              selectizeInput("appeals_status", 
                                             "Status",
                                             c("All", setdiff(unique(df$Status), NA)),
                                             multiple = TRUE,
                                             options = list(
                                               placeholder = "All",
                                               plugins = list(
                                                 "remove_button",
                                                 "drag_drop"))),
                              div(class = "menu-line")
        )
      }
      if (last_btn == "c") {
        l[[button_id]] <- gsub("menu-line", "", l[[button_id]])
        req(data_load())
        df <- data_load()
        l[[button_id]] <- div(l[[button_id]],
                              selectizeInput("contracts_countries", 
                                             "Country",
                                             c("All", setdiff(unique(df$Country), NA)),
                                             multiple = TRUE,
                                             options = list(
                                               placeholder = "All",
                                               plugins = list(
                                                 "remove_button",
                                                 "drag_drop"))),
                              selectizeInput("contracts_supplier", 
                                             "Supplier",
                                             c("All", setdiff(unique(df$Supplier), NA)),
                                             multiple = TRUE,
                                             options = list(
                                               placeholder = "All",
                                               plugins = list(
                                                 "remove_button",
                                                 "drag_drop"))),
                              selectizeInput("contracts_vaccine", 
                                             "Vaccine",
                                             c("All", setdiff(unique(df$Vaccine), NA)),
                                             multiple = TRUE,
                                             options = list(
                                               placeholder = "All",
                                               plugins = list(
                                                 "remove_button",
                                                 "drag_drop"))),
                              div(class = "menu-line")
        )
      }
    }
    output$menu_buttons <- renderUI({
      l
    })
  })
  
  
  # data --------------------------------------------------------------------
  
  data_load <-
    isolate({
      reactivePoll(1000,
                   session,
                   checkFunc = function() {
                     if (is.null(chosen_menu$id)) return()
                     df <- NULL
                     if (chosen_menu$id == "a") {
                       df <-    "data/data_request.RData"
                     }
                     if (chosen_menu$id %in% "b") {
                       df <- "data/data_appeals.RData"
                     }
                     if (chosen_menu$id %in% "c") {
                       df <- "data/data_contracts.RData"
                     }  
                     if (file.exists(df))
                       file.info(df)$uid
                     else
                       shinyalert(title = "file",text = "Archivo no encontrado")
                   },
                   
                   valueFunc = function() {
                     if (is.null(chosen_menu$id)) return()
                     load_data <- NULL
                     if (chosen_menu$id == "a") {
                       load_data <- "load_request_data.R"
                     }
                     if (chosen_menu$id %in% "b") {
                       load_data <- "load_appeals_data.R"
                     }
                     if (chosen_menu$id %in% "c") {
                       load_data <- "load_contracts_data.R"
                     }  
                     source(load_data)$value
                   }
      )
    })
  
  
  # updates all -------------------------------------------------------------
  
  observe({
    if ("All" %in% input$request_countries) {
      updateSelectizeInput(session, inputId = "request_countries", selected = "All")
    }
    if ("All" %in% input$request_status) {
      updateSelectizeInput(session, inputId = "request_status", selected = "All")
    }
    if ("All" %in% input$appeals_countries) {
      updateSelectizeInput(session, inputId = "appeals_countries", selected = "All")
    }
    if ("All" %in% input$appeals_status) {
      updateSelectizeInput(session, inputId = "appeals_status", selected = "All")
    }
    if ("All" %in% input$contracts_countries) {
      updateSelectizeInput(session, inputId = "contracts_countries", selected = "All")
    }
    if ("All" %in% input$contracts_supplier) {
      updateSelectizeInput(session, inputId = "contracts_supplier", selected = "All")
    }
    if ("All" %in% input$contracts_vaccine) {
      updateSelectizeInput(session, inputId = "contracts_vaccine", selected = "All")
    }
  })
  
  
  # Data filter -------------------------------------------------------------
  
  list_inputs <- reactive({
    if (is.null(chosen_menu$id)) return()
    ls <- NULL
    if (chosen_menu$id == "a") {
    ls <-  list("Request Country" = input$request_countries,
           "Status" = input$request_status)
    }
    if (chosen_menu$id == "b") {
    ls <-  list("Country" = input$appeals_countries,
           "Status" = input$appeals_status)
    }
    if (chosen_menu$id == "c") {
    ls <-  list("Country" = input$contracts_countries,
           "Supplier" = input$contracts_supplier,
           "Vaccine" = input$contracts_vaccine)
    }
    ls
  })
  
  dic_load <- reactive({
    if (is.null(chosen_menu$id)) return()
    dic <- NULL
    if (chosen_menu$id == "a") {
      dic <- load("data/dic_request.RData")
      dic <- dic_request
    }
    if (chosen_menu$id == "b") {
      dic <- load("data/dic_appeals.RData")
      dic <- dic_appeals
    }
    if (chosen_menu$id == "c") {
      dic <- load("data/dic_contracts.RData")
      dic <- dic_contracts
    }
    dic
  })
  
  data_down <- reactive({
    req(list_inputs())
    ls <- list_inputs()
    df <- dsapptools::data_filter(data = dplyr::as_tibble(data_load()),
                                  dic = dic_load(),
                                  var_inputs = ls,
                                  special_placeholder = "All",
                                  .id = NULL)
    df
  })
  
  # data to viz -------------------------------------------------------------
  
  # data_viz <- reactive({
  #   req(actual_but$active)
  #   if (actual_but$active == "table") return()
  #   req(data_down())
  #   if (nrow(data_down()) == 0) return()
  #   data_down() |>
  #     variable_selection(viz = actual_but$active) |>
  #     var_aggregation(dic_pharma, Total = dplyr::n())
  # })
  
  
  output$test <- renderPrint({
    data_down()
  })
  
}

shinyApp(ui, server)



