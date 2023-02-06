webshot::install_phantomjs(force = FALSE)
library(airtabler)
library(dsapptools)
library(hgchmagic)
library(lfltmagic)
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
          
          uiOutput("viz_view")
        )
  ),
  panel(title = "Detail",
        id = "pharma-detail",
        can_collapse = TRUE,
        width = 300,
        color = "chardonnay",
        body =  div(
          #verbatimTextOutput("test")
          shinycustomloader::withLoader(
            uiOutput("click_info"),
            type = "html", loader = "loader4"
          )
        ),
        footer =  div(class = "footer-logos",
                      img(src= 'img/logos/logo_gti.png',
                          width = 70, height = 70),
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
  
  # Renderizar graficos ------------------------------------------
  
  actual_but <- reactiveValues(active = NULL)
  
  observe({
    if (is.null(input$viz_selection)) return()
    viz_rec <- c("map", "bar", "treemap", "table")
    if (input$viz_selection %in% viz_rec) {
      actual_but$active <- input$viz_selection
    } else {
      actual_but$active <- viz_rec[1]
    }
  })
  
  output$viz_icons <- renderUI({
    viz <- c("map", "bar", "treemap", "table")
    viz_label <- c("Map", "Bar", "Treemap", "Table")
    
    suppressWarnings(
      shinyinvoer::buttonImageInput("viz_selection",
                                    " ",
                                    images = viz,
                                    tooltips = viz_label,
                                    path = "img/viz_icons/",
                                    active = actual_but$active,
                                    imageStyle = list(shadow = TRUE,
                                                      borderColor = "#ffffff",
                                                      padding = "3px")
      )
    )
  })
  
  # controls ----------------------------------------------------------------
  
  list_menu <- reactive({
    dsapptools:::make_buttons(c("a", "b", "c"),
                              c("<div class = 'info-menu'> <div class = 'arrow-menu'>&#x2192;</div>Freedom of Information Requests</div>",
                                "<div class = 'info-menu'> <div class = 'arrow-menu'>&#x2192;</div>Appealed FOI Requests</div>",
                                "<div class = 'info-menu'> <div class = 'arrow-menu'>&#x2192;</div>Contracts details</div>"),
                              class_buttons = "menu-line")
  })
  
  
  chosen_menu <- reactiveValues(id = "a")
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
  
  var_to_viz <- reactive({
    if (is.null(chosen_menu$id)) return()
    req(actual_but$active)
    var <- NULL
    if (chosen_menu$id == "a") {
      if (actual_but$active %in% c("map", "treemap")) {
        var <- "Request Country"
      } else {
        var <- "Status"
      }
    }
    if (chosen_menu$id == "b") {
      if (actual_but$active %in% c("map", "treemap")) {
        var <- "Country"
      } else {
        var <- "Status"
      }
    }
    if (chosen_menu$id == "c") {
      if (actual_but$active %in% c("map", "treemap")) {
        var <- "Country"
      } else {
        var <- "Vaccine"
      }
    }
    var
  })
  
  
  data_viz <- reactive({
    req(actual_but$active)
    if (actual_but$active == "table") return()
    req(data_down())
    req(var_to_viz())
    if (nrow(data_down()) == 0) return()
    df <- dplyr::as_tibble(data_down())
    df |>
      dsapptools::variable_selection(viz = actual_but$active,
                                     path = NULL, var_to_viz()) |> 
      dsapptools::var_aggregation(dic = dic_load(), 
                                  agg = "count", 
                                  group_var = var_to_viz(),
                                  to_agg = NULL, 
                                  name = "Total")
  })
  
  # viz styles --------------------------------------------------------------
  
  viz_opts <- reactive({
    req(data_viz())
    req(actual_but$active)
    
    myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "', {id:event.point.name, timestamp: new Date().getTime()});}")
    df <- dplyr::as_tibble(data_viz())
    nBins <- max(df[[2]], na.rm = T) - 1
    if (max(df[[2]], na.rm = T) == 1) nBins <- 3

    opts <- list(
      data = df,
      orientation = "hor",
      ver_title = " ",
      hor_title = " ",
      drop_na = TRUE,
      label_wrap_legend = 100,
      label_wrap = 40,
      background_color = "#ffffff",
      axis_line_y_size = 1,
      axis_line_color = "#dbd9d9",
      grid_y_color = "#dbd9d9",
      grid_x_color = "#fafafa",
      cursor = "pointer",
      map_zoom_snap = 0.25,
      map_zoom_delta = 0.25,
      map_provider_tile = "url",
      map_extra_layout = "https://maps.geoapify.com/v1/tile/osm-bright-smooth/{z}/{x}/{y}.png?apiKey=3ccf9d5f19894b32b502485362c99163",
      map_name_layout = "osm-brigh",
      legend_position = "bottomleft",
      border_weight = 0.3,
      format_sample_num = "1,234."
    )
    
    if (actual_but$active == "map") {
      opts$na_color <- "transparent"
      opts$palette_colors <-  c("#FFF6FF", "#da3592")
      opts$map_color_scale <- "Bins"
      opts$map_bins <- nBins
    } else {
      opts$clickFunction <- htmlwidgets::JS(myFunc)
      opts$palette_colors <- "#ef4e00"
    }
    if (actual_but$active == "treemap") {
      opts$dataLabels_align <- "middle"
      opts$dataLabels_inside <- TRUE
      opts$dataLabels_show <- TRUE
      opts$legend_show <- FALSE
    }
    
    opts
  })
  
  
  # Render Viz --------------------------------------------------------------
  
  viz_down <- reactive({
    req(data_viz())
    viz <- dsapptools::viz_selection(data = data_viz(),
                                     dic = dic_load(),
                                     viz = actual_but$active,
                                     num_hType = TRUE)
    suppressWarnings(do.call(eval(parse(text=viz)),viz_opts()))
  })
  
  
  
  output$hgch_viz <- highcharter::renderHighchart({
    req(actual_but$active)
    req(data_viz())
    if (actual_but$active %in% c("table", "map")) return()
    viz_down()
  })
  
  output$lflt_viz <- leaflet::renderLeaflet({
    req(actual_but$active)
    req(data_viz())
    if (!actual_but$active %in% c("map")) return()
    viz_down() |>
      leaflet::setView(lng = 0, lat = -5, 1.25)
  })
  
  output$dt_viz <- reactable::renderReactable({
    req(actual_but$active)
    if (actual_but$active != "table") return()
    req(data_down())
    df <- dplyr::as_tibble(data_down())
    id_df <- grep("id", names(df))
    if (!identical(id_df, integer())) {
      df <- df[,-id_df]
    }
    id_df <- grep("createdTime|filename|size|type|width|height", names(df))
    if (!identical(id_df, integer())) {
      df <- df[,-id_df]
    }
    if ("url" %in% names(df)) {
      df <- df |> dplyr::rename(URL = "url")
    }
    if ("Link to contract" %in% names(df)) {
      df <- df |> dplyr::rename(URL = "Link to contract")
    }
    
    dtable <- reactable::reactable(df,
                                   defaultPageSize = 5,
                                   searchable = TRUE,
                                   showPageSizeOptions = TRUE,
                                   width = 900, height = 700,
                                   columns = list(
                                    URL = reactable::colDef(cell = function(URL) {
                                       htmltools::tags$a(href = as.character(URL), target = "_blank", "link to view")
                                     })
                                     
                                   )
                                   )
    dtable
  })
  
  output$viz_view <- renderUI({
    req(actual_but$active)
    
    if (is.null(chosen_menu$id)) {
      return(HTML("<div class = 'back-init'>
               <div class = 'background-viz'>
               <img src='img/background/Background-ds.png' class = 'back-img'/></div>
               <div class = 'back-click'>
               <img src='img/click/click.svg' class = 'back-click-img'/><br/>
               <b>Select</b> a filter to see the visualization</div>
                  </div>")
      )
    }
    
    heigh_viz <- 600
    if (!is.null(input$dimension)) {
      heigh_viz <- input$dimension[2] - 150
    }
    
    if (actual_but$active != "table") {
      if (is.null(data_viz())) return("No information available")
    }
    
    viz <- actual_but$active
    if (viz %in% c("map", "map_bubbles")) {
      shinycustomloader::withLoader(
        leaflet::leafletOutput("lflt_viz", height = heigh_viz),
        type = "html", loader = "loader4"
      )
    } else if (viz == "table") {
      shinycustomloader::withLoader(
        reactable::reactableOutput("dt_viz"),
        type = "html", loader = "loader4"
      )
    } else {
      #shinycustomloader::withLoader(
      highcharter::highchartOutput("hgch_viz", height = heigh_viz)#,
      #   type = "html", loader = "loader4"
      # )
    }
  })
  
  
  
  # save click --------------------------------------------------------------
  
  
  click_viz <- reactiveValues(info = NULL)
  
  
  observeEvent(input$lflt_viz_shape_click, {
    req(chosen_menu$id)
    if (is.null(data_viz())) return()
    if (!any(grepl("Country", names(data_viz())))) return()
    req(actual_but$active)
    if (actual_but$active != "map") return()
    click <- input$lflt_viz_shape_click
    if (!is.null(click)) {
      if (chosen_menu$id == "a") {
        click_viz$info <- list("Request Country" = click$id)
      } else {
        click_viz$info <- list("Country" = click$id)
      }
    }
    
  })
  
  observeEvent(input$hcClicked, {
    req(chosen_menu$id)
    if (is.null(data_viz())) return()
    click <- input$hcClicked
    if (actual_but$active %in% c("bar")) {
      if (chosen_menu$id == "c") {
        if (!"Vaccine" %in% names(data_viz())) return()
        if (!is.null(click)) {
          click_viz$info <- list("Vaccine" = click$id)
        }
      } else {
        if (!"Status" %in% names(data_viz())) return()
        if (!is.null(click)) {
          click_viz$info <- list("Status" = click$id)
        }
      }
      
      
    }
    if (actual_but$active == "treemap") {
      if (!is.null(click)) {
        if (!any(grepl("Country", names(data_viz())))) return()
        if (chosen_menu$id == "a") {
          click_viz$info <- list("Request Country" = click$id)
        } else {
          click_viz$info <- list("Country" = click$id)
        }
      }
    }
  })
  
  
  info_click <- reactive({
    req(chosen_menu$id)
    tx <- NULL
    if (chosen_menu$id == "a") {
      tx <- c("Request Country", "Status", "Date", "Authority", "url")
    }
    if (chosen_menu$id == "b") {
      tx <- c("Country", "Status", "url")
    }
    if (chosen_menu$id == "c") {
      tx <- c("Country", "Supplier", "Vaccine")
    }
    tx
  })
  
  # Click Info --------------------------------------------------------------
  
  output$click_info <-renderUI({ # reactive({#
    tx <- HTML("<div class = 'click'>
               <img src='img/click/click.svg' class = 'click-img'/><br/>
               <b>Click</b> on the visualization to see more information.")
    if (is.null(click_viz$info)) return(tx)
    tryCatch({
      url_name <- "Document"
      if (chosen_menu$id == "b") url_name <- "Attachments"
      tx <- dsapptools::write_html_group(
        data = dplyr::as_tibble(data_down()),
        dic = dplyr::as_tibble(dic_load()),
        click = click_viz$info,
        separate_row = "url", 
        sep_url = ",", 
        url_name = url_name,
        url_class = "url-style",
        text_result_null = tx,
        id = NULL,
        info_click())
    },
    error=function(cond) {
      return(tx)
    })
    
    tx
    # if (is.null(click_viz$info)) return(tx)
    # if (is.null(data_viz())) return(tx)
    # if (is.null(data_down())) return(tx)
    # if (nrow(data_down()) == 0) return(tx)
    # if (nrow(data_viz()) == 0) return("No information available")
    # req(info_click())
    # tx <- dsapptools::write_html(
    #   data = dplyr::as_tibble(data_down()),
    #   dic = dplyr::as_tibble(dic_load()),
    #   click = click_viz$info,
    #   class_title = "click-title",
    #   class_body = "click-text",
    #   id = NULL,
    #   info_click())
    #tx
  })
  
  
  
  
  output$test <- renderPrint({
    click_info()
  })
  
  
  
  
  # downloads ---------------------------------------------------------------
  
  output$downloads <- renderUI({
    if (is.null(actual_but$active)) return()
    if (actual_but$active != "table") {
      dsmodules::downloadImageUI("download_viz",
                                 dropdownLabel ="Download",
                                 formats = c("jpeg", "pdf", "png", "html"),
                                 display = "dropdown",
                                 text = "Download")
    } else {
      dsmodules::downloadTableUI("dropdown_table",
                                 dropdownLabel = "Download",
                                 formats = c("csv", "xlsx", "json"),
                                 display = "dropdown", text = "Download")
    }
  })
  
  observe({
    dsmodules::downloadTableServer("dropdown_table",
                                   element = data_down(),
                                   formats = c("csv", "xlsx", "json"))
    dsmodules::downloadImageServer("download_viz",
                                   element = viz_down(),
                                   lib = "highcharter",
                                   formats = c("jpeg", "pdf", "png", "html"),
                                   file_prefix = "plot")
  })
  
}

shinyApp(ui, server)



