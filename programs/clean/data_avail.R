library(shiny)
library(dplyr)
library(tidyr)
library(scales)


# ---- LOAD & RESHAPE DATA ----
long_data <- read.csv("reference/dashboard_data.csv", stringsAsFactors = FALSE, check.names = FALSE)

years <- 2009:2023

ui <- fluidPage(
  
  tags$head(tags$style(HTML("
    .domain-btn-row {
      margin-bottom: 15px;
      display: flex;
      flex-wrap: wrap;
      gap: 6px;
    }

    .domain-btn {
      border-radius: 999px;
      border: 1px solid #cfd8dc;
      background: #f5f6f8;
      padding: 6px 12px;
      font-size: 13px;
      cursor: pointer;
    }

    .domain-btn.active {
      background: #1565c0;
      color: white;
      border-color: #1565c0;
    }

    .table-wrap {
      overflow-x: auto;
    }

    .header-row {
      display: flex;
      gap: 6px;
      margin-bottom: 8px;
      font-weight: 600;
      font-size: 13px;
      color: #444;
    }

    .row {
      display: flex;
      gap: 6px;
      align-items: center;
      margin-bottom: 10px;
    }

    .var-name {
      width: 280px;
      font-size: 13px;
    }

    .year-cell {
      width: 18px;
      height: 18px;
      border-radius: 3px;
      cursor: pointer;
    }

    .topic-header {
      margin-top: 18px;
      font-weight: 700;
      border-bottom: 1px solid #ddd;
      padding-bottom: 3px;
    }

    .domain-header {
      font-size: 20px;
      font-weight: 800;
      margin-top: 25px;
      border-bottom: 2px solid #ccc;
    }
  "))),
  
  titlePanel("Data Availability Explorer"),
  
  # DOMAIN BUTTONS
  uiOutput("domainButtons"),
  
  fluidRow(
    column(12,
           sliderInput(
             "yearRange",
             "Year range:",
             min = min(long_data$Year),
             max = max(long_data$Year),
             value = c(min(long_data$Year), max(long_data$Year)),
             step = 1,
             sep = ""
           ),
           
           checkboxGroupInput(
             "availability",
             "Availability:",
             choices = unique(long_data$availability_cat),
             selected = unique(long_data$availability_cat)
           )
    )
  ),
  
  hr(),
  
  uiOutput("explorer")
)

server <- function(input, output, session) {
  
  avail_levels <- unique(long_data$availability_cat)
  
  palette_colors <- c("#2e7d32", "#fbc02d", "#c62828", "#9e9e9e", "#1565c0")
  color_map <- setNames(palette_colors[seq_along(avail_levels)], avail_levels)
  
  years <- sort(unique(long_data$Year))
  
  selectedDomain <- reactiveVal("All")
  
  # ----------------------------
  # DOMAIN BUTTONS
  # ----------------------------
  output$domainButtons <- renderUI({
    
    domains <- c("All", sort(unique(long_data$Domain)))
    
    tags$div(
      class = "domain-btn-row",
      
      lapply(domains, function(d) {
        
        tags$button(
          class = if (selectedDomain() == d) "domain-btn active" else "domain-btn",
          onclick = sprintf("Shiny.setInputValue('domain_click', '%s', {priority: 'event'})", d),
          d
        )
      })
    )
  })
  
  observeEvent(input$domain_click, {
    selectedDomain(input$domain_click)
  })
  
  # ----------------------------
  # FILTER DATA
  # ----------------------------
  filtered <- reactive({
    
    df <- long_data %>%
      mutate(Year = as.integer(Year)) %>%
      filter(
        Year >= input$yearRange[1],
        Year <= input$yearRange[2],
        availability_cat %in% input$availability
      )
    
    if (selectedDomain() != "All") {
      df <- df %>% filter(Domain == selectedDomain())
    }
    
    df
  })
  
  # ----------------------------
  # MAIN EXPLORER
  # ----------------------------
  output$explorer <- renderUI({
    
    df <- filtered()
    yrs <- sort(unique(df$Year))
    
    domains <- sort(unique(df$Domain))
    
    lapply(domains, function(dom) {
      
      dom_df <- df %>% filter(Domain == dom)
      
      topics <- sort(unique(dom_df$Topic))
      
      tags$div(
        
        tags$div(class = "domain-header", dom),
        
        lapply(topics, function(tp) {
          
          tp_df <- dom_df %>% filter(Topic == tp)
          
          vars <- tp_df %>%
            distinct(Variable.Name, Variable.Label) %>%
            arrange(Variable.Label)
          
          tags$div(
            
            tags$div(class = "topic-header", tp),
            
            # HEADER ROW
            tags$div(
              class = "header-row",
              tags$div(style="width:280px;", "Variable"),
              lapply(yrs, function(y) {
                tags$div(style="width:18px; text-align:center;", y)
              })
            ),
            
            # VARIABLE ROWS
            lapply(seq_len(nrow(vars)), function(i) {
              
              v <- vars$Variable.Name[i]
              vlabel <- vars$Variable.Label[i]
              
              vdf <- tp_df %>% filter(Variable.Name == v)
              
              tags$div(
                class = "row",
                
                tags$div(class="var-name", vlabel),
                
                lapply(yrs, function(y) {
                  
                  cell <- vdf %>% filter(Year == y)
                  
                  status <- if (nrow(cell) == 0) "none" else cell$availability_cat[1]
                  
                  col <- if (status == "none") "#e0e0e0" else color_map[[status]]
                  
                  tags$div(
                    class = "year-cell",
                    style = paste0("background:", col, ";"),
                    title = paste(vlabel, y, status),
                    `data-var` = v
                  )
                })
              )
            })
          )
        })
      )
    })
  })
  
  # ----------------------------
  # CLICK POPUP
  # ----------------------------
  observeEvent(input$clicked_var, {
    
    var <- input$clicked_var
    
    meta <- long_data %>%
      filter(Variable.Name == var) %>%
      slice(1)
    
    detail <- long_data %>%
      filter(Variable.Name == var) %>%
      arrange(Year) %>%
      select(Year, availability_cat, na_pct, active_counties)
    
    showModal(modalDialog(
      title = meta$Variable.Label,
      
      tags$p(strong("Domain: "), meta$Domain),
      tags$p(strong("Topic: "), meta$Topic),
      tags$p(strong("Source: "), meta$Data.Source),
      tags$p(strong("Type: "), meta$Data.Type),
      
      hr(),
      
      renderTable(detail, striped = TRUE),
      
      easyClose = TRUE,
      size = "l"
    ))
  })
  
  # JS click handler
  session$onFlushed(function() {
    shiny::addResourcePath("www", "www")
  })
  
}

shinyApp(ui, server)