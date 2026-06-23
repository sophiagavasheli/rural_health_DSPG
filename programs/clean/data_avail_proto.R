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
      padding-bottom: 6px;
      margin-bottom: 10px;
      width: 100%;
    }

    .header-row {
      display: flex;
      gap: 8px;
      margin-bottom: 8px;
      font-weight: 600;
      font-size: 13px;
      color: #444;
      width: 100%;
    }

    .row {
      display: flex;
      gap: 8px;
      align-items: center;
      margin-bottom: 10px;
      width: 100%;
    }

    .var-name {
      width: 240px;
      min-width: 240px;
      font-size: 13px;
      white-space: normal;
    }

    .year-cell {
      flex: 1;
      height: 24px;
      border-radius: 3px;
      cursor: pointer;
      min-width: 0;
    }

    .year-label {
      flex: 1;
      text-align: center;
      min-width: 0;
    }

    .topic-header {
      margin-top: 20px;
      margin-bottom: 10px;
      font-weight: 700;
      font-size: 15px;
      border-bottom: 1px solid #ddd;
      padding-bottom: 4px;
    }

    .domain-header {
      font-size: 20px;
      font-weight: 800;
      margin-top: 30px;
      border-bottom: 2px solid #ccc;
      padding-bottom: 4px;
    }
  "))),
  
  h1("Data Availability Explorer"),

  
  fluidRow(
    column(4,
           sliderInput(
             "yearRange",
             "Year range:",
             min = min(long_data$Year),
             max = max(long_data$Year),
             value = c(min(long_data$Year), max(long_data$Year)),
             step = 1,
             sep = ""
           )
    ),
    
    column(4,       
           checkboxGroupInput(
             "availability",
             "Yearly Availability:",
             choices = unique(long_data$Yearly.Availability.Level),
             selected = unique(long_data$Yearly.Availability.Level)
           ),
    ),
    column(4,
           checkboxGroupInput(
             "coverage",
             "Average County Coverage Level:",
             choices = unique(long_data$Global.County.Coverage.Level),
             selected = unique(long_data$Global.County.Coverage.Level)
           )
    )
  ),
  # DOMAIN BUTTONS
  uiOutput("domainButtons"),
  
  p("Select the domain, years, level of availability for each year, and average county coverage to view variables."),
  
  hr(),
  
  uiOutput("explorer")
)

server <- function(input, output, session) {
  
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
          type = "button",
          class = if (selectedDomain() == d) "domain-btn active" else "domain-btn",
          `data-domain` = d,
          onclick = "Shiny.setInputValue('domain_click', this.getAttribute('data-domain'), {priority: 'event'})",
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
        Yearly.Availability.Level %in% input$availability,
        Global.County.Coverage.Level %in% input$coverage
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
            
            tags$div(
              class = "table-wrap",
              
              # HEADER ROW
              tags$div(
                class = "header-row",
                tags$div(style="width:240px; min-width:240px;", "Variable"),
                lapply(yrs, function(y) {
                  tags$div(class = "year-label", y)
                })
              ),
              
              # VARIABLE ROWS
              lapply(seq_len(nrow(vars)), function(i) {
                
                v <- vars$Variable.Name[i]
                vlabel <- vars$Variable.Label[i]
                
                vdf <- tp_df %>% filter(Variable.Name == v)
                
                tags$div(
                  class = "row",
                  
                  tags$div(
                    class="var-name",
                    v
                  ),
                  
                  lapply(yrs, function(y) {
                    
                    cell <- vdf %>% filter(Year == y)
                    
                    if (nrow(cell) == 0) {
                      col <- "#9e9e9e"
                      na_val <- NA
                    } else {
                      na_val <- suppressWarnings(as.numeric(cell$Yearly.County.Coverage.Pct[1]))
                      col <- if (na_val == 0) {
                        "#9e9e9e"
                      } else if (na_val < 30) {
                        "#c62828"
                      } else if (na_val < 50) {
                        "#ef6c00"
                      } else if (na_val < 70) {
                        "#fbc02d"
                      } else {
                        "#2e7d32"
                      }
                    }
                    
                    tags$div(
                      class = "year-cell",
                      style = paste0("background:", col, ";"),
                      title = paste0(vlabel, " — ", y, ": ", ifelse(is.na(na_val), "no data", paste0(na_val, "% available"))),
                      onclick = "Shiny.setInputValue('clicked_var', this.getAttribute('data-var'), {priority: 'event'})",
                      `data-var` = v
                    )
                  })
                )
              })
            )
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
      select(Year, Yearly.County.Coverage.Pct, Yearly.County.Coverage.Level, Active.Counties)
    
    showModal(modalDialog(
      title = "Details",
      tags$p(strong("Variable Description: "), meta$Variable.Label),
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
  
}

shinyApp(ui, server)