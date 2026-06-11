#libraries
library(shiny)
library(shinythemes)
library(shinyjs)
library(tidyverse)
library(bslib)

#all final data files must be in the shiny_dashboard directory

#UI
ui <- navbarPage("Health and Infrastructure",
                 header = tags$head(
                   tags$style(HTML("
      .navbar {
        background-color: #eeeeee !important;
      }
    "))
                 ),
  
  tabPanel("Overview", 
           h1("Exploring the Impacts of Infrastructure on Health Outcomes in the US"),
    fluidPage(
      textOutput(outputId = 'welcome'),
    )
  ),
  
  tabPanel("Literature Review",
    fluidPage(
      h1("Review of the Literature"),
      column(4,
             h2("Introduction"),
             p(" A persistent public health challenge in the United States is health disparities between rural and urban populations, particularly in rural Virginia counties (Zeng et al., 2015). Rural populations tend to face higher rates of chronic disease, mortality, and behavioral health conditions (Anderson et al., 2015). Much research has been conducted on the impact of socioeconomic factors on health outcomes such as educational attainment, household income, and social vulnerability (Bhowmik et al., 2021), as well as the impact of environmental factors like drinking water and air quality (VanDerslice, 2011). However, the impact of infrastructure on health outcomes is only recently being explored. Infrastructure can include broadband availability, access to transportation, and proximity of health centers. Internet access is crucial for booking appointments, finding the right care, and accessing telehealth services (Cummins et al., 2025). Quality roads and transportation access are necessary to reach health centers for chronic condition treatment and emergencies, and this is closely linked to the proximity of healthcare facilities (Kabayundo et al., 2025). Our project aims to construct a national county-level dataset with health outcomes and infrastructure variables to determine the relationships between them."),
             h2("Issues in Rural Health"),
             p("Rural populations face many challenges when accessing healthcare. The difficult terrain in rural regions, such as the mountains of Appalachia, makes it difficult to access transportation and increases drive times to health facilities (Donohoe et al., 2016). Financing healthcare is one of the biggest barriers, as health insurance can be very expensive. Furthermore, driving longer distances adds to financial expense in terms of fuel costs, lost income, and childcare costs (Kaboli et al., 2026). Limited internet availability can impede people’s use of telehealth and the ability to find up-to-date health information. Finally, the close-knit nature and self-sufficient attitude of many rural communities can discourage people from seeking medical care since they prefer to keep their health issues private and “power through” (Golembiewski et al., 2022). There is also a sense of stigma around seeking help for substance abuse and psychological disorders. Improving access to telemedicine is crucial to increase privacy and reduce stigma around seeking help (Golembiewski et al., 2022). All these challenges discourage many people in rural populations from getting medical care, so chronic conditions go unmanaged, and overall health outcomes for that population get worse."),

      column(4, p("")),
      column(4, p(""))
    )
  )),
  
  tabPanel(
    "Results",
    fluidPage(
      h1("Results", align = "center")
      # additional content
    )
  ), 
  
  tabPanel(
    "Data",
    fluidPage(
      h1("Data Sources", align = "center"),
      
      # FCC
      div(
        style = "display:flex; align-items:center; margin-bottom:20px;",
        img(src = "fcc.png",height = "150px", width = "150px"),
        div(
          tags$a(
            "Federal Communications Commission",
            href = "https://www.fcc.gov/",
            target_ = "_blank",
            style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#0072B2;"
        ),
        p('"The Current Population Survey (CPS) is sponsored by both the U.S. Census Bureau and the U.S. Bureau of Labor Statistics (BLS), and is the primary source of labor force statistics for the population of the USA."- Taken from the CPS website.',
          style = "font-size:14px; color:#333333; margin-top:5px;")
      )
      ),
      
      hr(),
      
      # ACS
      div(
        style = "display: flex; align-items: flex-start; gap: 20px;",
        img(src = "acs.png", height = "120px", style = "flex-shrink:0;"),
        div(
          tags$a(
            "American Comunity Survey",
            href = "https://www.cdc.gov/places/index.html",
            target = "_blank",
            style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#0072B2;"
          ),
          p('"The Current Population Survey (CPS) is sponsored by both the U.S. Census Bureau and the U.S. Bureau of Labor Statistics (BLS), and is the primary source of labor force statistics for the population of the USA."- Taken from the CPS website.',
            style = "font-size:14px; color:#333333; margin-top:5px;")
        )
      ),
      
      hr(),
      
      div(
        style = "display: flex; align-items: flex-start; gap: 20px;",
        img(src = "cdc_places.jpg", height = "100px", style = "flex-shrink:0;"),
        div(
          tags$a(
            "CDC PLACES",
            href = "https://www.cdc.gov/places/index.html",
            target = "_blank",
            style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#0072B2;"
          ),
          p('"The Current Population Survey (CPS) is sponsored by both the U.S. Census Bureau and the U.S. Bureau of Labor Statistics (BLS), and is the primary source of labor force statistics for the population of the USA."- Taken from the CPS website.',
            style = "font-size:14px; color:#333333; margin-top:5px;")
        )
      )
      
  )), 
  
  tabPanel(
    "About Us", 
    fluidPage(
      h1("Meet the Health and Infrastructure Team", 
         align = "center"),
      #headshots here
      
      hr(),
      
      h2("A special thanks to the entire 2026 DSPG Cohort", 
         align = "center"), 
      div(
        style = "text-align:center;",
        img(src = "interns.jpg", 
          height = "390px", 
          width = "700px")
      #additional content
    )
  )
  )
  
  
  
) #end ui


#server
server <- function(input, output)
  
  output$welcome<- renderText({
    paste0("Welcome to our 2026 DSPG Rural Health and Infrastructure project page!")
  })
  
  
  
  
#end server

shinyApp(ui, server)
