
# libraries ---------------------------------------------------------------
library(shiny)
library(leaflet)
library(tigris)
library(sf)
library(dplyr)
library(viridis)
library(bslib)


# data loading -------------------------------------------------------

## data availability dashboard
avail_dash_dat <- readRDS("dashboard_data.rds")

years <- 2009:2023

## health and infrastructure map
#inf_health = readRDS("")

## drive time map
drive_times = readRDS("health_site_drive_times_2023.rds")
us_counties = readRDS("us_counties_2020.rds")
states_sf <- states(year = 2023, class = "sf") %>% st_transform(4326)

drive_time_choices <- c(
  "Acute Care Hospital" = "acute_care_hospital",
  "Clinic & Urgent Care" = "clinic_urgent_care",
  "Dentist" = "dentist",
  "Doctor & Medical Specialist" = "doctors_medical_specialists",
  "Mental Health & Psychiatric Hospital" = "mental_health",
  "Pharmacy" = "pharmacy"
)

## health sites map
health_sites = readRDS("us_health_sites_2023.rds")
  

# UI ----------------------------------------------------------------------
ui <- navbarPage("Rural Health and Infrastructure",
                 theme = "theme.css", #theming specified in css file
                 
# overview panel ----------------------------------------------------------
  tabPanel("Overview", 
           fluidPage(
           h1("Exploring the Impacts of Infrastructure on Health Outcomes in the Rural US", align = "center"),
           h3("Data Science for the Public Good Program 2026", align="center"),
           h4("Stakeholders: Ballad Health and the Kohl Centre", align="center"),
           
           column(6,
                  h3("Project Overview"),
                  p("Health disparities between rural and urban communities remain a persistent challenge in the United States. Rural populations experience higher rates of chronic disease, mortality, and behavioral health conditions while facing greater barriers to accessing healthcare services. Although socioeconomic and environmental determinants of health have been extensively studied, the role of infrastructure—particularly broadband connectivity, transportation networks, and healthcare accessibility—has received comparatively less attention despite growing evidence that these factors significantly influence health outcomes. Infrastructure shapes the ability of individuals to obtain timely and effective healthcare. Broadband access enables telehealth services, appointment scheduling, and access to health information. Transportation infrastructure affects travel times to medical facilities and influences the ability to seek preventive and emergency care. Similarly, the geographic accessibility of healthcare facilities determines whether residents can obtain routine treatment, manage chronic conditions, and receive specialized services when needed. These challenges are particularly pronounced in rural communities, where geographic isolation, provider shortages, and infrastructure limitations often compound existing health disparities. "),
                  p("The project is being conducted through the Data Science for the Public Good (DSPG) program at Virginia Tech in collaboration with Ballad Health, the Kohl Centre, and the Whole Health Consortium.")
                  
                  
                  
                  ),
           column(6,
                  h3("Research Question"),
                  p("What are the relationships between infrastructure variables such as broadband and healthcare facility access, and health outcome variables such as mortality rate and mental health in rural Virginia?"),
                  h3("Project Objectives"),
                  p("This project seeks to examine the relationship between infrastructure and health outcomes by developing a comprehensive county-level dataset that integrates public health indicators with measures of broadband availability, transportation access, and healthcare accessibility. While the project will have a particular focus on rural Virginia and Appalachia, the dataset will be designed to support analysis across the broader United States. Objectives include:"),
                  tags$ul(
                    tags$li("Develop a comprehensive county-level infrastructure and health dataset"),
                    tags$li("Examine relationships between infrastructure and health outcomes"),
                    tags$li("Produce maps and visualizations illustrating geographic variation in infrastructure access and health outcomes across rural communities"),
                    tags$li("Predict health outcomes using infrastructure variables")
                  ),
                  
                  fluidRow(
                  div(
                    style = "display:flex; justify-content:center; align-items:center;",
                    tags$a(
                      href = "https://www.balladhealth.org",
                      target = "_blank",
                      img(
                        src = "ballad.png",
                        style = "width:200px; height:100px; object-fit:contain;"
                      )
                    )
                  ),
                  
                  div(
                    style = "display:flex; justify-content:center; align-items:center;",
                    tags$a(
                      href = "https://kohl.aaec.vt.edu/index.html",
                      target = "_blank",
                      img(
                        src = "kohlcentre_logo.png",
                        style = "width:200px; height:100px; object-fit:contain;"
                      )
                    )
                  )
                  )
                  
                  )
      
    )
  ),
  

# lit review --------------------------------------------------------------
  tabPanel("Literature Review",
    fluidPage(
      h1("Review of the Literature"),
      column(4,
             h2("Introduction"),
             p(" A persistent public health challenge in the United States is health disparities between rural and urban populations, particularly in rural Virginia counties (Zeng et al., 2015). Rural populations tend to face higher rates of chronic disease, mortality, and behavioral health conditions (Anderson et al., 2015). Much research has been conducted on the impact of socioeconomic factors on health outcomes such as educational attainment, household income, and social vulnerability (Bhowmik et al., 2021), as well as the impact of environmental factors like drinking water and air quality (VanDerslice, 2011). However, the impact of infrastructure on health outcomes is only recently being explored. Infrastructure can include broadband availability, access to transportation, and proximity of health centers. Internet access is crucial for booking appointments, finding the right care, and accessing telehealth services (Cummins et al., 2025). Quality roads and transportation access are necessary to reach health centers for chronic condition treatment and emergencies, and this is closely linked to the proximity of healthcare facilities (Kabayundo et al., 2025). Our project aims to construct a national county-level dataset with health outcomes and infrastructure variables to determine the relationships between them."),
             h2("Issues in Rural Health"),
             p("Rural populations face many challenges when accessing healthcare. The difficult terrain in rural regions, such as the mountains of Appalachia, makes it difficult to access transportation and increases drive times to health facilities (Donohoe et al., 2016). Financing healthcare is one of the biggest barriers, as health insurance can be very expensive. Furthermore, driving longer distances adds to financial expense in terms of fuel costs, lost income, and childcare costs (Kaboli et al., 2026). Limited internet availability can impede people’s use of telehealth and the ability to find up-to-date health information. Finally, the close-knit nature and self-sufficient attitude of many rural communities can discourage people from seeking medical care since they prefer to keep their health issues private and “power through” (Golembiewski et al., 2022). There is also a sense of stigma around seeking help for substance abuse and psychological disorders. Improving access to telemedicine is crucial to increase privacy and reduce stigma around seeking help (Golembiewski et al., 2022). All these challenges discourage many people in rural populations from getting medical care, so chronic conditions go unmanaged, and overall health outcomes for that population get worse."),
             h2("Health Outcomes"),
             p("The studies we reviewed used many different indicators to assess health outcomes. Mortality outcomes include overall mortality (“Appalachia Then and Now,” 2015.; “Appalachian Diseases of Despair,” 2025; Mullens et al., 2024), premature deaths (Anderson et al., 2015), life expectancy (Basu et al., 2019), inpatient mortality (Gujral & Basu, 2019), and diseases of despair such as suicide and drug overdose (“Appalachian Diseases of Despair, 2025,” 2015). Morbidity and chronic disease outcomes include diabetes prevalence (“Appalachia Then and Now,” 2015; Connect2Health FCC Task Force, 2019), cancer incidence (Hong et al., 2023; Zeng et al., 2015), cardiovascular disease (Basu et al., 2019; Hong et al., 2023; Zeng et al., 2015), Chronic Obstructive Pulmonary Disease (Basu et al., 2019; Gujral & Basu, 2019; Zeng et al., 2015), and obesity prevalence (“Appalachia Then and Now,” 2015). Other health outcomes include the number of poor physical health days (Anderson et al., 2015), poor mental health days (Anderson et al., 2015), low birth weight (Anderson et al., 2015), telemedicine usage rates (Cummins et al., 2025), visit completion rates (Haggerty et al., 2022), emergency room visits (Bailey et al., 2024), and missed appointments (Bailey et al., 2024; Haggerty et al., 2022)."),
             h2("Broadband and Health"),
             p("Broadband access is a crucial element to nearly every aspect of life, and it has become increasingly prominent in the healthcare field over time. As of 2019, broadband connectivity has been dubbed a “super determinant of health” by the Federal Communication Commission (FCC, 2019). This identification builds on the existing concept of social determinants of health (SDOH), a concept commonly used in government work and the literature to describe the conditions in the environments where people live, learn, work, and socialize, particularly in the context of how they influence one’s health (Vrtikapa et al., 2025). The Federal Communication Commission proposes that broadband is not only a social determinant of health itself but also that other social determinants of health, like education and employment, are considerably and increasingly dependent on broadband (Connect2Health FCC Task Force, 2019). However, broadband penetration rates decrease as counties become more rural (Coleman Drake et al., 2019). As of 2024, the FCC has increased its broadband speed benchmark to download speeds of 100 megabits per second and upload speeds of 20 megabits per second (Federal Communications Commission, 2024). This standard is used when assessing if an area has access to broadband internet.")
),
      column(4, 
             p('According to 2019 FCC data cited in the literature, 82.8% of Americans living in rural areas had access to fixed broadband internet, compared to 98.8% in urban areas (Johnson et al., 2024). A data overview from the Appalachian Regional Commission reports that broadband subscription rates in Appalachia remain below the national average, and in 116 counties within Appalachia, less than 80 percent of households have broadband (Srygley et al., 2025). Connectivity issues are present within rural health care institutions as well.    Rural health care facilities lag in terms of both upload and download speeds compared to metro health care facilities (Whitacre et al., 2016). With the increasing use of electronic recordkeeping, telemedicine use, and other reliance on the internet, it is crucial for healthcare facilities to have adequate connectivity. Telemedicine is one of the most important elements to consider when assessing broadband accessibility, as it reduces travel distance and transportation costs for rural patients. In rural Appalachian settings, telemedicine has been shown to increase visit completion rates by approximately 20% (Haggerty et al, 2022). Patients who live in, largely rural, medically underserved areas, as defined by the Health Resources and Services Administration, are less likely to have access to broadband than those who do not, meaning accessibility to services via telehealth are less available to those who could benefit from it most (Bell et al., 2023). While variables such as race, age, and socioeconomic status influence telemedicine use, low broadband access and rurality are the strongest predictors of low telemedicine use (Cummins et al., 2025).'),
             h2("Transportation and Health"),
             p("Transportation is a leading nonfinancial barrier to health in rural areas (Kabayundo et al., 2025). Long travel times to health facilities increase the risk of death for time-sensitive conditions. Rural hospital closures have been shown to increase inpatient mortality by 8.7% for time-sensitive conditions like sepsis, stroke, and acute myocardial infarction (Gujral & Basu, 2019). Because of the cost and time to travel, rural residents will visit health facilities less frequently, leading to unmanaged chronic diseases and delayed diagnosis. For example, rural cancer patients face reduced survival rates and worse outcomes (Ferriola et al., 2025), and the motor vehicle fatality rate can be double that of urban areas due to longer prehospital times (Anderson et al., 2015). Investments in highway infrastructure in Appalachia were linked to a rapid decline in infant mortality as isolated areas gained access to medical centers (“Appalachia Then and Now,” 2015). 
Longer drive times can greatly determine the type of care rural residents receive. For instance, rural breast cancer patients are more likely to undergo a mastectomy instead of breast-conserving surgery because the latter requires frequent, repeated travel for radiation therapy, which is often logistically or financially impossible for them (Ferriola et al., 2025; Guagliardo, 2004). Patients facing transportation barriers disproportionately rely on emergency departments and hospitalizations rather than preventative primary care (Bailey et al., 2024). In rural West Virginia, patients who had to pay someone for a ride had over 15 times higher odds of missing an appointment compared to those who did not (Bailey et al., 2024)Transportation barriers are closely linked to financial barriers due to the costs of travel. Furthermore, delaying care due to transportation barriers can lead to more expensive medical treatments once the patients reach a provider (Kaboli et al., 2026). Low-income populations are hit hardest because they are less likely to own a vehicle and often reside in areas where public transportation is insufficient (Guo et al., 2022; Hong et al., 2023)."), 
             h2("Accessibility of Health Facilities"), 
             p("Accessibility to healthcare facilities can be understood on spatial, physical, and digital levels (Cummins et al., 2025; Donohoe et al., 2016; Kaboli et al., 2026). It is common to see geospatial information systems analysis used to measure health care accessibility in the US and particularly Appalachia (Donohoe et al., 2016; Guagliardo, 2004). When properly distributed, primary care is one of the most important forms of health care in preventing large scale disease progression (Guagliardo, 2004). People living in rural areas face considerable challenges when accessing health care to manage chronic diseases (Golembiewski et al., 2022). The literature confirms that residents living in rural U.S are more prone to have poorer health than those living in urban counties, particularly when evaluating metrics like mortality, morbidity, clinical care, and health behaviors (Anderson et al., 2015). 42% of Appalachia’s population is classified as rural compared with the national average of 20%, marking the region as a considerable area of concern for access to health care (Donohoe et al., 2016). 
Inhabitants of rural areas across the US are disproportionally impacted by health care workforce shortages and geographic access to health care (Kaboli et al., 2026). From 2005 to 2015, greater primary care physician supply is associated with lower rates of mortality (Basu et al., 2019). In one study, every 10 additional primary care physicians per 100,000 population was associated with a 51.5-day increase in life expectancy (Basu et al., 2019). In addition to rurality, income is a determinant of healthcare accessibility. Low-income populations face compounded impacts of geographic barriers, like distance to health care infrastructure,as they are less likely to own a car and more ")
             ),
      column(4, 
             p("often live in areaswithout public transportation (Guo et al., 2022). In a study of 747 non-metropolitan counties, low-income residents (below 200% of the Federal Poverty Level) have significantly poorer access to care, often living more than 10 miles from the nearest health facility (Guo et al., 2022). Often because of the burden of time and cost of travel, rural patients are nearly twice as likely as urban residents to avoid seeking medical care. (Anderson et al., 2015; Golembiewski et al., 2022). As mentioned previously, access to healthcare via telemedicine is limited by broadband accessibility and lack of digital infrastructure (Cummins et al., 2025). However, use of telemedicine is associated with significantly higher appointment completion rates in Appalachia, as physical barriers are reduced and patients are more able to keep appointments (Haggerty et al., 2022).There have been 180 rural hospital closings since 2005 (Mullens et al., 2024). Rural hospital closures lead to increased travel times, increased EMS transport times, reduced emergency care availability, increased emergency department visits and hospital admission rates, and decreases total physician supply (Mullens et al., 2024). Additionally, rural hospital closure has led to over 800,000 people no longer being within a 15-min drive time of a hospital (Mullens et al., 2024). In the case of some rural counties with only one hospital, a closure of that hospital is associated with a decrease in income per capita and an increase in unemployment (Mullens et al., 2024)."),
             h2("Limitations"),
             p("Limitations of existing studies include unreliability of some key data sources, lack of standard defining characteristics of rurality, lack of causal evidence, sampling parameters, and measurement of spatial access. One author writes that the FCC’s Form 477 is an unreliable map that leads to poor policy and funding (Ali et al., 2022). This is said to be due to the FCC’s data collection being by census block. Even if one building in a census block has access to a service provider, the whole block is considered served (Ali et al, 2022). There is also a considerable lack of consensus on what defines “rurality” in the literature, with some studies using USDA Rural-Urban Continuum Codes and others using ARC designations (Anderson et al., 2015; Appalachian Diseases of Despair, 2025; Golembiewski et al., 2022). Additionally, many studies use a cross-sectional design for their data analysis, which makes them unable to establish a causal relationship and instead find correlations (Anderson et al., 2015; Basu et al., 2019; Connect2Health FCC Task Force, 2019). Notably limiting, various studies include data samples from only one state or institution (Bailey et al., 2024; Basu et al., 2019; Haggerty et al, 2022). Finally, measurements of spatial access pose an issue, as many times using counties or other rigid geographical borders to conduct analysis results in inaccurate representations of patients’ realized travel times, which often include crossing borders in rural settings (Guagliardo, 2004; Donohoe et al., 2024). "),
             h2("Our Approach"),
             p("In the research, not many studies have attempted to examine causal relationships between infrastructure and health outcome variables on a US-wide scale. This is exactly the gap we intend to fill. To be able to examine causal relationships, we aim to build a reproducible dataset with data from each US county examining various infrastructure measures and their impacts on health outcomes, using controls such as demographic information and rurality."),
             tags$details(
               class = "ref-box",
               
               tags$summary(
                 class = "ref-summary",
                 "References"
               ),
               
               div(
                 class = "ref-content",
                 
                 tags$ul(
                   tags$li("Anderson TJ, Saman DM, Lipsky MS, Lutfiyya MN (2015). A Cross-Sectional Study on Health Differences Between Rural and Non-Rural U.S. Counties Using the County Health Rankings. BMC Health Services Research, 15(1), 441."),
                   tags$li("Appalachian Regional Commission (2015). Appalachia Then and Now: Examining Changes to the Appalachian Region Since 1965."),
                   tags$li("Appalachian Regional Commission (2025). Appalachian Diseases of Despair, 2025."),
                   tags$li("Bailey J, Burchfield K, Redden J, et al. (2024). The Road to Access: Addressing Transportation Challenges in Rural Primary Care."),
                   tags$li("Basu S, Berkowitz SA, Phillips RL, et al. (2019). Association of Primary Care Physician Supply With Population Mortality in the United States, 2005–2015."),
                   tags$li("Bell N, Hung P, López-De Fede A, Adams SA (2023). Broadband Access Within Medically Underserved Areas and Its Implication for Telehealth Utilization."),
                   tags$li("Bhowmik T, Tirtha SD, Iraganaboina NC, Eluru N (2021). A Comprehensive Analysis of COVID-19 Transmission and Mortality Rates at the County Level in the United States."),
                   tags$li("Connect2Health FCC Task Force (2019). Broadband Connectivity: A 'Super' Determinant of Health."),
                   tags$li("Cummins MR, Wong B, Wan N, et al. (2025). Social Vulnerability, Lower Broadband Internet Access, and Rurality Associated With Lower Telemedicine Use in U.S. Counties."),
                   tags$li("Donohoe J, Marshall V, Tan X, et al. (2016). Spatial Access to Primary Care Providers in Appalachia: Evaluating Current Methodology."),
                   tags$li("Drake C, Zhang Y, Chaiyachati KH, Polsky D (2019). The Limitations of Poor Broadband Internet Access for Telemedicine Use in Rural America."),
                   tags$li("Federal Communications Commission (2024). FCC Increases Broadband Speed Benchmark."),
                   tags$li("Ferriola N, Alvarado-Richter C, Acharya E, et al. (2025). The Impact of Transportation Barriers on Rural Cancer Patients: A Systematic Review."),
                   tags$li("Golembiewski EH, Gravholt DL, Torres Roldan VD, et al. (2022). Rural Patient Experiences of Accessing Care for Chronic Conditions."),
                   tags$li("Guagliardo MF (2004). Spatial Accessibility of Primary Care: Concepts, Methods and Challenges."),
                   tags$li("Gujral K, Basu A (2019). Impact of Rural and Urban Hospital Closures on Inpatient Mortality."),
                   tags$li("Guo J, Hernandez I, Dickson S, et al. (2022). Income Disparities in Driving Distance to Health Care Infrastructure in the United States."),
                   tags$li("Haggerty T, Stephens HM, Peckens SA, et al. (2022). Telemedicine Versus In-Person Primary Care in a Rural Appalachian Population."),
                   tags$li("Hong I, Wilson B, Gross T, et al. (2023). Challenging Terrains: Socio-Spatial Analysis of Primary Health Care Access Disparities in West Virginia."),
                   tags$li("Kabayundo J, Ahuja M, Jadhav S, et al. (2025). Transportation Barriers to Healthcare Access: A Scoping Review."),
                   tags$li("Kaboli P, Blaine A, Mares J, et al. (2026). Health Care Access From the Rural Perspective: A Narrative Review."),
                   tags$li("Mullens CL, Hernandez JA, Murthy J, et al. (2024). Understanding the Impacts of Rural Hospital Closures."),
                   tags$li("Srygley S, Khairunnisa N, Elliott D (2025). The Appalachian Region: A Data Overview from the 2019–2023 ACS Chartbook."),
                   tags$li("VanDerslice J (2011). Drinking Water Infrastructure and Environmental Disparities."),
                   tags$li("Vrtikapa K, Hoque Urmy F, Hoque F (2024). Social Determinants of Health: The Impact of This Overlooked Vital Sign."),
                   tags$li("Whitacre BE, Wheeler D, Landgraf C (2017). What Can the National Broadband Map Tell Us About the Health Care Connectivity Gap?"),
                   tags$li("Zeng D, You W, Mills B, et al. (2015). A Closer Look at the Rural–Urban Health Disparities in Virginia.")
                 )
               )
             )
             )
    )
  ),

navbarMenu("Data",
# data avail dash ---------------------------------------------------------
tabPanel(
  "Data Availability Dashboard",
  fluidPage(
    
    tags$head(tags$style(HTML("
    .domain-btn-row {
      margin-bottom: 15px;
      display: flex;
      flex-wrap: wrap;
      gap: 6px;
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
    
    .irs-bar, .irs-bar-edge {
      background-color: #2d6a4f !important;
      border-top-color: #eef5ef !important;
      border-bottom-color: #eef5ef !important;
    }
      
    .irs-single, .irs-from, .irs-to, .irs-handle {
      background-color: #2d6a4f !important;
      border-color: #eef5ef !important;
    }
    
    input[type='checkbox']:checked {
        background-color: #2d6a4f !important;
        border-color: #eef5ef !important;
    }
      
      input[type='checkbox']:focus {
        border-color: #2d6a4f !important;
        box-shadow: 0 0 0 0.25rem #eef5ef !important;
      }
  "))),
    
    h1("Data Availability Dashboard"),
    p("Select the domain, years, level of availability for each year, and average county coverage to view variables."),
    
    # year slider
    fluidRow(
      column(3,
             sliderInput(
               "yearRange",
               "Year Range:",
               min = min(avail_dash_dat$Year),
               max = max(avail_dash_dat$Year),
               value = c(min(avail_dash_dat$Year), max(avail_dash_dat$Year)),
               step = 1,
               sep = ""
             )
      ),
      #availability checkbox
      column(2,       
             checkboxGroupInput(
               "availability",
               "Yearly Availability:",
               choices = unique(avail_dash_dat$Yearly.Availability.Level),
               selected = unique(avail_dash_dat$Yearly.Availability.Level)
             )
      ),
      #coverage checkbox
      column(3,       
             checkboxGroupInput( 
               "coverage", 
                "Average County Coverage Level:", 
                choices = unique(avail_dash_dat$Global.County.Coverage.Level), 
                selected = unique(avail_dash_dat$Global.County.Coverage.Level) 
               )
             
      ),
      #legend
      column(4,
             tags$div(
               class = "well", # Adds a border and light gray background box
               style = "padding: 10px; max-width: 250px;",
               
               tags$h5(strong("Legend:")),
               
               # Legend Items
               tags$div(style = "display: flex; align-items: center; margin-bottom: 5px;",
                        tags$span(style = "background-color: #2e7d32; width: 15px; height: 15px; display: inline-block; margin-right: 8px; border-radius: 3px;"),
                        "Mostly Full Coverage (>70%)"
               ),
               tags$div(style = "display: flex; align-items: center; margin-bottom: 5px;",
                        tags$span(style = "background-color: #fbc02d; width: 15px; height: 15px; display: inline-block; margin-right: 8px; border-radius: 3px;"),
                        "Partial Coverage (50-69%)"
               ),
               tags$div(style = "display: flex; align-items: center; margin-bottom: 5px;",
                        tags$span(style = "background-color: #c62828; width: 15px; height: 15px; display: inline-block; margin-right: 8px; border-radius: 3px;"),
                        "Little Coverage (<50%)"
               ),
               tags$div(style = "display: flex; align-items: center;",
                        tags$span(style = "background-color: #9e9e9e; width: 15px; height: 15px; display: inline-block; margin-right: 8px; border-radius: 3px;"),
                        "Unavailable"
               )
             )
      )
    ),
    
    # domain buttons and search bar
    fluidRow(
      column(9,
          p(strong("Domain:")),
          uiOutput("domainButtons"),
      ),
      column(3,
          textInput("varSearch", "Search Variables/Topics:", 
                    placeholder = "e.g. poverty, broadband...")
           )
  ),
    
    hr(),
    
    uiOutput("explorer")
  )
),
# data sources ------------------------------------------------------------
tabPanel(
  "Data Sources",
  fluidPage(
    h1("Primary Sources"),
    tags$p(
      "The following sources were used to construct the dataset presented in the Data Availability Dashboard. Please see the ",
      tags$a(href = "https://github.com/sophiagavasheli/rural_health_DSPG", "GitHub", target = "_blank"),
      " for more specific details on how the data was collected. You can download a CSV version of the data here:"
    ),
    downloadButton("download_data", "Download Data"),
    hr(),
    
    # CLH
    div(
      style = "display: flex; align-items: flex-start; gap: 20px;",
      img(src = "ahrq.png", height = "100px", style = "flex-shrink:0;"),
      div(
        tags$a(
          "Agency for Healthcare Research and Quality",
          href = "https://www.ahrq.gov/data/innovations/clh-data.html",
          target_ = "_blank",
          style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#007fa0;"
        ),
        p('"AHRQ\'s database on Community-Level Health (CLH) was created under a project funded by the Patient Centered Outcomes Research (PCOR) Trust Fund. The purpose of this project is to create easy to use, linkable small-area data on health-related factors to use in PCOR research, inform approaches to address emerging health issues, and ultimately contribute to improved health outcomes." Our data availability dashboard is constructed mostly from the CLH data.',
          style = "font-size:14px; color:#333333; margin-top:5px;")
      )
    ),
    
    hr(),
    
    # FCC
    div(
      style = "display: flex; align-items:center; margin-bottom:20px; gap: 20px;",
      img(src = "fcc.png",height = "150px", width = "150px",style = "flex-shrink:0;"),
      div(
        tags$a(
          "Federal Communications Commission",
          href = "https://www.fcc.gov/",
          target_ = "_blank",
          style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#007fa0;"
        ),
        p('"The Federal Communications Commission (FCC) regulates interstate and international communications by radio, television, wire, satellite, and cable in all 50 states, the District of Columbia and U.S. territories. A U.S. government agency overseen by Congress, the Commission is the federal agency responsible for implementing and enforcing America’s communications law & regulations." Using the FCC\'s Form 477, we were able to collect data on broadband adoption across counties.',
          style = "font-size:14px; color:#333333; margin-top:5px;")
      )
    ),
    
    hr(),
    
    #wonder
    div(
      style = "display: flex; align-items: flex-start; gap: 20px;",
      img(src = "cdc.jpg", height = "100px", style = "flex-shrink:0;"),
      div(
        tags$a(
          "CDC WONDER",
          href = "https://wonder.cdc.gov/",
          target = "_blank",
          style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#007fa0;"
        ),
        p('"Wide-ranging ONline Data for Epidemiologic Research is an easy-to-use, menu-driven system that makes the information resources of the Centers for Disease Control and Prevention (CDC) available to public health professionals and the public at large. It provides access to a wide array of public health information." We downloaded overall mortality from WONDER.',
          style = "font-size:14px; color:#333333; margin-top:5px;")
      )
    ),
    
    hr(),
    # arc
    div(
      style = "display: flex; align-items: flex-start; gap: 20px;",
      img(src = "arc.png", height = "100px", style = "flex-shrink:0;"),
      div(
        tags$a(
          "Appalachian Regional Commission (ARC)",
          href = "https://www.arc.gov/appalachian-counties-served-by-arc/",
          target = "_blank",
          style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#007fa0;"
        ),
        p(
          '"The Appalachian Regional Commission identifies counties within the Appalachian region to support economic development initiatives." We used their official county list for regional classification.',
          style = "font-size:14px; color:#333333; margin-top:5px;"
        )
      )
    ),
    
    hr(),
    h1("Spatial Data"),
    p("These data were used for the maps and drive time calculations."),
    # osm
    div(
      style = "display: flex; align-items: flex-start; gap: 20px;",
      img(src = "osm.png", height = "100px", style = "flex-shrink:0;"),
      div(
        tags$a(
          "OpenStreetMap",
          href = "https://planet.osm.org/planet/full-history/",
          target = "_blank",
          style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#007fa0;"
        ),
        p(
          '"OpenStreetMap provides freely accessible map data of roads, buildings, and points of interest worldwide." We used the full-history planet file to extract road and health-related infrastructure data.',
          style = "font-size:14px; color:#333333; margin-top:5px;"
        )
      )
    ),
    
    hr(),
    # unc
    div(
      style = "display: flex; align-items: flex-start; gap: 20px;",
      img(src = "unc.png", height = "100px", style = "flex-shrink:0;"),
      div(
        tags$a(
          "UNC Cecil G. Sheps Center",
          href = "https://www.shepscenter.unc.edu/programs-projects/rural-health/list-of-hospitals-in-the-u-s/",
          target = "_blank",
          style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#007fa0;"
        ),
        p('"The Sheps Center maintains national datasets on rural health and hospital locations in the United States." We downloaded their national hospital list.',
          style = "font-size:14px; color:#333333; margin-top:5px;"
        )
      )
    ),
    
    
    hr(),
    # census
    div(
      style = "display: flex; align-items: flex-start; gap: 20px;",
      img(src = "census.jpg", height = "100px", style = "flex-shrink:0;"),
      div(
        tags$a(
          "U.S. Census Bureau",
          href = "https://www.census.gov/geographies/reference-files/time-series/geo/centers-population.html",
          target = "_blank",
          style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#007fa0;"
        ),
        p(
          '"The Census Bureau\'s mission is to serve as the nation\'s leading provider of quality data about its people and economy." We downloaded the census tract population weighted centroids from here.',
          style = "font-size:14px; color:#333333; margin-top:5px;"
        )
      )
    ),
    
    hr(),
    
    h1("Other Sources"),
    p("The following sources are just a few that the CLH database is constructed with."),
    
    # ACS
    div(
      style = "display: flex; align-items: flex-start; gap: 20px;",
      img(src = "acs.png", height = "120px", style = "flex-shrink:0;"),
      div(
        tags$a(
          "American Comunity Survey",
          href = "https://www.census.gov/programs-surveys/acs.html",
          target = "_blank",
          style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#007fa0;"
        ),
        p("\"The American Community Survey (ACS) is the premier source of detailed information about the nation's people and housing. As an ongoing survey conducted by the U.S. Census Bureau since 2005, the ACS collects detailed social, economic, housing, and demographic information from a sample of households across the 50 states, the District of Columbia, and Puerto Rico.\" ",
          style = "font-size:14px; color:#333333; margin-top:5px;")
      )
    ),
    
    hr(),
    
    #places
    div(
      style = "display: flex; align-items: flex-start; gap: 20px;",
      img(src = "cdc_places.jpg", height = "100px", style = "flex-shrink:0;"),
      div(
        tags$a(
          "CDC PLACES",
          href = "https://www.cdc.gov/places/index.html",
          target = "_blank",
          style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#007fa0;"
        ),
        p('"PLACES provides health and health-related data using small area estimation for counties, incorporated and census designated places, census tracts, and ZIP Code Tabulation Areas (ZCTAs) across the United States. This project, which started in 2015, is a partnership between CDC, the Robert Wood Johnson Foundation, and the CDC Foundation." ',
          style = "font-size:14px; color:#333333; margin-top:5px;")
      )
    ),
    
    hr(),
    
    #hrsa
    div(
      style = "display: flex; align-items: flex-start; gap: 20px;",
      img(src = "hrsa.png", height = "100px", style = "flex-shrink:0;"),
      div(
        tags$a(
          "Health Resources and Services Administration",
          href = "https://www.hrsa.gov/",
          target = "_blank",
          style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#007fa0;"
        ),
        p('"Established in 1980, HRSA is the primary federal agency responsible for ensuring access to health care services for people who are uninsured, isolated, or medically vulnerable, including those living with HIV/AIDS, mothers and children, and those living in rural areas." ',
          style = "font-size:14px; color:#333333; margin-top:5px;")
      )
    ),
    
    hr(),
    
    #rucc
    div(
      style = "display: flex; align-items: flex-start; gap: 20px;",
      img(src = "usda_ruc.png", height = "100px", style = "flex-shrink:0;"),
      div(
        tags$a(
          "USDA Rural Urban Continuum Codes",
          href = "https://www.ers.usda.gov/data-products/rural-urban-continuum-codes",
          target = "_blank",
          style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#007fa0;"
        ),
        p('"The 2023 Rural-Urban Continuum Codes distinguish U.S. metropolitan (metro) counties by the population size of their metro area, and nonmetropolitan (nonmetro) counties by their degree of urbanization and adjacency to a metro area. " ',
          style = "font-size:14px; color:#333333; margin-top:5px;")
      )
    ),
    
    hr(),
    
    #chr
    div(
      style = "display: flex; align-items: flex-start; gap: 20px;",
      img(src = "county_health_rankings.jpg", height = "100px", style = "flex-shrink:0;"),
      div(
        tags$a(
          "County Health Rankings",
          href = "https://www.countyhealthrankings.org/",
          target = "_blank",
          style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#007fa0;"
        ),
        p('"County Health Rankings & Roadmaps (CHR&R), a program of the University of Wisconsin Population Health Institute, draws attention to why there are differences in health within and across communities. The program highlights policies and practices that can help everyone be as healthy as possible. CHR&R aims to grow a shared understanding of health, equity and the power of communities to improve health for all. This work is rooted in a long-term vision where all people and places have what they need to thrive." ',
          style = "font-size:14px; color:#333333; margin-top:5px;")
      )
    ),
    
    hr()
    
    
  ))
),

# maps -------------------------------------------------------------------
tabPanel(
  "Maps",
  navset_pill( 
    nav_panel("Infrastructure and Health Outcomes", 
              sidebarLayout(
                sidebarPanel(
                  
                  p("Please note that variables change year to year based on availability.")
                ),
                mainPanel(
                  leafletOutput("health_map", height = "800px") 
                )
                
              )
              ), 
    nav_panel("Drive Times", 
              sidebarLayout(
                sidebarPanel(
                  
                  selectInput(
                    "site_type",
                    "Drive time to the nearest:",
                    choices = drive_time_choices,
                    selected = "Acute Care Hospital"
                  ),
                  
                  p("Please see the Analysis page for the methods used to generate this data and the Health Sites map to see the locations of the sites.")
                ),
                
                mainPanel(
                  leafletOutput("drive_map", height = "800px")    
                )
                
              )
              ), 
    nav_panel("Health Sites", 
              sidebarLayout(
                sidebarPanel(

                  # filter bar
                  selectInput(
                    "type_filter",
                    "Health site type:",
                    choices = c("All", sort(unique(health_sites$health_site_label))),
                    selected = "All"
                  ),
                  
                  p("Hospitals are sourced from the UNC Shep's Center and the other health sites are sourced from OSM. Please be aware that OSM sites are user generated and many sites might be missing on this map.")
                ),
                
                mainPanel(
                  leafletOutput("health_sites_map", height = "800px")    
                )
                
              )
              ), 
    
  ), 
  id = "tab" 
), 

# analysis ----------------------------------------------------------------
  tabPanel(
    "Analysis",
    fluidPage(
      h1("Analysis")
    )
  ), 
  
  

# about  -------------------------------------------------------------------
  tabPanel(
    "About", 
    fluidPage(
      column(5,
        h2("The Team"),
        hr(),
        
        h4("Undergraduate Intern:"),
        p(
          tags$a(
            "Sophia Gavasheli",
            href = "www.linkedin.com/in/sophia-gavasheli",  
            target = "_blank"
          )
        ),
        
        h4("Graduate Mentor:"),
        p(
          tags$a(
            "Pragati Dahal",
            href = "https://www.linkedin.com/in/pragatidahal9843/",   
            target = "_blank"
          )
        ),
        
        h4("Faculty Mentors:"),
        p(
          tags$a(
            "Dr. Michael Cary",
            href = "https://aaec.vt.edu/people/faculty/cary.html",    
            target = "_blank"
          )
        ),
        p(
          tags$a(
            "Dr. Yujuan Gao",
            href = "https://aaec.vt.edu/people/adjunct-emeritus/gao.html",      
            target = "_blank"
          )
        ),
        
        h4("Program Director:"),
        p(
          tags$a(
            "Dr. Le Wang",
            href = "https://aaec.vt.edu/people/faculty/lwang.html",         
            target = "_blank"
          )
        )
      ),
      column(7,
        
        div(
          style = "text-align:center;",
          img(src = "interns.jpg", 
              height = "390px", 
              width = "700px"), 
        ),
        h4("2026 DSPG Cohort")
    ),
    
    h2("About DSPG"),
    
    p("The Data Science for the Public Good (DSPG) Young Scholars program is a summer immersive program held at the Virginia Tech Department of Agricultural and Applied Economics and the Virginia Cooperative Extension Service. The program engages students from across the country to work together on projects that address state, federal, and local government challenges around critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how information generated within every community can be leveraged to improve quality of life and inform public policy."),
    
  hr()
  
    )
  )
  
  
  
) #end ui



# server ------------------------------------------------------------------
server <- function(input, output, session) {
  

# maps ---------------------------------------------------------------------
  
  ## health infrastructure map
  
  
  ## drive time map 
  drive_filt <- reactive({
    drive_times %>%
      dplyr::filter(health_site_type == input$site_type)
  })
  
  output$drive_map <- renderLeaflet({
    
    dat <- drive_filt() 

    pal <- colorBin(
      palette = "YlOrRd",
      domain = dat$avg_drive_time_minutes,
      bins = c(0, 15, 30, 45, 60, 90, 120, Inf),
      na.color = "gray90"
    )
    
    leaflet(dat) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(lng1 = -125, lat1 = 24,lng2 = -66, lat2 = 50) %>% 
      
      addPolygons(
        fillColor = ~pal(avg_drive_time_minutes),
        fillOpacity = 0.7,
        color = "white",
        weight = 0.5,
        popup = ~paste0(
          "<b>County:</b> ", NAME, "<br>",
          "<b> Average Drive Time (min) :</b> ",
          round(avg_drive_time_minutes, 1), " min"
        )
      ) %>%
      
      addPolygons(
        data = states_sf,
        fill = FALSE,
        color = "lightgray",
        weight = 1,
        opacity = 0.6
      ) %>%
      
      addLegend(
        position = "bottomright",
        pal = pal,
        values = dat$avg_drive_time_minutes,
        title = "Average Drive Time (min)"
      )
  })
  
  ## health site map
 
  health_site_pal <- colorFactor(
    viridis::turbo(17),
    health_sites$health_site_label
  )
  
  filtered_data <- reactive({
    if (input$type_filter == "All") {
      health_sites
    } else {
      dplyr::filter(health_sites, health_site_label == input$type_filter)
    }
  })
  
  output$health_sites_map <- renderLeaflet({
    
    health_site_df <- sf::st_transform(filtered_data(), 4326)
    
    leaflet(health_site_df) %>%
      addProviderTiles("CartoDB.Positron") %>%
      fitBounds(lng1 = -125, lat1 = 24,lng2 = -66, lat2 = 50) %>% 
      
      addCircleMarkers(
        radius = 4,
        fillOpacity = 0.7,
        stroke = FALSE,
        color = ~health_site_pal(health_site_label),
        
        popup = ~paste0(
          "<b>", health_site_name, "</b><br>",
          "Type: ", health_site_label, "<br>",
          "County: ", county
        )
      ) %>%
      
      addLegend(
        "bottomright",
        pal = health_site_pal,
        values = health_sites$health_site_label,
        title = "Site type"
      )
  })

  # data avail dash ---------------------------------------------------------
  
  years <- sort(unique(avail_dash_dat$Year))
  
  selectedDomain <- reactiveVal("All")
  
  
  # DOMAIN BUTTONS (static — only depends on avail_dash_dat, not filtered data)
  
  output$domainButtons <- renderUI({
    
    domains <- c("All", sort(unique(avail_dash_dat$Domain)))
    
    tags$div(
      class = "domain-btn-row",
      
      lapply(domains, function(d) {
        
        tags$button(
          type = "button",
          class = if (selectedDomain() == d) "btn-primary domain-selected" else "btn-primary",
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
  
  
  # STAGE 1 — only recomputes when yearRange changes
  
  yearFiltered <- reactive({
    avail_dash_dat %>%
      mutate(Year = as.integer(Year)) %>%
      filter(
        Year >= input$yearRange[1],
        Year <= input$yearRange[2]
      )
  })
  
  
  # STAGE 1b — recompute availability dynamically for the SELECTED range
  # (depends only on yearRange, same invalidation tier as yearFiltered)
  
  availabilityRecalc <- reactive({
    df <- yearFiltered()
    
    total_years <- length(unique(df$Year))  # years present in dataset within selected range
    
    avail <- df %>%
      filter(Yearly.County.Coverage.Pct > 0) %>%
      group_by(Variable.Name) %>%
      summarise(years_available = n_distinct(Year), .groups = "drop") %>%
      mutate(
        Yearly.Availability.Level = case_when(
          years_available == total_years ~ "Full Availability",
          years_available >= total_years/2   ~ "Partial Availability",
          TRUE                            ~ "Very Little Availability"
        )
      ) %>%
      select(Variable.Name, Yearly.Availability.Level)
    
    df %>%
      select(-Yearly.Availability.Level) %>%   # drop the stale precomputed column
      left_join(avail, by = "Variable.Name")
  })
  
  
  # STAGE 2 — only recomputes when availability/coverage change
  
  levelFiltered <- reactive({
    availabilityRecalc() %>%
      filter(
        Yearly.Availability.Level %in% input$availability,
        Global.County.Coverage.Level %in% input$coverage
      )
  })
  
  
  # STAGE 3 — only recomputes when domain selection changes
  
  filtered <- reactive({
    df <- levelFiltered()
    
    if (selectedDomain() != "All") {
      df <- df %>% filter(Domain == selectedDomain())
    }
    
    df
  })
  
  # STAGE 4 — text search, applied after domain filtering.
  # Matches Variable.Name OR Variable.Label OR Topic, case-insensitive.
  # Only recomputes when input$varSearch or filtered() changes.
  
  searched <- reactive({
    df <- filtered()
    
    query <- trimws(input$varSearch)
    
    if (nzchar(query)) {
      df <- df %>%
        filter(
          grepl(query, Variable.Name, ignore.case = TRUE) |
            grepl(query, Variable.Label, ignore.case = TRUE) |
            grepl(query, Topic, ignore.case = TRUE)
        )
    }
    
    df
  })
  
  # LOOKUP TABLE — built once per `filtered()` change, not once per cell.
  # Hashed by "Variable.Name|Year" for O(1) access instead of a dplyr scan
  # per cell. This is the piece that replaces the nested filter() calls.
  
  cellLookup <- reactive({
    df <- searched()
    
    pct <- suppressWarnings(as.numeric(df$Yearly.County.Coverage.Pct))
    
    list(
      pct    = setNames(pct, paste(df$Variable.Name, df$Year, sep = "|")),
      active = setNames(df$Active.Counties, paste(df$Variable.Name, df$Year, sep = "|"))
    )
  })
  
  
  # Helper: map a coverage pct -> color. 
  pctToColor <- function(pct) {
    if (is.na(pct)) return("#9e9e9e")
    if (pct == 0) return("#9e9e9e")
    if (pct >= 70) return("#2e7d32")
    if (pct >= 50) return("#fbc02d")
    "#c62828"
  }
  
  
  # MAIN EXPLORER
  
  output$explorer <- renderUI({
    
    df  <- searched()
    lk  <- cellLookup()
    yrs <- sort(unique(df$Year))
    
    domains <- sort(unique(df$Domain))
    
    # split once, instead of re-filtering by Domain/Topic inside nested lapply
    dom_split <- split(df, df$Domain)
    
    lapply(domains, function(dom) {
      
      dom_df <- dom_split[[dom]]
      
      topics <- sort(unique(dom_df$Topic))
      topic_split <- split(dom_df, dom_df$Topic)
      
      tags$div(
        
        tags$div(class = "domain-header", dom),
        
        lapply(topics, function(tp) {
          
          tp_df <- topic_split[[tp]]
          
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
                tags$div(style = "width:240px; min-width:240px;", "Variable"),
                lapply(yrs, function(y) {
                  tags$div(class = "year-label", y)
                })
              ),
              
              # VARIABLE ROWS
              lapply(seq_len(nrow(vars)), function(i) {
                
                v      <- vars$Variable.Name[i]
                vlabel <- vars$Variable.Label[i]
                
                tags$div(
                  class = "row",
                  
                  tags$div(
                    class = "var-name",
                    v
                  ),
                  
                  lapply(yrs, function(y) {
                    
                    key    <- paste(v, y, sep = "|")
                    na_val <- unname(lk$pct[key])      # NA if not found
                    
                    col <- pctToColor(na_val)
                    
                    tags$div(
                      class = "year-cell",
                      style = paste0("background:", col, ";"),
                      title = paste0(
                        vlabel, " — ", y, ": ",
                        ifelse(is.na(na_val), "no data", paste0(na_val, "% available"))
                      ),
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
  
  
  # CLICK POPUP
  
  observeEvent(input$clicked_var, {
    
    var <- input$clicked_var
    
    meta <- avail_dash_dat %>%
      filter(Variable.Name == var) %>%
      slice(1)
    
    detail <- avail_dash_dat %>%
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
  

# data download -----------------------------------------------------------

  output$download_data <- downloadHandler(
    filename = function() {
      "clean_ALL_data.csv"
    },
    content = function(file) {
      file.copy("clean_ALL_data.csv", file)
    }
  )  
  
  
} #end server
  
  

#  run app ----------------------------------------------------------------


shinyApp(ui, server)
