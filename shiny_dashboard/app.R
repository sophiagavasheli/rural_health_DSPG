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
             h2("Health Outcomes"),
             p("The studies we reviewed used many different indicators to assess health outcomes. Mortality outcomes include overall mortality (“Appalachia Then and Now,” 2015.; “Appalachian Diseases of Despair,” 2025; Mullens et al., 2024), premature deaths (Anderson et al., 2015), life expectancy (Basu et al., 2019), inpatient mortality (Gujral & Basu, 2019), and diseases of despair such as suicide and drug overdose (“Appalachian Diseases of Despair, 2025,” n.d.). Morbidity and chronic disease outcomes include diabetes prevalence (“Appalachia Then and Now,” 2015; Connect2Health FCC Task Force, 2019), cancer incidence (Hong et al., 2023; Zeng et al., 2015), cardiovascular disease (Basu et al., 2019; Hong et al., 2023; Zeng et al., 2015), Chronic Obstructive Pulmonary Disease (Basu et al., 2019; Gujral & Basu, 2019; Zeng et al., 2015), and obesity prevalence (“Appalachia Then and Now,” 2015). Other health outcomes include the number of poor physical health days (Anderson et al., 2015), poor mental health days (Anderson et al., 2015), low birth weight (Anderson et al., 2015), telemedicine usage rates (Cummins et al., 2025), visit completion rates (Haggerty et al., 2022), emergency room visits (Bailey et al., 2024), and missed appointments (Bailey et al., 2024; Haggerty et al., 2022)."),
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
             p("In the research, not many studies have attempted to examine causal relationships between infrastructure and health outcome variables on a US-wide scale. This is exactly the gap we intend to fill. To be able to examine causal relationships, we aim to build a reproducible dataset with data from each US county examining various infrastructure measures and their impacts on health outcomes, using controls such as demographic information and rurality.")
             )
    )
  ),
  
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
        style = "display: flex; align-items:center; margin-bottom:20px; gap: 20px;",
        img(src = "fcc.png",height = "150px", width = "150px",style = "flex-shrink:0;"),
        div(
          tags$a(
            "Federal Communications Commission",
            href = "https://www.fcc.gov/",
            target_ = "_blank",
            style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#0072B2;"
        ),
        p('"The Federal Communications Commission (FCC) regulates interstate and international communications by radio, television, wire, satellite, and cable in all 50 states, the District of Columbia and U.S. territories. A U.S. government agency overseen by Congress, the Commission is the federal agency responsible for implementing and enforcing America’s communications law & regulations."',
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
            href = "https://www.census.gov/programs-surveys/acs.html",
            target = "_blank",
            style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#0072B2;"
          ),
          p("\"The American Community Survey (ACS) is the premier source of detailed information about the nation's people and housing. As an ongoing survey conducted by the U.S. Census Bureau since 2005, the ACS collects detailed social, economic, housing, and demographic information from a sample of households across the 50 states, the District of Columbia, and Puerto Rico.\"",
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
          p('"PLACES provides health and health-related data using small area estimation for counties, incorporated and census designated places, census tracts, and ZIP Code Tabulation Areas (ZCTAs) across the United States. This project, which started in 2015, is a partnership between CDC, the Robert Wood Johnson Foundation, and the CDC Foundation."',
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
      
      div(
        style = "text-align:center;",
        h2("A special thanks to the entire 2026 DSPG Cohort"),
        img(src = "interns.jpg", 
          height = "390px", 
          width = "700px"), 
        hr(),
        img(src = 'kohlcentre_logo.png', 
            height = "200px",
            style = "margin-bottom:25px;"
            
            )
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
