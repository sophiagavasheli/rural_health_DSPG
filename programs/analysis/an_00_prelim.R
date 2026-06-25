# preliminary models

library(dplyr)
library(lfe)
library(corrplot)
library(ggplot2)

#dat = read.csv("shiny_dashboard/clean_FCC_CLH_data.csv")

filtered = dat %>% 
  select(
    YEAR, COUNTYFIPS, STATE, COUNTY,
    #controls
    ACS_MEDIAN_AGE, ACS_PCT_AIAN, ACS_PCT_ASIAN, ACS_PCT_BLACK, ACS_PCT_HISPANIC, ACS_PCT_WHITE, ACS_MEDIAN_HH_INC, ACS_PCT_POSTHS_ED, SAHIE_PCT_UNINSURED64,
    
    #health outcomes
    CHR_PCT_DIABETES, CDCA_STROKE_DTH_RATE_ABOVE35, CHR_PCT_LOW_BIRTH_WT, CHR_PREMAT_DEATH_RATE, CDCA_HEART_DTH_RATE_ABOVE35, CHR_PCT_ADULT_OBESITY,CHR_PCT_ALCOHOL_DRIV_DEATH,
    
    #infrastructure
    AHRF_HOSP_BED_RATE, AHRF_HOSPS_RATE, POS_HOSP_OBSTETRIC_RATE,  AHRF_OB_GYN_RATE, AHRF_MED_SPEC_RATE, POS_HOSP_ALC_RATE, AHRF_CARDIOVAS_SPEC_RATE, FCC_res_connections_10_mbps, ACS_PCT_DRIVE_2WORK, ACS_PCT_PUBL_TRANSIT
    ) %>% 
  filter(YEAR >= 2012, YEAR <= 2017) %>% 
  group_by(COUNTYFIPS) %>%
  #lag 1 year
  mutate(across(c(ACS_MEDIAN_HH_INC, ACS_PCT_POSTHS_ED, SAHIE_PCT_UNINSURED64), 
                ~lag(.x, n = 1, order_by = YEAR), .names = "{.col}_lag1"))  %>% 
  ungroup() %>% 
  #scale values
  mutate(across(where(is.numeric) & -c(YEAR, COUNTYFIPS), 
                ~as.numeric(scale(.x, center = TRUE, scale = TRUE)))) %>% 
  filter(YEAR > 2012)


#simple regressions: outcome ~ predictors | fixed effects

diab = felm(CHR_PCT_DIABETES ~ ACS_MEDIAN_AGE+ ACS_PCT_AIAN+ ACS_PCT_ASIAN+ ACS_PCT_BLACK+ ACS_PCT_HISPANIC+ ACS_PCT_WHITE+ ACS_MEDIAN_HH_INC_lag1+ ACS_PCT_POSTHS_ED_lag1+ SAHIE_PCT_UNINSURED64_lag1+ AHRF_HOSP_BED_RATE+ AHRF_HOSPS_RATE+ POS_HOSP_OBSTETRIC_RATE+  AHRF_OB_GYN_RATE+ AHRF_MED_SPEC_RATE+ POS_HOSP_ALC_RATE+ AHRF_CARDIOVAS_SPEC_RATE+ FCC_res_connections_10_mbps+ ACS_PCT_DRIVE_2WORK+ ACS_PCT_PUBL_TRANSIT
             | YEAR + COUNTYFIPS, data = filtered)


low_birth = felm(CHR_PCT_LOW_BIRTH_WT ~ ACS_MEDIAN_AGE+ ACS_PCT_AIAN+ ACS_PCT_ASIAN+ ACS_PCT_BLACK+ ACS_PCT_HISPANIC+ ACS_PCT_WHITE+ ACS_MEDIAN_HH_INC_lag1+ ACS_PCT_POSTHS_ED_lag1+ SAHIE_PCT_UNINSURED64_lag1+ AHRF_HOSP_BED_RATE+ AHRF_HOSPS_RATE+ POS_HOSP_OBSTETRIC_RATE+  AHRF_OB_GYN_RATE+ AHRF_MED_SPEC_RATE+ POS_HOSP_ALC_RATE+ AHRF_CARDIOVAS_SPEC_RATE+ FCC_res_connections_10_mbps+ ACS_PCT_DRIVE_2WORK+ ACS_PCT_PUBL_TRANSIT
                 | YEAR + COUNTYFIPS, data = filtered)

stroke = felm(CDCA_STROKE_DTH_RATE_ABOVE35 ~ ACS_MEDIAN_AGE+ ACS_PCT_AIAN+ ACS_PCT_ASIAN+ ACS_PCT_BLACK+ ACS_PCT_HISPANIC+ ACS_PCT_WHITE+ ACS_MEDIAN_HH_INC_lag1+ ACS_PCT_POSTHS_ED_lag1+ SAHIE_PCT_UNINSURED64_lag1+ AHRF_HOSP_BED_RATE+ AHRF_HOSPS_RATE+ POS_HOSP_OBSTETRIC_RATE+  AHRF_OB_GYN_RATE+ AHRF_MED_SPEC_RATE+ POS_HOSP_ALC_RATE+ AHRF_CARDIOVAS_SPEC_RATE+ FCC_res_connections_10_mbps+ ACS_PCT_DRIVE_2WORK+ ACS_PCT_PUBL_TRANSIT
              | YEAR + COUNTYFIPS, data = filtered)

obesity = felm(CHR_PCT_ADULT_OBESITY ~ ACS_MEDIAN_AGE+ ACS_PCT_AIAN+ ACS_PCT_ASIAN+ ACS_PCT_BLACK+ ACS_PCT_HISPANIC+ ACS_PCT_WHITE+ ACS_MEDIAN_HH_INC_lag1+ ACS_PCT_POSTHS_ED_lag1+ SAHIE_PCT_UNINSURED64_lag1+ AHRF_HOSP_BED_RATE+ AHRF_HOSPS_RATE+ POS_HOSP_OBSTETRIC_RATE+  AHRF_OB_GYN_RATE+ AHRF_MED_SPEC_RATE+ POS_HOSP_ALC_RATE+ AHRF_CARDIOVAS_SPEC_RATE+ FCC_res_connections_10_mbps+ ACS_PCT_DRIVE_2WORK+ ACS_PCT_PUBL_TRANSIT
               | YEAR + COUNTYFIPS, data = filtered)

alc = felm(CHR_PCT_ALCOHOL_DRIV_DEATH ~ ACS_MEDIAN_AGE+ ACS_PCT_AIAN+ ACS_PCT_ASIAN+ ACS_PCT_BLACK+ ACS_PCT_HISPANIC+ ACS_PCT_WHITE+ ACS_MEDIAN_HH_INC_lag1+ ACS_PCT_POSTHS_ED_lag1+ SAHIE_PCT_UNINSURED64_lag1+ AHRF_HOSP_BED_RATE+ AHRF_HOSPS_RATE+ POS_HOSP_OBSTETRIC_RATE+  AHRF_OB_GYN_RATE+ AHRF_MED_SPEC_RATE+ POS_HOSP_ALC_RATE+ AHRF_CARDIOVAS_SPEC_RATE+ FCC_res_connections_10_mbps+ ACS_PCT_DRIVE_2WORK+ ACS_PCT_PUBL_TRANSIT
           | YEAR + COUNTYFIPS, data = filtered)


heart_dth = felm(CDCA_HEART_DTH_RATE_ABOVE35 ~ ACS_MEDIAN_AGE+ ACS_PCT_AIAN+ ACS_PCT_ASIAN+ ACS_PCT_BLACK+ ACS_PCT_HISPANIC+ ACS_PCT_WHITE+ ACS_MEDIAN_HH_INC_lag1+ ACS_PCT_POSTHS_ED_lag1+ SAHIE_PCT_UNINSURED64_lag1+ AHRF_HOSP_BED_RATE+ AHRF_HOSPS_RATE+ POS_HOSP_OBSTETRIC_RATE+  AHRF_OB_GYN_RATE+ AHRF_MED_SPEC_RATE+ POS_HOSP_ALC_RATE+ AHRF_CARDIOVAS_SPEC_RATE+ FCC_res_connections_10_mbps+ ACS_PCT_DRIVE_2WORK+ ACS_PCT_PUBL_TRANSIT
                 | YEAR + COUNTYFIPS, data = filtered)


summary(diab)
summary(low_birth)
summary(stroke)
summary(obesity)
summary(alc)
summary(heart_dth)


# coefficient plots
coef_plot <- function(model, health_out) {
  
  vars = c("AHRF_HOSP_BED_RATE", "AHRF_HOSPS_RATE", "POS_HOSP_OBSTETRIC_RATE",  
           "AHRF_OB_GYN_RATE", "AHRF_MED_SPEC_RATE", "POS_HOSP_ALC_RATE",
           "AHRF_CARDIOVAS_SPEC_RATE", "FCC_res_connections_10_mbps", 
           "ACS_PCT_DRIVE_2WORK", "ACS_PCT_PUBL_TRANSIT")
  var_labels <- c(
    "AHRF_HOSP_BED_RATE" = "Hospital Beds per 100k",
    "AHRF_HOSPS_RATE" = "Hospitals per 100k",
    "POS_HOSP_OBSTETRIC_RATE" = "Obstetric Hospitals per 100k",
    "AHRF_OB_GYN_RATE" = "OB/GYN Physicians per 100k",
    "AHRF_MED_SPEC_RATE" = "Medical Specialists per 100k",
    "POS_HOSP_ALC_RATE" = "Alcohol Treatment Hospitals per 100k",
    "AHRF_CARDIOVAS_SPEC_RATE" = "Cardiovascular Specialists per 100k",
    "FCC_res_connections_10_mbps" = "Broadband Connections (10 Mbps)",
    "ACS_PCT_DRIVE_2WORK" = "% Drive to Work",
    "ACS_PCT_PUBL_TRANSIT" = "% Public Transit"
  )
  coef_df <- broom::tidy(model, conf.int = TRUE)
  coef_df = coef_df %>% 
    filter(term %in% vars) %>% 
    mutate(
      sig = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        TRUE            ~ ""
      )) %>% 
    mutate(
      term_label = recode(term, !!!var_labels)
    )
    

 p= ggplot(coef_df, aes(x = estimate,
                      y = reorder(term_label, estimate))) +
    geom_point(colour = "maroon") +
    geom_errorbarh(aes(xmin = conf.low,
                       xmax = conf.high),
                   height = .2,
                   linewidth = 0.5,
                   colour = "maroon") +
    geom_text(
      aes(label = sig),
      hjust = -0.5,
      size = 6
    ) +
    geom_vline(xintercept = 0,
               linetype = "dashed") + 
    labs(
      title = paste("Coefficient Estimates and Intervals for", health_out, "Model"),
      y = "Model Variables",
      x = "Estimate",
      caption = "Sources: FCC and CLH, 2013-2017"
    ) +
    theme_minimal() +
    theme(
      plot.caption = element_text(hjust = 0.5),
      plot.title = element_text(hjust = 0)
    )
  
  ggsave(
    paste0("graphs/prelim_regress/", gsub(" ", "_", health_out), ".png"),
    plot = p,
    width = 10,
    height = 6,
    dpi = 300,  bg = "white"
  )
}
  
coef_plot(diab, "Diabetes Percentage")
coef_plot(low_birth, "Low Birth Rate")
coef_plot(stroke, "Stroke Death Rate")
coef_plot(obesity, "Adult Obesity Percentage")
coef_plot(alc, "Percentage of Alcohol Driving Deaths")
coef_plot(heart_dth, "Cardiovascular Death Rate (35+)")



# regressions for 1 year

dat2013 = filtered %>% 
  filter(YEAR ==2013)

dat2017 = filtered %>% 
  filter(YEAR == 2017)


#corr plot
cor_data = filtered %>% 
  tidyr::drop_na()


corrplot(cor(cor_data), 
         method = "shade",
         tl.cex = 0.6,
         tl.srt = 90,
         tl.col = "black", 
         type = "lower"
)