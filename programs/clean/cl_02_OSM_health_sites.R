# cleaning 2023 OSM health sites, assigning amenities and counties

library(sf)
library(stringr)
library(purrr)
library(tibble)
library(tidyr)
library(stringdist)


# load
health <- st_read("data/outcome/OSM/us_health_2023_deduplicated.geojson")

keep = c("hospital", "clinic", "doctors", "pharmacy", "healthcare", "first_aid", "Counseling", 
         "doctor", "doctors;pharmacy", "clinic;doctors", "dentist", "audiologist", "nursing_home")

filtered = health %>% 
  filter(!is.na(name)) %>% 
  filter(is.na(amenity) | amenity %in% keep) 

# manually changing amenity of the two sites under "doctors;pharmacy" and "clinic;doctors"
filtered$amenity[63795] = "clinic"
filtered$amenity[63486] = "clinic"

# lookup table to assign missing amenities

lookup <- tribble(
  ~pattern, ~amenity,
  
  # Pharmacies
  "pharmacy|walgreens|wallgreen's|wallgreens|cvs|rite[- ]?aid|pharmaca|kroger|albertsons|haggen|drug|publix|health mart|safeway|target|giant|medicine shoppe|apothecary", 
  "pharmacy",
  
  # Dentists
  "dentist|dentists|dentistry|root canal|dental|dental associates|family dental|orthodont|braces|endodontics|teeth|tooth|smile|\\bdds\\b|\\bdmd\\b|\\bd\\.d\\.s.?\\b|\\bd\\.m\\.d.?\\b",
  "dentist",
  
  #vision
  "optometr|optometric|ophthalm|vision|vision center|eye care|eyecare|eye center|eye clinic|eye associates|eye and contact|eye institute|eye|optical|optician|optics|retina", 
  "vision", 
  
  # Hospitals
  "hospital|medical center|regional medical center|specialty hospital|critical access hospital|sanatorium", 
  "hospital",
  
  # Clinics
  "clinic|urgent|walk[- ]?in|express care|immediate care|nowcare|family (medicine|medical|practice|care|planning|health|physicians)|internal medicine|community health center|community clinic|health clinic|free clinic|student health|outpatient clinic|va clinic|primary care|medical group|medical care|medical specialties|medical associates|planned parenthood|carbon health|one medical|women's care|women's health|medexpress|zoomcare|occupational medicine|medicenter|medical mall|medical plaza|professional building|professional center|professional plaza|medical suites|medical tower|patient first|patients first|care plus|clearchoicemd|medics usa|peak vista|kaiser|kaiser permanente|centra care|medcenter|women's center|first aid|\\bems\\b|ambulance|emergency medical service|\\ber\\b|emergency|\\bemt\\b",
  "clinic_urgent_care",
  
  # Aesthetic & Wellness
  "aesthetic|medispa|med[- ]?spa|massage|bodywork|beauty|plastic surgery|cosmetic|\\bspa\\b|day spa|salon|skincare|skin care|\\bvein\\b|vein care|vein specialist|\\blaser\\b|\\blipo\\b|botox|injectable|filler|ageless|\\brenew\\b|\\brevive\\b|rejuvenat|\\bface\\b|facial|contour|sculpt|cryotherapy|skin",
  "specialists_aesthetic",
  
  # 1. doctors and medical specialists
  "obstetrics|gynecology|ob/gyn|ob-gyn|obgyn|reproductive health|pediatric|pediatrix|neonatal|midwife|midwifery|fertility|maternal|children's|gynaecology|birth|oncolog|cancer|chemo|chemotherapy|infusion center|radiation center|allergy|allergist|asthma|audiology|audiologist|hearing|sleep|surgeon|surgical|surgery|cardiology|cardiovascular|dermatolog|urolog|neurolog|endocrinolog|\\bent\\b|ear nose & throat|ear nose and throat|ear\\, nose|gastroenterolog|otolaryngology|surgi-center|digestive|vein care|diabetes|osteoporosis|pulmonary|vascular|anesthesiology|doctor|doctors|physician|physicians|specialist|specialists|\\bdr\\.?\\b|\\bdrs\\b|\\bmd\\b|\\bdo\\b|\\bnp\\b|\\bfnp\\b|\\baprn\\b|\\bpa\\b|\\bm\\.d.?\\b|\\bd\\.o.?\\b|\\bpc\\b|\\bpllc\\b|adult medicine",
  "doctors_medical_specialists",
  
  # 2. Pain / Physical Therapy / Musculoskeletal
  "chiro|spine|spinal|physical therapy|\\bpt\\b|pt plus|occupational therapy|sports medicine|sports therapy|podiatry|podiatr|foot and ankle|foot & ankle|foot care|foot specialist|prosthetic|orthopedic|orthopaed|acupunctur|pain|pain care|pain management|\\bdpm\\b|\\bd\\.p\\.m\\b|\\bdc\\b|rheumatolog|arthritis|orthodics|\\bfeet\\b|hand center",
  "specialists_musculoskeletal_pain",
  
  
  # 4. Kidneys & Renal Care
  "kidney|renal|dialysis|nephrolog",
  "kidney_care",
  
  # 5. Diagnostics & Imaging
  "imaging|mri|diagnostic|diagnostics|radiolog|radiologist|x-ray|xray|ultrasound|scan center|pathology|blood center|laboratories|labcorp|endoscopy",
  "diagnostics",
  
  
  # Mental Health
  "mental health|behavioral health|behavior|counseling|counselling|psychiatry|psychiatric|psychology|psychological|recovery center|behavioral medicine|resource center|psychologist|counsel|psychotherapy", 
  "mental_health",
  
  
  # rehab
  "rehabilitation|rehab", "rehab",
  
  # Nursing / Long-term Care
  "nursing home|nursing|skilled nursing|recovery|assisted living|retirement home|retirement community|senior living|living center|memory care|convalescent|hospice|good samaritan|good shepherd|lutheran home|veterans home|care center|care facility|healthcare of|health care center|manor|village|campus|rest home|residence|residential|house|apartments|inn|community estate|guest homes|haven|home|specialty care", 
  "nursing_home", 
  
  # other Healthcare (Catch-all for names with health/medical)
  "healthcare|health care|health department|health services|health system|health center|wellness center|wellness|medical office|medical offices|medical arts|medical building|medical pavilion|health park|healthsource|aspirus|bickford|health centre|health service|health|phd|ph\\.d|red cross", 
  "other_healthcare"
)

# reclassify
fixed <- filtered %>%
  mutate(
    amenity = map2_chr(name, amenity, function(x, orig_amenity) {
      
      # Clean string for regex matching
      x_clean <- x %>%
        str_trim() %>%
        str_squish() %>%
        str_to_lower()
      
      # Search the lookup table
      hit <- lookup %>%
        filter(str_detect(x_clean, regex(pattern, ignore_case = TRUE))) %>%
        pull(amenity)
      
      # Core change: if hit is found, reclassify; if length is 0, keep orig_amenity
      if (length(hit) == 0) {
        return(orig_amenity) 
      } else {
        return(hit[1])
      }
    })
  )


# stats
sum(!is.na(filtered$amenity))/nrow(filtered) # 35% classified
sum(!is.na(fixed$amenity))/nrow(fixed) # 92% classified
unique(fixed$amenity)

# final cleaning
final_clean = fixed %>% 
  filter(!is.na(amenity)) %>% 
  mutate(amenity = case_when(
    amenity == "first_aid" ~ "clinic_urgent_care",
    amenity == "clinic" ~ "clinic_urgent_care",
    amenity == "doctors" ~ "other_healthcare",
    amenity == "healthcare" ~ "other_healthcare",
    amenity == "Counseling" ~ "mental_health",
    TRUE ~ amenity
  )) %>% 
  select(-osm_id)


# spatial join with counties
counties <- readRDS("data/outcome/census/us_counties_2020.rds") %>% st_transform(4326)

health_pts = st_transform(final_clean, crs=4326)

joined <- st_join(
  health_pts,
  counties %>% select(GEOID, NAME, STATEFP),
  join = st_within
)

joined_clean = joined %>% 
  rename(
    county_fips = GEOID,
    county = NAME,
    health_site_name = name,
    health_site_type = amenity,
    state_fips = STATEFP
  )


saveRDS(joined_clean, "data/outcome/OSM/clean_health_sites_2023.rds")

saveRDS(lookup, "reference/health_sites_lookup_tab.rds")