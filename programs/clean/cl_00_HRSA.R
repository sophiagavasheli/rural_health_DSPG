# cleaning HRSA Area Health Resources Data
# Sophia

library(here)

# data contained in the subset files
# env = environmental vars
# exp = expenditures
# geo = geographic stuff
# hf = health facilities
# hp = health professions
# pop = population demographics
# utl = utilization


env = read.csv(here("data", "source", "HRSA", "AHRF2025env.csv"))

exp = read.csv(here("data", "source", "HRSA", "AHRF2025exp.csv"))

geo = read.csv(here("data", "source", "HRSA", "AHRF2025geo.csv"))

hf = read.csv(here("data", "source", "HRSA", "AHRF2025hf.csv"))

hp = read.csv(here("data", "source", "HRSA", "AHRF2025hp.csv"))

pop = read.csv(here("data", "source", "HRSA", "AHRF2025pop.csv"))

utl = read.csv(here("data", "source", "HRSA", "AHRF2025utl.csv"))