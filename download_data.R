library(tidyverse)
library(lubridate)
library(sf)
library(jsonlite)
library(httr)


options(timeout = max(300, getOption("timeout"))) # increase timeout to improve probability for download success
download.file("https://github.com/robert-koch-institut/SARS-CoV-2-Infektionen_in_Deutschland/raw/main/Aktuell_Deutschland_SarsCov2_Infektionen.csv",
              destfile = "data/Aktuell_Deutschland_SarsCov2_Infektionen.csv")
RKIraw <- read_csv("data/Aktuell_Deutschland_SarsCov2_Infektionen.csv", show_col_types = FALSE)
# What is a case in rki? Complicated but becomes clearer when summing across gender, age, ...
# The variable IdLandkreis is the "Amtlicher Gemeindeschlüssel" 
# https://de.wikipedia.org/wiki/Amtlicher_Gemeindeschl%C3%BCssel for each Landkreis
# We ignore the last three digits end get the Bundesland Code number
# Use the BL codes fitting the the numerical code for the Amtlicher Gemeindeschlüssel
BL <- c("SH","HH","NI","HB","NW","HE","RP","BW","BY","SL","BE","BB","MV","SN","ST","TH")
RKIstate <- 
  # First make a grid of all dates and all states to include zero-cases days
  expand_grid(stateCode = BL, Refdatum = as_date("2020-01-01"):as_date("2022-12-31") |> as_date()) |> 
  # join all days with cases
  left_join(
    RKIraw %>%  mutate(stateCode = factor(IdLandkreis %/% 1000, labels = BL)) %>% 
      group_by(stateCode, Refdatum) %>% 
      summarize(Fall = sum(AnzahlFall), .groups = "drop"), 
    by = c("stateCode", "Refdatum")
  ) |> replace_na(list(Fall = 0))

# School Vacation Data
# Downloaded csv-files manually from 
# https://github.com/mehr-schulferien-de/www.mehr-schulferien.de/tree/master/priv/repo/seeds.d
get_summervac <- function(year) read_csv2(paste0("data/",year-1,"-",year,".csv")) |> select(Land, Sommer) |> 
  separate(Sommer, into = c("start","end"), sep = " [–-] ") |> 
  mutate(start =  paste0(start,year) |> lubridate::dmy(), 
         end =  paste0(end,year) |> lubridate::dmy(),
         year = year,
         BL = c("BW","BY","BE","BB","HB","HH","HE","MV","NI","NW","RP","SL","SN","ST","SH","TH"))
summervac <- bind_rows(get_summervac(2020),get_summervac(2021),get_summervac(2022))

# Shapefiles for federal states just for visualization purposes
download.file("https://biogeo.ucdavis.edu/data/diva/adm/DEU_adm.zip", "data/DEU_adm.zip")
unzip("data/DEU_adm.zip", exdir = "data/DEU_adm/")
DE_shp <- read_sf("data/DEU_adm/",layer = "DEU_adm1") %>% 
  mutate(BL = c("BW","BY","BE","BB","HB","HH","HE","MV","NI","NW","RP","SL","SN","ST","SH","TH"))
# Added BL manually checking https://de.wikipedia.org/wiki/ISO_3166-2:DE

save(DE_shp, RKIstate, summervac, file = "data/RKI_vacation_shp.RData")
