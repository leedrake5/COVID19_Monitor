library(ggplot2)      # For plotting
library(tmap)         # For creating tmap
library(tmaptools)    # For reading and processing spatial data related to tmap
library(dplyr)        # For data wrangling
library(sf)
library(raster)
library(leaflet)
library(tidyverse)
library(zoo)
library(data.table)
library(USAboundaries)
library(USAboundariesData)

lm_eqn = function(m) {
    
    l <- list(a = format(coef(m)[1], digits = 2),
    b = format(abs(coef(m)[2]), digits = 2),
    r2 = format(summary(m)$r.squared, digits = 3));
    
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
    
    
    as.character(as.expression(eq));
}

my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
my.min <- function(x) ifelse( !all(is.na(x)), min(x, na.rm=T), NA)
zips <- read.csv("data/zipcodes.csv")

#USA <- getData("GADM", country = "usa", level = 2)
USA_counties <- readRDS("data/gadm36_USA_2_sp.rds")
#ggtract<-fortify(USA, region = "GEOID")

data_bundle <- readRDS("data/covid_data.rds")
covid_county_processed <- data_bundle$covid_county_processed
covid_county_merge_spatial <- data_bundle$covid_county_merge_spatial

#counties <- us_counties()
#counties$index <- paste0(counties$state_name, "_", counties$name)
#covid_county_processed$state_name
#covid_counties <- counties %>% left_join(covid_county_processed, by = c("index"))
#USA <- covid_counties

USA_counties$index <-  paste0(USA_counties$NAME_1, "_", USA_counties$NAME_2)
#USA <- sp::merge(USA, covid_county_processed, by="index", duplicateGeoms=TRUE)

###Data Generation Function
covid_data_gen <- function(directory="data/"){
    covid_county_raw <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

    covid_county_raw$index <- paste0(covid_county_raw$state, "_", covid_county_raw$county)
    county_pops <- read.csv(paste0(directory, "countypops.csv"))
    county_pops$index <- paste0(county_pops$State, "_", county_pops$County)
    zips <- read.csv(paste0(directory, "zipcodes.csv"))
    zips$index <- paste0(zips$state, "_", zips$county)

    covid_county_merge <- merge(county_pops[,c("index", "Pop")], covid_county_raw, by="index")


    unique_counties <- unique(covid_county_merge$index)
    covid_county_list <- list()
    for(i in unique_counties){
        covid_county_list[[i]] <- covid_county_merge[covid_county_merge$index %in% i, ]
    }

    for(i in unique_counties){
        covid_county_list[[i]] <- covid_county_list[[i]] %>%
            mutate(date = as.Date(date)) %>%
            arrange(date) %>%
            mutate(new_cases = cases - dplyr::lag(cases)) %>% #new cases is diff over a 1-day lag
            mutate(new_cases_7d_avg = rollmean(new_cases, k=7, na.pad=TRUE, align="right")) %>%
            mutate(new_deaths = deaths - dplyr::lag(deaths)) %>%
            mutate(new_deaths_7d_avg = rollmean(new_deaths, k=7, na.pad=TRUE, align="right")) %>%
            mutate(new_cases = ifelse(new_cases < 0, 0, new_cases)) %>% #Set negative days to zero
            mutate(new_deaths = ifelse(new_deaths < 0, 0, new_deaths)) %>% #Set negative days to zero
            filter(!is.na(new_cases_7d_avg)) %>%
            mutate(cases_norm = new_cases/my.max(new_cases)) %>%
            mutate(deaths_norm = new_deaths/my.max(new_deaths)) %>%
            mutate(cases_pop = cases/Pop) %>%
            mutate(deaths_pop = deaths/Pop) %>%
            mutate(new_cases_pop = new_cases/Pop) %>%
            mutate(new_deaths_pop = new_deaths/Pop)
    }



    covid_county_processed <- as.data.frame(data.table::rbindlist(covid_county_list))


    covid_county_merge_spatial <- merge(zips[,c("index", "zip_code", "latitude", "longitude")], covid_county_processed, by="index")
    covid_county_merge_spatial$date <- as.Date(covid_county_merge_spatial$date)

    date_bundle <- list(covid_county_processed=as.data.table(covid_county_processed), covid_county_merge_spatial=as.data.table(covid_county_merge_spatial))
    return(date_bundle)
}
