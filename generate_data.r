library(tidyverse)
library(zoo)
library(data.table)

my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
my.min <- function(x) ifelse( !all(is.na(x)), min(x, na.rm=T), NA)

setwd("~/GitHub/COVID19_Monitor")
###Data Generation Function

###Data Generation Function
covid_data_gen <- function(directory="data/"){
    covid_county_raw <- read.csv(paste0(directory, "us-counties.csv"))

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
        covid_county_list[[i]] <- covid_county_list[[i]][order(as.Date(covid_county_list[[i]]$date)),]
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
            mutate(cases_pop = (cases/Pop)*100000) %>%
            mutate(deaths_pop = (deaths/Pop)*100000) %>%
            mutate(new_cases_pop = (new_cases)/Pop*100000) %>%
            mutate(new_deaths_pop = (new_deaths/Pop)*100000) %>%
            mutate(new_cases_7d_pop = (new_cases_7d_avg)/Pop*100000) %>%
            mutate(new_deaths_7d_pop = (new_deaths_7d_avg/Pop)*100000)
    }



    covid_county_processed <- as.data.frame(data.table::rbindlist(covid_county_list))


    covid_county_merge_spatial <- merge(zips[,c("index", "zip_code", "latitude", "longitude")], covid_county_processed, by="index")
    covid_county_merge_spatial$date <- as.Date(covid_county_merge_spatial$date)

    date_bundle <- list(covid_county_processed=as.data.table(covid_county_processed), covid_county_merge_spatial=as.data.table(covid_county_merge_spatial))
    return(date_bundle)
}


data_bundle <- covid_data_gen()

saveRDS(data_bundle, "data/covid_data.rds", compress="xz")


