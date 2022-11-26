## Download and store local copies of data
# you would need you own StatsNZ API key for this not to error
######### Data updates
options(timeout=180)
urlloc <- "https://github.com/minhealthnz/nz-covid-data/raw/main/cases/covid-cases-in-hospital-counts-location.xlsx"
local_url <- "../miscData/daily_hospitalisations.xlsx"
if(!file.exists(local_url)){
  download.file(url=urlloc, mode = "wb",
                destfile = local_url)
}
  lastmod <- file.info(local_url)[["mtime"]]
  last_mod <- floor_date(lastmod, unit="day")
  this_day <- floor_date(Sys.time(), unit="day")
# check if it was downloaded today
if(last_mod != this_day){
  file.remove(local_url)
  Sys.sleep(5) # keeping this in OneDrive where syncing can slow deletion
  download.file(url=urlloc, mode = "wb",
                destfile = local_url)
}
rm("lastmod", "last_mod", "this_day", "urlloc", "local_url")

urlloc <- "https://github.com/minhealthnz/nz-covid-data/raw/main/cases/covid-case-counts.csv"
local_url <- "../miscData/mohGithubCovidCaseCounts.csv"
if(!file.exists(local_url)){
  download.file(url=urlloc, mode = "wb",
                destfile = local_url)
}
lastmod <- file.info(local_url)[["mtime"]]
last_mod <- floor_date(lastmod, unit="day")
this_day <- floor_date(Sys.time(), unit="day")
if(last_mod != this_day){
  file.remove(local_url)
  Sys.sleep(5)
  download.file(url=urlloc, mode = "wb",
                destfile = local_url)
}
Sys.sleep(2)
rm("lastmod", "last_mod", "this_day", "urlloc", "local_url")

######### Weekly updates

## hospital admissions, downloaded on Friday
urlloc <- "https://raw.githubusercontent.com/minhealthnz/nz-covid-data/main/cases/weekly-hospitalisations-for-covid.csv"
local_url <- "../miscData/mohGithubHospitalAdmissions.csv"
if(!file.exists(local_url)){
  download.file(url=urlloc, mode = "wb",
                destfile = local_url)
}
  lastmod <- file.info(local_url)[["mtime"]]
  last_mod <- floor_date(lastmod, unit="day")
  this_day <- floor_date(Sys.time(), unit="day")
if( last_mod != this_day & 
   wday(this_day, label = TRUE) == "Fri"){
  download.file(url=urlloc, mode = "wb",
                destfile = local_url)
}
rm("lastmod", "last_mod", "this_day", "urlloc", "local_url")

## deaths, downloaded on Friday
urlloc <- "https://github.com/minhealthnz/nz-covid-data/raw/main/cases/weekly-deaths.csv"
local_url <- "../miscData/mohGithubDeaths.csv"
if(!file.exists(local_url)){
  download.file(url=urlloc, mode = "wb",
                destfile = local_url)
}
lastmod <- file.info(local_url)[["mtime"]]
last_mod <- floor_date(lastmod, unit="day")
this_day <- floor_date(Sys.time(), unit="day")
if( last_mod != this_day & 
    wday(this_day, label = TRUE) == "Fri"){
  download.file(url=urlloc, mode = "wb",
                destfile = local_url)
}
rm("lastmod", "last_mod", "this_day", "urlloc", "local_url")


## NZ morality from Statsnz via API on Thursday
## rather than rewrite all the StatsNZ functions, am just
## doing a reset of the working directory
local_url <- "../miscData/statsnzAPIweeklyDeaths.csv"
if(!file.exists(local_url)){
  ### statnz API
  source("../additionalScripts/get-odata-fun.R")
  theres_data <- get_odata(
    service = "https://api.stats.govt.nz/opendata/v1",
    endpoint = "Covid-19Indicators",
    entity = "Observations",
    query_option = "$filter=(ResourceID eq 'CPWEE1')",
    service_api_key = "to_be_blunt_get_your_own_API_key")
  write.csv(theres_data, local_url)
}
lastmod <- file.info(local_url)[["mtime"]]
last_mod <- floor_date(lastmod, unit="day")
this_day <- floor_date(Sys.time(), unit="day")
if( last_mod != this_day & 
    wday(this_day, label = TRUE) == "Thu"){
  ### statnz API
  source("../additionalScripts/get-odata-fun.R")
  theres_data <- get_odata(
    service = "https://api.stats.govt.nz/opendata/v1",
    endpoint = "Covid-19Indicators",
    entity = "Observations",
    query_option = "$filter=(ResourceID eq 'CPWEE1')",
    service_api_key = "to_be_blunt_get_your_own_API_key")
  write.csv(theres_data, local_url)
}



## current cases webpage, downloaded on Monday, so only run when page updated
## not overwriting
datename <- format(Sys.Date(), "%y_%m_%d")
urlloc <- "https://www.health.govt.nz/covid-19-novel-coronavirus/covid-19-data-and-statistics/covid-19-current-cases"
local_url <- paste0("../miscData/currentCases/currentCases_", 
                    datename, ".html")
this_day <- floor_date(Sys.time(), unit="day")
if(!file.exists(local_url) & 
   wday(this_day, label = TRUE) == "Mon"){
  download.file(url=urlloc, mode = "wb",
                destfile = local_url)
}
rm("datename", "this_day" ,"urlloc", "local_url")

## ESR wastewater counts, downloaded on Friday
urlloc <- "https://raw.githubusercontent.com/ESR-NZ/covid_in_wastewater/main/data/ww_site.csv"
local_url <- "../miscData/esrGithubww_site.csv"
if(!file.exists(local_url)){
  download.file(url=urlloc, mode = "wb",
                destfile = local_url)
}
lastmod <- file.info(local_url)[["mtime"]]
last_mod <- floor_date(lastmod, unit="day")
this_day <- floor_date(Sys.time(), unit="day")
if( last_mod != this_day & 
    wday(this_day, label = TRUE) == "Fri"){
  download.file(url=urlloc, mode = "wb",
                destfile = local_url)
}
rm("lastmod", "last_mod", "this_day", "urlloc", "local_url")

## ESR wastewater site reference, downloaded on Friday
urlloc <- "https://github.com/ESR-NZ/covid_in_wastewater/raw/main/data/sites.csv"
local_url <- "../miscData/esrGithubsites.csv"
if(!file.exists(local_url)){
  download.file(url=urlloc, mode = "wb",
                destfile = local_url)
}
lastmod <- file.info(local_url)[["mtime"]]
last_mod <- floor_date(lastmod, unit="day")
this_day <- floor_date(Sys.time(), unit="day")
if( last_mod != this_day & 
    wday(this_day, label = TRUE) == "Fri"){
  download.file(url=urlloc, mode = "wb",
                destfile = local_url)
}
rm("lastmod", "last_mod", "this_day", "urlloc", "local_url")

## ESR wastewater case counts reference, downloaded on Friday
urlloc <- "https://github.com/ESR-NZ/covid_in_wastewater/raw/main/data/cases_site.csv"
local_url <- "../miscData/esrGithubcases_site.csv"
if(!file.exists(local_url)){
  download.file(url=urlloc, mode = "wb",
                destfile = local_url)
}
lastmod <- file.info(local_url)[["mtime"]]
last_mod <- floor_date(lastmod, unit="day")
this_day <- floor_date(Sys.time(), unit="day")
if( last_mod != this_day & 
    wday(this_day, label = TRUE) == "Fri"){
  download.file(url=urlloc, mode = "wb",
                destfile = local_url)
}
rm("lastmod", "last_mod", "this_day", "urlloc", "local_url")

