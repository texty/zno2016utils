library(dplyr)
library(stringr)

df <- read.csv('zno_2016_utf8.csv', dec=',', sep=';', na.strings="null")

######################################
#todo temporary code
last_word <- function(TerName) {
  res <- str_extract(TerName, '[^ ]+$')
  return(res)
} 

df_last <- df0 %>% 
  mutate(ter_last=last_word(TerName)) %>% 
  mutate(area_last=last_word(AreaName))

########################################

is_city_region <- function(TerName) {
  return (str_detect(TerName, 'район міста$'))
}

is_region <- function(AreaName) {
  return (str_detect(AreaName, 'район$'))
}

city_name <- function(TerName, AreaName) {
  res <- ifelse(is_city_region(TerName), AreaName %>% as.character(), TerName %>% as.character())
  return(res)
}

region_name <- function(AreaName) {
  res <- ifelse(is_region(AreaName), AreaName %>% as.character(), NA)
  return(res)
}

#todo out to csv
df_out <- df0 %>% 
  mutate(city=city_name(TerName, AreaName)) %>% 
  mutate(region=region_name(AreaName)) %>% 
  mutate(oblast=Regname)