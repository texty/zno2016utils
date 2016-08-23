library(dplyr)
library(stringr)

df <- read.csv('zno_2016_utf8.csv', dec=',', sep=';', na.strings="null")

######################################
#todo temporary code
last_word <- function(TerName) {
  res <- str_extract(TerName, '[^ ]+$')
  return(res)
} 

first_word <- function(city) {
  res <- str_extract(city, '^[^ .]+[ .]')
  return(res)
}

#df_last <- df %>% 
#  mutate(ter_last=last_word(TerName)) %>% 
#  mutate(area_last=last_word(AreaName))
########################################

is_city_region <- function(TerName) {
  return (str_detect(TerName, 'район міста$'))
}

is_region <- function(AreaName) {
  return (str_detect(AreaName, 'район$'))
}

city_name_full <- function(TerName, AreaName) {
  res <- ifelse(is_city_region(TerName), AreaName %>% as.character(), TerName %>% as.character())
  return(res)
}

region_name <- function(AreaName) {
  res <- ifelse(is_region(AreaName), AreaName %>% as.character(), NA)
  return(res)
}

city_type <- function(city) {
  res <- str_extract(city, '^(м|с-ще|смт|с)')
  return(res)
}

city_name <- function(city) {
  res <- city %>% str_replace('^(м.|с-ще |смт |с.)', '') %>% str_trim()
  return(res)
}

#todo out to csv
df0 <- df %>% 
  mutate(city_full=city_name_full(TerName, AreaName)) %>% 
  mutate(region=region_name(AreaName)) %>%
  mutate(city_region=ifelse(is_city_region(TerName), TerName, NA)) %>% 
  mutate(city_type=city_type(city_full)) %>% 
  mutate(city_name=city_name(city_full)) %>% 
  rename(oblast=Regname)

df0 %>% write.csv('zno_2016_final.csv', row.names=FALSE) 
  