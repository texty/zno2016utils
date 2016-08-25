###################################################################################################################
#                                                                                                                 #
#  Скрипт для графіки про міста, в яких найбільший відсоток учнів, що склали ЗНО з трьох предметів на 180+ балів  #
#                                                                                                                 #
###################################################################################################################

library(dplyr)
library(stringr)

oblast_center <- data.frame(c('Вінниця','Луцьк', 'Дніпропетровськ','Донецьк','Житомир','Ужгород','Запоріжжя','Івано-Франківськ','Київ','Кіровоград','Луганськ','Львів','Миколаїв','Одеса','Полтава','Рівне','Суми','Тернопіль','Харків','Херсон','Хмельницький','Черкаси','Чернівці','Чернігів'))
names(oblast_center)[1]='city_name'
oblast_center <- oblast_center %>% mutate(oblast_or_city=city_name)

df <- read.csv('../zno_2016_final.csv')


df1 <- df %>% 
  left_join(oblast_center, by='city_name') %>%
  mutate(oblast_or_city=as.character(oblast_or_city)) %>% 
  mutate(oblast_or_city=ifelse(!is.na(oblast_or_city), oblast_or_city, as.character(oblast))) 


# Вибираємо та групуємо дані для трьох різних груп предметів (u-українська, h-історія, e-англійська, m-математика, p-фізика)

uhe <- df1 %>% 
  filter(city_type=='м') %>% 
  filter(!is.na(UkrBall100) & !is.na(HistBall100) & !is.na(EngBall100)) %>% 
  mutate(result=ifelse(UkrBall100 >= 180 & HistBall100 >= 180 & EngBall100 >=180, 'win', 'fail')) %>% 
  group_by(oblast_or_city, result) %>% 
  summarise(n=n()) %>% 
  group_by(oblast_or_city) %>% 
  summarise(win=sum(ifelse(result=='win', n, 0)), all=sum(n)) %>% 
  ungroup() %>% 
  mutate(ratio=win/all * 100) %>% 
  arrange(desc(ratio))

ume <- df1 %>% 
  filter(city_type=='м') %>% 
  filter(!is.na(UkrBall100) & !is.na(MathBall100) & !is.na(EngBall100)) %>% 
  mutate(result=ifelse(UkrBall100 >= 180 & MathBall100 >= 180 & EngBall100 >=180, 'win', 'fail')) %>% 
  group_by(oblast_or_city, result) %>% 
  summarise(n=n()) %>% 
  group_by(oblast_or_city) %>% 
  summarise(win=sum(ifelse(result=='win', n, 0)), all=sum(n)) %>% 
  ungroup() %>% 
  mutate(ratio=win/all * 100) %>% 
  arrange(desc(ratio))

ump <- df1 %>% 
  filter(city_type=='м') %>% 
  filter(!is.na(UkrBall100) & !is.na(MathBall100) & !is.na(PhysBall100)) %>% 
  mutate(result=ifelse(UkrBall100 >= 180 & MathBall100 >= 180 & PhysBall100 >=180, 'win', 'fail')) %>% 
  group_by(oblast_or_city, result) %>% 
  summarise(n=n()) %>% 
  group_by(oblast_or_city) %>% 
  summarise(win=sum(ifelse(result=='win', n, 0)), all=sum(n)) %>% 
  ungroup() %>% 
  mutate(ratio=win/all * 100) %>% 
  arrange(desc(ratio))

uhe %>% write.csv('uhe.csv', row.names=FALSE)
ume %>% write.csv('ume.csv', row.names=FALSE)
ump %>% write.csv('ump.csv', row.names=FALSE)

#df1 %>% write.csv('zno_2016_oblast_or_city.csv', row.names=FALSE)   
