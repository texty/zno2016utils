library(ggplot2)

df1 <- read.csv('../zno_2016_final.csv') 

df2 <- df1 %>%  
  mutate(city_type=ifelse(city_type=='с-ще', 'с', city_type))

# plot(df2 %>% filter(city_type=='м' | city_type=='с'| city_type=='с-ще'| city_type=='смт'),aes(MathBall100, fill=city_type, colour=city_type)) +
#   geom_density(alpha=0.2, adjust=2, kernel = "rectangular") +
#   facet_wrap(~ city_type)

ggplot(df2 %>% filter(city_type=='м' | city_type=='с'| city_type=='с-ще'| city_type=='смт'),aes(UkrBall100, fill=city_type, colour=city_type)) +
  geom_freqpoly(binwidth = 10) +
  xlim(100, 200)  +
  theme_bw()+
  xlab("Бали ЗНО з української\n")+
  ylab("Кількість осіб\n")+
  ggtitle("В містах здають краще\n")+
  guides(color=guide_legend(title="населений пункт"))+
  theme(#basic elements
    line = element_line(color="#EFF2F4"),
    rect = element_rect(color="#CCD0D7"),
    text = element_text(size=12, family="Open Sans Light", colour = "#5D646F"),
    plot.title = element_text(size=22, family="Open Sans", face="bold", colour = "#3A3F4A", hjust=-0.125),
    #panel
    panel.border=element_blank(),
    panel.background = element_rect(fill = "#EFF2F4"),
    plot.background = element_rect(fill = "#EFF2F4"),
    #axes
    axis.title=element_text(size=12, family="Open Sans Light", colour = "#5D646F"),
    axis.ticks=element_blank(),
    axis.text=element_text(size=10, family="Open Sans Light", colour = "#5D646F"),
    #grid
    panel.grid.major = element_line(colour = "#CCD0D7", size=.25),
    panel.grid.minor = element_blank(),
    #legend
    legend.margin = unit(0, "points"),
    legend.key.height=unit(5, "points"),
    legend.position = c(0.215, 1.05),
    legend.direction = "horizontal", 
    legend.box = "vertical",
    legend.box.just = "left",
    legend.key = element_blank(),
    legend.background = element_rect(fill = "#EFF2F4"),
    legend.text = element_text(family="Open Sans Light", colour = "#3A3F4A", hjust=0),
    legend.title.align = 0,
    legend.text.align = 0,
    plot.margin = unit(c(60, 45, 60, 45), "points"))

# +  facet_wrap(~ city_type)

