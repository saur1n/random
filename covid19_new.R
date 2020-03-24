##### COVID19 TRACKING
##### Author  : Saurin Parikh (dr.saurin.parikh@gmail.com)
##### Date    : 03/23/2020

##### INITIALIZE
library(ggplot2)
library(scales)
library(stringr)
library(stringi)
library(reshape2)
out_path = 'figs/';
dat.dir <- "/home/sbp29/R/Projects/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/"

##### FIGURE SIZE
one.c <- 90 #single column
one.5c <- 140 #1.5 column
two.c <- 190 #full width

##### TEXT SIZE
titles <- 7
txt <- 5
lbls <- 9


#####
dat.files <- list.files(path = dat.dir,pattern = ".csv", recursive = TRUE)

data <- NULL
temp.dat <- NULL
for (file in dat.files) {
  temp <- read.csv(paste0(dat.dir, file), na.strings = "NaN")
  temp.dat$Date <- as.Date(str_remove(file, '.csv'),  tryFormats = c("%m-%d-%Y"))
  temp.dat$Province.State <- temp[,stringr::str_detect(colnames(temp), 'vince')]
  temp.dat$Country.Region <- temp[,stringr::str_detect(colnames(temp), 'ountr')]
  temp.dat$Confirmed <- temp[,stringr::str_detect(colnames(temp), 'onfirm')]
  temp.dat$Death <- temp[,stringr::str_detect(colnames(temp), 'eath')]
  temp.dat$Recovered <- temp[,stringr::str_detect(colnames(temp), 'ecover')]
  data <- rbind(data,data.frame(temp.dat))
}

data2 <- melt(data, id.vars = c('Date','Country.Region','Province.State'))

data2 <- data2[data2$Country.Region %in% c('Canada','Finland','Germany','India','Italy','Lithuania','Nepal','Portugal','Spain','Turkey','US'),]
data2$Country.Region <- factor(data2$Country.Region, levels = c('Canada','Finland','Germany','India','Italy','Lithuania','Nepal','Portugal','Spain','Turkey','US'))

data2$Date <- as.character(data2$Date)
data2$value <- as.numeric(data2$value)
# data2$variable <- as.character(data2$variable)
# data2$Country.Region <- as.character(data2$Country.Region)

covid <- NULL
for (d in unique(data2$Date)) {
  for (v in unique(data2$variable[data2$Date == d])) {
    for (c in unique(data2$Country.Region[data2$Date == d & data2$variable == v])) {
      if (c == 'US') {
        covid <- rbind(covid, c(Date = d, Country = c, Status = v, Count = sum(data2$value[data2$Date == d & 
                                                                             data2$variable == v &
                                                                             data2$Country.Region == c])))
        covid <- rbind(covid, c(Date = d, Country = 'US-Pennsylvania', Status = v, Count = sum(data2$value[data2$Date == d & 
                                                                                                             data2$variable == v &
                                                                                                             data2$Country.Region == c &
                                                                                                             data2$Province.State == 'Pennsylvania'])))
        covid <- rbind(covid, c(Date = d, Country = 'US-California', Status = v, Count = sum(data2$value[data2$Date == d & 
                                                                                                           data2$variable == v &
                                                                                                           data2$Country.Region == c &
                                                                                                           data2$Province.State == 'California'])))
      } else {
        covid <- rbind(covid, c(Date = d, Country = c, Status = v, Count = sum(data2$value[data2$Date == d & 
                                                                             data2$variable == v &
                                                                             data2$Country.Region == c])))
      }
    }
  }
}
covid <- data.frame(covid)
covid$Count <- as.numeric(as.character(covid$Count))
covid$Count[is.na(covid$Count)] <- 0
covid$Date <- as.Date(covid$Date)


##### EKLAVYA
ggplot(covid[covid$Country %in% c('Canada','Finland','Germany','India','Lithuania','US','US-California','US-Pennsylvania'),]) +
  geom_line(aes(x = Date, y = Count, col = Status),
            lwd = 1.2) +
  facet_wrap(.~Country, scale = 'free_y') +
  # scale_y_log10() +
  labs(title = 'COVID-19 in Eklavya Countries/Regions',
       y = 'Count') +
  theme_linedraw() +
  theme(axis.title = element_text(size = titles),
        axis.text = element_text(size = txt),
        legend.title = element_text(size = titles),
        legend.text = element_text(size = txt),
        strip.text = element_text(size = txt),
        legend.key.size = unit(5, "mm"))
ggsave(sprintf("%s%s_covid_eklavya.jpg",out_path,format(Sys.time(), "%Y%b%d")),
       height = two.c, width = two.c, units = 'mm',
       dpi = 600) 


##### FAB12
ggplot(covid[covid$Country %in% c('Finland','Italy','India','Nepal','Portugal','Spain','Turkey','US','US-Pennsylvania'),]) +
  geom_line(aes(x = Date, y = Count, col = Status),
            lwd = 1.2) +
  facet_wrap(.~Country, scale = 'free_y') +
  # scale_y_log10() +
  labs(title = 'COVID-19 in Fab12 Countries/Regions',
       y = 'Count') +
  theme_linedraw() +
  theme(axis.title = element_text(size = titles),
        axis.text = element_text(size = txt),
        legend.title = element_text(size = titles),
        legend.text = element_text(size = txt),
        strip.text = element_text(size = txt),
        legend.key.size = unit(5, "mm"))
ggsave(sprintf("%s%s_covid_fab12.jpg",out_path,format(Sys.time(), "%Y%b%d")),
       height = two.c, width = two.c, units = 'mm',
       dpi = 600) 

##### PARIKH-GAJJAR
ggplot(covid[covid$Country %in% c('India','US','US-California','US-Pennsylvania'),]) +
  geom_line(aes(x = Date, y = Count, col = Status),
            lwd = 1.2) +
  facet_wrap(.~Country, scale = 'free_y') +
  # scale_y_log10() +
  labs(title = 'COVID-19 in India & US',
       y = 'Count') +
  theme_linedraw() +
  theme(axis.title = element_text(size = titles),
        axis.text = element_text(size = txt),
        legend.title = element_text(size = titles),
        legend.text = element_text(size = txt),
        strip.text = element_text(size = txt),
        legend.key.size = unit(5, "mm"))
ggsave(sprintf("%s%s_covid_india_us.jpg",out_path,format(Sys.time(), "%Y%b%d")),
       height = two.c*2/3, width = two.c*2/3, units = 'mm',
       dpi = 600) 

##### INDIA
ggplot(covid[covid$Country %in% c('India'),]) +
  geom_line(aes(x = Date, y = Count, col = Status),
            lwd = 1.2) +
  facet_wrap(.~Country, scale = 'free_y') +
  # scale_y_log10() +
  labs(title = 'COVID-19 in India',
       y = 'Count') +
  theme_linedraw() +
  theme(axis.title = element_text(size = titles),
        axis.text = element_text(size = txt),
        legend.title = element_text(size = titles),
        legend.text = element_text(size = txt),
        strip.text = element_text(size = txt),
        legend.key.size = unit(5, "mm"))
ggsave(sprintf("%s%s_covid_india.jpg",out_path,format(Sys.time(), "%Y%b%d")),
       height = one.c, width = one.c, units = 'mm',
       dpi = 600) 

##### PENNSYLVANIA
ggplot(covid[covid$Country %in% c('US-Pennsylvania'),]) +
  geom_line(aes(x = Date, y = Count, col = Status),
            lwd = 1.2) +
  facet_wrap(.~Country, scale = 'free_y') +
  # scale_y_log10() +
  labs(title = 'COVID-19 in Pennsylvania',
       y = 'Count') +
  theme_linedraw() +
  theme(axis.title = element_text(size = titles),
        axis.text = element_text(size = txt),
        legend.title = element_text(size = titles),
        legend.text = element_text(size = txt),
        strip.text = element_text(size = txt),
        legend.key.size = unit(5, "mm"))
ggsave(sprintf("%s%s_covid_pa.jpg",out_path,format(Sys.time(), "%Y%b%d")),
       height = one.c, width = one.c, units = 'mm',
       dpi = 600) 

