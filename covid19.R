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

##### FIGURE SIZE
one.c <- 90 #single column
one.5c <- 140 #1.5 column
two.c <- 190 #full width

##### TEXT SIZE
titles <- 7
txt <- 5
lbls <- 9

us.covid <- data.frame(read.csv("https://covidtracking.com/api/states/daily.csv"))

top10 <- head(us.covid$state[order(us.covid$total[us.covid == 20200322], decreasing = T)], n = 10)

ggplot(us.covid) +
  geom_line(aes(x=date, y=positive, col = state))


us.covid2 <- us.covid[,c('state','date','positive','negative','pending','hospitalized','death')]
us.covid2 <- melt(us.covid2, id.vars = c('state','date'))

ggplot(us.covid2[us.covid2$date == '20200322',]) +
  geom_bar(aes(x = state, y = value, fill = variable),
                 stat = 'identity', position = "stack")


#####
covid <- data.frame(read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"))
covid$status <- 'Confirmed'
death <- data.frame(read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"))
death$status <- 'Deaths'
recov <- data.frame(read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"))
recov$status <- 'Recovered'

covid <- rbind(covid, recov, death)

temp <- covid[covid$Country.Region %in% c('Finland','Italy','India','Nepal','Portugal','Spain','Turkey','US'),]
temp$Country.Region <- factor(temp$Country.Region)

fab12 <- NULL
for (s in unique(temp$status)) {
  for (c in unique(temp$Country.Region)) {
    if (c == 'US') {
      fab12 <- rbind(fab12, c(Country = c, Status = s, colSums(temp[temp$status == s & 
                                                          temp$Country.Region == c, 5:(dim(temp)[2]-1)])))
      fab12 <- rbind(fab12, c(Country = 'US-PA', Status = s, colSums(temp[temp$status == s & 
                                                                temp$Country.Region == c &
                                                                temp$Province.State == 'Pennsylvania',5:(dim(temp)[2]-1)])))
    } else {
      fab12 <- rbind(fab12, c(Country = c, Status = s, colSums(temp[temp$status == s & 
                                                          temp$Country.Region == c,5:(dim(temp)[2]-1)])))
    }
  }
}

fab12 <- data.frame(fab12)
fab12$Country <- factor(fab12$Country, levels = c('Finland','Italy','India','Nepal','Portugal','Spain','Turkey','US','US-PA'))

fab12 <- melt(fab12, id.vars = c('Country','Status'))
fab12$variable <- as.character(fab12$variable)
fab12$variable <- str_remove(fab12$variable, 'X')
fab12$Date <- str_replace_all(fab12$variable, "[[:punct:]]", "/")
fab12$Date <- as.Date(fab12$Date, "%m/%d/%y")
fab12$value <- as.integer(fab12$value)

ggplot(fab12) +
  geom_line(aes(x = Date, y = value, col = Status),
            lwd = 1.2) +
  facet_wrap(.~Country, scale = 'free_y') +
  # scale_y_log10() +
  labs(title = 'COVID-19 in Fab12 Countries',
       y = 'Count') +
  theme_linedraw() +
  theme(axis.title = element_text(size = titles),
        axis.text = element_text(size = txt),
        legend.title = element_text(size = titles),
        legend.text = element_text(size = txt),
        strip.text = element_text(size = txt),
        legend.key.size = unit(5, "mm"))
ggsave(sprintf("%s%s_covid.jpg",out_path,format(Sys.time(), "%Y%b%d")),
       height = two.c, width = two.c, units = 'mm',
       dpi = 600)  


