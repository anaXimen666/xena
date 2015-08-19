library("RGA")
library("stringr")
library(plyr)
library("dplyr")
library(ggplot2)
library(RColorBrewer)


ga_token <- authorize(client.id = "44510395713-cngu0lhjk5rud2o8s3noj6qj17f7obng.apps.googleusercontent.com", client.secret = "FHsO_3tCeb4FgFXjw-GPbCTW")

# выбор профиля, загрузка целей и метаданных

profiles<-list_profiles()
profiles[str_detect(profiles$website.url, "ncc"),] # собственно выбор профиля
goals <- list_goals(account.id = "42404090", webproperty.id = "UA-42404090-1", profile.id = "74463920")
md <- list_metadata()

# параметры по умолчанию

ID <- "19063151"
dateStart <- "2014-06-01"
dateEnd <- "2015-07-31"
allMetrics <- "ga:visits, ga:bounces, ga:pageviews, ga:newUsers, ga:goal4Completions, ga:goal18Completions"

#выгрузка

#основные данные
timeData <- get_ga(ID, 
                   start.date=dateStart, 
                   end.date=dateEnd, 
                   metrics = allMetrics, 
                   dimensions = "ga:date, ga:sourceMedium, ga:medium, ga:source")

timeData <- rbind( timeData, data.frame("date"="2015-05-11", 
                                        "source.medium"=" wi-fi_pulkovo / banner",
                                        "medium"=" banner",
                                        "source"=" wi-fi_pulkovo",
                                        "visits"= 2,
                                        "bounces"= 1,
                                        "pageviews"= 1,
                                        "new.users"= 1,
                                        "goal4Completions"= 1,
                                        "goal18Completions"= 1
))


timeData$Channels <- ifelse(timeData$source.medium == "(direct) / (none)","Прямые переходы",
                            ifelse(timeData$medium == "organic","Органический поиск",
                                   ifelse(timeData$medium == "referral","Переходы по ссылкам",
                                          ifelse(str_detect(timeData$source.medium, "wi-fi"),"wi-fi",
                                          ifelse(str_detect(timeData$source.medium, "email|print"),"Почта",
                                                 ifelse(str_detect(timeData$source.medium, "yandex|google|adwords|direct"),"Контекстная реклама",
                                                               ifelse(str_detect(timeData$source.medium, "vkontakte|facebook|fb|vkontate|vk"),"Социальные сети / cpc" ,"Другая баннерная реклама"
                                                               )))))))

timeData$months <- str_match(timeData$date,".+?\\-\\d{2}")



ggdata <- timeData %>%
  group_by(months, Channels) %>%
  summarise(visits=sum(visits))

ggplot(ggdata, aes( months, visits, group=Channels,colour = Channels, fill= Channels)) + 
  geom_area() + 
  theme_bw() + 
  ggtitle("Весь трафик с разбивкой по каналам")+
  xlab("Месяцы") +
  ylab("Посетители") +
  scale_fill_manual(values=brewer.pal(8,"Set3"))+
  scale_colour_manual(values=brewer.pal(8,"Set3"))


ggdata <- timeData %>%
  group_by(months, Channels) %>%
  summarise(visits=sum(visits)-sum(bounces))

ggplot(ggdata, aes( months, visits, group=Channels,colour = Channels, fill= Channels)) + 
  geom_area() + 
  theme_bw() + 
  ggtitle("Посещения без отказов с разбивкой по каналам")+
  xlab("Месяцы") +
  ylab("Посетители") +
  scale_fill_manual(values=brewer.pal(8,"Set3"))+
  scale_colour_manual(values=brewer.pal(8,"Set3"))


ggdata <- timeData %>%
  group_by(months, Channels) %>%
  summarise(goal4Completions=sum(goal4Completions))

ggplot(ggdata, aes( months, goal4Completions, group=Channels,colour = Channels, fill= Channels)) + 
  geom_area() + 
  theme_bw() + 
  ggtitle("Все уникальные отправки форм с разбивкой по каналам")+
  xlab("Месяцы") +
  ylab("Отправки форм") +
  scale_fill_manual(values=brewer.pal(8,"Set3"))+
  scale_colour_manual(values=brewer.pal(8,"Set3"))

ggdata <- timeData %>%
  group_by(months, Channels) %>%
  summarise(goal18Completions=sum(goal18Completions))

ggplot(ggdata, aes( months, goal18Completions, group=Channels,colour = Channels, fill= Channels)) + 
  geom_area() + 
  theme_bw() + 
  ggtitle("Все уникальные отправки форм с разбивкой по каналам")+
  xlab("Месяцы") +
  ylab("Отправки форм") +
  scale_fill_manual(values=brewer.pal(8,"Set3"))+
  scale_colour_manual(values=brewer.pal(8,"Set3"))



ggdata <- timeData %>%
  group_by(months, Channels) %>%
  summarise(conversion=sum(goal4Completions) / sum(visits))

ggplot(ggdata, aes(months, conversion*100, group = Channels,colour = Channels)) + 
  geom_line(size=2) + 
  theme_bw() + 
  ggtitle("Конверсия с разбивкой по каналам")+
  xlab("Месяцы") +
  ylab("Конверсия, %") +
  scale_fill_manual(values=brewer.pal(8,"Set3"))+
  scale_colour_manual(values=brewer.pal(8,"Set3"))



ggdata <- timeData %>%
  group_by(months) %>%
  summarise(conversion=sum(goal4Completions) / sum(visits))

ggplot(ggdata, aes(months, conversion*100, group = 1)) + 
  geom_line(size=2,colour="#9CA1E6") +   
  theme_bw() + 
  ggtitle("Конверсия по всем каналам")+
  xlab("Месяцы") +
  ylab("Конверсия, %") 

ggdata <- data.frame(table(timeData$months))
ggdata$Freq <- c(1572,1271,1898,2289,1663, 1760)

ggplot(ggdata, aes(Var1, Freq, group = 1)) + 
  geom_area(fill = "#9CA1E6") +   
  theme_bw() + 
  ggtitle("Рекламный бюджет")+
  xlab("Месяцы") +
  ylab("Затраты, тыс. руб.") 

library("psych")

ggdata <- timeData %>%
  group_by(months) %>%
  summarise(conversion=sum(goal4Completions) / sum(visits),
            goal4Completions=sum(goal4Completions),
            visits=sum(visits))
ggdata$money <- c(1572,1271,1898,2289,1663)

rownames(ggdata) <- ggdata$months
ggdata <- ggdata[,-1]
pairs.panels(ggdata,
             gap =0)

######################CAMPAIGNS
timeData <- get_ga(ID, 
                   start.date=dateStart, 
                   end.date=dateEnd, 
                   metrics = allMetrics, 
                   dimensions = "ga:date, ga:sourceMedium, ga:medium, ga:source, ga:campaign",
                   filters = "ga:source=~^yandex|google|gmail|mk;ga:medium=~cpc|display|banner")

timeData$Channels <- ifelse(str_detect(timeData$source, ignore.case("rsy")),"yandex / РСЯ",
                            ifelse(str_detect(timeData$campaign, ignore.case("gmail")) | str_detect(timeData$source, ignore.case("gmail")),"google / gmail",  
                                   ifelse(str_detect(timeData$source, ignore.case("yandex")) ,"yandex / Поиск",  
  ifelse((str_detect(timeData$campaign, ignore.case("ретарге|ремаркети|retarg|remarark|мкб|mkb"))|str_detect(timeData$source, ignore.case("ретарге|ремаркети|retarg|remarark|мкб|mkb")),"google / МКБ","google / gmail"
                                                             ))))
timeData <- timeData[timeData$source!="googleMKB",]
timeData$months <- str_match(timeData$date,".+?\\-\\d{2}")

ggdata <- timeData %>%
  group_by(months, Channels) %>%
  summarise(visits=sum(visits))

ggplot(ggdata, aes( months, visits, group=Channels,colour = Channels, fill= Channels)) + 
  geom_area() + 
  theme_bw() + 
  ggtitle("Весь трафик с разбивкой по каналам")+
  xlab("Месяцы") +
  ylab("Посетители") +
  scale_fill_manual(values=brewer.pal(8,"Set3"))+
  scale_colour_manual(values=brewer.pal(8,"Set3"))



ggdata <- timeData %>%
  group_by(months, Channels) %>%
  summarise(visits=sum(visits))



ggdata <- timeData %>%
  group_by(months, Channels) %>%
  summarise(goal4Completions=sum(goal4Completions))

ggplot(ggdata, aes( months, goal4Completions, group=Channels,colour = Channels, fill= Channels)) + 
  geom_area() + 
  theme_bw() + 
  ggtitle("Все уникальные отправки форм с разбивкой по каналам")+
  xlab("Месяцы") +
  ylab("Отправки форм") +
  scale_fill_manual(values=brewer.pal(8,"Set3"))+
  scale_colour_manual(values=brewer.pal(8,"Set3"))

ggdata <- timeData %>%
  group_by(months, Channels) %>%
  summarise(conversion=sum(goal4Completions) / sum(visits))

ggplot(ggdata, aes(months, conversion*100, group = Channels,colour = Channels)) + 
  geom_line(size=2) + 
  theme_bw() + 
  ggtitle("Конверсия с разбивкой по каналам")+
  xlab("Месяцы") +
  ylab("Конверсия, %") +
  scale_fill_manual(values=brewer.pal(8,"Set3"))+
  scale_colour_manual(values=brewer.pal(8,"Set3"))



######################seaarch
timeData <- get_ga(ID, 
                   start.date=dateStart, 
                   end.date=dateEnd, 
                   metrics = allMetrics, 
                   dimensions = "ga:date, ga:sourceMedium, ga:medium, ga:source, ga:campaign",
                   filters = "ga:source=~google;ga:medium==cpc;ga:campaign!~ретарге|ремаркети|retarg|remarark|мкб|mkb")

timeData$months <- str_match(timeData$date,".+?\\-\\d{2}")
timeData<- timeData[str_detect(timeData$campaign,"(Gmail\\s\\-\\sKL\\s\\-\\sСПб|Gmail\\s\\-\\sSK\\s\\-\\sСПб|Gmail\\s\\-\\sКазахстан|Gmail\\s\\-\\sРФ\\sбез\\sСПб\\sи\\sЛО)"),]

ggdata <- timeData %>%
  group_by(months, campaign) %>%
  summarise(goal4Completions=sum(goal4Completions))

ggplot(ggdata, aes( months, goal4Completions, group=campaign,colour = campaign, fill= campaign)) + 
  geom_area() + 
  theme_bw() + 
  ggtitle("Все уникальные отправки форм с разбивкой по кампаниям")+
  xlab("Месяцы") +
  ylab("Отправки форм") +
  scale_fill_manual(values=brewer.pal(8,"Set3"))+
  scale_colour_manual(values=brewer.pal(8,"Set3"))


ggdata <- timeData %>%
  group_by(months, campaign) %>%
  summarise(conversion=sum(goal4Completions) / sum(visits))

ggplot(ggdata, aes(months, conversion*100, group = campaign,colour = campaign)) + 
  geom_line(size=2) + 
  theme_bw() + 
  ggtitle("Конверсия с разбивкой по каналам")+
  xlab("Месяцы") +
  ylab("Конверсия, %") +
  scale_fill_manual(values=brewer.pal(8,"Set3"))+
  scale_colour_manual(values=brewer.pal(8,"Set3"))


ggdata <- timeData %>%
  group_by(months, campaign) %>%
  summarise(visits=sum(visits))

ggplot(ggdata, aes(months, visits, group = campaign,colour = campaign)) + 
  geom_line(size=2) + 
  theme_bw() + 
  ggtitle("Конверсия с разбивкой по каналам")+
  xlab("Месяцы") +
  ylab("Конверсия, %") +
  scale_fill_manual(values=brewer.pal(8,"Set3"))+
  scale_colour_manual(values=brewer.pal(8,"Set3"))

######################seaarch
timeData <- get_ga(ID, 
                   start.date=dateStart, 
                   end.date=dateEnd, 
                   metrics = allMetrics, 
                   dimensions = "ga:sourceMedium, ga:medium, ga:source, ga:campaign",
                   filters = "ga:medium=~cpc|banner|display")
timeData$conv <- timeData$goal4Completions *100/ timeData$visits
timeData <- timeData[order(-timeData$conv),]
timeData <- timeData[timeData$goal4Completions>5,]
######################

######################seaarch

dateStart <- "2015-01-01"
dateEnd <- "2015-05-31"
timeData <- get_ga(ID, 
                   start.date="2015-05-01", 
                   end.date="2015-05-31", 
                   metrics = allMetrics, 
                   dimensions = "ga:sourceMedium, ga:medium, ga:source, ga:landingPagePath",
                   filters = "ga:source=~google;ga:medium==cpc;ga:campaign=~Gmail")

timeData$months <- str_match(timeData$date,".+?\\-\\d{2}")

ggdata <- timeData %>%
  group_by(months, source, campaign) %>%
  summarise(goal4Completions=sum(goal4Completions))
ggdata$Источник <- paste(ggdata$source,ggdata$campaign, sep = " / ")
ggplot(ggdata, aes( months, goal4Completions, group=Источник,colour = Источник, fill= Источник)) + 
  geom_area() + 
  theme_bw() + 
  ggtitle("Все уникальные отправки форм с разбивкой по кампаниям")+
  xlab("Месяцы") +
  ylab("Отправки форм") +
  scale_fill_manual(values=brewer.pal(8,"Set3"))+
  scale_colour_manual(values=brewer.pal(8,"Set3"))


ggdata <- timeData %>%
  group_by(months, campaign) %>%
  summarise(conversion=sum(goal4Completions) / sum(visits))

ggplot(ggdata, aes(months, conversion*100, group = campaign,colour = campaign)) + 
  geom_line(size=2) + 
  theme_bw() + 
  ggtitle("Конверсия с разбивкой по каналам")+
  xlab("Месяцы") +
  ylab("Конверсия, %") +
  scale_fill_manual(values=brewer.pal(8,"Set3"))+
  scale_colour_manual(values=brewer.pal(8,"Set3"))


ggdata <- timeData %>%
  group_by(months, campaign) %>%
  summarise(visits=sum(visits))

ggplot(ggdata, aes(months, visits, group = campaign,colour = campaign)) + 
  geom_line(size=2) + 
  theme_bw() + 
  ggtitle("Конверсия с разбивкой по каналам")+
  xlab("Месяцы") +
  ylab("Конверсия, %") +
  scale_fill_manual(values=brewer.pal(8,"Set3"))+
  scale_colour_manual(values=brewer.pal(8,"Set3"))
##################
ggdata <- timeData %>%
  group_by(date) %>%
  summarise(visits=sum(visits))

ggplot(ggdata, aes( date, visits)) + 
  geom_line()

monthdata <- get_ga(ID, 
                    start.date="2011-02-01", 
                    end.date="2015-05-20", 
                    metrics = allMetrics, 
                    dimensions = "ga:date")

monthdata$date <- str_match(monthdata$date,".+?\\-\\d{2}")

ggdata <- monthdata %>%
  group_by(date) %>%
  summarise(visits=sum(visits))

ggplot(ggdata, aes(date, visits,group = 1)) + 
  geom_line()

tss<-ts(ggdata$visits,start=c(2011,2),frequency=12)
tsss<-stl(tss, "per")
plot(tsss)
plot(tsss$time.series[1:12, "seasonal"], type = "b")

fit <- arima(tss,order=c(2,1,0),seasonal=list(order=c(2,1,0),period=12))
fore <- predict(fit,n.ahead=6)
U <- fore$pred+2*fore$se
L <- fore$pred-2*fore$se

ts.plot(tss,fore$pred, U, L,col=c(1,2,4),lty=c(1,1,2,2))

daydata <- get_ga(ID, 
                  start.date="2014-01-01", 
                  end.date="2015-04-30", 
                  metrics = "ga:goal4ConversionRate", 
                  dimensions = "ga:date,ga:sourceMedium")

ggplot(daydata, aes(date, visits,group = 1)) + 
  geom_line()

tss<-ts(daydata$visits,start=c(2015,1,1),frequency=7)
tsss<-stl(tss, "per")
plot(tsss)
plot(tsss$time.series[1:7, "seasonal"], type = "b")

fit <- arima(tss,order=c(2,1,0),seasonal=list(order=c(2,1,0),period=7))
fore <- predict(fit,n.ahead=7)
U <- fore$pred+2*fore$se
L <- fore$pred-2*fore$se

ts.plot(tss,fore$pred, U, L,col=c(1,2,4),lty=c(1,1,2,2))



#######


timeData <- get_ga(ID, 
                   start.date=dateStart, 
                   end.date=dateEnd, 
                   metrics = allMetrics, 
                   dimensions = "ga:date, ga:userAgeBracket")


timeData$months <- str_match(timeData$date,".+?\\-\\d{2}")



ggdata <- timeData %>%
  group_by(months, user.age.bracket) %>%
  summarise(visits=sum(visits))

ggplot(ggdata) + 
  geom_area(aes( months, visits, group=user.age.bracket,colour = user.age.bracket, fill= user.age.bracket)) + 
  theme_bw() + 
  ggtitle("Весь трафик с разбивкой по возрасту")+
  xlab("Месяцы") +
  ylab("Посетители") +
  scale_fill_manual(values=brewer.pal(8,"Set3"))+
  scale_colour_manual(values=brewer.pal(8,"Set3"))


