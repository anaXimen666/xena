# авторизация и подгрузка библиотек

library("RGA")
library("stringr")
library(plyr)
library("dplyr")

ga_token <- authorize(client.id = "44510395713-cngu0lhjk5rud2o8s3noj6qj17f7obng.apps.googleusercontent.com", client.secret = "FHsO_3tCeb4FgFXjw-GPbCTW")


# параметры по умолчанию

ID <- "48921571"
dateStart <- "2015-08-01"
monthStart <- "2015-08-01"
dateEnd <- "2015-08-09"

#выгрузка

#основные данные

Geo <- read.csv("GeoDoors.csv", stringsAsFactors=F)


campaigndata <- get_ga(ID, 
                       start.date=dateStart, 
                       end.date=dateEnd, 
                       metrics = "ga:visits, ga:bounces, ga:pageviews, ga:newUsers, ga:goal9Completions, ga:goal13Completions", 
                       dimensions = "ga:campaign, ga:sourceMedium, ga:medium, ga:source", 
                       sort = "-ga:visits", 
                       filters="ga:medium=~cpc")
library(httr)

clientID <- "3722"


token <- content(GET("https://api.adhands.ru/getToken/?login=glushkov@realweb.ru&applicationId=13&grantType=password&password=uyf0p2hb&callback=docs.google.com"), "parsed",  "application/json")[1]
LNXdateStart <- as.numeric(as.POSIXct(paste(dateStart,"0:00:00")))
LNXdateEnd <- as.numeric(as.POSIXct(paste(dateEnd,"23:59:59")))

# CONTEXT NAMES
test<-POST("https://api.adhands.ru/?requestType=json", 
           add_headers(Accept = "application/vnd.realweb.adhands-v2+json"),
           body = sprintf('
{
                          "method": "getCampaigns",
                          "applicationId": 13,
                          "login": "glushkov@realweb.ru",
                          "token": "%s",
                          "args":{
                          "clientId":[%s],
                          "periodStart":%s,
                          "periodEnd":%s
                          }
}
                          ', token, clientID, LNXdateStart, LNXdateEnd))
stop_for_status(test)
rowdata <- content(test, "parsed", "application/json")
for (i in 1:length(rowdata)) {rowdata[[i]]<-rowdata[[i]][c(1,3,4,5)]}
campaignNames <- do.call(rbind.data.frame, rowdata)

# CONTEXT DATA
test<-POST("https://api.adhands.ru/?requestType=json", 
           add_headers(Accept = "application/vnd.realweb.adhands-v2+json"),
           body = sprintf('
                                                    
{
                          "method": "getStats",
                          "applicationId": 13,
                          "login": "glushkov@realweb.ru",
                          "token": "%s",
                          "args":{
                          "type": "contextCv",
                          "clientId":[%s],
                          "timeStart":%s,
                          "timeEnd":%s,
                          "groupBy": [
                          "campaign"
                          ]
                          }
}
                          ', token, clientID, LNXdateStart, LNXdateEnd))
stop_for_status(test)
rowdata <- content(test, "parsed", "application/json")
contextdf <- do.call(rbind.data.frame, rowdata)
mergedcontextdf <- merge(campaignNames, contextdf, by.x = "id", by.y = "row.names")


# MERGING WITH ANALYTICS


campaigndata$campaign <- sub("(\\d{8}).+","\\1",campaigndata$campaign)
campaigndata <- campaigndata %>%
  group_by(campaign, source.medium, medium,source) %>%
  summarise_each(funs(sum),5:10)
mergdata<-merge(campaigndata, mergedcontextdf, by = "campaign", by.y = "remoteId", all.x = TRUE)
mergdata<-mergdata[!is.na(mergdata$engine),]
mergdata$campaign <- mergdata$name
ad.clicks <- as.integer(as.character(mergdata$clicks))
ad.cost <- as.numeric(as.character(mergdata$price))
mergdata <- mergdata[,-c(11:17)]
mergdata <- data.frame(mergdata, ad.clicks, ad.cost)
mergdata$campaign <- as.character(mergdata$campaign)
mergdata$ad.clicks <- as.integer(mergdata$ad.clicks)
mergdata$ad.cost <- as.numeric(mergdata$ad.cost)


# if google by names

googleData <- get_ga(ID, 
                       start.date=dateStart, 
                       end.date=dateEnd, 
                       metrics = "ga:visits, ga:bounces, ga:pageviews, ga:newUsers, ga:goal9Completions, ga:goal13Completions, ga:adClicks, ga:adCost", 
                       dimensions = "ga:campaign, ga:sourceMedium, ga:medium, ga:source", 
                       sort = "-ga:visits", 
                       filters="ga:medium=~cpc;ga:source==google;ga:adwordsCustomerID=~6174444461|3105565936")


mergdata <- rbind(mergdata, googleData)

mergdata<- mergdata[!is.na(mergdata$campaign),]



###### MONTH DATA

LNXdateStart <- as.numeric(as.POSIXct(paste(monthStart,"0:00:00")))

# CONTEXT NAMES
test<-POST("https://api.adhands.ru/?requestType=json", 
           add_headers(Accept = "application/vnd.realweb.adhands-v2+json"),
           body = sprintf('
{
                          "method": "getCampaigns",
                          "applicationId": 13,
                          "login": "glushkov@realweb.ru",
                          "token": "%s",
                          "args":{
                          "clientId":[%s],
                          "periodStart":%s,
                          "periodEnd":%s
                          }
}
                          ', token, clientID, LNXdateStart, LNXdateEnd))
stop_for_status(test)
rowdata <- content(test, "parsed", "application/json")
for (i in 1:length(rowdata)) {rowdata[[i]]<-rowdata[[i]][c(1,3,4,5)]}
campaignNames <- do.call(rbind.data.frame, rowdata)

# CONTEXT DATA
test<-POST("https://api.adhands.ru/?requestType=json", 
           add_headers(Accept = "application/vnd.realweb.adhands-v2+json"),
           body = sprintf('
                          
{
                          "method": "getStats",
                          "applicationId": 13,
                          "login": "glushkov@realweb.ru",
                          "token": "%s",
                          "args":{
                          "type": "contextCv",
                          "clientId":[%s],
                          "timeStart":%s,
                          "timeEnd":%s,
                          "groupBy": [
                          "campaign"
                          ]
                          }
}
                          ', token, clientID, LNXdateStart, LNXdateEnd))
stop_for_status(test)
rowdata <- content(test, "parsed", "application/json")
contextdf <- do.call(rbind.data.frame, rowdata)
mergedcontextdf <- merge(campaignNames, contextdf, by.x = "id", by.y = "row.names")[,c(2,3,8)]
mergedcontextdf <- mergedcontextdf[mergedcontextdf$engine=="yandex",]



# if google by names

googleData <- get_ga(ID, 
                     start.date=monthStart, 
                     end.date=dateEnd, 
                     metrics = "ga:adCost,ga:visits", 
                     dimensions = "ga:campaign,ga:source", 
                     filters="ga:medium=~cpc;ga:source==google;ga:adwordsCustomerID=~6174444461|3105565936")[,-4]

colnames(mergedcontextdf) <- c("campaign","source","ad.cost")
monthData <- rbind(googleData,mergedcontextdf)
colnames(monthData)[3] <- "monthcost"
mergdata <- merge(mergdata, monthData, by = c("campaign", "source"), all = T)
mergdata[is.na(mergdata)] <- 0
############################

sumMatrix <- mergdata

sumMatrix <- sumMatrix %>%
  mutate(region=ifelse(str_detect(campaign,"овгород"),"нижний_новгород" ,ifelse(str_detect(campaign,"челн"),"набережные_челны", tolower(str_match(campaign,"(^(нижн|набереж)|^[- а-яА-Я]+)")))),
         type=ifelse(str_detect(campaign,ignore.case("кухн")),"кухни","остальное"),
         franchise = ifelse(region %in% Geo$region[Geo$franchise == "франшиза"],"франшиза", "не франшиза"),
         clicks=as.numeric(as.character(ad.clicks)),
         monthcost=as.numeric(as.character(monthcost)),        
         price=as.numeric(as.character(ad.cost))) %>%
  group_by(region,franchise,type, source, campaign) %>%
  summarize("Затраты за неделю" = sum(price),
            "Затраты за месяц" = sum(monthcost),
            "Клики" = sum(clicks),
            "Отправки формы" = sum(goal13Completions),
            "Просмотр адресов" = sum(goal9Completions))

#test
ifelse(sumMatrix$region %in% Geo$region,1, sumMatrix$region)

sumMatrix <- sumMatrix[sumMatrix$region %in% Geo$region,]

sumMatrix <- sumMatrix[order(-sumMatrix$"Клики"),]

write.csv(sumMatrix, "Кампании Дорс.csv", row.names=F)


