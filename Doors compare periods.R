# авторизация и подгрузка библиотек

library("RGA")
library("stringr")
library(plyr)
library("dplyr")
library(httr)

ga_token <- authorize(client.id = "44510395713-cngu0lhjk5rud2o8s3noj6qj17f7obng.apps.googleusercontent.com", client.secret = "FHsO_3tCeb4FgFXjw-GPbCTW")
token <- content(GET("https://api.adhands.ru/getToken/?login=glushkov@realweb.ru&applicationId=13&grantType=password&password=uyf0p2hb&callback=docs.google.com"), "parsed",  "application/json")[1]




#выгрузка

#основные данные

Geo <- read.csv("GeoDoors.csv", stringsAsFactors=F)
# параметры по умолчанию

ID <- "48921571"
clientID <- "3722"

dataLoad <- function(dateStart,dateEnd){
  
googleData <- get_ga(ID, 
                     start.date=dateStart, 
                     end.date=dateEnd, 
                     metrics = "ga:adCost,ga:visits,ga:goal13Completions", 
                     dimensions = "ga:campaign,ga:source", 
                     filters="ga:medium=~cpc;ga:source==google;ga:adwordsCustomerID=~6174444461|3105565936")[,-4]


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

yandexAnalyticsData <- get_ga(ID, 
                     start.date=dateStart, 
                     end.date=dateEnd, 
                     metrics = "ga:visits,ga:goal13Completions", 
                     dimensions = "ga:campaign,ga:source", 
                     filters="ga:medium=~cpc;ga:source==yandex")[,-c(2,3)]

yandexAnalyticsData$campaign <- sub("(\\d{8}).+","\\1",yandexAnalyticsData$campaign)

yandexData <- merge(mergedcontextdf,yandexAnalyticsData, by.x = "remoteId", by.y = "campaign") 
yandexData <- yandexData[,c(3,4,8,9)]
colnames(yandexData) <- c("campaign","source","ad.cost","goal13Completions")
sumData <- rbind(googleData, yandexData)
sumData <- sumData[sumData$ad.cost>0,]
return(sumData)
}
######################

firstPeriod <- dataLoad("2015-07-01","2015-07-09")
secondPeriod <- dataLoad("2015-08-01","2015-08-09")

##################### КООООЛЛЛЛЛЯЯЯЯ!!!!!!!!!!!!!!!!

superSum <- merge(firstPeriod,secondPeriod, by = c("campaign", "source"), all=T)


############################


sumMatrix <- superSum
sumMatrix[is.na(sumMatrix)] <- 0

sumMatrix$campaign<-str_replace(sumMatrix$campaign, "архив ","")

sumMatrix <- sumMatrix %>%
  mutate(region=ifelse(str_detect(campaign,"овгород"),"нижний_новгород" ,ifelse(str_detect(campaign,"челн"),"набережные_челны", tolower(str_match(campaign,"(^(нижн|набереж)|^[- а-яА-Я]+)")))),
         type=ifelse(str_detect(campaign,ignore.case("кухн")),"кухни","остальное"),
         ad.cost.x=as.numeric(as.character(ad.cost.x)),        
         ad.cost.y=as.numeric(as.character(ad.cost.y)),
         franchise = ifelse(region %in% Geo$region[Geo$franchise == "франшиза"],"франшиза", "не франшиза")) %>%
  group_by(region,franchise,type, source, campaign) %>%
  summarize(firstCost = sum(ad.cost.x),
            secondCost = sum(ad.cost.y),
            firstGoal = sum(goal13Completions.x),
            secondGoal = sum(goal13Completions.y))



GeoVector <- c(Geo$region, "сибирь", "поволжье", "юг")
ifelse(sumMatrix$region %in% GeoVector,1, sumMatrix$region)

#khm <- sumMatrix[!sumMatrix$region %in% GeoVector,]
#khm$region <- tolower(str_replace(khm$campaign, ".+?_(.+?)$", "\\1"))
#sumMatrix1 <- rbind(sumMatrix[sumMatrix$region %in% GeoVector,], khm)


sumMatrix$franchise[sumMatrix$region %in% c("сибирь", "поволжье", "юг")] <- "old"

sumMatrix <- sumMatrix %>%
  group_by(region,franchise, type, source) %>%
  summarize("Затраты первый период" = sum(firstCost),
            "Затраты второй период" = sum(secondCost),
            "Формы первый период" = sum(firstGoal),
            "Формы второй период" = sum(secondGoal))


write.csv(sumMatrix, "Дорс периоды.csv", row.names=F)




