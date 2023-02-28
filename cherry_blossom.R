install.packages("tidyverse")
install.packages("rnoaa")
library(tidyverse)
library(rnoaa)

###########################################################################################
#################################### WASHINGTON, DC #######################################
###########################################################################################

DCBloom <- read.csv("data/washingtondc.csv")
DCBloom.df <- data.frame(DCBloom)
 
DCBloom.df <- DCBloom.df %>% select(-c(lat, long, alt))
DCBloom.df$date <- as.Date(DCBloom.df$bloom_date, format="%Y-%m-%d")
 
   
tempDC <- ghcnd_search(stationid = "USC00186350",
                       var = c("tmin","tmax","prcp"),
                       date_min = "1921-01-01",
                       date_max = "2022-12-31")

DCtemp <- left_join(tempDC$tmax, tempDC$tmin, by="date") 
DCtemp["tmax"][is.na(DCtemp["tmax"])] <- 0
DCtemp["tmin"][is.na(DCtemp["tmin"])] <- 0
DCtemp$tavg <- (DCtemp$tmax +DCtemp$tmin) /20
 
DCTemp.df <- data.frame(DCtemp)
DCTemp.df <- DCTemp.df %>% select(c(tavg, date)) 
 
colnames(DCTemp.df) <- c("tavg", "date")
 
DCBloom.df <- DCBloom.df %>% filter(year > 1948)
 
DC <- left_join(DCTemp.df, DCBloom.df, by = "date")
 
temp.date.DC <- DCTemp.df$date
temp.date.DC.df <- data.frame( 
   year = as.numeric(format(temp.date.DC, format = "%Y")),
   month = as.numeric(format(temp.date.DC, format = "%m")),
   day = as.numeric(format(temp.date.DC, format = "%d")))
 
DC <- cbind(DC, temp.date.DC.df)
colnames(DC)[4] <- "bloom_year"
DC <- DC %>% filter(year > 1948)
 
DC <- DC %>% group_by(year) %>% 
   slice(1:which.max(!is.na(bloom_doy))) %>% filter(!is.na(tavg))
 
DCTemps <- DC %>% group_by(year) %>% summarize(sum(tavg^2))
colnames(DCTemps)[2] <- "temp"
DCTemps <- DCTemps %>% filter(year != 1953, year != 2002)
DCBloom.df <- DCBloom.df %>% filter(year != 1953, year != 2002)
DCBloom.df <-  left_join(DCBloom.df, DCTemps)

DCTemps.recent <- DCTemps %>% filter(year > 1989)

DCTemps2023 <- read.csv("data/3247299")
DCtemps2023 <- DCTemps2023 %>% filter(STATION=="USW00013743") %>% 
  mutate(TAVG = (TAVG - 32) * 0.56)
colnames(DCtemps2023)[7] <- "daily_avg"

DCForecast <- read.csv("data/washington dc 2023-02-24 to 2023-04-30")
DCForecast$daily_avg <- (DCForecast$tempmax + DCForecast$tempmin) / 2
DCForecast <- DCForecast %>% mutate(daily_avg = (daily_avg - 32) * 0.56)

temp_DC <- bind_rows(DCtemps2023, DCForecast)

Quetelet_DC <- mean(DCTemps$temp)

doy_prediction <- which.max(cumsum(pmax(temp_DC$daily_avg[1:120], na.rm = TRUE)^2) > Quetelet_DC)
DC_predictions <- predict(lm(bloom_doy~temp, data=DCBloom.df))


###########################################################################################
##################################### KYOTO, JAPAN ########################################
###########################################################################################

kyotoBloom <- read.csv("data/kyoto")
kyotoBloom.df <- data.frame(kyotoBloom)

kyotoBloom.df <- kyotoBloom.df %>% select(-c(lat, long, alt))
kyotoBloom.df$date <- as.Date(kyotoBloom.df$bloom_date, format="%Y-%m-%d")


kyotoTemp <- ghcnd_search(stationid = "JA000047759",
                          var = "tavg",
                          date_min = "1953-01-01",
                          date_max = "2022-12-31")
kyotoTemp.df <- data.frame(kyotoTemp)
kyotoTemp.df <- kyotoTemp.df %>% select(-c(tavg.id,tavg.mflag,tavg.qflag,tavg.sflag)) 

colnames(kyotoTemp.df) <- c("tavg", "date")
kyotoTemp.df$tavg_red <- kyotoTemp.df$tavg / 10

kyotoBloom.df <- kyotoBloom.df %>% filter(year > 1952)

kyoto <- left_join(kyotoTemp.df, kyotoBloom.df, by = "date")

temp.date <- kyotoTemp.df$date
temp.date.df <- data.frame( 
  year = as.numeric(format(temp.date, format = "%Y")),
  month = as.numeric(format(temp.date, format = "%m")),
  day = as.numeric(format(temp.date, format = "%d")))

kyoto <- cbind(kyoto, temp.date.df)
colnames(kyoto)[5] <- "bloom_year"

kyoto <- kyoto %>% group_by(year) %>% 
  slice(1:which.max(!is.na(bloom_doy))) %>% filter(!is.na(tavg_red))

kyotoTemps <- kyoto %>% group_by(year) %>% summarize(sum((tavg_red)^2 ))
colnames(kyotoTemps)[2] <- "temp"
kyotoTemps <- kyotoTemps %>% filter(year != 1974, year != 1985, year != 1988)
kyotoBloom.df <- kyotoBloom.df %>% filter(year != 1974, year != 1985, year != 1988)
kyotoBloom.df <- left_join(kyotoBloom.df, kyotoTemps)

kyoto_predictions <- predict(lm(bloom_doy~temp, data=kyotoBloom.df))

kyotoTemps2023 <- read.csv("data/3247299")
kyototemps2023 <- kyotoTemps2023 %>% filter(STATION=="JA000047759") %>% 
  mutate(TAVG = (TAVG - 32) * 0.56)
colnames(kyototemps2023)[7] <- "daily_avg"

kyotoForecast <- read.csv("data/kyoto, japan 2023-02-24 to 2023-04-30")
kyotoForecast$daily_avg <- (kyotoForecast$tempmax + kyotoForecast$tempmin) / 2
kyotoForecast <- kyotoForecast %>% mutate(daily_avg = (daily_avg - 32) * 0.56)

temp_kyoto <- bind_rows(kyototemps2023, kyotoForecast)

Quetelet_kyoto <- mean(kyotoTemps$temp)

doy_prediction_kyoto <- which.max(cumsum(pmax(temp_kyoto$daily_avg[1:120], na.rm = TRUE)^2) > Quetelet_kyoto)


###########################################################################################
################################# LIESTAL, SWITZERLAND ####################################
###########################################################################################


liestalBloom <- read.csv("data/liestal")
liestalBloom.df <- data.frame(liestalBloom)

liestalBloom.df <- liestalBloom.df %>% select(-c(lat, long, alt))
liestalBloom.df$date <- as.Date(liestalBloom.df$bloom_date, format="%Y-%m-%d")


tempLiestal <- ghcnd_search(stationid = "GME00127786",
                            var = c("tmin","tmax","prcp"),
                            date_min = "1921-01-01",
                            date_max = "2022-12-31")

liestaltemp <- left_join(tempLiestal$tmax, tempLiestal$tmin, by="date") 
liestaltemp["tmax"][is.na(liestaltemp["tmax"])] <- 0
liestaltemp["tmin"][is.na(liestaltemp["tmin"])] <- 0
liestaltemp$tavg <- (liestaltemp$tmax +liestaltemp$tmin) /20
liestalTemp.df <- data.frame(liestaltemp)
liestalTemp.df <- liestalTemp.df %>% select(date, tavg)



liestalBloom.df <- liestalBloom.df %>% filter(year > 1953)

liestal <- left_join(liestalTemp.df, liestalBloom.df, by = "date")



temp.date.liestal <- liestalTemp.df$date
temp.date.df <- data.frame( 
  year = as.numeric(format(temp.date.liestal, format = "%Y")),
  month = as.numeric(format(temp.date.liestal, format = "%m")),
  day = as.numeric(format(temp.date.liestal, format = "%d")))

liestal <- cbind(liestal, temp.date.df)
colnames(liestal)[4] <- "bloom_year"
liestal <- liestal %>% filter(year >1953)

liestal <- liestal %>% group_by(year) %>% 
  slice(1:which.max(!is.na(bloom_doy))) %>% filter(!is.na(tavg))


liestalTemps <- liestal %>% group_by(year) %>% summarize(sum((tavg)^2))
colnames(liestalTemps)[2] <- "temp"
bloom_doy.liestal <- liestal %>% drop_na(bloom_doy)
liestalTemps <- liestalTemps %>% filter(year != 2016)

liestalBloom.df <- left_join(bloom_doy.liestal, liestalTemps)

liestalTemps2023 <- read.csv("data/3247299")
liestaltemps2023 <- liestalTemps2023 %>% filter(STATION=="SZ000003700") %>% 
  mutate(TAVG = (TAVG - 32) * 0.56)
colnames(liestaltemps2023)[7] <- "daily_avg"

liestalForecast <- read.csv("data/liestal switzerland 2023-02-24 to 2023-04-30")
liestalForecast$daily_avg <- (liestalForecast$tempmax + liestalForecast$tempmin) / 2
liestalForecast <- liestalForecast %>% mutate(daily_avg = (daily_avg - 32) * 0.56)

temp_liestal <- bind_rows(liestaltemps2023, liestalForecast)

Quetelet_liestal <- mean(liestalTemps$temp)

doy_prediction_liestal <- which.max(cumsum(pmax(temp_liestal$daily_avg[1:120], na.rm = TRUE)^2) > Quetelet_liestal)

liestal_predictions <- predict(lm(bloom_doy~temp, data=liestalBloom.df))

###########################################################################################
############################## VANCOUVER, BRITISH COLUMBIA ################################
###########################################################################################

vanTemps2023 <- read.csv("data/3247299")
vantemps2023 <- vanTemps2023 %>% filter(STATION=="CA001108380") %>% 
  mutate(TAVG = (TAVG - 32) * 0.56)
colnames(vantemps2023)[7] <- "daily_avg"

vanForecast <- read.csv("data/vancouver canada 2023-02-24 to 2023-04-30")
vanForecast$daily_avg <- (vanForecast$tempmax + vanForecast$tempmin) / 2
vanForecast <- vanForecast %>% mutate(daily_avg = (daily_avg - 32) * 0.56)

temp_van <- bind_rows(vantemps2023, vanForecast)
Quetelet_van <- (Quetelet_DC + Quetelet_kyoto + Quetelet_liestal) / 3

doy_prediction_van <- which.max(cumsum(pmax(temp_van$daily_avg[1:120], na.rm = TRUE)^2) > Quetelet_van)

vanTemps <- left_join(kyotoBloom.df, DCBloom.df, by = "year")
vanTemps <- left_join(vanTemps, liestalBloom.df, by = "year")

vanTemps <- vanTemps %>% select(c(year, bloom_doy.x, temp.x, bloom_doy.y, temp.y, bloom_doy, temp))

vanTemps$tavg <- (vanTemps[,3] + vanTemps[,5] + vanTemps[,7]) / 3
vanTemps$bloom_doy_avg <- (vanTemps[,2] + vanTemps[,4] + vanTemps[,6]) / 3

van_predictions <- predict(lm(bloom_doy_avg ~ tavg, data = vanTemps))

###########################################################################################
###################################### PREDICTIONS ########################################
###########################################################################################

prediction_year <- c(2023,2024,2025,2026,2027,2028,2029,2030,2031,2032)
kyoto_predict <- c(doy_prediction_kyoto,kyoto_predictions[[50]],kyoto_predictions[[51]],
                   kyoto_predictions[[52]],kyoto_predictions[[53]],kyoto_predictions[[54]],
                   kyoto_predictions[[55]],kyoto_predictions[[56]],kyoto_predictions[[57]],
                   kyoto_predictions[[58]])
liestal_predict <- c(doy_prediction_liestal,liestal_predictions[[59]],liestal_predictions[[60]],
                    liestal_predictions[[61]],liestal_predictions[[62]],liestal_predictions[[63]],
                    liestal_predictions[[64]],liestal_predictions[[65]],liestal_predictions[[66]],
                    liestal_predictions[[67]])
DC_predict <- c(doy_prediction,DC_predictions[[59]],DC_predictions[[60]],
                DC_predictions[[61]],DC_predictions[[62]],DC_predictions[[63]],
                DC_predictions[[64]],DC_predictions[[65]],DC_predictions[[66]],
                DC_predictions[[67]])
van_predict <- c(doy_prediction_van,van_predictions[[46]],van_predictions[[47]],
                 van_predictions[[48]],van_predictions[[49]],van_predictions[[50]],
                 van_predictions[[51]],van_predictions[[52]],van_predictions[[53]],
                 van_predictions[[54]])
submission_predictions <- data.frame(prediction_year,round(kyoto_predict),round(liestal_predict),
                                       round(DC_predict),round(van_predict))
colnames(submission_predictions) <- c("year","kyoto","liestal","washingtondc","vancouver")

write.csv(submission_predictions, file = "cherry-predictions.csv",
          row.names = FALSE)























