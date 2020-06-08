######### Script for cleaning and combining the max daily water temperature for lake washington #########
setwd("H:/lake_washington_automailer")

#load libraries
library(tidyverse)
library(dplyr)
library(fpp2)
library(lubridate)
library(rnoaa)
library(gridExtra)
library(rvest)
library(blastula)
library(keyring)
library(knitr)
library(kableExtra)
options(noaakey = 'ovBBRCZSBfeVOpnOjTlwylHGTVflaNMh')

#read in the data
lw2008 = read.delim('lw2008.txt', sep="\t", header=TRUE)
lw2009 = read.delim('lw2009.txt', sep="\t", header=TRUE)
lw2010 = read.delim('lw2010.txt', sep="\t", header=TRUE)
lw2011 = read.delim('lw2011.txt', sep="\t", header=TRUE)
lw2012 = read.delim('lw2012.txt', sep="\t", header=TRUE)
lw2013 = read.delim('lw2013.txt', sep="\t", header=TRUE)
lw2014 = read.delim('lw2014.txt', sep="\t", header=TRUE)
lw2015 = read.delim('lw2015.txt', sep="\t", header=TRUE)
lw2016 = read.delim('lw2016.txt', sep="\t", header=TRUE)
lw2017 = read.delim('lw2017.txt', sep="\t", header=TRUE)
lw2018 = read.delim('lw2018.txt', sep="\t", header=TRUE)
lw2019 = read.delim('lw2019.txt', sep="\t", header=TRUE)
lw2020 = read.delim('lw2020.txt', sep="\t", header=TRUE)

#combine lw2009 through lw2019
lw_historic = rbind(lw2008, lw2009)
lw_historic = rbind(lw_historic, lw2010)
lw_historic = rbind(lw_historic, lw2011)
lw_historic = rbind(lw_historic, lw2012)
lw_historic = rbind(lw_historic, lw2013)
lw_historic = rbind(lw_historic, lw2014)
lw_historic = rbind(lw_historic, lw2015)
lw_historic = rbind(lw_historic, lw2016)
lw_historic = rbind(lw_historic, lw2017)
lw_historic = rbind(lw_historic, lw2018)
lw_historic = rbind(lw_historic, lw2019)
lw_historic = rbind(lw_historic, lw2020)

# parse the ymd from "Date"
lw_historic$Day = as.Date(lw_historic$Date, format = "%m/%d/%Y")

# convert to daily max water temps
lw_historic_daily = lw_historic %>% 
  group_by(Day) %>% 
  summarize(peakTempC = max(Temperature..Â.C.))

# looks like we might be missing some days -- lets see which ones
Day = seq(ymd('2009-01-01'), ymd('2020-06-06'), by = 'day')
lakeDays = lw_historic_daily$Day
missingDays = as.data.frame(Day) %>% 
  anti_join(as.data.frame(lakeDays), by = c("Day" = "lakeDays"))

# we may have to inerperolate a few days (455) to be exact--will join so that the data frame is the proper length

lw_historic_daily = as.data.frame(Day) %>% 
  left_join(lw_historic_daily)

#grab the average wind (km/h) and max temps (tenths of a degree celsius) from the SEATAC weather station
dailyMaxTemps = as.data.frame(ghcnd_search("USW00024234", var = "TMAX", date_min = '2009-01-01', date_max = as.character(Sys.Date())))
dailyAverageWindSpeed = as.data.frame(ghcnd_search("USW00024234", var = "AWND", date_min = '2009-01-01', date_max = as.character(Sys.Date())))

#converting each of the variablesinto time series objects
dailyPeakLakeTemps = ts(lw_historic_daily$peakTempC, frequency = 365.25, start = c(2009,1))
dailyPeakAirTemps = ts(dailyMaxTemps$tmax.tmax/10, frequency = 365.25, start = c(2009,1))
dailyAverageWindSpeed = ts(dailyAverageWindSpeed$awnd.awnd, frequency = 365.25, start = c(2009,1))


#visualizing our time series
lakeAirMovements = autoplot(cbind(dailyPeakAirTemps,dailyPeakLakeTemps),facets = TRUE) +
  theme_classic() +
  ylab("") +
  xlab("") +
  ggtitle("Daily peak air and lake temps (*Celsius)") +
  labs(subtitle = "For Lake Washington", caption = "Source: King County Lake Bouy data + NOAA SEATAC weatherstation data") +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        strip.text.y = element_text(size = 15, face = 'bold'),
        plot.title = element_text(size = 19),
        plot.subtitle = element_text(size = 17, face = 'italic'),
        plot.caption = element_text(size = 15)) +
  theme(axis.ticks.x = element_blank())
lakeAirMovements
ggsave(lakeAirMovements, filename = 'Historical Lake Washington Max Temps and Max Air Temps.png', height = 10, width = 8, units = 'in')  

lakeWindMovements = autoplot(cbind(dailyAverageWindSpeed,dailyPeakLakeTemps),facets = TRUE) +
  theme_classic() +
  ylab("") +
  xlab("") +
  ggtitle("Daily average wind speed (kn/h) and peak lake temps (degrees Celsius)") +
  labs(subtitle = "For Lake Washington", caption = "Source: King County Lake Bouy data + NOAA SEATAC weatherstation data") +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        strip.text.y = element_text(size = 15, face = 'bold'),
        plot.title = element_text(size = 19),
        plot.subtitle = element_text(size = 17, face = 'italic'),
        plot.caption = element_text(size =15)) +
  theme(axis.ticks.x = element_blank())
lakeWindMovements
ggsave(lakeWindMovements, filename = 'Historical Lake Washington Max Temps and Average Wind Speeds.png', height = 10, width = 8, units = 'in')

  # scatterplots
  lakeAirScatter = ggplot(data.frame(cbind(dailyPeakAirTemps,dailyPeakLakeTemps)), aes(x = dailyPeakAirTemps, y = dailyPeakLakeTemps)) +
    geom_point()+ 
    theme_classic() +
    ggtitle('') +
    labs(subtitle = "For Lake Washington", caption = "Source: King County Lake Bouy data + NOAA SEATAC weatherstation data") +
    scale_color_discrete(name = '', labels = c('Weekend', 'Weekday')) + 
    xlab("Daily Air Temperature (Celsius)") +
    ylab("Daily Lake Temperature (Celsius)") +
    theme(
      legend.title = element_text(size = 17),
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      axis.title.y = element_text(size = 18),
      axis.title.x = element_text(size = 18),
      legend.text = element_text(size = 17),
      plot.title = element_text(size = 19),
      plot.subtitle = element_text(size = 17, face = 'italic'),
      plot.caption = element_text(size = 15))
  lakeAirScatter
  ggsave(lakeAirScatter, filename = 'Scatterplot of Lake temps and Air temps.png', heigh = 10, width = 10, units = 'in')
 
  lakeWindScatter = ggplot(data.frame(cbind(dailyAverageWindSpeed,dailyPeakLakeTemps)), aes(x = dailyAverageWindSpeed, y = dailyPeakLakeTemps)) +
    geom_point() + 
    theme_classic() +
    ggtitle('') +
    labs(subtitle = "For Lake Washington", caption = "Source: King County Lake Bouy data + NOAA SEATAC weatherstation data") +
    scale_color_discrete(name = '', labels = c('Weekend', 'Weekday')) + 
    xlab("Daily Average Wind Speeds (kn/h)") +
    ylab("Daily Lake Temperature (Celsius)") +
    theme(
      legend.title = element_text(size = 17),
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      axis.title.y = element_text(size = 18),
      axis.title.x = element_text(size = 18),
      legend.text = element_text(size = 17),
      plot.title = element_text(size = 19),
      plot.subtitle = element_text(size = 17, face = 'italic'),
      plot.caption = element_text(size = 15))
  lakeWindScatter
  ggsave(lakeWindScatter, filename = 'Scatterplot of Lake temps and wind speeds.png', heigh = 10, width = 10, units = 'in')
  
  # correlogram
  correlograms = grid.arrange(
    ggAcf(dailyAverageWindSpeed) +
      theme_classic() + 
      ylab("") +
      xlab("Lag") +
      ggtitle("Correlogram: Daily Average Wind Speeds (km/h") +
      labs(subtitle = "For Lake Washington", caption = "Source: NOAA SEATAC weatherstation data") +
      theme(axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15),
            axis.title.y = element_text(size = 18),
            axis.title.x = element_text(size = 18),
            strip.text.y = element_text(size = 15, face = 'bold'),
            plot.title = element_text(size = 19),
            plot.subtitle = element_text(size = 17, face = 'italic'),
            plot.caption = element_text(size = 15)) +
      theme(axis.ticks.x = element_blank()),
    ggAcf(dailyPeakAirTemps)+
      theme_classic() + 
      ylab("") +
      xlab("Lag") +
      ggtitle("Correlogram: Daily Max Air Temperatures (Celsius)") +
      labs(subtitle = "For Lake Washington", caption = "Source: NOAA SEATAC weatherstation data") +
      theme(axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15),
            axis.title.y = element_text(size = 18),
            axis.title.x = element_text(size = 18),
            strip.text.y = element_text(size = 15, face = 'bold'),
            plot.title = element_text(size = 19),
            plot.subtitle = element_text(size = 17, face = 'italic'),
            plot.caption = element_text(size = 15)) +
      theme(axis.ticks.x = element_blank()),
    ggAcf(dailyPeakLakeTemps)+
      theme_classic() + 
      ylab("") +
      xlab("Lag") +
      ggtitle("Correlogram: Daily Max Air Temperatures (Celsius)") +
      labs(subtitle = "For Lake Washington", caption = "Source: King County Lake Bouy data") +
      theme(axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15),
            axis.title.y = element_text(size = 18),
            axis.title.x = element_text(size = 18),
            strip.text.y = element_text(size = 15, face = 'bold'),
            plot.title = element_text(size = 19),
            plot.subtitle = element_text(size = 17, face = 'italic'),
            plot.caption = element_text(size = 15)) +
      theme(axis.ticks.x = element_blank()),
    ncol = 3
  )
  correlograms
  ggsave(correlograms, filename = 'correlograms.png', height = 6.5, width = 8*3, units = 'in')
  
  # computing Ljung-Box test for autocorrelations for daily max temperature and daily peak loads
  Box.test(dailyPeakAirTemps,lag=10,fitdf=0, type="Lj")
  Box.test(dailyPeakLakeTemps,lag=10,fitdf=0,type="Lj")
  
  # summary statistics
  summary(dailyAverageWindSpeed)
  summary(dailyPeakAirTemps)
  summary(dailyPeakLakeTemps)
  
  # split into training and test sets
  dailyAverageWindSpeed = na.interp(dailyAverageWindSpeed)
  dailyPeakAirTemps = na.interp(dailyPeakAirTemps)
  dailyPeakLakeTemps = na.interp(dailyPeakLakeTemps)
  LWData = cbind(dailyAverageWindSpeed, dailyPeakAirTemps)
  LWData = cbind(LWData, dailyPeakLakeTemps)
  training = window(LWData, end = c(2020,nrow(LWData) - 4077))
  test = window(LWData, start = c(2020, nrow(LWData) - 4076), end = c(2020, nrow(LWData) - 4076 + 57))
  
# # designing nn model
# 
#   # neural network model
#   nnmodel = nnetar(training[, 'dailyPeakLakeTemps'], xreg = training[, 'LWData.dailyPeakAirTemps'])
#   summary(nnmodel)
#   
#   # residual diagnostics
#   checkresiduals(nnmodel)
# 
# # deploying forecasts
#   newData = data.frame('LWData.dailyPeakAirTemps' = test[, 'LWData.dailyPeakAirTemps'])
#   names(newData) = c('LWData.dailyPeakAirTemps')
#   nnforecast = forecast(nnmodel, h = nrow(LWData) - 4076, xreg = test[,'LWData.dailyPeakAirTemps'])
#   
# autoplot(test[,'dailyPeakLakeTemps'], series = 'Actual Values') +
#   autolayer(nnforecast, PI = FALSE, series = 'Neural Network forecast') +
#   theme_classic() +
#   ylab('Daily Peak Lake Temps') +
#   xlab("") +
#   labs(subtitle = "For Lake Washington", caption = "Source: King County Lake Bouy data")
# 
# # training on a shorter time-frame
# training2 = window(LWData, start = c(2018, 1), end = c(2020, nrow(LWData) - 4077))
# 
# nnmodel2 = nnetar(training2[, 'dailyPeakLakeTemps'], xreg = training2[, 'LWData.dailyPeakAirTemps'])
# nnforecast2 = forecast(nnmodel2, h = nrow(LWData) - 4076, xreg = test[,'LWData.dailyPeakAirTemps'])
# autoplot(test[,'dailyPeakLakeTemps'], series = 'Actual Values') +
#   autolayer(nnforecast, PI = FALSE, series = 'Long-term Neural Network forecast') +
#   autolayer(nnforecast2, PI = FALSE, series = 'Short-term Neural Network forecast') +
#   theme_classic() +
#   ylab('Daily Peak Lake Temps') +
#   xlab("") +
#   labs(subtitle = "For Lake Washington", caption = "Source: King County Lake Bouy data")
# 
# 
# # the longer term model may work better--lets see the forecast for the last few days of lake data that we have where temperatures are not available
# 
# # first traing the model on all data through june 3rd
# 
# training3 = window(LWData, start = c(2009, 1), end = c(2020, 157))
# test2 = window(LWData, start = c(2020, 158), end = c (2020, 160))
# 
# # 21.11, 18.89, 17.78
# nnmodelfinal = nnetar(training3[, 'dailyPeakLakeTemps'], xreg = training3[, 'LWData.dailyPeakAirTemps'])
# nnforecastfinal = forecast(nnmodelfinal, h = 3, xreg = c(21.11, 18.89, 17.78))
# autoplot(test2[, 'dailyPeakLakeTemps'], series = 'Actual Values') +
#   autolayer(nnforecastfinal, PI = FALSE, series = 'Long-term Neural network forecast') +
#   theme_classic() +
#   ylab('Daily Peak Lake Temps') +
#   labs(subtitle = "For Lake Washington", caption = "Source: King COunty Lake Bouy data")
# 
# # using the short-term model trained beginning 2018
# training4 = window(LWData, start = c(2017, 1), end = c(2020, 157))
# nnmodelfinalshort = nnetar(training4[, 'dailyPeakLakeTemps'], xreg = training4[, 'LWData.dailyPeakAirTemps'])
# nnforecastfinalshort = forecast(nnmodelfinalshort, h = 3, xreg = c(21.11 ,18.89, 17.78))
# autoplot(test2[, 'dailyPeakLakeTemps'], series = 'Actual Values') +
#   autolayer(nnforecastfinal, PI = FALSE, series = 'Long-term Neural network forecast') +
#   autolayer(nnforecastfinalshort, PI = FALSE, series = 'Short-term Neural network forecast') +
#   theme_classic() +
#   ylab('Daily Peak Lake Temps') +
#   labs(subtitle = "For Lake Washington", caption = "Source: King County Lake Bouy data")
# 

# creating the model we will use to forecast
LWData = LWData[,-1]
LWData[,'dailyPeakLakeTemps'] = na.interp(LWData[, 'dailyPeakLakeTemps'])

# repeating the training + forecasting for the previous week's data
training_data_old = window(LWData, start = c(2017,1))
n = dim(training_data_old)[1]
test_data_old = training_data_old[(n-7):n,]
training_data_old = training_data_old[1:(n-7),]
training_data_old = ts(training_data_old, start = c(2017,1), frequency = 365.25)
training_data_live = window (LWData, start = c(2017,1))
nnmodelold = nnetar(training_data_old[, 'dailyPeakLakeTemps'], xreg = training_data_old[, 'LWData.dailyPeakAirTemps'])
nnmodellive = nnetar(training_data_live[, 'dailyPeakLakeTemps'], xreg = training_data_live[, 'LWData.dailyPeakAirTemps'])

# pulling upcoming week's air temperature forecasts
forecastpage = read_html('https://www.king5.com/10-day')
tenDayForecasthtml = html_nodes(forecastpage, '.weather-10-day__temperature-high')
tenDayData = html_text(tenDayForecasthtml)
head(tenDayData) # looks like it works! :D
tenDayData = as.numeric(tenDayData) # convert to celsisu
tenDayData = (tenDayData-32) * 5/9

# generating forecasts on the models
lastLake7dayforecast = forecast(nnmodelold, h = 7, xreg = test_data_old[, 'LWData.dailyPeakAirTemps'])
lake7dayforecast = forecast(nnmodellive, h = 7, xreg = tenDayData)

# switch from celsius to fahrenheit
lake7dayforecast$mean = lake7dayforecast$mean *9/5 + 32
lastLake7dayforecast$mean = lastLake7dayforecast$mean *9/5 + 32


# plotting the upcoming forecast
test_data_old = ts(test_data_old, start = c(2020,(end(lastLake7dayforecast$mean)[1]-2020)*365.25-6), frequency = 365.25)
lakeforcastchart = autoplot(test_data_old[, 'dailyPeakLakeTemps']*9/5+32, series = 'Acutal Lake Temps') +
  autolayer(lastLake7dayforecast$mean, series = "Last-week's 7 Day Forecast") +
  autolayer(lake7dayforecast$mean, series = 'Future 7 Day  Forecast') +
  theme_classic() +
  ylab('Daily Peak Lake Temps (Fahrenheit)') +
  xlab('Last Sunday to Next Sunday') +
  labs(subtitle = "For Lake Washington", caption = "Source: King County Lake Bouy data") +
  theme(axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 35),
        axis.title.x = element_text(size = 35),
        legend.text = element_text(size = 38),
        legend.title = element_text(size = 40),
        strip.text.y = element_text(size = 35, face = 'bold'),
        plot.title = element_text(size = 45),
        plot.subtitle = element_text(size = 37, face = 'italic'),
        plot.caption = element_text(size = 30),
        legend.position="bottom", 
        legend.box = "horizontal") 
lakeforcastchart
ggsave(lakeforcastchart, filename = 'Two-Week LW Temp Forecast.png', height = 20, width = 28, units = 'in') 

# create the nice table for the forecast
Dates = as.character(seq(Sys.Date(), by = "day", length.out = 7))
Dates = str_sub(Dates,6)
Dates = paste(Dates, "     ")
LW_Daily_Peak_Temp_Forecast = paste(as.character(format(round(lake7dayforecast$mean,2)), nsmall = 2), "*F")
Lake_Washington_Temps = as.data.frame(cbind(Dates,LW_Daily_Peak_Temp_Forecast[1:7]))
Lake_Washington_Temps$LW_Actual_Temp = "TBD"
names(Lake_Washington_Temps) = c("Dates", "LW_Daily_Peak_Temp_Forecast", "LW_Actual_Temp")

# create the nice table for the previous week's forecast
Old_Dates = as.character(seq(Sys.Date()-7, by = 'day', length.out=7))
Old_Dates = str_sub(Old_Dates,6)
Old_Dates = paste(Old_Dates, "     ")
LW_Forecasted_Temps = paste(as.character(format(round(lastLake7dayforecast$mean,2)), nsmall = 2), "*F")
LW_Actual_Temps = paste(as.character(format(round(test_data_old[, 'dailyPeakLakeTemps']*9/5+32,2)), nsmall = 2), "*F")
Last_Week_Lake_Washington_Temps = as.data.frame(cbind(Old_Dates,cbind(LW_Forecasted_Temps,LW_Actual_Temps)))
names(Last_Week_Lake_Washington_Temps) = c("Dates", "LW_Daily_Peak_Temp_Forecast", "LW_Actual_Temp")

# combine the two tables
Lake_Washington_Temps_Old_And_New = rbind(Last_Week_Lake_Washington_Temps[1:7,], Lake_Washington_Temps)

# set up the email with the upcoming forecast
email = compose_email(
  body = md(c("Still working on improving the front end of this--let me know your thoughts! \n",
              "Please find the previous and upcoming daily-peak Lake Washington surface temperature forecasts below: \n", 
  Lake_Washington_Temps_Old_And_New %>% 
    kable() %>% 
    kable_styling(bootstrap_option = "striped"),
  "\n",
  add_image(file = "Two-Week LW Temp Forecast.png")
  )
  ),
  footer = blocks(
    block_text(' \n ---------------------------------------------------------------------------------------------------------------------------------------------------------'),
    block_text("This is an automated message sent every Sunday, to request to be removed or request to add other email addresses please reach out to topherlacrampe@gmail.com"),
    block_text("\n More information about my code and data sources can be found here: https://github.com/tlacrampe/lake_washington_automailer"),
    block_social_links(
      social_link(
        service = "GitHub",
        link = "https://github.com/tlacrampe/lake_washington_automailer",
        variant = "color"
      )),
    block_text(' \n ---------------------------------------------------------------------------------------------------------------------------------------------------------'),
    block_text("\n \n Data sources include:"),
    block_text("\n King County Lake Washington Surface Bouy Data: https://green2.kingcounty.gov/lake-buoy/Data.aspx"),
    block_text("\n NOAA GHCND SEATAC Weatherstation Data: https://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:USW00024233/detail"),
    block_text("\n King5 10-Day Forecast (for generating week-ahead predictions): https://www.king5.com/10-day"),
    block_text(' \n ---------------------------------------------------------------------------------------------------------------------------------------------------------'),
    block_text(paste("\n \n Email sent on ", add_readable_time(), ".")),
    block_text(' \n ---------------------------------------------------------------------------------------------------------------------------------------------------------'),
    block_text( " \n Historical Lake Washington Surface and Air Temp (*Celsius): \n \n"),
    add_image(file = "Historical Lake Washington Max Temps and Max Air Temps.png"))
  )

# email = add_attachment(
#   email,
#   "Ten Day LW Temp Forecast.png"
# )

#create_smtp_creds_key(
#  id = 'gmail',
#  user = 'tophalacrampe@gmail.com',
#  provider = 'gmail'
#)
#email$attachments
email %>% 
  smtp_send(
    from = "tophalacrampe@gmail.com",
    to = c("topherlacrampe@gmail.com"),
    subject = "LW Forecasting Temp automailer (for father's day)--updated",
    credentials = creds_key(id="gmail")
  )

