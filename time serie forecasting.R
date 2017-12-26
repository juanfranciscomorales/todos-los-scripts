

setwd("C:/Users/Francisco/Desktop")

library(ggplot2)

library(forecast)

library(tseries)

daily_data = read.csv('day.csv', header=TRUE, stringsAsFactors=FALSE) ## leo los datos

daily_data$Date = as.Date(daily_data$dteday) ## transformo a clase Date (fecha) la columna que tiene las fechas

ggplot(daily_data, aes(Date, cnt)) +  ## grafico para ver como se distribuyen los datos en el tiempo
        geom_line() + 
        scale_x_date('month')  + 
        ylab("Daily Bike Checkouts") +
        xlab("")

count_ts = ts(daily_data[, c('cnt')])  ## transformo la columna que tiene los datos en clase time serie

daily_data$clean_cnt = tsclean(count_ts) ## limpio los datos que son time serie

ggplot() +   ### grafico para ver como estan los datos luegos de la limpieza
        geom_line(data = daily_data, aes(x = Date, y = clean_cnt)) + 
        ylab('Cleaned Bicycle Count')


adf.test(daily_data$clean_cnt, alternative = "stationary") ## para testear si mis datos son estacionarios o no. Si es mejor a 0.05, entonces se acepta la hipotesis alternativa

fit <- auto.arima(daily_data$clean_cnt, seasonal=TRUE , stepwise = FALSE , approximation = FALSE , max.p = 100, max.q = 100, max.P = 10, max.Q = 10, max.order = 100, max.d = 100, max.D = 10 , trace = TRUE)

par(mar=c(3,3,3,3))

tsdisplay(residuals(fit), lag.max = 45)

fcast <- forecast(fit , h = 30)

plot(fcast)



#######



hold <- window(ts(daily_data$clean_cnt), start=700)

fit_no_holdout = auto.arima(daily_data$clean_cnt[-c(700:725)], seasonal=TRUE , stepwise = FALSE , approximation = FALSE , max.p = 100, max.q = 100, max.P = 10, max.Q = 10, max.order = 100, max.d = 100, max.D = 10 , trace = TRUE)c

fcast_no_holdout <- forecast(fit_no_holdout , h = 25)

plot(fcast_no_holdout)

lines(ts(daily_data$clean_cnt))
