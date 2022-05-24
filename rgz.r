{
  data = read.csv(file = "data.csv")[, c(0:2)] #вытаскиваем нужные данные
  
  {
    graphdata = data.frame(date = array(1:length(data[, 1])), value = data[, 2])#данные для построение графиков
    
    {
      library(ggplot2)
      ggplot(graphdata, aes(x = date, y = value, group = 1)) + geom_line() +
        geom_smooth(method = lm, formula = y ~ poly(x, 2))
    }#основной график
    
    {
      b = boxplot(graphdata$value, xlab = "value", ylab = "$")
      b <-
        boxplot(
          graphdata$value,
          xlab = "value",ylab = "$",
          main = paste(
            "медиана>",b$stats[3],
            "\n25%-75%->",b$stats[2],"-",b$stats[4],
            "\n0%-100%->",b$stats[1],"-",b$stats[5]
          )
        )
    }#ящик с усами
    
    {
      library(forecast)
      ggAcf(graphdata$value)
    }#автокорреляция
    
    {
      library(forecast)
      ggPacf(graphdata$value)
    }#Частичная автокорреляция
  }#графики
  
  {
    library(forecast)
    library(ggplot2)
    {
      length <- length(data[, 1]) - 25
      reduceddata = data.frame(date = array(1:length), value = data[1:length, 2])
      ggplot(reduceddata, aes(x = date, y = value, group = 1)) + geom_line() 
    }#подготовка
    {
      etsauto <- ets(reduceddata[, 2])
      summary(etsauto)
      coef = etsauto$fitted[1] / etsauto$residuals[1]/10
      drawdata <-
        data.frame(
          date = reduceddata$date,
          base = reduceddata$value,
          fit = etsauto$fitted,
          res = (etsauto$residuals)
        )
      print(accuracy(etsauto$fitted,reduceddata$value))
      ggplot(drawdata, aes(x = date)) +
        geom_line(col = "red", aes(y = base)) +
        geom_line(col = "blue", aes(y = fit)) +
        geom_line(col = "darkgreen", aes(y = res*coef))+
        scale_y_continuous(sec.axis = sec_axis(~./coef))
    }#автоподбор коэффициентов
    {
      etsa <-ets(reduceddata[, 2],alpha = 0.95,model = "ZZZ",beta = 0.02,phi=0.98)
      summary(etsa)
      coef = etsa$fitted[1] / etsa$residuals[1]/10
      drawdata <-
        data.frame(
          date = reduceddata$date,
          base = reduceddata$value,
          fit = etsa$fitted,
          res = (etsa$residuals)
        )
      print(accuracy(etsa$fitted,reduceddata$value))
      ggplot(drawdata, aes(x = date)) +
        geom_line(col = "red", aes(y = base)) +
        geom_line(col = "blue", aes(y = fit)) +
        geom_line(col = "darkgreen", aes(y = res*coef))+
        scale_y_continuous(sec.axis = sec_axis(~.*coef))
    }#a=0.95,b=0.2, p=0,98
    {
      etsb <-ets(reduceddata[, 2],alpha = 0.93,model = "ZZZ",beta = 0.2, phi=0.96)
      summary(etsb)
      b=accuracy(etsb$fitted,reduceddata[,2])
      coef = etsb$fitted[1] / etsb$residuals[1]/10
      x=etsb$states
      drawdata <-
        data.frame(
          date = reduceddata$date,
          base = reduceddata$value,
          fit = etsb$fitted,
          res = (etsb$residuals)
        )
      print(accuracy(etsb$fitted,reduceddata$value))
      ggplot(drawdata, aes(x = date)) +
        geom_line(col = "red", aes(y = base)) +
        geom_line(col = "blue", aes(y = fit)) +
        geom_line(col = "darkgreen", aes(y = res*coef))+
        scale_y_continuous(sec.axis = sec_axis(~.*coef))
    }#a=0.93,b=0.3,p=0,05
    
    {
      estdrawData=data.frame(x=c(1:486), y=etsauto$residuals)
      ggplot(estdrawData, aes(x = x, y = y, group = 1)) + geom_line()
      ggAcf(etsauto$residuals)
    }#графики для модели
    {
      forecast<-forecast(etsauto$fitted, h=(length(data[,1])-length))
      printData=data.frame(date=c(1:25), 
                           lower=forecast$lower[,2], 
                           upper=forecast$upper[,2], 
                           real=data$United.States.USD.[487:511], 
                           mean=forecast$mean)
      print(printData)
      ggplot(printData, aes(x=date))+
        ylab("cost")+
        geom_line(col = "red", aes(y = real)) +
        geom_line(col = "blue", aes(y = mean)) +
        geom_line(col = "darkgreen", aes(y = lower))+
        geom_line(col = "darkgreen", aes(y = upper))+
        geom_smooth(method = lm, formula = y ~ log(x, base = 10), aes(y=real), col="darkred")
      accuracy(forecast, printData$real)
        
    }#прогноз
    
    {
      arima<-auto.arima(reduceddata$value)
      summary(arima)
      arimaForecast<-forecast(arima, h=25)
      arimaData=data.frame(date=c(1:25), 
                           real=data$United.States.USD.[487:511], 
                           upper=arimaForecast$upper[,2], 
                           lower=arimaForecast$lower[,2], 
                           mean=arimaForecast$mean)
      print(arimaData)
      ggplot(arimaData, aes(x=date))+
        ylab("cost")+
        geom_line(col = "red", aes(y = real)) +
        geom_line(col = "blue", aes(y = mean)) +
        geom_line(col = "darkgreen", aes(y = upper))+
        geom_line(col = "darkgreen", aes(y = lower))+
        geom_smooth(method = lm, formula = y ~ log(x, base = 10), aes(y=real), col="darkred")
      ggAcf(arima$residuals)
      accuracy(arimaData$mean, arimaData$real)
      accuracy(arimaForecast, arimaData$real)
    }#arima
  }#моделирование
  
}
