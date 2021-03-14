
tsb_obsolescence<-function(customer, forecast_n, productVariants){
  
  obsolescence<-FALSE
  forecast_n<- as.numeric(forecast_n)
  sales<-read_from_csv(customer, "sales")
  list.df<-return_preprocessed_ts_product(sales,productVariants)
  myQuantity<-list.df$quantity
  TS_mySeries_TSB_Quantity_daily<-tsb(myQuantity, h=forecast_n ,outplot=c(TRUE))
  obsolescence.df<-as.data.frame(TS_mySeries_TSB_Quantity_daily$frc.out)
  
  if(TS_mySeries_TSB_Quantity_daily$frc.out[forecast_n] == 0){
    obsolescence<-TRUE
  }
  return(l<-list(isObsolete=obsolescence, obsolescence_demandRate_df=obsolescence.df))
  
  
}

product_forecasting<-function(customer, forecast_n, productVariants){
  
  forecast_n<- as.numeric(forecast_n)
  sales<-read_from_csv(customer, "sales")
  list.df<-return_preprocessed_ts_product(sales,productVariants)
  myQuantity<-list.df$quantity
  df<-list.df$salesDf
  df_quantity<-as.data.frame(myQuantity)
  TS_mySeries_TSB_Quantity_daily<-tsb(myQuantity, h=forecast_n ,outplot=c(TRUE))
  myRate<-TS_mySeries_TSB_Quantity_daily$frc.out
  
  TotalForecastedDemand <- sum(myRate)
  TotalForecastedSales <-round(as.numeric(TotalForecastedDemand, 0))*max(df$Price)
  dfForeacts_Croston<-df_quantity
  nrows<- nrow(dfForeacts_Croston)
  window<-nrows-forecast_n +1
  myYTest <-  ts(dfForeacts_Croston[-c(window:nrows),])      
  myYValidation<-dfForeacts_Croston[c(window:nrows),]
  validationTotalDemand<-sum(myYValidation)
  fit2 <- tsb(myYTest, h=forecast_n ,outplot=c(TRUE))
  myRate2 <- fit2$frc.out
  TotalForecastedDemand2 <- round(sum(myRate2),0)
  MAECroston<-abs(as.numeric(TotalForecastedDemand2)-as.numeric(validationTotalDemand))
  MAECroston<-as.numeric(MAECroston)
  
  
  #Train Comb model using Theta, ARIMA and Neural Network
  
  fit_nnetar <- nnetar(myQuantity)
  fit_nnetar_2 <- nnetar(myYTest)
  
  fit_arima <- auto.arima(myQuantity)
  fit_arima_2 <- auto.arima(myYTest)
  
  TS_mySeries_theta_daily <- thetaf(myQuantity, h=forecast_n)
  TS_mySeries_theta_Test<- thetaf(myYTest,h=forecast_n )
  
  TS_mySeries_nnetar_daily <- forecast(fit_nnetar, h=forecast_n)
  TS_mySeries_nnetar_Test <- forecast(fit_nnetar_2,h=forecast_n )
  
  TS_mySeries_arima_daily <- forecast(fit_arima, h=forecast_n)
  TS_mySeries_arima_Test <- forecast(fit_arima_2,h=forecast_n )
  
  forecast_Continuous_daily_df<-(TS_mySeries_theta_daily$mean+TS_mySeries_nnetar_daily$mean+TS_mySeries_arima_daily$mean)/3
  forecast_Continuous_test_df<-(TS_mySeries_theta_Test$mean+TS_mySeries_nnetar_Test$mean+TS_mySeries_arima_Test$mean)/3
  
  #Final forecasts
  TotalForecastedDemandArima<-round(sum(forecast_Continuous_daily_df),0)
  TotalSales<-TotalForecastedDemandArima*max(df$Price)
  
  
  #Final forecasts using tests data
  TotalForecastedDemand_arima<-round(sum(forecast_Continuous_test_df),0)
  
  
  #Forecast error
  ME_Hybrid<-abs(as.numeric(validationTotalDemand)-as.numeric(TotalForecastedDemand_arima))
  ME_Hybrid<-as.numeric(ME_Hybrid)
  
  
  if (ME_Hybrid>MAECroston){
    
    forecastedDemand<-round(as.numeric(TotalForecastedDemand, 0))
    forecastedRate<-round( as.numeric(myRate[1]) , 4 )
    forecastedSales<-round(TotalForecastedSales,4)
    forecastedDemand<-as.character(forecastedDemand)
    forecastedError<-MAECroston
    l<-list(method="Teunter Syntetos Babai", forecastedDemand= forecastedDemand,forecastedSales= forecastedSales, 
            forecastedError=forecastedError)
    
    
  }
  
  else{
    forecastedDemand<-as.numeric(TotalForecastedDemandArima)
    forecastedSales<-round(TotalSales,4)
    forecastedError<-ME_Hybrid
    forecastedDemand<-as.character(forecastedDemand)
    l<-list(method="Comb (Theta - ARIMA - MLP)",forecastedDemand= forecastedDemand,forecastedSales= forecastedSales, forecastError=forecastedError)
    
  }
  l<-as.data.frame(l)
  return(l)
}
