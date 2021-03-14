
return_preprocessed_ts_store<-function(sales){
  
  sales<-sales[,c("OrderItemId","ProductVariantsId","Quantity","Price","CreationTime")]
  sales$Month_Yr_D<- format(as.Date(sales$CreationTime), "%Y-%m-%d")
  sales$Quantity<-as.numeric(sales$Quantity)
  sales$Price<-as.numeric(sales$Price)
  sales$OrderItemId<-as.numeric(sales$OrderItemId)
  sales$ProductVariantsId<-as.numeric(sales$ProductVariantsId)
  #Add a new column Store_Sales which contains sales (quantity*price)
  sales$Store_Sales<-sales$Quantity*sales$Price
  sales<-sales[,c("Store_Sales","OrderItemId","ProductVariantsId","Quantity","Price","Month_Yr_D")]
  #Aggregate data by time (Month-Yr_D)  
  df<-aggregate(. ~Month_Yr_D, data=sales, sum, na.rm=TRUE)
  #create a new data frame with only sales and time (time series model)
  df$Date = as.Date(df$Month_Yr_D)
  df$Month<- format(as.Date(df$Date), "%m")
  df$Yr<- format(as.Date(df$Date), "%Y")
  df$day <- weekdays(as.Date(df$Date))
  return (df)
}


return_preprocessed_ts_product<-function(sales,productVariants){
  
  sales<-subset( sales, ProductVariantsId == productVariants)
  
  sales<-sales[,c("OrderItemId","ProductVariantsId","Quantity","Price","CreationTime")]
  sales$Month_Yr_D<- format(as.Date(sales$CreationTime), "%Y-%m-%d")
  sales$Quantity<-as.numeric(sales$Quantity)
  sales$Price<-as.numeric(sales$Price)
  sales$OrderItemId<-as.numeric(sales$OrderItemId)
  sales$ProductVariantsId<-as.numeric(sales$ProductVariantsId)
  #Add a new column Store_Sales which contains sales (quantity*price)
  sales$Store_Sales<-sales$Quantity*sales$Price
  sales<-sales[,c("Store_Sales","OrderItemId","ProductVariantsId","Quantity","Price","Month_Yr_D")]
  #Add a new column Store_Sales which contains sales (quantity*price)
  sales$Store_Sales<-sales$Quantity*sales$Price
  #Aggregate data by time (Month-Yr_D)  
  df<-aggregate(. ~Month_Yr_D, data=sales, sum, na.rm=TRUE)
  #create a new data frame 
  df$Date = as.Date(df$Month_Yr_D)
  df$Price<-df$Store_Sales/df$Quantity
  df_sales_daily<-df[,c("Date","Store_Sales")]
  df_quantity_daily<-df[,c("Date","Quantity")]
  df_price_daily<-df[,c("Date","Price")]
  mySeriesQuantity<-df_quantity_daily
  latestDemandDate<-as.Date(df[nrow(df),1])
  intervalDemand<-as.Date('2018-09-29')-latestDemandDate
  #ceci est a recuperer une fois avec de nouvelles donnees
  #intervalDemand<-as.Date(format(Sys.time(), "%Y-%m-%d"))-latestDemandDate
  m <- c()  
  for(i in 1:intervalDemand) {
    y <- 0
    m <- c(m, y)
  }
  
  new.df <- data.frame(Date=seq(max(df$Date) + 1, max(df$Date) + length(m), "days"), Quantity=m)
  new_df<-rbind(df_quantity_daily, new.df)
  new.df.sales <- data.frame(Date=seq(max(df$Date) + 1, max(df$Date) + length(m), "days"), Store_Sales=m)
  new_df_sales<-rbind(df_sales_daily, new.df.sales)
  new.df.price <- data.frame(Date=seq(max(df$Date) + 1, max(df$Date) + length(m), "days"), Price=m)
  new_df_price<-rbind(df_price_daily, new.df.price)
  
  z.price<- read.zoo( new_df_price, sep = ",", header = TRUE, index = 1:1, tz = "", format = "%Y-%m-%d")
  t <- time(z.price)
  t.daily <- seq(from = min(t), to=max(t),by="1 day")
  dummy <- zoo(,t.daily)
  z.interpolated.price <- base::merge(z.price,dummy,all=TRUE)
  date_ts<-index(z.interpolated.price)
  date_ts<-as.data.frame(date_ts)
  date_ts$Date<-as.Date(date_ts$date_ts, format = "%Y-%m-%d")
  date_ts$Date<-date_ts$Date+1
  
  mySeriesPrice<-ts(z.interpolated.price) 
  myY.price<-na.replace(z.interpolated.price , fill = 0)
  myY.price<-ts(myY.price)
  df_price<-as.data.frame(myY.price)
  
  z<- read.zoo(new_df, sep = ",", header = TRUE, index = 1:1, tz = "", format = "%Y-%m-%d")
  t <- time(z)
  t.daily <- seq(from = min(t), to=max(t),by="1 day")
  dummy <- zoo(,t.daily)
  z.interpolated <- base::merge(z,dummy,all=TRUE)
  date_ts<-index(z.interpolated)
  date_ts<-as.data.frame(date_ts)
  date_ts$Date<-as.Date(date_ts$date_ts, format = "%Y-%m-%d")
  date_ts$Date<-date_ts$Date+1
  
  mySeriesQuantity<-ts(z.interpolated) 
  myYY<-na.replace(z.interpolated , fill = 0)
  myY<-ts(myYY)
  myQuantity<-myY
  df_quantity<-as.data.frame(myQuantity)
  
  z.sales<- read.zoo(new_df_sales, sep = ",", header = TRUE, index = 1:1, tz = "", format = "%Y-%m-%d")
  t <- time(z.sales)
  t.daily <- seq(from = min(t), to=max(t),by="1 day")
  dummy <- zoo(,t.daily)
  z.interpolated.sales <- base::merge(z.sales,dummy,all=TRUE)
  mySeriesSales<-ts(z.interpolated.sales) 
  myYY.sales<-na.replace(z.interpolated.sales , fill = 0)
  myY.sales<-ts(myYY.sales, freq=7)
  mySales<-myY.sales
  df_sales<-as.data.frame(mySales)
  
  return(l<-list(salesDf=df, quantity=myQuantity,price=myY.price, salesTs=mySales, timeIndex=date_ts))
}

