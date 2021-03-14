
plotProductDemand<-function(customer,  productVariants){
  
  sales<-read_from_csv(customer, "sales")
  list<-return_preprocessed_ts_product(sales,productVariants)
  myQuantity<-list$quantity
  #date_ts<-list.df$timeIndex
  #df_quantity<-as.data.frame(myQuantity)
  #dataframe_quantity <- with(df_quantity,data.frame(quantity=df_quantity$x))
  #dataframe_quantity$Date <- date_ts$Date
  return(myQuantity)
  
}


plotSalesHeatMap<-function(customer){
  
  sales<-read_from_csv(customer, "sales")
  df<-return_preprocessed_ts_store(sales)
  df_sales_daily<-df[,c("Date","Store_Sales", "Quantity","Yr", "Month", "day")]
  mySeries<-df_sales_daily[-c(1),]
  heatMap_1 <- ggplot(data = mySeries, aes(x =Yr, y = Month, fill = Store_Sales)) +
    geom_tile(colour = "white")  + scale_fill_gradient(low="red", high="yellow")
  
  heatMap_2<- ggplot(data = mySeries, aes(x =day, y = Month, fill = Store_Sales)) + 
    geom_tile(colour = "white")+  facet_grid(Yr~Month)+ scale_fill_gradient(low="red", high="yellow") 
  return(l<-list(heatMap_1=heatMap_1,heatMap_2=heatMap_2))
}