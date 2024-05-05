df<-read.csv("D:\\datasets\\stocks_ida.csv")
head(df)
library(ggplot2)
library(zoo)
library(tseries)
library(forecast)
library(tidyverse)
#finding the null values in dataset

df$Date<-as.Date(df$Date)#Converting the date time into the date time object
str(df)#It tells about the data types of data set


#function declaration
train_model<-function(x,y,a_o){
  p=NULL
  h=x
  for (t in seq(1,length(y))){
    model=arima(h,order = a_o,method = 'ML')
    ff<-predict(model,n.ahead = 1)
    yp=ff$pred[1]
    p<-c(p,yp)
    h=c(h,y[t])
    
  }
  p<-c(x,p)
  return (p)
}

evaluate_model<-function(data,test,p_val,d_val,q_val){
  bs=1000
  bcg=NULL
  for (p in p_val){
    for (d in d_val){
      for (q in q_val){
        or=c(p,d,q)
        rmse=train_model(data,test,or)
        if (rmse<bs){
          bs=rmse
          bcg=or
        }
      }
    }
  }
  print(bcg)
}

summary_df<-function(df1){
  mini<-12345
  maxi<-0
  for(i in seq(1,length(df1))){
    if(mini>df1[i]){
      mini<-df1[i]
    }else{}
    if(maxi<df1[i]){
      maxi<-df1[i]
    }else{}
  }
  s=0
  for(i in seq(1,length(df1))){
    s<-s+df[i]
  }
  means<-s/length(df1)
  if(length(df1)%%2==0){
    med<-(df1[length(df1)/2]+df1[(length(df1)/2)+1])/2
  }else{
    med<-df1[(length(df1)+1)/2]
  }
  dd<-data.frame(min=mini,first_quartile=quantile(df1,probs=0.25),median=med,third_quantile=quantile(df1,probs = 0.75),max=maxi)
print(dd)
return (dd)
  }

#EDA related function
cal_fun<-function(dat,colnam,df1){
  
  zoo_data <- zoo(dat, order.by = df1$Date)
  
  # Calculate the rolling mean for a window of 45 days
  #calculate the mean of a rolling window of width 45 for a zoo_data object, aligning the window to the right and filling missing values with NA.
  rolling_mean <- rollapply(zoo_data, width = 45, align = "right",FUN = mean,fill=NA)
  
  
  rolling_std <- rollapply(zoo_data, width  = 45, FUN = sd, align = "right",fill=NA)
  #rolling_mean_data <- data.frame(Date = index(rolling_mean), RollingMean = na.omit(coredata(rolling_mean)),rstd=na.omit(coredata(rolling_std)),close=df1$Close)
  
  rolling_mean_data <- data.frame(Date = index(rolling_mean), RollingMean = rolling_mean,rstd=rolling_std,close=df1$Close)
  
  
  # Create a line plot with ggplot2
  print(ggplot(data = na.omit(rolling_mean_data), aes(x = Date,group=1)) +
          geom_line(aes(y = RollingMean), color = "blue", linetype = "solid",size=1) +
          geom_line(aes(y = rstd), color = "red", linetype = "solid",size=1)+geom_line(aes(y = close), color = "green", size = 1) +
          scale_color_manual(values = c("Close Price" = "green", "Rolling Mean" = "blue", "Rolling Std" = "red")))
  
  
  
  #checking for stationary
  tse<-ts(df1[,colnam],frequency = 1)
  
  adf_result<-adf.test(dat)
  
  if(adf_result[4]<0.05){kk='st'
  print("it is stationary")
  tso<-ts(dat,frequency=12)
  decomposed_result<-decompose(tso,type = 'multiplicative')
  }else{kk='nst'
  print('non-stationary')
  tso<-ts(dat,frequency=12)
  
  
  #identifying the trends in data
  decomposed_result<-decompose(tso,type = 'multiplicative')
  trend_component <- decomposed_result$trend
  seasonal_component <- decomposed_result$seasonal
  remainder_component <- decomposed_result$random
  
  # Plot the components
  par(mfrow = c(3, 1))  # Arrange plots in a 3x1 grid
  print(plot(df1$Date,trend_component, main = "Trend Component"))
  print(plot(df1$Date,seasonal_component, main = "Seasonal Component"))
  print(plot(df1$Date,remainder_component, main = "Residual Component"))
  par(mfrow = c(1, 1)) 
  }

  plot(decomposed_result)
  
  acf_res <- acf(tso)
  
  # Plot ACF
  print(plot(acf_res, main = "Autocorrelation Function (ACF)"))
  
  pacf_res<-pacf(tso)
  print(plot(pacf_res, main = "pacf"))
  return(kk)
}


#using the ETA model



#EDA


overall_EDA_univariate<-function(data,coln,comp){

  
  #some knowledge about the data
  da<-data[data$company==comp,]
  print(summary(da))
  
  da$Date<-as.Date(da$Date)
  df1<-da[,c('Date',coln)]
  print(str(da))
  
  #normal plot (Close vs time)
  print(ggplot(data=df1,aes(y=df1[,coln],x=df1[,'Date'],group=1))+geom_line()+labs(x = "Date", y = "Close Price", title = "Stock Close Price Over Time"))
  
  #check for normality
  print(ggplot(data=df1,aes(x=df1[,coln]))+geom_histogram(binwidth = 10,fill='green'))#histogram
  print(ggplot(data=df1,aes(df1[,coln]))+geom_density(size=2,color="red"))#density plot
  print(qqplot(df1$Date,df1$Close))#qqplot
  
  n_res<-shapiro.test(x = df1$Close)
  if(n_res$p.value>0.05){
    print("It follows normality")
  }else{
    print("it does not follows normality")
  }
  
  #check the outliers visually and if present remove 
  
  print(boxplot(df1[,coln]))#boxplot
  
  q1 <- quantile(df1[,coln], 0.25)
  q3 <- quantile(df1[,coln], 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  n_outliers<-sum(df1[,coln] < lower_bound | df1[,coln] > upper_bound)#number of the outliers
  
  
  if(n_outliers>0){
    print('There are outliers')
    df1<-df1[df1[,coln] >= lower_bound | df1[,coln] <= upper_bound,]#removing the outliers if present
  }else{
    print('There are no outliers')
  }
  

  
  lak<-cal_fun(df1[,coln],coln,df1)
  h1<-0
  train_dat<-df1[1:as.integer(nrow(df1)*0.8),]
  test_dat<-df1[as.integer((nrow(df1)*0.8)+1):nrow(df1),]
  
  
  
  plot(train_dat, type = "l", col = "blue", xlab = "Date", ylab = "Closing price", main = "Train_test curve")
  lines(test_dat,col='red')
  for(h1 in seq(1,6)){
    
    if(lak=='nst'){
      ndat<-diff(df1[,coln],h1)
      ndat<-na.omit(ndat)
      print('stt')
      lak<-cal_fun(ndat,coln,df1)
      if(lak=='st'){print(lak)
        break;
      }
    }}
  print('h1:')
  print(h1)
  kk<-auto.arima(as.numeric(train_dat[,'Close']),trace=T,stepwise=F,approximation=F)
  orde<-arimaorder(kk)
  mod<-arima(as.numeric(train_dat$Close),order =orde)

  p<-orde[1]
  df11<-data
  for (i in 1:p) {
    for (col in 'Close') {
      df11 <- df11 %>%
        mutate(!!paste0(col, 'lag', i) := lag(!!sym(col), i))
    }
  }
  

  df11 <- na.omit(df11)
  

  columns_for_analysis <- 'Close'
  

  columns_selected <- grep(paste0(columns_for_analysis, collapse="|"), names(df11), value=TRUE)
  

  cov_matrix <- cov(df11[, columns_selected])
  print(cov_matrix)
  
  total_with_prediction<-train_model(train_dat$Close,test_dat$Close,orde)
  plot(df1$Date,total_with_prediction,col='green',xlab='Time',ylab='Price',main=paste('comparision between the predicted price and exact stock close price',comp))
  lines(df1$Date,df1[,coln],col="black")
  legend("topright", legend = c("Predicted Price", "Exact Stock Close Price"), col = c("green", "black"), lty = 1)
  print(total_with_prediction[1])
  plot(df1[as.integer((length(total_with_prediction)*0.8)+1):length(total_with_prediction),'Date'],total_with_prediction[as.integer((length(total_with_prediction)*0.8)+1):length(total_with_prediction)],xlab='Time',ylab='Predicted close price',main= paste('Predicted stock Price  for', comp))
  lines(df1[as.integer((length(total_with_prediction)*0.8)+1):length(total_with_prediction),'Date'],total_with_prediction[as.integer((length(total_with_prediction)*0.8)+1):length(total_with_prediction)],xlab='Time',ylab='Predicted close price',main= paste('Predicted stock Price  for', comp))


  print(accuracy(total_with_prediction[1:length(total_with_prediction)], df1[1:length(total_with_prediction),'Close']))

  }



temp_fun<-function(df,coln,comp){
  df1<-df[df$company==comp,]
  train_dat<-df1[1:as.integer(nrow(df1)*0.8),]
  test_dat<-df1[as.integer((nrow(df1)*0.8)+1):nrow(df1),]
  kk<-auto.arima(as.numeric(train_dat[,'Close']),trace=T,stepwise=F,approximation=F)
  orde<-arimaorder(kk)
  mod<-arima(as.numeric(train_dat$Close),order =orde)
  total_with_prediction<-train_model(train_dat$Close,test_dat$Close,orde)
 # plot(df1$Date,total_with_prediction,col='green',xlab='Time',ylab='Price',main=paste('comparision between the predicted price and exact stock close price',comp))
  #lines(df1$Date,df1[,coln],col="black")
  #legend("topright", legend = c("Predicted Price", "Exact Stock Close Price"), col = c("green", "black"), lty = 1)
  print(total_with_prediction[1])
  plot(df1[as.integer((length(total_with_prediction)*0.8)+1):length(total_with_prediction),'Date'],total_with_prediction[as.integer((length(total_with_prediction)*0.8)+1):length(total_with_prediction)],xlab='Time',ylab='Predicted close price',main= paste('Predicted stock Price  for', comp))
  
}



=

myets <- ets(lynx)

etsfore <- forecast(myets, h = 10)

# Comparison plot for 2 models




overall_EDA_univariate(df,'Close','apple')
overall_EDA_univariate(df,'Close','TCS')
overall_EDA_univariate(df,'Close','tesla')
overall_EDA_univariate(df,'Close','dr reddy lab')
overall_EDA_univariate(df,'Close','abott')
overall_EDA_univariate(df,'Close','IBM')
overall_EDA_univariate(df,'Close','nvdia')
overall_EDA_univariate(df,'Close','google')
overall_EDA_univariate(df,'Close','accenture')
overall_EDA_univariate(df,'Close','micro soft')
overall_EDA_univariate(df,'Close','amazon')
overall_EDA_univariate(df,'Close','Hp')


#for multivariate
library(tseries)
library(MTS)
lak<-df[df$company=='apple',]
apply(lak[,3:ncol(lak)-1],2,adf.test)
st<-diffM(lak[,3:ncol(lak)-1])
apply(st,2,adf.test)
