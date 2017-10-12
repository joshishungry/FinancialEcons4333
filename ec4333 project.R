options(stringsAsFactors = F)

rm(list=ls())

library(quantmod)
library(reshape2)
library(ggplot2)
library(forecast)

####################################
### Section 1: Prepare Data

data_to_download <- c("DGS1MO","DGS3MO","DGS6MO",
                      "DGS1","DGS2","DGS3","DGS5",
                      "DGS7","DGS10","DGS20","DGS30")

getSymbols(data_to_download, src="FRED")

for (i in data_to_download){assign(i,to.weekly(get(i))[,4])}

usYields <- merge(DGS1MO,DGS3MO,DGS6MO,DGS1,DGS2,
                    DGS3,DGS5,DGS7,DGS10,DGS20,DGS30); #saveRDS(usYields, "usYields.rds") 

usYields <- readRDS("usYields.RDS") #remove this for submission

#remove rows with more than 4 NAs (thus, each row has at least 7 datapoints to fit)
usYields <- usYields[rowSums(is.na(usYields))<4,]


maturity_months <-c(1,3,6,12,24,36,60,84,120,240,360) 

colnames(usYields)<-maturity_months

summary(usYields)


##############################################
###### Section 2: Calculate optimal lambda

medium_term_factor <- function (lambda, time_maturity = 30*30.4365/31){
  (1-exp(-lambda*time_maturity))/(lambda*time_maturity)-exp(-lambda*time_maturity)
}

optimal_lambda <- optimize(medium_term_factor,time_maturity = 30*30.4365/31, lower = 0, upper = 10, maximum = T)

lambda_value <- optimal_lambda$maximum

plot(medium_term_factor,xlim=c(0, 0.5))
abline(v=lambda_value);legend("topright",legend=paste("lambda=",signif(lambda_value,3)))

######################################
##### Section 3: Function for Diebold and Li Estimation

dl_estimator<- function(dataset, maturity,lambda){
  lm(formula = dataset ~ I((1-exp(-lambda*maturity))/(lambda*maturity)) + 
       I((1-exp(-lambda*maturity))/(lambda*maturity) - exp(-lambda*maturity)))
}

diebold_li_estimation_function<- function(dataset, maturity,lambda) 
{
  FinalResults <- matrix(0, nrow(dataset), 3)
  colnames(FinalResults) <- c("beta_1", "beta_2", "beta_3")
  
  j <- 1
  pb = txtProgressBar(min = 0, max = length(dataset),  initial = 0) 
  while (j <= nrow(dataset)) {
    
    beta<- dl_estimator(as.numeric(dataset[j,]), maturity,lambda)
    FinalResults[j, ] <- coef(beta)
   
      setTxtProgressBar(pb,j)
    j = j + 1
        } 
  reclass(FinalResults, dataset)
}



#Derive Beta estimates in time series
betas_estimate <- diebold_li_estimation_function(dataset = usYields,
                                                   maturity = maturity_months, 
                                                   lambda = lambda_value)



######################################
##### Section 4: Plot historical beta time series (recreate plots on 350)

betas_estimate_diebold <- window(betas_estimate, start="1985-01-01", end = "1999-12-31")

plot(betas_estimate_diebold$beta_1)
plot(-betas_estimate_diebold$beta_2)
plot(0.3*betas_estimate_diebold$beta_3)

ggplot(betas_estimate_diebold) + geom_line(aes( x=index(betas_estimate_diebold), y =beta_1))


summary(betas_estimate_diebold)


acf(betas_estimate_diebold$beta_1,lag.max=60)
acf(betas_estimate_diebold$beta_2,lag.max=60)
acf(betas_estimate_diebold$beta_3,lag.max=60)


######################################
##### Section X:  forecast for betas

period_forecast <- 12

start_window = "2001-01-01"
end_window = "2008-01-01"

betas_estimate_psuedo <- window(betas_estimate, start = start_window, end = end_window) #betas_estimate[1:(nrow(betas_estimate)-period_forecast),]

for (i in colnames(betas_estimate_psuedo)){
  assign(paste0("f_",i), ar.ols(betas_estimate_psuedo[,i], demean=FALSE, intercept=TRUE, order.max=1))
  assign(paste0("predicted_",i), forecast(get(paste0("f_",i)), h=period_forecast))
  print(get(paste0("predicted_",i)))
  
}

predicted_beta_all <- ts.intersect(predicted_beta_1$mean,predicted_beta_2$mean,predicted_beta_3$mean)

predicted_beta_all <- data.frame(predicted_beta_all)

row.names(predicted_beta_all) <- seq.Date(index(window(betas_estimate,start=start_window)[nrow(betas_estimate_psuedo)+1]),
                                          length.out = period_forecast, by="week")


predicted_beta_all <- as.xts(predicted_beta_all)


fit_estimates <- function (Coeff, maturity) {
  Curve <- xts(matrix(0, nrow(Coeff), length(maturity)), order.by = time(Coeff))
  colnames(Curve) <- maturity_months
  Coeff <- as.matrix(Coeff)
  for (i in 1:nrow(Curve)) {
    Curve[i, ] <- as.numeric(Coeff[i, 1]) * rep(1, length(maturity)) + 
      as.numeric(Coeff[i, 2]) * (1-exp(-lambda_value*maturity))/(lambda_value*maturity) + 
      as.numeric(Coeff[i, 3]) * ( (1-exp(-lambda_value*maturity))/(lambda_value*maturity) - exp(-lambda_value*maturity) )
  }
  return(Curve)
}

estimate_yield <- fit_estimates(predicted_beta_all, maturity_months)


error <- (data.frame(estimate_yield, check.names=F) - 
            data.frame(window(usYields, start = paste(index(estimate_yield)[1]), end = paste(index(estimate_yield)[nrow(estimate_yield)])), check.names = F)
)

RMSE <- sqrt(apply(error^2,2,sum,na.rm=T)/nrow(error))
bias <- apply(error,2,mean)


######################################
##### Section 5: Plot yield quantiles  (recreate plots on 346)

usYields_quantile <- apply(last(usYields, '20 years'),2,quantile,seq(from=0.25,to=0.75,by=0.25), na.rm=T)

colnames(usYields_quantile) <- maturity_months

quantile_betas <- diebold_li_estimation_function(dataset = usYields_quantile,
                                                 maturity = maturity_months, 
                                                 lambda = lambda_value)


ggplot(melt(usYields_quantile)) + geom_point(aes(x=Var2,y=value, color=Var1)) +
  geom_smooth(aes(x=Var2,y=value, color=Var1), method="lm",
              formula = y ~ I((1-exp(-lambda_value*x))/(lambda_value*x)) + 
                I((1-exp(-lambda_value*x))/(lambda_value*x) - exp(-lambda_value*x)),se=F ) +
  labs(list(y = "Yield (percent)", x = "Time to Maturity", title = "Quantiles"))



######################################
##### Section 6: Plot individual yield quantiles  (recreate plots on 348)

usYields_long <- dcast(melt(data.frame(date=index(usYields), coredata(usYields), check.names=F), id.var="date"), 
                       variable ~ date, value.var="value")


usYields_long$variable <- as.numeric(levels(usYields_long$variable)[usYields_long$variable])
#usYields_long <- usYields_long[order(usYields_long$ind),]

individual_date_plot <- function(df = usYields_long, date_plot=date_to_plot){
  print(df[,c("variable",date_to_plot)])
  ggplot(df) + geom_point(aes(x=variable, y=get(date_plot))) +
    geom_smooth(aes(x=variable,y=get(date_plot)), method="lm",
                formula = y ~ I((1-exp(-lambda_value*x))/(lambda_value*x)) + 
                  I((1-exp(-lambda_value*x))/(lambda_value*x) - exp(-lambda_value*x)),se=F ) +
    scale_x_continuous(breaks = seq(0,360,20)) +
    labs(list(y = "Yield (percent)", x = "Time to Maturity", title = paste("Yield Curve on",date_plot)))
 }


date_to_plot <- "2015-01-02"    

individual_date_plot(df = usYields_long, date_plot=date_to_plot)



check_fitting <- as.data.frame(matrix(coef(dl_estimator(usYields_long[,date_to_plot], maturity_months, lambda_value)), nrow=1))
colnames(check_fitting) <- c("beta1","beta2","beta3")

values_check <-  as.numeric(check_fitting[1, 1]) * rep(1, length(maturity_months)) + 
  as.numeric(check_fitting[1, 2]) * (1-exp(-lambda_value*maturity_months))/(lambda_value*maturity_months) + 
  as.numeric(check_fitting[1, 3]) * ( (1-exp(-lambda_value*maturity_months))/(lambda_value*maturity_months) - exp(-lambda_value*maturity_months) )


ggplot(usYields_long) + geom_point(aes(x=variable, y=get(date_to_plot))) +
  geom_smooth(aes(x=variable,y=get(date_to_plot)), method="lm",
              formula = y ~ I((1-exp(-lambda_value*x))/(lambda_value*x)) + 
                I((1-exp(-lambda_value*x))/(lambda_value*x) - exp(-lambda_value*x)),se=F ) +
  scale_x_continuous(breaks = seq(0,360,20)) + scale_y_continuous(breaks = seq(0,2.5,0.1)) +
  labs(list(y = "Yield (percent)", x = "Time to Maturity", title = paste("Yield Curve on",date_to_plot))) + 
  geom_vline(aes(xintercept=maturity_months)) + geom_hline(aes(yintercept=values_check))

##########################################
######## Section 7: Conduct Forecast

#naive AR forecast

period_forecast <- 4


last_date <- NULL
for (i in ls(pattern="DGS")[nchar(ls(pattern="DGS"))<=7]){
  if (i != "DGS1MO" | i != "DGS30" ){
  assign(paste0("pred_",i),ar.ols(window(get(i),start=start_window,end=end_window), 
                                na.action = na.exclude, demean=FALSE, intercept=TRUE, order.max=1))
  assign(paste0("forecast_",i), forecast(get(paste0("pred_",i)),h = period_forecast)) 
  last_date <- as.Date(unique(c(last_date, index(window(get(i),start=start_window))[length(window(get(i),start=start_window,end=end_window))+1])))
  if (length(last_date) >=2){warning("2 last dates")}
  }
}

ar_estimate_yield <- data.frame(ts.intersect(forecast_DGS3MO$mean,forecast_DGS6MO$mean,forecast_DGS1$mean, 
                                  forecast_DGS2$mean, forecast_DGS3$mean, forecast_DGS5$mean, 
                                  forecast_DGS7$mean, forecast_DGS10$mean,forecast_DGS20$mean))

row.names(ar_estimate_yield) <- seq.Date(last_date,length.out = period_forecast, by="week")


error <- ar_estimate_yield - data.frame(window(usYields, start = last_date, 
                               end = row.names(ar_estimate_yield)[nrow(ar_estimate_yield)]))[,c(-1,-ncol(usYields))]
  

RMSE <- sqrt(apply(error^2,2,sum,na.rm=T)/nrow(error))
bias <- apply(error,2,mean)



#####################################
######## AR(p) diebold li

period_forecast <- 12

start_window = "2001-01-01"
end_window = "2008-01-01"

betas_estimate_psuedo <- window(betas_estimate, start = start_window, end = end_window) #betas_estimate[1:(nrow(betas_estimate)-period_forecast),]

for (i in colnames(betas_estimate_psuedo)){
  assign(paste0("f_",i), ar.ols(betas_estimate_psuedo[,i], demean=FALSE, intercept=TRUE))
  assign(paste0("predicted_",i), forecast(get(paste0("f_",i)), h=period_forecast))
  print(get(paste0("predicted_",i)))
  
}

predicted_beta_all <- ts.intersect(predicted_beta_1$mean,predicted_beta_2$mean,predicted_beta_3$mean)

predicted_beta_all <- data.frame(predicted_beta_all)

row.names(predicted_beta_all) <- seq.Date(index(window(betas_estimate,start=start_window)[nrow(betas_estimate_psuedo)+1]),
                                          length.out = period_forecast, by="week")


predicted_beta_all <- as.xts(predicted_beta_all)


fit_estimates <- function (Coeff, maturity) {
  Curve <- xts(matrix(0, nrow(Coeff), length(maturity)), order.by = time(Coeff))
  colnames(Curve) <- maturity_months
  Coeff <- as.matrix(Coeff)
  for (i in 1:nrow(Curve)) {
    Curve[i, ] <- as.numeric(Coeff[i, 1]) * rep(1, length(maturity)) + 
      as.numeric(Coeff[i, 2]) * (1-exp(-lambda_value*maturity))/(lambda_value*maturity) + 
      as.numeric(Coeff[i, 3]) * ( (1-exp(-lambda_value*maturity))/(lambda_value*maturity) - exp(-lambda_value*maturity) )
  }
  return(Curve)
}

estimate_yield <- fit_estimates(predicted_beta_all, maturity_months)


error <- (data.frame(estimate_yield, check.names=F) - 
            data.frame(window(usYields, start = paste(index(estimate_yield)[1]), end = paste(index(estimate_yield)[nrow(estimate_yield)])), check.names = F)
)

RMSE <- sqrt(apply(error^2,2,sum,na.rm=T)/nrow(error))
bias <- apply(error,2,mean)











###############

names_predicted <- c(NULL)
for (i in colnames(usYields)){
  if (i != "1" | i != "360" ){
    assign(paste0("f_",i), ar.ols(na.omit(usYields[,i]), na.action = na.exclude, demean=FALSE, intercept=TRUE, order.max=1))
    assign(paste0("predicted_",i), forecast(get(paste0("f_",i)), h=period_forecast))
    #  print(get(paste0("predicted_",i)))
    names_predicted <- c(names_predicted, paste0("predicted_",i))
  }

