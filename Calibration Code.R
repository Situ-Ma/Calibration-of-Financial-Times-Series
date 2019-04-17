install.packages("RQuantLib")
library(RQuantLib)

setwd("~/Desktop/UCLA/164 Project/Rcode/")
data=read.csv("AAPL_032018.csv")

#*****stage 1: Black Scholes Model
#historical volatility method
aapldaily=read.csv("AAPL.csv")

log_return=diff(log(aapldaily[,5]))
yearly_hist_sigmasq=1/length(log_return)*sum((log_return-mean(log_return))^2)
monthly_log_return=log_return[230:251]
monthly_hist_sigmasq=1/22*sum((monthly_log_return-mean(monthly_log_return))^2)
weekly_log_return=log_return[247:251]
weekly_hist_sigmasq=1/5*sum((weekly_log_return-mean(weekly_log_return))^2)

hist_sigmay=sqrt(yearly_hist_sigmasq*length(log_return))
hist_sigmam=sqrt(monthly_hist_sigmasq*length(log_return))
hist_sigmaw=sqrt(weekly_hist_sigmasq*length(log_return))

hist_sigmasq1=yearly_hist_sigmasq*length(log_return)
hist_sigmasq2=monthly_hist_sigmasq*length(log_return)
hist_sigmasq3=weekly_hist_sigmasq*length(log_return)

stock_price=data$Underlying_last_price
strike_price=data$Price_strike
data$days_to_maturity=as.numeric(as.Date(as.character(data$Option_expiration),format="%m/%d/%Y")-as.Date(as.character(data$Trade_date),format="%m/%d/%Y"))
maturity=as.numeric(data$days_to_maturity)/365
interest_rate=0

#calculate the option price based on yearly volatility
d1=1/sqrt(hist_sigmasq1)/sqrt(maturity)*(log(stock_price/strike_price)+(interest_rate+hist_sigmasq1/2)*(maturity))
d2=d1-sqrt(hist_sigmasq1)*sqrt(maturity)
optionprice1Cy=pnorm(d1,0,1)*stock_price-pnorm(d2,0,1)*strike_price*exp(-interest_rate*(maturity))
optionprice1Py=pnorm(-d2,0,1)*strike_price*exp(-interest_rate*(maturity))-pnorm(-d1,0,1)*stock_price

for(i in 1:length(data$Option_trade_price)){
  if(data$Call_Put[i]=="P"){
    data$optionprice1y[i]=optionprice1Py[i]
    data$option_type[i]="put"
  } else if (data$Call_Put[i]=="C"){
    data$optionprice1y[i]=optionprice1Cy[i]
    data$option_type[i]="call"
  }
}

par(mfrow=c(1,2))
plot(data$optionprice1y-as.numeric(data$Option_trade_price),type="l",xlab="",ylab="difference",main="Difference of option price, yearly historical volatility")
h<-hist((data$optionprice1y-as.numeric(data$Option_trade_price))/as.numeric(data$Option_trade_price), breaks=40, col="white", xlab="difference percentage, yearly volatility", 
        main="Histogram of difference in option price") 

mean(data$optionprice1y-as.numeric(data$Option_trade_price))
var(data$optionprice1y-as.numeric(data$Option_trade_price))
#MSE
mse=sum((data$optionprice1y-as.numeric(data$Option_trade_price))^2)/length(data$Option_trade_price)

#calculate the option price based on monthly volatility
d1=1/sqrt(hist_sigmasq2)/sqrt(maturity)*(log(stock_price/strike_price)+(interest_rate+hist_sigmasq2/2)*(maturity))
d2=d1-sqrt(hist_sigmasq2)*sqrt(maturity)
optionprice1Cm=pnorm(d1,0,1)*stock_price-pnorm(d2,0,1)*strike_price*exp(-interest_rate*(maturity))
optionprice1Pm=pnorm(-d2,0,1)*strike_price*exp(-interest_rate*(maturity))-pnorm(-d1,0,1)*stock_price

for(i in 1:length(data$Option_trade_price)){
  if(data$Call_Put[i]=="P"){
    data$optionprice1m[i]=optionprice1Pm[i]
  } else if (data$Call_Put[i]=="C"){
    data$optionprice1m[i]=optionprice1Cm[i]
  }
}

par(mfrow=c(1,2))
plot(data$optionprice1m-as.numeric(data$Option_trade_price),type="l",xlab="",ylab="difference",main="Difference of option price, monthly historical volatility")
h<-hist((data$optionprice1m-as.numeric(data$Option_trade_price))/as.numeric(data$Option_trade_price), breaks=40, col="white", xlab="difference percentage, monthly volatility", 
        main="Histogram of difference in option price") 

mean(data$optionprice1m-as.numeric(data$Option_trade_price))
var(data$optionprice1m-as.numeric(data$Option_trade_price))
#MSE
mse=sum((data$optionprice1m-as.numeric(data$Option_trade_price))^2)/length(data$Option_trade_price)

#calculate the option price based on weekly volatility
d1=1/sqrt(hist_sigmasq3)/sqrt(maturity)*(log(stock_price/strike_price)+(interest_rate+hist_sigmasq3/2)*(maturity))
d2=d1-sqrt(hist_sigmasq3)*sqrt(maturity)
optionprice1Cw=pnorm(d1,0,1)*stock_price-pnorm(d2,0,1)*strike_price*exp(-interest_rate*(maturity))
optionprice1Pw=pnorm(-d2,0,1)*strike_price*exp(-interest_rate*(maturity))-pnorm(-d1,0,1)*stock_price

for(i in 1:length(data$Option_trade_price)){
  if(data$Call_Put[i]=="P"){
    data$optionprice1w[i]=optionprice1Pw[i]
  } else if (data$Call_Put[i]=="C"){
    data$optionprice1w[i]=optionprice1Cw[i]
  }
}

par(mfrow=c(1,2))
plot(data$optionprice1w-as.numeric(data$Option_trade_price),type="l",xlab="",ylab="difference",main="Difference of option price, weekly historical volatility")
h<-hist((data$optionprice1w-as.numeric(data$Option_trade_price))/as.numeric(data$Option_trade_price), breaks=40, col="white", xlab="difference percentage, weekly volatility", 
        main="Histogram of difference in option price") 

mean(data$optionprice1w-as.numeric(data$Option_trade_price))
var(data$optionprice1w-as.numeric(data$Option_trade_price))
#MSE
mse=sum((data$optionprice1w-as.numeric(data$Option_trade_price))^2)/length(data$Option_trade_price)

#calculate under and over estimate
overestimateHVy0.1=sum((data$optionprice1y-as.numeric(data$Option_trade_price))/as.numeric(data$Option_trade_price)>0.1)
overestimateHVm0.1=sum((data$optionprice1m-as.numeric(data$Option_trade_price))/as.numeric(data$Option_trade_price)>0.1)
overestimateHVw0.1=sum((data$optionprice1w-as.numeric(data$Option_trade_price))/as.numeric(data$Option_trade_price)>0.1)

overestimateHVy0.05=sum((data$optionprice1y-as.numeric(data$Option_trade_price))/as.numeric(data$Option_trade_price)>0.05)
overestimateHVm0.05=sum((data$optionprice1m-as.numeric(data$Option_trade_price))/as.numeric(data$Option_trade_price)>0.05)
overestimateHVw0.05=sum((data$optionprice1w-as.numeric(data$Option_trade_price))/as.numeric(data$Option_trade_price)>0.05)

overestimateHVy0.01=sum((data$optionprice1y-as.numeric(data$Option_trade_price))/as.numeric(data$Option_trade_price)>0.01)
overestimateHVm0.01=sum((data$optionprice1m-as.numeric(data$Option_trade_price))/as.numeric(data$Option_trade_price)>0.01)
overestimateHVw0.01=sum((data$optionprice1w-as.numeric(data$Option_trade_price))/as.numeric(data$Option_trade_price)>0.01)

underestimateHVy0.1=sum((data$optionprice1y-as.numeric(data$Option_trade_price))/as.numeric(data$Option_trade_price)< -0.1)
underestimateHVm0.1=sum((data$optionprice1m-as.numeric(data$Option_trade_price))/as.numeric(data$Option_trade_price)< -0.1)
underestimateHVw0.1=sum((data$optionprice1w-as.numeric(data$Option_trade_price))/as.numeric(data$Option_trade_price)< -0.1)

underestimateHVy0.05=sum((data$optionprice1y-as.numeric(data$Option_trade_price))/as.numeric(data$Option_trade_price)< -0.05)
underestimateHVm0.05=sum((data$optionprice1m-as.numeric(data$Option_trade_price))/as.numeric(data$Option_trade_price)< -0.05)
underestimateHVw0.05=sum((data$optionprice1w-as.numeric(data$Option_trade_price))/as.numeric(data$Option_trade_price)< -0.05)

underestimateHVy0.01=sum((data$optionprice1y-as.numeric(data$Option_trade_price))/as.numeric(data$Option_trade_price)< -0.01)
underestimateHVm0.01=sum((data$optionprice1m-as.numeric(data$Option_trade_price))/as.numeric(data$Option_trade_price)< -0.01)
underestimateHVw0.01=sum((data$optionprice1w-as.numeric(data$Option_trade_price))/as.numeric(data$Option_trade_price)< -0.01)

overestimateHVy0.1
overestimateHVm0.1
overestimateHVw0.1

overestimateHVy0.05
overestimateHVm0.05
overestimateHVw0.05

overestimateHVy0.01
overestimateHVm0.01
overestimateHVw0.01

underestimateHVy0.1
underestimateHVm0.1
underestimateHVw0.1

underestimateHVy0.05
underestimateHVm0.05
underestimateHVw0.05

underestimateHVy0.01
underestimateHVm0.01
underestimateHVw0.01

#implied volatility method
real_option_price=data$Option_trade_price
implied_vol=0
for (i in 1:length(data$Option_trade_price)) {
  #ERROR HANDLING
  possibleError <- tryCatch({
    implied_vol[i]=EuropeanOptionImpliedVolatility(type=data$option_type[i], real_option_price[i],
                                                   stock_price[i], strike_price[i], 0, 0, maturity[i], -0.05)
  },
  error=function(e) e
  )
  
  if(inherits(possibleError, "error")) next
  
  #REAL WORK
  implied_vol[i]=EuropeanOptionImpliedVolatility(type=data$option_type[i], real_option_price[i],
                                                 stock_price[i], strike_price[i], 0, 0, maturity[i], -0.05)
  
}

data$implied_vol=implied_vol


h<-hist(implied_vol, breaks=40, col="white", xlab="Implied Volatility", 
        main="Histogram of Implied Volatility") 
imp_volfit<-seq(0,0.8,length=40) 
yfit<-dnorm(imp_volfit,mean=mean(implied_vol[!is.na(implied_vol)]),sd=sd(implied_vol[!is.na(implied_vol)])) 
yfit <- yfit*diff(h$mids[1:2])*length(implied_vol) 
lines(imp_volfit, yfit, col="red", lwd=2)

mean(implied_vol[!is.na(implied_vol)])
var(implied_vol[!is.na(implied_vol)])

#spline interpolation
maturities=unique(data$days_to_maturity)

#calculate maturity group
data$maturity_group=0
for (i in 1:length(data$days_to_maturity)){
  
  for (j in 1:length(maturities)){
    if (data$days_to_maturity[i]==maturities[j]){
      data$maturity_group[i]=j
    }
  }
}

#plot spline interpolation
par(mfrow=c(1,3))
for (i in 1:length(maturities)){
  jpeg(paste("IV plot for maturity",maturities[i],".jpeg"))
  if(maturities[i]==822){
    group=data[(data$maturity_group ==i) & data$Price_strike>200 & data$Price_strike<300,]
  } else if(maturities[i]==668){
    group=data[(data$maturity_group ==i) & data$Price_strike>200 & data$Price_strike<300 & !(data$Price_strike>230 & data$Price_strike<250),]
  }else if(maturities[i]==213){
    group=data[(data$maturity_group ==i) & data$Price_strike<200,]
  }else if(maturities[i]==59){
    group=data[(data$maturity_group ==i) & data$Price_strike>140 & data$Price_strike<250,]
  }else if(maturities[i]==241){
    group=data[(data$maturity_group ==i) & data$Price_strike>160,]
  }else if(maturities[i]==3){
    group=data[(data$maturity_group ==i) & data$Price_strike>140,]
  }else if(maturities[i]==458){
    group=data[(data$maturity_group ==i) & !(data$Price_strike>150 & data$Price_strike<190),]
  }else if(maturities[i]==38){
    group=data[(data$maturity_group ==i) & !(data$Price_strike>184 & data$Price_strike<194),]
  }else{
    group=data[(data$maturity_group ==i),]
  }
  
  
  with(data[(group$maturity_group ==i) ,], plot(spline(group$Price_strike, group$implied_vol,n=10000)$x,spline(group$Price_strike, group$implied_vol,n=10000)$y,xlab="strike price", ylab="implied volatility", main=paste("implied volatility for maturity",maturities[i],"days")))
  
  dev.off()
}

#plot 3-D sigma and K and T
install.packages("rgl")
library(rgl)
install.packages("plotly")
library(plotly)
plot_ly(x = data$Price_strike, y = data$days_to_maturity, z = data$implied_vol, col="skyblue")

install.packages("deldir")
library(deldir)


x <- data$Price_strike
y <-  data$days_to_maturity
z <- data$implied_vol

# Triangulate it in x and y
del <- deldir(x, y, z = z)
triangs <- do.call(rbind, triang.list(del))

# Plot the resulting surface
plot3d(x, y, z, type = "n",xlab="Strike Price",ylab="Maturity",zlab="Implied Volatility", main="3-D implied volatility")
triangles3d(triangs[, c("x", "y", "z")], col = "pink")


#SVM method to interpolate the volatility
install.packages("e1071")
library(e1071)

data$datetime=strptime(paste(as.character(data$Trade_date),as.character(data$Trade_time)), "%m/%d/%Y %H:%M:%S")

data$testdatagroup=as.numeric(difftime(data$datetime,data$datetime[1]))%%360>300

fdata=data[!is.na(data$implied_vol),]

testdataindex1=which(!is.na(match(diff(fdata$testdatagroup),1)))
testdataindex2=c(1,which(!is.na(match(diff(fdata$testdatagroup),-1))))

MSEnumbers<-numeric()
totnumbers<-numeric()
underestimate0.1<-numeric()
overestimate0.1<-numeric()
underestimatep0.1<-numeric()
overestimatep0.1<-numeric()

underestimate0.01<-numeric()
overestimate0.01<-numeric()
underestimatep0.01<-numeric()
overestimatep0.01<-numeric()

underestimate0.05<-numeric()
overestimate0.05<-numeric()
underestimatep0.05<-numeric()
overestimatep0.05<-numeric()

for(i in 1:length(testdataindex2)){
  
  traindata=cbind(fdata$implied_vol[testdataindex2[i]:testdataindex1[i]],fdata$days_to_maturity[testdataindex2[i]:testdataindex1[i]],fdata$Price_strike[testdataindex2[i]:testdataindex1[i]])
  testdata=cbind(fdata$implied_vol[testdataindex1[i]:testdataindex2[i+1]],fdata$days_to_maturity[testdataindex1[i]:testdataindex2[i+1]],fdata$Price_strike[testdataindex1[i]:testdataindex2[i+1]])
  
  x=traindata[,1]
  y=traindata[,-1]
  
  xp=testdata[,1]
  yp=testdata[,-1]
  
  vol_svm_model <- svm(x ~ . ,data=traindata)
  
  #estimate the future 1 minute volatility
  vol_pred<-predict(vol_svm_model,testdata)
  
  est_data=cbind(vol_pred,fdata$days_to_maturity[testdataindex1[i]:testdataindex2[i+1]],fdata$Price_strike[testdataindex1[i]:testdataindex2[i+1]],fdata$Underlying_last_price[testdataindex1[i]:testdataindex2[i+1]],fdata$option_type[testdataindex1[i]:testdataindex2[i+1]],fdata$Option_trade_price[testdataindex1[i]:testdataindex2[i+1]])
  
  est_opprice=0
  for(j in 1:nrow(est_data)){
    
    volatility=as.numeric(est_data[j,1])
    maturity=as.numeric(est_data[j,2])/365
    strikeprice=as.numeric(est_data[j,3])
    stockprice=as.numeric(est_data[j,4])
    type=est_data[j,5]
    
    d1=1/volatility/sqrt(maturity)*(log(stockprice/strikeprice)+(interest_rate+volatility^2/2)*(maturity))
    d2=d1-volatility*sqrt(maturity)
    
    if(type=="call"){
      est_opprice[j]=pnorm(d1,0,1)*stockprice-pnorm(d2,0,1)*strikeprice*exp(-interest_rate*(maturity))
      
    }else if(type=="put"){
      est_opprice[j]=pnorm(-d2,0,1)*strikeprice*exp(-interest_rate*(maturity))-pnorm(-d1,0,1)*stockprice
    }
  }
  
  #MSE of option price
  MSEnumbers=c(MSEnumbers, sum((est_opprice-as.numeric(est_data[,6]))^2)/length(est_opprice))
  totnumbers=c(totnumbers, length(est_opprice))
  #take 20% threshold
  overestimate0.1=c(overestimate0.1,sum((est_opprice-as.numeric(est_data[,6]))/as.numeric(est_data[,6])>0.1))
  underestimate0.1=c(underestimate0.1,sum((est_opprice-as.numeric(est_data[,6]))/as.numeric(est_data[,6])< -0.1))
  overestimatep0.1=c(overestimatep0.1,sum((est_opprice-as.numeric(est_data[,6]))/as.numeric(est_data[,6])>0.1)/length(est_opprice))
  underestimatep0.1=c(underestimatep0.1,sum((est_opprice-as.numeric(est_data[,6]))/as.numeric(est_data[,6])< -0.1)/length(est_opprice))
  
  #take 10% threshold
  overestimate0.01=c(overestimate0.01,sum((est_opprice-as.numeric(est_data[,6]))/as.numeric(est_data[,6])>0.01))
  underestimate0.01=c(underestimate0.01,sum((est_opprice-as.numeric(est_data[,6]))/as.numeric(est_data[,6])< -0.01))
  overestimatep0.01=c(overestimatep0.01,sum((est_opprice-as.numeric(est_data[,6]))/as.numeric(est_data[,6])>0.01)/length(est_opprice))
  underestimatep0.01=c(underestimatep0.01,sum((est_opprice-as.numeric(est_data[,6]))/as.numeric(est_data[,6])< -0.01)/length(est_opprice))
  
  #take 5% threshold
  overestimate0.05=c(overestimate0.05,sum((est_opprice-as.numeric(est_data[,6]))/as.numeric(est_data[,6])>0.05))
  underestimate0.05=c(underestimate0.05,sum((est_opprice-as.numeric(est_data[,6]))/as.numeric(est_data[,6])< -0.05))
  overestimatep0.05=c(overestimatep0.05,sum((est_opprice-as.numeric(est_data[,6]))/as.numeric(est_data[,6])>0.05)/length(est_opprice))
  underestimatep0.05=c(underestimatep0.05,sum((est_opprice-as.numeric(est_data[,6]))/as.numeric(est_data[,6])< -0.05)/length(est_opprice))
  
  
  plot(est_opprice,col="red",type="l",main=paste("prediction of group",i),xlab="",ylab="option price")
  lines(as.numeric(est_data[,6]),col="black",lty=3)
  legend("topleft",c("predict","market"),lty=c(3,1),col=c("red","black"))
}

MSEnumbers
totnumbers
underestimate0.1
overestimate0.1
underestimatep0.1
overestimatep0.1

underestimate0.01
overestimate0.01
underestimatep0.01
overestimatep0.01

underestimate0.05
overestimate0.05
underestimatep0.05
overestimatep0.05


#*****stage 2: jump diffusion model
install.packages("stats")
library(stats)

p=0.7 # relative probability of a positive jump
alphap=3 #expected positive jump size
alphan=0.1 #expected negative jump size
alpha=1.1 #mean of log jump size
lambda=3 #parameter of poisson process (jump intensity)
mu=0 #drift term of BS model (under risk neutral process is interest rate, here is 0)
sigma=0.02 # volatility term in black scholes model
w=-1/2*alpha^2-lambda*(p/(alphap+1)-(1-p)/(alphan-1))
n=1000
T=100
dedj.char<-function(mu,w,T,sigma,lambda,p,alphap,alphan) {
  exp(complex(real=0,imaginary=mu*w*T)-1/2*mu^2*sigma^2*T+lambda*T*(p/(alphap-complex(real=0,imaginary=mu))-(1-p)/(alphan+complex(real=0,imaginary=mu))))
}

density=Re(fft(dedj.char(mu,w,T,sigma,lambda,p,alphap,alphan)))
density=c(density[(n/2+1):n],density[1:(n/2)])


characteristic_function_to_density <- function(
  dedj_char, # characteristic function; should be vectorized
  n=1024,   # Number of points, ideally a power of 2
  a, b # Evaluate the density on [a,b[
) {
  i <- 0:(n-1)            # Indices
  dx <- (b-a)/n           # Step size, for the density
  x <- a + i * dx         # Grid, for the density
  dt <- 2*pi / ( n * dx ) # Step size, frequency space
  c <- -n/2 * dt          # Evaluate the characteristic function on [c,d]
  d <-  n/2 * dt          # (center the interval on zero)
  t <- c + i * dt         # Grid, frequency space
  phi_t <- dedj_char(t)
  X <- exp( -(0+1i) * i * dt * a ) * phi_t
  Y <- fft(X)
  density <- dt / (2*pi) * exp( - (0+1i) * c * x ) * Y
  data.frame(
    i = i,
    t = t,
    characteristic_function = phi_t,
    x = x,
    density = Re(density)
  )
}

d <- characteristic_function_to_density(
  function(mu,w=-3.470297,T=100,sigma=0.2,lambda=0.1,p=0.5,alphap=0.01,alphan=-0.01) 
    exp(complex(real=0,imaginary=mu*w*T)-1/2*mu^2*sigma^2*T+lambda*T*(p/(alphap-complex(real=0,imaginary=mu))-(1-p)/(alphan+complex(real=0,imaginary=mu))))
  
  ,2^8,
  -5, 8
)
plot(d$x, d$density, las=1)
curve(dnorm(x,1,2), add=TRUE)
