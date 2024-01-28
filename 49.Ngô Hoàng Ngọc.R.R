#Cac thu vien can dung:
library(fBasics)
library(fGarch)
library(rugarch)
library(quantmod)
library(tseries)
#So tien dau tu
Tien=10000000
#Mo du lieu
t=file.choose()
#Tien hanh doc du lieu
da=read.csv(t)
#Xem qua du lieu
View(da)
#Gan du lieu gia
pr=da[,2]
# Mo hinh tinh dung cua gia
plot(pr,type="l")
#Tinh loi nhuan ngay cua HPG
r=diff(log(pr))
#Thong ke co ban ve loi nhuan HPG
basicStats(r)
#Bieu do Histogram loi nhuan dang line
den=density(r)
plot(den,type="l")
#Kiem dinh Normal
normalTest(r)
#Kiem dinh Adf
adf.test(r)
#kiem dinh kpss
kpss.test(r)
#So do kiem tra Tu tuong quan
acf(r)
#kiem tra tu tuong quan voi do tre bang 10
Box.test(r,lag=10)
#kiem tra tu tuong quan voi do tre bang 10, ljung
Box.test(r,lag=10,type="Ljung-Box")
#% thua lo cua HPG
nr=-r
#Chi dinh thu muc co Rmeasure
setwd("C:/Users/Admin/Desktop/PTRRTC/RMeasure")
source("RMeasure.R")
#Mo hinh risk metrics
spec1 = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(model = "iGARCH", garchOrder=c(1,1)))
m1=ugarchfit(spec=spec1,data=nr)
m1
ugarchforecast(m1,n.ahead=1)
#Tinh Var1 thu cong
v1=0+qnorm(0.99)*0.02444
v1
Var1= Tien*v1
Var1
#Tinh Var1 bang Rmeasure
RMeasure(0, 0.02444)
Var1_RM=Tien*0.05685594
Var1_RM
#chay mo hinh econometric
spec2 = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(model= "fGARCH", submodel="GARCH", garchOrder=c(1,1)))
m2= garchFit(~arma(0,0) + garch(1,1), data = nr, trace = F)
m2
#Du doan
predict(m2,1)
#Tinh Var thu cong
v2=-0.002206848+qnorm(0.99)*0.02258973
v2
Var2=Tien*v2
Var2
#Tinh bang RMeasure
RMeasure(-0.002206848,0.02258973 )
Var2_RM=Tien*0.05034472
Var2_RM
#phuong phap quantile
v3=-quantile(nr,0.01)
v3
Var3=Tien*v3
Var3
#phuong phap Monte Carlos Simulation
set.seed(246810)
mu=mean(nr)
sigma=stdev(nr)
sim=rnorm(100000,mean=mu,sd=sigma)
v4=-quantile(sim,0.01)
v4
Var4=Tien*v4
Var4