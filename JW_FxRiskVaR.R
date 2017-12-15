##FX risk
install.packages("Quandl")
install.packages("gdata")
suppressPackageStartupMessages(library(gdata)) ##Import from Excell
suppressPackageStartupMessages(library(Quandl))

Quandl.api_key("Vgee32tr5JRhe8FzJBwy")

#Download Exposures
FxExp=read.xls("C:\\Users\\....\\....\\Documents\\FX_Risk.xlsx",sheet="Data")
FxExp[,"Exposure"]=as.numeric(FxExp[,"Exposure"])

#Download risk factors
FxRate=Quandl(c("ECB/EURUSD","ECB/EURGBP","ECB/EURCHF","ECB/EURJPY","ECB/EURAUD","ECB/EURCAD"),start_date='2014-01-01',end_date='2016-01-01')

n=length(FxRate[,1])
m=length(FxRate[1,])

#Clean risk factors
colnames(FxRate)=c("Date",as.vector(FxExp[,"Currency"]))

#weights calculation
w=FxExp[,"Exposure"]/sum(abs(FxExp[,"Exposure"]))

#Daily Return calculation
FxRateR=data.frame(Date=FxRate[2:n,1],log(FxRate[2:n,2:m]/FxRate[1:(n-1),2:m]))

ReturnP=matrix(nrow=n-1,ncol=1)
for (i in 1:(n-1)){ReturnP[i,1]=apply(FxRateR[i,2:m]*w,1,sum)}

FxRateR=cbind(FxRateR,ReturnP)

#Individual VaR and ES calculation
nn=length(FxRateR[,1])
op=250 #Observation period
p=0.99 #Confidence level
VaR.DN=matrix(0,nrow=nn-op+1,ncol=(m-1)*2)
ES.DN=matrix(0,nrow=nn-op+1,ncol=(m-1)*2)
colnames(VaR.DN)=c(as.vector(FxExp[,"Currency"]),as.vector(FxExp[,"Currency"]))
colnames(ES.DN)=c(as.vector(FxExp[,"Currency"]),as.vector(FxExp[,"Currency"]))
for (i in 1:(nn-op+1)){
#Delta-Normal VaR and ES
VaR.DN[i,]=c(apply(FxRateR[i:(op+i-1),2:m],2,mean)-qnorm(p,mean=0,sd=1)*apply(FxRateR[i:(op+i-1),2:m],2,sd),
apply(FxRateR[i:(op+i-1),2:m],2,mean)+qnorm(p,mean=0,sd=1)*apply(FxRateR[i:(op+i-1),2:m],2,sd))
ES.DN[i,]=c(apply(FxRateR[i:(op+i-1),2:m],2,mean)-(1-p)^-1*dnorm(qnorm(p,mean=0,sd=1))*apply(FxRateR[i:(op+i-1),2:m],2,sd),
apply(FxRateR[i:(op+i-1),2:m],2,mean)+(1-p)^-1*dnorm(qnorm(p,mean=0,sd=1))*apply(FxRateR[i:(op+i-1),2:m],2,sd)) }

#Portfolio VaR and ES calculation
VaR.DN.P=matrix(0,nrow=nn-op+1,ncol=1)
ES.DN.P=matrix(0,nrow=nn-op+1,ncol=1)
for (i in 1:(nn-op+1)){
#Delta-Normal Portfolio VaR and ES
VaR.DN.P[i,1]=-qnorm(p,mean=0,sd=1)*(t(w)%*%cov(FxRateR[i:(op+i-1),2:m])%*%w)^0.5
ES.DN.P[i,1]=-(1-p)^-1*dnorm(qnorm(p,mean=0,sd=1))*(t(w)%*%cov(FxRateR[i:(op+i-1),2:m])%*%w)^0.5}

#VaR Instruments
MVaR=matrix(ncol=1,nrow=m-1)
CVaR=matrix(ncol=1,nrow=m-1)
for (i in 2:m){MVaR[i-1]=qnorm(p,mean=0,sd=1)*cov(FxRateR[(nn-op+1):nn,i],FxRateR[(nn-op+1):nn,m+1])/(t(w)%*%cov(FxRateR[(nn-op+1):nn,2:m])%*%w)^0.5 CVaR[i-1]=VaR.DN.P[261]*w[i-1]*
cov(FxRateR[(nn-op+1):nn,i],FxRateR[(nn-op+1):nn,m+1])/
(t(w)%*%cov(FxRateR[(nn-op+1):nn,2:m])%*%w)}

##Backtesting of the VaR
#Portfolio VaR
backTestP=data.frame(Date=FxRateR[(op+1):nn,1],
                     ReturnP=FxRateR[(op+1):nn,"ReturnP"],
                     VaR.DN.P=VaR.DN.P[1:(261-1),1],
                     Except=abs(VaR.DN.P[1:(261-1),1])-abs(FxRateR[(op+1):nn,"ReturnP"]))
for (i in 1:260){
                 if (backTestP[i,"Except"]<0){backTestP[i,"Except"]=1}
                 else{backTestP[i,"Except"]=0}}

#INdividual VaR
backTestI=data.frame(Date=FxRateR[(op+1):nn,1],
                     FxRateR[(op+1):nn,2:m],
                     VaR.DN=VaR.DN[1:(261-1),])

ExceptI=matrix(0,ncol=m-1,nrow=260)
for (i in 1:260){for (j in 1:(m-1)){
  if (backTestI[i,j+1]<=0){
        if (abs(backTestI[i,j+7])-abs(backTestI[i,j+1])<0){ExceptI[i,j]=1}}
                else{if (abs(backTestI[i,j+13])-abs(backTestI[i,j+1])<0){
                     ExceptI[i,j]=1}}}}
                  
#Number of Exceptions                
N.Except=apply(ExceptI[11:260,],2,sum)
N.ExceptP=sum(backTestP[11:260,"Except"])

















      
        
       









