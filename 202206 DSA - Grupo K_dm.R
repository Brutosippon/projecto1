#------------------------------------------------------------------------ #
#Student / Number_NOVA_IMS_CV
#Anderlina Marcal / 20222015
#Daniel Moreira / 20222023
#Elizangela Fernandes / 20222029
#João Carlos Fidalgo / 20222059 
#Grupo K
#------------------------------------------------------------------------ #
# ........................................................................
# Parte I
# ........................................................................

#  usar # 
#As a first task, you are asked to answer the following questions on a small report:
#1. Perform a detailed descriptive statistical data analysis of the Number of Claims of the Third
#Party Liability on Automobile Insurance. Comment on the features observed and highlight
#values or patterns that you think are important to characterize the phenomenom.
#2. Perform a detailed descriptive statistical data analysis of Claims Severity of the Third Party
#Liability on Automobile Insurance. Comment on the features observed and highlight values or
#patterns that you think are important to characterize the phenomenom.
#3. Fit distributions to the Number of Claims and Claim Severity, including the following requests:
#    For the Number of Claims, remove the highest outlier from data. Refer that fact on your
#report.
#    For Claims Severity, choose an upper bound that allows you to fit a distribution of the
#Exponential Family. Comment, on your report, the upper bound considered and the
#number of claims removed from data and justify your choice.
#    What is the mean value and standard deviation of the claims removed from data in question3? 
#Plot the removed data in a histogram and a boxplot. Comment. Give your opinion on
#how should the insurer include that data on the final premium structure."""


rm(list=ls(all=TRUE))
# ------------------------------------------------------------------------
### Reading the data files ###
# ------------------------------------------------------------------------

portfolio=read.table(choose.files(),header=TRUE,sep=";")#choose autodata.txt
names(portfolio)
# The popdensity and region variables were used to define the zone. 
# We will not used them in the tariff
head(portfolio)
nrow(portfolio)
fix(portfolio)

claims=read.table(choose.files(),header=TRUE,sep=";")  #choose claimsdatanew
names(claims)
head(claims)
nrow(claims)
fix(claims)



# ------------------------------------------------------------------------
### The study will rely only on the Third Party Liability coverage ###
# ------------------------------------------------------------------------
TPclaims=claims[claims$coverage=="1RC",]
nrow(TPclaims)


#1. Perform a detailed descriptive statistical data analysis of the Number of Claims of the Third Party Liability on Automobile Insurance. 

# ------------------------------------------------------------------------
### Number of Claims per Policy ###
# Counting the number of claims for each policy
# ------------------------------------------------------------------------
T=table(TPclaims$ncontract)
T1=as.numeric(names(T))
T2=as.numeric(T)
n1 = data.frame(ncontract=T1,nclaims=T2)
I = portfolio$ncontract%in%T1
T1=portfolio$ncontract[I==FALSE]
n2 = data.frame(ncontract=T1,nclaims=0)
number=rbind(n1,n2)

# Frequency
baseFREQ = merge(portfolio,number)
head(baseFREQ)
nrow(baseFREQ)

# ------------------------------------------------------------------------
#The portfolio have 50000 policies and 7.569 claims, 15% of the policies have claims:
# 0 claims = 47510
# 1 claim = 2313
# 2 claims = 162 
# 3 claims = 12
# 4 claims = 2
# 16 claims = 1
#total of claims = 2313 + 162*2 + 12*3 + 2*4 + 1*16 = 2697
# ------------------------------------------------------------------------


#1.1Comment on the features observed and highlight values or patterns that you think are important to characterize the phenomenom.



# ------------------------------------------------------------------------
#       DESCRIPTIVE STATISTICAL ANALYSIS OF THE PORTFOLIO
# ------------------------------------------------------------------------

# ........................................................................
#   NUMBER OF CLAIMS
# ........................................................................

### Mean and Variance ###

N<-baseFREQ$nclaims # number of claims
E<-baseFREQ$exposition # exposure
sum_N<-sum(N) # total number of claims 2.697
sum_e<-sum(E) # total exposure 26.431.13

lambda<-print(sum(N)/sum(E)) # mean is 0.1020 claims per policy

variance<-print(weighted.mean((N/E-lambda)^2,E)) #variance is 0.34 that means that the data is not Poisson distributed





# ........................................................................
#   Individual Analysis of Explanatory Variables
# ........................................................................

### Example: Fuel ####

X1<-factor(baseFREQ$fuel);X1 #factor is a categorical variable
levels(X1)

tapply(N,X1,sum)  # tapplies the function sum to the variable N according to the levels of X1
# Shows the number of claims in each of the Fuel variable Levels

tapply(N,X1,sum)/tapply(E,X1,sum) 
# Claim Frequency for type of fuel

# Comparing the means for type of fuel
#install.packages("weights")
library(weights)
wtd.t.test(x=(N/E)[X1=="D"],y=(N/E)[X1=="E"],weight=E[X1=="D"],weighty=E[X1=="E"],samedata=FALSE)  


 ### Contingency Tables ###

## Fuel vs Age of the Vehicle ##

# N? of policies
X2<-cut(baseFREQ$agevehicle,c(0,3,10,101),right=FALSE)
levels(X2) # be aware of option right=FALSE
# transforming the agevehicle numeric variable into a categorical variable 
# with levels [0,3[, [3,10[, [10,101[
names1<-levels(X1)
names2<-levels(X2)
(fuelvehicle=table(X1,X2))

# Exposition
exposition=fuelvehicle
for(k in 1:nrow(exposition)){
  exposition[k,]=tapply(E[X1==names1[k]],X2[X1==names1[k]],sum)    
}
exposition

# N? claims
fclaims=fuelvehicle
for(k in 1:nrow(fclaims)){
  fclaims[k,]=tapply(N[X1==names1[k]],X2[X1==names1[k]],sum)  
}
fclaims


# Claim Frequency
frequency=fclaims/exposition
frequency


# Age of the Driver
age<-seq(18,100,by=1)
FREQ=rep(NA,length(age))
for(k in 1:length(FREQ)){
  I=baseFREQ$agedriver==age[k]
  X=baseFREQ$nclaims[I]
  W=baseFREQ$exposition[I]
  FREQ[k]=weighted.mean(X/W,W)
}
fclaims_global=sum(baseFREQ$nclaims)/sum(baseFREQ$exposition)
plot(age,FREQ,main="Age of Driver vs Claim Frequency", xlab="Age",ylab="Claim Frequency",col="skyblue",pch=19)
abline(h=fclaims_global,lty=12,col="dodgerblue")



# ........................................................................
# Graphical Analysis of Relations between Explanatory Variables 
# and Number of Claims
# ........................................................................

# Function that constructs a graphic with claim frequency over risk levels
# Includes estimate through confidence interval for claim frequency
interactiongraphic=function(title="Claim Frequency vs Age of the Driver",name="agedriver", lev=c(17,21,24,29,34,44,64,84,100),
                            contin=TRUE){
  
  if(contin==TRUE){X=cut(baseFREQ[,name],lev)}
  if(contin==FALSE){X=as.factor(baseFREQ[,name])}
  E=baseFREQ$exposition
  Y=baseFREQ$nclaims
  FREQ=levels(X)
  mea=variance=n=rep(NA,length(FREQ))
  for(k in 1:length(FREQ)){
    mea[k]=weighted.mean(Y[X==FREQ[k]]/E[X==FREQ[k]],E[X==FREQ[k]])
    variance[k]=weighted.mean((Y[X==FREQ[k]]/E[X==FREQ[k]]-mea[k])^2,E[X==FREQ[k]])
    n[k]=sum(E[X==FREQ[k]])
  }
  
  w=barplot(n,names.arg=FREQ,col="light blue", axes=FALSE,xlim=c(0,1.2*length(FREQ)+0.5))
  mid=w[,1]
  axis(2)
  par(new=TRUE)
  IC1=mea+1.96/sqrt(n)*sqrt(variance)
  IC2=mea-1.96/sqrt(n)*sqrt(variance)
  globalmean=sum(Y)/sum(E)
  
  plot(mid,mea,main=title,ylim=range(c(IC1,IC2)),type="b",col="red",axes=FALSE,xlab="",ylab="",xlim=c(0,1.2*length(FREQ)+0.5))
  segments(mid,IC1,mid,IC2,col="red")
  segments(mid-0.1,IC1,mid+0.1,IC1,col="red")
  segments(mid-0.1,IC2,mid+0.1,IC2,col="red")
  points(mid,mea,pch=19,col="red")
  axis(4)
  abline(h=globalmean,lty=2,col="red")
  
  mtext("Exposition",2,line=2,cex=1.2,col="light blue")
  mtext("Annual Frequency",4,line=-2,cex=1.2,col="red")
}


### Plotting some interaction graphics ###

# Age of the Driver
interactiongraphic()

# If you rather have equivalent number of policies in each level  
# Redefine the levels
# You can use Quantiles to do it automatically 
Q=quantile(baseFREQ[,"agedriver"],(0:10/10))
Q[1]=Q[1]-1
interactiongraphic(name="agedriver",lev=Q, contin=TRUE)


#Zone of Residence 
interactiongraphic(title="Claim Frequency vs Zone of Residence",name="zone",contin=FALSE)

# Power of Vehicle
interactiongraphic(title="Claim Frequency vs Power of Vehicle",name="power",contin=FALSE)

# Age of Vehicle
interactiongraphic(title="Claim Frequency vs Age of Vehicle",name="agevehicle", contin=FALSE)

# Brand of Vehicle
interactiongraphic(title="Claim Frequency vs Brand of Vehicle",name="brand", contin=FALSE)

# Type of Fuel
interactiongraphic(title="Claim Frequency vs Type of Fuel",name="fuel", contin=FALSE)


#2. Perform a detailed descriptive statistical data analysis of Claims Severity of the Third Party
#Liability on Automobile Insurance. Comment on the features observed and highlight values or
#patterns that you think are important to characterize the phenomenom.

# ------------------------------------------------------------------------
#  Severity
# ------------------------------------------------------------------------

baseSEV=merge(portfolio,TPclaims) 
tail(baseSEV)
nrow(baseSEV)
baseSEV=baseSEV[baseSEV$cost>0,] # >0 to remove the claims with cost=0 because they are not relevant for the study (accident report only)
nrow(baseSEV) # 1.924 severity claims

## Note: Variable n is the database key. It allows to link the insureds data in both files

# ........................................................................
#   CLAIMS SEVERITY
# ........................................................................
### Preliminary Analysis of Data ###

baseSEV<-baseSEV[baseSEV$cost>0,]
# min, max
min(baseSEV$cost)
max(baseSEV$cost)

# mean, standard deviation, variation coefficient
meanCOST<-print(mean(baseSEV$cost))
sdCOST<-print(sd(baseSEV$cost))
vcCOST<-print(sdCOST/meanCOST)

# quantiles, boxplot
quantile(baseSEV$cost,prob=c(0.5,0.9,0.95,0.99))
boxplot(baseSEV$cost, main="Claim Costs BoxPlot",horizontal=TRUE, col="dodgerblue")
# highly right skewed, 2 highly severe outliers

# histogram
breakshist=seq(0,max(baseSEV$cost)+1,by=1000)
histCOST<-hist(baseSEV$cost, breaks=breakshist,main="Claim Costs", col="dodgerblue",xlab="Cost", ylab="",ylim=c(0,1000))
# long right tail

# Setting lower and upper limits for claim amounts
# lower=0 , upper=95% quantile
limInf=0
limSup=6000
step=1000

baseSEV_withlim<-baseSEV[baseSEV$cost<=limSup,]
nrow(baseSEV) #total database
nrow(baseSEV_withlim) #limited database
nrow(baseSEV)-nrow(baseSEV_withlim)
# removed 74 claims from data
# we should include them in the tariff structure at the end

# Boxplot
boxplot(baseSEV_withlim$cost, main="Claim Costs BoxPlot",horizontal=TRUE, col="dodgerblue")

# Histogram
breakshist=seq(0,limSup+1,by=step)
histCOST_withlim<-hist(baseSEV_withlim$cost, main="Claim Costs", col="dodgerblue",xlab="Cost", ylab="",xlim=c(0,limSup),ylim=c(0,1000),breaks=breakshist)


# ........................................................................
# Graphical Analysis of Relations between Explanatory Variables 
# and Claim Costs
# ........................................................................

# Function that constructs a graphic with claim frequency and claim severity
# over risk levels
# Includes estimate through confidence interval 

interactiongraphic_cost=function(title="Frequency and Severity vs Age of Driver",name="agedriver", lev=c(17,21,24,29,34,44,64,84,100),
                                 contin=TRUE){
  if(contin==TRUE){X=cut(baseFREQ[,name],lev)}
  if(contin==FALSE){X=as.factor(baseFREQ[,name])}
  E=baseFREQ$exposition
  Y=baseFREQ$nclaims
  FREQ=levels(X)
  median=variancian=nn=rep(NA,length(FREQ))
  for(k in 1:length(FREQ)){
    median[k]=weighted.mean(Y[X==FREQ[k]]/E[X==FREQ[k]],E[X==FREQ[k]])
    variancian[k]=weighted.mean((Y[X==FREQ[k]]/E[X==FREQ[k]]-median[k])^2,E[X==FREQ[k]])
    nn[k]=sum(E[X==FREQ[k]])
  }
  
  globalmeann=sum(Y)/sum(E)
  
  w=barplot(nn,names.arg=FREQ,col="light green", axes=FALSE,xlim=c(0,1.2*length(FREQ)+0.5))
  mid=w[,1]
  
  if(contin==TRUE){X=cut(baseSEV[,name],lev)}
  if(contin==FALSE){X=as.factor(baseSEV[,name])}
  Y=baseSEV$cost
  FREQ=levels(X)
  mediac=varianciac=nc=rep(NA,length(FREQ))
  for(k in 1:length(FREQ)){
    mediac[k]=mean(Y[X==FREQ[k]])
    varianciac[k]=var(Y[X==FREQ[k]])
    nc[k]=length(Y[X==FREQ[k]])
  }
  
  globalmeanc=mean(Y)
  
  par(new=TRUE)
  IC1=median+1.96/sqrt(nn)*sqrt(variancian)
  IC2=median-1.96/sqrt(nn)*sqrt(variancian)
  
  plot(mid,median,main=title,ylim=range(c(IC1,IC2)),type="b",col="red",axes=FALSE,xlab="",ylab="",xlim=c(0,1.2*length(FREQ)+0.5))
  segments(mid,IC1,mid,IC2,col="red")
  segments(mid-0.1,IC1,mid+0.1,IC1,col="red")
  segments(mid-0.1,IC2,mid+0.1,IC2,col="red")
  points(mid,median,pch=19,col="red")
  axis(4)
  abline(h=globalmeann,lty=2,col="red")
  
  par(new=TRUE)
  IC1=mediac+1.96/sqrt(nc)*sqrt(varianciac)
  IC2=mediac-1.96/sqrt(nc)*sqrt(varianciac)
  
  plot(mid,mediac,main=title,ylim=range(c(IC1,IC2)),type="b",col="blue",axes=FALSE,xlab="",ylab="",xlim=c(0,1.2*length(FREQ)+0.5))
  segments(mid,IC1,mid,IC2,col="blue")
  segments(mid-0.1,IC1,mid+0.1,IC1,col="blue")
  segments(mid-0.1,IC2,mid+0.1,IC2,col="blue")
  axis(2)
  abline(h=globalmeanc,lty=2,col="blue")
  
  mtext("Avergae Cost",2,line=2,cex=1.2,col="blue")
  mtext("Annual Claim Frequency",4,line=-2,cex=1.2,col="red")
}



### Plotting some interaction graphics ###

# Age of the Driver
interactiongraphic_cost()

# Using an approximate number of policies in each cell
Q=quantile(baseFREQ[,"agedriver"],(0:10)/10)
Q[1]=Q[1]-1

interactiongraphic_cost(title="Frequency and Claim Cost vs Age of Driver",name="agedriver",lev=Q,contin=TRUE)


# Age of Vehicle
Q=quantile(baseFREQ[,"agevehicle"],(0:10)/10)
Q[1]=Q[1]-1

interactiongraphic_cost(title="Frequency and Claim Cost vs Age of Vehicle",name="agevehicle",lev=Q,contin=TRUE)

# ........................................................................
#3. Fit distributions to the Number of Claims and Claim Severity, including the following requests:
#    3.1-For the Number of Claims, remove the highest outlier from data. Refer that fact on your report.
#    3.2-For Claims Severity, choose an upper bound that allows you to fit a distribution of the Exponential Family. Comment, on your report, the upper bound considered and the number of claims removed from data and justify your choice.
#    3.3-What is the mean value and standard deviation of the claims removed from data in question3? Plot the removed data in a histogram and a boxplot. Comment. Give your opinion on how should the insurer include that data on the final premium structure.
# ........................................................................
    
#3.1-For the Number of Claims, remove the highest outlier from data. Refer that fact on your report.
# ........................................................................
# Lets start assuming that Age of the Driver is the only risk factor
# ........................................................................

# ........................................................................
# Poisson GLM with logarithmic link function - Multiplicative Model
# ........................................................................

preg_age<-glm(nclaims~agedriver,family=poisson(link="log"),data=baseFREQ)
summary(preg_age)

# Access the regression coefficients:
coefficients(preg_age)[1]
exp(coefficients(preg_age)[1])

coefficients(preg_age)[2]
exp(coefficients(preg_age)[2]) 

# Impact for each year old
1-exp(coefficients(preg_age)[2])


# ........................................................................
# The highest estimate for claim frequency 
# ........................................................................

exp(coefficients(preg_age)[1])*exp(coefficients(preg_age)[2]*18)


# ........................................................................
# Poisson GLM with identity link function - Additive Model
# ........................................................................

preg_age_id<-glm(nclaims~agedriver,family=poisson(link="identity"),data=baseFREQ)
summary(preg_age_id)

# highest claim frequency
coefficients(preg_age_id)[1]+coefficients(preg_age_id)[2]*18

# ........................................................................
#Poisson GLM with logarithmic link function and Exposure as offset
# ........................................................................

preg_age_offset<-glm(nclaims~agedriver+offset(log(exposition)),
                     family=poisson(link="log"),data=baseFREQ)
summary(preg_age_offset)

# Accessing the coefficients estimates
coefficients(preg_age_offset)[1]
exp(coefficients(preg_age_offset)[1])

coefficients(preg_age_offset)[2]
exp(coefficients(preg_age_offset)[2])

# Impact for each year old
1-exp(coefficients(preg_age_offset)[2])


# ........................................................................
# The highest estimate for claim frequency 
# ........................................................................

exp(coefficients(preg_age_offset)[1])*exp(coefficients(preg_age_offset)[2]*18)


# ........................................................................
# Claim Frequency estimate for a 50 years old
# ........................................................................
exp(coefficients(preg_age_offset)[1])*exp(coefficients(preg_age_offset)[2]*50)
# or
predict(preg_age_offset,newdata=data.frame(agedriver=50,exposition=1),
        type="response")

# ........................................................................
# Plot Predictions from both models for all ages
# ........................................................................

# Define the set of ages for which you want predictions
age=18:max(baseFREQ$agedriver)

# Multiplicative Model without offset
lambda_est<-predict(preg_age,newdata=data.frame(agedriver=age,exposition=1),type="response")

# Multiplicative Model with offset
lambda_est_offset<-predict(preg_age_offset,newdata=data.frame(agedriver=age,exposition=1),type="response")

# Plot both predictions
par(mfrow=c(2,1))

plot(age,lambda_est,type="l",lwd=2,col="blue", main="Poisson regression without Exposition",ylab="")
abline(h=sum(N)/nrow(baseFREQ),col="firebrick3",lwd=2)

plot(age,lambda_est_offset,type="l",lwd=2,col="blue", main="Poisson regression with Exposition", ylab="")
abline(h=sum(N)/sum(E),col="firebrick3",lwd=2)

par(mfrow=c(1,1))


# ........................................................................
# Poisson GLM with logarithmic link function - age driver categorical
# ........................................................................

# ........................................................................
# Defining Levels
# ........................................................................

agedriver_lev<-c(18,22,26,31,41,51,61,71,81,101)
baseFREQ$agecut<-cut(baseFREQ$agedriver,breaks=agedriver_lev,right=FALSE)
head(baseFREQ$agecut)

# ........................................................................
# Fitting GLM Poisson Regression with age driver categorical
# ........................................................................
preg_age_cat<-glm(nclaims~agecut+offset(log(exposition)),data=baseFREQ,family=poisson(link="log"))
summary(preg_age_cat)


# ........................................................................
# Claim Frequency estimate for Standard Insured and for 50 years old
# ........................................................................

# Standard Insured
exp(coefficients(preg_age_cat)[1])
#or
predict(preg_age_cat,newdata=data.frame(agecut="[18,22)",exposition=1),type="response")

# 50 years old driver
predict(preg_age_cat,newdata=data.frame(agecut="[41,51)",exposition=1),type="response")
exp(coefficients(preg_age_cat)[1])*exp(coefficients(preg_age_cat)[5])

# comparing both insureds ins terms of claim frequency
exp(coefficients(preg_age_cat)[5])
1-exp(coefficients(preg_age_cat)[5])


# ........................................................................
# Relevel Standard Insured to be [31,41[
# ........................................................................

baseFREQ$agecut<-relevel(baseFREQ$agecut,"[31,41)")
preg_age_cat2<-glm(nclaims~agecut+offset(log(exposition)),data=baseFREQ,family=poisson(link="log"))
summary(preg_age_cat2)

predict(preg_age_cat2,newdata=data.frame(agecut="[41,51)",exposition=1),type="response")


# ........................................................................
# Improving the model
# ........................................................................

# Check the result of Wald Test in R output

# You can also perform a separated test:
library(car)
linearHypothesis(preg_age_cat2,c("agecut[41,51)=0"))


# Grouping the age classes
baseFREQ$agecut2=baseFREQ$agecut
baseFREQ$agecut2[baseFREQ$agecut%in%c("[31,41)","[41,51)")]="[31,41)"

preg_age_cat3<-glm(nclaims~agecut2+offset(log(exposition)),data=baseFREQ,family=poisson(link="log"))
summary(preg_age_cat3)


# What about joining classe [26,31) to Standard Insurer?
# For you to perform later

# ........................................................................
# Quality of the Models
# ........................................................................

# Deviance
deviance(preg_age_offset)
deviance(preg_age_cat2)


# AIC
AIC(preg_age_offset)
AIC(preg_age_cat2)

#or
-2*logLik(preg_age_offset)+2*2 #2 parameters estimated
-2*logLik(preg_age_cat2)+2*9 #9 parameters estimated

# Residuals
plot(residuals(preg_age_offset,type="deviance"),main="Residual Deviance", col="green")
hist(residuals(preg_age_offset,type="deviance"),main="Residual Deviance", col="green")

plot(residuals(preg_age_cat2,type="deviance"),main="Residual Deviance", col="green")
hist(residuals(preg_age_cat2,type="deviance"),main="Residual Deviance", col="green")


# ........................................................................
# A GLM Poisson regression with all risk factors
# ........................................................................

# ........................................................................
# Age of Vehicle to Categorical variable
# ........................................................................

agevehicle_lev<-c(0,4,11,16,101)
baseFREQ$vehcut<-cut(baseFREQ$agevehicle,breaks=agevehicle_lev,right=FALSE)
head(baseFREQ$vehcut)

# ........................................................................
# Fit a model with all categorical variables
# ........................................................................
baseFREQ$zone<-as.factor(baseFREQ$zone)
model_full<-glm(nclaims~zone+as.factor(power)+vehcut+agecut+as.factor(brand)+fuel+offset(log(exposition)),family=poisson(link="log"),data=baseFREQ)
summary(model_full)


# ........................................................................
# Estimate the claim frequency of an insured with risk factors:
# Age=50 ; Age Vehicle = 0 ; Zone = A ; 
# Brand = Peugeout ; Fuel = Gasoline ; Power = 11
# ........................................................................

predict(model_full,newdata=data.frame(zone="A",power=11,vehcut="[0,4)",
                                      agecut="[41,51)",brand=2,fuel="E",
                                      exposition=1),type="response")


# ........................................................................
# Test the removal of variable power
# ........................................................................

# fit a model without the risk factor power
model_no_pow<-glm(nclaims~zone+vehcut+agecut+as.factor(brand)+fuel+offset(log(exposition)),family=poisson(link="log"),data=baseFREQ)
summary(model_no_pow)

# Likelihood Ratio Test
anova(model_no_pow,model_full,test="Chisq")


# ........................................................................
# Test the removal of variable age of vehicle
# ........................................................................

model_no_veh<-glm(nclaims~zone+as.factor(power)+agecut+as.factor(brand)+fuel+offset(log(exposition)),family=poisson(link="log"),data=baseFREQ)
summary(model_no_veh)

# Likelihood Ratio Test
anova(model_no_veh,model_full,test="Chisq")


# ........................................................................
# Test for grouping (Zone A , Zone B) and (Zone E , Zone F)
# ........................................................................

summary(model_full)


# H0: Zone A = Zone B 
library(aod)
s<-vcov(model_full)
head(s)
wald.test(b=coef(model_full),Sigma=vcov(model_full),Terms=2) 

# Grouping Risk Factor Levels
baseFREQ$zone2=baseFREQ$zone
baseFREQ$zone2[baseFREQ$zone%in%c("A","B")]="A"


# H0: Zone E = Zone F 

library(multcomp)
summary(glht(model_full,mcp(zone="Tukey")))


# ........................................................................
# Improve the model
# ........................................................................

# For you to develop

# ........................................................................
# Export for txt file
# ........................................................................

results_model <-summary.glm(model_full)$coefficients

write.table(results_model,"nsin.txt",append = FALSE, quote = TRUE, sep = ";", eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE, qmethod = c("escape", "double"))



# ........................................................................
# Overdispersion
# ........................................................................

# ........................................................................
# Test for Overdispersion and estimate phi
# ........................................................................

# H0: Mean=Variance
library(AER)

# Estimate of Phi
dispersiontest(model_full)

# Quasi-Poisson model
model_full_qp<-glm(nclaims~zone+as.factor(power)+vehcut+agecut+as.factor(brand)+fuel+offset(log(exposition)),family=quasipoisson(link="log"),data=baseFREQ)
summary(model_full_qp)


# ........................................................................
# Fitting a GLM Negative Binomial Model
# ........................................................................

library(MASS)
model_nb<-glm.nb(nclaims~zone+as.factor(power)+vehcut+
                   agecut+as.factor(brand)+fuel+offset(log(exposition)),data=baseFREQ)
summary(model_nb)

# ........................................................................
# Comparing the coeficients from Poisson and NB regressions
# ........................................................................

### Parameter Estimates ###

# In a graphic
plot(model_nb$coefficients,model_full$coefficients,xlab="Negative Binomial Regression",ylab="Poisson Regression",col="SkyBlue",pch=19)
abline(a=0,b=1,lty=2,col="blue")

# In a table
cbind(model_nb$coefficients,model_full$coefficients)


### Parameter Standard Error ###

# In a graphic
plot(summary(model_nb)$coefficients[, 2],summary(model_full)$coefficients[, 2],xlab="Negative Binomial Regression Std. Error",ylab="Poisson Regression Std. Error",col="SkyBlue",pch=19)
abline(a=0,b=1,lty=2,col="blue")

# In a table
cbind(summary(model_nb)$coefficients[, 2],summary(model_full)$coefficients[, 2])


# ........................................................................
# Improve the Model for the Number of Claims 
# ........................................................................

# For you to perform


# ........................................................................
# Evaluate the quality of the model
# ........................................................................

# For you to perform

# Estimate the claim frequency of an insured with risk factors:
# Age=50 ; Age Vehicle = 0 ; Zone = A ; 
# Brand = Peugeout ; Fuel = Gasoline ; Power = 11
# ........................................................................

predict(model_nb,newdata=data.frame(zone="A",power=11,vehcut="[0,4)",agecut="[41,51)",brand=2,fuel="E",exposition=1),type="response")

#3.2-For Claims Severity, choose an upper bound that allows you to fit a distribution of the Exponential Family. Comment, on your report, the upper bound considered and the number of claims removed from data and justify your choice.

# Setting Categorical data on Age of Driver and Age of Vehicle on baseSEV database
agevehicle_lev<-c(0,4,11,16,101)
baseSEV$vehcut<-cut(baseSEV$agevehicle,breaks=agevehicle_lev,right=FALSE)
head(baseSEV$vehcut)

agedriver_lev<-c(18,22,26,31,41,51,61,71,81,101)
baseSEV$agecut<-cut(baseSEV$agedriver,breaks=agedriver_lev,right=FALSE)


# Setting lower and upper limits for claim amounts
limInf=0
limSup=6000
step=1000

baseSEV_withlim<-baseSEV[baseSEV$cost<=limSup,]
nrow(baseSEV) #total database
nrow(baseSEV_withlim) #limited database
nrow(baseSEV)-nrow(baseSEV_withlim)

# Setting Categorical data on Age of Driver and Age of Vehicle on baseSEV database
baseSEV_withlim$vehcut<-cut(baseSEV_withlim$agevehicle,breaks=agevehicle_lev,right=FALSE)

baseSEV_withlim$agecut<-cut(baseSEV_withlim$agedriver,breaks=agedriver_lev,right=FALSE)

# ........................................................................
# GLM Regression Model for Claim Costs 
# ........................................................................

# ........................................................................
# Gamma GLM
# ........................................................................

model_gamma<-glm(cost~zone+as.factor(power)+vehcut+agecut+as.factor(brand)+fuel,family=Gamma(link="log"),data=baseSEV)
summary(model_gamma)

predict(model_gamma,newdata=data.frame(zone="A",power=11,vehcut="[0,4)",agecut="[41,51)",brand=2,fuel="E"),type="response")

# with lim
model_gamma_withlim<-glm(cost~zone+as.factor(power)+vehcut+agecut+as.factor(brand)+fuel,family=Gamma(link="log"),data=baseSEV_withlim)
summary(model_gamma_withlim)

predict(model_gamma_withlim,newdata=data.frame(zone="A",power=11,vehcut="[0,4)",agecut="[41,51)",brand=2,fuel="E"),type="response")



# ........................................................................
# Inverse Gaussian GLM
# ........................................................................

model_ig<-glm(cost~zone+as.factor(power)+vehcut+agecut+as.factor(brand)+fuel,family=inverse.gaussian(link="log"),data=baseSEV)
# There is a divergence problem. The model doesn't fit
# This is due to this data characterstics. Probably tail issues.
# Remember that, in a previous step, we decided to truncate the data and model only "common" claims
# The instruction is just for you to keep
summary(model_ig)

predict(model_ig,newdata=data.frame(zone="A",power=11,vehcut="[0,4)",agecut="[41,51)",brand=2,fuel="E",exposition=1),type="response")


# ........................................................................
# LogNormal GLM
# ........................................................................

model_ln<-lm(log(cost)~zone+as.factor(power)+vehcut+agecut+as.factor(brand)+fuel,data=baseSEV_withlim)
summary(model_ln)

sigma<-summary(model_ln)$sigma
mu<-print(predict(model_ln,newdata=data.frame(zone="A",power=11,vehcut="[0,4)",agecut="[41,51)",brand=2,fuel="E"),type="response"))
# The expected value of claim cost for this insured is
exp(mu+0.5*sigma^2)

#3.3-What is the mean value and standard deviation of the claims removed from data in question3? Plot the removed data in a histogram and a boxplot. Comment. Give your opinion on how should the insurer include that data on the final premium structure.

# ........................................................................
# Parte II
# ........................................................................

# ........................................................................
#1. Fit a GLM to the Number of Claims data and estimate the claim frequency for each risk profile in your portfolio. Remember to:
#   1.1-Detail and justify your model assumptions and choices.
#   1.2-Improve your model, using adequate statistical tests.
#   1.3-Identify the Standard Insured characteristics and the correspondent claim frequency estimate.
#   1.4-Identify the insured’s profile for both highest and lowest risk regarding claim frequency risk. Estimate the claim frequency for both of them.
# ........................................................................

#1.1-Detail and justify your model assumptions and choices.

#1.2-Improve your model, using adequate statistical tests.

#1.3-Identify the Standard Insured characteristics and the correspondent claim frequency estimate.

#1.4-Identify the insured’s profile for both highest and lowest risk regarding claim frequency risk. Estimate the claim frequency for both of them.

# ........................................................................
#2. Fit a GLM to the Claim Costs of “common” claims. Remember to:
#   2.1-Be clear about your definition of “common” claim.
#   2.2-Detail and justify your model assumptions and choices.
#   2.3-Improve your model, using adequate statistical tests.
#   2.4-Identify the Standard Insured characteristics and claim severity estimate.
#   2.5- Identify the insured’s profile for both highest and lowest risk regariding claim severity risk. Estimate the claim severity for both of them.
# ........................................................................

#2.1-Be clear about your definition of “common” claim.

#2.2-Detail and justify your model assumptions and choices.

#2.3-Improve your model, using adequate statistical tests.

#2.4-Identify the Standard Insured characteristics and claim severity estimate.

#2.5-Identify the insured’s profile for both highest and lowest risk regariding claim severity risk. Estimate the claim severity for both of them.

# ........................................................................
#3.Propose a Pricing Structure to the “common” claims. Identify the highest and lowest insured’s risk profile and premiums to be charged.
# ........................................................................


# ........................................................................
#4.Give your opinion on how the large claims should be included in the Pricing Structure. Rely your opinion on some description of data.
# ........................................................................

