#------------------------------------------------------------------------ #
#Student / Number_NOVA_IMS_CV
#Anderlina Marcal / 20222015
#Daniel Moreira / 20222023
#Elizangela Fernandes / 20222029
#João Carlos Fidalgo / 20222059 
#Grupo K
#------------------------------------------------------------------------ #


rm(list=ls(all=TRUE))
# ------------------------------------------------------------------------
### 1. Reading the data files ###
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
### 2. The study will rely only on the Third Party Liability coverage ###
# ------------------------------------------------------------------------
TPclaims=claims[claims$coverage=="1RC",]
nrow(TPclaims)

### Organizing the files data ###
# ------------------------------------------------------------------------
### 3.1 Number of Claims per Policy ###
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
table(number$nclaims)

# Frequency
baseFREQ = merge(portfolio,number)
head(baseFREQ)
nrow(baseFREQ)

# ------------------------------------------------------------------------
# 3.2 Severity
# ------------------------------------------------------------------------

baseSEV=merge(portfolio,TPclaims) 
tail(baseSEV)
nrow(baseSEV)
baseSEV=baseSEV[baseSEV$cost>0,]
nrow(baseSEV)

## Note: Variable n is the database key. It allows to link the insureds data in both files

# ------------------------------------------------------------------------
#       DESCRIPTIVE STATISTICAL ANALYSIS OF THE PORTFOLIO
# ------------------------------------------------------------------------

# ........................................................................
#   NUMBER OF CLAIMS
# ........................................................................

### Mean and Variance ###

N<-baseFREQ$nclaims
E<-baseFREQ$exposition
lambda<-print(sum(N)/sum(E))
#or
lambda<-print(weighted.mean(N/E,E))

variance<-print(weighted.mean((N/E-lambda)^2,E))


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



# You should fit a distribution to Claim Amounts data
