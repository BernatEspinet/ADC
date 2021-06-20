#First we are going to import both datasets
setwd("/Users/berna/Desktop/ADC treball/")
df_happiness <- read.table("/Users/berna/Desktop/ADC treball/world-happiness-report.csv",header = TRUE, sep = ",")
df_P <- read.table("/Users/berna/Desktop/ADC treball/openaq.csv",header = TRUE, sep = ";",fill=TRUE,stringsAsFactors = FALSE)
scalar1 <- function(x) {x / sqrt(sum(x^2))}
#Get the year by date of openaq dataset
df_P$Last_Updated <- substr(df_P$Last_Updated, 1,4) 
#Get all data of CO pollution
to_purge=which(df_P$Pollutant == "CO")
df_Pollution=df_P[c(to_purge),]

#Set all values to same unit (µg/m^3)

for(row in 1:nrow(df_Pollution)){
  if(df_Pollution$Unit[row] == "ppm"){
    df_Pollution$Value[row]= as.character(as.numeric(as.character(df_Pollution$Value[row]))*1145.609)
    df_Pollution$Unit[row] = "Âµg/mÂ³"}
  }

#Get the concurrent countries with data
Countries <- unique(df_happiness$ï..country)
Countries2 <- unique(df_Pollution$Country_Label)
Countries_in_Common=Reduce(intersect, list(Countries,Countries2))


#Get data from all concurrent countries
Life_Ladder=c()
CO=c()
for(c in Countries_in_Common){
  #For each country, we find what years we have concurrences of both datasets
  index_countryH=which(df_happiness$ï..country == c)
  
  index_country1=which(df_Pollution$Country_Label == c)
  index_country2=which(df_Pollution$Pollutant == "CO")
  index_countryP=Reduce(intersect, list(index_country1,index_country2))
  
  #We reduce the dataset to country and CO 
  df_per_countryH=df_happiness[c(index_countryH),]
  df_per_countryP=df_Pollution[c(index_countryP),]
  #We calculate the common years
  common_years=Reduce(intersect, list(unique(df_per_countryP[ , 9]),unique(df_per_countryH[ , 2])))
  for(year in common_years){
    #For each country and year pair, we will find the mean of CO pollution and pair it with the Life Ladder
    group1=which(df_per_countryH$year == year)
    group2=which(df_per_countryP$Last_Updated == year)
    mean_CO=sum(as.numeric(as.vector(df_per_countryP[group2,]$Value)))/length(df_per_countryP[group2,]$Value)
    #We will filter out negative pollution values
    if(mean_CO>0){
      CO=c(CO,mean_CO)
      Life_Ladder=c(Life_Ladder,df_per_countryH[group1,]$Life_Ladder)
    }
  }
}
log_CO=c(log(CO))
plot(Life_Ladder,log_CO,main="The relationship between CO in air and the Life Ladder",xlab="Life Ladder score", ylab="Log CO in the air (µg/m^3)")
data<-cbind(Life_Ladder,log_CO)
fit<-lm(data[,2]~data[,1])
summary(fit)
abline(summary(fit)$coefficients[1], summary(fit)$coefficients[2],col="red")

#We are going to make a permutation test on the F-statistic to ensure the pvalue is under 0.05
b<-anova(fit)
Ftrue<-b$"F value"[1];
nr=10000 #number of rearrangements to be examined
F=numeric(nr);
for (i in 1:nr){
  newx<- sample(data, 60)
  fit<-lm(newx ~ data[,1])
  b<-anova(fit)
  F[i]<-b$"F value"[1]}
experimental_pvalue=length(F[F >= Ftrue])/nr

#The current ranking of countries (2020)
index_LL=which(df_happiness$year == "2020")
df_LL_2021=df_happiness[c(index_LL),]
df_2=subset(df_LL_2021, select=c("ï..country", "Life_Ladder"))
x=df_2[order(df_2$Life_Ladder),]
x[order(nrow(x):1),]

#Get CI for CO to get to top 10
x=Life_Ladder
y=log_CO
data=data.frame(Life_Ladder,log_CO)
n<-length(x); data<-cbind(x,y)
theta<-summary(fit1)$coefficients[1]
sdtheta<-summary(fit1)$coefficients[3]
nb<-1000; z<-seq(1,n);tb<-numeric(nb);predb<-numeric(nb)
thetab<-numeric(nb)
for(i in 1:nb){
  zb<-sample(z,n,replace=T)
  ajustb<- lm(data[zb,2]~ data[zb,1])
  thetab[i]<-summary(ajustb)$coefficients[1]
  sdthetab<-summary(ajustb)$coefficients[3]
  predb[i]<-summary(ajustb)$coefficients[1]+summary(ajustb)$coefficients[2]*7.213
  tb[i]<-(thetab[i]-theta)/sdthetab}

hist(predb,main="Histogram of the CO needed to get in the top 10")
Prediction_CO_to_top10Score=quantile(predb,c(0.025,0.975))

#Get the data from spain
CO_s=c()
Life_Ladder_s=c()
index_countryH=which(df_happiness$ï..country == "Spain")
index_country1=which(df_Pollution$Country_Label == "Spain")
index_country2=which(df_Pollution$Pollutant == "CO")
index_countryP=Reduce(intersect, list(index_country1,index_country2))

#We reduce the dataset to Spain and CO 
df_per_countryH=df_happiness[c(index_countryH),]
df_per_countryP=df_Pollution[c(index_countryP),]
#We calculate the common years
common_years=Reduce(intersect, list(unique(df_per_countryP[ , 9]),unique(df_per_countryH[ , 2])))
for(year in common_years){
  #For each year, we will find the mean of CO pollution and pair it with the Life Ladder
  group1=which(df_per_countryH$year == year)
  group2=which(df_per_countryP$Last_Updated == year)
  mean_CO=sum(as.numeric(as.vector(df_per_countryP[group2,]$Value)))/length(df_per_countryP[group2,]$Value)
  #We will filter out negative pollution values
  if(mean_CO>0){
    CO_s=c(CO_s,mean_CO)
    Life_Ladder_s=c(Life_Ladder_s,df_per_countryH[group1,]$Life_Ladder)
  }
}
#Log regression fit
log_CO_s=c(log(CO_s))
plot(Life_Ladder_s,log_CO_s,main="The relationship between CO in air and the Life Ladder of Spain",xlab="Life Ladder score", ylab="Log CO in the air(µg/m^3)")
data_s<-cbind(Life_Ladder_s,log_CO_s)
fit_S<-lm(data_s[,2]~data_s[,1])
abline(summary(fit_S)$coefficients[1], summary(fit_S)$coefficients[2],col="red")

