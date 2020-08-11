library(reshape2)
library(ggplot2)
library(circular)
library(plyr)
library(maptools)
library(sp)
library(rgdal)

cumsumfun = function(x) {cumsum(ifelse(is.na(x), 0, x))/sum(x, na.rm=T)}

## set working directory
setwd("/Users/tjb1a13/Google Drive/DFO/Citizen Cod/Data")

cod2017=read.csv("Citizen Cod Access_2017.csv")
cod2018=read.csv("Citizen Cod 2018 Access.csv")
cod2019=read.csv("Citizen Cod 2019.csv")

loc=read.csv("Surveyed_Locations.csv")
loc$population2016=with(loc, popdense*area)
NAFO=readOGR("Divisions/Divisions.shp")

# commercial dates Aug 3 2017, Aug 5 2018
# Aug 12 - Sep 8, 2018  reopened to steward fishers on Sep 22, 2018. opened on Sep 30 
cod=rbind(cod2017[,names(cod2017) %in% names(cod2018)], cod2018[,names(cod2018) %in% names(cod2017)])
cod=rbind(cod[,names(cod) %in% names(cod2019)], cod2019[,names(cod2019) %in% names(cod)])
cod$date=as.Date(as.character(cod$Date), format="%d-%b-%y")
cod$year=as.numeric(format(cod$date, format="%Y"))
cod$hour=round(cod$Time/100)
cod$julian=as.numeric(format(cod$date, "%j"))
cod$weekday=factor(weekdays(cod$date), levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
cod$weekdaynum=as.numeric(cod$weekday)
cod$weekend=with(cod, ifelse(grepl("Sat|Sun", weekday), 1,0))
cod$X.Fish[is.na(cod$X.Fish)]=0
cod$X.Fishers[is.na(cod$X.Fishers)]=round(cod$X.Fish[is.na(cod$X.Fishers)]/5)
cod$X.Fishers[cod$X.Fishers==15]=3
cod$adjFishers=with(cod, ifelse((X.Fish/X.Fishers/5) <= 2 , X.Fishers +1, X.Fishers))
cod=subset(cod, !is.na(adjFishers))

### fix na.hours on cod data
for(i in unique(cod$year)){
  for(j in unique(cod$Community)){
    cod$hour[which(cod$year==i & cod$Community==j & is.na(cod$hour))]=mean(cod$hour[which(cod$year==i & cod$Community==j & !is.na(cod$hour))])
  }
  }

for(i in unique(cod$date)){
    cod$Weather[which(cod$date==i & is.na(cod$Weather))]=round(mean(cod$Weather[which(cod$date==i & !is.na(cod$Weather))]))
    }

cod$Weather[is.na(cod$Weather)]=round(mean(cod$Weather, na.rm=T))


cod=subset(cod, (!as.character(Community) %in% "" & !is.na(year)))
cod$unit.area=toupper(cod$NAFO)
cod=subset(cod, !as.character(Community) %in% c("", "Carbonear", "Embree", "Bay De Verde", "Exploits") & !(unit.area==""))
cod$Community=factor(cod$Community)

cod$wt=10^(3.0879*( log10(cod$Length..cm.))-5.2106)

## get unit areas for commnities
Community.unit=with(cod,unique(data.frame(Community, unit.area)))

## Fill in missing data

#CCcatch=unique(subset(cod, select=c(2:10, 12:13, 15,16,17)))
### Summarise citizen cod by community
daily.cod=ddply(cod, .(Community, date), summarise,
                          boats=length(unique(SurveyID)),
                          nfishers=sum(unique(data.frame(SurveyID, adjFishers, X.Fish))$adjFishers),
                          fish=length(X.Fish),
                          Fish=X.Fish[1],
                          mean.length=mean(Length..cm., na.rm=T),
                          mean.wt=mean(wt, na.rm=T),
                          sum.wt=sum(wt, na.rm=T),
                          #meancatch=mean(with(unique(data.frame(SurveyID, X.Fish, X.Fishers)), X.Fish/X.Fishers)),
                          #meancatchwt=mean(with(unique(data.frame(SurveyID, X.Fish, X.Fishers)), X.Fish/X.Fishers)),
                          year=year[1],
                          Weather=mean(Weather, na.rm=T),
                          unit.area=unit.area[1],
                          time=max(hour)-min(hour)+1)



hourly.cod=ddply(cod, .(Community, date, hour), summarise,
                          boats=length(unique(SurveyID)),
                          nfishers=sum(unique(data.frame(SurveyID, adjFishers, X.Fish))$adjFishers),
                          fish=length(X.Fish),
                          mean.length=mean(Length..cm., na.rm=T),
                          mean.wt=mean(wt, na.rm=T),
                          sum.wt=sum(wt, na.rm=T),
                          Weather=mean(Weather, na.rm=T),
                          year=year[1],
                          #meancatch=mean(with(unique(data.frame(SurveyID, X.Fish, X.Fishers)), X.Fish/X.Fishers)),
                          #meancatchwt=mean(with(unique(data.frame(SurveyID, X.Fish, X.Fishers)), X.Fish/X.Fishers))
                          unit.area=unit.area[1])

### fix na.hours on cod data
for(i in unique(hourly.cod$year)){
  for(j in unique(hourly.cod$Community)){
    hourly.cod$hour[(hourly.cod$year==i & hourly.cod$Community==j & is.na(hourly.cod$hour))]=mean(hourly.cod$hour[which(hourly.cod$year==i & hourly.cod$Community==j & !is.na(hourly.cod$hour))])
  }
}

CClength=unique(subset(cod, select=c(2:7,14:18)))

### commercial catch
load("commercial.catch.R")

### load weather
load( file="hourly.weather.R")
hourly.weather=subset(hourly.weather, Community %in% hourly.cod$Community)
load(file="daily.weather.R")
daily.weather=subset(daily.weather, Community %in% daily.cod$Community)

### load ecapelin
load("ecap.all.R")

#Merge weather, ecap
daily.cov=merge(daily.weather, ecap.all, by="date", all.x=T)
daily.cov$sum.ecap[is.na(daily.cov$sum.ecap)]=0
hourly.cov=merge(hourly.weather, ecap.all, by="date", all.x=T)
hourly.cov$sum.ecap[is.na(hourly.cov$sum.ecap)]=0

# add nafo.unit
daily.cov2=merge(daily.cov, Community.unit, by="Community", all.x=T)
hourly.cov2=merge(hourly.cov, Community.unit, by="Community", all.x=T)

#### add community
daily.cov3=merge(daily.cov2, loc, by="Community", all.x=T)
hourly.cov3=merge(hourly.cov2, loc, by="Community", all.x=T)

### add commercial
daily.cov4=merge(daily.cov3, comcatch, by=c("date", "unit.area"), all.x=T)
hourly.cov4=merge(hourly.cov3, comcatch, by=c("date", "unit.area"), all.x=T)


#### Merge citizen cod and weather, commercial, population
daily.cod2=merge(daily.cod, daily.cov4, by=c("Community", "date", "unit.area"), all.y=T)
hourly.cod2=merge(hourly.cod, hourly.cov4, by=c("Community", "date", "hour", "unit.area"), all.y=T)

save(daily.cod2, file="daily.cod.R")
save(hourly.cod2, file="hourly.cod.R")


##########
### Analyze
##########
## Summarise across dates for Pop density vs average.daily in a community
load("daily.cod.R")
load("hourly.cod.R")


#######
## Models
###### Using a model for total catch observed, we can predict
#### Model for total catch
library(lme4)
library(gam)
library(nlme)
library(gamm4)

## Daily model
mod.d.dat=subset(daily.cod2, !is.na(fish))
pred.d.dat=subset(daily.cod2, is.na(fish))
d.codgamm=gamm4(fish ~  s(julian) + mean.landed + NComboats + Weather+time + weekend  + pressure + max.wind +sum.ecap , 
              random = ~ (1|year) + (1|Community), 
              data = mod.d.dat,
              family=stats::poisson)

mod.d.dat$predicted=exp(predict(d.codgamm$gam))
gam.check(d.codgamm$gam)
plot(fitted(d.codgamm$gam)~residuals(d.codgamm$gam))
plot(d.codgamm$gam)

summary(d.codgamm$mer)
summary(d.codgamm$gam)

acf(resid(d.codgamm$mer), lag.max = 36, main = "ACF")
pacf(resid(d.codgamm$mer), lag.max = 36, main = "pACF")

# summarise by unit.area
d.sum=ddply(mod.d.dat, .(Community), summarize,
            obs=sum(fish),
            pred=sum(predicted))
d.sum[order(d.sum$obs),]
with(d.sum, plot(obs, pred))
abline(a=0, b=1)

## Hourly model
mod.h.dat=subset(hourly.cod2, !is.na(fish))
h.codgamm=gamm4(fish ~  s(julian)+ s(hour) + (mean.landed) + NComboats +Weather+ weekend + max.wind +sum.ecap , 
                 random=~(1|year) + (1|Community), 
                    data=mod.h.dat,
                    family=stats::poisson )
gam.check(h.codgamm$gam)
plot(h.codgamm$gam)
summary(h.codgamm$gam)
summary(h.codgamm$mer)

acf(resid(h.codgamm$mer), lag.max = 36, main = "ACF")
pacf(resid(h.codgamm$mer), lag.max = 36, main = "pACF")

mod.h.dat$predicted=exp(predict(h.codgamm$gam))
h.obspred.d=ddply(mod.h.dat, .(date, Community), summarize,
                obs=sum(fish),
                pred=sum(predicted))

with(h.obspred, plot(obs, pred))
abline(a=0, b=1)


subdat.2017=subset(daily.cod.weath.pop.com, year==2017 & !is.na(fish))

weathermod.2017=glm(fish~mean.wind  + cum.eCap + cum.com+ I(cum.com^2)+ I(cum.eCap^2)+ weekday, 
                    random=~(1|unit.area/Community),
                      data=subdat.2017, family=stats::poisson)
plot(subdat.2017$fish, exp(predict(weathermod.2017)))
cor((subdat.2017)$fish, exp(predict(weathermod.2017)))^2
summary(weathermod.2017)
abline(a=0, b=1)

subdat.2018=subset(daily.cod.weath.pop.com, year==2018 & !is.na(fish))
weathermod.2018=glm(fish~mean.wind  + cum.com+ I(cum.com^2)+ cum.eCap +  I(cum.eCap^2) + 
                      weekend +Community, 
                      data=(subdat.2018), family=stats::poisson)
plot((subdat.2018)$fish, exp(predict(weathermod.2018)))
cor((subdat.2018)$fish, exp(predict(weathermod.2018)))^2
summary(weathermod.2018)
abline(a=0, b=1)
library(xtable)
print(xtable(summary(weathermod.2018), caption="Summary of coefficients from the poisson regression of cod landings against model covariates."),
      file="../Figures/weathermod.doc", include.rownames=T , type="html")

weathermod=glm(fish~mean.wind  + cum.eCap + cum.com+ I(cum.com^2)+I(cum.eCap^2)  + (Community) + year + weekday , 
                    data=(daily.cod.weath.pop.com), family=stats::poisson)

daily.cod.weath.pop.com$pred.fish=exp(predict(weathermod, newdata=daily.cod.weath.pop.com))

pred.true.2018=ggplot(subset(daily.cod.weath.pop.com, !(Community=="Flatrock")& year==2018), aes(x=date, y=(pred.fish))) + 
  geom_point() +
  facet_wrap(~Community) +
  geom_point(aes(x=date, y=fish), colour="red") +
  theme(panel.background=element_rect(fill="white", colour="black"),
        axis.text.x=element_text(angle=90),
        axis.text=element_text(size=14),
        axis.title=element_text(size=18))+
  xlab("Date") + ylab("Landed Fish")

ggsave("../Figures/predicted.catch.2018.jpg", pred.true.2018)

pred.true.2017=ggplot(newdata.2017, aes(x=date, y=exp(pred.fish))) + 
  geom_point(alpha=0.3) +
  facet_wrap(~Community) +
  geom_point(aes(x=date, y=fish), colour="red")+
  theme(panel.background=element_rect(fill="white", colour="black"),
        axis.text.x=element_text(angle=90),
        axis.text=element_text(size=14),
        axis.title=element_text(size=18))
ggsave("../Figures/predicted.catch.2017.jpg", pred.true.2017)


### Add biomass averages to catch numbers
biomass.date=ddply(daily.cod.weath.pop.com, .(unit.area, date), summarize,
                   mn.wt=mean(mean.wt, na.rm=T))

daily.cod.weath.pop.com.wt=merge(daily.cod.weath.pop.com, biomass.date, by=c("unit.area", "date"))
daily.cod.weath.pop.com.wt$pred.wt=with(daily.cod.weath.pop.com.wt, pred.fish*mn.wt)

Est.catch=ddply(daily.cod.weath.pop.com.wt, .(Community, year), summarize,
                sum.observed=sum(fish, na.rm=T),
                sum.predicted=sum((pred.fish), na.rm=T),
                sum.predicted.wt=sum((pred.wt), na.rm=T),
                unit.area=unit.area[1])
sum(Est.catch$sum.predicted.wt)


write.csv(Est.catch, file="../Figures/Estimated.Catch.csv") 


###### total Commercial catch
tot.commercial.catch=sum(subset(catch, year==2018 & date %in% cod$date & 
                                  unit.area %in% unique(daily.cod.weath.pop.com$unit.area))$live.weight.kg, na.rm=T)

#### Community pop density vs predicted catch



library(effects)
comm.eff=effect("Community", weathermod.2018)
comm.eff=as.matrix(summary(comm.eff)$effect)
colnames(comm.eff)="comm.eff"
comm.eff.pop=merge(comm.eff, popdense, by.x=0, by.y="Name", all.x=T)
with(comm.eff.pop, plot(popdense, comm.eff))
with(comm.eff.pop, cor(popdense, comm.eff)^2)






###### Here we switch to NAFO region
#Commercial and rec cod catch
cod.nafo=ddply(cod, .(date, unit.area), summarise,
               sum.rec=sum(unique(data.frame(SurveyID, X.Fish)$X.Fish)),
               mean.rec=mean(X.Fish/X.Fishers, na.rm=T),
               mean.rec.len=mean(Length..cm., na.rm=T),
               SE.rec.len=sd(Length..cm., na.rm=T)/sqrt(sum(X.Fish, na.rm=T)),
               mean.rec.wt=mean(wt, na.rm=T),
               SE.rec.wt=sd(wt, na.rm=T)/sqrt(sum(X.Fish, na.rm=T)),
               sum.Fishers=sum(unique(data.frame(SurveyID, X.Fishers)$X.Fishers, na.rm=T)),
               sum.rec.wt=sum(wt, na.rm=T),
               Whales=sum(Whales, na.rm=T),
               Dolphins=sum(Dolphin, na.rm=T)>0,
               Seals=sum(Seal, na.rm=T),
               Capelin=sum(Capelin, na.rm=T),
               Berry=sum(Blackberry, na.rm=T))

cod.com=merge(cod.nafo, subcatch2[,c(1:3)], by=c("date", "unit.area"), all=T)
cod.com$year=format(cod.com$date, "%Y")
cod.com$jday=as.numeric(format(cod.com$date, "%j"))

sub.cod.com=subset(cod.com, (grepl("3L", unit.area)))


length.date.2018=ggplot(subset(sub.cod.com, !is.na(mean.rec.len) & year==2018), aes(x=date, y=mean.rec.len))+
  geom_point()+
  geom_errorbar(aes(x=date, ymin=mean.rec.len - 2*SE.rec.len, ymax=mean.rec.len + 2*SE.rec.len))+
  theme(axis.text.x=element_text(angle=90),
        panel.background=element_rect(fill="white", colour="black"),
        axis.title=element_text(size=18),
        axis.text=element_text(size=16))+
  geom_smooth(method="lm", se=F)+
  ylab("Mean length (cm)")+xlab("Date")+
  facet_wrap(~unit.area)

ggsave("../Figures/lengthvsdate.2018.jpg", length.date.2018)

### Average daily number fishers
Effort.date.2018=ggplot(subset(sub.cod.com, !is.na(sum.Fishers) & year==2018), aes(x=date, y=sum.Fishers))+
  geom_point()+
  theme(axis.text.x=element_text(angle=90),
        panel.background=element_rect(fill="white", colour="black"),
        axis.title=element_text(size=18),
        axis.text=element_text(size=14))+
  xlab("Date") +ylab("Number of Anglers") +
  geom_smooth(method="loess", se=F)+
  facet_wrap(~unit.area)

ggsave("../Figures/Effortvsdate.2018.jpg", Effort.date.2018)

### Average daily catch
Catch.date.2018=ggplot(subset(sub.cod.com, (!is.na(sum.Fishers) )&( sum.Fishers>5)), aes(x=date, y=mean.rec))+
  geom_point()+
  #geom_errorbar(aes(x=jday, ymin=mean.rec.len - 2*SE.rec.len, ymax=mean.rec.len + 2*SE.rec.len))+
  theme(axis.text.x=element_text(angle=90),
        panel.background=element_rect(fill="white", colour="black"),
        axis.title=element_text(size=18),
        axis.text=element_text(size=14))+
  geom_smooth(method="lm", se=F)+
  facet_wrap(~unit.area)+
  #geom_segment(aes(x=min(jday[Capelin==1]), xend=max(jday[Capelin==1]), y=1, yend=1), col="red")+
  geom_hline(yintercept=5)+
  ylab("Catch per Angler") +
  xlab("Date")
ggsave("../Figures/Catchvsdate.2018.jpg", Catch.date.2018)

### Total daily catch
sub.sub.cod.com=subset(sub.cod.com, grepl("3L", unit.area) & year==2018 &jday>140 &jday<270)

### rec vs commercial
rec.vs.com=subset(cod.com, year==2018 & jday>140 &(unit.area %in% c("3LA", "3LB", "3LF", "3LJ")),
                  select=c("date", "unit.area", "mean.rec.wt", "sum.rec.wt", "sum.Fishers", "sum.rec",
                           "live.weight.kg", "year", "Capelin", "Berry", "Whales", "jday"))
colnames(rec.vs.com)[colnames(rec.vs.com)=="live.weight.kg"]="Commercial"
colnames(rec.vs.com)[colnames(rec.vs.com)=="sum.rec.wt"]="Recreational"
colnames(rec.vs.com)[colnames(rec.vs.com)=="sum.rec.wt"]="Fish Caught"

melt.rec.vs.com=melt(rec.vs.com, id.vars=c("date", "unit.area","jday", "year"))

rec.vs.com.2018=ggplot(subset(melt.rec.vs.com, variable %in% c("Recreational", "Commercial")), 
       aes(x=date, y=value, fill=NULL))+
  geom_point()+
  facet_grid(variable~unit.area, scale="free_y")+
  geom_smooth(method="loess", se=F)+
  geom_vline(xintercept=as.Date("2018-08-12", format="%Y-%m-%d"), colour="grey")+
  theme(panel.background=element_rect(fill="white", colour="black"),
        axis.title=element_text(size=18),
        axis.text=element_text(size=16))+
  xlab("Date") + ylab("Landed weight (kg)")

ggsave("../Figures/rec.vs.com.2018.jpg", rec.vs.com.2018)


rec.vs.cap.2018=ggplot(subset(melt.rec.vs.com, variable %in% c("sum.rec", "Capelin", "mean.rec.wt") &
                                unit.area %in% c("3LB", "3LF") & jday<250), 
                       aes(x=date, y=value, fill=NULL))+
  geom_point()+
  facet_grid(variable~unit.area, scale="free_y")+
  geom_smooth(method="loess", se=F)+
  geom_vline(xintercept=as.Date("2018-08-12", format="%Y-%m-%d"))+
  theme(panel.background=element_rect(fill="white", colour="black"))

ggsave("../Figures/rec.vs.cap.2018.jpg", rec.vs.cap.2018)





## Newfoundland Map
library(maptools)
Canada=readShapePoly("GIS/")
Canada_3=readRDS("/Users/tjb1a13/Google Drive/DFO/Citizen Cod/Data/GIS/gadm36_CAN_3_sp.rds")
NL=subset(Canada_3, grepl("Newfoundland", NAME_1))
#plot(NL)
head(NL@data)


#### Figures
ave.daily.com=ddply(cod.dat, .(Community, Year), summarise,
                    mean.daily.surveys=mean(surveys, na.rm=T),
                    #se.daily.surveys=sd(surveys, na.rm=T)/sqrt(length(unique(julian))),
                    mean.sum.wt=mean(sum.wt, na.rm=T),
                    mean.catch.rate=mean(meancatch, na.rm=T),
                    unit.area=unit.area[1],
                    popdense=popdense[1])
#se.sum.wt=sd(sum.wt, na.rm=T)/sqrt(length(unique(julian))))


melt.daily.com=melt(ave.daily.com, id.vars=c("Community", "year", 
                                             "popdense", "unit.area" ))

#### plot population density vs catch and effort
popdenseplot=ggplot(subset(ave.daily.com, !Community %in% c("Carbonear", "Embree", "Foxtrap")), 
                    aes(x=popdense, y=mean.sum.wt))+
  geom_point()+
  ylab("Average daily landed weight (kg)")+
  xlab("Community population density (km2)")+
  theme(axis.text.x=element_text(angle=90),
        axis.text=element_text(size=14),
        axis.title=element_text(size=18),
        panel.background=element_rect(fill="white", colour="black"))+
  #facet_wrap(~variable, scale="free_y")+
  geom_smooth(method="lm")

ggsave("../Figures/popdenseplot.jpg", popdenseplot)

##### weather influences fishing effort
sub.weatherplot=subset(daily.cod.weath.pop.com, !Community %in% c("Embree","Dildo", "Dover","Hare Bay","Harbour Grace", "Wareham", "Winterton", "Carbonear", "New Harbour", "Exploits"))
#sub.weatherplot=subset(daily.cod.weath.pop.com, Community %in% c("Foxtrap", "Hant's Harbour"))
plot.weather=ggplot(sub.weatherplot, aes(x=mean.wind, y=nfishers))+
  geom_point()+
  facet_wrap(~Community, scale="free_y", ncol=3)+
  geom_smooth(method="lm") +
  theme(panel.background=element_rect(fill="white", colour="black"),
        axis.text=element_text(size=14),
        axis.title=element_text(size=18))+
  ylab("Number of Anglers")+
  xlab("Mean wind strength (km/h)")

ggsave("../Figures/plot.weather.jpg", plot.weather)


cumsumfun=function(x) {cumsum(ifelse(is.na(x), 0, x))/sum(x, na.rm=T)}

for(j in c(2017, 2018)){
  ind1=which(daily.cod.weath.pop.com$year==j)
  daily.cod.weath.pop.com$cum.cap[ind1]=cumsumfun(daily.cod.weath.pop.com$Capelin[ind1])
  daily.cod.weath.pop.com$cum.whale[ind1]=cumsumfun(daily.cod.weath.pop.com$Whales[ind1])
  for(i in unique(daily.cod.weath.pop.com$unit.area)){
    ind=which(daily.cod.weath.pop.com$unit.area==i & daily.cod.weath.pop.com$year==j)
    daily.cod.weath.pop.com$cum.rec[ind]=cumsumfun(daily.cod.weath.pop.com$sum.wt[ind])
    daily.cod.weath.pop.com$cum.com[ind]=cumsumfun(daily.cod.weath.pop.com$live.weight.kg[ind])
  }}


#### Unused plots

cumtrends=subset(daily.cod.weath.pop.com, unit.area %in% c( "3LA", "3LB", "3LF", "3LJ") & 
                   year==2018 & julian >175, 
                 select=c("date", "cum.rec", "cum.cap", "cum.com", "unit.area"))

melt.cumtrends=melt(cumtrends, id.vars=c("date", "unit.area"))
melt.cumtrends$Cumulative=ifelse(melt.cumtrends$variable=="cum.rec", "Recreational", 
                                 ifelse(melt.cumtrends$variable=="cum.com", "Commercial", "Capelin"))

cum.trends=ggplot(melt.cumtrends, aes(x=date, y=value, colour=Cumulative)) + 
  geom_line()+
  facet_wrap(~unit.area)+
  ylab("Cumulative Fraction")+
  theme(panel.background=element_rect(fill="white", colour="black"))

ggsave("../Figures/Cumulative.trends.jpg", cum.trends)