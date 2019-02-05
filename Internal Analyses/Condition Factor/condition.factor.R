##### Exploratory Condition Factor Analysis
setwd("~/Google Drive/Hakai Personal/SEMSP/Analyses/Condition Factor")
lab2015 <- read.csv(file="lab2015.csv", stringsAsFactors = FALSE)
stockid <- read.csv(file="stock.id.csv")
library(plyr)
library(dplyr)

theme_set(theme_gray(base_size=18))
# Correct date class
class(lab2015$date)
date<-lab2015$date
date<-as.Date(date,"%Y-%m-%d") #formats temporary date object to correct date format
class(date)#checks for correct class (date)
head(date)
lab2015$date<-date<-as.Date(date,"%y-%m-%d")#applies new date format to semsp15 dataframe
class(lab2015$date)#checks to ensure class of date is not vector but rather date
rm(date)

class(lab2015$fork.length)
length <- lab2015$fork.length
length <- as.numeric(length)
lab2015$fork.length <- length
range(length, na.rm= TRUE)
rm(length)

class(lab2015$weight)

#Assign migration routes based on site code
lab2015$migration.route[lab2015$site.id == "D09" | lab2015$site.id == "D07"| lab2015$site.id == "D03"] <- "central"
lab2015$migration.route[lab2015$site.id == "D06"] <- "west"
lab2015$migration.route[lab2015$site.id == "D01" | lab2015$site.id == "D02" | lab2015$site.id == "D05" |lab2015$site.id == "D08"] <- "east"


### assign date_groups to summarise by, based on 7 day averages
lab2015$date_group[lab2015$date >= as.Date("2015-05-02") & lab2015$date <= as.Date("2015-05-08")] <- 1
lab2015$date_group[lab2015$date >= as.Date("2015-05-09") & lab2015$date <= as.Date("2015-05-15")] <- 2
lab2015$date_group[lab2015$date >= as.Date("2015-05-16") & lab2015$date <= as.Date("2015-05-22")] <- 3
lab2015$date_group[lab2015$date >= as.Date("2015-05-23") & lab2015$date <= as.Date("2015-06-29")] <- 4
lab2015$date_group[lab2015$date >= as.Date("2015-05-30") & lab2015$date <= as.Date("2015-06-05")] <- 5
lab2015$date_group[lab2015$date >= as.Date("2015-06-06") & lab2015$date <= as.Date("2015-06-12")] <- 6
lab2015$date_group[lab2015$date >= as.Date("2015-06-13") & lab2015$date <= as.Date("2015-06-19")] <- 7
lab2015$date_group[lab2015$date >= as.Date("2015-06-20") & lab2015$date <= as.Date("2015-06-26")] <- 8
lab2015$date_group[lab2015$date >= as.Date("2015-06-27") & lab2015$date <= as.Date("2015-07-03")] <- 9
lab2015$date_group[lab2015$date >= as.Date("2015-07-04") & lab2015$date <= as.Date("2015-07-10")] <- 10
lab2015$date_group[lab2015$date >= as.Date("2015-07-11") & lab2015$date <= as.Date("2015-07-18")] <- 11

### assign region column to summarise by
lab2015$region[lab2015$site.id == as.character("J01") | lab2015$site.id == as.character("J02") | lab2015$site.id == as.character("J03") | lab2015$site.id == as.character("J04") | lab2015$site.id == as.character("J05") | lab2015$site.id == as.character("J06") | lab2015$site.id == as.character("J07") | lab2015$site.id == as.character("J08") | lab2015$site.id == as.character("J09") | lab2015$site.id == as.character("J10")] <- "js"
lab2015$region[lab2015$site.id == as.character("D07") | lab2015$site.id == as.character("D01") | lab2015$site.id == as.character("D02") | lab2015$site.id == as.character("D03") | lab2015$site.id == as.character("D04") | lab2015$site.id == as.character("D05") | lab2015$site.id == as.character("D06")] <- "di"
lab2015$region[lab2015$site.id == as.character("D08") | lab2015$site.id == as.character("D09") | lab2015$site.id == as.character("D10")| lab2015$site.id == as.character("D11")] <- "di"

#combine contents of two columns into one to create unique seine ID's to group by later
lab2015$seine <- paste(lab2015$date, lab2015$site.id)
class(lab2015$seine)
lab2015$seine <- as.factor(lab2015$seine)
class(lab2015$seine)
rm(stockid)
lab2015 <- subset(lab2015, species == "SO") 
lab2015 <- select(lab2015,date, site.id, species, ufn, weight, fork.length, date_group,region)
lab2015 <- mutate(lab2015, k = (weight/(fork.length ^3))*100000)


### Import and alter 2016 data
lab2016 <- read.csv(file = "lab2016.csv", stringsAsFactors = FALSE) 

lab2016 <- select(lab2016, date, site.id, species,universal.fish.number, weight.g., fork.length.mm.)
lab2016 <- subset(lab2016, species == "SO")

class(lab2016$date)
date<-lab2016$date
date<-as.Date(date,"%y-%m-%d") #formats temporary date object to correct date format
class(date)#checks for correct class (date)
head(date)
lab2016$date<-date<-as.Date(date,"%y-%m-%d")#applies new date format to semsp15 dataframe
class(lab2016$date)#checks to ensure class of date is not vector but rather date
rm(date)

class(lab2016$fork.length)
length <- lab2016$fork.length.mm.
length <- as.numeric(length)
lab2016$fork.length.mm. <- as.numeric(length)
class(lab2016$fork.length.mm.)
rm(length)

class(lab2016$weight.g.)
weight <- lab2016$weight.g
weight <- as.numeric(weight)
lab2016$weight.g. <- as.numeric(weight)
class(weight)
rm(weight)

#Categorize dates into groups/ranges for CPUE calculation; based on 7 day aggregates
lab2016$date_group[lab2016$date >= as.Date("2016-05-02") & lab2016$date <= as.Date("2016-05-08")] <- 1
lab2016$date_group[lab2016$date >= as.Date("2016-05-09") & lab2016$date <= as.Date("2016-05-15")] <- 2
lab2016$date_group[lab2016$date >= as.Date("2016-05-16") & lab2016$date <= as.Date("2016-05-22")] <- 3
lab2016$date_group[lab2016$date >= as.Date("2016-05-23") & lab2016$date <= as.Date("2016-06-29")] <- 4
lab2016$date_group[lab2016$date >= as.Date("2016-05-30") & lab2016$date <= as.Date("2016-06-05")] <- 5
lab2016$date_group[lab2016$date >= as.Date("2016-06-06") & lab2016$date <= as.Date("2016-06-12")] <- 6
lab2016$date_group[lab2016$date >= as.Date("2016-06-13") & lab2016$date <= as.Date("2016-06-19")] <- 7
lab2016$date_group[lab2016$date >= as.Date("2016-06-20") & lab2016$date <= as.Date("2016-06-26")] <- 8
lab2016$date_group[lab2016$date >= as.Date("2016-06-27") & lab2016$date <= as.Date("2016-07-03")] <- 9
lab2016$date_group[lab2016$date >= as.Date("2016-07-04") & lab2016$date <= as.Date("2016-07-10")] <- 10
lab2016$date_group[lab2016$date >= as.Date("2016-07-11") & lab2016$date <= as.Date("2016-07-18")] <- 11

lab2016$region[lab2016$site.id == as.character("J01") | lab2016$site.id == as.character("J02") | lab2016$site.id == as.character("J03") | lab2016$site.id == as.character("J04") | lab2016$site.id == as.character("J05") | lab2016$site.id == as.character("J06") | lab2016$site.id == as.character("J07") | lab2016$site.id == as.character("J08") | lab2016$site.id == as.character("J09") | lab2016$site.id == as.character("J10")] <- "js"
lab2016$region[lab2016$site.id == as.character("D07") | lab2016$site.id == as.character("D01") | lab2016$site.id == as.character("D02") | lab2016$site.id == as.character("D03") | lab2016$site.id == as.character("D04") | lab2016$site.id == as.character("D05") | lab2016$site.id == as.character("D06") | lab2016$site.id == as.character("D08") | lab2016$site.id == as.character("D09") | lab2016$site.id == as.character("D10")| lab2016$site.id == as.character("D11") | lab2016$site.id == as.character("D20") | lab2016$site.id == as.character("D21") | lab2016$site.id == as.character("D22") | lab2016$site.id == as.character("D23") | lab2016$site.id == as.character("D24") | lab2016$site.id == as.character("D25") | lab2016$site.id == as.character("D26") | lab2016$site.id == as.character("D27") | lab2016$site.id == as.character("D28")] <- "di"

#Calculate condition for 2016

class(lab2016$weight.g.)
lab2016 <- mutate(lab2016, k = ((weight.g./(fork.length.mm. ^3))*100000))

k <- lab2016$k
class(k)
hist(k)

#### Create mean weight length regression slope for both years and both regions combined

weight2015 <- as.numeric(lab2015$weight)
class(weight2015)
weight2016 <- as.numeric(lab2016$weight.g.)
class(weight2016)

length2015<- as.numeric(lab2015$fork.length)
length2016 <- as.numeric(lab2016$fork.length.mm.)

t15 <-cbind(weight2015, length2015)
t16 <- cbind(weight2016, length2016)

t15<- as.data.frame(t15)
t16 <- as.data.frame(t16)
colnames(t15) <- c("weight2016", "length2016")
wl<- bind_rows(t15,t16)
colnames(wl) <- c("weight", "length")

wl <- mutate(wl, logw = log10(weight))
wl <- mutate(wl, logl = log10(length))
library(ggplot2)
library(mgcv)
lwreg <- ggplot(wl, aes(logl, logw)) +
  geom_point()+
  geom_smooth(method=lm)
lwreg
### removing row because it was exerting significant leverage on the regression and seemd to be an improperly measured fish
wl[1837,]
wl <- wl[-1837,]

logl <- wl$logl
logw <- wl$logw
#Create linear model based on log transformed length and weight from all fish captured in 2015 and 2016 in both regions

fit <- lm(logw~logl)
summary(fit)
write.csv(wl,file='weight.length.csv')
coef(fit)
fitted(fit)

### Fit length weight regression to 2015 and 2016 seperately so I can interpret b value annualy
#2015
logl2015 <- log10(t15$length)
logw2015 <- log10(t15$weight)
fit2015 <- lm(logw2015~logl2015)
coef(fit2015)
plot(fit2015)

#2016
t16[511,]
t16 <- t16[-511,]
logl2016 <- log10(t16$length)
logw2016 <- log10(t16$weight)
fit2016 <- lm(logw2016~logl2016)
coef(fit2016)
plot(fit2016)

#apply equation parameters from regression above to determine expected weight based on length, and then calculate relative condition.
#expected weight = 10^(intercept) + b(length)
#first create logweight column in each of 2016 and 2015 files to run relative weight equation from
#next take the anti log of intercept (10^a)

#Based on all individuals from 2015 and 2016:

#lab2015 <- mutate(lab2015, w.prime = 10^-4.549814*(fork.length^2.782762))
#lab2015 <- mutate(lab2015, Kn = weight / w.prime)

#lab2016 <- mutate(lab2016, w.prime = 10^-4.549814*(fork.length.mm.^2.782762))
#lab2016 <- mutate(lab2016, Kn = weight.g. / w.prime)

#compute based on individual year regression:

#lab2015 <- mutate(lab2015, w.prime = 10^-4.356619*(fork.length^2.698358))
#lab2015 <- mutate(lab2015, Kn = weight / w.prime)

#lab2016 <- mutate(lab2016, w.prime = 10^-4.214880*(fork.length.mm.^2.601352))
#lab2016 <- mutate(lab2016, Kn = weight.g. / w.prime)

#Based on geometric mean of 2015 and 2016 regresion parameters

lab2015 <- mutate(lab2015, w.prime = 10^-4.285164*(fork.length^2.649411))
lab2015 <- mutate(lab2015, Kn = weight / w.prime)

lab2016 <- mutate(lab2016, w.prime = 10^-4.285164*(fork.length.mm.^2.649411))
lab2016 <- mutate(lab2016, Kn = weight.g. / w.prime)
library(lubridate)
lab2016 <- filter(lab2016, Kn < 3)
lab2016$j.date <- yday(lab2016$date)
lab2015$j.date <- yday(lab2015$date)

check2015 <- ggplot(lab2015, aes(log10(fork.length), log10(Kn), colour = region))+
  geom_point()+
  geom_smooth(method='lm')
check2015 +ggtitle("2015")

check2016 <- ggplot(lab2016, aes(fork.length.mm., Kn, colour = region))+
  geom_point()+
  geom_smooth(method='lm')
check2016 + ggtitle ("2016")

another.check <- ggplot (lab2015, aes(j.date, Kn))+
  geom_point(colour="#CC0000")+
  geom_point(data=lab2016, aes(j.date, Kn))+
  geom_smooth(data=lab2016,method='lm')+
  geom_smooth(data=lab2015, method='lm',colour="#CC0000")
another.check

check2015.by.date <- ggplot(lab2015, aes(log10(fork.length), log10(Kn), colour = date_group))+
  geom_point()+
  geom_smooth(method='lm')
check2015.by.date +ggtitle("2015")

check2016.by.date <- ggplot(lab2016, aes(log10(fork.length.mm.), log10(Kn), colour = date_group))+
  geom_point()+
  geom_smooth(method='lm')
check2016.by.date+ggtitle("2016")

lab2015 <- lab2015[!is.na(lab2015$Kn),]
lab2016 <- lab2016[!is.na(lab2016$Kn),]

?geom_smooth
### Now I can move on to summarizing condition based on stock for 2015, or by migration route or region.
library(plyr)
###Boxplot of condition of DI fish per day over the whole season
stockid <- read.csv(file="stock.id.csv")
di2015 <- filter(lab2015, region == "di")
di2015.stk<- left_join(di2015, stockid, by = "ufn")
di2015.stk <- filter(di2015.stk, Region.1 == "1" | Region.1 == "2" | Region.1 == "3" | Region.1 == "4")

di.cond.boxplot <- ggplot(di2015.stk, aes(factor(date), y = Kn))+
  geom_boxplot()

di.cond.boxplot + theme(axis.text.x  = element_text(angle=60, vjust=0.5))+
  xlab("Date")+
  ylab("Relative Condition Factor (Kn)")+
  ggtitle("Fraser River Sockeye Condition")


###Boxplot of length of DI fish per day over the whole season
di.length <- ggplot(di2015.stk, aes(factor(date), y = fork.length))+
  geom_boxplot()

di.length + theme(axis.text.x  = element_text(angle=60, vjust=0.5))

####Boxplot of condition of JS fish over the whole season by day
js2015 <- filter(lab2015, region == "js")
js2015.stk<- left_join(js2015, stockid, by = "ufn")
js2015.stk <- filter(js2015.stk, Region.1 == "1" | Region.1 == "2" | Region.1 == "3" | Region.1 == "4")

js.cond.boxplot <- ggplot(js2015.stk, aes(factor(date), y = Kn))+
  geom_boxplot()
js.cond.boxplot + theme(axis.text.x  = element_text(angle=60, vjust=0.5))

##Boxplot length over season in JS by day
js.length.boxplot <- ggplot(js2015.stk, aes(factor(date), y = fork.length))+
  geom_boxplot()

js.length.boxplot + theme(axis.text.x  = element_text(angle=60, vjust=0.5))


##### Make data frame to compare 2015 and 2016 conditions factor


#############################
# 2015 Plots based on stock ID (anything done to compare 2015 and 2016 must be done above this code becuase lab2015 is redefined below)
#############################
library(gridExtra)
lab2015 <- right_join(lab2015, stockid, by = "ufn")
lab2015 <- filter(lab2015, Region.1 == "1" | Region.1 == "2" | Region.1 == "3" | Region.1 == "4")

cond.2015 <- ggplot(lab2015, aes(factor(date_group),y=Kn, fill = region))+
    geom_boxplot()  + scale_x_discrete(limits=c("3", "4", "5","6","7","8","9"),
                               labels=c("May 19", "May 26", "June 2", "June 9","June 16", "June 23", "June 30"))+
  xlab("7 Day Aggregates")+
  ylab("Relative Condition Factor (Kn)")+
  coord_cartesian(ylim = c(.55, 1.65))+
  scale_fill_discrete(name="Region",
                      breaks=c("di", "js"),
                      labels=c("Discovery Islands", "Johnstone Strait"))+
  theme(legend.text= element_text(size=16))+
  theme(legend.title = element_text( size = 16))+
  theme(legend.position=c(.8,.1))+
  theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")) + ggtitle("2015")

cond.2015
length.2015 <- ggplot(lab2015, aes(factor(date_group), y = fork.length, fill=region))+
    geom_boxplot() + scale_x_discrete(limits=c("3", "4", "5","6","7","8","9"),
                 labels=c("May 19", "May 26", "June 2", "June 9","June 16", "June 23", "June 30"))+
                xlab("7 Day Aggregates")+
                ylab("Fork Length (mm)")+
                coord_cartesian(ylim = c(70, 150))+
  scale_fill_discrete(name="Region",
                      breaks=c("di", "js"),
                      labels=c("Discovery Islands", "Johnstone Strait"))+
  theme(legend.text= element_text(size=16))+
  theme(legend.title = element_text( size = 16))+
  theme(legend.position=c(.8,.1))+
  theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))+ggtitle("2015")

length.2015

#######
#2016 summary
#######

condition.2016 <- ggplot(lab2016, aes(factor(date_group), y = Kn, fill = region))+
  geom_boxplot()+ coord_cartesian(ylim = c(.6, 1.5)) + scale_x_discrete(limits=c("3", "4", "5","6","7","8","9"),
                                                                       labels=c("May 19", "May 26", "June 2", "June 9","June 16", "June 23", "June 30"))+
  xlab("7 Day Aggregates")+
  ylab("Relative Condition Factor (Kn)")+
  coord_cartesian(ylim = c(.55, 1.65))+
  scale_fill_discrete(name="Region",
                      breaks=c("di", "js"),
                      labels=c("Discovery Islands", "Johnstone Strait"))+
  theme(legend.text= element_text(size=16))+
  theme(legend.title = element_text( size = 16))+
  theme(legend.position=c(.8,.1))+
  theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")) + ggtitle("2016")

condition.2016

length.2016 <- ggplot(lab2016, aes(factor(date_group), y= fork.length.mm., fill= region))+
  geom_boxplot() + scale_x_discrete(limits=c("3", "4", "5","6","7","8","9"),
                                    labels=c("May 19", "May 26", "June 2", "June 9","June 16", "June 23", "June 30"))+
  xlab("7 Day Aggregates")+
  ylab("Fork Length (mm)")+
  coord_cartesian(ylim = c(70, 150))+
  scale_fill_discrete(name="Region",
                      breaks=c("di", "js"),
                      labels=c("Discovery Islands", "Johnstone Strait"))+
  theme(legend.text= element_text(size=16))+
  theme(legend.title = element_text( size = 16))+
  theme(legend.position=c(.8,.1))+
  theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))+ ggtitle("2016")
length.2016

###
###compare years

grid.arrange(cond.2015, condition.2016, nrow=2, ncol=1)

######
# COMPARE SPECIFIC STOCKS BETWEEN REGIONS
######

compare <- ddply(lab2015, c("region","date_group", "Stock.1"), summarise,
                 N= length(Kn),
                 mean.Kn= (mean(Kn)),
                 sd = sd(Kn),
                 se = sd / sqrt(N)
)

compare3 <- filter(compare, N >= 3)
### average the Kn of the 5 biggest stocks to see if their condition factor changes between regions

compare.big.5 <- filter(compare, Stock.1 == "Chilko" | Stock.1 == "L_Adams" | Stock.1 == "Mitchell" | Stock.1 == "Quesnel_Horsef" | Stock.1 == "Seymour")



compare.big.5.no.stock <- ddply(compare.big.5, c("region", "date_group"), summarise,
                                N= length(mean.Kn),
                                mean.Kn= (mean(mean.Kn)),
                                sd = sd(mean.Kn),
                                se = sd / sqrt(N)
)

compare.big.5.no.stock <- filter(compare.big.5.no.stock, N >= 3)
big5.cond <- ggplot (compare.big.5.no.stock, aes(x=date_group, y = mean.Kn, group = region, colour = region))+
  # geom_errorbar(aes(ymin=mean.Kn - sd, ymax=mean.Kn+sd), width = .1, postion=pd)+
  geom_point()+
  geom_line()

big5.cond
### Compare four most numerous stocks between JS and DI
compare.mitchell <- filter(compare, Stock.1 == "Mitchell" & N>=3)
mitchell.cond <- ggplot (compare.mitchell, aes(x=date_group, y = mean.Kn, group = region, colour = region))+
  # geom_errorbar(aes(ymin=mean.Kn - sd, ymax=mean.Kn+sd), width = .1, postion=pd)+
  geom_point()+
  geom_line()

mitchell.cond

compare.Seymour <- filter(compare, Stock.1 == "Seymour" & N >= 3)
seymour.cond <- ggplot (compare.Seymour, aes(x=date_group, y = mean.Kn, group = region, colour = region))+
  # geom_errorbar(aes(ymin=mean.Kn - sd, ymax=mean.Kn+sd), width = .1, postion=pd)+
  geom_point()+
  geom_line()
seymour.cond

compare.L_Adams <- filter (compare, Stock.1== "L_Adams" & N >= 3)
L_Adams.cond <- ggplot (compare.L_Adams, aes(x=date_group, y = mean.Kn, group = region, colour = region))+
  # geom_errorbar(aes(ymin=mean.Kn - sd, ymax=mean.Kn+sd), width = .1, postion=pd)+
  geom_point()+
  geom_line()
L_Adams.cond

### Compare relative condition factor of chilko Lake Stocks over the entire season.
compare.chilko <- filter(compare, Stock.1 == "Chilko" & N >= 3)
pd <- position_dodge(.11)
chilko.cond <- ggplot (compare.chilko, aes(x=date_group, y = mean.Kn, group = region, colour = region))+
  # geom_errorbar(aes(ymin=mean.Kn - sd, ymax=mean.Kn+sd), width = .1, postion=pd)+
  geom_point()+
  geom_line()

chilko.cond
##Make a boxplot of distributions of Kn based on week aggregates to compare relative condition of chilko fish
una.chilko <- filter(lab2015, Stock.1 == "Chilko")
chilko.box.plot <- ggplot(una.chilko, aes(factor(date_group), y = Kn ))+
  geom_boxplot(aes(fill=region))+
  scale_x_discrete(limits=c("3", "4", "5","6","7","8"),
                   labels=c("May 19", "May 26", "June 2", "June 9","June 16", "June 23"))

chilko.box.plot +
  ylim(0.7,1.5)+
  xlab("7 Day Aggregates")+
  ylab("Relative Condition Factor (Kn)")+
  scale_fill_discrete(name="Region",
                      breaks=c("di", "js"),
                      labels=c("Discovery Islands", "Johnstone Strait"))+
  theme(legend.text= element_text(size=16))+
  theme(legend.title = element_text( size = 16))+
  theme(legend.position= c(.8,.1))+
  theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))+
  ggtitle("Chilko Lake Sockeye")

##########
#Make HISTOGRAMS OF LENGTH AND CONDITION TO COMPARE 2015 AND 2016
##########

#First need to make a dataframe that combines both years lengths and condition factors 
l.kn.2015 <- select(lab2015, fork.length, Kn)
l.kn.2015$year <- "2015"


l.kn.2016 <- select(lab2016, fork.length.mm., Kn)
l.kn.2016$year <- "2016"
colnames(l.kn.2016) <- c("fork.length", "Kn", "year")

l.kn <- bind_rows(l.kn.2015, l.kn.2016)
##filter out fish that have Kn greater than 2 becasue I think these are measurement errors
l.kn <- filter(l.kn, Kn < 2)
length.histo.2015 <-ggplot(lab2015, aes(fork.length))+
  geom_density()
  #geom_histogram(binwidth=4)
length.histo.2015 + geom_density()

length.histo.2016 <- ggplot(lab2016, aes(fork.length.mm.)) + 
  geom_density()
  geom_histogram(binwidth=4)
length.histo.2016

## Plot both years on the same graph


length.histo <- ggplot(l.kn, aes(x=fork.length, fill = year))+
  geom_histogram(aes(y=5*..density..*100),
                 alpha=1, position='identity', binwidth=5)+
  facet_wrap(~year, nrow = 2)

length.histo +
  xlab("Fork Length (mm)")+
  ylab("Frequency (%)")+
  coord_cartesian(x= 60:150)


cond.histo <- ggplot(l.kn, aes(x=Kn, fill = year))+
  geom_histogram(aes(y=0.04*..density..*100),
                 alpha=1, position='identity', binwidth=.04)+
  facet_wrap(~year, nrow=2)

cond.histo +
  xlab("Relative Condition Factor (Kn)")+
  ylab("Frequency (%)")+
  coord_cartesian(x=0.5:1.7)



