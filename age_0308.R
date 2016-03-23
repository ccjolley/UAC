library(ggplot2)
library(lubridate)
library(reshape2)
Sys.setlocale("LC_TIME", "usa")

setwd("C:/Users/Craig/Desktop/UAC")
source('UAC_load.R')
all_uac$age <- as.numeric(all_uac$age)
#all_uac$date <- ymd(all_uac$date)
all_uac <- unique(all_uac)

# Clean up locations
geo_unique <- read.csv('allnames-0302.csv',stringsAsFactors=FALSE)
geo_unique <- geo_unique[!is.na(geo_unique$lat),]
geo_unique <- geo_unique[geo_unique$lat != 0,]
geo_unique[geo_unique$country=='GUATEMALA' & !is.na(geo_unique$country),'country'] <- 'Guatemala'
geo_unique[geo_unique$country=='EL SALVADOR' & !is.na(geo_unique$country),'country'] <- 'El Salvador'
names(geo_unique) <- c('lat','lon','country','admin1','address','type','locstr','str2','count')
geo_unique[geo_unique$admin1 == 'San Marcos Department','admin1'] <- 'San Marcos'


# First, I'm curious about age today. What does the distribution look like?
all_uac$age <- as.numeric(all_uac$age)
ggplot(data=all_uac,aes(x=age)) +
  geom_bar(binwidth=1,fill='slategray3',color='gray12',origin=-0.5) +
  theme_classic() +
  theme(text=element_text(size=20),
        axis.line=element_blank(),axis.ticks=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank())


# Has it changed over time?
uac_date <- all_uac[,c('locstr','date','age')]
uac_date$date <- ymd(uac_date$date)
uac_date <- uac_date[!is.na(uac_date$date),]
uac_date$month <- month(uac_date$date)
uac_date$year <- year(uac_date$date)
uac_date <- uac_date[order(uac_date$date),]
uac_date$monthnum <- (uac_date$year - uac_date$year[1])*12 + (uac_date$month - uac_date$month[1])
uac_date$age <- as.numeric(uac_date$age)
m <- ddply(uac_date,'monthnum',summarize,mean=mean(age),
           conf95_lo=t.test(age)$conf.int[1],conf95_hi=t.test(age)$conf.int[2])
m$date <- uac_date$date[1] + months(m$monthnum)
mm <- melt(m,id.vars='monthnum')
ggplot(m,aes(x=date,y=mean,ymin=conf95_lo,ymax=conf95_hi)) +
  geom_line(size=3,color='tomato') +
  geom_errorbar(width=1) +
  labs(y='Mean age',x='') +
  theme_classic() +
  theme(text=element_text(size=20),
        axis.line=element_blank(),axis.ticks=element_blank())
# This could be prettier, but it gets my point across: When volume is high, people 
# send younger kids. This makes sense -- those are times when they see benefits 
# as outweighing risks.

# To interpret this well, I need a plot of the overall time course
ggplot(all_uac,aes(x=date)) +
  geom_histogram(binwidth=1,fill='chocolate4') +
  labs(y='Daily count',x='') +
  theme_classic() +
  theme(text=element_text(size=20),
        axis.line=element_blank(),axis.ticks=element_blank())


# There's a problem here; months 0 and 15 are only 15 days long, because 
# reporting began on 6/16/2014 and ended on 9/15/2015. Also, months 7 and 8
# have been cut in half due to a data gap in the middle of the series.
uac_date$day <- day(uac_date$date)
uac_date[uac_date$day > 15,'monthnum'] <- uac_date[uac_date$day > 15,'monthnum'] + 0.5
uac_date[uac_date$monthnum==7,'monthnum'] <- 7.5
m2 <- ddply(uac_date,'monthnum',summarize,mean=mean(age),count=length(age))
quantile(uac_date[uac_date$monthnum==6,'date'])
summary(lm(m2$count ~ m2$mean)) # rather significant
ggplot(m2,aes(x=count,y=mean)) +
  geom_smooth(method='lm',size=2) +
  geom_point(size=10,color='tomato') +
  theme_classic() +
  labs(y='Mean age','Overall count') +
  theme(text=element_text(size=20),
        axis.line=element_blank(),axis.ticks=element_blank())


# Average age for admin 1 units? 
# Any significantly above or below the regional average?
tmp <- uac_date[,c('locstr','date','age')]
j <- join(tmp,geo_unique,by='locstr')
adm1 <- unique(geo_unique[,c('country','admin1')])
sig <- ldply(1:nrow(adm1), function(i) {
  country <- adm1[i,'country']
  admin1 <- adm1[i,'admin1']
  avg <- mean(j[j$country==country & j$admin1==admin1,'age'])
  pval <- 1
  tryCatch({
    tt <- t.test(j[j$country==country & j$admin1==admin1,'age'],
                 j[j$country!=country | j$admin1!=admin1,'age'],
                 alternative='less')
    avg <- tt$estimate[1]
    pval <- tt$p.value
  }, error=function(e) {})
  data.frame(country=country,admin1=admin1,avg=avg,pval=pval)
})
sig$sig <- as.numeric(sig$pval < 0.01)
sig <- na.omit(sig)

source('NT-isocodes.R')
sig$admin1 <- as.character(sig$admin1)
writeme <- join(sig,shp_clean,by=c('country','admin1'))
writeme[is.na(writeme$adm1_code),]
write.csv(writeme,'adm1-age-0307.csv',row.names=FALSE)
# All of the places with significantly younger kids were HND/SLV. 
# Put them on the map!