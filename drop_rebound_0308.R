library(ggplot2)
library(lubridate)
library(reshape2)
Sys.setlocale("LC_TIME", "usa")

setwd("C:/Users/Craig/Desktop/UAC")
source('UAC_load.R')
all_uac$age <- as.numeric(all_uac$age)
all_uac$date <- ymd(all_uac$date)
all_uac <- unique(all_uac)

# Clean up locations
geo_unique <- read.csv('allnames-0302.csv',stringsAsFactors=FALSE)
geo_unique <- geo_unique[!is.na(geo_unique$lat),]
geo_unique <- geo_unique[geo_unique$lat != 0,]
geo_unique[geo_unique$country=='GUATEMALA' & !is.na(geo_unique$country),'country'] <- 'Guatemala'
geo_unique[geo_unique$country=='EL SALVADOR' & !is.na(geo_unique$country),'country'] <- 'El Salvador'
names(geo_unique) <- c('lat','lon','country','admin1','address','type','locstr','str2','count')
geo_unique[geo_unique$admin1 == 'San Marcos Department','admin1'] <- 'San Marcos'

# Looking at the overall time course, I think the drop/rebound dynamic seems to apply everywhere.
# Where is it the most pronounced? Compare peak level with lowest dip.

tmp <- geo_unique[,c('lat','lon','locstr','address')]
j <- join(all_uac,tmp,by='locstr')
uac_date <- j[,c('lat','lon','date','age','address','locstr','country')]
rm(tmp,j)
uac_date$date <- ymd(uac_date$date)
uac_date <- uac_date[order(uac_date$date),]
t <- as.data.frame(table(uac_date$date))
names(t) <- c('date','count')
t$date <- as.Date(t$date)
ggplot(data=t,aes(x=date,y=count)) +
  geom_point(color='tomato') +
  theme_classic() +
  scale_x_date() +
  theme(axis.line=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


# We'll say that the initial peak is from 2014-6-16 to 2014-7-15, the minimum
# is from 2015-1-16 to 2015-2-15, and the end of the series is from 
# 2015-8-16 to 2015-9-15.

j2 <- join(uac_date[,c('date','age','locstr'),],
           geo_unique[,c('locstr','admin1','country')],by='locstr')
m <- ddply(j2,c('admin1','country'),summarize,
           mean=mean(age),count=length(age))
d <- ymd(c('2014-6-15','2014-7-16','2015-1-15','2015-2-16','2015-8-15','2015-9-16'))
change <- ldply(1:nrow(m), function(i) {
  country <- m[i,'country']
  admin1 <- m[i,'admin1']
  avg <- mean(j2[j2$country==country & j2$admin1==admin1,'age'])
  peak <- nrow(j2[j2$country==country & j2$admin1==admin1 &
                   j2$date > d[1] & j2$date < d[2],])
  dip <- nrow(j2[j2$country==country & j2$admin1==admin1 &
                  j2$date > d[3] & j2$date < d[4],])
  end <- nrow(j2[j2$country==country & j2$admin1==admin1 &
                  j2$date > d[5] & j2$date < d[6],])
  data.frame(country=country,admin1=admin1,drop=peak/dip,rebound=end/dip)
})

source('NT-isocodes.R')
change$admin1 <- as.character(change$admin1)
writeme <- join(change,shp_clean,by=c('country','admin1'))
writeme[is.na(writeme$adm1_code),]
write.csv(writeme,'change-0307.csv')
