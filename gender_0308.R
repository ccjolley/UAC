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

# I can infer genders using this API:
# https://market.mashape.com/namsor/gendre-infer-gender-from-world-names
# Pricing is:
# $0/month for 1000, with $0.01 per additional ($385.98)
# $9.99/month for 5000, with $0.005 per additional ($182.98)
# $45/month for 25,000, with $0.002 per additional ($74.20)
# $99/month for 100,000, with $0.001 per additional ($99)

# So if I *had* to purchase this, I could do it for about $75. 
# More than I want to pay out of pocket.

fnames=sapply(all_uac$name, function(x) strsplit(x,', ')[[1]][2])
first_names <- unlist(strsplit(fnames,' '))
t <- as.data.frame(table(first_names))
t <- t[order(t$Freq,decreasing=TRUE),]
(nrow(t) - 1000)*0.01 # $70.75 to do all unique first names

###############################################################################
# Try some lists from internet. This isn't the most sophisticated approach, 
# but let's see how it does.
###############################################################################
# List of the most 5000 most common male and female names in Spain, from 
# http://www.ine.es/tnombres/formGeneralresult.do?vista=4
girl_es <- read.delim('female_names_es.txt',stringsAsFactors=FALSE)
boy_es <- read.delim('male_names_es.txt',stringsAsFactors=FALSE)
girl_es$percent <- 100 * girl_es$Total / sum(girl_es$Total)
boy_es$percent <- 100 * boy_es$Total / sum(boy_es$Total)
girl_es$Nombre <- tolower(girl_es$Nombre)
boy_es$Nombre <- tolower(boy_es$Nombre)

# Most common names from 1990 US Census
# http://www2.census.gov/topics/genealogy/1990surnames/dist.female.first
girl_us <- read.table('census-dist-female-first.txt',stringsAsFactors=FALSE)
boy_us <- read.table('census-dist-male-first.txt',stringsAsFactors=FALSE)
names(girl_us) <- c('name','percent','cum_percent','rank')
names(boy_us) <- c('name','percent','cum_percent','rank')
girl_us$name <- tolower(girl_us$name)
boy_us$name <- tolower(boy_us$name)


names <- data.frame(fullname=all_uac$name,given=tolower(fnames),
                    male_match=0,female_match=0,stringsAsFactors=FALSE)

# Match on Spanish names first
names$female_match <- 0
for (i in 1:nrow(girl_es)) {
  p1<- paste('(^| )',girl_es[i,'Nombre'],'( |$)',sep='')
  m1 <- grep(p1,names$given,perl=TRUE)
  names[m1,'female_match'] <- names[m1,'female_match'] + girl_es[i,'percent']
}

names$male_match <- 0
for (i in 1:nrow(boy_es)) {
  p1<- paste('(^| )',boy_es[i,'Nombre'],'( |$)',sep='')
  m1 <- grep(p1,names$given,perl=TRUE)
  names[m1,'male_match'] <- names[m1,'male_match'] + boy_es[i,'percent']
}

names$female <- NA
names[names$male_match > names$female_match,'female'] <- 0
names[names$male_match < names$female_match,'female'] <- 1

nrow(names[!is.na(names$female),])/nrow(names) # placed 89.4% based on Spanish names
head(names[is.na(names$female),],100)

# In cases where this didn't work, try the US names

names$female_match_us <- 0
for (i in 1:nrow(girl_us)) {
  p1<- paste('(^| )',girl_us[i,'name'],'( |$)',sep='')
  m1 <- grep(p1,names$given,perl=TRUE)
  names[m1,'female_match_us'] <- names[m1,'female_match_us'] + girl_us[i,'percent']
}

names$male_match_us <- 0
for (i in 1:nrow(boy_us)) {
  p1<- paste('(^| )',boy_us[i,'name'],'( |$)',sep='')
  m1 <- grep(p1,names$given,perl=TRUE)
  names[m1,'male_match_us'] <- names[m1,'male_match_us'] + boy_us[i,'percent']
}

names[is.na(names$female) & names$female_match_us > names$male_match_us,'female'] <- 1
names[is.na(names$female) & names$female_match_us < names$male_match_us,'female'] <- 0

nrow(names[!is.na(names$female),])/nrow(names) # up to 91.2% with US names included

# I can guess based on whether the final letter is a or o.
names$final_letter <- NA
names[grep('a($| )',names$given),'final_letter'] <- 'a'
names[grep('o($| )',names$given),'final_letter'] <- 'o'
names[is.na(names$female) & !is.na(names$final_letter) & 
        names$final_letter == 'a','female'] <- 1
names[is.na(names$female) & !is.na(names$final_letter) & 
        names$final_letter == 'o','female'] <- 0

nrow(names[!is.na(names$female),])/nrow(names) # up to 94% with final letters
# Let's call that good enough -- it's as good as my geocoding at this point.

###############################################################################
# Now let's see if there's anything useful in there. Summary stats first.
###############################################################################
all_uac$female <- names$female

binom.test(sum(all_uac$female,na.rm=TRUE),sum(!is.na(all_uac$female)))
# 32.8 - 33.7% female
t.test(all_uac[!is.na(all_uac$female) & all_uac$female==1,'age'],
       all_uac[!is.na(all_uac$female) & all_uac$female==0,'age'])
# Girls are significantly younger -- 13.9 years vs 14.8

###############################################################################
# Age distribution
###############################################################################
plotme <- all_uac[,c('age','female')]
plotme <- plotme[!is.na(plotme$age) & !is.na(plotme$female),]
plotme[plotme$female==1,'gender'] <- 'Female'
plotme[plotme$female==0,'gender'] <- 'Male'
ggplot(plotme,aes(x=age,group=gender,fill=gender)) +
  geom_density(alpha=0.2,adjust=1.4) +
  theme_classic() +
  labs(x='Age (years)',y='Density') +
  theme(text=element_text(size=20),
        axis.line=element_blank(),axis.ticks=element_blank())

###############################################################################
# Time trend in female fraction?
###############################################################################
uac_date <- all_uac[,c('locstr','date','female')]
uac_date$date <- ymd(uac_date$date)
uac_date <- uac_date[!is.na(uac_date$date),]
uac_date$month <- month(uac_date$date)
uac_date$year <- year(uac_date$date)
uac_date <- uac_date[order(uac_date$date),]
uac_date$monthnum <- (uac_date$year - uac_date$year[1])*12 + (uac_date$month - uac_date$month[1])
m <- ddply(uac_date,'monthnum',summarize,mean=mean(female,na.rm=TRUE),
           conf95_lo=binom.test(sum(female,na.rm=TRUE),sum(!is.na(female)))$conf.int[1],
           conf95_hi=binom.test(sum(female,na.rm=TRUE),sum(!is.na(female)))$conf.int[2])
m$date <- uac_date$date[1] + months(m$monthnum)
ggplot(m,aes(x=date,y=mean,ymin=conf95_lo,ymax=conf95_hi)) +
  geom_line(size=3,color='aquamarine4') +
  geom_errorbar(width=100,color='darkgray') + 
  theme_classic() +
  labs(x='',y='Female fraction') +
  theme(text=element_text(size=20),
        axis.line=element_blank(),axis.ticks=element_blank())
# Fraction of girls stays close to 1/3 and decreases with time but increases
# significantly from Apr-May 2015. What happened then that made people more 
# willing to send girls?

###############################################################################
# Geography of girl migrants?
###############################################################################
tmp <- uac_date[,c('locstr','date','female')]
j <- join(tmp,geo_unique,by='locstr')
j <- j[!is.na(j$female),]
j <- j[!is.na(j$admin1),]
adm1 <- unique(geo_unique[,c('country','admin1')])
sig <- ldply(1:nrow(adm1), function(i) {
  country <- adm1[i,'country']
  admin1 <- adm1[i,'admin1']
  avg <- mean(j[j$country==country & j$admin1==admin1,'female'])
  signif <- 0
  tryCatch({
    bt_in <- binom.test(sum(j[j$country==country & j$admin1==admin1,'female']),
                        sum(j$country==country & j$admin1==admin1))
    bt_out <- binom.test(sum(j[j$country!=country | j$admin1!=admin1,'female']),
                         sum(j$country!=country | j$admin1!=admin1))
    # non-overapping intervals?
    signif <- as.numeric(bt_in$conf.int[1] > bt_out$conf.int[2]) 
  }, error=function(e) {})
  data.frame(country=country,admin1=admin1,avg=avg,sig=signif)
})

source('NT-isocodes.R')
sig$admin1 <- as.character(sig$admin1)
sig[sig$admin1 == 'San Marcos Department','admin1'] <- 'San Marcos'
writeme <- join(sig,shp_clean,by=c('country','admin1'))
writeme[is.na(writeme$adm1_code),]
write.csv(writeme,'adm1-gender-0309.csv',row.names=FALSE)

###############################################################################
# Girls more likely to travel in groups?
###############################################################################
match2 <- data.frame(last_name=sapply(all_uac$name, function(x) strsplit(x,', ')[[1]][1]),
                     city=all_uac$city,date=as.character(all_uac$date),
                     sector=all_uac$sector)
match2$last_city_date_sector <- paste(match2$last_name,match2$city,match2$date,match2$sector,sep='_')
t <- as.data.frame(table(match2$last_city_date_sector)) 
t <- t[t$Freq>1,]
t <- t[order(t$Freq,decreasing=TRUE),]
groups <- colsplit(t$Var1,'_',c('lastname','city','date','sector'))
groups$size <- t$Freq
groups <- na.omit(groups) 
groups <- groups[groups$city != '',]
tmp <- all_uac
tmp$lastname <- match2$last_name
j <- join(tmp,groups,by=c('lastname','city','date','sector'))
group_members <- j[!is.na(j$size),] # 4428 total kids in these groups
binom.test(sum(all_uac$female,na.rm=TRUE),
           sum(!is.na(all_uac$female)))
# Overall fraction is 32.75 - 33.72%
binom.test(sum(group_members$female,na.rm=TRUE),
           sum(!is.na(group_members$female)))
# Fraction in groups is 46.53 - 49.59% --> *much* higher


sum(group_members$female,na.rm=TRUE)/sum(all_uac$female,na.rm=TRUE) # 16.4%
nrow(group_members)/nrow(all_uac) # 11.4%
