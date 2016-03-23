library(ggplot2)
library(lubridate)
library(reshape2)
Sys.setlocale("LC_TIME", "usa")

setwd("C:/Users/Craig/Desktop/UAC")
source('UAC_load.R')
all_uac$age <- as.numeric(all_uac$age)
all_uac$date <- ymd(all_uac$date)

# Clean up locations
geo_unique <- read.csv('allnames-0302.csv',stringsAsFactors=FALSE)
geo_unique <- geo_unique[!is.na(geo_unique$lat),]
geo_unique <- geo_unique[geo_unique$lat != 0,]
geo_unique[geo_unique$country=='GUATEMALA' & !is.na(geo_unique$country),'country'] <- 'Guatemala'
geo_unique[geo_unique$country=='EL SALVADOR' & !is.na(geo_unique$country),'country'] <- 'El Salvador'
names(geo_unique) <- c('lat','lon','country','admin1','address','type','locstr','str2','count')
geo_unique[geo_unique$admin1 == 'San Marcos Department','admin1'] <- 'San Marcos'

###############################################################################
# First, are there any duplicates?
###############################################################################
dupnames <- all_uac[duplicated(all_uac),'name']
dups <- all_uac[all_uac$name %in% dupnames,]
dups <- dups[order(dups$name),]
# My duplicates were all apprehended on the 15th of the month, which means 
# they showed up on multiple spreadsheets.
all_uac <- unique(all_uac) # I should do this for all my analyses!
rm(dupnames,dups)

###############################################################################
# Are kids with the same last name unusually likely to be apprehended in the 
# same sector on the same day?
###############################################################################
match1 <- data.frame(last_name=sapply(all_uac$name, function(x) strsplit(x,', ')[[1]][1]),
                    date=as.character(all_uac$date),
                    sector=all_uac$sector)
match1$last_date_sector <- paste(match1$last_name,match1$date,match1$sector,sep='_')
t <- as.data.frame(table(match1$last_date_sector))                    
t$num_pairs <- choose(t$Freq,2)
same_name_same_date <- sum(t$num_pairs) 
rm(t)
t <- as.data.frame(table(match1$last_name))
t$num_pairs <- choose(t$Freq,2)
same_name <- sum(t$num_pairs)
same_name_different_date <- same_name - same_name_same_date
rm(t)
match1$date_sector <- paste(match1$date,match1$sector,sep='_')
t <- as.data.frame(table(match1$date_sector))
t$num_pairs <- choose(t$Freq,2)
same_date <- sum(t$num_pairs)
rm(t)
different_name_same_date <- same_date - same_name_same_date
different_name_different_date <- choose(nrow(all_uac),2) - same_name_same_date

mt <- matrix(c(same_name_same_date,different_name_same_date,
               same_name_different_date,different_name_different_date),ncol=2)
pt_samename <- prop.test(mt)
pt_samename # *very* significant
pt_samename$estimate[1] / pt_samename$estimate[2] 
# kids with the same last name 23x more likely to show up on the same day
rm(different_name_same_date,different_name_different_date,same_name,
   same_name_same_date,same_name_different_date,mt,match1)

###############################################################################
# Now constrain them to be from the same town as well; I suspect this will make 
# things even more significant
###############################################################################
match2 <- data.frame(last_name=sapply(all_uac$name, function(x) strsplit(x,', ')[[1]][1]),
                     city=all_uac$city,date=as.character(all_uac$date),
                     sector=all_uac$sector)
match2$last_city_date_sector <- paste(match2$last_name,match2$city,match2$date,match2$sector,sep='_')
t <- as.data.frame(table(match2$last_city_date_sector))                    
t$num_pairs <- choose(t$Freq,2)
same_namecity_same_date <- sum(t$num_pairs) 
rm(t)
match2$last_city <- paste(match2$last_name,match2$city,sep='_')
t <- as.data.frame(table(match2$last_city))                    
t$num_pairs <- choose(t$Freq,2)
same_namecity <- sum(t$num_pairs) 
rm(t)
same_namecity_different_date <- same_namecity - same_namecity_same_date
different_namecity_same_date <- same_date - same_namecity_same_date
different_namecity_different_date <- choose(nrow(all_uac),2) - same_namecity_same_date

mt <- matrix(c(same_namecity_same_date,different_namecity_same_date,
               same_namecity_different_date,different_namecity_different_date),ncol=2)
pt_samenamecity <- prop.test(mt)
pt_samenamecity # *very* significant
pt_samenamecity$estimate[1] / pt_samenamecity$estimate[2] 
# kids with the same last name and the same city of origin are 253x more 
# likely to be apprehended together
rm(different_namecity_different_date,different_namecity_same_date,
   same_namecity,same_namecity_different_date,same_namecity_same_date,
   same_date,mt)
rm(pt_samename,pt_samenamecity)

###############################################################################
# What makes these co-travelers different from the rest?
# Look first at age.
###############################################################################
t <- as.data.frame(table(match2$last_city_date_sector)) 
t <- t[t$Freq>1,]
t <- t[order(t$Freq,decreasing=TRUE),]
groups <- colsplit(t$Var1,'_',c('lastname','city','date','sector'))
groups$size <- t$Freq
groups <- na.omit(groups) 
groups <- groups[groups$city != '',] # 2121 groups of related kids traveling together

tmp <- all_uac
tmp$lastname <- match2$last_name
j <- join(tmp,groups,by=c('lastname','city','date','sector'))
group_members <- j[!is.na(j$size),] # 4428 total kids in these groups
t.test(group_members$age,all_uac$age)
# kids traveling together are significantly younger -- 12.63 instead of 14.5
rm(j,tmp,t)

###############################################################################
# Average ages of oldest and youngest kids in co-traveling groups?
###############################################################################
age_minmax <- ldply(1:nrow(groups),function(i) {
  lname <- groups[i,'lastname']
  city <- groups[i,'city']
  matches <- group_members[group_members$lastname==lname & 
                             group_members$city==city,]
  data.frame(younger=min(matches$age),older=max(matches$age))
})
mean(age_minmax$older)
median(age_minmax$older)
m <- melt(age_minmax)
names(m) <- c('variable','age')
ggplot(m,aes(x=age,group=variable,fill=variable)) +
  geom_density(alpha=0.3) +
  theme_classic() +
  theme(text=element_text(size=20),
        axis.line=element_blank(),axis.ticks=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank())
mean(age_minmax$younger) # 11 years old
mean(age_minmax$older) # 14.3 years old -- close to overall average

age_minmax$gap <- age_minmax$older - age_minmax$younger
ggplot(age_minmax,aes(x=gap)) +
  geom_histogram(binwidth=1,fill='goldenrod',origin=-0.5) +
  labs(x='Age difference (years)',y='') +
  theme_classic() +
  theme(text=element_text(size=20),
        axis.line=element_blank(),axis.ticks=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank())

in_groups <- ldply(0:max(all_uac$age,na.rm=TRUE),function(i) {
  in_groups <- sum(group_members$age==i & !is.na(group_members$age))
  total <- sum(all_uac$age==i & !is.na(all_uac$age))
  data.frame(age=i,fraction=in_groups/total)
})
ggplot(in_groups,aes(x=age,y=fraction)) +
  geom_line(size=3) +
  theme_classic() +
  theme(text=element_text(size=20),
        axis.line=element_blank(),axis.ticks=element_blank())
# Kids 7-11 years old are the most likely to be traveling with a sibling

###############################################################################
# Size distribution of groups?
###############################################################################
ggplot(data=groups,aes(x=size)) +
  geom_bar(binwidth=1,fill='slategray3',color='gray12',origin=-0.5) +
  theme_classic() +
  labs(x='Group size',y='') +
  theme(text=element_text(size=20),
        axis.line=element_blank(),axis.ticks=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank())
# Pairs are by far the most common

###############################################################################
# How does the fraction of kids traveling in groups vary with time?
###############################################################################
all_uac$date <- ymd(all_uac$date)
group_members$date <- ymd(group_members$date)

uac_date <- all_uac[,c('locstr','date','age')]
uac_date <- uac_date[!is.na(uac_date$date),]
uac_date$month <- month(uac_date$date)
uac_date$year <- year(uac_date$date)
uac_date <- uac_date[order(uac_date$date),]
uac_date$monthnum <- (uac_date$year - uac_date$year[1])*12 + (uac_date$month - uac_date$month[1])

group_date <- group_members[,c('locstr','date','age')]
group_date <- group_date[!is.na(group_date$date),]
group_date$month <- month(group_date$date)
group_date$year <- year(group_date$date)
group_date <- group_date[order(group_date$date),]
group_date$monthnum <- (group_date$year - group_date$year[1])*12 + (group_date$month - group_date$month[1])

m1 <- ddply(uac_date,'monthnum',summarize,uac=length(monthnum))
m2 <- ddply(group_date,'monthnum',summarize,group=length(monthnum))
res <- ldply(1:nrow(m1),function(i) {
  uac <- m1[i,'uac']
  group <- m2[i,'group']
  bt <- binom.test(group,uac)
  data.frame(monthnum=i,mean=bt$estimate,low=bt$conf.int[1],high=bt$conf.int[2])
})

resm <- melt(res,id.vars='monthnum')
ggplot(resm,aes(x=monthnum,y=value,group=variable,color=variable)) +
  geom_line(size=2) +
  theme_classic() +
  theme(text=element_text(size=20),
        axis.line=element_blank(),axis.ticks=element_blank())
# not sure I see a story here.

###############################################################################
# Do kids traveling in groups come from some regions more than others?
###############################################################################
j2 <- join(group_members[,c('name','locstr')], 
           geo_unique[,c('locstr','admin1','country')],by='locstr')
adm1 <- unique(geo_unique[,c('country','admin1')])
adm1_groups <- ldply(1:nrow(adm1), function(i) {
  country <- adm1[i,'country']
  admin1 <- adm1[i,'admin1']
  num_groups <- sum(j2$country==country & 
                    j2$admin1==admin1,na.rm=TRUE)
  num_all <- sum(geo_unique[geo_unique$country==country & 
                            geo_unique$admin1==admin1,'count'],
                 na.rm=TRUE)
  data.frame(country=country,admin1=admin1,groups=num_groups,all=num_all)
}) 

# Everybody accounted for? (These should both be zero.)
sum(!is.na(j2$admin1)) - sum(adm1_groups$groups) 
sum(geo_unique[!is.na(geo_unique$admin1),'count']) - sum(adm1_groups$all)

adm1_groups_sig <- ldply(1:nrow(adm1_groups),function(i) {
  in_groups <- adm1_groups[i,'groups']
  in_all <- adm1_groups[i,'all']
  out_groups <- sum(adm1_groups[-i,'groups'])
  out_all <- sum(adm1_groups[-i,'all'])
  mt <- matrix(c(in_groups,out_groups,in_all,out_all),ncol=2)
  if (in_groups > 10) {
    tt <- prop.test(mt,alternative='greater')
  } else {
    tt <- fisher.test(mt,alternative='greater')
  }
  data.frame(country=adm1_groups[i,'country'],
             admin1=adm1_groups[i,'admin1'],
             fraction=in_groups / in_all,
             pval=tt$p.value,
             sig=as.numeric(tt$p.value < 0.01),stringsAsFactors=FALSE)
})

source('NT-isocodes.R')
adm1_groups_sig$admin1 <- as.character(adm1_groups_sig$admin1)
writeme <- join(adm1_groups_sig,shp_clean,by=c('country','admin1'))
writeme[is.na(writeme$adm1_code),]
write.csv(writeme,'adm1_groups_0308.csv')

