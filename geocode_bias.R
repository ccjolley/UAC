# Check for systematic biases in the locations that weren't successfully geocoded

library(ggplot2)
library(lubridate)
library(reshape2)
Sys.setlocale("LC_TIME", "usa")

setwd("C:/Users/Craig/Desktop/UAC")
source('UAC_load.R')
all_uac$age <- as.numeric(all_uac$age)
all_uac$date <- ymd(all_uac$date)
all_uac <- unique(all_uac)
names(all_uac) <- c('name','country','city','date','sector','age','str')

# Clean up locations
geo_unique <- read.csv('allnames-0302.csv',stringsAsFactors=FALSE)

j <- join(all_uac,geo_unique[,c('str','lat','lon')],by='str')

mean(is.na(j$lat)) # 4.7% missing

###############################################################################
# Country-level bias?
###############################################################################

gtm <- 'GUATEMALA'
slv <- 'EL SALVADOR'
hnd <- 'HONDURAS'
mx <- 'MEXICO'

m_gtm <- matrix(c(sum(j$country==gtm & is.na(j$lat)),
                 sum(j$country==gtm & !is.na(j$lat)),
                 sum(j$country!=gtm & is.na(j$lat)),
                 sum(j$country!=gtm & !is.na(j$lat))),
                nrow=2,
                dimnames=list(c('Unlaced','Placed'),c('GTM','not GTM')))
fisher.test(m_gtm) # p = 0.3204 -- Guatemala isn't singled out
mean(j$country==gtm)
sum(j$country==gtm & is.na(j$lat)) / sum(is.na(j$lat))

m_slv <- matrix(c(sum(j$country==slv & is.na(j$lat)),
                  sum(j$country==slv & !is.na(j$lat)),
                  sum(j$country!=slv & is.na(j$lat)),
                  sum(j$country!=slv & !is.na(j$lat))),
                nrow=2,
                dimnames=list(c('Unlaced','Placed'),c('SLV','not SLV')))
fisher.test(m_slv) # p = 1.6e-14 
# El Salvador is over-represented 1.46-fold among unplaced locations
mean(j$country==slv)
sum(j$country==slv & is.na(j$lat)) / sum(is.na(j$lat))

m_hnd <- matrix(c(sum(j$country==hnd & is.na(j$lat)),
                  sum(j$country==hnd & !is.na(j$lat)),
                  sum(j$country!=hnd & is.na(j$lat)),
                  sum(j$country!=hnd & !is.na(j$lat))),
                nrow=2,
                dimnames=list(c('Unlaced','Placed'),c('HND','not HND')))
fisher.test(m_hnd) # p = 2.2e-16
# Honduras is under-represented 0.47-fold among unplaced locations
mean(j$country==hnd)
sum(j$country==hnd & is.na(j$lat)) / sum(is.na(j$lat))

m_mx <- matrix(c(sum(j$country==mx & is.na(j$lat)),
                  sum(j$country==mx & !is.na(j$lat)),
                  sum(j$country!=mx & is.na(j$lat)),
                  sum(j$country!=mx & !is.na(j$lat))),
                nrow=2,
                dimnames=list(c('Unlaced','Placed'),c('MX','not MX')))
fisher.test(m_mx) # p = 0.8784
# Mexico isn't singled out
mean(j$country==mx)
sum(j$country==mx & is.na(j$lat)) / sum(is.na(j$lat))

# What's the best way to correct for this? when mapping out the points of 
# origin, we'll need to scale the counts in El Salvador to compensate for
# the lack of good placements in El Salvador, scale Honduras similarly.
# By how much?

scale_slv <- sum(j$country==slv)*sum(!is.na(j$lat))/(nrow(j)*sum(j$country==slv & !is.na(j$lat)))
# 1.0123

# Double-check
scale_slv*sum(j$country==slv & !is.na(j$lat))/sum(!is.na(j$lat))
mean(j$country==slv)

scale_hnd <- sum(j$country==hnd)*sum(!is.na(j$lat))/(nrow(j)*sum(j$country==hnd & !is.na(j$lat)))
# 0.9778

# Double-check
scale_hnd*sum(j$country==hnd & !is.na(j$lat))/sum(!is.na(j$lat))
mean(j$country==hnd)

###############################################################################
# Sector bias?
# This won't be as useful for mapping, but is still good to know about.
###############################################################################

table(j$sector)
# Rio Grande Valley is by far the largest

sector_test <- function(s) {
  m <- matrix(c(sum(j$sector==s & is.na(j$lat)),
                sum(j$sector==s & !is.na(j$lat)),
                sum(j$sector!=s & is.na(j$lat)),
                sum(j$sector!=s & !is.na(j$lat))),
                nrow=2,
                dimnames=list(c('Unlaced','Placed'),c('in sector','out of sector')))
  fisher.test(m)
}

for (s in unique(j$sector)) {
  print(paste(s,sector_test(s)$p.value))
}
# El Paso, Laredo, Rio Grande Valley might be significant

sector_test('EL PASO')
# El Paso kids are 1.56x as likely not to have been placed

sector_test('LAREDO')
# Laredo kids are 1.59x as likely not to have been placed

sector_test('RIO GRANDE VALLEY')
# RGV kids are 0.767x as likely not to have been placed

###############################################################################
# Age bias?
# This won't be as useful for mapping, but is still good to know about.
###############################################################################

t.test(j[is.na(j$lat),'age'],j[!is.na(j$lat),'age'])
# Successfully-geocoded kids might be a little younger

t.test(j[is.na(j$lat) & j$country==gtm,'age'],j[!is.na(j$lat) & j$country==gtm,'age'])
t.test(j[is.na(j$lat) & j$country==slv,'age'],j[!is.na(j$lat) & j$country==slv,'age'])
t.test(j[is.na(j$lat) & j$country==hnd,'age'],j[!is.na(j$lat) & j$country==hnd,'age'])

# The difference is only significant in Guatemala, where the average is older.
# It's possible that older kids are more likely to come from small rural towns
# that don't geocode well.

###############################################################################
# Date bias?
# This won't be as useful for mapping, but is still good to know about.
###############################################################################

tmp2 <- j[,c('date','lat')]
tmp2[!is.na(tmp2$lat),'placed'] <- 1
tmp2[is.na(tmp2$lat),'placed'] <- 0
tmp2$placed <- as.factor(tmp2$placed)

ggplot(data=tmp2,aes(x=date,group=placed,color=placed)) +
  geom_density(size=2) +
  theme_classic()

# Doesn't look to me like a systematic bias.
