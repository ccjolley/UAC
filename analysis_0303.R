library(ggplot2)
library(mapproj)
library(ggmap)
library(lubridate)
library(geosphere)
library(reshape2)
library(plyr)
library(maps)

setwd("C:/Users/Craig/Desktop/UAC")
geo_unique <- read.csv('allnames-0302.csv')

centAm <- c('guatemala','el salvador','honduras')
geo_centam <- geo_unique[!is.na(geo_unique$lat) & geo_unique$lat != 0 &
                         tolower(geo_unique$country) %in% centAm,]
geo_centam$country <- tolower(geo_centam$country)

###############################################################################
# display points using orthographic projection
###############################################################################
plot_map <- function(data,group='country') {
  xy <- mapproject(data$lon,data$lat,projection='orthographic')
  df <- data.frame(x=xy$x,y=xy$y,country=tolower(data$country),  
                 count=data$count)
  df$group <- as.character(data[,group])
  ggplot(data=df,aes(x=x,y=y,color=group,size=count)) +  
  geom_point() +
  theme_classic() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
}

plot_map(geo_centam)

###############################################################################
# Aggregate to fewer locations using k-means clustering
###############################################################################
nloc <- 10
tmp <- geo_centam
xy <- mapproject(tmp$lon,tmp$lat,projection='orthographic')
tmp$x <- xy$x
tmp$y <- xy$y
km <- kmeans(tmp[,c('x','y')],nloc)
tmp$group <- km$cluster
str2group <- tmp[,c('str','group')]
geo_binned <- ddply(tmp,'group',summarize,lat=lat[which.max(count)],
                    lon=lon[which.max(count)],
                    admin1=administrative_area_level_1[which.max(count)],
                    address=address[which.max(count)],
                    country=country[which.max(count)],
                    count=sum(count))
 
tmp <- geo_binned[,c('lat','lon','count')]
tmp$country <- 'binned'
tmp <- rbind(tmp,geo_centam[,c('lat','lon','count','country')])

plot_map(tmp)

###############################################################################
# Time series analysis
###############################################################################
source('UAC_load.R')
tmp <- geo_unique[,c('lat','lon','str','address')]
names(tmp) <- c('lat','lon','locstr','address')
j <- join(all_uac,tmp,by='locstr')
uac_date <- j[,c('lat','lon','date','age','address','locstr','country')]
ggplot(data=uac_date,aes(x=date)) +
  geom_histogram(binwidth=5) +
  theme_classic() +
  theme(axis.line=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

uac_date <- uac_date[order(uac_date$date),]
t <- as.data.frame(table(uac_date$date))
names(t) <- c('date','count')
Sys.setlocale("LC_TIME", "usa") # lubridate needs this
t$date <- ymd(as.character(t$date)) 
ggplot(data=t,aes(x=date,y=count)) +
  geom_point(color='tomato') +
  theme_classic() +
  theme(axis.line=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

# significant weekday trends?
t$wday <- wday(t$date,label=TRUE)
weekday <- ddply(t,'wday',summarize,count=mean(count))
t.test(t[t$wday=='Sun','count'])$conf.int
t.test(t[t$wday=='Wed','count'])$conf.int # difference not significant

###############################################################################
# What I'd like to do here is similar to what I did before -- cluster
# locations together based on similarities in their time course. Use geo_binned
# in order to cut down on noise.
###############################################################################
uac_date <- uac_date[!is.na(uac_date$lat) & uac_date$lat != 0,]
uac_date$month <- month(uac_date$date)
uac_date$year <- year(uac_date$date)
uac_date <- uac_date[order(uac_date$date),]
uac_date$monthnum <- (uac_date$year - uac_date$year[1])*12 + (uac_date$month - uac_date$month[1])
names(str2group) <- c('locstr','geobin')
uac_date <- join(uac_date,str2group,by='locstr')
uac_date <- uac_date[!is.na(uac_date$geobin),]

uac_wide <- dcast(uac_date, lat + lon + geobin + address + country ~ monthnum,length,
                  margins='monthnum')
names(uac_wide)[ncol(uac_wide)] <- 'total'
mnums <- as.character(unique(uac_date$monthnum))
sums <- rowsum(uac_wide[,mnums],uac_wide$geobin)
sums$group <- row.names(sums)
bin_wide <- join(geo_binned,sums,by='group')

cor(as.numeric(bin_wide[1,mnums]),as.numeric(bin_wide[2,mnums]))

n <- nrow(bin_wide)
m <- matrix(nrow=n,ncol=n) 
# This needs to be a distance matrix, so take 1 - cor/2; distances will be 0-1
for (i in 1:(n-1)) {
  m[i,i] <- 0
  for (j in (i+1):n) {
    c <- cor(as.numeric(bin_wide[i,mnums]),as.numeric(bin_wide[j,mnums]))
    m[i,j] <- 1 - c/2
    m[j,i] <- 1 - c/2
  }
}
m[n,n] <- 0

# k-means clustering will want to use Euclidean coordinates instead; I'll have
# to project onto two PCA dimensions and then cluster. With this full 
# correlation matrix, I'm better off using hierarchical agglomerative. Try
# both and compare results.

fit <- hclust(as.dist(m),method='ward.D2') 
plot(fit) # use this to choose cut height to get a reasonable # of clusters
# to me, this looks like two significant clusters
groups <- cutree(fit, k=2)
bin_wide$hclust <- groups

bin_wide$country <- tolower(bin_wide$country)
#plot_map(bin_wide)
plot_map(bin_wide,'hclust')

plotme <- bin_wide[,c(5,8:23)]
m <- melt(plotme)
ggplot(m,aes(x=variable,y=value,group=address,color=address)) +
  geom_line(size=2) +
  theme_classic()

###############################################################################
# Next, I want to see what average time courses look like for each of my bins
###############################################################################
# s <- sums[,1:16]
# norm <- (s - apply(s,1,mean)) / apply(s,1,sd)
# norm$hclust <- bin_wide$hclust
# cor(as.numeric(norm[1,1:16]),as.numeric(bin_wide[1,8:23])) # sanity check
# plotme <- rowsum(norm[,1:16],norm$hclust)
#plotme <- plotme / table(bin_wide$hclust)

sums$hclust <- bin_wide$hclust
plotme <- rowsum(sums[,1:16],sums$hclust)
plotme$hclust <- row.names(plotme)
m <- melt(plotme)
ggplot(m,aes(x=variable,y=value,group=hclust,color=hclust)) +
  geom_line(size=3) +
  theme_classic()

###############################################################################
# What if we just look at countries?
###############################################################################
country_sums <- rowsum(uac_wide[,6:21],uac_wide$country)
country_sums$country <- tolower(row.names(country_sums))
m <- melt(country_sums)
names(m) <- c('country','month_num','total')
ggplot(m,aes(x=month_num,y=total,group=country,color=country)) +
  geom_line(size=3) +
  theme_classic()