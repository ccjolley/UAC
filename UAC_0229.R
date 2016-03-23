# Putting everything in one place, starting from the saved Google geocoding runs

setwd("C:/Users/Craig/Desktop/UAC")
source('UAC_load.R')
library(ggmap)
library(geosphere)

geocode_0222 <- read.csv('geocode-0222.csv')
geocode_0223 <- read.csv('geocode-0223.csv')
geocode_0224 <- read.csv('geocode-0224.csv')

f <- c('lat','lon','country','administrative_area_level_1','address','type')
geocode <- rbind(geocode_0222[,f],geocode_0223[,f],geocode_0224[,f])
geocode$str <- geocode_us
rm(geocode_us,geocode_0222,geocode_0223,geocode_0224)

###############################################################################
# Set invalid location names to lat/long = 0,0
###############################################################################
set_zero <- function(data,patterns) {
  tmp <- data
  for (p in patterns) {
    tmp[grep(p,tmp$str),c('lat','lon')] <- c(0,0)
    tmp[grep(p,tmp$str2),c('lat','lon')] <- c(0,0)
  }
  tmp
}

###############################################################################
# Clean up invalid locations in dataset. If I never want to try again on a 
# location, set it to lat/long = 0,0. If I think there's still a chance to
# find it, set to NA.
###############################################################################
clean_geocode <- function(data,admin1=TRUE) {
  tmp <- data
  # Set places outside Mesoamerica to lat/long = NA
  meso_am <- c('Mexico','Guatemala','El Salvador','Honduras')
  tmp[!tmp$country %in% meso_am,c('lat','lon')] <- NA
  # Set overly-large locations to NA
  if (admin1) {  # remove admin1
    too_big <- c('administrative_area_level_1','country') 
  }
  else {
    too_big <- c('country')
  }
  tmp[tmp$type %in% too_big & !is.na(tmp$type),c('lat','lon')] <- NA
  # Set overly-small locations to NA
  too_small <- c('premise','bus_station','transit_station','route',
                 'natural_feature','park','airport','street address','null')
  tmp[tmp$type %in% too_small & !is.na(tmp$type),c('lat','lon')] <- NA
  # Set invalid location names to lat/long = 0,0
  tmp <- set_zero(tmp,c('^UNK','^,','^.,','PROSECUTION'))
  tmp
}

geocode <- clean_geocode(geocode)
mean(!is.na(geocode$lat))  # 61.8% located -- I can probably find more 

###############################################################################
# Now there are some locations I'd like to retry.
# First, duplicated department and municipality names 
# (e.g. Yoro, Yoro, Honduras)
###############################################################################

adm1_retry <- geocode[geocode$str == toupper(geocode$address) & 
                        geocode$type == 'administrative_area_level_1' & 
                        !is.na(geocode$type),'str']
adm1_retry <- data.frame(str=adm1_retry,city=sapply(adm1_retry, function(x) 
  strsplit(x,', ')[[1]][1]),row.names=NULL)
adm1_retry$str2 <- paste(adm1_retry$city,adm1_retry$str,sep=', ')



###############################################################################
# Unplaced locations beginning with "Caserio", "Aldea", "Canton", "Mun.",
# "Dept."
###############################################################################
remove_prefix <- function(data,patterns) {
  ldply(patterns, function(p) {
    tmp <- data[grep(p,data$str),c('lat','str')]
    tmp <- tmp[is.na(tmp$lat),]
    tmp$str2 <- sub(p,'',tmp$str)
    tmp$str2 <- sub('^ ','',tmp$str2) # remove leading spaces
    tmp[,c('str','str2')]
  })
}

prefix_retry <- remove_prefix(geocode,c('^CASERIO','^CANTON','^ALDEA ',
                        '^MUN[^ \t\n\r\f\v]* ','^DEP[^ \t\n\r\f\v]* '))

###############################################################################
# "DISTRITO CENTRAL" and variants
###############################################################################
capitals <- geocode[grep('^DIST',geocode$str),c('lat','str')]
capitals <- capitals[is.na(capitals$lat),]
capitals[grep('HONDURAS',capitals$str),'str2'] <- 'TEGUCIGALPA, HONDURAS'
capitals[grep('GUATEMALA',capitals$str),'str2'] <- 'GUATEMALA CITY, GUATEMALA'
capitals[grep('MEXICO',capitals$str),'str2'] <- 'MEXICO CITY, MEXICO'

retry_us <- rbind(adm1_retry[,c('str','str2')],prefix_retry,
                  capitals[,c('str','str2')])
rm(adm1_retry,prefix_retry,capitals)

###############################################################################
# Now geocode these. After running this the first time, comment out and just
# reload the saved results.
###############################################################################
# library(ggmap)
# retry_geocode <- geocode(retry_us$str2,source='google',output='more')
# write.csv(retry_geocode,'geocode_0229.csv',row.names=FALSE)

retry_geocode <- read.csv('geocode_0229.csv')
retry_geocode <- retry_geocode[,f]
retry_geocode <- cbind(retry_geocode,retry_us)
retry_geocode <- clean_geocode(retry_geocode,admin1=FALSE)
rm(retry_us)


###############################################################################
# Merge revised geocodes back into original dataset. Join using the "str"
# field, replace the lat,long,address,country,admin1 fields in the old
# data frame with those in the new one. 
###############################################################################
merge_geocodes <- function(oldgeo,newgeo,replace) {
  str_fields <- c('country','administrative_area_level_1','address','type',
                  'str2','str')
  for (x in str_fields) {
    if (x %in% names(oldgeo)) { oldgeo[,x] <- as.character(oldgeo[,x]) }
    if (x %in% names(newgeo)) { newgeo[,x] <- as.character(newgeo[,x]) }
  }
  merge_fields <- c('str','origin')
  merge_fields <- intersect(merge_fields,names(oldgeo))
  names(newgeo)[names(newgeo) %in% replace] <- 
    paste(names(newgeo)[names(newgeo) %in% replace],'_new',sep='')
  j <- join(oldgeo,newgeo,by=merge_fields) 
  if (nrow(j) != nrow(oldgeo)) {
    print('WARNING: non-unique rows in merge_geocodes()!!!')
  }
  j[is.na(j$lat) & !is.na(j$lat_new),replace] <- 
    j[is.na(j$lat) & !is.na(j$lat_new),paste(replace,'_new',sep='')]
  j[,names(oldgeo)]
}

# Note that merge_geocodes only copies the str2 field for cases that were 
# successfully placed. I'd also like to do it for others, so that I can try
# again to place them later.
j <- join(geocode,retry_geocode,by='str')
geocode$str2 <- j$str2
rm(j)
mean(!is.na(geocode$lat)) # 61.8%
geocode <- merge_geocodes(geocode,retry_geocode,replace=c(f,'str2'))
mean(!is.na(geocode$lat)) # 64.8%
rm(retry_geocode)

###############################################################################
# Now that we've gotten as far as we're likely to get with the Google 
# geocoding, assemble a master list of locations for hierarchical clustering
###############################################################################

get_uac_locs <- function(my_geocode) {
  counts <- as.data.frame(table(all_uac$locstr))
  names(counts) <- c('str','count')
  j_count <- join(my_geocode,counts,by='str')
  
  out_loc <- my_geocode[,c(f,'str')]
  out_loc$origin <- 'str'
  out_loc$count <- j_count$count
  out_str2 <- my_geocode[,c(f,'str2')]
  names(out_str2) <- c(f,'str')
  out_str2$origin <- 'str2'
  out_str2$count <- j_count$count
  out_str2 <- out_str2[!is.na(out_str2$str),]
  rbind(out_loc,out_str2)
}

uac_locs <- get_uac_locs(geocode)

aldeas <- read.delim("shapefiles/aldeas-HND.txt") 
ald2 <- aldeas[,c('Latitude','Longitude','Ald74name')]
names(ald2) <- c('lat','lon','name')
ald2$str <- paste(toupper(ald2$name),', HONDURAS',sep='')
ald2$origin <- 'shapefile'
ald2$country <- 'Honduras'
ald2$type <- 'shp_aldeas'
ald2$administrative_area_level_1 <- NA
ald2$address <- tolower(ald2$str)
ald2$count <- NA
ald2 <- ald2[,c(f,'str','origin','count')]
rm(aldeas)


load_osm <- function(fname,country) { # read in locations from OSM shapefile
  places <- read.csv(fname)
  fields <- c('POINT_Y','POINT_X','NAME','name')
  fields <- intersect(fields,names(places))
  res <- places[,fields]
  names(res) <- c('lat','lon','name')
  res$str <- toupper(paste(res$name,country,sep=', '))
  Encoding(res$str) <- "UTF-8" # no special characters
  res$str <- iconv(res$str, from='UTF-8', to='ASCII//TRANSLIT')
  res$str <- toupper(res$str)
  res$origin <- 'shapefile'
  res$country <- country
  res$administrative_area_level_1 <- NA
  res$address <- res$str
  res$type <- 'shp_osm'
  res$count <- NA
  res <- res[,c(f,'str','origin','count')]
  res <- res[-grep('^KM',res$str),]
  res <- res[-grep('^.,',res$str),]
}

gtm_places <- load_osm('shapefiles/GTM-places.txt','GUATEMALA')
slv_places <- load_osm('shapefiles/SLV-places.txt','EL SALVADOR')
new_places <- rbind(ald2,gtm_places,slv_places)
# keep only place names I don't already have
new_places <- new_places[!new_places$str %in% uac_locs$str,]
rm(ald2,gtm_places,slv_places)

all_places <- rbind(uac_locs,new_places) # 8481 places
rm(new_places)
library(stringdist)
sdm <- stringdistmatrix(all_places$str,method='osa') 
fit <- hclust(sdm,method='ward.D2') 

###############################################################################
# Take a cut height (maximum intra-group distance) and return a data frame with
# all of the newly-placed locations based on clustering at that height
###############################################################################
dup_uac_place <- function(locs,x) {
  new_locs <- locs[locs$group==x & is.na(locs$lat),]
  good_locs <- locs[locs$group==x & !is.na(locs$lat),]
  best_loc <- good_locs[which.max(good_locs$count),]
  new_locs[,f] <- best_loc[,f]
  new_locs$match_to <- best_loc$str
  new_locs
}

nearby_uac <- function(ll,uac_data,radius=20) {
  # Determine the number of UACs within a given radius, so that I have 
  # comparable 'count' numbers for the OSM data.
  # Make sure ll has the order (longitude,latitude)
  r <- 3959 # Earth's radius in miles
  tmp <- uac_data[!is.na(uac_data$lat),]
  dists <- sapply(1:nrow(tmp),function(i) {
    distCosine(ll,c(tmp$lon[i],tmp$lat[i]),r)
  }) 
  tmp$dist <- dists
  sum(tmp[tmp$dist<radius,'count'])
}

dup_shp_place <- function(locs,x) {
  new_locs <- locs[locs$group==x & is.na(locs$lat),]
  good_locs <- locs[locs$group==x & !is.na(locs$lat),]
  if (nrow(good_locs) > 1) {
    good_locs$count <- sapply(1:nrow(good_locs), function(i) {
     ll <- c(good_locs$lon[i],good_locs$lat[i])
      nearby_uac(ll,uac_locs)
    })
    best_loc <- good_locs[which.max(good_locs$count),]
  } else {
    best_loc <- good_locs
  }
  new_locs[,f] <- best_loc[,f]
  new_locs$match_to <- best_loc$str
  new_locs
}

cluster_assign <- function(locs,height) {
  groups <- cutree(fit,h=height) #
  locs$group <- groups
  # which groups contain an unplaced location?
  unplaced <- unique(locs[is.na(locs$lat),'group'])
  # which groups contain a geocoded UAC origin?
  placed_uac <- unique(locs[(locs$origin=='str' | locs$origin=='str2') & 
                              !is.na(locs$lat),'group'])
  uac_condense_groups <- intersect(unplaced,placed_uac)
  new_locs_uac <- ldply(uac_condense_groups,function(x) dup_uac_place(locs,x)) 
  # now find groups where no UAC locations are available but shapefile locations are
  placed_shp <- unique(locs[locs$origin=='shapefile','group'])
  shp_condense_groups <- intersect(setdiff(unplaced,placed_uac),placed_shp)
  new_locs_shp <- ldply(shp_condense_groups,function(x) dup_shp_place(locs,x))
  res <- rbind(new_locs_uac,new_locs_shp)
  #res[,c(1:3,5:7)]
  res[!is.na(res$lat),c(f,'str','match_to','origin')]
}

new1 <- cluster_assign(all_places,1) # 168 new locations; all look good
#new1[,c('lat','address','type','str','match_to')]
all_places <- merge_geocodes(all_places,new1[,c(f,'str','origin')],replace=f)

new2 <- cluster_assign(all_places,2) # 208 new locations; good
#new2[,c('lat','address','type','str','match_to')]
all_places <- merge_geocodes(all_places,new2[,c(f,'str','origin')],replace=f)

new3 <- cluster_assign(all_places,3)
#new3[,c('lat','address','type','str','match_to')] # 247 new locations; mixed
all_places <- merge_geocodes(all_places,new3[,c(f,'str','origin')],replace=f)

new4 <- cluster_assign(all_places,4) # 225 new locations; most are bad
#new4[,c('lat','address','type','str','match_to')] 

rm(new1,new2,new3,new4)

# Now merge all of these new locations back into geocode
new_str <- intersect(all_places[!is.na(all_places$lat) & all_places$origin=='str','str'],
                     geocode[is.na(geocode$lat),'str']) # 595 of these
new_str2 <- intersect(all_places[!is.na(all_places$lat) & all_places$origin=='str2','str'],
                      geocode[is.na(geocode$lat),'str2']) # 28 of these

new_str_places <- all_places[all_places$str %in% new_str & all_places$origin=='str',]
new_str2_places <- all_places[all_places$str %in% new_str2 & all_places$origin=='str2',]

mean(!is.na(geocode$lat)) # 64.8%
geocode <- merge_geocodes(geocode,new_str_places[,c(f,'str')],replace=f)
mean(!is.na(geocode$lat)) # 76.4%

names(new_str2_places) <- c(f,'str2','origin','count')
j2 <- join(new_str2_places,geocode,by='str2')
new_str2_places$str <- j2$str
rm(j2)
geocode <- merge_geocodes(geocode,new_str2_places[,c(f,'str','str2'),],replace=f)

rm(all_places,new_str,new_str2,new_str_places,new_str2_places,uac_locs,sdm,fit)

counts <- as.data.frame(table(all_uac$locstr))
names(counts) <- c('str','count')
j_count <- join(geocode,counts,by='str')
geocode$count <- j_count$count
rm(counts,j_count)

mean(!is.na(geocode$lat)) # 76.9% of locations geocoded
sum(geocode[!is.na(geocode$lat),'count'])/sum(geocode$count) # 95.3% of UACs coded

# One last piece of bookkeeping -- reverse geocoding to get admin1 for 
# shapefile locations
length(unique(geocode$administrative_area_level_1)) # 88 before join

no_adm1 <- geocode[is.na(geocode$administrative_area_level_1) & 
                     !is.na(geocode$lat) & geocode$lat != 0,] # 99 of these
rgc <- ldply(1:nrow(no_adm1),function(i) {
  ll <- as.numeric(no_adm1[i,c('lon','lat')])
  revgeocode(ll,output='more')
})
no_adm1$administrative_area_level_1 <- as.character(rgc$administrative_area_level_1)
# need to remove Spanish accented characters
Encoding(no_adm1$administrative_area_level_1) <- "UTF-8" 
no_adm1$administrative_area_level_1 <- 
  iconv(no_adm1$administrative_area_level_1, from='UTF-8', to='ASCII//TRANSLIT')
no_adm1 <- no_adm1[,c('str','administrative_area_level_1')]
names(no_adm1) <- c('str','adm1')
j_adm1 <- join(geocode,no_adm1,by='str')
head()
geocode[is.na(j_adm1$administrative_area_level_1) & 
          !is.na(j_adm1$adm1),'administrative_area_level_1'] <- 
  j_adm1[is.na(j_adm1$administrative_area_level_1) & !is.na(j_adm1$adm1),'adm1']
# Just one it couldn't find on its own; I resorted to Wikipedia
geocode[geocode$address=='amapala, honduras' & !is.na(geocode$address),
        'administrative_area_level_1'] <- 'Valle'

###############################################################################
# Final cleanup
###############################################################################

geocode[is.na(geocode$lat),f] <- NA
write.csv(geocode,'allnames-0302.csv',row.names=FALSE)

# Condense to unique addresses only

geo_unique <- geocode[!is.na(geocode$lat),]
geo_unique <- ddply(geo_unique,f,summarize,count=sum(count))
geocode[geocode$address=='san martin, el salvador' & !is.na(geocode$address),]

# Use hierarchical clustering to condense locations
n <- nrow(geo_unique)
r <- 3959 # Earth's radius in miles
m <- matrix(nrow=n,ncol=n)
for (i in 1:(n-1)) {
  m[i,i] <- 0
  ll_i <- c(geo_unique$lon[i],geo_unique$lat[i])
  for (j in (i+1):n) {
    ll_j <- c(geo_unique$lon[j],geo_unique$lat[j])  
    d <- distCosine(ll_i,ll_j,r)
    m[i,j] <- d
    m[j,i] <- d
  }
}
m[n,n] <- 0

fit <- hclust(as.dist(m),method='ward.D2') 
# merge locations within 5 miles of each other
groups <- cutree(fit,h=5)
length(unique(groups)) # from 1632 down to 988

geo_unique$group <- groups
geo_binned <- ddply(geo_unique,'group',summarize,lat=lat[which.max(count)],
                    lon=lon[which.max(count)],
                    admin1=administrative_area_level_1[which.max(count)],
                    address=address[which.max(count)],
                    type=type[which.max(count)],count=sum(count))
geo_binned <- geo_binned[,-1]
geo_binned <- geo_binned[geo_binned$type!='country',]
write.csv(geo_binned,'binned-0302.csv',row.names=FALSE)
