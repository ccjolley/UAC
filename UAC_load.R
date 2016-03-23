library(xlsx)
library(plyr)

setwd("C:/Users/Craig/Desktop/UAC/data")
files <- c('UACApprehensions_201406.xls','UACApprehensions_201407.xls',
           'UACApprehensions_201407_201408.xls','UACApprehensions_201408_201409.xls',
           'UACApprehensions_201409_201410.xls','UACApprehensions_201410_201411.xls',
           'UACApprehensions_201411_201412.xls','UACApprehensions_201501_201502.xls',
           'UACApprehensions_201502_201503.xlsx','UACApprehensions_201503_201504.xlsx',
           'UACApprehensions_201504_201505.xlsx','UACApprehensions_201505_201506.xlsx',
           'UACApprehensions_201506_201507.xlsx','UACApprehensions_201507_201508.xlsx',
           'UACApprehensions_201508_201509.xlsx')
start <- rep(4,15)
start[[4]] <- 3 # inconsistent format between spreadsheets
start[[5]] <- 3
datalist <- lapply(1:15,function(i) read.xlsx2(files[i],1,startRow=start[i],
                                               stringsAsFactors=FALSE)) 
all_uac <- data.frame(
  name=unlist(lapply(datalist,function(x) x$Subject.Name)),
  country=unlist(lapply(datalist,function(x) x$Citizenship.Country.Name)),
  city=unlist(lapply(datalist,function(x) x$Birth.City.Name)),
  date=unlist(lapply(datalist,function(x) x$Apprehension.Date)),
  sector=unlist(lapply(datalist,function(x) x$Sector.Name)),
  age=unlist(lapply(datalist,function(x) x$Age.at.Apprehension)),
  stringsAsFactors=FALSE
)   # 39,598 kids
setwd("C:/Users/Craig/Desktop/UAC")

# fix dates; see https://en.wikipedia.org/wiki/Microsoft_Excel#Date_problems
all_uac$date <- as.Date(as.numeric(all_uac$date),origin=as.Date("1899-12-30"))
# remove leading periods from some names
all_uac$city <- sub('^[.]','',all_uac$city)
# Fix El Salvador misspelling
all_uac[all_uac$country=='EL SAVADOR','country'] <- 'EL SALVADOR'
# merge country, city into a single location string
all_uac$locstr <- paste(all_uac$city,all_uac$country,sep=', ')

###############################################################################

geocode_us <- unique(all_uac$locstr) # 5117 unique locations
rm(files,start,datalist)
