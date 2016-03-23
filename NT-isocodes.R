shp <- read.csv('NE-admin1.txt',stringsAsFactors=FALSE)
shp_clean <- shp[,c('admin','name','adm1_code')]
names(shp_clean) <- c('country','adm1_bad','adm1_code')

# Take out weird "Honduras aggregation" line
shp_clean <- shp_clean[shp_clean$adm1_code != 'HND+00?',]

# Not sure how accented characters got butchered so badly
shp_clean$admin1 <- shp_clean$adm1_bad
shp_clean[grep('^Pet',shp_clean$adm1_bad),'admin1'] <- 'Peten'
shp_clean[grep('^Quezaltenango',shp_clean$adm1_bad),'admin1'] <- 'Quetzaltenango'
shp_clean[grep('^Suchite',shp_clean$adm1_bad),'admin1'] <- 'Suchitepequez'
shp_clean[grep('^Sacatep',shp_clean$adm1_bad),'admin1'] <- 'Sacatepequez'
shp_clean[grep('^Solol',shp_clean$adm1_bad),'admin1'] <- 'Solola'
shp_clean[grep('^Totonicap',shp_clean$adm1_bad),'admin1'] <- 'Totonicapan'
shp_clean[grep('^Quich',shp_clean$adm1_bad),'admin1'] <- 'Quiche'
shp_clean[grep('^La Uni',shp_clean$adm1_bad),'admin1'] <- 'La Union'
shp_clean[grep('^Atl',shp_clean$adm1_bad),'admin1'] <- 'Atlantida'
shp_clean[grep('^Col',shp_clean$adm1_bad),'admin1'] <- 'Colon'
shp_clean[grep('^Francisco Moraz',shp_clean$adm1_bad),'admin1'] <- 'Francisco Morazan'
shp_clean[grep('^Islas de la Bah',shp_clean$adm1_bad),'admin1'] <- 'Bay Islands'
shp_clean[grep('^Islas de la Bah',shp_clean$adm1_bad),'country'] <- 'Honduras'
shp_clean[grep('^Cort',shp_clean$adm1_bad),'admin1'] <- 'Cortes'
shp_clean[grep('^Intibuc',shp_clean$adm1_bad),'admin1'] <- 'Intibuca'
shp_clean[grep('^Santa B',shp_clean$adm1_bad),'admin1'] <- 'Santa Barbara'
shp_clean[grep('^Cop',shp_clean$adm1_bad),'admin1'] <- 'Copan'
shp_clean[grep('^El Para',shp_clean$adm1_bad),'admin1'] <- 'El Paraiso'
shp_clean[grep('^Ahuachap',shp_clean$adm1_bad),'admin1'] <- 'Ahuachapan'
shp_clean[grep('^Caba',shp_clean$adm1_bad),'admin1'] <- 'Cabanas'
shp_clean[grep('^Cuscatl',shp_clean$adm1_bad),'admin1'] <- 'Cuscatlan'
shp_clean[grep('^La Uni',shp_clean$adm1_bad),'admin1'] <- 'La Union'
shp_clean[grep('^Moraz',shp_clean$adm1_bad),'admin1'] <- 'Morazan'
shp_clean[grep('^Usulut',shp_clean$adm1_bad),'admin1'] <- 'Usulutan'

rm(shp)
