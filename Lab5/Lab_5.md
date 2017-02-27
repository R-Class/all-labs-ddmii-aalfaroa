Lab\_5
================
Alejandro Alfaro
2/27/2017

1. Setting up housing dataset
=============================

``` r
my.url <- "https://raw.githubusercontent.com/lecy/hedonic-prices/master/Data/Housing%20Price%20In-Class%20Exercise%20(Responses).csv"
housing.raw <- getURL( my.url, ssl.verifypeer=FALSE )
dathouse <- read.csv( textConnection(housing.raw), stringsAsFactors=FALSE )
names( dathouse ) <- c("timestamp","price","X1","X2","sqft","your.name","lot.size","beds",
                  "bath","garage","year","elementary","middle","high","walk","tax","highway",
                  "restaurant","starbucks","park","mall","address","zip","tract" )
rm(my.url)
rm(housing.raw)


dathouse$price <- as.numeric( gsub( ",","", dathouse$price ) )
dathouse$tax <- as.numeric( gsub( ",","", dathouse$tax ) )
dathouse$lot.size <- as.numeric( gsub( ",","", dathouse$lot.size ) )
dathouse$sqft <- as.numeric( gsub( ",","", dathouse$sqft ) )
dathouse$lot.size[ is.na( dathouse$lot.size ) ] <- mean( dathouse$lot.size, na.rm=T )

# houses <- dathouse[ , c("address","zip") ]
# houses$address <- gsub( ",", "", houses$address )
# houses$address <- gsub( "\\.", "", houses$address )
# addresses <- paste( houses$address, "Syracuse, NY", houses$zip, sep=", " )
# lat.long <- geocode( addresses )
# geocodehouses <- lat.long
# write.csv(geocodehouses, file="HousesGeocode.csv")
geocodehouses <- read.csv("HousesGeocode.csv")
dathouse$lon <- geocodehouses$lon
dathouse$lat <- geocodehouses$lat
rm(geocodehouses)
```

2. Add Census Tracts to each Home
=================================

``` r
# #dir.create( "shapefiles" )
# setwd( "./shapefiles" )
# download.file("ftp://ftp2.census.gov/geo/tiger/TIGER2010/TRACT/2010/tl_2010_36067_tract10.zip", "onondaga census tracts.zip")
# unzip( "onondaga census tracts.zip" )
# file.remove( "onondaga census tracts.zip" )
setwd( "./shapefiles" )
onondaga <- readShapePoly( fn="tl_2010_36067_tract10", proj4string=CRS("+proj=longlat +datum=WGS84") )

#Spatial join 1
coordinates(dathouse) = ~lon+lat
proj4string(dathouse) <- '+proj=longlat +datum=WGS84'
poly.data.matched.to.points <- over( dathouse, onondaga )
dathouse@data <- cbind( dathouse@data, poly.data.matched.to.points )
pts.poly <- point.in.poly( dathouse, onondaga )
dathouse@data <- pts.poly@data
rm(poly.data.matched.to.points)
rm(pts.poly)
```

3. Add Census Data to Each Home
===============================

``` r
library(plyr)
library(dplyr)
mycesuskey <- "6623ba8c58d3b2eeec361d16664573e96d878534"
datcensus <- getCensus(name = "acs5", vintage= "2015",
                 key = mycesuskey, vars= c("NAME","B17001_002E","B19013_001E", "B01001A_001E",
                                           "B01001B_001E", "B01001D_001E", "B01001I_001E"),
                 region="tract:*", regionin="State:36")
datcensus <- subset(datcensus, county == "067")
datcensus <- plyr::rename(datcensus, c("B17001_002E"="PovertyLevel", "B19013_001E"="MedHInc",
                                 "B01001A_001E"="White", "B01001B_001E"="Black",
                                 "B01001D_001E"="Asian", "B01001I_001E"="Latino"))
dathouse <- merge(x=dathouse, y=datcensus, by.x="TRACTCE10", by.y="tract",all.x=T)
rm(mycesuskey)
rm(datcensus)
```

4. Aggregrate by Census tract and add to the dataset
====================================================

``` r
# Add Crime data
crimedata <- read.xlsx("Crime in Syracuse Dec to Jan 2014.xlsx")
# addresses <- paste( crimedata$Address, crimedata$City, "NY", sep=", " )
# lat.long <- geocode( addresses )
# geocodecrime <- lat.long
# write.csv(geocodecrime, file="CrineGeocode.csv")
geocodecrime <- read.csv("CrimeGeocode.csv")
crimedata$lon <- geocodecrime$lon
crimedata$lat <- geocodecrime$lat
crimedatafiltered <- is.na(!crimedata$lat)
crimedata <- crimedata[!crimedatafiltered,]
rm(crimedatafiltered)
rm(geocodecrime)

#Spatial join 2
coordinates(crimedata) = ~lon+lat
proj4string(crimedata) <- '+proj=longlat +datum=WGS84'
poly.data.matched.to.points <- over( crimedata, onondaga)
crimedata@data <- cbind( crimedata@data, poly.data.matched.to.points )
pts.poly <- point.in.poly( crimedata, onondaga )
crimedata@data <- pts.poly@data
crimedatafinal <- data.frame(table(crimedata$TRACTCE10))
crimedatafinal <- plyr::rename(crimedatafinal, c("Var1"="Ctract", "Freq"="Crimes"))
dathouse <- merge(x=dathouse, y=crimedatafinal, by.x="TRACTCE10", by.y="Ctract", all.x=T)
rm(poly.data.matched.to.points)
rm(pts.poly)
rm(crimedatafinal)
rm(crimedata)
```

Saving & Printing
=================

``` r
write.csv(dathouse, file="HousingData_All.csv")
head(dathouse, 10)
```

    ##               coordinates TRACTCE10       timestamp  price X1 X2 sqft
    ## 109 (-76.19918, 43.02561)    004800 1/15/2015 16:11 179900 NA NA 1600
    ## 110 (-76.18848, 43.02894)    004800 1/15/2015 16:17 128000 NA NA 1992
    ## 111  (-76.1854, 43.02957)    004800 1/15/2015 16:25 114900 NA NA 1378
    ## 112 (-76.18301, 43.02757)    004800 1/15/2015 16:34 107500 NA NA 1452
    ## 113  (-76.18324, 43.0321)    004800 1/15/2015 16:41  43000 NA NA  850
    ## 118 (-76.17196, 43.03111)    005000 1/15/2015 16:52  85000 NA NA 1639
    ## 119 (-76.17217, 43.02621)    005000 1/15/2015 16:56 254900 NA NA 3560
    ## 122  (-76.1626, 43.02542)    005100 1/15/2015 17:00  63000 NA NA 1788
    ## 120 (-76.16863, 43.03126)    005000 1/15/2015 17:05 188000 NA NA 2832
    ## 121 (-76.16885, 43.02092)    005000 1/15/2015 17:09 169900 NA NA 1864
    ##          your.name lot.size beds bath garage year elementary middle high
    ## 109 Emily Simonson    43560    3  2.0    Yes 1994          9      4    2
    ## 110 Emily Simonson     6969    4  2.5    Yes 1950          2      4    2
    ## 111 Emily Simonson     5227    4  1.0    Yes 1930          2      4    2
    ## 112 Emily Simonson     5227    3  1.0    Yes 1931          2      4    1
    ## 113 Emily Simonson     6098    2  1.0    Yes 1955          2      9    1
    ## 118 Emily Simonson     7840    4  1.0     No 1915          2      4    1
    ## 119 Emily Simonson    13503    5  3.5    Yes 1940          2      4    2
    ## 122 Emily Simonson     5227    3  1.0     No 1890          4      1    2
    ## 120 Emily Simonson    10454    4  2.0    Yes 1905          2      4    1
    ## 121 Emily Simonson     6534    4  2.0    Yes 1926          2      4    2
    ##     walk  tax highway restaurant starbucks park mall            address
    ## 109   15 3182      No         22       3.2   18  1.3 504 Winkworth Pkwy
    ## 110   43 1393      No          7       2.6    5  0.6     136 Austin Ave
    ## 111   50 1331      No          6       2.6    7  0.5     701 Velasko Rd
    ## 112   42  157      No         12       2.6    8  0.8    518 Wolcott Ave
    ## 113   57 1525      No          8       2.3   20  0.6    112 Wolcott Ave
    ## 118   36 2184      No         32       1.9   11  2.5    212 Roberts Ave
    ## 119   28 5202      No         27       2.2   13  2.6    168 Robineau Rd
    ## 122   43  788      No         34       1.7    6  3.3   136 Parkside ave
    ## 120   49 2500      No         12       1.7   11  1.9     121 Ruskin Ave
    ## 121   52 2251      No         22       2.3    4  4.1   203 Wellesley Rd
    ##       zip tract STATEFP10 COUNTYFP10     GEOID10 NAME10      NAMELSAD10
    ## 109 13219    NA        36        067 36067004800     48 Census Tract 48
    ## 110 13207    NA        36        067 36067004800     48 Census Tract 48
    ## 111 13207    NA        36        067 36067004800     48 Census Tract 48
    ## 112 13207    NA        36        067 36067004800     48 Census Tract 48
    ## 113 13207    NA        36        067 36067004800     48 Census Tract 48
    ## 118 13207    NA        36        067 36067005000     50 Census Tract 50
    ## 119 13207    NA        36        067 36067005000     50 Census Tract 50
    ## 122 13207    NA        36        067 36067005100     51 Census Tract 51
    ## 120 13207    NA        36        067 36067005000     50 Census Tract 50
    ## 121 13207    NA        36        067 36067005000     50 Census Tract 50
    ##     MTFCC10 FUNCSTAT10 ALAND10 AWATER10  INTPTLAT10   INTPTLON10
    ## 109   G5020          S 1206079        0 +43.0279451 -076.1920648
    ## 110   G5020          S 1206079        0 +43.0279451 -076.1920648
    ## 111   G5020          S 1206079        0 +43.0279451 -076.1920648
    ## 112   G5020          S 1206079        0 +43.0279451 -076.1920648
    ## 113   G5020          S 1206079        0 +43.0279451 -076.1920648
    ## 118   G5020          S 1149255    45948 +43.0272963 -076.1689815
    ## 119   G5020          S 1149255    45948 +43.0272963 -076.1689815
    ## 122   G5020          S  804288        0 +43.0236890 -076.1604174
    ## 120   G5020          S 1149255    45948 +43.0272963 -076.1689815
    ## 121   G5020          S 1149255    45948 +43.0272963 -076.1689815
    ##     STATEFP10.1 COUNTYFP10.1 TRACTCE10.1   GEOID10.1 NAME10.1
    ## 109          36          067      004800 36067004800       48
    ## 110          36          067      004800 36067004800       48
    ## 111          36          067      004800 36067004800       48
    ## 112          36          067      004800 36067004800       48
    ## 113          36          067      004800 36067004800       48
    ## 118          36          067      005000 36067005000       50
    ## 119          36          067      005000 36067005000       50
    ## 122          36          067      005100 36067005100       51
    ## 120          36          067      005000 36067005000       50
    ## 121          36          067      005000 36067005000       50
    ##        NAMELSAD10.1 MTFCC10.1 FUNCSTAT10.1 ALAND10.1 AWATER10.1
    ## 109 Census Tract 48     G5020            S   1206079          0
    ## 110 Census Tract 48     G5020            S   1206079          0
    ## 111 Census Tract 48     G5020            S   1206079          0
    ## 112 Census Tract 48     G5020            S   1206079          0
    ## 113 Census Tract 48     G5020            S   1206079          0
    ## 118 Census Tract 50     G5020            S   1149255      45948
    ## 119 Census Tract 50     G5020            S   1149255      45948
    ## 122 Census Tract 51     G5020            S    804288          0
    ## 120 Census Tract 50     G5020            S   1149255      45948
    ## 121 Census Tract 50     G5020            S   1149255      45948
    ##     INTPTLAT10.1 INTPTLON10.1                                       NAME
    ## 109  +43.0279451 -076.1920648 Census Tract 48, Onondaga County, New York
    ## 110  +43.0279451 -076.1920648 Census Tract 48, Onondaga County, New York
    ## 111  +43.0279451 -076.1920648 Census Tract 48, Onondaga County, New York
    ## 112  +43.0279451 -076.1920648 Census Tract 48, Onondaga County, New York
    ## 113  +43.0279451 -076.1920648 Census Tract 48, Onondaga County, New York
    ## 118  +43.0272963 -076.1689815 Census Tract 50, Onondaga County, New York
    ## 119  +43.0272963 -076.1689815 Census Tract 50, Onondaga County, New York
    ## 122  +43.0236890 -076.1604174 Census Tract 51, Onondaga County, New York
    ## 120  +43.0272963 -076.1689815 Census Tract 50, Onondaga County, New York
    ## 121  +43.0272963 -076.1689815 Census Tract 50, Onondaga County, New York
    ##     state county PovertyLevel MedHInc White Black Asian Latino Crimes
    ## 109    36    067          150   74150  1280   217    78     20      9
    ## 110    36    067          150   74150  1280   217    78     20      9
    ## 111    36    067          150   74150  1280   217    78     20      9
    ## 112    36    067          150   74150  1280   217    78     20      9
    ## 113    36    067          150   74150  1280   217    78     20      9
    ## 118    36    067          364   70132  1950   690     8    108      6
    ## 119    36    067          364   70132  1950   690     8    108      6
    ## 122    36    067          790   24595   575  1201    44    322     12
    ## 120    36    067          364   70132  1950   690     8    108      6
    ## 121    36    067          364   70132  1950   690     8    108      6
