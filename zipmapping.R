###### ACS in R####
library(tigris)
library(acs)
library(stringr)
library(dplyr)
library(leaflet)
library(htmlwidgets)


# grab the spatial data (tigris)
counties <- c(5, 47, 61, 81, 85)
tracts <- tracts(state = 'NY', county = c(5, 47, 61, 81, 85), cb=TRUE)

#setting api key
api.key.install(key = "apikey")

##getting tabular data
# create a geographic set to grab tabular data (acs)
geo<-geo.make(state=c("NY"),
              county=c(5, 47, 61, 81, 85), tract="*")

income<-acs.fetch(endyear = 2012, span = 5, geography = geo,
                  table.number = "B19001", col.names = "pretty")

# the resulting "income" object is not a data.frame it's a list
# to see what's available
names(attributes(income))
attr(income, "acs.colnames")

# convert to a data.frame for merging
income_df <- data.frame(paste0(str_pad(income@geography$state, 2, "left", pad="0"), 
                               str_pad(income@geography$county, 3, "left", pad="0"), 
                               str_pad(income@geography$tract, 6, "left", pad="0")), 
                        income@estimate[,c("Household Income: Total:",
                                           "Household Income: $200,000 or more")], 
                        stringsAsFactors = FALSE)

income_df <- select(income_df, 1:3)
rownames(income_df)<-1:nrow(income_df)
names(income_df)<-c("GEOID", "total", "over_200")
income_df$percent <- 100*(income_df$over_200/income_df$total)

#merge spatial and tabular data
income_merged<- geo_join(tracts, income_df, "GEOID", "GEOID")
# there are some tracts with no land that we should exclude
income_merged <- income_merged[income_merged$ALAND>0,]

#making map (leaflet)
popup <- paste0("GEOID: ", income_merged$GEOID, "<br>", "Percent of Households above $200k: ", round(income_merged$percent,2))
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = income_merged$percent
)

map3<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = income_merged, 
              fillColor = ~pal(percent), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = income_merged$percent, 
            position = "bottomright", 
            title = "Percent of Households<br>above $200k",
            labFormat = labelFormat(suffix = "%")) 
map3

#saving map
saveWidget(map1, file="map1.html", selfcontained=FALSE)

####for TX####
#counties - harris, fort bend, montgomery, brazoria, galveston, liberty, waller, chambers
counties <- c(39, 157, 167, 201, 291, 339, 473, 71)
tracts <- tracts(state = 'TX', county = c(39, 157, 167, 201, 291, 339, 473), cb=TRUE)

# create a geographic set to grab tabular data (acs)
api.key.install(key = "1d2e99a9d125613a69921d50ce5c1e1c0999f4d9")

geo<-geo.make(state=c("TX"),
              county=c(39, 157, 167, 201, 291, 339, 473), tract="*")

income<-acs.fetch(endyear = 2015, span = 5, geography = geo,
                  table.number = "B19001", col.names = "pretty")

# convert to a data.frame for merging
income_df <- data.frame(paste0(str_pad(income@geography$state, 2, "left", pad="0"), 
                               str_pad(income@geography$county, 3, "left", pad="0"), 
                               str_pad(income@geography$tract, 6, "left", pad="0")), 
                        income@estimate[,c("B19001. Household Income in the Past 12 Months (in 2015 Inflation-Adjusted Dollars): Total:",
                                           "B19001. Household Income in the Past 12 Months (in 2015 Inflation-Adjusted Dollars): $200,000 or more")], 
                        stringsAsFactors = FALSE)



income_df <- select(income_df, 1:3)
rownames(income_df)<-1:nrow(income_df)
names(income_df)<-c("GEOID", "total", "over_200")
income_df$percent <- 100*(income_df$over_200/income_df$total)

#merge spatial data and tabular data
income_merged<- geo_join(tracts, income_df, "GEOID", "GEOID")
# there are some tracts with no land that we should exclude
income_merged <- income_merged[income_merged$ALAND>0,]

#making map with leaflet
popup <- paste0("GEOID: ", income_merged$GEOID, "<br>", "Percent of Households above $200k: ", round(income_merged$percent,2))
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = income_merged$percent
)

map1<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = income_merged, 
              fillColor = ~pal(percent), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = income_merged$percent, 
            position = "bottomright", 
            title = "Percent of Households<br>above $200k",
            labFormat = labelFormat(suffix = "%")) 
map1

