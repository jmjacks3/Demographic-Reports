###############################################
####zip codes analysis of fort bend county ####
##############################################

#necessary libraries beyond tigris
library(sp)
library(rgeos)
library(leaflet)
library(acs)
library(stringr)
library(dplyr)
library(ggplot2)
library(rgdal)
library(tigris)
library(scales)

#fixing temporary directory
options(tigris_use_cache = FALSE)

#need zip codes in/adjacent to fort bend: 777406 77417 77420 77430 77435 77441 77444
# 77451 77459 77461 77464 77469 77471 77476 77478 77477 77479 77481
# 77485 77489 77494 77407 77498 77545


zipfb <- zctas(starts_with = c(777406, 77417, 77420, 77430, 77435, 77441, 77444,
                               77451, 77459, 77461, 77464, 77469, 77471, 77476, 77478, 77477, 77479, 77481,
                               77485, 77489, 77494, 77407, 77498, 77545), cb=TRUE)


# create a geographic set to grab tabular data (acs)
api.key.install(key = "apikey")

geo<-geo.make(zip.code = c(77406, 77417, 77420, 77430, 77435, 77441, 77444,
                           77451, 77459, 77461, 77464, 77469, 77471, 77476, 77478, 77477, 77479, 77481,
                           77485, 77489, 77494, 77407, 77498, 77545))

income<-acs.fetch(endyear = 2015, span = 5, geography = geo,
                  table.number = "B19013", col.names = "pretty")

names(attributes(income))
attr(income, "acs.colnames")

# convert to a data.frame for merging
hhincmdn_df <- data.frame(paste0(str_pad(income@geography$zipcodetabulationarea, 5, "left", pad="0")), 
                        income@estimate[,c("B19013. Median Household Income in the Past 12 Months (in 2015 Inflation-Adjusted Dollars): Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)")], 
                        stringsAsFactors = FALSE) 
                                           
                                          



hhincmdn_df <- select(hhincmdn_df, 1:2)
rownames(hhincmdn_df)<-1:nrow(hhincmdn_df)
names(hhincmdn_df)<-c("GEOID10", "median_income")

####ANALYSIS#####

#average % hhouseholds
summarise(hhincmdn_df, mean(median_income, na.rm=TRUE))

top3zip <- hhincmdn_df %>%
    arrange(desc(median_income)) %>%
    slice(1:3)

#bar graph of top 3 zip codes with highest percent of high-income households
a1 <- ggplot(data=top3zip, aes(x=GEOID10, y=median_income))

a1 + geom_bar(colour="black", stat="identity", fill="navy", alpha=0.9, width=0.5) +
  geom_text(aes(label=dollar(median_income)), vjust=-0.5, fontface=2, size=5) +    #add data labels
  xlab("Zip code") + # Set axis labels
  ggtitle("Median Household Income, Top 3 Zip Codes in Fort Bend County") +     # Set title
  coord_cartesian(ylim=c(100000, 150000)) +
  scale_y_continuous(breaks=seq(100000, 150000, 10000), labels = scales::dollar) +  #adjusting scale of axis
  theme(axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5))  #background, adjusting title positioning

#save graph
setwd("~/BusinessGraphs")
ggsave("MedianHHinc.png")

