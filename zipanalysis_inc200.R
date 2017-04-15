###############################################
####zip codes analysis of fort bend county ####
##############################################

#necessary libraries beyond tigris
library(sp)
library(rgeos)
library(tigris)
library(leaflet)
library(acs)
library(stringr)
library(dplyr)
library(htmlwidgets)
library(ggplot2)
library(CartoDB)

#fixing temporary directory
options(tigris_use_cache = FALSE)

#need zip codes in/adjacent to fort bend: 777406 77417 77420 77430 77435 77441 77444
# 77451 77459 77461 77464 77469 77471 77476 77478 77477 77479 77481
# 77485 77489 77494 77407 77498 77545


zipfb <- zctas(starts_with = c(777406, 77417, 77420, 77430, 77435, 77441, 77444,
                               77451, 77459, 77461, 77464, 77469, 77471, 77476, 77478, 77477, 77479, 77481,
                               77485, 77489, 77494, 77407, 77498, 77545), cb=TRUE)


# create a geographic set to grab tabular data (acs)
api.key.install(key = "1d2e99a9d125613a69921d50ce5c1e1c0999f4d9")

geo<-geo.make(zip.code = c(77406, 77417, 77420, 77430, 77435, 77441, 77444,
                           77451, 77459, 77461, 77464, 77469, 77471, 77476, 77478, 77477, 77479, 77481,
                           77485, 77489, 77494, 77407, 77498, 77545))

income<-acs.fetch(endyear = 2015, span = 5, geography = geo,
                  table.number = "B19001", col.names = "pretty")

# convert to a data.frame for merging
income_df <- data.frame(paste0(str_pad(income@geography$zipcodetabulationarea, 5, "left", pad="0")), 
                        income@estimate[,c("B19001. Household Income in the Past 12 Months (in 2015 Inflation-Adjusted Dollars): Total:",
                                           "B19001. Household Income in the Past 12 Months (in 2015 Inflation-Adjusted Dollars): $200,000 or more")], 
                        stringsAsFactors = FALSE)



income_df <- select(income_df, 1:3)
rownames(income_df)<-1:nrow(income_df)
names(income_df)<-c("GEOID10", "total", "over_200")
income_df$percent <- 100*(income_df$over_200/income_df$total)

####ANALYSIS#####

#average % hhouseholds
summarise(income_df, mean(percent))

top5zip <- income_df %>%
    arrange(desc(percent)) %>%
    slice(1:5)

#bar graph of top 5 zip codes with highest percent of high-income households
a1 <- ggplot(data=top5zip, aes(x=GEOID10, y=percent))

a1 + geom_bar(colour="black", stat="identity", fill="navy", alpha=0.9) +
  geom_text(aes(label=round(percent, 1)), vjust=-0.5, fontface=2, size=5) +    #add data labels
  xlab("Zip code") + ylab("Percent") + # Set axis labels
  ggtitle("Top 5 Zip Codes with Household Income over $200k") +     # Set title
  theme(plot.title = element_text(hjust = 0.5))

#save graph
setwd("~/misc/graphs")
ggsave("top5zips.png")

