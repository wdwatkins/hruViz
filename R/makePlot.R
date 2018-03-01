library(rgdal)
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)

#David Watkins December 2017
#running this for a full year takes ~12 hours in serial on a laptop

streamSegments <- readOGR(dsn = "OfficialSegments", 
                          layer = "OfficialSegments", stringsAsFactors = FALSE)
streamOrder <- fread('gfSegsStreamOrder.csv', colClasses = c(seg_id_nat="character"))
streamSegments@data <- left_join(streamSegments@data, streamOrder, by = "seg_id_nat")
streamSegments_filt <- subset(streamSegments, streamSegments$SO > 0 & !region.y %in% c("19", "20", "21"))
#streamSegments_filt <- subset(streamSegments, !region.y %in% c("19", "20", "21"))

percTable1 <- fread('mthPCNT1_seg_outflow.tsv') %>% mutate(pct='p1')
percTable10 <- fread('mthPCNT10_seg_outflow.tsv') %>% mutate(pct='p10')
percTable25 <- fread('mthPCNT25_seg_outflow.tsv') %>% mutate(pct='p25')
percTable75 <- fread('mthPCNT75_seg_outflow.tsv') %>% mutate(pct='p75')
percTable90 <- fread('mthPCNT90_seg_outflow.tsv') %>% mutate(pct='p90')
percTable100 <- fread('mthPCNT100_seg_outflow.tsv') %>% mutate(pct='p100')
allPerc <- do.call(what = bind_rows, 
                   args = list(percTable1, percTable10, percTable25,
                               percTable75, percTable90, percTable100))
rm(list=ls(pattern = "percTable")) #free some space
#convert to actual integer day from col name
allPerc <- rename(allPerc, seg_id = V1)  
gatherPerc <- gather(allPerc, day, value, -seg_id, -pct) %>%
  mutate(day=as.integer(gsub(pattern = "V", replacement = "", x = day)) - 1) %>% 
   spread(pct, value)

#actual outflow file is massive, can't load it in at once
#get dates first
allDates <- data.table::fread(input = 'seg_outflow.tsv', 
                              select = 1, col.names = "Date")

startDate <- "2010-06-01"
endDate <- "2010-06-01"
startDateIndex <- which(allDates$Date == startDate)
endDateIndex <- which(allDates$Date == endDate)
actualOutflows <- fread('seg_outflow.tsv', 
                        nrows = (endDateIndex - startDateIndex + 1),
                        skip = startDateIndex - 1)
names(actualOutflows)[1] <- "Date"
outflows_gathered <- gather(actualOutflows, seg_id, value, -Date) %>% 
  mutate(seg_id = as.integer(gsub(pattern = "V", replacement = "", x = seg_id)) - 1)

#start making plots!
plotDates <- seq(as.Date(startDate), as.Date(endDate), by = "day")
plotDays <- yday(plotDates)

#filter down to segments left in spatial df
outflows_gathered <- filter(outflows_gathered, seg_id %in% streamSegments_filt@data$seg_id_nat) %>% 
  mutate(day = yday(Date))
gatherPerc <- filter(gatherPerc, seg_id %in% streamSegments_filt@data$seg_id_nat, 
                     day %in% plotDays) %>% select(-p100, p100) #move p100 to right

joinedPercOutflow <- left_join(outflows_gathered, gatherPerc, by = c("seg_id", "day")) %>% 
  mutate(cat = ifelse(test = value > p1 & value < p10,
                      yes = "lt10", 
                      no = ifelse(test = value > p10 & value < p25,
                                  yes = "lt25", 
                                  no = ifelse(test = value > p25 & value < p75,
                                              yes = "lt75",
                                              no = ifelse(test = value > p75 & value < p90,
                                                          yes = "lt90", 
                                                          no = ifelse(test = value > p90, 
                                                                      yes = "gt100",
                                                                      no = ifelse(value < p1,
                                                                                  yes = "lt1",
                                                                                  no = NA)))))))
#color lookup table
color_lookup <- data.frame(cat = c("lt1", "lt10", "lt25", "lt75", "lt90", "lt100", "gt100"), 
                           #col = c("red", "red3", "orange2", "forestgreen", "darkturquoise", "blue", "black"),
                           #col = brewer.pal(7, "Blues"),
                           #col = brewer.pal(7, "RdBu"),
                           col = c("#d73027", "#fc8d59", "#fddbc7", "#f7f7f7", "#e0f3f8",
                                   "#91bfdb", "#4575b4"),
                           stringsAsFactors = FALSE)
joinedPercOutflow <- left_join(joinedPercOutflow, color_lookup, by = "cat")




source('R/functions.R')
#add lwd column
streamSegments_filt@data <- streamSegments_filt@data %>% mutate(lwd = ifelse(SO < 3, yes = 0.4, no = 1)) %>% 
  mutate(lwd = ifelse(SO > 5, yes = 2, no = lwd))
plotFunc(plotDates, joinedPercOutflow, streamSegments_filt, color_lookup)
