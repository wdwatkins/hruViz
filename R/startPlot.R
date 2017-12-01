library(rgdal)
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)

streamSegments <- readOGR(dsn = "OfficialSegments", 
                          layer = "OfficialSegments", stringsAsFactors = FALSE)
streamOrder <- fread('gfSegsStreamOrder.csv', colClasses = c(seg_id_nat="character"))
streamSegments@data <- left_join(streamSegments@data, streamOrder, by = "seg_id_nat")
streamSegments_filt <- subset(streamSegments, streamSegments$SO > 4 & !region.y %in% c("19", "20", "21"))

#TODO: combine these to one table?  not unmanageable size
#go to long?
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

startDate <- "1997-04-01"
endDate <- "1997-08-01"
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
                           col = c("red", "red3", "orange2", "forestgreen", "darkturquoise", "blue", "black"),
                           stringsAsFactors = FALSE)
joinedPercOutflow <- left_join(joinedPercOutflow, color_lookup, by = "cat")

#local parallelization doesnt seem to help, probably memory limited
plotFunc <- function(plotDates, joinedPercOutflow, streamSegments_filt) {
   for(i in seq_along(plotDates)) {
    d <- plotDates[i]
    #filter to particular day, join to spatial df
    dayDF <- joinedPercOutflow %>% filter(Date == d) %>% mutate(seg_id=as.character(seg_id)) %>% 
      rename(seg_id_nat=seg_id)
    day_spatial <- left_join(streamSegments_filt@data, dayDF, by = "seg_id_nat")
    
    #actual plotting
    png(filename = paste0("img/day_", sprintf("%03d", i), ".png"))
    plot(streamSegments_filt, col = day_spatial$col)
    dev.off()
    print(paste("done", d))
  }
}
plotFunc(plotDates, joinedPercOutflow, streamSegments_filt)