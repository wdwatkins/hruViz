library(maptools)
#local parallelization doesnt seem to help, probably memory limited
plotFunc <- function(plotDates, joinedPercOutflow, streamSegments_filt, color_lookup) {
  #get state shapes from maps package
  map <- to_sp(proj.string = streamSegments_filt@proj4string, 
               database="state")
  for(i in seq_along(plotDates)) {
    d <- plotDates[i]
    #filter to particular day, join to spatial df
    dayDF <- joinedPercOutflow %>% filter(Date == d) %>% mutate(seg_id=as.character(seg_id)) %>% 
      rename(seg_id_nat=seg_id)
    day_spatial <- left_join(streamSegments_filt@data, dayDF, by = "seg_id_nat")
    
    #actual plotting
    # png(filename = paste0("blue/day_", sprintf("%03d", i), ".png"),
    #     width = 1280, height = 720)
    
    pdf(file = paste0("blue/day_", sprintf("%03d", i), ".pdf"),
        width = 12, height = 6.5)
    par(mar=c(0,0,2,4))
    plot(map)
    plot(streamSegments_filt, col = day_spatial$col, add = TRUE, lwd = day_spatial$lwd)
    #TODO: nicer colors; key
    legend(x = grconvertX(0.85, from = "npc"),
           y = grconvertY(0.6, from = "npc"), 
           legend = c('Low', '<10%', '10-24%', '25%-75%', '76%-90%', '>90%', 'High'),
           title = "Flow Percentile",
           bty = 'o',
           pch = 15,
           pt.cex = 2,
           col    = c(color_lookup$col),
           cex    = 1.3 )
    title(main = "Simulated Stream Flow Percentiles\nJune 1, 2010", 
          line = -2, 
          cex.main = 1.8)
    #create timeline!
    #main line
    # yMain <- 0.1
    # leftLimit <- 0.03
    # rightLimit <- 0.35
    # quarter <- (rightLimit - leftLimit) / 4
    # year <- (rightLimit - leftLimit) / 365
    # yearSeq <- seq(from = leftLimit, to = rightLimit, by = year)
    # quarterSeq <- seq(from = leftLimit+quarter, to = rightLimit - quarter, by = quarter)
    # arrows(x0 = grconvertX(leftLimit, from = "npc"), 
    #        y0 = grconvertY(yMain, from = "npc"), 
    #        x1 = grconvertX(rightLimit, from = "npc"), code =3, angle = 90, lwd = 5)
    # segments(x0 = grconvertX(quarterSeq, from = "npc"),
    #          y0 = grconvertY(0.08, from = "npc"), y1 = grconvertY(0.12, from = "npc"))
    # segments(y0 = grconvertY(0.07, from = "npc"), 
    #          y1 = grconvertY(0.13, from = "npc"),
    #          x0 = grconvertX(grconvertX(yearSeq[i]), from = "npc"), lwd = 8, col = "red")
    # text(x = grconvertX(quarterSeq, from = "npc"), pos = 1, 
    #      y = grconvertY(0.07, from = "npc"), 
    #      labels = c("Apr", "Jul", "Oct"), cex = 2)
    # text(x = grconvertX(quarterSeq[2], from = "npc"), y = grconvertY(0.13, from = "npc"),
    #      labels = d, cex = 2.5, pos = 3)
    dev.off()
    print(paste("done", d))
  }
}


to_sp <- function(proj.string, ...){
  map <- maps::map(..., fill=TRUE, plot = FALSE)
  IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
  map.sp <- map2SpatialPolygons(map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
  map.sp.t <- spTransform(map.sp, proj.string)
  return(map.sp.t)
}