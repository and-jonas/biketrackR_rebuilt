
library(raster)
library(scales)

# helper function to add previous positions
shift.vec <- function(vec, shift){
  if(length(vec) <= abs(shift)){
    rep(NA ,length(vec))
  } else {
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)])
    }
    else {
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) 
    } 
  } 
}

# helper function to calculate distance between points
calc_dist <- function (row) {
  pointDistance(c(as.numeric(row["lon.p1"]),
                  as.numeric(row["lat.p1"])),
                c(as.numeric(row["lon"]), as.numeric(row["lat"])),
                lonlat = T)
}

# extract geodata
get_geodata <- function(track){
  
  # extract track
  geo_df <- track$tracks[[1]][[1]] %>% as_tibble()
  
  # shiFt vectors for lat and lon, 
  # so that each row also contains the next position
  geo_df$lat.p1 <- shift.vec(geo_df$lat, -1)
  geo_df$lon.p1 <- shift.vec(geo_df$lon, -1)
  
  # Calculate distances (in metres)
  geo_df$dist.to.prev <- apply(geo_df, 1, FUN = calc_dist)
  
  # smooth elevation
  geo_df$lowess.ele <- lowess(geo_df$ele, f = 0.0001)$y
  
  # calculate total distance,
  geo_df <- transform(geo_df, dist_tot = ave(dist.to.prev, FUN = cumsum))
  # calculate total distance in kilometers
  tot_dist <- as.numeric(max(geo_df$dist_tot, na.rm = T)/1000)
  # simply transform from metres to kilometers
  geo_df <- transform(geo_df, dist_tot = rescale(dist_tot, to = c(0, tot_dist)))
  # round
  geo_df <- transform(geo_df, dist_tot = round(dist_tot, 2))
  # keep a subset of rows to reduce subsequent computational burden
  geo_df <- geo_df[seq(1, nrow(geo_df), by = 5),]
  
  geo_df$ele.p1 <- shift.vec(geo_df$ele, -1)
  geo_df$hdiff <- as.numeric(geo_df$ele.p1) - as.numeric(geo_df$ele)
  geo_df <- transform(geo_df, cum.uphill = ave(ifelse(geo_df$hdiff < 0, 0, geo_df$hdiff), FUN = cumsum))
  geo_df <- transform(geo_df, cum.downhill = ave(ifelse(geo_df$hdiff > 0, 0, - geo_df$hdiff), FUN = cumsum))
  
  #data types
  geo_df$ele <- as.numeric(geo_df$ele)
  geo_df$ele.p1 <- as.numeric(geo_df$ele.p1)
  
  # add a grouping variable
  geo_df$part <- "A"
  
  # classify tracks based on distance 
  geo_df$activity <- ifelse(max(geo_df$dist_tot, na.rm = T) < 22.5, "Wandern", "Fahrrad fahren")
  
  return(geo_df)
  
}

# create a height profile
profilplot <- function(data){
  
  data_list <- split(data, data$date)

  #get the daily distances
  max_dists <- lapply(data_list, "[[", 9) %>% lapply(., max, na.rm = TRUE)
  #shift forward by one 
  max_dists <- unlist(c(0, max_dists))
  #cumulative distance
  cum_dist <- cumsum(max_dists)
  #add to each distance the distances of previous days
  grand_tot <- list()
  for(i in 1:length(data_list)){
    grand_tot[[i]] <- data_list[[i]]$dist_tot + cum_dist[i]
  }
  
  for(i in 1:length(data_list)){
    data_list[[i]]$grand_tot_dist <- grand_tot[[i]]
  }
  
  # get daily max distance for vertical lines 
  daily_max_dist <- lapply(data_list, function(df) max(df$grand_tot_dist, na.rm = TRUE)) %>% 
    unlist() %>% unname()
  
  data <- bind_rows(data_list)
  
  #get min, max, range
  min_ele <- min(data$lowess.ele, na.rm = T)
  max_ele <- max(data$lowess.ele)
  range_ele <- range(data$lowess.ele, na.rm = T)
  
  profile <- ggplot(data, aes(y = lowess.ele, x = grand_tot_dist)) +
    geom_ribbon(aes(ymin = min_ele-25, ymax = data$lowess.ele, 
                    alpha = 0.5), fill="darkgreen") +
    geom_vline(xintercept = daily_max_dist) +
    scale_x_continuous(name = "Distanz [km]") +
    scale_y_continuous(name = "Höhe [M.ü.M]", 
                       expand = c(0, 0), 
                       breaks = seq(500, 3000, by = 500),
                       limits = c(min(data$lowess.ele)-25, 
                                  max(data$lowess.ele + 
                                        0.55*range_ele))) +
    geom_line(col = "grey20") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.border = element_blank(),
          plot.background = element_blank(),
          legend.position = "none",
          axis.line = element_line(colour = "grey50"))
  
  return(profile)
}

