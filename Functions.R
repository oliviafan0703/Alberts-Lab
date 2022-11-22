# ---- FUNCTION TO SUBTRACT 6 MONTHS FROM DATE ----------------------------
# up = 1, down = -1
six.mo.mover<-function(date,up.or.down) {
  last.day <- month(date) != month(as.Date(date)+1) 
  if(last.day) {
    adj.date <- as.Date(date) - day(as.Date(date)-1) + up.or.down*months(6)
    adj.mo <- month(adj.date)
    if (adj.mo == 2) {
      dy <- 28 + leap_year(year(adj.date))
    }
    else {
      dy <- 31-(adj.mo-1)%%7%%2
    }
    adj.date + days(dy-1)
  } 
  else {
    as.Date(date)+up.or.down*months(6)
  }
}

# ---- FUNCTIONS FOR RAINFALL ---------------------------------------------
# Function to get hydroyear
get_hydroyear <- function(d) {
  
  if (!(is.Date(d) | is.POSIXct(d))) {
    stop("The hydroyear function only works on dates.")
  }
  year_of <- lubridate::year(d)
  month_of <- lubridate::month(d)
  
  year_of <- ifelse(month_of >= 11, year_of + 1, year_of)
  
  return(year_of)
}

# Helper function to get season
get_season <- function(d) {
  if (!(is.Date(d) | is.POSIXct(d))) {
    stop("The season function only works on dates.")
  }
  month_of <- lubridate::month(d)
  season <- ifelse(month_of >= 6 & month_of <= 10, "Dry", "Wet")
  return(season)
}

# Function to calculate average rain during a given date window
get_rain_normal <- function(rain_df, date_col, rain_col, d_start, d_end) {
  
  if (d_end < d_start) message("Error: end date before start date.")
  
  yday_start <- yday(d_start)
  yday_end <- yday(d_end)
  
  # Make complete sequence of dates and fill with zeros
  rain_df <- rain_df %>%
    rename(date_of = !!date_col) %>%
    complete(date_of = full_seq(date_of, 1), fill = list(rain = 0)) %>%
    mutate(yday = yday(!!date_col))
  
  if (yday_end > yday_start) {
    
    r <- rain_df %>%
      filter(yday >= yday_start & yday <= yday_end) %>%
      mutate(year_of = year(!!date_col)) %>%
      group_by(year_of) %>%
      summarise(sum_rain = sum(!!rain_col, na.rm = TRUE),
                n = n())
    
    # Remove any years with truncated data
    r <- r %>%
      filter(n == median(n))
    
    return(mean(r$sum_rain))
    
  } else { # Time interval is split across calendar years
    r <- rain_df %>%
      filter(yday >= yday_start | yday <= yday_end) %>%
      mutate(year_of = year(!!date_col),
             year_of = fifelse(yday >= yday_start, year_of,
                               fifelse(yday <= yday_end, year_of - 1, -999), -9999)) %>%
      group_by(year_of) %>%
      summarise(sum_rain = sum(!!rain_col, na.rm = TRUE),
                n = n())
    
    # Remove any years with truncated data
    r <- r %>%
      filter(n == median(n))
    
    return(mean(r$sum_rain))
  }
  
}

get_rain_total <- function(rain_df, date_col, rain_col, d_start, d_end) {
  if (d_end < d_start) message("Error: end date before start date.")
  
  # Make complete sequence of dates and fill with zeros
  rain_total <- rain_df %>%
    rename(date_of = !!date_col) %>%
    filter(date_of >= d_start & date_of <= d_end) %>%
    summarise(sum_rain = sum(!!rain_col, na.rm = TRUE)) %>%
    pull(sum_rain)
  
  return(rain_total)
}


# Function to calculate average rainfall during a given date window
get_rain_avg <- function(rain_df, start, end) {
  sub.rain.df <- subset(rain_df, start <= date_of & date_of <= end)
  return(mean(sub.rain.df$rain, na.rm=T))
}

# Function to calculate rainfall variability during a given date window
get_rain_var <- function(rain_df, start, end) {
  sub.rain.df <- subset(rain_df, start <= date_of & date_of <= end)
  return(vegan::diversity(sub.rain.df$rain, index = "shannon"))
}


# ---- FUNCTIONS FOR TEMPERATURE ---------------------------------------------

# Function to calculate avg min temperature during a given date window
get_min_temp <- function(temp_df, start, end) {
  sub.temp.df <- subset(temp_df, start <= date & date <= end)
  return <- mean(sub.temp.df$tempmin, na.rm = T)
}

# Function to calculate avg max temperature during a given date window
get_max_temp <- function(temp_df, start, end) {
  sub.temp.df <- subset(temp_df, start <= date & date <= end)
  return <- mean(sub.temp.df$tempmax, na.rm = T)
}


# ---- FUNCTIONS FOR GRP OBSERVATION EFFORT ----------------------------------
# Function to calculate grp focal mins during a given date window
get_focal_mins <- function(focal_df, start, end, grp) {
  #browser()
  sub.focal.df <- subset(focal_df, start <= date & date <= end & grp_of_focal == grp) # DO I WANT GRP OR GRP_OF_FOCAL
  return(sum(sub.focal.df$minsis, na.rm=T))
}


# ---- FUNCTION FOR DSI DATAFRAME -------------------------------------------
#' @examples
make_target_date_df_l <- function(target_df, babase, members_l, .by_grp = TRUE) {
  
  if (class(babase) != "PostgreSQLConnection") {
    stop("Invalid connection to babase.")
  }
  
  # Return an empty tibble if the subset is empty
  if (is.null(target_df) |
      !all(c("sname", "sex", "date") %in% names(target_df))) {
    stop("Problem with input data. Target data frame must include rows 'sname', 'sex', and 'date'.")
  }
  
  # babase-tables -----------------------------------------------------------
  
  message("Creating connections to babase tables...")
  
  # Database connections
  biograph <- dplyr::tbl(babase, "biograph")
  maturedates <- dplyr::tbl(babase, "maturedates")
  rankdates <- dplyr::tbl(babase, "rankdates")
  
  # Local
  biograph_l <- dplyr::collect(biograph)
  
  md_females <- maturedates %>%
    dplyr::semi_join(dplyr::filter(biograph, sex == "F"), by = "sname") %>%
    collect()
  
  rd_males <- rankdates %>%
    dplyr::semi_join(dplyr::filter(biograph, sex == "M"), by = "sname") %>%
    collect()
  
  # Find last date
  last_date <- max(members_l$date)
  
  message("Creating target-date data set...")
  
  target_df <- target_df %>%
    dplyr::left_join(biograph_l, by = c("sname", "sex")) %>%
    dplyr::left_join(dplyr::select(md_females, sname, matured), by = "sname") %>%
    dplyr::left_join(dplyr::select(rd_males, sname, ranked), by = "sname") %>%
    dplyr::select(sname, obs_date = date, start, end, sex, birth, statdate, matured, ranked)


  target_df <- target_df %>%
    dplyr::filter(start <= end) %>%
    arrange(sname, obs_date)
  
  # .by_grp <- TRUE
  if (.by_grp) {
    ## Check in which groups the individual was present in the focal year
    ## and create one row per focal year per group
    temp <- target_df %>%
      dplyr::left_join(dplyr::select(members_l, sname, date, grp), by = c("sname")) %>% #this is where I drop in number of individuals
      dplyr::filter(date >= start & date <= end) %>%
      dplyr::distinct(sname, start, end, grp)
    
    zdata <- target_df %>%
      dplyr::inner_join(temp, by = c("sname", "start", "end")) %>%
      tibble::rownames_to_column()
    
    ## And check how many days the focal was present in the group in a focal year
    zdata <- zdata %>%
      dplyr::inner_join(dplyr::select(members_l, sname, grp, date), by = c("sname", "grp")) %>%
      dplyr::filter(date >= start & date <= end) %>%
      dplyr::group_by(sname, grp, start, end, rowname) %>%
      dplyr::summarise(days_present = n()) %>%
      dplyr::arrange(sname, grp, start, end)
    
    target_df <- zdata %>%
      dplyr::inner_join(target_df, by = c("sname", "start", "end")) %>%
      dplyr::arrange(sname, grp, start, end) %>%
      dplyr::select(-rowname)
  } else {
    ## Check how many days the focal was present in ANY group in a focal year
    # temp <- target_df %>%
    #   dplyr::inner_join(dplyr::select(members_l, sname, date), by = c("sname")) %>%
    #   dplyr::filter(date >= start & date <= end) %>%
    #   dplyr::group_by(sname, start, end) %>%
    #   dplyr::summarise(days_present = n()) %>%
    #   dplyr::arrange(sname, start, end)
    #
    # target_df <- temp %>%
    #   dplyr::inner_join(target_df, by = c("sname", "start", "end")) %>%
    #   dplyr::arrange(sname, start, end)
    
    stop("Not Yet Completed.")
  }
  
  # Calculate date variables
  target_df <- target_df %>%
    dplyr::mutate(midpoint = start + floor((end - start) / 2),
                  age_start_yrs = as.numeric(start - birth) / 365.25,
                  age_class = floor(plyr::round_any(age_start_yrs, 0.005)) + 1,
                  first_start_date = start)
  
  target_df <- dplyr::ungroup(target_df) %>%
    distinct()
  
  return(target_df)
}

# ---- FUNCTIONS FOR CONSORTS ---------------------------------------------
# Function to calculate average temperature during a given date window

#sub.consorts.df <- consorts %>%
#  filter(actee=="EUR" & as.Date("2017-11-22") <= date & date <= as.Date("2018-11-21"))
#count(sub.consorts.df, n())

#sub.consorts.df <- consorts %>%
#sub.consorts.df <- filter(consorts, actee=="EUR" & as.Date("2017-11-22") <= date & date <= as.Date("2018-11-21"))
#count(sub.consorts.df, n())

# ---- FUNCTION FOR SOCIAL NETWORK ANALYSIS --------------------------------
# Function to get all grooms in a group during a given date window
subset_grooming <- function(my_grp, my_start, my_end) {
  result <- grooming_l %>%
    filter(actee_grp == my_grp, date >= my_start, date <= my_end)
}

# Function to get all member accounts in a group during a given date window
subset_membership <- function(my_grp, my_start, my_end) {
  result <- adult_members %>%
    filter(grp == my_grp, date >= my_start, date <= my_end)
}

# IN PROGRESS- Function to get all member accounts in a group during a given date window
subset_membership_WITHOUT183 <- function(my_grp, my_start, my_end) {
  #Count days with membership for each individual
  adult_members_days <- adult_members %>%
    filter(grp == my_grp, date >= my_start, date <= my_end) %>%
    group_by(sname, .drop = FALSE) %>%
    tally() %>%
    dplyr::rename(days_present=n) %>%
    mutate(start = my_start, 
           end = my_end) %>%
    ungroup()
  
  #Create temporary data frame with grp data
  adult_grp <- adult_members %>%
    filter(grp == my_grp, date >= my_start, date <= my_end) %>%
    group_by(sname) %>%
    slice(which.max(grp))
  
  #Join adult group to data frame
  result <- adult_members_days %>%
    dplyr::left_join(select(adult_grp, sname, grp, sex), by = "sname") %>%
    filter(days_present >= 183)

}


# Function to get network density - only nodes with grooms
get_net_density_og <- function(my_df) { # Or use a more descriptive function name
  #Create edges
  edges.df <- my_df %>%
    select(actor, actee) %>% #need actor, actee, total grooms
    mutate(pairs = paste(actor, actee))
  edges <- edges.df %>%
    count(pairs) %>%
    mutate(actor = str_sub(pairs, 1, 3),
           actee = str_sub(pairs, 5, 7)) %>% 
    rename(weight = n) %>% 
    select(actor, actee, weight, -pairs)
  #edgelist <- my_df %>% select(actor, actee) #create edges
  #edges <- as.matrix(edgelist) #create edge matrix
  #Create nodes
  actor <- my_df %>%
    select(actor) %>%
    rename(name = actor)
  actee <- my_df %>%
    select(actee) %>%
    rename(name = actee)
  nodes <- rbind(actor, actee) %>%
    distinct(name,.keep_all = TRUE) #need to fix to include individuals with no grooms
  nodes$id <- 1:nrow(nodes)
  my_network <- graph_from_data_frame(d=edges, vertices=nodes, directed=TRUE) #create the network object from my_df
  my_network <- asNetwork(my_network)
  network_density <- network::network.density(my_network)
  network_density
}


# Function to get network density - includes nodes without grooms
get_net_density_wog <- function(grooms_df, members_df) { 
  #Create edges
  edges.df <- grooms_df %>%
    select(actor, actee) %>% #need actor, actee, total grooms
    mutate(pairs = paste(actor, actee))
  edges.df <- edges.df %>%
    count(pairs) %>%
    mutate(actor = str_sub(pairs, 1, 3),
           actee = str_sub(pairs, 5, 7)) %>% 
    rename(weight = n) %>% 
    select(actor, actee, weight, -pairs)
  #edgelist <- grooms_df %>% select(actor, actee) #create edges
  #edges <- as.matrix(edgelist) #create edge matrix
  #Create nodes
  nodes <- members_df %>%
    rename(name = sname) %>%
    distinct(name,.keep_all = TRUE) %>%
    select(name, sex)
  nodes$id <- 1:nrow(nodes)
  #browser()
  edges <- edges.df[(as.character(edges.df$actor) %in% as.character(nodes$name)), ] #Removes individs with grooms but no membership from edges
  edges <- edges[(as.character(edges$actee) %in% as.character(nodes$name)), ] #Removes individs with grooms but no membership from edges
  #edges <- edges.df %>%
  #  subset(actor %in% nodes$name | actee %in% nodes$name) #Removes individs with grooms but no membership from edges
  #edges <- subset(edges.df, actor %in% nodes$name | actee %in% nodes$name)
  my_network <- graph_from_data_frame(d=edges, vertices=nodes, directed=TRUE) #create the network object from my_df
  my_network <- asNetwork(my_network)
  network_density <- network::network.density(my_network)
  network_density
}
