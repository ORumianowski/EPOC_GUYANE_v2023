source("001_utils_packages.R")


depoc0 = read_excel("data/FG_EPOC (export_221_35745_17022023_101127).xlsx", 
                   skip = 0, 
                   na = "",
                   col_types = c("guess", "guess", "guess", "guess", "guess",
                                 "guess", "guess", "date", "guess", "guess",
                                 "date", "date", "guess", "guess", "guess",
                                 "guess", "guess", "guess", "guess", "guess",
                                 "guess", "guess", "guess", "guess", "guess",
                                 
                                 "guess", "guess", "guess", "guess", "guess",
                                 "guess", "guess", "guess", "guess", "guess",
                                 "guess", "guess", "guess", "guess", "guess",
                                 "guess", "guess", "guess", "guess", "guess",
                                 "guess", "guess", "guess", "guess", "guess",
                                 
                                 "guess", "guess", "guess", "guess", "guess"
                                 )
                   )


depoc = depoc0 [-2, ]%>% 
  dplyr::select(ID_SPECIES, LATIN_SPECIES, 
                DATE, TIME_START, TIME_STOP,
                TRACE, 
                ID_FORM,
                COORD_LAT, COORD_LON,
                ID_OBSERVER,
                TRA_SURNAME, TRA_NAME,
                PROJECT_CODE) %>% 
  subset(., PROJECT_CODE == 10 | PROJECT_CODE == "EPOC" | PROJECT_CODE == "EPOC-ODF") %>% 
  mutate(COORD_LAT = as.numeric(COORD_LAT),
         COORD_LON  = as.numeric(COORD_LON))


depoc$TIME_STOP = lubridate::hour(depoc$TIME_STOP)*60 + lubridate::minute(depoc$TIME_STOP)

depoc$TIME_START = lubridate::hour(depoc$TIME_START)*60 + lubridate::minute(depoc$TIME_START)

depoc$DURATION = depoc$TIME_STOP - depoc$TIME_START


 
formatage_trace = function(TRACE){
  
  
  tr <- gsub("LINESTRING|[(]|[)]","", TRACE)
  tr <- gsub(",",":",tr)
  
  
  Y_Position_obs_L93 <- gsub(": [0-9]{1,}.[0-9]{1,}|: -[0-9]{1,}.[0-9]{1,}|: [0-9]{1} .+:|: -[0-9]{1} .+:|^[0-9]{1,}.[0-9]{1,}|^[0-9]{1,}|^-[0-9]{1,}.[0-9]{1,}|^-[0-9]{1,}","",tr) %>% 
    str_split(., " ") 
  
  Y_Position_obs_L93 = Y_Position_obs_L93[[1]][-1] %>% 
    as.numeric()
  
  X_Position_obs_L93 <- gsub(" [0-9]{1,}.[0-9]{1,}:| -[0-9]{1,}.[0-9]{1,}:| [0-9]{1,}:| -[0-9]{1,}:| [0-9]{1,}.[0-9]{1,}$| [0-9]{1,}$","",tr)%>% 
    str_split(., " ") 
  
  
  X_Position_obs_L93 = X_Position_obs_L93[[1]] %>% 
    as.numeric()
  
  
  
  #trace_expli = cbind(X_Position_obs_L93, Y_Position_obs_L93) %>% 
   # st_linestring()
  
  
  d_x = quantile(X_Position_obs_L93,  probs = c(0.8)) - quantile(X_Position_obs_L93,  probs = c(0.2))
  d_y = quantile(Y_Position_obs_L93,  probs = c(0.8)) - quantile(Y_Position_obs_L93,  probs = c(0.2))
  disp = d_x * d_y
  
  trace_expli = c(mean(X_Position_obs_L93), mean(Y_Position_obs_L93), disp)
  

}


depoc$TRACE_BARY_X = NULL
depoc$TRACE_BARY_Y = NULL

for (k in 1:length(depoc$TRACE)){
  depoc$TRACE_BARY_X[k] = formatage_trace(depoc$TRACE[k])[1]
  depoc$TRACE_BARY_Y[k] = formatage_trace(depoc$TRACE[k])[2]
  depoc$TRACE_disp[k] = formatage_trace(depoc$TRACE[k])[3]
}


loc_obs.sf <- st_as_sf(depoc,
                       coords = c("COORD_LON","COORD_LAT"),
                       crs = 4326)

loc_bary.sf <- st_as_sf(depoc,
                       coords = c("TRACE_BARY_X","TRACE_BARY_Y"),
                       crs = 4326)




dist_observation <- st_distance(x = loc_obs.sf,
                                           y = loc_bary.sf,
                                           by_element = TRUE)


depoc$DISTANCE = dist_observation


#données infomationnelle
#write_xlsx(depoc, "data_epoc.xlsx")

#données de la trace
#write_xlsx(depoc, "data_epoc.xlsx")

#données d'analyse
write_xlsx(depoc, "data_epoc.xlsx")

