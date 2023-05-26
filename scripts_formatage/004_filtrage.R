


depoc = read_excel("data_epoc.xlsx", skip = 0, na = "NA") 


depoc <- st_as_sf(depoc,
                   coords = c("TRACE_BARY_X","TRACE_BARY_Y"),
                   crs = 4326) %>% 
  dplyr::select(geometry,
                ID_FORM, PROJECT_CODE,
                ID_OBSERVER,
                DATE, TIME_START, 
                ID_SPECIES,
                DISTANCE,
                DURATION,
                LATIN_SPECIES,
                TRACE_disp)

#filtre
depoc = subset(depoc, DURATION %in% 5:6)

#filtre
depoc = subset(depoc, TRACE_disp < 100*mean(depoc$TRACE_disp))

#filtre
depoc = subset(depoc, DISTANCE < 500 & DISTANCE > 0) #min a mettre a zero


#filtre

table(depoc$ID_OBSERVER) %>% 
  sort()

#Observer with more than 500 data: 1107, 41, 478, 57, 1317, 1680, 1026

depoc = subset(depoc, ID_OBSERVER %in% c(1107, 41, 478, 57, 1317, 1680, 1026))



#Nombre de points EPOC exploitables
depoc$ID_FORM %>% 
  unique() %>% 
  length()

#filtre

table(depoc$ID_SPECIES) %>% 
  sort()

