source("001_utils_packages.R")



# Données environnementales -----------------------------------------------



onf =  st_read("data_enviro/oc_sol_2015.shp",
               ) %>% 
  dplyr::select(ID,
                NIVEAU3_15,
                geometry) %>% 
  rename(sol = NIVEAU3_15) 

#library(suggest_crs)
#crsuggest::suggest_crs(onf)

onf = onf %>% 
  st_transform(., 2971)

tm_shape(onf) +
  tm_fill(col="sol") +
  tm_borders() 


# Zone d'étude ------------------------------------------------------------


survey.area = onf$geometry %>%
  st_combine() 


library("raster")
library("rgeos")
library("dismo")

guyane =  st_read("data_admini/limites_guyane500.shp",
) 

guyane = guyane %>% 
  st_transform(., 2971)

tm_shape(guyane) +
  tm_fill() +
  tm_borders() 



survey.area_buf = st_buffer(guyane, dist = 1000 * 1)

survey.area_buf %>% 
  tm_shape() +
  tm_fill()


j <- st_make_grid(survey.area_buf, cellsize = 1000,what="centers") # grid de point espace de 1km
j1 <- st_intersection(j, survey.area_buf) # intersection w/ guyane

plot(j1)


grid <- st_intersection(j1, onf)%>% 
  st_transform(., 2971)  %>% 
  st_as_sf()



plot(grid)

grid_enviro = st_join(grid, onf["sol"]) 


# Données d'observation ---------------------------------------------------


  
depoc = read_excel("data_epoc.xlsx", skip = 0, na = "NA") 


depoc <- st_as_sf(depoc,
                  coords = c("TRACE_BARY_X","TRACE_BARY_Y"),
                  crs = 2971) %>% 
  dplyr::select(geometry,
                ID_FORM, PROJECT_CODE,
                ID_OBSERVER,
                DATE, TIME_START, 
                ID_SPECIES,
                DISTANCE,
                DURATION,
                LATIN_SPECIES,
                TRACE_disp)


# Detection function ------------------------------------------------------


source("004_filtrage.R")

library(Distance)

dquiquivi_DS = dquiquivi %>% 
  dplyr::select(DISTANCE, ID_OBSERVER, DATE, TIME_START) %>% 
  rename(distance = DISTANCE ) %>% 
  mutate(distance = distance %>% 
           as.numeric(),
         ID_OBSERVER = ID_OBSERVER %>% 
           as.factor(),
         TIME_START = TIME_START %>% 
           as.numeric(),
         MONTH = DATE %>% 
           month() %>% 
           as.factor()
  ) %>% 
  st_drop_geometry() %>% 
  mutate(TIME_START_2 = TIME_START**2)



dquiquivi_DS$TIME_CAT = dquiquivi_DS$TIME_START%>% 
  as.vector() %>% 
  cut(.,
      breaks=c(0, 5*60,8*60, 14*60, 17*60, 19*60, 24*60),
      labels=c('Night','Dawn', 'Morning', 'Afternoon', 'Twilight','Night'))


dquiquivi_DS$SAISON = dquiquivi_DS$DATE%>% 
  month() %>% 
  as.vector() %>% 
  cut(.,
      breaks=c(0, 3, 6, 9, 12),
      labels=c('Winter', 'Spring', 'Summer','Fall'))




ggplot(dquiquivi_DS, aes(x=distance, color = SAISON)) + geom_histogram()



res <- ds(dquiquivi_DS, 
          truncation = list(left=10,right=150),
          transect = "point",
          formula=~ 1) #  ID_OBSERVER + SAISON + TIME_CAT)  




t = res %>% 
  summary()


# Density surface modelling -----------------------------------------------

library("dsm")


df_ht = res


obsdata = dquiquivi %>% 
  dplyr::select(ID_FORM,
                DISTANCE)%>% 
  st_drop_geometry() %>% 
  mutate(size = rep(1, nrow(obsdata)),
         object = seq.int(nrow(obsdata))) %>% 
  rename(Sample.Label = ID_FORM,
         distance = DISTANCE)


segdata = dquiquivi %>% 
  dplyr::select(ID_FORM) %>% 
  rename(Sample.Label = ID_FORM) %>% 
  mutate(Effort = rep(1, nrow(.))) %>% 
  st_transform(., 2971) %>% 
  st_join(., onf["sol"]) %>% 
  st_drop_geometry() %>% 
  unique()


mod_tw <- dsm(count~sol, 
              ddf.obj=df_ht, 
              segment.data=segdata, 
              observation.data=obsdata, 
              family=tw(), 
              transect="point")


summary(mod_tw)



# Prediction --------------------------------------------------------------

pression_sol = segdata$sol %>% 
  table() %>% 
  data.frame()

sol_retenus = subset(pression_sol, Freq > 6)

type_sol_considere = sol_retenus$.


grid_enviro_2 = grid_enviro %>%
  subset(., sol %in% type_sol_considere)
  
 # subset(., sol %in% segdata$sol %>% 
#           unique())


grid_enviro_2  %>% 
  tm_shape() + tm_dots(col="sol", size = 0.3)


mod_tw_pred <- predict(mod_tw, grid_enviro_2, off.set = 1000)


# essai 2

grid_enviro_3 = grid_enviro_2 %>% 
  mutate(x = st_coordinates(grid_enviro_2)[, "X"],
         y = st_coordinates(grid_enviro_2)[, "Y"]) %>% 
  st_drop_geometry()
  


plot_pred_by_term(mod_tw, grid_enviro_3, location.cov = c("x", "y"))




