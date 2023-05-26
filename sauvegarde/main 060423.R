

source("scripts_formatage/001_utils_packages.R")


# Paramètres --------------------------------------------------------------


CELL_SIZE = 5000


# Données environnementales -----------------------------------------------
# Occupation du sol -------------------------------------------------------

library(forcats)

onf =  st_read("data_enviro/sol_onf/oc_sol_2015.shp",
               ) %>% 
  dplyr::select(ID,
                NIVEAU3_15,
                geometry) %>% 
  rename(sol = NIVEAU3_15) %>% 
  mutate(sol = as.factor(sol)) %>% 
  mutate(sol = fct_collapse(sol,
                            Tissu_urbain = c("111", "112"),
                            Zone_urbaine_autre = c("113", "114"),
                            Zone_industrielle = c("121", "122", "123", "124"),
                            Decharge_mine_chantier = c("131", "132", "133"),
                            Riziere = c("213"),
                            Foret_type_cotiere = c("3151", "3152", "3153", "3154"),
                            Plan_d_eau = c("512", "513")
                              ))%>% 
  st_transform(., 2971)

pression_sol = onf$sol %>% 
  table() %>% 
  data.frame()

#library(suggest_crs)
#crsuggest::suggest_crs(onf)

plot = tm_shape(onf) +
  tm_fill(col="sol") +
  tm_borders() 

# Bioclimatique -----------------------------------------------------------



denviro_epoc = read_excel("denviro_epoc.xlsx", skip = 0, na = "NA") %>% 
  rename( Sample.Label = ID_FORM) %>% 
  mutate(elevation = scale(elevation),
         temperature_mean = scale(temperature_mean),
         precipitation_mean = scale(precipitation_mean),
         Temperature_amplitude_mean = scale(Temperature_amplitude_mean))



# Zone d'étude ------------------------------------------------------------


survey.area = onf$geometry %>%
  st_combine() 

guyane =  st_read("data_admini/limites_guyane500.shp") %>% 
  st_transform(., 2971)

plot  = tm_shape(guyane) +
  tm_fill() +
  tm_borders() 



survey.area_buf = st_buffer(guyane, dist = CELL_SIZE)

plot  = survey.area_buf %>% 
  tm_shape() +
  tm_fill()


grid <- st_make_grid(survey.area_buf, cellsize = CELL_SIZE,what="centers") %>% 
  st_intersection(., survey.area_buf) %>% 
  st_intersection(., onf) %>% 
  st_transform(., 2971)  %>% 
  st_as_sf()


grid_enviro = st_join(grid, onf["sol"]) 

st_buffer(grid, dist = CELL_SIZE) %>% plot()


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


source("scripts_formatage/004_filtrage.R")

library(Distance)

dspecies = subset(depoc, ID_SPECIES == 14345) 



dspecies_DS = dspecies %>% 
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
           as.factor()) %>% 
  st_drop_geometry() %>% 
  mutate(TIME_START_2 = TIME_START**2)



dspecies_DS$TIME_CAT = dspecies_DS$TIME_START%>% 
  as.vector() %>% 
  cut(.,
      breaks=c(0, 5*60,8*60, 14*60, 17*60, 19*60, 24*60),
      labels=c('Night','Dawn', 'Morning', 'Afternoon', 'Twilight','Night'))


dspecies_DS$SAISON = dspecies_DS$DATE%>% 
  month() %>% 
  as.vector() %>% 
  cut(.,
      breaks=c(0, 3, 6, 9, 12),
      labels=c('Winter', 'Spring', 'Summer','Fall'))




plot = ggplot(dspecies_DS, aes(x=distance, color = SAISON)) + geom_histogram()



res <- ds(dspecies_DS, 
          truncation = list(left=10,right=150),
          transect = "point",
          formula=~ SAISON) # ID_OBSERVER +  + TIME_CAT)  




t = res %>% 
  summary()

gof_ds(res)

plot(res,
     pdf=FALSE)

# Density surface modelling -----------------------------------------------


library("dsm")


df_ht = res


obsdata = dspecies %>% 
  dplyr::select(ID_FORM,
                DISTANCE,
                ID_OBSERVER)%>% 
  st_drop_geometry() %>% 
  mutate(size = rep(1, nrow(dspecies)),
         object = 1:nrow(dspecies)) %>% 
  rename(Sample.Label = ID_FORM,
         distance = DISTANCE)


segdata = dspecies %>% 
  dplyr::select(ID_FORM) %>% 
  rename(Sample.Label = ID_FORM) %>% 
  mutate(Effort = rep(1, nrow(.))) %>% 
  st_transform(., 2971) %>% 
  st_join(., onf["sol"]) %>% 
  st_drop_geometry() %>% 
  merge(., denviro_epoc)%>% 
  unique() 

segdata = left_join(segdata, unique(obsdata[,c("Sample.Label","ID_OBSERVER")]))


mod_tw <- dsm(density.est ~ sol + elevation + temperature_mean + Temperature_amplitude_mean, # + s(X,Y), 
              ddf.obj=df_ht, 
              segment.data=segdata, 
              observation.data=obsdata, 
              family=tw(), 
              transect="point")


summary(mod_tw)





# Prediction --------------------------------------------------------------

sol_retenus = subset(pression_sol, Freq > 6)
type_sol_considere = sol_retenus$.


elevation_guyane = rast("data_enviro/bioclim/moyenne_annuelle/elevation.tif") %>% 
  project(., "EPSG:2971", method = "near") %>% 
  crop(., survey.area_buf)


Tamplitude_mean_guyane = rast("data_enviro/bioclim/moyenne_annuelle/Tamplitude_mean.tif") %>% 
  project(., "EPSG:2971", method = "near") %>% 
  crop(., survey.area_buf)


temperature_mean_guyane = rast("data_enviro/bioclim/moyenne_annuelle/average_temp_mean.tif") %>% 
  project(., "EPSG:2971", method = "near") %>% 
  crop(., survey.area_buf)




grid_enviro_2 = grid_enviro %>%
  subset(., sol %in% type_sol_considere)


interm = terra::extract(elevation_guyane, grid_enviro_2)
grid_enviro_2 = cbind(grid_enviro_2, interm$elevation) %>% 
  rename(elevation = interm.elevation)

interm = terra::extract(Tamplitude_mean_guyane, grid_enviro_2)
grid_enviro_2 = cbind(grid_enviro_2, interm$wc2.1_30s_tmax_01) %>% 
  rename(Temperature_amplitude_mean = interm.wc2.1_30s_tmax_01)

interm = terra::extract(temperature_mean_guyane, grid_enviro_2)
grid_enviro_2 = cbind(grid_enviro_2, interm$wc2.1_30s_tavg_01) %>% 
  rename(temperature_mean = interm.wc2.1_30s_tavg_01)

  
grid_enviro_3 = grid_enviro_2 %>% 
  mutate(x = st_coordinates(grid_enviro_2)[, "X"],
         y = st_coordinates(grid_enviro_2)[, "Y"]) %>% 
  st_drop_geometry()
  


#plot_pred_by_term(mod_tw, grid_enviro_3, location.cov = c("x", "y"))

pred1 = predict(mod_tw, grid_enviro_3,  off.set = CELL_SIZE, type = "response")

sum(pred1, na.rm = TRUE)



grid_enviro_4 = grid_enviro_2 %>% 
  dplyr::select() %>% 
  mutate(prediction = as.numeric(pred1)) %>% 
  dplyr::select(prediction)

tm_shape(grid_enviro_4) +
  tm_squares(size= 0.07, 
             col="prediction",
             border.alpha = 0)

