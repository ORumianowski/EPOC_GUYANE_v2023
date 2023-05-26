

source("scripts_formatage/001_utils_packages.R")

# Bioclimatique -----------------------------------------------------------


elevation_guyane = rast("data_enviro/bioclim/moyenne_annuelle/elevation.tif") %>% 
  project(., "EPSG:2971", method = "near")

T_amplitude_mean_guyane = rast("data_enviro/bioclim/moyenne_annuelle/Tamplitude_mean.tif") %>% 
  project(., "EPSG:2971", method = "near")

T_mean_guyane = rast("data_enviro/bioclim/moyenne_annuelle/average_temp_mean.tif") %>% 
  project(., "EPSG:2971", method = "near") 


# Zone d'étude ------------------------------------------------------------

grid =  st_read("grid/grid1000/grid1000.shp") %>% 
  st_transform(., 2971) %>% 
  st_as_sf()

grid_prop = read_excel("prop_occupancy/grid_prop_1000.xlsx", skip = 0, na = "NA") 

grid_full = merge(grid, grid_prop) %>% 
  mutate(elevation = terra::extract(elevation_guyane, grid)[2] %>% 
           scale()) %>% 
  mutate(T_amplitude_mean = terra::extract(T_amplitude_mean_guyane, grid)[[2]]%>% 
           scale()) %>% 
  mutate(T_mean = terra::extract(T_mean_guyane, grid)[[2]]%>% 
           scale()) 


grid_full_drop  = grid_full %>% 
  mutate(x = st_coordinates(.)[, "X"],
         y = st_coordinates(.)[, "Y"]) %>% 
  st_drop_geometry() %>% 
  dplyr::select(., -FID)
  

# Données d'observation ---------------------------------------------------

depoc = read_excel("data_epoc.xlsx", skip = 0, na = "NA") %>% 
  st_as_sf(.,
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
                TRACE_disp) %>% 
  rename(Sample.Label = ID_FORM )


denviro_epoc = read_excel("denviro_epoc.xlsx", skip = 0, na = "NA") %>% 
  rename( Sample.Label = ID_FORM) %>% 
  rename(T_mean = temperature_mean,
         T_amplitude_mean = Temperature_amplitude_mean) %>% 
  mutate(elevation = scale(elevation),
         T_mean = scale(T_mean),
         T_amplitude_mean = scale(T_amplitude_mean)) %>% 
  unique()


epoc_prop = read_excel("prop_occupancy/epoc_prop.xlsx", skip = 0, na = "NA")%>% 
  rename( Sample.Label = ID_FORM) %>% 
  unique()


depoc_full = merge(depoc, 
                   inner_join(epoc_prop, denviro_epoc),
                   by = "Sample.Label") 

depoc_full = depoc_full %>% 
  mutate(couverture = Zone_urbaine + Zone_interface + Zone_vegetalise_artificiel 
                       + Foret +  Mangrove + Zone_humide)



depoc_full =   subset(depoc_full, DISTANCE < 150 ) %>% 
  subset(., couverture >= 0.8 ) %>% 
  subset(., couverture <=  1) 
  

# Detection function ------------------------------------------------------


source("scripts_formatage/004_filtrage.R")

library(Distance)

dspecies = subset(depoc_full, ID_SPECIES == 14345) 



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
  st_drop_geometry()



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
          formula=~ SAISON )#+ TIME_CAT + ID_OBSERVER )  




res_summary = res %>% 
  summary()

gof_ds(res)

plot(res,
     pdf=TRUE)

# Density surface modelling -----------------------------------------------


library("dsm")


df_ht = res


obsdata = dspecies %>% 
  dplyr::select(Sample.Label,
                DISTANCE,
                ID_OBSERVER)%>% 
  st_drop_geometry() %>% 
  mutate(size = rep(1, nrow(dspecies)),
         object = 1:nrow(dspecies)) %>% 
  rename(distance = DISTANCE) 


segdata = dspecies %>% 
  st_drop_geometry() %>% 
  mutate(Effort = rep(1, nrow(.))) %>% 
  dplyr::select(Sample.Label,
                Effort,
                Zone_urbaine, Zone_interface, Zone_vegetalise_artificiel,
                Foret,  Mangrove, Zone_humide,
                T_mean, T_amplitude_mean) %>% 
  unique()

#appliquer un unique


#segdata = left_join(segdata, unique(obsdata[,c("Sample.Label","ID_OBSERVER")]))

#'Sample.Labels are non-unique in segment data!

mod_tw <- dsm(abundance.est ~ 
                Zone_urbaine + Zone_interface + Zone_vegetalise_artificiel 
                + Foret +  Mangrove + Zone_humide
                + T_mean + T_amplitude_mean, # + s(X,Y), 
              ddf.obj=df_ht, 
              segment.data=segdata, 
              observation.data=obsdata, 
              family=tw(), 
              transect="point")


summary(mod_tw)





# Prediction --------------------------------------------------------------


#plot_pred_by_term(mod_tw, grid_enviro_3, location.cov = c("x", "y"))

CELL_SIZE = 1000
LENGTH_SIZE = CELL_SIZE / 1000


pred = predict(mod_tw, grid_full_drop,  
                off.set = CELL_SIZE, type = "response")

pred_cor  = pred *(LENGTH_SIZE*LENGTH_SIZE)/(3.14*0.15*.15)

pred_density = exp(pred) /(3.14*0.15*.15)

(pred_cor/LENGTH_SIZE*LENGTH_SIZE) %>% summary()
sum(pred_cor, na.rm = TRUE) #pour abundance


pred_density%>% summary()
sum(pred_density, na.rm = TRUE) #pour density

grid_pred = grid_full %>% 
  dplyr::select() %>% 
  mutate(prediction = as.numeric(pred_cor)) %>% 
  dplyr::select(prediction) %>% 
  na.omit()

tm_shape(grid_pred) +
  tm_squares(size= 0.07, 
             col="prediction",
             border.alpha = 0 )

