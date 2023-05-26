

source("scripts_formatage/001_utils_packages.R")

# Bioclimatique -----------------------------------------------------------

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

dspecies = subset(depoc_full, ID_SPECIES == 		
                    14345) 



dspecies_DS = dspecies %>% 
  dplyr::select(Sample.Label, DISTANCE, ID_OBSERVER, DATE, TIME_START) %>% 
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
      breaks=c(0, 3, 5, 7, 12),
      labels=c('Rain', 'Dry', 'Rain','Dry'))




plot = ggplot(dspecies_DS, aes(x=distance, color = SAISON)) + geom_histogram()



res <- ds(dspecies_DS, 
          truncation = list(left= 0 , right = 150),
          transect = "point",
          formula=~ SAISON + TIME_CAT ) #+ ID_OBSERVER )  




res_summary = res %>% 
  summary()

gof_ds(res)

plot(res,
     pdf=TRUE)

# Density surface modelling -----------------------------------------------


library("dsm")


df_ht = res


obsdata = dspecies_DS %>% 
  rename(DISTANCE = distance) %>% 
  dplyr::select(Sample.Label,
                DISTANCE,
                ID_OBSERVER,
                SAISON,
                TIME_CAT)%>% 
  mutate(size = rep(1, nrow(dspecies)),
         object = 1:nrow(dspecies)) %>% 
  rename(distance = DISTANCE) 


segdata = dspecies %>% 
  mutate(X = st_coordinates(.)[, "X"],
         Y = st_coordinates(.)[, "Y"])%>% 
  st_drop_geometry() %>% 
  mutate(Effort = rep(1, nrow(.))) %>% 
  dplyr::select(Sample.Label,
                Effort, X, Y,
                Zone_urbaine, Zone_interface, Zone_vegetalise_artificiel,
                Foret,  Mangrove, Zone_humide,
                T_mean, T_amplitude_mean) %>% 
  unique()
  
segdata = left_join(segdata, unique(obsdata[,c("Sample.Label","ID_OBSERVER")]), 
                    by = "Sample.Label")

segdata = left_join(segdata, unique(obsdata[,c("Sample.Label","SAISON")]), 
                    by = "Sample.Label")

segdata = left_join(segdata, unique(obsdata[,c("Sample.Label","TIME_CAT")]), 
                    by = "Sample.Label")


n_distinct(segdata$Sample.Label)


#'Sample.Labels are non-unique in segment data!

mod_tw <- dsm(count ~ 
                Zone_urbaine + Zone_interface + Zone_vegetalise_artificiel 
                + Foret +  Mangrove + Zone_humide
                + s(X,Y), 
              ddf.obj=df_ht, 
              segment.data=segdata, 
              observation.data=obsdata, 
              family=tw(), 
              transect="point")


summary(mod_tw)


# Prediction --------------------------------------------------------------

CELL_SIZE = 1000 * 1000 # aire des cellules

grid_full_drop  = grid_full_drop %>% 
  mutate(X = x, 
         Y = y)

pred = predict(mod_tw, grid_full_drop,  
                off.set = CELL_SIZE, type = "response")


res_pred = pred %>% summary()

min = res_pred[1] %>% 
  round(., digits = -1)
max = res_pred[6]%>% 
  round(., digits = -1)
amplitude = max - min

nb_cat = 8


grid_pred = grid_full %>% 
  dplyr::select() %>% 
  mutate(prediction = as.numeric(pred)) %>% 
  dplyr::select(prediction) %>% 
  na.omit()

tm_shape(grid_pred) +
  tm_squares(size= 0.07, 
             col="prediction",
             border.alpha = 0,
             breaks = c(min + 0:nb_cat * amplitude/nb_cat ))+
  tm_layout(legend.position = c("left", "bottom"), 
            title= "Merle à lunettes (Turdus nudigenis)",
            title.position = c('right', 'bottom'),
            title.size = 1,
            panel.labels = c("Prédiction des densités (ind./km2)"))


             
            


# estimate ---------------------------------------------------------------




grid_full_drop_na_omit = grid_full_drop %>% 
  na.omit()

grid_full_drop_na_omit$off.set = rep(1000000, nrow(grid_full_drop_na_omit))

mod_tw_var <- dsm_varprop(mod_tw, newdata = grid_full_drop_na_omit)#, off.set=  CELL_SIZE)
summary(mod_tw_var)



# estimate density - habitat ----------------------------------------------

grid_full_drop$pred = pred

summary(grid_full_drop$pred, na.rm = TRUE)
print("Par habitat")

prop_ref = 0.8

stat_Zone_urbaine = grid_full_drop$pred[grid_full_drop$Zone_urbaine > prop_ref]
stat_Zone_interface = grid_full_drop$pred[grid_full_drop$Zone_interface > prop_ref]
stat_Zone_vegetalise_artificiel = grid_full_drop$pred[grid_full_drop$Zone_vegetalise_artificiel > prop_ref]
stat_Zone_humide = grid_full_drop$pred[grid_full_drop$Zone_humide > prop_ref]
stat_Mangrove= grid_full_drop$pred[grid_full_drop$Mangrove > prop_ref]
stat_Foret = grid_full_drop$pred[grid_full_drop$Foret > prop_ref]

density_per_hab = tibble(Habitat = c("Zone urbaine", "Zone d'interface", 
                                     "Zone végétalisée artificielle", "Zone humide",
                                     "Mangrove", "Forêt"),
                         Density = c(stat_Zone_urbaine %>% mean(., na.rm = TRUE),
                                     stat_Zone_interface %>% mean(., na.rm = TRUE), 
                                     stat_Zone_vegetalise_artificiel %>% mean(., na.rm = TRUE), 
                                     stat_Zone_humide %>% mean(., na.rm = TRUE), 
                                     stat_Mangrove %>% mean(., na.rm = TRUE),
                                     stat_Foret %>% mean(., na.rm = TRUE)),
                         N = c(stat_Zone_urbaine %>% length(.),
                               stat_Zone_interface %>% length(.),
                               stat_Zone_vegetalise_artificiel %>% length(.),
                               stat_Zone_humide %>% length(.),
                               stat_Mangrove %>% length(.),
                               stat_Foret %>% length(.)))


density_per_hab 



