

#'---------------------------------------------
# Other required libraries
#'---------------------------------------------
library(dsmextra)     # Extrapolation toolkit for ecological models
library(raster)       # Geographic data analysis and modeling
library(tidyverse)    # Packages for data science
library(magrittr)     # Pipe operator

#'--------------------------------------------------------------------
# Set tibble options
#'--------------------------------------------------------------------
options(tibble.width = Inf) # All tibble columns shown
options(pillar.neg = FALSE) # No colouring negative numbers
options(pillar.subtle = TRUE)
options(pillar.sigfig = 4)

#'--------------------------------------------------------------------
# Set knitr options
#'--------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)




#'---------------------------------------------
# Load and extract the data
#'---------------------------------------------


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


# -------------------------------------------------------------------------


segs = depoc_full %>% 
  mutate(x = st_coordinates(.)[, "X"],
         y = st_coordinates(.)[, "Y"]) %>% 
  st_drop_geometry() %>% 
  dplyr::select(x,y,
               Zone_urbaine, Zone_interface, Zone_vegetalise_artificiel,
               Foret,  Mangrove, Zone_humide, T_mean, T_amplitude_mean)

predgrid = grid_full_drop %>% 
  dplyr::select(x,y,
                Zone_urbaine, Zone_interface, Zone_vegetalise_artificiel,
                Foret,  Mangrove, Zone_humide, T_mean, T_amplitude_mean)
 

segs %>% head()
predgrid %>% head()

covariates.spermwhale = c("Zone_urbaine", "Zone_interface", "Zone_vegetalise_artificiel",
                          "Foret",  "Mangrove", "Zone_humide", "T_mean", "T_amplitude_mean")

CRS_Guyane = CRS("+init=EPSG:2971")

spermwhale.extrapolation <- compute_extrapolation(samples = segs,
                                                  covariate.names = covariates.spermwhale,
                                                  prediction.grid = predgrid,
                                                  coordinate.system = CRS_Guyane)


summary(spermwhale.extrapolation)

# maps --------------------------------------------------------------------



#'---------------------------------------------
# Rename coordinates and convert to SpatialPointsdf
#'---------------------------------------------
obs.sp <- segs %>%
  sp::SpatialPointsDataFrame(coords = cbind(.$x, .$y), data = ., 
                             proj4string = CRS_Guyane) %>%
  sp::spTransform(., CRSobj = CRS_Guyane)

# 

library(conflicted)

conflict_prefer("Which", "base")
conflict_prefer("which.min", "base")

map_extrapolation(map.type = "extrapolation",
                  extrapolation.object = spermwhale.extrapolation,
                  sightings = obs.sp)


# Bilan -------------------------------------------------------------------

#1 : Covariate d'intéret centrale est la nature du sol (non-utilisé dans dsm extra?)
