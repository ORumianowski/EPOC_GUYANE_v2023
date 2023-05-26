

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


segs <- spermwhales$segs


predgrid <- spermwhales$predgrid



# -------------------------------------------------------------------------


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
  mutate(sol = as.factor(sol))%>% 
  st_transform(., 2971)


plot = tm_shape(onf) +
  tm_fill(col="sol") +
  tm_borders() 

# Bioclimatique -----------------------------------------------------------



denviro_epoc = read_excel("denviro_epoc.xlsx", skip = 0, na = "NA") %>% 
  rename( Sample.Label = ID_FORM)



# Zone d'étude ------------------------------------------------------------


survey.area = onf$geometry %>%
  st_combine() 


library("raster")
library("rgeos")
library("dismo")

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
  st_intersection(., onf)%>% 
  st_transform(., 2971)  %>% 
  st_as_sf()



predgrid0 = st_join(grid, onf["sol"]) 



# -------------------------------------------------------------------------


# Données d'observation ---------------------------------------------------



source("scripts_formatage/004_filtrage.R")

segs = depoc %>% 
  dplyr::select(ID_FORM) %>% 
  rename(Sample.Label = ID_FORM) %>% 
  mutate(Effort = rep(1, nrow(.))) %>% 
  st_transform(., 2971) %>% 
  st_join(., onf["sol"]) %>% 
  mutate(x = st_coordinates(.)[, "X"],
         y = st_coordinates(.)[, "Y"]) %>% 
  st_drop_geometry() %>% 
  merge(., denviro_epoc)%>% 
  unique()

segs = segs %>% 
  dplyr::select(c(x, y, elevation, temperature_mean, precipitation_mean, Temperature_amplitude_mean))
#il manque les coordonnées

predgrid = predgrid0 %>% 
  mutate(x = st_coordinates(.)[, "X"],
         y = st_coordinates(.)[, "Y"]) %>% 
  st_drop_geometry() %>% 
  merge(., denviro_epoc)%>% 
  unique() %>% 
  dplyr::select(c(x, y, elevation, temperature_mean, precipitation_mean, Temperature_amplitude_mean))
  

segs %>% head()
predgrid %>% head()

covariates.spermwhale =c("elevation", "temperature_mean", "precipitation_mean", "Temperature_amplitude_mean")


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
