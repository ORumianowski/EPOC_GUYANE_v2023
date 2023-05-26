

source("scripts_formatage/001_utils_packages.R")


# Grille ------------------------------------------------------------------

grid =  st_read("grid/grid1000/grid1000.shp") %>%  #1000
  st_transform(., 2971)

# Occupation des sols brute -----------------------------------------------

onf =  st_read("data_enviro/sol_onf/oc_sol_2015.shp") %>% 
  dplyr::select(ID,
                NIVEAU3_15,
                geometry) %>% 
  rename(sol = NIVEAU3_15) %>% 
  mutate(sol = as.factor(sol)) %>% 
  mutate(sol = fct_collapse(sol,
                            Zone_urbaine = c("111", "112", "121", "122", "123", "124"),
                            Zone_interface = c("113", "114", "14", "222", "242", "243", "343", "131", "132", "133"),
                            Zone_vegetalise_artificiel = c("213", "231", "321", "322", "332", "211"), 
                            Foret = c("3151", "3152", "3153", "3154", "341", "342", "317", "3161", "3162", "319"),
                            Mangrove = c("318"),
                            Zone_humide = c("411", "412", "421", "512", "513", "331")
  ))%>% 
  st_transform(., 2971) 


# Point EPOC --------------------------------------------------------------

depoc = read_excel("data_epoc.xlsx", skip = 0, na = "NA")  %>% 
  dplyr::select(TRACE_BARY_X,
                TRACE_BARY_Y,
                ID_FORM)%>% 
  st_as_sf(.,
           coords = c("TRACE_BARY_X","TRACE_BARY_Y"),
           crs = 4326) %>% 
  st_transform(., 2971) %>% 
  unique()

# Proportion  -------------------------------------------------------------


CELL_SIZE = 200

depoc_buffer = st_buffer(depoc, dist = CELL_SIZE)
#plot(depoc_buffer)

grid_buffer = st_buffer(grid, dist = CELL_SIZE)
#plot(grid_buffer)


sol_names = onf$sol %>% unique()


# grid_prop ---------------------------------------------------------------


grid_prop = matrix(0,
       nrow = length(grid_buffer$FID),
       ncol = length(sol_names),
       dimnames = list(c(grid_buffer$FID), c(sol_names))) %>% 
  data.frame() %>% 
  tibble() %>% 
  mutate(FID = grid_buffer$FID) 


for (FID_j in grid_buffer$FID){
  
  disque_i = subset(grid_buffer, FID == FID_j)
  
  disque_sol = sf::st_intersection(disque_i, onf) %>% 
    dplyr::mutate(
      area       = sf::st_area(.),
      proportion = (area / (3.14*200*200)) %>% 
        units::drop_units()
    ) %>%
    tibble::as_tibble() %>%
    dplyr::select(
      sol,
      proportion,
    )
  
  for (hab_i in sol_names){
    
    if (hab_i %in% disque_sol$sol){
      
      sol_info_i = subset(disque_sol, sol == hab_i)
      grid_prop[grid_prop$FID == FID_j, hab_i] = sum(sol_info_i$proportion) %>% 
        round(., digits = 2)
      
    }
  }
}


# epoc_prop ---------------------------------------------------------------


epoc_prop = matrix(0,
                   nrow = length(depoc$ID_FORM),
                   ncol = length(sol_names),
                   dimnames = list(c(depoc$ID_FORM), c(sol_names))) %>% 
  data.frame() %>% 
  tibble() %>% 
  mutate(ID_FORM = depoc$ID_FORM) 

for (ID_FORM_j in depoc$ID_FORM){
  
  disque_i = subset(depoc_buffer, ID_FORM == ID_FORM_j)
  
  disque_sol = sf::st_intersection(disque_i, onf) %>% 
    dplyr::mutate(
      area       = sf::st_area(.),
      proportion = (area / (3.14*200*200)) %>% 
        units::drop_units()
    ) %>%
    tibble::as_tibble() %>%
    dplyr::select(
      sol,
      proportion,
    )
  
  for (hab_i in sol_names){
    
    if (hab_i %in% disque_sol$sol){
      
      sol_info_i = subset(disque_sol, sol == hab_i)
      epoc_prop[epoc_prop$ID_FORM == ID_FORM_j, hab_i] = sum(sol_info_i$proportion) %>% 
        round(., digits = 2)
      
    }
  }
}

library(writexl)


write_xlsx(grid_prop,'grid_prop_1000.xlsx')
#write_xlsx(epoc_prop,'epoc_prop.xlsx')


