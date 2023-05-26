
source("001_utils_packages.R")

source("004_filtrage.R")

#http://examples.distancesampling.org/Distance-points/pointtransects-distill.html

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
          formula=~ ID_OBSERVER + SAISON + TIME_CAT)  




t = res %>% 
  summary()

gof_ds(res)




