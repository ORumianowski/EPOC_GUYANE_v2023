

source("scripts_formatage/001_utils_packages.R")

  
library(emmeans)
library(MuMIn)
library(ggplot2)

# gestion des 0 ! 

# Données d'observation ---------------------------------------------------

depoc = read_excel("data_epoc.xlsx", skip = 0, na = "NA") %>% 
  st_as_sf(.,
           coords = c("TRACE_BARY_X","TRACE_BARY_Y"),
           crs = 2971) %>% 
  dplyr::select(geometry,
                ID_FORM, 
                ID_OBSERVER,
                DATE, TIME_START, 
                ID_SPECIES,
                DURATION,
                LATIN_SPECIES)

liste_espece = read_excel("liste_espece.xlsx", skip = 0, na = "NA")
depoc = merge(depoc, liste_espece, by = "ID_SPECIES")

depoc_sp = subset(depoc, NAME_SPECIES == "Merle à lunettes" ) 


ID_FORM_sp =  tibble(ID_FORM = unique(depoc$ID_FORM))
ID_FORM_all =  tibble(ID_FORM = unique(depoc_sp$ID_FORM))
ID_FORM_non_sp = subset(a, !(ID_FORM %in% b$ID_FORM))


dt_sp = tibble(ID_FORM = unique(depoc_sp$ID_FORM)) %>% 
  mutate(COUNT = table(depoc_sp$ID_FORM))

depoc_sp2 = depoc_sp %>% 
  dplyr::select(ID_FORM,
                ID_OBSERVER,
                TIME_START,
                DATE,
                DURATION) %>% 
  unique()

dt1 = merge(dt_sp, depoc_sp2, by="ID_FORM", all = TRUE)

dt_non_sp = tibble(ID_FORM = ID_FORM_non_sp$ID_FORM) %>% 
  mutate(COUNT = rep(0, nrow(.)))

depoc_non_sp2 = depoc %>% 
  dplyr::select(ID_FORM,
                ID_OBSERVER,
                TIME_START,
                DATE,
                DURATION) %>% 
  unique()

dt2 = merge(dt_non_sp,depoc_non_sp2 , by="ID_FORM")



dt = rbind(dt1, dt2)

dpheno = dt %>% 
  mutate(ID_OBSERVER = ID_OBSERVER %>% 
                  as.factor(),
                TIME_START = TIME_START %>% 
                  as.numeric(),
                MONTH = DATE %>% 
                  month() %>% 
                  as.factor())


# creation des variables --------------------------------------------------


dpheno$TIME_CAT = dpheno$TIME_START%>% 
  as.vector() %>% 
  cut(.,
      breaks=c(0, 5*60,8*60, 14*60, 17*60, 19*60, 24*60),
      labels=c('Night','Dawn', 'Morning', 'Afternoon', 'Twilight','Night'))

dpheno$TIME_CAT2 = dpheno$TIME_START%>% 
  as.vector() %>% 
  cut(.,
      breaks=c(0, 7*60, 14*60,  18*60, 24*60),
      labels=c('Night', 'Morning', 'Afternoon','Night'))


dpheno$SAISON = dpheno$DATE%>% 
  month() %>% 
  as.vector() %>% 
  cut(.,
      breaks=c(0, 3, 6, 9, 12),
      labels=c('Winter', 'Spring', 'Summer','Fall'))

dpheno$SAISON2 = dpheno$DATE%>% 
  month() %>% 
  as.vector() %>% 
  cut(.,
      breaks=c(0, 2, 4, 6, 8, 10, 12),
      labels=c('1_2', '3_4', '5_6','7_8', '9_10', '11_12'))


# dredge ------------------------------------------------------------------


options(na.action = "na.fail")

mod = glm(COUNT ~ 
            SAISON2 + SAISON + MONTH + 
            TIME_CAT + TIME_CAT2 +
            DURATION + 
            ID_OBSERVER, 
          data = dpheno, family = poisson)
dred = dredge(mod)
dred

# modele final ------------------------------------------------------------


dpheno = subset(dpheno, TIME_CAT != "Night")

mod = glm(COUNT ~ SAISON2 + 
             TIME_CAT +
             DURATION ,
           data = dpheno, family = poisson)

summary(mod)


emme = emmeans(mod, specs = pairwise ~ TIME_CAT)
emme
emmip(mod, ~ TIME_CAT, CIs = TRUE, type = "response") 


emme = emmeans(mod, specs = pairwise ~ SAISON2)
emme
emmip(mod, ~ SAISON2, CIs = TRUE, type = "response") 


