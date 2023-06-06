library(tidyverse)
library(herramientas)
library(sf)
library(arrow)
library(osrm)

puna <- read_file_srv("/srv/DataDNMYE/capas_sig/puna_localidades_bahra.gpkg")

puna <- puna %>%
  filter(! st_is_empty(geom))

## Terminales de ómnibus
#Capa IGN
terminales <- read_sf("entradas/terminales_omnibus.json") %>% 
  select(gid, gna, nam)


puna_terminales <- st_join(puna, terminales, join = sf::st_nearest_feature) %>% 
  as.data.frame() %>% 
  left_join(terminales) 

ruteo <- function(x,y) {
  list(osrm::osrmTable(src = st_cast(x, "POINT"), 
                       dst = st_cast(y, "POINT"), 
                       measure = c('duration', 'distance'))
  )
}

j <- 1

for (i in j:nrow(puna_terminales)) {
  
  puna_terminales[i,] %>% 
    mutate(osrm = ruteo(geom, geometry)) %>% 
    write_rds(.,file = glue::glue("entradas/ruteos/terminales/puna_terminales_osrm_{i}.rds"))
  
  
  print(i)
  Sys.sleep(runif(1, min = 2, max = 3))
  
  j <- i
}

archivos <- list.files("entradas/ruteos/terminales/", full.names = T)

puna_terminales_osrm <- map(archivos, function(x) {read_rds(x)}) %>% 
  list_rbind()

puna_terminales_osrm <- bind_rows(puna_terminales_osrm) %>% 
  as_tibble()

puna_terminales_osrm <- puna_terminales_osrm %>%
  unnest_wider('osrm')

puna_terminales <- puna_terminales_osrm %>% 
  mutate(distancia = map_int(distances, function(x) {unlist(x)[[1]]}),
         duracion = map_dbl(durations, function(x) {unlist(x)[[1]]})) %>% 
  select(-c(13:16))

write_rds(puna_terminales, "salidas/puna_terminales_osrm.rds")
