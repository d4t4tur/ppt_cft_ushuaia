library(tidyverse)
library(herramientas)
library(sf)
library(arrow)
library(osrm)

puna <- read_file_srv("/srv/DataDNMYE/capas_sig/puna_localidades_bahra.gpkg")

puna <- puna %>%
  filter(! st_is_empty(geom))

imperdibles <- read_file_srv("/srv/DataDNMYE/capas_sig/imperdibles.gpkg") %>% 
  st_centroid()

puna_imperdibles <- st_join(puna, imperdibles, join = sf::st_nearest_feature) %>% 
  as.data.frame() %>% 
  left_join(imperdibles, by = c("nombre_ppal","ruta_natural")) 

mapview::mapview(list(puna_imperdibles$geom.x, puna_imperdibles$geom.y), col.regions = list("red","blue"))


ruteo <- function(x,y) {
  list(osrm::osrmTable(src = st_cast(x, "POINT"), 
                       dst = st_cast(y, "POINT"), 
                       measure = c('duration', 'distance'))
  )
}

j <- 31

for (i in j:nrow(puna_imperdibles)) {
  
  puna_imperdibles[i,] %>% 
    mutate(osrm = ruteo(geom.x, geom.y)) %>% 
    write_rds(.,file = glue::glue("entradas/ruteos/imperdibles/puna_imperdibles_osrm_{i}.rds"))
  
  
  print(i)
  Sys.sleep(runif(1, min = 2, max = 3))
  
  j <- i
}

archivos <- list.files("entradas/ruteos/imperdibles/", full.names = T)

puna_imperdibles_osrm <- map(archivos, function(x) {read_rds(x)}) %>% 
  list_rbind()

puna_imperdibles_osrm <- bind_rows(puna_imperdibles_osrm) %>% 
  as_tibble()

puna_imperdibles_osrm <- puna_imperdibles_osrm %>%
  unnest_wider('osrm')

puna_imperdibles <- puna_imperdibles_osrm %>% 
  mutate(distancia = map_int(distances, function(x) {unlist(x)[[1]]}),
         duracion = map_dbl(durations, function(x) {unlist(x)[[1]]})) %>% 
  select(-c(12,14:17))

write_rds(puna_imperdibles, "salidas/puna_imperdibles_osrm.rds")
