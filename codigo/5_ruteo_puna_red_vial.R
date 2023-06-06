library(tidyverse)
library(herramientas)
library(sf)
library(arrow)
library(osrm)

puna <- read_file_srv("/srv/DataDNMYE/capas_sig/puna_localidades_bahra.gpkg")

puna <- puna %>%
  filter(! st_is_empty(geom))

ruteo <- function(x,y) {
  list(osrm::osrmTable(src = st_cast(x, "POINT"), 
                       dst = st_cast(y, "POINT"), 
                       measure = c('duration', 'distance'))
  )
}


#Distancia/tiempo a punto más cercano ruta pavimentada

red_vial <- read_sf("entradas/red_vial_consolidado.geojson") %>% 
  filter(rst == "Pavimentado")

reach.nearest <- st_nearest_feature(puna, red_vial)

reach.nearest <- red_vial[reach.nearest,]

# Locate nearest point along nearest reach (Returns line)
dist <- c()

for (i in 1:nrow(puna)) {
  
  dist <- append(dist, st_nearest_points(puna[i,], reach.nearest[i,]))
  
}


# st_nearest_points returns lines, so use st_cast
point <- c()

for (i in dist) {
  
  point <- append(point,  st_cast(dist[i], "POINT")[2])
  
}

nearest_point <- data.frame(nearest_point = point[-1])

puna_rutas <- cbind(puna, nearest_point)

mapview::mapview(list(puna_rutas$geom, puna_rutas$geometry), col.regions = list("red","blue"))


j <- 1

for (i in j:nrow(puna_rutas)) {
  
  puna_rutas[i,] %>% 
    mutate(osrm = ruteo(geom, geometry)) %>% 
    write_rds(.,file = glue::glue("entradas/ruteos/red_vial/puna_red_vial_osrm_{i}.rds"))
  
  print(i)
  Sys.sleep(runif(1, min = 1, max = 2))
  
  j <- i
}


archivos <- list.files("entradas/ruteos/red_vial/", full.names = T)

puna_red_vial_osrm <- map(archivos, function(x) {read_rds(x)}) %>% 
  list_rbind()

puna_red_vial_osrm <- bind_rows(puna_red_vial_osrm) %>% 
  as_tibble()

puna_red_vial_osrm <- puna_red_vial_osrm %>%
  unnest_wider('osrm')

rutas_puna <- cbind(puna %>% st_drop_geometry(), reach.nearest %>% st_drop_geometry())

puna_red_vial <- puna_red_vial_osrm %>% 
  left_join(rutas_puna) %>% 
  mutate(distancia = map_int(distances, function(x) {unlist(x)[[1]]}),
         duracion = map_dbl(durations, function(x) {unlist(x)[[1]]})) %>% 
  select(-c(10:13))

write_rds(puna_red_vial, "salidas/puna_red_vial_osrm.rds")



## Red vial consolidado
# vias_nac <- read_sf("entradas/vial_nacional.json") %>%
#   mutate(typ = case_when(typ == 40 ~ "Ruta",
#                          typ == 41 ~ "Autopista",
#                          typ == 47 ~ "Autovía",
#                          TRUE  ~ "Sin dato"),
#          rst = case_when(rst == 1 ~ "Pavimentado",
#                          rst == 2 ~ "Consolidado",
#                          rst == 3 ~ "Tierra",
#                          TRUE ~ "Sin dato")) %>% 
#   mutate(gna = "RN")
# 
# vias_prov <- read_sf("entradas/vial_provincial.json") %>%
#   mutate(typ = case_when(typ == 40 ~ "Ruta",
#                          typ == 41 ~ "Autopista",
#                          typ == 47 ~ "Autovía",
#                          typ == 49 ~ "Camino",
#                          TRUE  ~ "Sin dato"),
#          rst = case_when(rst == 1 ~ "Pavimentado",
#                          rst == 2 ~ "Consolidado",
#                          rst == 3 ~ "Tierra",
#                          TRUE ~ "Sin dato")) %>% 
#   mutate(gna = "RP")
# 
# #Agrupo tramos
# group_vias_nac <- vias_nac %>% 
#   group_by(rtn, typ, rst, gna) %>% 
#   summarise(geometry = st_union(geometry)) %>% 
#   ungroup()
# 
# group_vias_prov <- vias_prov %>% 
#   group_by(rtn, typ, rst, gna) %>% 
#   summarise(geometry = st_union(geometry)) %>% 
#   ungroup()
# 
# 
# red_vial <- bind_rows(group_vias_nac, group_vias_prov)
# 
# red_vial <- st_set_crs(red_vial, value = 4326)
# 
# mapview::mapview(list(puna, red_vial))
# 
# st_write(red_vial, "entradas/red_vial_consolidado.geojson")