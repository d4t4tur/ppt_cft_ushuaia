library(tidyverse)
library(herramientas)
library(sf)
library(leaflet)
library(geoAr)

puna_geom <- read_file_srv("/srv/DataDNMYE/capas_sig/puna_localidades_bahra.gpkg")

puna_geom <- puna_geom %>% 
  mutate(clasificacion = case_when(
    plazas <= 600 ~ "Emergente",
    plazas > 600 & plazas <= 2000 ~ "En desarrollo",
    plazas > 2000 ~ "Consolidado", 
  )) %>% 
  mutate(clasificacion =  factor(clasificacion, levels = c("Emergente",
                                                           "En desarrollo",
                                                           "Consolidado"))
  )

bahra <- read_file_srv("/srv/DataDNMYE/capas_sig/bahra_base_total.geojson") %>% 
  filter(str_detect(nombre, "Tolhuin")) %>% 
  select(nombre, geometry) #%>% 
  
tolhuin <- bahra %>% 
  mutate(long = unlist(map(bahra$geometry,1)),
         lat = unlist(map(bahra$geometry,2))) %>% 
  st_drop_geometry()

tolhuin <- puna_geom %>% 
  st_drop_geometry() %>% 
  filter(localidad == "tolhuin") %>% 
  cbind(tolhuin %>% select(-1)) %>% 
  st_drop_geometry()

puna <- puna_geom %>% 
  st_cast("POINT") %>% 
  mutate(long = unlist(map(geom,1)),
         lat = unlist(map(geom,2))) %>% 
  st_drop_geometry() %>% 
  rbind(tolhuin) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

#write_sf(puna, "salidas/puna_localidades_bahra2.geojson")

puna <- puna %>% 
  mutate(across(c(provincia, departamento_partido, localidad),  toupper))

labels <- paste0("Localidad: ", puna$localidad,
                 "<br>Departamento: ",  puna$departamento_partido,
                 "<br>Provincia: ",  puna$provincia,
                 "<br>Plazas: ",  puna$plazas,
                 "<br>Establecimientos: ",  puna$establecimientos,
                 "<br>Clasificación: ",  puna$clasificacion) %>%
    lapply(htmltools::HTML)

pal <- colorFactor(
  palette = c(comunicacion::dnmye_colores("purpura"),
              comunicacion::dnmye_colores("azul verde"),
              comunicacion::dnmye_colores("naranja")),
  domain = puna$clasificacion
)

leaflet() %>% 
  addArgTiles() %>% 
  addCircles(data = puna, color = ~ pal(clasificacion),
             label = labels, popup = labels) %>% 
  addLegend(pal = pal, values = puna$clasificacion, opacity = 1)

