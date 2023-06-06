library(herramientas)
library(tidyverse)
library(sf)
library(leaflet)
library(geoAr)

source('codigo/1_tabla_aeropuertos.R')

puna_aeropuertos <- read_rds('salidas/puna_aeropuertos_osrm.rds')

puna_aeropuertos <- puna_aeropuertos %>%
  unnest_wider('osrm')

puna_aeropuertos <- puna_aeropuertos %>%
  select(-c(12:35))

puna_aeropuertos <- left_join(puna_aeropuertos, aeropuertos)

puna_aeropuertos <- puna_aeropuertos %>% 
  mutate(distancia = map_int(distances, function(x) {unlist(x)[[1]]}),
         duracion = map_dbl(durations, function(x) {unlist(x)[[1]]})) %>% 
  select(-c(distances, durations))


write_rds(puna_aeropuertos, 'salidas/puna_aeropuertos.rds')

# armado de variables proxy |> pasar df a 1 fila x localidad puna
puna_aeropuertos <- puna_aeropuertos %>%
  # excluyo palomar
  filter(!str_detect(puna_aeropuertos$aeropuerto_etiqueta_anac, 'Palomar')) %>%
  select(-c(9:13))

puna_aeropuertos <- puna_aeropuertos %>%
  group_by(provincia.x, departamento_partido, localidad) %>%
  mutate(aero_mas_cerca = aeropuerto_etiqueta_anac[which.min(duracion)],
         vuelos_aero_cerca = max_frecuencias[which.min(duracion)],
         max_asientos_cerca = max_asientos[which.min(duracion)],
         tiempo_aero_cerca = duracion[which.min(duracion)],
         indice_cabotaje_mas_cerca = indice_cabotaje[which.min(duracion)],
         indice_int_mas_cerca = indice_int[which.min(duracion)],
         aero_ppal = aeropuerto_etiqueta_anac[which.max(max_asientos)],
         vuelos_aero_ppal = max_frecuencias[which.max(max_asientos)],
         pax_aero_ppal = max_asientos[which.max(max_asientos)],
         tiempo_aero_ppal = duracion[which.max(max_asientos)],
         indice_cabotaje_mas_cerca = indice_cabotaje[which.max(max_asientos)],
         indice_int_mas_cerca = indice_int[which.max(max_asientos)]
         )
# 
puna_aeropuertos <- puna_aeropuertos %>%
  distinct(pick(-c(8:21)))

write_rds(puna_aeropuertos, 'salidas/puna_aeropuertos.rds')


puna_aeropuertos <- read_rds('salidas/puna_aeropuertos.rds')




puna_aeropuertos <- puna_aeropuertos %>% 
  mutate(etiq = glue::glue(
    "{str_to_title(localidad)}
    <table>
  <tr>
    <th>Aeropuerto cercano</th>
    <th>{aero_mas_cerca}</th>
  </tr>
  <tr>
    <th>Tiempo</th>
    <th>{lbl_int(tiempo_aero_cerca)}min.</th>
  </tr>
  <tr>
    <th>Indice Cabotaje</th>
    <th>{lbl_int(indice_cabotaje_mas_cerca)}</th>
  </tr>
  <tr>
    <th>Indice Internacional</th>
    <th>{lbl_int(indice_int_mas_cerca)}</th>
  </tr>
</table>"
  ))

puna <- read_file_srv("/srv/DataDNMYE/capas_sig/puna_localidades_bahra.gpkg")


puna_aeropuertos <- left_join(puna, puna_aeropuertos)

puna_aeropuertos %>% 
  filter(!st_is_empty(geom)) %>% 
  mutate(geom = st_cast(geom, "POINT")) %>% 
  leaflet() %>% 
  addArgTiles() %>% 
  addCircleMarkers(color = comunicacion::dnmye_colores("rosa"), label = ~ lapply(etiq, htmltools::HTML) , popup = ~ lapply(etiq, htmltools::HTML))

# 