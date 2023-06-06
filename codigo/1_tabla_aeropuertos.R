library(tidyverse)
library(arrow)
library(comunicacion)
library(sf)

#...............................................................................
#                                                                              .
#  Script de preprocesamiento de aeropuertos:                                  .
#                                                                              .
#    para cada aeropuerto toma el max mensual de vuelos y pax para 2019        .
#                                                                              .
#    se le agregan las coordenadas de los aeropuertos                          .
#                                                                              .
#...............................................................................



base_total <- herramientas::read_file_srv("aerocomercial/anac/base_anac_agrupada_diaria.parquet")


# calculo metricas x aeropuerto
base_total <- base_total %>%
  pivot_longer(cols = c(origen_oaci, destino_oaci),
               names_to = "variable",
               values_to = "oaci") %>%
  filter(oaci != "EGYP" & anio_local == 2019) %>%
  group_by(oaci, mes_local) %>%
  summarise(asientos = sum(asientos_pax, na.rm = T),
            sum_asientos_cabotaje = sum(asientos_pax[clasificacion_vuelo == "Cabotaje"]),
            sum_asientos_internacional = sum(asientos_pax[clasificacion_vuelo == "Internacional"]),
            frecuencias = sum(ceiling(vuelos), na.rm = T), 
            rutas_int = n_distinct(ruta_nombre[clasificacion_vuelo == "Internacional"]),
            rutas_cabotaje = n_distinct(ruta_nombre[clasificacion_vuelo == "Cabotaje"]))%>%
  group_by(oaci) %>% 
  summarise(max_asientos = max(asientos), # capacidad potencial
            max_frecuencias = max(frecuencias), # capacidad potencial
            media_asientos_cabotaje = mean(sum_asientos_cabotaje),
            media_asientos_internacional = mean(sum_asientos_internacional),
            rutas_int = median(rutas_int), # menos sensible a ruido de rutas circunstanciales
            rutas_cabotaje = median(rutas_cabotaje)) %>% # menos sensible a ruido de rutas circunstanciales
  ungroup()

base_total <- base_total %>%
  mutate(indice_cabotaje =  media_asientos_cabotaje*rutas_cabotaje,
         indice_int = media_asientos_internacional*rutas_int)


aeropuertos <- herramientas::read_file_srv("aerocomercial/libros_de_codigos/ANAC/aeropuertos_nuevos.xlsx",
                                           col_types = "text") %>%
  mutate(aeropuerto_etiqueta_anac = case_when(is.na(aeropuerto_etiqueta_anac) ~ nombre_ingles,
                                              T ~ aeropuerto_etiqueta_anac),
         localidad_etiqueta_indec = case_when(is.na(localidad_etiqueta_indec) ~ localidad_ingles,
                                              T ~ localidad_etiqueta_indec))

base_total <- base_total %>%
  left_join(aeropuertos, by = c("oaci" = "codigo_oaci"))

base_total <- base_total %>%
  filter(iso_country == "AR")

aeropuertos <- base_total %>%
  st_as_sf(coords = c('longitud', 'latitud'), crs = 4326)


aeros_regulares <- herramientas::read_file_srv("/srv/DataDNMYE/aerocomercial/tablero/mapa_conectividad.gpkg")

lista_aeros <- unique(c(aeros_regulares$aeropuerto_etiqueta_anac.y, 
                      aeros_regulares$aeropuerto_etiqueta_anac.x))

aeropuertos <- aeropuertos %>% 
  filter(aeropuerto_etiqueta_anac %in%  lista_aeros)


aeropuertos <- aeropuertos %>% 
  select(-c(10:22, 25:30))

aeropuertos <- aeropuertos %>% 
  filter(aeropuerto_etiqueta_anac != 'Aeropuerto El Palomar')

