library(herramientas)
library(tidyverse)
library(sf)
library(leaflet)
library(geoAr)

padron_afip <- read_file_srv('/srv/DataDNMYE/padron_afip/ubicacion_claes_turismo_empleo_dic19.csv')

puna <- read_file_srv("/srv/DataDNMYE/capas_sig/puna_localidades_bahra.gpkg")


# bahra <- geoAr::get_bahra()


padron_afip <- padron_afip %>% 
  filter(! (is.na(departamento_arcgis) & is.na(provincia) )) %>% 
  mutate(across(c(provincia, departamento_arcgis, localidad_arcgis), 
                .fns = \(x) str_squish(textclean::replace_non_ascii(tolower(x))))) %>% 
  mutate(provincia = case_when(provincia == 'caba' ~  'ciudad de buenos aires', 
                               T ~ provincia),
         departamento_arcgis = case_when(provincia == 'ciudad de buenos aires' ~  'caba', 
                                         T ~ departamento_arcgis),
         localidad_arcgis = case_when(provincia == 'ciudad de buenos aires' ~  'ciudad de buenos aires', 
                                      T ~ localidad_arcgis))
padron_afip <- padron_afip %>% 
  mutate(cat_rct = case_when(substr(clae6,1,3) %in% c(473,491,492,501,502,511,524,771)~ "Transporte",
                           substr(clae6,1,3) %in% c(551,552)~ "Alojamiento",
                           substr(clae6,1,3) %in% c(561,562) ~ "Gastronomía",
                           substr(clae6,1,3) == 791 ~ "Agencias de Viaje",
                           substr(clae6,1,3) %in% c(591,592,681,780,854,900,910,920,931,939)~ "Otros Servicios Turísticos"))

padron_afip <- padron_afip %>% 
  filter(empleadora_dic19 == 1) %>% 
  group_by(localidad_arcgis, departamento_arcgis, provincia, cat_rct) %>% 
  summarise(prestadores = n()) 

padron_afip <- padron_afip %>% 
  pivot_wider(names_from = cat_rct, values_from = prestadores)

# bahra <-  bahra %>% 
#   mutate(across(c(nom_pcia, nom_depto, nombre), 
#                 .fns = \(x) str_squish(textclean::replace_non_ascii(tolower(x))))) %>% 
#   mutate(nom_depto = case_when(codprov_censo == '02' & coddepto_censo == '012' ~ 'comuna 12', 
#                                T ~ nom_depto))

puna_padron_afip <- left_join(puna, padron_afip,
                               by = c("provincia" = "provincia",
                                      "departamento_partido" = "departamento_arcgis",
                                      "localidad" = "localidad_arcgis"))


# padron_afip_bahra %>%
#   distinct(pick(1:6), .keep_all = T) %>%
#   view
# 
# padron_afip_bahra %>%
#   distinct(pick(1:6), .keep_all = T) %>%
#   filter(is.na(codprov_censo) & localidad_arcgis !=  'no_pertenece') %>%
#   group_by(provincia, departamento_arcgis, localidad_arcgis) %>%
#   summarise(n = n()) %>% view
# 
# padron_afip_bahra %>%
#   ungroup() %>%
#   distinct(pick(1:6), .keep_all = T) %>%
#   filter(is.na(codprov_censo) & localidad_arcgis !=  'no_pertenece') %>%
#   summarise(sum(prestadores))



test <- padron_afip_aeropuertos %>% 
  filter(if_all(.cols = 27:31 , .fns = is.na))

write_rds(puna_padron_afip, 'salidas/puna_prestadores.rds')

puna_padron_afip <- read_rds('salidas/puna_prestadores.rds') 


puna_padron_afip %>% 
  st_cast(to = 'POINT') %>% 
  filter(!st_is_empty(geom)) %>% 
  filter(!is.na(Transporte) & !is.na(Gastronomía)) %>% 
  mutate(across(.cols = c(Transporte,Gastronomía, Alojamiento, `Otros Servicios Turísticos`), function(x) {replace_na(as.character(x), "sd.")})) %>% 
  mutate(etiq = glue::glue('{str_to_title(localidad)}<br>Gastronomía = {Gastronomía}<br>Transporte:{Transporte}')) %>% 
  leaflet() %>%
  addArgTiles() %>% 
  addCircleMarkers(label = ~ lapply(etiq, htmltools::HTML))
