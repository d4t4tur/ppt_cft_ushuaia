library(tidyverse)
library(herramientas)
library(sf)
library(arrow)
# install.packages('osrm')
library(osrm)


#...............................................................................
#                                                                              .
#  Asigno top 3 aeropuertos mas cercanos a cada localidad georeferenciada de   .
#  puna                                                                        .
#                                                                              .
#  Se usan distancias lineales y dps se toman distancias y tiempo de viaje     .
#  con osrm                                                                    .
#                                                                              .
#...............................................................................



puna <- herramientas::read_file_srv("/srv/DataDNMYE/capas_sig/puna_localidades_bahra.gpkg")

puna <- puna %>%
  filter(! st_is_empty(geom))

# aeropuertos <- herramientas::read_file_srv("/srv/DataDNMYE/capas_sig/aeropuertos_anac_total.gpkg")

source("codigo/1_tabla_aeropuertos.R")

aeropuertos <- aeropuertos %>%
  mutate(indice = 1:n())

mdistancias <- st_distance(puna, aeropuertos)


ranking <- function(x, p) {
  sort(as.numeric(x), method = "shell", index.return = T)$ix[1:p]

}

top <- apply(mdistancias, MARGIN = 1,FUN = function(x) ranking(x, p = 3))
top <- as_tibble(t(top))
colnames(top) <- c(paste0('aeropuerto', 1:ncol(top)))

puna_aeropuertos <- bind_cols(puna, top)

puna_aeropuertos <- puna_aeropuertos %>%
  pivot_longer(cols = colnames(top), names_to = "orden_aeropuerto", values_to = "aeropuerto")

puna_aeropuertos <-  left_join(puna_aeropuertos, as_tibble(aeropuertos),
                   by = c("aeropuerto" = "indice"))


ruteo <- function(x,y) {
  list(osrm::osrmTable(src = st_cast(x, "POINT"), 
                       dst = st_cast(y, "POINT"), 
                       measure = c('duration', 'distance'))
  )
}

j <- 1

for (i in j:nrow(puna_aeropuertos)) {

  puna_aeropuertos[i,] %>% 
    mutate(osrm = ruteo(geom, geometry)) %>% 
    write_rds(.,file = glue::glue("entradas/ruteos/puna_aeropuertos_osrm_{i}.rds"))
  
  
  print(i)
  Sys.sleep(runif(1, min = 2, max = 3))
  
  j <- i
}


archivos <- list.files("entradas/ruteos/", full.names = T)

puna_aeropuertos_osrm <- map(archivos, function(x) {read_rds(x)}) %>% 
  list_rbind()

puna_aeropuertos_osrm <- bind_rows(puna_aeropuertos_osrm) %>% 
  as_tibble()

# temp <- bind_rows(puna_aeropuertos_osrm, temp)

write_rds(puna_aeropuertos_osrm, "salidas/puna_aeropuertos_osrm.rds")
