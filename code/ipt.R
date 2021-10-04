#functions
A4_stops <- function(A4_nm){
  #libraries
  library(readxl)
  library(dplyr)
  library(stringr)
  #read & munging
  paradas <- read_xlsx(path = paste0("data/", A4_nm), 
                       sheet = 1, 
                       col_types = c("numeric", 
                                     "skip", 
                                     "text", 
                                     "text", 
                                     "text", 
                                     "numeric", 
                                     "text", 
                                     "text", 
                                     "skip", 
                                     "skip", 
                                     "skip", 
                                     "skip", 
                                     "skip", 
                                     "skip", 
                                     "text", 
                                     "skip", 
                                     "skip")
                       ) %>% 
    mutate(ROUTE_NAME = if_else(condition = is.na(`Varian-te`), 
                                true = paste0(`Código Usuario`, 
                                              str_sub(`Sentido Servicio`, 1, 1)),
                                false = paste0(`Código Usuario`, 
                                               str_sub(`Sentido Servicio`, 1, 1),
                                               `Varian-te`), 
                                missing = NA_character_) 
    ) %>% 
    select(-`Código Usuario`, -`Sentido Servicio`, -`Varian-te`)
  #output
  return(paradas)
}

routes <- function(rt_shp){
  library(dplyr)
  library(sf)
  rts <- st_read(dsn = paste0("data/", rt_shp, ".shp"), 
                 layer = rt_shp) %>%
    mutate_if(is.character, 
              function(x){iconv(x, to = "UTF-8")}
              )
  return(rts)
}

stops <- function(stp_shp){
  #libraries
  library(sf)
  library(dplyr)
  library(tibble)
  library(readr)
  #data
  #subway&train stops
  mt_stp <- read_delim(file = "data/dicc_mt.csv", 
                       delim = ";", 
                       col_types = list(col_character(), 
                                        col_double(), 
                                        col_double(), 
                                        col_character(), 
                                        col_character(), 
                                        col_character()
                                        )
                       ) %>%
    st_as_sf(coords = c("X", "Y"), 
             crs = 32719)
  #bus stops
  bus_stp <- st_read(dsn = paste0("data/", stp_shp, ".shp"), layer = stp_shp) %>%
    select(CODINFRA, SIMT, `Nombre Paradero` = NOMBRE_PAR) %>%
    add_column(Modo = "BUS")
  stp <- rbind(bus_stp, mt_stp)
  return(stp)
}

od <- function(DDBB_v, DDBB_e){
  library(DBI)
  library(dtplyr)
  library(dplyr)
  library(tidyr)
  #connection
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver   = "SQL Server",
                        Server   = "10.222.128.21,9433",
                        Database = "uchile",
                        UID      = rstudioapi::askForPassword("Database user"),
                        PWD      = rstudioapi::askForPassword("Database password"))
  #sql
  ##od4coverage
  sql_od4cov <- paste0("SELECT SUM(CAST([factorexpansion] AS FLOAT)) AS DDA,
                               [paraderosubida], 
                               [paraderobajada] 
                        FROM [uchile].[dbo].", DDBB_v, 
                        " WHERE tipodia = 'LABORAL' AND 
                          periodomediodeviaje = '04 - PUNTA MANANA' AND
                          [paraderosubida] <> '-' AND 
                          [paraderobajada] <> '-'
                        GROUP BY [paraderosubida], 
                                 [paraderobajada];"
                       )
  od4cov <- lazy_dt(DBI::dbGetQuery(conn = con, 
                                    statement = sql_od4cov)
                    )
  #MH2period
  sql_per <- paste0("SELECT [HR_MH] AS [mh_subida],
                            [PER_DTPM2],
                            [TIPO_DIA]
                     FROM [uchile].[dbo].[peri_mh]"
                    ) 
  per <- lazy_dt(DBI::dbGetQuery(conn = con, 
                                 statement = sql_per)
                 ) %>%
    as_tibble() %>%
    unite("dia_mh", 
          c(TIPO_DIA, mh_subida), 
          sep = "|", 
          remove = F)
  ##sql_od4overlapping
  sql_od4over <- paste0("SELECT SUM(CAST([f_exp_servicioperiodots] AS FLOAT)) AS DDA, 
                               [servicio_subida], 
                               [par_subida],
                               [par_bajada], 
                               [tipo_transporte], 
                               [tipo_dia], 
                               [mh_subida]
                         FROM [uchile].[dbo].", DDBB_e, 
                        "WHERE [servicio_subida] <> '-' AND 
                               [par_subida] <> '-' AND
                               [par_bajada] <> '-' AND
                               [tipo_transporte] <> '-' AND 
                               [tipo_dia] <> '-' AND
                               [mh_subida] <> '-'
                         GROUP BY [servicio_subida], 
                                  [par_subida],
                                  [par_bajada], 
                                  [tipo_transporte], 
                                  [tipo_dia], 
                                  [mh_subida];"
                        )
  od4over <- lazy_dt(DBI::dbGetQuery(conn = con, 
                                     statement = sql_od4over)
                     ) %>%
    as_tibble() %>%
    unite("dia_mh", 
          c(tipo_dia, mh_subida), 
          sep = "|", 
          remove = F) %>%
    left_join(per) %>%
    select(-dia_mh, 
           -mh_subida, 
           -TIPO_DIA) %>%
    group_by(servicio_subida, 
             par_subida, 
             par_bajada, 
             tipo_transporte, 
             tipo_dia, 
             PER_DTPM2) %>%
    summarise(DDA = sum(DDA)) %>%
    ungroup() %>%
    mutate(DDA = if_else(condition = tipo_dia == "LABORAL", 
                         true = DDA/5,
                         false = DDA,
                         missing = NA_real_)
           )
  #output_list
  od_lst <- list(od4cov, od4over)
  names(od_lst) <- c("od4cov", "od4over")
  return(od_lst)
}
metro_dt <- function(){
  library(dplyr)
  library(sf)
  #subway spatial data
  red_metro <- sf::st_read(dsn = "data/Red_Metro.shp") %>%
    select(layer)
  est_Metro <- sf::st_read(dsn = "data/Est_Metro.shp") %>%
    select(layer)
  ext_est_Metro <- sf::st_read(dsn = "data/Est_Ext_Metro.shp") %>%
    select(layer)
  ext_Metro <- sf::st_read(dsn = "data/Ext_Red_Metro.shp") %>%
    select(layer)
  metro_lst <- list(red_metro, est_Metro, ext_est_Metro, ext_Metro)
  names(metro_lst) <- c("red_metro", "est_Metro", "ext_est_Metro", "ext_Metro")
  #output list
  return(metro_lst)
}

landuse <- function(Region_nm){
  #libraries
  library(dplyr)
  library(osmdata)
  library(sf)
  #data
  zones <- st_read("data/Regional.shp") %>%
    st_transform(4326) %>%
    filter(Region == Region_nm) %>%
    select(codregion, Region)
  ###############
  #operations
  residential <- opq(bbox = st_bbox(zones)) %>%
    add_osm_feature(key = "landuse", value = "residential") %>%
    osmdata_sf()
  residential <- residential$osm_polygons %>%
    select(landuse) %>%
    mutate(landuse = "residential") %>%
    st_transform(32719) %>%
    st_intersection(x = st_transform(zones, 32719))
  oth_uses <- st_difference(st_transform(zones, 32719), st_union(residential)) %>%
    mutate(landuse = "other") %>%
    relocate(landuse, .before = geometry) %>%
    st_transform(4326)
  landuse <- rbind(st_transform(residential, 4326), oth_uses)
}

mnz_cns2017 <- function(mnz_censo){
  library(sf)
  library(dplyr)
  st_read(paste0("data/", mnz_censo)) %>%
    select(Hab = PERSONAS, Dens)
}

legend_creator = function(col.regions, xlab, ylab, nbins){
  bilegend = levelplot(matrix(1:(nbins * nbins), nrow = nbins),
                       axes = FALSE, col.regions = col.regions,
                       xlab = xlab, ylab = ylab,
                       cuts = 8, colorkey = FALSE, scales = list(draw = 0))
  bilegend
}

add_new_var = function(x, var1, var2, nbins, style = "fisher"){
  class1 = suppressWarnings(findCols(classIntervals(c(x[[var1]]), 
                                                    n = nbins, 
                                                    style = style)))
  
  class2 = suppressWarnings(findCols(classIntervals(c(x[[var2]]), 
                                                    n = nbins, 
                                                    style = style)))
  
  x$new_class = class1 + nbins * (class2 - 1)
  return(x)
}
