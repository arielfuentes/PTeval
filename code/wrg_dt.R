source(file = "code/ipt.R", encoding = "utf-8")

#parameters
A4_nm <- "2019-04-01_consolidado_anexo4_(Circunvalación)_anual.xlsx"
rt_shp <- "Shapes 06Jul2019"
stp_shp <- "Paradas 06Jul2019"
DDBB_v <- "[baseviajesDLN201904]"
DDBB_e <- "[etapas201904]"
mnz_censo <- "mnz_cns2017.gpkg"
Region_nm <- "Región Metropolitana de Santiago"

#input objects
A4 <- A4_stops(A4_nm)
rts <- routes(rt_shp)
metro_lst <- metro_dt()
censo <- mnz_cns2017(mnz_censo)
suelo <- landuse(Region_nm)
#rts <- dplyr::filter(.data = rts, UN == 6) #only for testing
stp <- stops(stp_shp)
if(!exists("od_lst")) 
  od_lst <- od(DDBB_v = DDBB_v, DDBB_e = DDBB_e) 

###OD by line --- data preparation
library(sf)
library(dplyr)
library(data.table)
library(dtplyr)
library(tidyr)
library(stringr)

od_sersen <- right_join(lazy_dt(od_lst$od4over), 
                       lazy_dt(st_drop_geometry(select(.data = stp, par_subida = CODINFRA)
                                                )
                                        )
                       ) %>% 
  right_join(lazy_dt(st_drop_geometry(select(.data = stp, par_bajada = CODINFRA))
                              )
             ) %>% 
  left_join(lazy_dt(select(.data = rts, servicio_subida = COD_SINRUT, ROUTE_NAME, COD_USUARI)
                    )
            ) %>%
  as_tibble() %>%
  na.omit() %>%
  select(c(5:6, 1:2, 7:10, 3))
# od4cov <- od_lst$od4cov
# rm(od_lst)
#add order 
od_sersen <- left_join(lazy_dt(od_sersen), 
                        lazy_dt(select(.data = A4, 
                                       ROUTE_NAME,
                                       par_subida = `Código paradero TS`, 
                                       orden_sub = `Orden`,
                                       nom_sub = `Nombre Paradero`)
                                )
                       ) %>% left_join(lazy_dt(select(.data = A4,
                                                      ROUTE_NAME,
                                                      par_bajada = `Código paradero TS`, 
                                                      orden_baj = `Orden`,
                                                      nom_baj = `Nombre Paradero`,
                                                      UN)
                                               )
                                       ) %>%
  as_tibble() %>%
  select(c(6, 2, 7, 3, 8, 4, 5, 1, 10:14)) %>%
  unite("parOD", par_subida, par_bajada, remove = F, sep = "|") %>%
  select(c(2:14, 1)) %>%
  dplyr::group_by(par_subida, 
                  par_bajada, 
                  orden_sub, 
                  orden_baj, 
                  nom_sub, 
                  nom_baj, 
                  UN, 
                  ROUTE_NAME, 
                  tipo_dia, 
                  PER_DTPM2, 
                  parOD,
                  COD_USUARI) %>% 
  summarise(DDA = sum(DDA)) %>%
  na.omit() %>%
  ungroup() 
#summary dda
ser <- group_by(.data = od_sersen, COD_USUARI, PER_DTPM2) %>% 
  summarise(DDAser = sum(DDA))

sen <- group_by(.data = od_sersen, ROUTE_NAME, COD_USUARI, PER_DTPM2) %>% 
  summarise(DDAsen = sum(DDA))

DDAsersen <- left_join(ser, sen) %>%
  mutate(Porcentaje = paste0(round(DDAsen/DDAser*100, 2), "%"), 
         Eleccion = case_when(
           DDAsen/DDAser > 0.5 ~ "Este es el Sentido más cargado",
           DDAsen/DDAser < 0.5 ~ "Este es el Sentido menos cargado",
           DDAsen/DDAser == 0.5 ~ "Ambos sentidos pesan lo mismo"
           )
         )
rm(ser, sen)
