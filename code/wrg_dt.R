source(file = "code/ipt.R", encoding = "utf-8")

#parameters
A4_nm <- "2019-04-01_consolidado_anexo4_(Circunvalación)_anual.xlsx"
rt_shp <- "Shapes 06Jul2019"
stp_shp <- "Paradas 06Jul2019"
DDBB_v <- "[baseviajesDLN201904]"
DDBB_e <- "[etapas201904]"
mnz_censo <- "mnz_cns2017.gpkg"
Region_nm <- "Región Metropolitana de Santiago"

library(readr)
#input objects
A4 <- A4_stops(A4_nm)
rts <- routes(rt_shp) #with geometry
rts2 <- rts %>%
  st_drop_geometry() %>%
  select(ROUTE_NAME, COD_USUARI, COD_SINRUT, COD_USUSEN) %>%
  filter(COD_SINRUT != "NA") %>%
  distinct() %>%
  bind_rows(read_delim("data/faltantes.csv", ";")) %>%
  as_tibble()
metro_lst <- metro_dt()
censo <- mnz_cns2017(mnz_censo)
suelo <- landuse(Region_nm)
#rts <- dplyr::filter(.data = rts, UN == 6) #only for testing
stp <- stops(stp_shp) #with geometry
stp2 <- stp %>% #without geometry
  st_drop_geometry() %>%
  select(-`Nombre Paradero`) %>%
  distinct() 
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
                        select(.data = stp2, par_subida = CODINFRA)) %>% 
  right_join(select(.data = stp2, par_bajada = CODINFRA)) %>% 
  left_join(select(.data = rts2, 
                   servicio_subida = COD_SINRUT, 
                   ROUTE_NAME, 
                   COD_USUARI)) %>%
  as_tibble() %>%
  na.omit()
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
                       ) %>% 
  left_join(lazy_dt(select(.data = A4,
                           ROUTE_NAME,
                           par_bajada = `Código paradero TS`, 
                           orden_baj = `Orden`,
                           nom_baj = `Nombre Paradero`,
                           UN)
                    )
            ) %>%
  as_tibble() %>%
  select(-c("servicio_subida", "tipo_transporte")) %>%
  unite("parOD", par_subida, par_bajada, remove = F, sep = "|") %>%
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
#connectivity data
# # Primera <- od_lst$od4connec %>%
#   select(serv = serv_1era, bajada = paraderobajada_1era, periodo, Demanda)
Primera <- od_lst$od4connec %>%
  select(serv1 = serv_1era, 
         bajada = paraderobajada_1era, 
         serv2 = serv_2da, 
         subida = paraderosubida_2da, 
         periodo, 
         Demanda)
# Segunda <- od_lst$od4connec %>%
#   filter(netapa > 2) %>%
#   select(serv = serv_2da, bajada = paraderobajada_2da, periodo, Demanda)
Segunda <- od_lst$od4connec %>%
  filter(netapa > 2) %>%
  select(serv1 = serv_2da, 
         bajada = paraderobajada_2da, 
         serv2 = serv_3era, 
         subida = paraderosubida_3era, 
         periodo, 
         Demanda)
Tercera <- od_lst$od4connec %>%
  filter(netapa == 4) %>%
  select(serv1 = serv_3era, 
         bajada = paraderobajada_3era, 
         serv2 = serv_4ta, 
         subida = paraderosubida_4ta, 
         periodo, 
         Demanda)

Connec <- bind_rows(as_tibble(Primera), as_tibble(Segunda), as_tibble(Tercera)) %>%
  group_by(serv1, bajada, serv2, subida, periodo) %>%
  summarise(Demanda = sum(Demanda)) %>%
  ungroup() %>%
  arrange(periodo, serv1) %>%
  left_join(select(.data = stp2, 
                   bajada = CODINFRA, 
                   bajada_SIMT = SIMT)) %>%
  left_join(select(.data = stp2, 
                   subida = CODINFRA, 
                   subida_SIMT = SIMT)) %>%
  left_join(select(rts2, SERV_i = COD_USUSEN, serv1 = COD_SINRUT)) %>%
  left_join(select(rts2, SERV_j = COD_USUSEN, serv2 = COD_SINRUT)) %>%
  select(-c("serv1", "serv2", "subida", "bajada"))

rm(Primera, Segunda, Tercera, od_lst)

# trx_baj <- left_join(rename(Connec, bajada_DTPM = bajada), 
#                      select(.data = stp2, 
#                             bajada_DTPM = CODINFRA, 
#                             bajada_SIMT = SIMT)
# ) %>%
#   select(-bajada_DTPM) %>%
#   rename(bajada = bajada_SIMT) %>%
#   left_join(select(rts2, COD_USUSEN, serv = COD_SINRUT)) %>%
#   filter(!is.na(bajada)) %>%
#   select(-serv) %>%
#   relocate(periodo, COD_USUSEN, bajada) %>%
#   na.omit()
#   
# 
# stop_frame <- A4 %>%
#   select(COD_USUSEN = ROUTE_NAME, bajada = `Código  paradero Usuario`, Orden)
# #needs subway stops
# filter(trx_baj, 
#        stringr::str_starts(COD_USUSEN, "[LM]") == T) %>%
#   select(-Demanda) %>%
#   mutate(Orden = NA)
# per_frame <- tibble(COD_USUSEN = rep(unique(stop_frame$COD_USUSEN), each = 29), 
#                     PER_DTPM2 = rep(unique(od_sersen$PER_DTPM2), length(unique(stop_frame$COD_USUSEN)))) %>%
#   arrange(COD_USUSEN, PER_DTPM2)
# 
# dicc_per <- tibble(PER_DTPM2 = unique(per_frame$PER_DTPM2)[1:12], 
#                    periodo = unique(trx_baj$periodo))
# 
# per_stop <- left_join(per_frame, stop_frame) %>%
#   left_join(dicc_per) %>%
#   select(-PER_DTPM2) %>%
#   bind_rows(filter(trx_baj, 
#          stringr::str_starts(COD_USUSEN, "[LM]") == T) %>%
#   select(-Demanda) %>%
#   mutate(Orden = NA)) %>%
#   # na.omit() %>%
#   # select(-PER_DTPM2) %>%
#   right_join(trx_baj)
# ####fix ROUTE_NAMES WITH ("_")
# rm(stop_frame, per_frame)