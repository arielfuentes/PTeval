spp_global <- function(x, y){
  library(tibble)
  library(dplyr)
  library(sf)
  A4_sersen <- filter(.data = A4, ROUTE_NAME == x) %>%
    rename(`Servicio Sentido` = ROUTE_NAME)
#od by bus & period 
od_xy <- filter(.data = od_sersen, ROUTE_NAME == x & PER_DTPM2 == y)
# demand by OD only
od_x <- filter(.data = od_sersen, PER_DTPM2 == y) %>%
  select(parOD, DDA) %>%
  group_by(parOD) %>%
  summarise(DDA = sum(DDA))

buff_z <- lapply(X = 1:nrow(od_xy),
                 FUN = function(z) st_buffer(x = dplyr::arrange(left_join(filter(.data = stp,
                                                                                 CODINFRA %in% c(od_xy$par_subida[z],
                                                                                                 od_xy$par_bajada[z])), 
                                                                          select(.data = A4_sersen,
                                                                                 CODINFRA = `Código paradero TS`, 
                                                                                 Orden)
                 ), Orden),
                 dist = 300
                 )
)                  
#extract bus & competitors stops
inter_aa <- lapply(X = buff_z, 
                   FUN = function(aa) select(.data = st_drop_geometry(st_join(x = aa, y = stp, 
                                                                              join = st_intersects)
                   ),
                   CODINFRA.x, CODINFRA.y
                   )
)
#create OD
inter_ab <- lapply(X = inter_aa,
                   FUN = function(ab) group_split(.tbl = add_column(.data = ab,
                                                                    grupo = str_sub(row.names(ab),
                                                                                    start = 1,
                                                                                    end = 1
                                                                    )
                   ),
                   grupo
                   )
)

#list of competitors OD
comb_ac1 <- unlist(lapply(X = inter_ab, 
                          FUN = function(ab) as.vector(outer(ab[[1]]$CODINFRA.y, 
                                                             ab[[2]]$CODINFRA.y, 
                                                             paste, 
                                                             sep = "|"
                          )
                          )
)
)
#list of bus OD                  
comb_ac2 <- unlist(lapply(X = inter_ab, 
                          FUN = function(ab) as.vector(outer(ab[[1]]$CODINFRA.x, 
                                                             ab[[2]]$CODINFRA.x, 
                                                             paste, 
                                                             sep = "|"
                          )
                          )
)
)

od_xy_spp <- nrow(filter(.data = od_sersen,
                         PER_DTPM2 == y & (parOD %in% comb_ac1)
)
)/nrow(od_xy)

spp_df <- tibble(sersen = x, periodo = y, idx_spp = od_xy_spp)
return(spp_df)
}
per <- unique(od_sersen$PER_DTPM2)
rutas <- filter(.data = rts, UN == 6)$ROUTE_NAME
# spp_global(x = "B01I", y = "04 - PMA")

spp_lstdf <- lapply(X = rutas, 
       FUN = function(r) lapply(X = per, 
                                FUN = function(p) tryCatch(spp_global(x = r, y = p), 
                                                           error = function(e) NULL
                                                           )
                                )
       )

idx_dfGL <- function(){
  library(dplyr)
  library(openxlsx)
  library(readr)
  library(stringr)
  #day type frame
  tp_dia <- select(.data = od_sersen, 
         UN, 
         sersen = ROUTE_NAME, 
         tipo_dia, 
         periodo = PER_DTPM2) %>% 
    filter(UN == 6) %>% 
    distinct()
  #dataframe
  spp_df <- bind_rows(spp_lstdf) %>%
    left_join(tp_dia) %>%
    mutate(periodo_n = as.numeric(str_sub(periodo, 1, 2)
                                  )
           ) %>%
    arrange(sersen, periodo_n)
  
  #output
  write_delim(x = spp_df, 
              path = "output/spp.csv", 
              delim = ",", 
              na = "", 
              col_names = T)
  return(spp_df)
}

spp_dfGL <- idx_dfGL()

sum_stat <- group_by(.data = spp_dfGL, tipo_dia) %>% 
  summarise(promedio = mean(idx_spp), 
            `Dev. Est.` = sd(idx_spp)
            )
spp_tdia <- left_join(spp_dfGL, sum_stat) %>%
  arrange(periodo_n) %>%
  group_by(tipo_dia) %>%
  mutate(pos1 = lag(promedio), pos2 = lead(`Dev. Est.`)) %>%
  mutate_at(vars(pos1, pos2), ~replace(., is.na(.), 0)) 
  
sum_stat2 <- group_by(.data = spp_dfGL, periodo_n) %>% 
  summarise(promedio = mean(idx_spp), 
            `Dev. Est.` = sd(idx_spp)
  )
spp_per <- left_join(spp_dfGL, sum_stat2) %>%
  arrange(periodo_n) %>%
  group_by(periodo_n) %>%
  mutate(pos1 = lag(promedio), pos2 = lead(`Dev. Est.`))

rmarkdown::render(input = "code/Índices Generales.Rmd", 
                  output_file = "output/indicadores.html", 
                  output_dir = "output/",
                  encoding = "utf-8")
