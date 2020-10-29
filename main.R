#libraries
library(rmarkdown)
#calling base codes
spp_gf <- magick::image_read("output/spp.gif")
#run multiple Rmarkdown
source("code/wrg_dt.R", encoding = "utf-8")
per <- unique(od_sersen$PER_DTPM2)
rutas <- rts$ROUTE_NAME
lapply(X = per, 
       FUN = function (y) lapply(X = rutas, 
                                 FUN = function(x) tryCatch(render(input = "code/Reporte2.Rmd", 
                                                          output_file =  paste("reporte_", x, "_", y, ".html", sep=''), 
                                                          output_dir = "output/", 
                                                          encoding = "utf-8"),
                                                          error = function(e) NULL)
                                 )
       )
