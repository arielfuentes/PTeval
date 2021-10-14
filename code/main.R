#libraries
library(rmarkdown)
#calling base codes
#run multiple Rmarkdown
source("code/wrg_dt.R", encoding = "utf-8")
if(exists("output/spp.gif") == F){
  source("code/overlapDescrip.R", encoding = "utf-8")
  file.copy(from = "spp.gif", to = "output/")
}
spp_gf <- magick::image_read("output/spp.gif")
per <- unique(od_sersen$PER_DTPM2)
per <- "04 - PMA"
rutas <- rts$ROUTE_NAME
rutas <- "B02R"
lapply(X = per, 
       FUN = function (y) lapply(X = rutas, 
                                 FUN = function(x) tryCatch(render(input = "code/Report.Rmd", 
                                                          output_file =  paste("reporte_", x, "_", y, ".html", sep=''), 
                                                          output_dir = "output/", 
                                                          encoding = "utf-8"),
                                                          error = function(e) NULL)
                                 )
       )
