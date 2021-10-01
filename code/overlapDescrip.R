library(magick)
library(tibble)
library(png)
library(dplyr)
library(ggplot2)
library(ggforce)
library(animation)
#input
df_spp <- tibble::tibble(x = c(2, 8, 2, 8, 2, 8, 2, 8, 2.5, 7.5), 
                     y = c(2, 8, 3.5, 6.5, 2.5, 6, 4, 7.5, 1.5, 8.5), 
                     r = c(1, 1, NA, NA, NA, NA, NA, NA, NA, NA), 
                     cut = c("l1", "l1", "l2", "l2", "l3", "l3", "l4", "l4", "l5", "l5")
                     )

si <- readPNG(source = "data/Circle-icons-check.svg.png")
no <- readPNG(source = "data/everything_you_thought_was_wrong.png")
ct_lst <- c("l2", "l3", "l4", "l5")
#list of plots
g_spp <- lapply(X = ct_lst, FUN = function(z) if (z == "l5") {
  ggplot() +
    geom_circle(aes(x0 = x, y0 = y, r = r), linetype = "twodash", size = 1, data = df_spp) +
    geom_line(aes(x, y, shape = cut), 
              lineend = "round", 
              size = 5, 
              data = filter(.data = df_spp, cut %in% c("l1", z))) +
    geom_line(aes(x, y, color = cut), size = 2, data = filter(.data = df_spp, cut %in% c("l1", z))) +
    geom_point(aes(x, y), color = "red", data = filter(.data = df_spp, cut %in% c("l1", z))) +
    ggtitle("Proceso de Elección")+
    theme(plot.title = element_text(hjust = 0.5)) +
    annotation_raster(si, ymin = 7.5,ymax= 9,xmin = 3,xmax = 4.5) + 
    geom_point()
} else {ggplot() +
    geom_circle(aes(x0 = x, y0 = y, r = r), linetype = "twodash", size = 1, data = df_spp) +
    geom_line(aes(x, y, shape = cut), 
              lineend = "round", 
              size = 5, 
              data = filter(.data = df_spp, cut %in% c("l1", z))) +
    geom_line(aes(x, y, color = cut), size = 2, data = filter(.data = df_spp, cut %in% c("l1", z))) +
    geom_point(aes(x, y), color = "red", data = filter(.data = df_spp, cut %in% c("l1", z))) +
    ggtitle("Proceso de Elección")+
    theme(plot.title = element_text(hjust = 0.5)) +
    annotation_raster(no, ymin = 7.5,ymax= 9,xmin = 3,xmax = 4.5) + 
    geom_point()}
)
#save graphs
lapply(X = 1:4, 
       FUN = function(x) ggsave(plot = g_spp[[x]], 
                                filename = paste0("output/spp_", x, ".png"), 
                                device = "png")
)
#animated gif
imgs <- list.files(pattern="spp_", path = "output/")
saveGIF(movie.name = "spp.gif", 
        expr = lapply(X = imgs, 
                      FUN = function(x) plot(as.raster(as.raster(image_read(paste0("output/", x) 
                                                                            )
                                                                 ) 
                                                       ) 
                                             ) 
                      ) 
        )
