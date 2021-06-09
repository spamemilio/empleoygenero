##install.packages("skimr")
# install.packages("sf")
# install.packages(c("cowplot", "googleway", "ggrepel", 
#                    "ggspatial", "libwgeom", "rnaturalearth", "rnaturalearthdata"))
# install.packages("rgeos ")
# 
# install.packages("spData")
#                 
# install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source") 
# 
# install.packages("tmap")
#library(skimr)

# install.packages("ggrepel")
# library(ggrepel)

# skim(datos_ordenados)

# na.omit(datos_ordenados) %>%
#   ggplot(aes(x=año, y=valor))+
# geom_boxplot() +
#   facet_wrap(~variable)

# na.omit(datos_ordenados) %>%
#   filter(year>=1991) %>% 
#   ggplot(aes(x=año, y=valor))+
#   geom_boxplot() +
#   facet_wrap(~variable)

# na.omit(datos_ordenados) %>%
#   filter(year==2000) %>% 
#   ggplot(aes(x=variable, y=valor))+
# geom_density() +
#   facet_wrap(~año)



# install.packages('rgeos', type="source")
# install.packages('rgdal', type="source")

#Liberías que sí usamos# library("rnaturalearth")
# library("rnaturalearthdata")
# library(sp)
# library(c("cowplot", "googleway", "ggrepel", 
#                    "ggspatial", "libwgeom", "rnaturalearth", "rnaturalearthdata"))
# 
# library(adehabitatHR)
# library(sf)
# library(magrittr)
# library(raster)
# library(dplyr)
# library(spData)
# library(spDataLarge)
# library(tmap)    # for static and interactive maps

# world <- ne_countries(scale = "medium", returnclass = "sf")

# class(world)

# st_as_sfc.SpatialPolygons(sp::geometry(x), ...)

#######################

# mapa <- borders("world", regions = regions, 
#                 colour = "black")


# clean everything --------------------------------------------------------

# rm(list=ls(all=TRUE))


# load maps ---------------------------------------------------------------

# install.packages("ggmap")
install.packages("gganimate") 
install.packages("gifski") 
# Los datos 
library(gapminder) 
# Cargar las librerias 
library(ggplot2) 
library(gganimate) 
# Luego copiaremos este código que genera el grafico de puntos separados por continentes 
# comparando las variables expectativa de vida y DGP per cápita del paquete gapminder que 
# hicimos en las prácticas anteriores: 
  
  ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = 
                          country)) + 
  geom_point(alpha = 0.7, show.legend = FALSE) + 
  scale_colour_manual(values = country_colors) + 
  scale_size(range = c(2, 12)) + 
  scale_x_log10() + 
  facet_wrap(~continent) + 
  # Aqui está el código que genera la animación
    
    # guayana <- world %>% 
    #   filter(admin == "France") %>% 
    #   st_geometry(obj) 
    # 
    # g_g <-  st_sfc(guayana_geom)
    # guayana_geom =
    #            list(
    #              list(     c(-52.288916015625, -52.324609375, -52.2199707031249, -52.05810546875, -52.0123046875, 
    #                          -51.9619140625, -51.9793457031249, -52.001708984375, -52.0029296875, -51.95478515625, 
  #                          -51.927685546875, -51.9195800781249, -51.8802734375, -51.8275390625, -51.78564453125, 
  #                          -51.6986328125, -51.6658203124999, -51.653271484375, -51.65810546875, -51.6525390625,
  #                          -51.683447265625, -51.76708984375, -51.8052734375, -51.827490234375, -51.8794921874999,
  #                          -51.92890625, -51.9443359375, -51.9906249999999, -51.99951171875,      -52.11611328125,
  #                          -52.16259765625, -52.229443359375, -52.271240234375, -52.327880859375, -52.356640625,
  #                          -52.356640625, -52.39638671875, -52.418408203125, -52.455859375, -52.5546875,
  #                          -52.55947265625, -52.5830078125, -52.653173828125, -52.700634765625, -52.7833984375,
  #                          -52.87041015625, -52.903466796875, -52.96484375, -53.009716796875, -53.082275390625,
  #                          -53.180078125, -53.22978515625, -53.252197265625, -53.2854980468749, -53.334423828125,
  #                          -53.366015625, -53.4318359375, -53.508984375, -53.56396484375,      -53.68369140625,
  #                          -53.734716796875, -53.750146484375, -53.7677734375, -53.79423828125, -53.829541015625,
  #                          -53.8766113281249, -53.946435546875, -54.08974609375, -54.130078125, -54.1673828124999,
  #                          -54.227978515625, -54.29306640625, -54.43310546875, -54.515087890625, -54.55048828125,
  #                          -54.591943359375, -54.616259765625, -54.604736328125, -54.568408203125, -54.5359375,
  #                          -54.485546875, -54.402001953125, -54.25673828125, -54.1955078125, -54.1880859375, 
  #                          -54.170703125, -54.203125, -54.188037109375, -54.06318359375,      -54.0095703124999,
  #                          -54.00595703125, -53.990478515625, -54.005908203125, -54.034228515625, -54.081982421875,
  #                          -54.11279296875, -54.1974609375, -54.255517578125, -54.350732421875, -54.342138671875, 
  #                          -54.369140625, -54.3983886718749, -54.396240234375, -54.416015625, -54.440673828125,
  #                          -54.449609375, -54.42607421875, -54.440234375, -54.471142578125, -54.4796875, 
  #                          -54.47333984375, -54.446875, -54.452197265625, -54.331640625, -54.240185546875,
  #                          -54.15595703125, -54.085302734375, -53.9895996093749, -53.919921875,  
  #                          -53.84716796875, -53.454443359375, -53.270361328125, -52.89931640625, 
  #                          -52.764990234375, -52.453955078125, -52.29052734375, -52.288916015625,
  #                          4.87612304687498, 4.77089843750004, 4.86279296875001, 4.71738281249996,
  #                          4.64599609374999, 4.514404296875, 4.42988281250001, 4.38623046875, 4.35229492187501, 
  #                          4.39907226562505, 4.43613281250005, 4.52431640624998, 4.63374023437498, 4.63569335937503,
  #                          4.57050781249998, 4.28681640625, 4.22880859375, 4.13876953125003, 4.09848632812501,
  #                          4.06127929687497, 4.03969726562502,      3.99267578125003, 3.92993164062504, 
  #                          3.869580078125, 3.828564453125, 3.77695312500005, 3.73510742187504, 3.70200195312496,
  #                          3.64687500000001, 3.45229492187498, 3.364697265625, 3.27167968750005, 3.23710937500002,
  #                          3.18173828125002, 3.11772460937503, 3.05156249999997, 2.97221679687497, 2.90385742187499,
  #                          2.86416015624998, 2.64765625000001, 2.57314453125002, 2.52890624999998, 2.42573242187501,
  #                          2.36367187499998, 2.31718750000002, 2.26665039062502, 2.21152343749998, 2.18354492187497,
  #                          2.18173828125005, 2.20170898437503, 2.21132812500004, 2.20488281249996, 
  #                          2.23227539062503, 2.29521484374997, 2.33974609374999, 2.32421875000003, 2.27944335937497,
  #                          2.25312500000003, 2.26191406250004, 2.29291992187504, 2.30854492187503, 2.335009765625,
  #                          2.35483398437505, 2.34599609375002, 2.31293945312504, 2.278271484375, 2.23256835937501,
  #                          2.15048828124998, 2.12104492187503, 2.13706054687501, 2.15332031250003, 2.15424804687504,
  #                          2.20751953125006, 2.24545898437496, 2.29306640624999, 2.31376953125003, 2.32675781250001,
  #                          2.33579101562499, 2.34257812500003, 2.343310546875, 2.41611328125001, 2.46152343750002, 
  #                          2.71372070312498, 2.81787109375006, 2.87485351562503, 2.99360351562497, 3.13818359375003,
  #                          3.17875976562503, 3.35332031249999, 3.44853515625002, 3.53051757812503, 3.58955078124997,
  #                          3.62041015625005, 3.62939453125, 3.70595703124997, 3.76943359375002, 3.83442382812503,
  #                          3.90107421875003, 4.05410156250002, 4.14003906250004, 4.17094726562502, 4.20249023437496,
  #                          4.24140624999998, 4.33764648437506, 4.42802734374999,      4.48500976562497, 
  #                          4.58300781250004, 4.69199218750003, 4.74931640624999, 4.83652343749999, 4.91469726562502,
  #                          4.95878906250003, 5.01347656249999, 5.18740234374998, 5.28823242187501, 5.35898437500003, 5.41181640625004, 5.67602539062504, 5.76899414062498, 5.78222656250001, 
  #                          5.56347656250003, 5.54326171875, 5.42504882812499, 5.27348632812502, 5.02133789062499,
  #                          4.94218750000003, 4.87612304687498))
  #            )
  
  
  # indicadores_anio_pais <- 
  # st_join(indicadores_anio_pais, latinoamerica)
    
    # geom_label_repel(aes(label = valor), data=tipo_empleo_anio_latam_etiquetas, colour="black", segment.colour="red", max.overlaps = 15, stat = "identity", direction = "x" )
    
  pruebita <- 
    indicadores_anio_pais %>% 
    filter(año %in% c(1970,1981,1991,2001,2011) ) %>% 
    mutate(cantidad_indicadores = ifelse(is.na(cantidad_indicadores),0,cantidad_indicadores)) %>% 
    filter(admin %in% c("Surinam") )
  
  
  # scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
    
    tipo_empleo = str_sub(variable,
                          start = unlist(gregexpr(pattern ='_',variable))[1] + 1,
                          end = unlist(gregexpr(pattern ='_',variable))[2] - 1
                          
                          
                          tipo_empleo_anio_pais_bigenero <- tipo_empleo_anio_pais %>% 
                            pivot_wider(names_from = genero, 
                                        values_from = valor) %>% 
                            group_by(pais_region,tipo_empleo, year ) %>% 
                            summarise(varones = sum(varones, na.rm = TRUE),mujeres=sum(mujeres, na.rm = TRUE))
                          
                          tipo_empleo_anio_pais_bigenero %>% 
                            ggplot(aes(x=year,y=valor))+
                            geom_line()+
                            facet_grid(tipo_empleo ~ pais_region)+
                            labs(x ="Año", y = "Porcentaje por tipo de empleo", title = "Evolución de la participación en el empleo en Latinoamérica",
                                 subtitle = "Desagregado por país y tipo de empleo según género (1991-2018)")
                          
                          
                          
                          #Año 1970
                          
                          indicadores_anio_pais_1970 <- indicadores_anio_pais %>% 
                            filter(año==1970) 
                          
                          ggplot(data=indicadores_anio_pais_1970)+
                            theme_bw()+
                            xlab("Longitud") + ylab("Latitud") + 
                            theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey80")
                                  , panel.grid.minor = element_blank())+
                            # scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
                            geom_sf(aes(fill = cantidad_indicadores)) +
                            ggtitle("Indicadores Laborales en Latinoamérica", subtitle = paste0("Año 1970"))
                          
                          
                          #Año 1980
                          
                          indicadores_anio_pais_1981 <- indicadores_anio_pais %>% 
                            filter(año==1981) 
                          
                          ggplot(data=indicadores_anio_pais_1981)+
                            theme_bw()+
                            xlab("Longitud") + ylab("Latitud") + 
                            theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey80")
                                  , panel.grid.minor = element_blank())+
                            # scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
                            geom_sf(aes(fill = cantidad_indicadores)) +
                            # geom_sf_label(aes(label = cantidad_indicadores),label.size = 0.05)+
                            ggtitle("Indicadores Laborales en Latinoamérica", subtitle = paste0("Año 1981"))
                          
                          
                          #Año 1991
                          indicadores_anio_pais_1991 <- indicadores_anio_pais %>% 
                            filter(año==1991) 
                          
                          ggplot(data=indicadores_anio_pais_1991)+
                            theme_bw()+
                            xlab("Longitud") + ylab("Latitud") + 
                            theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey80")
                                  , panel.grid.minor = element_blank())+
                            # calse_fill_viridis_c(option = "plasma", trans = "sqrt")+
                            geom_sf(aes(fill = cantidad_indicadores)) +
                            ggtitle("Indicadores Laborales en Latinoamérica", subtitle = paste0("Año 1991"))+
                            # scale_alpha(range = c(0.1, 1) )
                            scale_fill_continuous(breaks =  c( 0, 10, 20,30))
                          # scale_fill_gradient(low = 3, high = 0, space = "Lab")
                          guides(fill = guide_colourbar(reverse = TRUE,barheight = unit(2, "cm")))
                          # guides(fill = guide_coloursteps(show.limits = FALSE))+
                          # guides(fill = guide_legend(ncol = ))
                          
                          mapear_indicadores_anio_pais <- function(dataset,anio) {
                            
                            dataset_filtrado <- dataset %>% 
                              filter(año==anio) 
                            
                            print(
                              ggplot(data=dataset_filtrado)+
                                theme_bw()+
                                xlab("Longitud") + ylab("Latitud") + 
                                theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey80")
                                      , panel.grid.minor = element_blank())+
                                # scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
                                geom_sf(aes(fill = cantidad_indicadores)) +
                                ggtitle("Latinoamérica", subtitle = paste0("Año", anio))
                            )
                            
                          }
                          
                          a <- mapear_indicadores_anio_pais(indicadores_anio_pais,1970)
                          
                          print(a)