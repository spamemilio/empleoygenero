theme_set(theme_bw())
options("scipen"=100, "digits"=4)


library(ggmap)
library(ggplot2)
library(raster)
library(maptools)


library(rgeos)

library(dplyr)


# Bajamos la base con todos los países del mundo


# Paises de Latinoamérica
regions = c("Brazil", "Uruguay", "Argentina", "French Guiana", "Suriname", "Colombia", "Venezuela",
            "Bolivia", "Ecuador", "Chile", "Paraguay", "Peru", "Guyana", "Panama", "Costa Rica", 
            "Nicaragua", "Honduras", "El Salvador", "Belize", "Guatemala", "Mexico", "Trinidad and Tobago",
            "Caribe", "Puerto Rico", "Dominican Republic", "Haiti", "Jamaica", "Cuba", "Bahamas", "Antiles",
            "Dominica", "Saba","Suriname")

# Filtramos los países del mundo por los Latinoaméricamos
latinoamerica <- world %>% 
  # Corregimos error colonial
  mutate(admin = ifelse(admin=="Falkland Islands","Argentina",admin)) %>% 
  filter(admin %in% regions) 
  

# mapa_paises_indicadores <-
#   theme_bw()+
#   xlab("Longitud") + ylab("Latitud") + 
#   theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey80")
#         , panel.grid.minor = element_blank())+
#   geom_sf(aes(fill = cantidad_indicadores)) 
#   ggtitle("Latinoamérica", subtitle = paste0("(", length(regions), " países)"))

# Unificamos la información del datasets con la información geográfica de los países
indicadores_anio_pais <- latinoamerica %>%
  left_join(indicadores_anio_pais)


#Filtramos por los años que elegimos para tomar como muestra de la evolución de los indicadores

indicadores_1970_1981_1991_2001_2011 <- indicadores_anio_pais %>% 
  filter(año %in% c(1970,1981,1991,2001,2011) ) %>% 
  mutate(cantidad_indicadores = ifelse(is.na(cantidad_indicadores),0,cantidad_indicadores))

# Generamos gráfico de indicadores para los ejecrcicios seleccionados
grafico_mapa_indicadores <- ggplot(data=indicadores_1970_1981_1991_2001_2011)+
  xlab(element_blank()) + ylab(element_blank()) + 
  theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey80")
        , panel.grid.minor = element_blank(), axis.text = element_blank(), legend.position = c(0.9,0.15),
        legend.title = element_text(size = 10),
        strip.text.x = element_text(size = 12,face="bold"),
        strip.background = element_rect(color="black", fill="#FFE599", size=0.5, linetype="solid"))+
  labs(fill="Cantidad de\n Indicadores")+
  scale_fill_viridis_c(option = "cividis", trans = "sqrt")+
  geom_sf(aes(fill = cantidad_indicadores)) +
  ggtitle("Indicadores Laborales en Latinoamérica", subtitle = paste0("Cantidad de indicadores por país por año " ))+
  facet_wrap(~año)
