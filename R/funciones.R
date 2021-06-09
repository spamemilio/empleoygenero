corregir_porcentaje <- function(porcentaje) {
  if(is.na(porcentaje)) return(porcentaje)  
  else if (porcentaje>100) return( porcentaje/1000)
  else return (porcentaje)
}

cantidad_indicadores_por_anio_pais <- function(anio,pais=NULL) {
  
  respuesta <- datos_ordenados %>% 
    right_join(paises_codigos) %>% 
    group_by(year,admin) %>% 
    summarise(cantidad_indicadores =sum(!(is.na(valor)))) %>% 
    filter(year==anio)
  
  if (!is.null(pais)) {
    respuesta <- respuesta %>% 
      filter(admin==pais) %>% 
      dplyr::select(cantidad_indicadores)
  }
  
  else {
    respuesta <- respuesta %>% 
    group_by(year) %>% 
    summarise(cantidad_indicadores =sum(cantidad_indicadores)) %>% 
      dplyr::select(cantidad_indicadores)
  }
  
  return(
  respuesta 
  )
  
}
