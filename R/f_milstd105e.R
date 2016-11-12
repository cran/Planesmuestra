f_milstd105e<-function(N,L,NCA,type){
# Encontrar el numero de linea de intervalo del lote
  if (missing(N)){
    stop("El lote debe ser igual o mayor que 2")
     } else {
	    data(lot_size, envir = environment())
            lot_interval<-findInterval(N,lot_size[,1])+1
# Asigna  el nivel de inspeccion si no es declarado explicitamente
            if (missing(L)){
              stop("Es necesario el argumento nivel o L")
              } else {
# Encuentra la letra correspondiente al tama?o del lote y nivel de inspeccion
                      data(code_letter, envir = environment())
	              code_l<-code_letter[lot_interval,L]
                      }
# Asigna el valor por defecto de NCA si no es declarado
# Intepolando el valor dado de NCA a los valores de tabla
           if (missing(NCA)){
              stop("Es necesario el argumento NCA")
              } else {
                     data(NCA_values, envir = environment())
                     if (any(NCA==NCA_values)){
                        NCA2<-NCA
                        } else {
                                NCA_interval<-findInterval(NCA,NCA_values)
                                NCA2<-NCA_values[NCA_interval]
                               }
                     }
# Asigna el tipo de inpeccion si no es declarado
           if (missing(type)){
              stop("Es necesario el argumento type del tipo de inspeccion")
              }
           if(type=="n"){
             T_ins<-"Normal"
             } else {
                     if(type=="r"){
                       T_ins<-"Reducida"
                       } else {
                               T_ins<-"Rigurosa"
                               }
                    }
# Leer los planes de inspeccion
# La informacion queda
           data(milstd105eplans, envir = environment())
# Busca con los argumetos code_l, T, NCA2 los valores n y c para el
# Plan de inspecci?n
           code1<-milstd105eplans[milstd105eplans$code_letter==as.vector(code_l),]
           T1<-code1[code1$T==type,]
           NCA3<-T1[T1$NCA==NCA2,]
           c<-NCA3$c
           muestra<-NCA3$n
# Objetos con los nombres de los argumentos y los argumentos
           argumentos_nombres<-c("Lote","Tipo de Inspeccion",
           "Nivel de Inspeccion", "Nivel de Calidad Aceptable")
           argumentos_plan<-c(N,T_ins,L,NCA)
# Objeto con los parametros del plan
           resultados_nombres<-c("Codigo Letra","Nivel de Calidad Aceptable", "Muestra",
           "Numero de Aceptacion", "Numero de Rechazo")
           resultados_plan<-c(as.vector(code_l),NCA2,muestra,c,c+1)
# Integrar los resultados en un data frame
           names(argumentos_plan)<-argumentos_nombres
           names(resultados_plan)<-resultados_nombres
# Presentar resultados
           print(argumentos_plan)
           print(resultados_plan)
            }
}
