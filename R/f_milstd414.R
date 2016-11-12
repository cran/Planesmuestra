f_milstd414<-function(N,L,NCA,type){
  # Encontrar el intervalo del lote si este existe
  if (missing(N)){
    # si no encuentra el lote, para la funcion
    stop("Debe definir un Lote para continuar")
  } else {
    if (N<3){
      stop("El lote debe ser igual o mayor que 3")
    } else {
    # De ser cierto encuentra el intervalo especifico
        data(lot_size.milstd414, envir = environment())
        lot_interval<-findInterval(N,lot_size.milstd414$N)
        lot_fix<-lot_size.milstd414[lot_interval,1]
if (missing(L)){
          stop("Debe definir un nivel de inspeccion")
          } else {
            data(code_letter.milstd414, envir = environment())
    # Asigna la letra segun el intervalo del lote
            code_letter<-as.vector(code_letter.milstd414[lot_interval,L])
          }
if (missing(type)){
          stop("Debe definir un tipo de inspeccion")
          } else {
            data(k_plans.milstd414, envir = environment())
    # Indexa todos los planes y filtra de acuerdo al plan
            NCA_T<-k_plans.milstd414[k_plans.milstd414$T==type,]
    # Indexa el objeto anterior y lo filtra de acuerdo a la letra
            NCA_T_c.l<-NCA_T[NCA_T$code_letter==code_letter,]
    # Busca el intervalo del NCA
            NCA_interval<-findInterval(NCA,NCA_T_c.l$NCA)
    # Decide si el intervalo es menor al menor del NCA de tabla
    # para aproximarlo al primero, de lo contrario la fijacion del
    # NCA de tabla es normal y los valores mayores se quedan en el
    # valor maximo de tabla
            if (NCA < min(NCA_T_c.l$NCA)){
              NCA_fix<-NCA_T_c.l$NCA[1]
            } else {
              NCA_fix<-NCA_T_c.l$NCA[NCA_interval]
            }
    # Indexa y filtra la unica fila de acuerdo al NCA de tabla
            milstd414_plan<-NCA_T_c.l[NCA_T_c.l$NCA==NCA_fix,]
if(type=="n"){
              T_plan<-"Normal"
            } else {
              T_plan<-"Riguroso"
            }
          }
    #
    # Consolida los resultados en vectores con nombres
    #
    argumentos_nombres<-c("Lote","Plan",
                          "Nivel de Inspeccion","NCA Inicial")
    argumentos_plan<-c(as.integer(N),T_plan,L,NCA)
    resultados_nombres<-c("Codigo Letra","NCA Tabla","Muestra","k")
    resultados_plan<-c(code_letter,NCA_fix,milstd414_plan$sample,
                                 milstd414_plan$k)
    names(argumentos_plan)<-argumentos_nombres
    names(resultados_plan)<-resultados_nombres
    # Presentar resultados
    print(argumentos_plan)
    print(resultados_plan)
    }
  }
}
