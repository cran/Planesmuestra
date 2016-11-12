f_dodge.romig.simple<-function(N,plan,p){
  # Encontrar el numero de linea de intervalo dellote
  # Atendiendo al tipo de plan AOQL 0.03 O LPTD 0.01
  if (missing(N)){
    # si no encuentra el lote, para la funcion
    stop("El lote debe ser igual o mayor que 2")
  } else {
    # Filtra las series de lotes de acuerdo al plan declarado
    data(lot_size_DR, envir = environment())
    plan_lot<-lot_size_DR[lot_size_DR$plan==plan,]
    # Si el plan es AOQL
    if (plan == "AOQL"){
      # Compara si el lote existe dentro de los limites superiores de
      # las series filtrada de lotes
      if (any(plan_lot$N == N)){
        # De ser cierto encuentra el intervalo especifico
        lot_interval<-findInterval(N,as.integer(levels(as.factor(plan_lot$N))))
      } else {
        # De lo contrario compara si es mayor a cualquier valor
        # para asignarle el mayor de los limites superiores
        # de lo contrario lo busca y lo ubica en el lugar i + 1
        if (N > max(as.integer(levels(as.factor(plan_lot$N))))){
          lot_interval<-findInterval(N,as.integer(levels(as.factor(plan_lot$N))))
        } else {
          lot_interval<-findInterval(N,as.integer(levels(as.factor(plan_lot$N))))+1
        }
      }
      plan2<-"LPTD"
    } else {
      # Repite el proceso anterior con la variante del tipo de plan
      # Para el proceso si no esta definido cualquiera de los dos planes
      if (plan == "LPTD"){
        if(any(plan_lot$N == N)){
          lot_interval<-findInterval(N,as.integer(levels(as.factor(plan_lot$N))))
        } else {
          if (N > max(as.integer(levels(as.factor(plan_lot$N))))){
            lot_interval<-findInterval(N,as.integer(levels(as.factor(plan_lot$N))))
          } else {
            lot_interval<-findInterval(N,as.integer(levels(as.factor(plan_lot$N))))+1
          }
        }
        plan2<-"AOQL"
      } else {
        stop("Debe de definir plan como AOQL o LPTD")
      }
    }
    # Fija el lote exacto segun las series
    lot_fix<-plan_lot[lot_interval,1]
    #
    # Busca y fija la proporcion de los no conformes de acuerdo a la tabla
    # Al igual que con lote, compara y fija el intervalo correcto
    if (missing(p)){
      stop("Debe definir una proporcion promedio de no conformes")
    } else {
      data(ap_DR, envir = environment())
      if(any(ap_DR[plan] == p)){
        p_interval<-findInterval(p,ap_DR[,plan])
      } else {
        if (p > max(ap_DR[,plan])){
          p_interval<-findInterval(p,ap_DR[,plan])
        } else {
          p_interval<-findInterval(p,ap_DR[,plan])+1
        }
      }
      # con el intervalo se fija el valor p de la tabla
      # encuentra la serie de valores con p de tabla
      # y el lote de la interpolacion
      p_fix<-ap_DR[p_interval,plan]
      plan_lot_p_fix<-plan_lot[plan_lot$p==p_fix,]
      plan_lot_n<-plan_lot_p_fix[plan_lot_p_fix$N==lot_fix,]
    }
    # Presentar resultados
    structure(list("plan" = c("n"=eval(as.vector(plan_lot_n$n)),
                              "c"= eval(as.vector(plan_lot_n$c)),
                              "p"= eval(p_fix)),
                   "Argumentos" = c("Lote" = as.integer(N),"Tipo de plan" = plan,
                                    "Porcentaje Promedio de no conformes"= p_fix*100),
                   "Resultados" = c("Muestra" =as.vector(plan_lot_n$n),
                                    "Numero de Aceptacion" = as.vector(plan_lot_n$c),
                                    "Numero Rechazo" = as.vector(plan_lot_n$c)+1,
                                    "plan2" = c(plan2,as.vector(plan_lot_n$LPTD._AOQL)))))
  }
}
