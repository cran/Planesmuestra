f_milstd414.test<-function(x,k,S,Limite,L){
  if(missing(x)){
    stop("Debe de haber una referencia valida de los datos de muestra")
  } else {
    .x<-x[,1]
    med_x<-mean(.x)
  }
  if(missing(S)){
    S<-sd(.x)
    } else {
      S<-S
    }
  if(missing(k)){
    stop("Falta el argumento de corrimiento k")
    } else {
      if(missing(Limite)){
        stop("Falta especificar si es limite superior o inferior, S o I")
      } else {
        if(missing(L)){
          stop("Falta especificar el valor del limite de especificacion")
          } else {
            k_critico<-(L - med_x)/S
            if(Limite=="S"){
              if(k_critico >= k){
                print("Aceptar el lote")
                } else {
                  print("Rechazar el lote")
                }
            } else {
              if(k_critico <= k){
                print("Aceptar el lote")
                } else {
                  print("Rechazar el lote")
                }
            }
          }
      }
    }
}
