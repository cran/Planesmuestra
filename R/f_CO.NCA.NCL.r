# Calcula una curva de operacion por medio de la distribucion binomial
# Los datos vienen del calculo de un plan de aceptacion calculado
# Agrega las lineas del par NCA NCL
f_CO.NCA.NCL<-function(NCA,NCL,n,c){
  if (missing(NCL)){
    stop("No existe el nivel de Calidad Limite")
  } else {
    if (missing(NCA)){
      stop("No existe el nivel de Calidad Aceptable")
    } else {
      if (missing(n)){
        stop("Tiene que definir la muestra")
      } else {
        if (missing(c)){
          stop("Tiene que defir el numero de aceptacion")
        } else {
          beta.NCL<-pbinom(c,n,NCL)
          alpha.NCA<-pbinom(c,n,NCA)
          prob.x<-seq(0,0.125,by=0.001)
          n.1<-pbinom(c,n,prob.x)
          plot(prob.x, n.1, type = "l", lwd = 2,
               col = 2, cex = 2, bg = "grey",
               xlab = "NCA - NCL", ylab = "Pa",
               xlim = c(0,0.13), ylim = c(0,1),
               main = "CURVA DE OPERACION, Relacion NCA-NCL
                      OC Curve AOQL - LPTD")
          # Agrega opciones de graficas de bajo nivel
          segments(x0=0.0,y0=beta.NCL,x1=NCL,y1=beta.NCL,col="blue",lwd=2)
          segments(x0=NCL,y0=0,x1=NCL,y1=beta.NCL,col="blue",lwd=2)
          segments(x0=0.0,y0=alpha.NCA,x1=NCA,y1=alpha.NCA,col="blue",lwd=2)
          segments(x0=NCA,y0=0,x1=NCA,y1=alpha.NCA,col="blue",lwd=2)
          segments(x0=-0.0,y0=0,x1=0.125,y1=0,col="black",lwd=1)
          segments(x0=-0.0,y0=0,x1=0,y1=1,col="black",lwd=1)
          #Texto beta
          text(NCL*0.95,beta.NCL*1.6, expression(paste(beta)),cex = 1, col="black")
          text(NCL,beta.NCL*1.6, expression(" = "),cex = 1, col="black")
          text(NCL*1.05,beta.NCL*1.6, round(beta.NCL,3),cex = 1, col="black")
          # Texto alfa
          text(NCA*1.3,alpha.NCA, expression(paste(alpha)),cex = 1, col="black")
          text(NCA*1.4,alpha.NCA, expression(" = "),cex = 1, col="black")
          text(NCA*1.55,alpha.NCA, round(1-alpha.NCA,3),cex = 1, col="black")
          grid(10, 10, lwd = 1)
          structure(cbind("c"=c, "n"=n, "NCA"=NCA, "NCL"=NCL,
                          "beta"=beta.NCL, "alpha"= 1-alpha.NCA))
        }
      }
    }
  }
}
