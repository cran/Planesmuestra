# Calcula una curva de operacion por medio de la distribucion binomial
# Los datos vienen del calculo de un plan de aceptacion calculado
f_CO.plan<-function(plan){
  if (missing(plan)){
    stop("No existe el vector que especifica el plan")
  }
  c<-as.numeric(plan["c"])
  n<-as.numeric(plan["n"])
  p<-as.numeric(plan["p"])
  beta.x<-pbinom(c,n,p)
  prob.x<-seq(0,0.1,by=0.001)
  n.1<-pbinom(c,n,prob.x)
  plot(prob.x, n.1, type = "l", lwd = 2,
       col = 2, cex = 2, bg = NA,
       xlab = "p", ylab = expression(1- alpha),
       xlim = c(0,0.1), ylim = c(0,1),
       main = "CURVA DE OPERACION
       OC Curve")
  # Agrega opciones de graficas de bajo nivel
  segments(x0=0.0,y0=beta.x,x1=p,y1=beta.x,col="blue",lwd=2)
  segments(x0=p,y0=0.0,x1=p,y1=beta.x,col="blue",lwd=2)
  segments(x0=-0.0,y0=0,x1=0.1,y1=0,col="black",lwd=1)
  segments(x0=-0.0,y0=0,x1=0,y1=1,col="black",lwd=1)
  text(0.07,0.9, expression(paste(beta)),cex = 1, col="black")
  text(0.075,0.9, expression(" = "),cex = 1, col="black")
  text(0.09,0.9, round(beta.x,3),cex = 1, col="black")
  grid(10, 10, lwd = 1)
  structure(cbind("c"=c, "n"=n, "p"=p, "beta"=beta.x))
  }
