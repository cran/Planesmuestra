\name{f_milstd414.test}
\alias{f_milstd414.test}
\title{
Accept or reject a variable sample considering a shift factor
}
\description{
Accept or reject a variable sample considering a shift factor, the data comes from an especific variable plan.
}
\usage{
f_milstd414.test(x,k,S,Limite,L)
}
\arguments{
  \item{x}{Vector or data frame containing the taken sample values, the function evaluates only the first column or variable}
  \item{k}{A vector of length one, equal shift factor}
  \item{S}{Know standard deviation, if value not exists, function gives the sample standard deviation}
  \item{Limite}{A character vector of length one, "S" for upper control limit and "I" for lower control limit}
  \item{L}{A vector of length one, equal to a specific Control Limit value}
}
\references{
Montgomery, D.C. (2005) \emph{Introduction to Statistical Quality Control}, 5th ed. New York: John Wiley & Sons, ISBN 0-471-65631-3
}
\author{
Erick Marroquin
}
\seealso{
\code{\link{f_milstd414}}
}
\examples{
x<-c(4.7,5.1,4.9,4.9,4.8,4.9,4.9,4.8,4.8,4.7,4.7,4.9,4.8,4.9,4.6,4.8,4.9,5.1,4.8,5,5,4.7,5,5,4.8)
f_milstd414.test(as.data.frame(x),k=1.98,Limite="S", L=5.1)
f_milstd414.test(as.data.frame(x),k=1.98,Limite="I", L=4.7)
}
\keyword{acceptance}
\keyword{shift}
