\name{f_milstd414}
\alias{f_milstd414}
\title{
Calculate the acceptance sampling for MIL STD 414 / ANSI ASQ C Z 1.9 / ISO 3951 plan
}
\description{
Given lot size, an inspection level, a type of inspection and the NCA, show the calculated acceptance plan based in the MIL STD 414 tables.
}
\usage{
f_milstd414(N,L,NCA,type)
}
\arguments{
  \item{N}{
Is the lot size, an integer number, must be grater than 2
}
\item{L}{
A character string for inspection level (I,II,III,IV,V)
}
\item{NCA}{
A numeric value for the NCA
}
\item{type}{
Type of inspection, - n - normal, - t - tightened
}
}
\details{
The master table of MIL STD 414 for plans based in variables, contains the values for both type of inspection.
}
\references{
Montgomery, D.C. (2005) \emph{Introduction to Statistical Quality Control}, 5th ed. New York: John Wiley & Sons, ISBN 0-471-65631-3
}
\author{
Erick Marroquin
}
\seealso{
\code{\link{f_DR.CO}},  \code{\link{f_dodge.romig.simple}},  \code{\link{f_milstd105e}} ,   \code{\link{f_milstd414.test}}
}
\examples{
## L = 1200, NCA = 1, level III, tightened inspection
##
f_milstd414(N=1200,NCA=1,L="III",type="t")
}
\keyword{variable}
\keyword{acceptance}
