\name{f_milstd105e}
\alias{f_milstd105e}
\title{
Calculate the acceptance sampling for MIL STD 105E / ANSI ASQ C Z 1.4 / ISO 2589 plan
}
\description{
Given lot size,  a type of inspection (Normal, Reduced, Tightened), type of sampling (Simple, double or multiple), and the AQL, show the calculated acceptance plan based in the MIL STD 105e tables. The function is for simple acceptance sample plans only.
}
\usage{
f_milstd105e(N,L,NCA,type)
}
\arguments{
  \item{N}{
Is the lot size, an integer number, must be grater than 2
}
\item{L}{
A character string for inspection level (S-1,S-2,S-3,S-4,I, II, III)
}
\item{NCA}{
A numeric value for the AQL
}
\item{type}{
A character string whith the type of inspection, - n - normal, - r - reduced, in other case is tightened
}
}
\references{
Montgomery, D.C. (2005) \emph{Introduction to Statistical Quality Control}, 5th ed. New York: John Wiley & Sons, ISBN 0-471-65631-3
}
\author{
Erick Marroquin
}
\seealso{
\code{\link{f_DR.CO}}  \code{\link{f_dodge.romig.simple}}  \code{\link{f_milstd414}}
}
\examples{
## L = 1200 , an AQL = 1, level III, tightened inspection
f_milstd105e(N=11000,L="II",type="n",NCA=15)

}
\keyword{plan}
\keyword{attribute}
