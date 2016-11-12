\name{f_CO.plan}
\alias{f_CO.plan}
\title{
Plot the OC Curve for a specific Dodge Romig acceptance sampling plan results
}
\description{
Plot the OC Curve for a specific acceptance plan. The function need the plan defined in a previous function. The calculation uses the binomial trials. Applies for attribute plans.
}
\usage{
f_CO.plan(plan)
}
\arguments{
  \item{plan}{
A vector with acceptance number \emph{c} , the sample size \emph{n}, and the fraction of the non conforming items \emph{p}.
}
}
\value{
\item{c}{
An integer number grater than zero, for the acceptance number.
}
\item{n}{
An integer number grater than the acceptance number for the sample size.
}
\item{p}{
Fraction average of the nonconforming items.
}
\item{beta}{
Acceptance probability.
}
}
\references{
Montgomery, D.C. (2005) \emph{Introduction to Statistical Quality Control}, 5th ed. New York: John Wiley & Sons, ISBN 0-471-65631-3
}
\author{
Erick Marroquin
}
\seealso{\link{f_dodge.romig.simple}, \code{\link{f_milstd414}},  \code{\link{f_milstd105e}}, \code{\link{f_DR.CO}}
}
\examples{
r1<-f_dodge.romig.simple(N=2500,"AOQL", p=0.01)
f_CO.plan(r1$plan)
}
\keyword{OC}
\keyword{beta}
