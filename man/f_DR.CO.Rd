\name{f_DR.CO}
\alias{f_DR.CO}
\title{
Plot the OC Curve for a specific acceptance sampling plan
}
\description{
Plot the OC Curve for a specific acceptance plan. Needs the acceptance number \emph{c} , the sample size \emph{n}, and the fraction of the non conforming items \emph{p}. The calculation uses the binomial trials. Applies for attribute plans.
}
\usage{
f_DR.CO(c,n,p)
}
\arguments{
  \item{c}{
An integer number grater than zero, for the acceptance number.
}
\item{n}{
An integer number grater than the acceptance number for the sample size.
}
\item{p}{
Fraction average of the nonconforming items.
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
\seealso{\link{f_dodge.romig.simple}, \code{\link{f_milstd414}},  \code{\link{f_milstd105e}},
\code{\link{f_CO.plan}}
}
\examples{
# n = 125 items, c=2, p =  0.01
f_DR.CO(2,125,0.1)
}
\keyword{OC}
\keyword{beta}
