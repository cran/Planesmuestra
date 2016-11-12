\name{f_CO.NCA.NCL}
\alias{f_CO.NCA.NCL}
\title{
OC Curve for AOQL and LPTD relation
}
\description{
Given an AOQL, LPTD, sample size and acceptance number, return the plot the OC curve and producer and consumer risk.The calculation uses the binomial trials. Applies for attribute plans.
}
\usage{
f_CO.NCA.NCL(NCA,NCL,n,c)
}
\arguments{
  \item{NCA}{Specific AOQL value}
  \item{NCL}{Specific LPTD value}
  \item{n}{sample size}
  \item{c}{acceptance number}
}
\details{
Functionn stops if any value is missing
}
\value{
  \item{NCA}{Specific AOQL value}
  \item{NCL}{Specific LPTD value}
  \item{n}{sample size}
  \item{c}{acceptance number}
  \item{beta}{consumer risk}
  \item{alpha}{producer risk}
}
\references{
Montgomery, D.C. (2005) \emph{Introduction to Statistical Quality Control}, 5th ed. New York: John Wiley & Sons, ISBN 0-471-65631-3
}
\author{
Erick Marroquin
}
\seealso{
\link{f_dodge.romig.simple}, \code{\link{f_milstd414}},  \code{\link{f_milstd105e}},
\code{\link{f_CO.plan}}, \code{\link{f_DR.CO}}
}
\examples{
f_CO.NCA.NCL(NCA=0.02,NCL=0.1,n=69,c=3)
}
\keyword{OC.curve }
\keyword{risk}
\keyword{alpha}
\keyword{beta}
