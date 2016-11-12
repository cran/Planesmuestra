\name{ap_DR}
\alias{ap_DR}
\docType{data}
\title{
Data: Dodge Romig table of Nonconforming fraction levels for AOQL and LPTD plans
}
\description{Contains the different maximum non conforming fractions of AQL and LTPD plan, according Dodge Romig plans. A data frame with six maximum levels of the nonconforming fraction for each AOQL and LPTD plan.
}
\usage{data("ap_DR")}
\format{
  A data frame with 6 observations on the following 2 plans.
  \describe{
    \item{\code{AOQL}}{a numeric vector containing the nonconforming fraction level for AOQL plan}
    \item{\code{LPTD}}{a numeric vector containing the nonconforming fraction level for LPTD plan}
  }
}
\source{
Montgomery, D.C. (2005) \emph{Introduction to Statistical Quality Control}, 5th ed. New York: John Wiley & Sons, ISBN 0-471-65631-3
}
\examples{
data(ap_DR)
}
\keyword{datasets}
