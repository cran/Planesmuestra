\name{NCA_values}
\alias{NCA_values}
\docType{data}
\title{
Data: AQL levels for MIL STD 105E acceptance sampling plans.
}
\description{
Contains the AQL level values for sample size and acceptance number. The row order is the sames as the code letter, previously determined.
}
\usage{data("NCA_values")}
\format{

  \describe{
    \item{\code{NCA_values}}{a numeric vector containing 26 AQL levels}
  }
}
\source{
Montgomery, D.C. (2005) \emph{Introduction to Statistical Quality Control}, 5th ed. New York: John Wiley & Sons, ISBN 0-471-65631-3
}
\examples{
data(NCA_values)
## NCA values is the same for AQL values
}
\keyword{datasets}
