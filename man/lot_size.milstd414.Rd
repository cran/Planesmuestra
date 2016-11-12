\name{lot_size.milstd414}
\alias{lot_size.milstd414}
\docType{data}
\title{
Data: Lot size levels for MIL STD 414 variable acceptance sampling plans
}
\description{
Interpolate the table lot size level starting from a real lot size.
}
\usage{data("lot_size.milstd414")}
\format{
  A data frame with 17 minimun levels for size lot.
  \describe{
    \item{\code{N}}{A numeric vector containing the minimun level. For lots greater than 550001, the function fixes the lot size as the last one of the "lot_size" data frame.
  }
  }
}
\source{
Montgomery, D.C. (2005) \emph{Introduction to Statistical Quality Control}, 5th ed. New York: John Wiley & Sons, ISBN 0-471-65631-3
}
\examples{
data(lot_size.milstd414)
}
\keyword{datasets}
