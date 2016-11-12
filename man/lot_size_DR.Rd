\name{lot_size_DR}
\alias{lot_size_DR}
\docType{data}
\title{
Data: Lot size for Dodge Romig acceptance sampling plan
}
\description{
Shows the results for a given lot size, AOQL or LPTD plan and a fraction of non conforming items. The results are: the acceptance number - n -, the rejection number - c -, and the corresponding AOQL - LPTD percentage.
}
\usage{data("lot_size_DR")}
\format{
  A data frame with 222 observations on the following 6 variables.
  \describe{
    \item{\code{N}}{a numeric vector whith the interpolated lot}
    \item{\code{plan}}{a factor with two levels, the AOQL and the LPTD plan.}
    \item{\code{p}}{a character vector whith six levels, for each AOQL and the LPTD plan.}
    \item{\code{n}}{a numeric vector for the sample size for a specific acceptance plan.}
    \item{\code{c}}{a numeric vector for the acceptance number for a specific acceptance plan.}
    \item{\code{LPTD._AOQL}}{a numeric vector for the LPTD or AOQL index.}
  }
}
\source{
Montgomery, D.C. (2005) \emph{Introduction to Statistical Quality Control}, 5th ed. New York: John Wiley & Sons, ISBN 0-471-65631-3
}
\examples{
data(lot_size_DR)
## maybe str(lot_size_DR) ; plot(lot_size_DR) ...
}
\keyword{datasets}
