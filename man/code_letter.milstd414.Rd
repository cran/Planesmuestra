\name{code_letter.milstd414}
\alias{code_letter.milstd414}
\docType{data}
\title{
Data: Inspection level and the code letter for a MIL STD 414 acceptance sampling plan and normal inspection.
}
\description{
Contains the unique code letter for a specific size lot, interpolated through the f_milstd105E function, and specificl normal or special inspection level.
}
\usage{data("code_letter.milstd414")}
\format{
  A data frame with 0 observations on the following 2 variables.
  \describe{
     \item{\code{I}}{a character vector with the code letters, for the I inspection level}
     \item{\code{II}}{a character vector with the code letters, for the II inspection level}
     \item{\code{III}}{a character vector with the code letters, for the III inspection level}
     \item{\code{IV}}{a character vector with the code letters, for the IV inspection level}
     \item{\code{V}}{a character vector with the code letters, for the V inspection level}
  }
}
\source{
Montgomery, D.C. (2005) \emph{Introduction to Statistical Quality Control}, 5th ed. New York: John Wiley & Sons, ISBN 0-471-65631-3
}
\examples{
data(code_letter.milstd414)
}
\keyword{datasets}
