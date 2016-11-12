\name{code_letter}
\alias{code_letter}
\docType{data}
\title{
Data: Inspection level and the code letter for a MIL STD 105E acceptance sampling plan.
}
\description{
Contains the unique code letter for a specific size lot, interpolated through the f_milstd105E function, and specificl normal or special inspection level.
}
\usage{data("code_letter")}
\format{
  A data frame with 0 observations on the following 2 variables.
  \describe{
    \item{\code{S.1}}{a character vector with the code letters, for the S.1 special inspection level}
     \item{\code{S.2}}{a character vector with the code letters, for the S.2 special inspection level}
     \item{\code{S.3}}{a character vector with the code letters, for the S.3 special inspection level}
     \item{\code{S.4}}{a character vector with the code letters, for the S.4 special inspection level}
     \item{\code{I}}{a character vector with the code letters, for the I normal inspection level}
     \item{\code{II}}{a character vector with the code letters, for the II normal inspection level}
     \item{\code{III}}{a character vector with the code letters, for the III normal inspection level}
  }
}
\source{
Montgomery, D.C. (2005) \emph{Introduction to Statistical Quality Control}, 5th ed. New York: John Wiley & Sons, ISBN 0-471-65631-3
}
\examples{
data(code_letter)
}
\keyword{datasets}
