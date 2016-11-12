\name{milstd105eplans}
\alias{milstd105eplans}
\docType{data}
\title{
Data: Extract the sample size and the acceptance number for MIL STD 105E acceptance sampling plans.
}
\description{
Data for indexing sample size and acceptance number, given the code lette, AQL value and inspection type code.
}
\usage{data(milstd105eplans)}
\format{
  A data frame with 1274 entries on the following 5 variables.
  \describe{
    \item{\code{code_letter}}{a factor for code letters, levels are A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S}
    \item{\code{n}}{a numeric vector for sample size}
    \item{\code{T}}{a factor for type of inspection, among tightened, reduced or normal, "t", "r", "n" respectively}
    \item{\code{NCA}}{a factor containing the different AQL levels, 26 in total}
    \item{\code{c}}{a numeric vector for acceptance number }
  }
}
\source{
Montgomery, D.C. (2005) \emph{Introduction to Statistical Quality Control}, 5th ed. New York: John Wiley & Sons, ISBN 0-471-65631-3
}
\examples{
data(milstd105eplans)
}
\keyword{datasets}
