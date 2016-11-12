\name{k_plans.milstd414}
\alias{k_plans.milstd414}
\docType{data}
\title{
Data: Extract the sample size and k value for MIL STD 414 variable acceptance sampling plans and normal type.
}
\description{
Data for indexing sample size and k value, given the code lette, AQL value and inspection type code.
}
\usage{data("k_plans.milstd414")}
\format{
  A data frame with 432 observations on the following 5 variables.
  \describe{
   \item{\code{code_letter}}{a factor for code letters, levels are B, C, D, E, F, G, H, J, K, L, M, N, P, Q}
    \item{\code{sample}}{a numeric vector for sample size}
    \item{\code{k}}{a numeric vector containing the k value}
    \item{\code{NCA}}{a factor containing the different AQL levels}
    \item{\code{T}}{a character vector for normal inspection }
  }
}
\source{
Montgomery, D.C. (2005) \emph{Introduction to Statistical Quality Control}, 5th ed. New York: John Wiley & Sons, ISBN 0-471-65631-3
}
\examples{
data(k_plans.milstd414)
}
\keyword{datasets}
