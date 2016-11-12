\name{f_dodge.romig.simple}
\alias{f_dodge.romig.simple}
\title{
Calculate the acceptance sampling for Dodge Romig method
}
\description{
Starting with a known lot N, and a specific AOQL or LPTD plan, and an average of proportion of defectives or nonconforming items, the plan is calculated, giving the sample size, the acceptance number and the rejection number. The function is for simple acceptance sample plans only.
}
\usage{
f_dodge.romig.simple(N,plan,p)
}
\arguments{
  \item{N}{
Is the lot size, an integer number, must be grater than 2
}
\item{plan}{
A character string for specify the AOQL or LPTD plan
}
\item{p}{
Fraction average of the nonconforming items
}
}
\references{
Montgomery, D.C. (2005) \emph{Introduction to Statistical Quality Control}, 5th ed. New York: John Wiley & Sons, ISBN 0-471-65631-3
}
\author{
Erick Marroquin
}
\seealso{
\code{\link{f_DR.CO}}  \code{\link{f_milstd414}}   \code{\link{f_milstd105e}}
}
\examples{
f_dodge.romig.simple(N=5000,plan="AOQL",p=0.017)
}
\keyword{variable}
\keyword{plan}
