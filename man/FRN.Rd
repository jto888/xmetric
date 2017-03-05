\name{FRN}
\alias{FRN}

\title{ 
Fill Rate Numerator, poisson }

\description{
 This  function calculates the Fill Rate Numerator per Barlow and Proschan description
 given an initial stock level (j), demand rate (Lam), and average time to restore (t).
}

\usage{
FRN(j,Lam,t) 
}

\arguments{
\item{j}{The initial stock level}
\item{Lam}{The fail rate.}
\item{t}{The average time to restore (repair time).} 
}


\value{
Returns a scalar value for the FRN.
}

\references{
  Richard E. Barlow and Frank Proschan, (1975) "Statistical Theory of Reliability and Life Testing, 
  Probability Models" 
}

\examples{
FillRateNumerator<-FRN(5,.01,100)
}

\keyword{ optimal inventory }

