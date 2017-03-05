\name{EBO}
\alias{EBO}

\title{ 
Estimated Back Orders, poisson }

\description{
 This is a the initial function for calculating the Estimated Back Orders assuming a poisson distribution
 for demand given a an initial stock level (j), demand rate (Lam), and average time to restore (t).
}

\usage{
EBO(j,Lam,t)
}

\arguments{
\item{j}{The initial stock level}
\item{Lam}{The fail rate.}
\item{t}{The average time to restore (repair time).} 
}


\value{
Returns a scalar value for the EBO.
}

\references{
  Craig C. Sherbrooke, (2004) "Optimal Inventory Modeling of Systems, Multi-Echelon Techniques, Second Edition"
}

\examples{
BackOrders<-EBO(0,92.8,0.02531)
}

\keyword{ optimal inventory }

