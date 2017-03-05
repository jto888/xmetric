\name{PLpart}
\alias{PLpart}

\title{ 
pipeline for a part at bases }

\description{
 This is the initial function for calculating the pipeline for parts demand at bases given various stock levels
}

\usage{
PLpart(Lam, t,r,O,TAT,ds,dLam)
}

\arguments{
\item{Lam}{Average annual demand at base.}
\item{t}{The average time to restore (repair time) at base, in years.} 
\item{r}{Probability of repair at base.}
\item{O}{Average order and ship time from depot to base.}
\item{TAT}{Turnaround time at depot.}
\item{ds}{initial stock at depot.}
\item{dLam}{demand at depot.}
}


\value{
Returns a scalar value.
}

\references{
  Craig C. Sherbrooke, (2004) "Optimal Inventory Modeling of Systems, Multi-Echelon Techniques, Second Edition"
}

\examples{
PLz<-PLpart(23.2,0.01,0.5,0.01,0.02531,0,92.8)
}

\keyword{ optimal inventory }

