\name{METRIC1}
\alias{METRIC1}

\title{ 
Initial METRIC algorithm }

\description{
 This is a the initial function for calculating the optimal distribution of parts to depot and bases.
}

\usage{
METRIC1(x, convergence_limit=0.015, show=FALSE)
}

\arguments{
\item{x}{A dataframe holding part data at bases conforming with METRIC1 input.}
\item{convergence_limit}{The end point for EBO reduction after incremental addition of parts to stores at depot or bases.}
\item{show}{A logical value controlling graphical output.} 
}


\value{
Returns a dataframe tabulating selections for the optimal distribution of stock to depot and bases at various total Cost
resulting in an overall EBO performance measure.
}

\references{
  Craig C. Sherbrooke, (2004) "Optimal Inventory Modeling of Systems, Multi-Echelon Techniques, Second Edition"
}

\examples{
data(Sherbrooke.METRIC)
Selections<-METRIC1(Sherbrooke.METRIC)

}

\keyword{ optimal inventory }

