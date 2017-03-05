\name{Sherbrooke.METRIC}
\alias{Sherbrooke.METRIC}
\docType{data}
\title{
    Part data for METRIC, two eschelon, example with one part
}
\description{
    Dataset of one parts handled at five bases with one depot
}
\usage{Sherbrooke.METRIC}
\format{
    An dataframe with 5 rows, used as unsorted with columns \code{$Base} \code{$LRU}
    (Line Replaceable Unit), \code{$bLam} (Demand rate for spare part at base), \code{$bfT} (Repair time at base),
	\code{$Pbr} (Probability of base repair), \code{$transp} (transport time to depot),
	\code{$dTAT} (TurnAround Time at depot), and \code{C} (Purchase cost part)	
}
\source{
      Craig C. Sherbrooke, (2004) "Optimal Inventory Modeling of Systems, Multi-Echelon Techniques, Second Edition"
   }
%\references{
%  Craig C. Sherbrooke, (2004) "Optimal Inventory Modeling of Systems, Multi-Echelon Techniques, Second Edition"
%}
\keyword{datasets}