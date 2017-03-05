\name{Kettelle}
\alias{Kettelle}

\title{ An Implementation of the General Kettelle Algorithm
 }

\description{
 Determination of the un-dominated allocations of parts at a single echelon.
}

\usage{
Kettelle(x, limit=1e-4, data.name="", performance="FRN", show=FALSE)
}

\arguments{
\item{x}{A dataframe holding part data specific to a Barlow and Proschan example}
\item{limit}{The end point for performance enhancement after incremental addition of parts to stores.}
\item{data.name}{A character string to identify the input, used only for the graphical output title} 
\item{performance}{A character string defining the performance measure for output reporting.  
	any entry other than "Fill Rate", or "EBO" will be handled as "FRN".} 
\item{show}{A logical value controlling graphical output.} 
}


\value{
Returns a dataframe tabulating un-dominated allocations with cumulative performance measure and cost
for stocking replacement parts at a depot (single eschelon).
}

\references{
  Richard E. Barlow and Frank Proschan, (1975) "Statistical Theory of Reliability and Life Testing, Probability Models"
}

\examples{
data(Barlow.Proschan)
allocations<-Kettelle(Barlow.Proschan[1:2,],data.name="Parts 1 and 2")
}

\keyword{ optimal inventory }

