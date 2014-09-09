


\name{transforms}
\alias{C.C.eq}

\title{Various formulas, equations and transforms.}
\description{Computes different formulas.

\code{C.C.eq}: Clapeyron-Clausius equation (saturation evaporation
pressure) where  \code{x} is a data object holding the temperature.
}

\usage{
C.C.eq(x)
}

\arguments{
  \item{x}{a data object}
}

\value{The right hand side of the equation}


\author{R. Benestad, MET Norway}
\examples{
t2m <- t2m.ERAINT(lon=c(-70,-10),lat=c(20,60))
es <- C.C.eq(t2m)
map(es)
}
\keyword{parameter,element}