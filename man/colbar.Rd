\name{col.bar}
\alias{colbar}

\title{Continuous or discrete colored bar or colored point}

\description{Display a colored bar into a map or a plot. There are two
  types that can be displayed: "r"ectangular or "p"oints. The colored
  bar could be continuous (v=0) or discrete (v>0) while the colored points
  are always discrete. The symbols of the colored points could be any "pch"
  symbol from the graphical parameter.}

\usage{

col.bar(breaks, horiz=TRUE, pch=21, v=1, h=1, col=col, cex=2,
cex.lab=0.81, type="r", verbose=FALSE, vl=0.5, border=FALSE, ...)

}
\arguments{
  \item{breaks}{A vector giving the breakpoints between color bars or points,
    An esd object to be mapped}
  \item{horiz}{Not tested !}
  \item{pch}{A graphical parameter. See \code{\link{select.station}}}
  \item{type}{Whether the color bar is rectangular (type="r") or points ("p").}
  \item{n.breaks}{An integer giving the _desired_ number of intervals. \code{\link{pretty}}}
  \item{h}{Horizontal space between the color bar and the upper axis. \code{\link{colbar}}}
  \item{v}{Vertical space to be used between the colored components.}
  \item{border}{Logical value. If TRUE, the border around the bars or
    the points are displyed. The default is set to FALSE}
  \item{col}{A specification for the default plotting color. Rainbow
    colors are used as defaults.}
  \item{cex.lab}{A numerical value giving the magnification to be used for labels.}
  \item{verbose}{Display progress details.}
  \item{...}{Additional graphical parameters. See \code{\link{par}}}
}

\value{A colbar object}

\seealso{
  \code{\link{plot}},\code{\link{map}}
}

\author{A. Mezghani}
\examples{

# Map the mean precipitation values for a selection of
  stations across Norway with a minimum of 120 years of recorded daily
  data.
  y <- station.metnod(nmin=120,param="precip")
  map(y, cex=1.5, FUN="mean", colbar=list(n.breaks=30,type="r",h=0.6,v=0,
  cex.lab=0.4), showaxis=TRUE, border=FALSE, cex.axis=0.6, add.text=FALSE)
}
\keyword{map,plot}
