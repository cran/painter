\name{ColorBy}
\alias{ColorBy}
\alias{ColorBy2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Assign colors using one or two numeric vectors
}
\description{
These functions are intended to be used to color plotting symbols according to some numeric values or pair of numeric values associated with each point
}
\usage{
ColorBy(x, palette)
ColorBy2(x,y,palette1, palette2, mode = "RGB")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A vector of numeric values to color the points by}
  \item{y}{A vector of numeric values to color the points by, must be of the same length as x}
  \item{palette}{A vector of colors}
  \item{palette1}{A vector of colors}
  \item{palette2}{A vector of colors}
  \item{mode}{Specifies whether color mixtures should be in "RGB" or "HSV" mode}
}
\value{
A vector of colors of the same length as x
}
\author{
Brody Sandel
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
x = runif(100)
y = runif(100)

colors = ColorBy(x,rainbow(100))
plot(x,y,col=colors,pch=16,cex=2)

colors = ColorBy2(x,y,SetSaturation("Red",seq(0,1,0.1)),SetSaturation("Blue",seq(0,1,0.1)))
plot(x,y,col=colors,pch=16,cex=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{  color  }% use one of  RShowDoc("KEYWORDS")
