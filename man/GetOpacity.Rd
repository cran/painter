\name{GetOpacity}
\alias{GetOpacity}
\alias{GetHue}
\alias{GetSaturation}
\alias{GetValue}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract the opacity, hue, saturation or value from a color or vector of colors
}
\description{
These functions simply extact information about a given color or vector of colors, given either as names (e.g. "red") or hex codes (e.g. "FF0000")
}
\usage{
GetOpacity(color)
GetHue(color)
GetSaturation(color)
GetValue(color)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{color}{A color or vector of colors}
  }
\value{
Numeric value(s) between 0 and 1, with the same length as color
}
\author{
Brody Sandel
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
GetValue("red")
GetOpacity("blue")
GetHue(rainbow(100))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{  color  }% use one of  RShowDoc("KEYWORDS")
