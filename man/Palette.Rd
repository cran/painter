\name{Palette}
\alias{Palette}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generates a color palette (a vector of colors) between two specified colors.
}
\description{
Generates a vector of n colors that ramp between the two specified colors, evenly spaced in either RGB space (mode = "RGB") or HSV space (mode = "HSV")
}
\usage{
Palette(color1, color2, n, mode = "RGB",circular = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{color1}{
A color, either specified by name (e.g. "red") or hex code (e.g. "FF0000")
}
  \item{color2}{
A second color
}
  \item{n}{
The number of colors to produce
}
  \item{mode}{
Either "RGB" or "HSV", specifies whether to ramp between the colors in RGB space or HSV space.
}
  \item{circular}{
If using mode = "HSV", specifies whether to ramp between hues using circular means. This is usually a good idea because hues are essentially circular (a hue of 0.01 is very similar to 0.99), but produces results with a clear break if the span of hues covers more than half of the circle.
}
}
\value{
A vector of n colors.
}
\author{
Brody Sandel
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
TestPalette(Palette("Green","Red",100))
TestPalette(Palette("Green","Red",100,"HSV"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ color }% use one of  RShowDoc("KEYWORDS")
