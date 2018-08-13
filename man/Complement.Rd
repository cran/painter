\name{Complement}
\alias{Complement}
\alias{ComplementPalette}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate the complement (opposite hue) of a color, or generate a palette from a color and its complement.
}
\description{
Given a color, Complement() maintains the same value and saturation, but returns a color of the opposite hue. ComplementPalette() creates a color palette that ramps between a color and its complement.
}
\usage{
Complement(color)
ComplementPalette(color,n=100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{color}{
A color or (for Complement) possibly a vector of colors
}
  \item{n}{
The number of colors to produce
}
}
\value{
For Complement(), a vector of colors with the same length as color. For ComplementPalette() a vector of n colors
}
\author{
Brody Sandel
}


%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
TestPalette(Complement(terrain.colors(100)))
TestPalette(ComplementPalette("blue"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ color }% use one of  RShowDoc("KEYWORDS")
