\name{TestPalette}
\alias{TestPalette}
\alias{VisPalette}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tools for seeing a palette, and how it spans HSV space.
}
\description{
TestPalette() simply produces a row of bars of colors, with as many bars as there are elements of the supplied color vector. VisPalette() displays the HSV values of the palette.
}
\usage{
TestPalette(color)
VisPalette(color)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{color}{
A vector of colors.
}
}
\value{
Nothing is returned.
}
\author{
Brody Sandel
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
pal = Palette("Red","Blue",100)
TestPalette(pal)
VisPalette(pal)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ color }% use one of  RShowDoc("KEYWORDS")
