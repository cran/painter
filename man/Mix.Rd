\name{Mix}
\alias{Mix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create mixtures of color pairs, in either RGB or HSV mode.
}
\description{
Creates a mixture between pairs of colors by averaging their red/green/blue components (RGB mode), or hue/saturation/value components (HSV mode)
}
\usage{
Mix(color1, color2, mode = "RGB",circular = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{color1}{
A color or vector of colors, either specified by name (e.g. "red") or hex code (e.g. "FF0000")
}
  \item{color2}{
A second color or vector of colors. If color1 and color2 are not the same length, but one is an integer multiple of the other, the shorter one will be recycled.
}
  \item{mode}{
Either "RGB" or "HSV", specifies whether to find the intermediate color in RGB space or HSV space.
}
\item{circular}{
If using mode = "HSV", specifies whether to ramp between hues using circular means. This is usually a good idea because hues are essentially circular (a hue of 0.01 is very similar to 0.99).
}

}
\value{A color}
\author{
Brody Sandel
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
TestPalette(Mix("Red","Yellow"))

TestPalette(c("Red",Mix("Red","Yellow"),"Yellow"))
TestPalette(c("salmon",Mix("salmon","turquoise"),"turquoise"))
TestPalette(c("salmon",Mix("salmon","turquoise",mode = "HSV"),"turquoise"))
TestPalette(Mix(rainbow(10),terrain.colors(10)))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{  color  }% use one of  RShowDoc("KEYWORDS")
