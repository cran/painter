\name{SetOpacity}
\alias{SetOpacity}
\alias{SetHue}
\alias{SetSaturation}
\alias{SetValue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Modify the opacity, hue, saturation or value of color(s)
}
\description{
Change the charactistics of a color or vector of colors
}
\usage{
SetOpacity(color,opacity)
SetHue(color,hue)
SetSaturation(color,saturation)
SetValue(color,value)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{color}{a vector of colors}
  \item{opacity}{a vector of new opacity values}
  \item{hue}{a vector of new hues}
  \item{saturation}{a vector of new saturations}
  \item{value}{a vector of new values}
}
\details{
These functions accept colors specified by name (e.g. "red") or hex codes (e.g. "FF0000"). If the color argument and the other argument both have length n, then each color will be assigned the corresponding new opacity, hue, saturation or value. Otherwise, at least one of the arguments should have length 1, in which case each it will be recycled to length n.
}
\value{
A vector of colors of length n.
}
\author{
Brody Sandel
}
\examples{
TestPalette(SetOpacity("red",seq(0,1,0.02)))
TestPalette(SetHue("red",seq(0,1,0.02)))
TestPalette(SetSaturation("red",seq(0,1,0.02)))
TestPalette(SetValue("red",seq(0,1,0.02)))

x = runif(200)
y = runif(200)
color = SetHue("red",x)
color = SetValue(color,y)
plot(x,y,col = color,pch = 16,cex = 2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{  color  }% use one of  RShowDoc("KEYWORDS")
