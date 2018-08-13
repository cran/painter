

SetOpacity = function(color,opacity)
	{
  RGB = col2rgb(color)
	RGB = rgb(RGB[1,]/255,RGB[2,]/255,RGB[3,]/255,opacity)

	if(length(opacity) > 1 & length(color) > 1 & length(color)!=length(opacity))
		{print("Warning - opacity and color are incompatible lengths")}
	return(RGB)
	}

SetValue = function(color,value)
	{
	HSV = rgb2hsv(col2rgb(color))
	if(length(value > 1) & length(color) == 1)
		{
		HSV = matrix(rep(HSV,length(value)),ncol = length(value))
		HSV[3,] = value
		}
	if(length(value) == 1 | length(value) == length(color))
		{
		HSV[3,] = value
		}
	if(length(value) > 1 & length(color) > 1 & length(color)!=length(value))
		{print("Warning - value and color are incompatible lengths")}
	return(hsv(HSV[1,],HSV[2,],HSV[3,]))
}

SetHue = function(color,hue)
	{
	HSV = rgb2hsv(col2rgb(color))
	if(length(hue > 1) & length(color) == 1)
		{
		HSV = matrix(rep(HSV,length(hue)),ncol = length(hue))
		HSV[1,] = hue
		}
	if(length(hue) == 1 | length(hue) == length(color))
		{
		HSV[1,] = hue
		}
	if(length(hue) > 1 & length(color) > 1 & length(color)!=length(hue))
		{print("Warning - hue and color are incompatible lengths")}
	return(hsv(HSV[1,],HSV[2,],HSV[3,]))
}

SetSaturation = function(color,saturation)
	{
	HSV = rgb2hsv(col2rgb(color))
	if(length(saturation > 1) & length(color) == 1)
		{
		HSV = matrix(rep(HSV,length(saturation)),ncol = length(saturation))
		HSV[2,] = saturation
		}
	if(length(saturation) == 1 | length(saturation) == length(color))
		{
		HSV[2,] = saturation
		}
	if(length(saturation) > 1 & length(color) > 1 & length(color)!=length(saturation))
		{print("Warning - saturation and color are incompatible lengths")}
	return(hsv(HSV[1,],HSV[2,],HSV[3,]))
	}

#ColorBy functions - assign colors based on values, or two sets of values
ColorBy = function(x,palette)
	{
	groups = cut(x,breaks = length(palette),labels = FALSE)
	return(palette[groups])
}

ColorBy2 = function(x,y,palette1, palette2, mode = "RGB")
	{
	groups1 = cut(x,breaks = length(palette1),labels = FALSE)
	groups2 = cut(y,breaks = length(palette2),labels = FALSE)
	out = Mix(palette1[groups1],palette2[groups2],mode)
	return(out)
	}


#"Get" functions - simple functions to pull out the opacity,
#value, hue or saturation of a color
GetOpacity = function(color)
	{
	return(col2rgb(color,alpha = TRUE)[4,]/255)
	}
GetValue = function(color)
	{
	HSV = rgb2hsv(col2rgb(color))
	return(HSV[3,])
	}
GetHue = function(color)
	{
	HSV = rgb2hsv(col2rgb(color))
	return(HSV[1,])
	}
GetSaturation = function(color)
	{
	HSV = rgb2hsv(col2rgb(color))
	return(HSV[2,])
	}

#Invert color (Find the opposite hue)
Complement = function(color)
  {
  return(SetHue(color,(GetHue(color) + 0.5) %% 1))
  }

ComplementPalette = function(color, n = 100)
  {
  return(Palette(color,Complement(color),n))
  }

#Mix function - take the midpoint between colors in RGB or HSV space
Mix = function(color1, color2, mode = "RGB",circular = TRUE)
	{
	if(mode == "RGB")
		{
		if(!(length(color1) == 1 | length(color2) == 1 | length(color1) == length(color2)))
			{
			print("Error - color1 and color2 are incompatible lengths")
			return(NULL)
			}

		if(length(color1) == 1 | length(color2) == 1 | length(color1) == length(color2))
			{
			rgb1 = col2rgb(color1)
			rgb2 = col2rgb(color2)

			if(length(color1) == length(color2))
				{
				out = (rgb1 + rgb2)/2
				out = out/255
				return(rgb(out[1,],out[2,],out[3,]))
				}
			if(length(color1) == 1 & length(color2) > 1)
				{
				rgb1 = matrix(rep(as.vector(rgb1),length(color2)),nrow = 3)
				out = (rgb1 + rgb2)/2
				out = out/255
				return(rgb(out[1,],out[2,],out[3,]))
				}
			if(length(color2) == 1 & length(color1) > 1)
				{
				rgb2 = matrix(rep(as.vector(rgb2),length(color1)),nrow = 3)
				out = (rgb1 + rgb2)/2
				out = out/255
				return(rgb(out[1,],out[2,],out[3,]))
				}
			}
		}
	if(mode == "HSV")
		{
	  if(!(length(color1) == 1 | length(color2) == 1 | length(color1) == length(color2)))
	    {
			print("Error - color1 and color2 are incompatible lengths")
			return(NULL)
			}

	  if(length(color1) == 1 | length(color2) == 1 | length(color1) == length(color2))
	    {
			hsv1 = rgb2hsv(col2rgb(color1))
			hsv2 = rgb2hsv(col2rgb(color2))

			if(length(color1) == length(color2))
				{
				out = (hsv1 + hsv2)/2

				if(circular)
				  {
				  h = c(hsv1[1,],hsv2[1,]) #Calculate the ciricular mean for hues
  				s = mean(sin(h*2*pi))
  				c = mean(cos(h*2*pi))
  				h2 = (atan2(s, c)/(2*pi)) %% 1
    			out[1,]	= h2
				  }
  			return(hsv(out[1,],out[2,],out[3,]))
				}

			if(length(color1) == 1 & length(color2) > 1)
				{
				hsv1 = matrix(rep(as.vector(hsv1),length(color2)),nrow = 3)
				out = (hsv1 + hsv2)/2

				if(circular)
          {
				  h = cbind(hsv1[1,],hsv2[1,]) #Calculate the ciricular mean for hues
  				s = apply(sin(h*2*pi),1,mean)
  				c = apply(cos(h*2*pi),1,mean)
  				h2 = (atan2(s, c)/(2*pi)) %% 1
          out[1,] = h2
          }
				return(hsv(out[1,],out[2,],out[3,]))
				}

			if(length(color2) == 1 & length(color1) > 1)
				{
				hsv2 = matrix(rep(as.vector(hsv2),length(color1)),nrow = 3)
				out = (hsv1 + hsv2)/2

				if(circular)
				  {
				  h = cbind(hsv1[1,],hsv2[1,]) #Calculate the ciricular mean for hues
				  s = apply(sin(h*2*pi),1,mean)
				  c = apply(cos(h*2*pi),1,mean)
				  h2 = (atan2(s, c)/(2*pi)) %% 1
				  out[1,] = h2
				  }

				return(hsv(out[1,],out[2,],out[3,]))
				}
			}
		}
	}

#Functions to define color palettes, starting from one color
#and ending at another
Palette = function(color1,color2,n = 100,mode = "RGB",circular = TRUE)
	{
  if(length(color1) != 1 | length(color2) != 1)
    {
    print("Error - color1 and color2 should both be length 1")
    return(NULL)
    }

  if(mode == "RGB")
		{
		rgb1 = col2rgb(color1)
		rgb2 = col2rgb(color2)
		return(rgb(seq(rgb1[1],rgb2[1],length.out = n),
			seq(rgb1[2],rgb2[2],length.out = n),
			seq(rgb1[3],rgb2[3],length.out = n),maxColorValue = 255))
		}
	if(mode == "HSV")
		{
		hsv1 = rgb2hsv(col2rgb(color1))
		hsv2 = rgb2hsv(col2rgb(color2))

		#Use a cicular ramp for the hue values
		if(circular)
		  {
		  h = cbind(hsv1[1,],hsv2[1,])
  		s = seq(sin(hsv1[1,]*2*pi),sin(hsv2[1,]*2*pi),length.out = n)
  		c = seq(cos(hsv1[1,]*2*pi),cos(hsv2[1,]*2*pi),length.out = n)
  		h2 = (atan2(s, c)/(2*pi)) %% 1
		  }
		if(!circular){h2 = seq(hsv1[1],hsv2[1],length.out = n)}
		return(hsv(h2,
			seq(hsv1[2],hsv2[2],length.out = n),
			seq(hsv1[3],hsv2[3],length.out = n)))
		}
	}

TestPalette = function(color)
	{
	plot(1:length(color),rep(1,length(color)),pch = "|",cex = 10,col = color,axes = FALSE, xlab = "",ylab = "")
	}

VisPalette = function(color)
  {
  par(mfrow = c(3,1),mar = c(2,2,0.5,0.5))
  plot(GetHue(color),pch = 16, cex = 3,ylim = c(0,1),col = color)
  text(1,0.95,"Hue",cex = 2,pos = 4)
  plot(GetSaturation(color),pch = 16, cex = 3,ylim = c(0,1),col = color)
  text(1,0.95,"Saturation",cex = 2,pos = 4)
  plot(GetValue(color),pch = 16, cex = 3,ylim = c(0,1),col = color)
  text(1,0.95,"Value",cex = 2,pos = 4)
  }

# End functions
#####################################################################

