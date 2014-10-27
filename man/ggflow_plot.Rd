\name{ggflow_plot}
\alias{ggflow_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
2D scatter plots of a flowFrame object with ggplot2 
}
\description{
this function will read a flowFrame object from flowCore and plot a 2-D scatter plot of values for single cell data. 
}
\usage{
ggflow_plot(flowFrame,
            x_value = "SSC-H",
            y_value = "FSC-H",
            logx    = TRUE,
            logy    = TRUE,
            color_v = c("blue","red"),
            x_lim   = NA,
            y_lim   = NA,
            contour = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flowFrame}{
a flowFrame object from flowCore.
}
  \item{x_value}{
a string describing the parameter to be plotted on the x-axis e.g. x_value = "SSC-H"
}
  \item{y_value}{
a string describing the parameter to be plotted on the x-axis e.g. y_value = "FSC-H"
}
  \item{color_v}{
this option specifies the density gradient color scale and defaults to low blue and high red 
}
  \item{logx}{
this option specifies to use log10 scale on the x axis
}
  \item{logy}{
this option specifies to use log10 scale on the y axis
}
  \item{x_lim}{
a vector with min and max values displayed on the x-axis 
}
  \item{y_lim}{
a vector with min and max values displayed on the y-axis 
}
  \item{contour}{
if TRUE (default), it will print out a contour plot around the dots 
}
}

\value{
This function returns a ggplot2 object. It can be used in input with the other functions in this package.
}

\author{
Francesco Vallania
}

\examples{
  #load data from flowCore 
  data(GvHD)
  
  #plot data
  ggflow_plot(GvHD[[1]],x_value="FL4-H",y_value="FL1-H")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ggflow }
