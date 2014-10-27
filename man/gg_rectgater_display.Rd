\name{gg_rectgater_display}
\alias{gg_rectgater_display}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Display a rectangular gate object on a a ggflow plot  
}
\description{
This function adds a rectangular gate on top of a ggflow plot object. The gate will also display cell percentage at the bottom of the gate.
}
\usage{
gg_rectgater_display(gg_flow_plot,
                     rectGate = NULL,
                     size_seg = 1,
                     col_seg  = "orange",
                     label    = TRUE,
                     perc     = TRUE,
                     totc     = TRUE,
                     col_lab  = "orangered",
                     type_seg = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gg_flow_plot}{
ggflow plot object
}
  \item{rectGate}{
a rectanglar gate object created with flowCore
}
  \item{size_seg}{
size of the gate lines
}
  \item{col_seg}{
color of the gate lines
}
  \item{label}{
display \% of gated events
}
  \item{col_lab}{
color of the display label
}
  \item{type_seg}{
linetype parameter on the gate lines
}
}

\value{
This function will return a ggflow plot object with an added rectangular gate drawn on top of the plot
}

\author{
Francesco Vallania
}

\examples{
  #load data from flowCore 
  data(GvHD)
  
  #plot data and save it into a variable
  ggplot_obj <- ggflow_plot(GvHD[[1]],x_value="FL4-H",y_value="FL1-H")
  
  #create gate
  rectGate <- rectangleGate("FL1-H"=c(300, 6500),"FL4-H"=c(1.5,30))

  #apply gate to ggflow plot object to create a new plot object
  gg_rectgater_display(ggplot_obj,rectGate)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ggflow}
