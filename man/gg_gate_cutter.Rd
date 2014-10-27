\name{gg_gate_cutter}
\alias{gg_gate_cutter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gate cells on a ggflow plot  
}
\description{
this function modifies a ggflow object by gating on certain specific values. This function extends and replaces gg_rectgater_cut by allowing multiple type of gates using flowCore's built-in gate operations
}
\usage{
gg_gate_cutter(gg_flow_plot, flowGate = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gg_flow_plot}{
a ggflow plot object 
}
  \item{flowGate}{
a flowCore gate/filter object such as rectangleGate
}
}
\value{
This function will return a new ggflow plot object consisting of the subset of the previous data gated using the gate/filter object.
}

\author{
Francesco Vallania
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
  #load data from flowCore 
  data(GvHD)
  
  #plot data and save it into a variable
  ggplot_obj <- ggflow_plot(GvHD[[1]],x_value="FL4-H",y_value="FL1-H")
  
  #create gate
  rectGate_cut <- rectangleGate("FSC-H"=c(60, 300),"SSC-H"=c(100,1000))
  
  #apply gate to ggflow plot object to create a new plot object
  gg_rectgater_cut(ggplot_obj,rectGate_cut)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ggflow }
\keyword{ filter }
