\name{gg_rectgater_cut}
\alias{gg_rectgater_cut}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gate cells on a ggflow plot  
}
\description{
this function will gate a ggflow object and regenerate the plot containing only the gated cells/events 
}
\usage{
gg_rectgater_cut(gg_flow_plot, rectGate = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gg_flow_plot}{
%%     ~~Describe \code{gg_flow_plot} here~~
}
  \item{rectGate}{
%%     ~~Describe \code{rectGate} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (gg_flow_plot, rectGate = NULL) 
{
    if (!is.null(rectGate)) {
        name_map <- match(names(rectGate@max), colnames(gg_flow_plot$data))
        if (all(!is.na(name_map))) {
            set_one <- intersect(which(gg_flow_plot$data[, name_map[1]] >= 
                rectGate@min[1]), which(gg_flow_plot$data[, name_map[1]] <= 
                rectGate@max[1]))
            set_two <- intersect(which(gg_flow_plot$data[, name_map[2]] >= 
                rectGate@min[2]), which(gg_flow_plot$data[, name_map[2]] <= 
                rectGate@max[2]))
            gg_flow_plot$data <- gg_flow_plot$data[intersect(set_one, 
                set_two), ]
        }
    }
    return(gg_flow_plot)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
