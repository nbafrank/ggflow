\name{gg_rectgater_display}
\alias{gg_rectgater_display}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Display a rectangular gate object on a a ggflow plot  
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gg_rectgater_display(gg_flow_plot, rectGate = NULL, size_seg = 1, col_seg = "orange", label = TRUE, col_lab = "orangered", type_seg = 2)
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (gg_flow_plot, rectGate = NULL, size_seg = 1, col_seg = "orange", 
    label = TRUE, col_lab = "orangered", type_seg = 2) 
{
    if (!is.null(rectGate)) {
        name_map <- match(c(gg_flow_plot$labels$y, gg_flow_plot$labels$x), 
            names(rectGate@max))
        if (all(!is.na(name_map))) {
            x_min <- rectGate@min[name_map[2]]
            x_max <- rectGate@max[name_map[2]]
            y_min <- rectGate@min[name_map[1]]
            y_max <- rectGate@max[name_map[1]]
            x_select <- intersect(which(gg_flow_plot$data$x >= 
                x_min), which(gg_flow_plot$data$x <= x_max))
            y_select <- intersect(which(gg_flow_plot$data$y >= 
                y_min), which(gg_flow_plot$data$y <= y_max))
            selected_cells <- length(intersect(x_select, y_select))
            selected_perc <- selected_cells/nrow(gg_flow_plot$data)
            gg_flow_plot <- gg_flow_plot + geom_segment(data = data.frame(x = x_min, 
                xend = x_min, y = y_min, yend = y_max), aes(x = x, 
                y = y, xend = xend, yend = yend), size = size_seg, 
                colour = col_seg, linetype = type_seg) + geom_segment(data = data.frame(x = x_min, 
                xend = x_max, y = y_min, yend = y_min), aes(x = x, 
                y = y, xend = xend, yend = yend), size = size_seg, 
                colour = col_seg, linetype = type_seg) + geom_segment(data = data.frame(x = x_max, 
                xend = x_max, y = y_min, yend = y_max), aes(x = x, 
                y = y, xend = xend, yend = yend), size = size_seg, 
                colour = col_seg, linetype = type_seg) + geom_segment(data = data.frame(x = x_min, 
                xend = x_max, y = y_max, yend = y_max), aes(x = x, 
                y = y, xend = xend, yend = yend), size = size_seg, 
                colour = col_seg, linetype = type_seg)
            if (label == TRUE) {
                gg_flow_plot <- gg_flow_plot + annotate("text", 
                  label = paste(round(selected_perc * 100, 2), 
                    "\%", sep = ""), x = x_min, y = y_min, colour = col_lab, 
                  size = 8)
            }
        }
    }
    return(gg_flow_plot)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
