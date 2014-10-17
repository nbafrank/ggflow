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
ggflow_plot(flowFrame, x_value, y_value, logx = TRUE, logy = TRUE, x_lim = NA, y_lim = NA, contour = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flowFrame}{
a flowFrame object from flowCore.
}
  \item{x_value}{
a string describing the parameter to be plotted on the x-axis e.g. x_value = "FSC-A"
}
  \item{y_value}{
a string describing the parameter to be plotted on the x-axis e.g. y_value = "SSC-A"
}
  \item{logx}{
this option specify to use log10 scale on the x axis
}
  \item{logy}{
this option specify to use log10 scale on the y axis
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (flowFrame, x_value, y_value, logx = TRUE, logy = TRUE, 
    x_lim = NA, y_lim = NA, contour = TRUE) 
{
    value_matrix <- as.data.frame(exprs(flowFrame))
    x_pos <- match(x_value, colnames(value_matrix))
    y_pos <- match(y_value, colnames(value_matrix))
    ggplot_obj <- NA
    if (all(!is.na(c(x_pos, y_pos)))) {
        input_df <- data.frame(x = value_matrix[, x_pos], y = value_matrix[, 
            y_pos], value_matrix)
        colnames(input_df) <- c("x", "y", colnames(value_matrix))
        input_df$x[which(input_df$x == 0)] <- 1
        input_df$y[which(input_df$y == 0)] <- 1
        ggplot_obj <- ggplot(input_df, aes(x = x, y = y))
        if (logx == TRUE) {
            ggplot_obj <- ggplot_obj + scale_x_log10()
        }
        if (logy == TRUE) {
            ggplot_obj <- ggplot_obj + scale_y_log10()
        }
        ggplot_obj <- ggplot_obj + xlab(x_value) + ylab(y_value) + 
            geom_point(size = 3, colour = "darkgrey", alpha = 0.9) + 
            theme_bw() + theme(axis.title.x = element_text(size = 20), 
            axis.title.y = element_text(size = 20), axis.text.x = element_text(size = 20), 
            axis.text.y = element_text(size = 20))
        if (contour == TRUE) {
            ggplot_obj <- ggplot_obj + stat_density2d(aes(alpha = ..level.., 
                fill = ..level..), contour = TRUE, size = 2, 
                bins = 500, geom = "polygon") + scale_fill_gradient(low = "blue", 
                high = "red") + scale_alpha(range = c(0.03, 0.03), 
                guide = FALSE) + labs(fill = "Density")
        }
        if (all(!is.na(x_lim))) {
            if (x_lim[1] == 0) {
                x_lim[1] <- 1
            }
            ggplot_obj <- ggplot_obj + coord_cartesian(xlim = x_lim)
        }
        if (all(!is.na(y_lim))) {
            if (y_lim[1] == 0) {
                y_lim[1] <- 1
            }
            ggplot_obj <- ggplot_obj + coord_cartesian(ylim = y_lim)
        }
        if (all(!is.na(y_lim)) && all(!is.na(x_lim))) {
            if (x_lim[1] == 0) {
                x_lim[1] <- 1
            }
            if (y_lim[1] == 0) {
                y_lim[1] <- 1
            }
            ggplot_obj <- ggplot_obj + coord_cartesian(xlim = x_lim, 
                ylim = y_lim)
        }
    }
    return(ggplot_obj)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
