#######################################################################################
#######################################################################################
#GGFLOW
#copyright by Francesco Vallania
#2014/10/27
#######################################################################################
#######################################################################################

#######################################################################################
#R Functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#######################################################################################

require(flowCore);
require(ggplot2);

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ggflow_plot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#This function plots a flowFrame into a scatter plot. This object is refered to as 
#a "ggflow" object
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggflow_plot <- function(flowFrame,
                        x_value = "SSC-H",
                        y_value = "FSC-H",
                        logx    = TRUE,
                        logy    = TRUE,
                        color_v = c("blue","red"),
                        x_lim   = NA,
                        y_lim   = NA,
                        contour = TRUE){
  #extract data
  value_matrix <- as.data.frame(exprs(flowFrame));
  
  #match positions 
  x_pos <- match(x_value,colnames(value_matrix));
  y_pos <- match(y_value,colnames(value_matrix));
  
  #Create default entry for GGPLOT object
  ggplot_obj <- NA;
  
  #if both matches proceed
  if(all(!is.na(c(x_pos,y_pos)))){
    #create input data.frame
    input_df <- data.frame(x=value_matrix[,x_pos],
                           y=value_matrix[,y_pos],
                           value_matrix);
    
    #adjsut names of data.frame
    colnames(input_df) <- c('x','y',colnames(value_matrix));
    
    #remove 0 values [change this into-> turn 0s into 1s]
    input_df$x[which(input_df$x==0)] <- 1;
    input_df$y[which(input_df$y==0)] <- 1;
    
    #create ggplot object
    ggplot_obj <- ggplot(input_df,aes(x=x,y=y));
    
    #introduce log scale if necessary
    if(logx==TRUE){
      ggplot_obj <- ggplot_obj + scale_x_log10();
    }
    if(logy==TRUE){
      ggplot_obj <- ggplot_obj + scale_y_log10();
    }
    #
    ggplot_obj <- ggplot_obj                    + 
      xlab(x_value)   + ylab(y_value)           + 
      geom_point(size =3,
                 colour="darkgrey",
                 alpha=0.9)                     +
      theme_bw()                                +
      theme(axis.title.x = element_text(size=20),
            axis.title.y = element_text(size=20),
            axis.text.x  = element_text(size=20),
            axis.text.y  = element_text(size=20));
    
    if(contour==TRUE){
      ggplot_obj <- ggplot_obj                  +
        stat_density2d(aes(alpha=..level..,
                           fill =..level..),
                       contour=TRUE,
                       size=2,
                       bins=500,
                       geom="polygon")          +
        scale_fill_gradientn(colours=color_v)   +
        scale_alpha(range = c(0.03, 0.03),
                    guide = FALSE)              +
        labs(fill="Density")
    }
    
    #specificy the 
    if(all(!is.na(x_lim))){
      #make sure there is enough value
      if(x_lim[1]==0){x_lim[1] <- 1}
      
      ggplot_obj <- ggplot_obj +
        coord_cartesian(xlim = x_lim); 
    }
    if(all(!is.na(y_lim))){
      #make sure there is enough value
      if(y_lim[1]==0){y_lim[1] <- 1}
      
      ggplot_obj <- ggplot_obj +
        coord_cartesian(ylim = y_lim); 
    }
    if(all(!is.na(y_lim)) && all(!is.na(x_lim))){
      #make sure there is enough value
      if(x_lim[1]==0){x_lim[1] <- 1}
      if(y_lim[1]==0){y_lim[1] <- 1}
      
      ggplot_obj <- ggplot_obj +
        coord_cartesian(xlim = x_lim,
                        ylim = y_lim); 
    } 
  }
  
  #add the flowFrame obj into the ggplot_obj
  ggplot_obj$flowFrame <- flowFrame;
  
  #add options 
  ggplot_obj$inputopts <- list(x_value = x_value,
                               y_value = y_value,
                               logx    = logx,
                               logy    = logy,
                               color_v = color_v,
                               x_lim   = x_lim,
                               y_lim   = y_lim,
                               contour = contour);
  
  #return object
  return(ggplot_obj);
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#gg_rectgater_display
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#this function displays a gate on a ggflow object
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gg_rectgater_display <- function(gg_flow_plot,
                                 rectGate = NULL,
                                 size_seg = 1,
                                 col_seg  = "orange",
                                 label    = TRUE,
                                 perc     = TRUE,
                                 totc     = TRUE,
                                 col_lab  = "orangered",
                                 type_seg = 2){
  
  if(!is.null(rectGate)){
    
    #check if gate dimensions are even in the plot
    name_map <- match(c(gg_flow_plot$labels$y,gg_flow_plot$labels$x),
                      names(rectGate@max));
    
    #if both x and y match add gate
    if(all(!is.na(name_map))){
      
      #define min and max coordinates
      x_min <- rectGate@min[name_map[2]];
      x_max <- rectGate@max[name_map[2]];
      y_min <- rectGate@min[name_map[1]];
      y_max <- rectGate@max[name_map[1]];
      
      #Calculate numbers [for x and y]
      x_select <- intersect(which(gg_flow_plot$data$x>=x_min),
                            which(gg_flow_plot$data$x<=x_max));
      y_select <- intersect(which(gg_flow_plot$data$y>=y_min),
                            which(gg_flow_plot$data$y<=y_max));
      
      #identify events selected by gating
      selected_cells <- length(intersect(x_select,y_select));
      selected_perc  <- selected_cells/nrow(gg_flow_plot$data);
      
      #Add gate as segments
      gg_flow_plot <- gg_flow_plot + 
        geom_segment(data=data.frame(x    = x_min,
                                     xend = x_min,
                                     y    = y_min,
                                     yend = y_max),
                     aes(x=x,y=y,xend=xend,yend=yend),
                     size     = size_seg,
                     colour   = col_seg,
                     linetype = type_seg)+
        geom_segment(data=data.frame(x    = x_min,
                                     xend = x_max,
                                     y    = y_min,
                                     yend = y_min),
                     aes(x=x,y=y,xend=xend,yend=yend),
                     size     = size_seg,
                     colour   = col_seg,
                     linetype = type_seg)+
        geom_segment(data=data.frame(x    = x_max,
                                     xend = x_max,
                                     y    = y_min,
                                     yend = y_max),
                     aes(x=x,y=y,xend=xend,yend=yend),
                     size     = size_seg,
                     colour   = col_seg,
                     linetype = type_seg)+
        geom_segment(data=data.frame(x    = x_min,
                                     xend = x_max,
                                     y    = y_max,
                                     yend = y_max),
                     aes(x=x,y=y,xend=xend,yend=yend),
                     size     = size_seg,
                     colour   = col_seg,
                     linetype = type_seg);
      
      #display percentage cells
      if(label==TRUE){
        #percentage
        if(perc==TRUE){
        gg_flow_plot <- gg_flow_plot + 
          annotate("text",
                   label=paste(round(selected_perc*100,2),"%",sep=""),
                   x=x_min,
                   y=y_min,
                   colour= col_lab,
                   size=8);
        }
        #total count
        if(totc==TRUE){
          gg_flow_plot <- gg_flow_plot + 
            annotate("text",
                     label=selected_cells,
                     x=x_max,
                     y=y_min,
                     colour= col_lab,
                     size=8);
        }
      }
    }
  }
  
  #return ggplot object
  return(gg_flow_plot);
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#gg_gate_cutter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#this function modifies a ggflow object by gating on certain specific values. This 
#function extends and replaces gg_rectgater_cut by allowing multiple type of gates
#using flowCore's built-in gate operations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gg_gate_cutter <- function(gg_flow_plot,
                           flowGate=NULL){
  
  #check if gate dimensions are even in the plot
  if(!is.null(flowGate)){
    #redefine flowframe object
    newflowFrame <- Subset(gg_flow_plot$flowFrame,
                           filter(gg_flow_plot$flowFrame,
                                  flowGate));
    #replot using new flowFrame
    gg_flow_plot <- ggflow_plot(newflowFrame,
                                x_value = ggplot_obj$inputopts$x_value,
                                y_value = ggplot_obj$inputopts$y_value,
                                logx    = ggplot_obj$inputopts$logx,
                                logy    = ggplot_obj$inputopts$logy,
                                color_v = ggplot_obj$inputopts$color_v,
                                x_lim   = ggplot_obj$inputopts$x_lim,
                                y_lim   = ggplot_obj$inputopts$y_lim,
                                contour = ggplot_obj$inputopts$contour)
  }
  
  #return ggplot object
  return(gg_flow_plot);
}
