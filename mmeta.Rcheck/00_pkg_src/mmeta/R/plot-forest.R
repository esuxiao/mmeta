


forestPlot <- function(specific_summary, 
                       measure, 
                       group_name = 'study_name',
                       xlim = xlim, 
                       add_vertical = add_vertical,
                       show_CI = show_CI){
  
  
  ggplot2_obj <- ggplot(data = specific_summary, aes(y= study_id)) +
    scale_y_continuous(breaks = 1:length(specific_summary$study_name),
                     labels = specific_summary$study_name, 
                     sec.axis = sec_axis(~.,
                                         breaks = 1:length(specific_summary$study_name),
                                         labels = specific_summary$cishow)) +
    geom_pointrange(aes(x=mean, xmin=ET_lower, xmax=ET_upper) ) +
    xlab(measure) +
    ylab('') +
    theme(panel.background = element_blank()) +
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5)) 
  
  if(show_CI == FALSE){
    ggplot2_obj <- ggplot2_obj +
      scale_y_continuous(breaks = 1:length(specific_summary$study_name),
                         labels = specific_summary$study_name) 
                         
    
  }
  
  
  if(!is.null(xlim)){
    ggplot2_obj <- ggplot2_obj + xlim(xlim[1], xlim[2])
  }
  
  if(!is.null(add_vertical)){
    ggplot2_obj <- ggplot2_obj + geom_vline(xintercept = add_vertical )
  }
  return(ggplot2_obj)
}
