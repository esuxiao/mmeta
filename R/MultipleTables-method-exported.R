

MultipleTables.create<- function(data=NULL, 
                                 measure=NULL, 
                                 model= NULL){
  ## sanity check
  checkModelArgument(model)
  checkMeasureArgument(measure)
  checkMultipleTablesData(data)
  
  ### create object
  multiple_tables_object <- list(
              data = data,
              measure=measure,
              model=model,
              alpha=NULL,
              chi2_value = NULL,
              p_value = NULL,
              prior_mle = NULL,
              hessian_log = NULL,
              cov_matrix_log = NULL,
              overall_measure_estimation = NULL,
              specific_summary = NULL,
              samples= NULL,
              density= NULL
  )

  attr(multiple_tables_object, "class") <- "MultipleTables"
  invisible(multiple_tables_object)
  
}


MultipleTables.modelFit<- function(multiple_tables_object,
                                   method = 'exact',
                                   verbose = FALSE, 
                                   control =  list()){
  
  ## check input
  if (!inherits(multiple_tables_object, "MultipleTables"))
    stop("Use only with 'MultipleTables' objects.\n")
  
  multiple_tables_object$method <- checkMethodArgument(method, 
                                        multiple_tables_object$measure, 
                                        verbose = verbose)
  
  
  
  ### load control list
  control <- MultipleTables._setControlList(multiple_tables_object, control)
  
  
  
  ## Estimate prior parameters and do likelihood ratio test for correlation
  prior_estimate_and_test <- MultipleTables._priorParemetersEstimate(multiple_tables_object, control, verbose)
  multiple_tables_object$chi2_value <- prior_estimate_and_test$chi2_value
  multiple_tables_object$p_value <- prior_estimate_and_test$p_value
  multiple_tables_object$prior_mle <- prior_estimate_and_test$prior_mle
  multiple_tables_object$cov_matrix_log <- prior_estimate_and_test$cov_matrix_log
  multiple_tables_object$hessian_log <- prior_estimate_and_test$hessian_log
  
  

  ## draw posterior samples 
  multiple_tables_object$samples <- MultipleTables._studySpecificPosteriorSamples(multiple_tables_object, control)
  
  # calculate density  for study specific measures
  multiple_tables_object$density <- MultipleTables._studySpecificPosteriorDensity(multiple_tables_object, control)
  
  
  invisible(multiple_tables_object)
  
}


MultipleTables.summary<- function(multiple_tables_object, 
                                  alpha = 0.05,
                                  verbose = TRUE, 
                                  digit = 3,
                                  control = list() ){
  ## check input
  checkAlpha(alpha)
  checkDigit(digit)
  MultipleTables._isObjectFitted(multiple_tables_object)
  
  ## update object
  multiple_tables_object$alpha <- alpha
  multiple_tables_object$digit <- digit
  
  ### load control list
  control <- MultipleTables._setControlList(multiple_tables_object, control)
  
  
  ## Estimate overall measure(point and Wald confident internal)
  overall_measure_estimate <- MultipleTables._overallMeasureEstimate(multiple_tables_object)
  overall_measure_estimation <- list(point = overall_measure_estimate$overall,
                                    confindent_interval = overall_measure_estimate$CI)
  multiple_tables_object$overall_measure_estimation <- overall_measure_estimation
  
  
  ## Estimate study specific summary
  multiple_tables_object$specific_summary <- MultipleTables._StudySpecificMeasureEstimate(
                                              multiple_tables_object, 
                                              control)
  
  ## print out report if user requests
  if(verbose){
    MultipleTables._summaryPrint(multiple_tables_object)
  }
  
  
  invisible(multiple_tables_object)
}


MultipleTables.plot <- function(multiple_tables_object,
                                plot_type = 'forest',
                                layout_type = 'overlay',
                                selected_study_names = NULL,
                                xlim = NULL, 
                                add_vertical = NULL,
                                show_CI = TRUE,
                                by = 'line_type'){
  ## check input
  MultipleTables._isObjectFitted(multiple_tables_object)
  checkPlotType(plot_type)
  checkPlotLayoutType(layout_type)
  checkPlotBy(by)
  select_study_names <- checkSelectStudy(selected_study_names, multiple_tables_object)
  
  
  if(plot_type == 'density'){
    
    if(layout_type == 'overlay'){
      ggplot2_obj <- MultipleTables._plotDensityOverlay(multiple_tables_object,
                                         select_study_names, 
                                         xlim = xlim,
                                         add_vertical,
                                         by = by)
    
      
    }
    
    if(layout_type == 'side_by_side'){
      ggplot2_obj <- MultipleTables._plotDensitySideBySide(multiple_tables_object,
                                                        select_study_names, 
                                                        xlim = xlim,
                                                        add_vertical)
    }
    
  }
  
  if(plot_type == 'forest'){
    ggplot2_obj <- MultipleTables._plotForest(multiple_tables_object,
                                                         select_study_names, 
                                                         xlim = xlim,
                                                         add_vertical,
                                                         show_CI = show_CI)
  }
  
  
  return(ggplot2_obj)
}


