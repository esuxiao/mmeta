


SingleTable.create <- function(a1 = NULL, b1= NULL, 
                               a2 = NULL, b2 = NULL, 
                               rho = NULL, 
                               y1 = NULL, n1 = NULL, 
                               y2 = NULL, n2 = NULL,
                               model = 'Sarmanov', 
                               measure = measure){
  ## check input
  checkModelArgument(model)
  checkMeasureArgument(measure)
  checkModelParameters(a1 = a1, b1= b1, a2 = a2, b2 = b2, rho = rho, model = model)
  checkSingleTableData(y1 = y1, n1 = n1, y2 = y2, n2 = n2)

  ## create object
  single_table_object <- list(
    parms_prior = list(a1 = a1, b1= b1, a2 = a2, b2 = b2, rho = rho),
    data = list(y1 = y1, n1 = n1, y2 = y2, n2 = n2),
    model = model,
    measure = measure,
    method = NULL,
    density = NULL,
    samples = NULL,
    alpha = NULL,
    summary = NULL
  )
  attr(single_table_object, "class") <- "SingleTable"
  
  
  invisible(single_table_object)
  
}




SingleTable.modelFit <- function(single_table_Obj, 
                                 method = 'exact',
                            verbose = TRUE, 
                            control = list()){
  
  if (!inherits(single_table_Obj, "SingleTable"))
    stop("Use only with 'SingleTable' objects.\n")
  
  ## check input
  single_table_Obj$method <- checkMethodArgument(method, single_table_Obj$measure, verbose = verbose)
  



  ## load control list
  control <- SingleTable._setControlList(single_table_Obj, control)

  
  ## draw MCMC samples and calculate the density
  single_table_Obj$samples <- SingleTable._sampleGen(single_table_Obj, control)
  single_table_Obj$density <- SingleTable._densityCal(single_table_Obj, control)
  invisible(single_table_Obj)
}







SingleTable.summary <- function(single_table_Obj,
                                alpha = 0.05, 
                                verbose = TRUE, 
                                digit = 3,
                                control = list()){
  ## check input
  checkAlpha(alpha)
  SingleTable._isObjectFitted(single_table_Obj)
  checkDigit(digit)

  
  ## load control list
  control <- SingleTable._setControlList(single_table_Obj, control)
  
  # update single_table_Obj  
  single_table_Obj$alpha <- alpha
  single_table_Obj$digit <- digit 

  ## calculate summary
  if(single_table_Obj$method == 'sampling'){
    single_table_Obj$summary <- SingleTable._summaryCalSampling(single_table_Obj, control)
  }
  if(single_table_Obj$method == 'exact'){
    single_table_Obj$summary <- SingleTable._summaryCalExact(single_table_Obj, control)
  }
  
  ## print summary if user requests
  if(verbose){
    SingleTable._summaryPrint(single_table_Obj)
  }
  
  invisible(single_table_Obj)
}







SingleTable.plot <- function(single_table_Obj, 
                             type = 'side_by_side',
                             xlim = NULL, 
                             add_vertical = NULL,
                             by = 'line_type'){
  ## check input
  checkPlotLayoutType(type)
  checkPlotBy(by)
  SingleTable._isObjectFitted(single_table_Obj)

  
  if(type == 'side_by_side')
    return(SingleTable._plotSideBySide(single_table_Obj, 
                                  xlim = xlim, 
                                  add_vertical = add_vertical))
  
  if(type == 'overlay'){
    return(SingleTable._plotOverlay(single_table_Obj, 
                                    xlim = xlim, 
                                    add_vertical = add_vertical,
                                    by = by))
  }
}


