


#' @useDynLib mmeta
#' @title Create a components list of exact posterior inference based on a single 2 by 2 table.
#' @usage SingleTable.create(a1,b1,a2,b2,rho,y1,n1,y2,n2,model,measure)
#' @param a1 a numeric value specifying the first hyperparameter of the beta prior for group 1.
#' @param b1 a numeric value specifying the second hyperparameter of the beta prior for group 1.
#' @param a2 a numeric value specifying the first hyperparameter of the beta prior for group 2.
#' @param b2 a numeric value specifying the second hyperparameter of the beta prior for group 2.
#' @param rho a numeric value specifying correlation coefficient for Sarmanov bivariate prior distribution.
#' @param y1 an integer indicating the number of events in group 1.
#' @param n1 an integer indicating the total number of subjects in group 1.
#' @param y2 an integer indicating the number of events in group 2.
#' @param n2 an integer indicating the total number of subjects in group 2.
#' @param model a character string specifying the model. Options are \code{Independent} and \code{Sarmanov}. \code{Independent} is
#' independent beta-binomial model. \code{Sarmanov}is Sarmanov beta-binomial model. 
#' @param measure a character string specifying a measure. Options are 
#'   \code{OR}, \code{RR}, and \code{RD}. \code{OR} is odds
#'    ratio, \code{RR} is relative risk, and \code{RD} is risk difference.
#' @details There are two kinds of study design, i.e., prospective study or
#' clinical trial, and retrospective or case-control study. 
#' In a prospective study or clinical trial, \code{data} is a data
#' frame that contains \code{y1}, \code{n1}, \code{y2}, \code{n2},
#' \code{studynames}. \code{y1} is the number of subjects
#' experienced a certain event in the unexposed group. \code{n1} is the number
#' of subjects in the unexposed group. \code{y2} is the number of subjects experienced
#' a certain event in the exposed group. \code{n2} is the number of
#' subjects in the exposed group. In this study, \code{OR} is odds ratio
#' of event comparing exposed group with unexposed group. \code{RR}
#' is relative risk of event comparing exposed group with unexposed group. \code{RD} is risk
#' difference of event comparing exposed group with unexposed group.
#' 
#' For case-control study, \code{y1} is the number of subjects with
#' exposure in the control group. \code{n1} is the number of
#' subjects in the control group. \code{y2} is the number of
#' subjects with exposure in the case group. \code{n2} is the
#' number of subjects in the case group. In this study, \code{OR} is odds ratio
#' of event comparing case group with control group. \code{RR} is
#' relative risk of event comparing case group with control group. \code{RD} is risk
#' difference of event comparing case group with control group.
#' When model='\code{Sarmanov}', \code{rho} is subject to constraints. See Chen et al(2011) for details.
#' @examples 
#' ## Specify data (y1, n1, y2, n2), parameters (a1, b1, a2, b2, rho), model (Sarmanov/Independent),
#' ## and Specify measure(OR/RR/RD)
#' ## Assume we have a 2x2 table:{{40,56},{49,60}} and set prior parameters as a1=b1=a2=b2=rho=0.5. 
#'  \donttest{library(mmeta)
#'  single_table_obj_exact <- SingleTable.create(a1=0.5,b1=0.5, 
#'  a2=0.5,b2=0.5,rho=0.5, y1=40, n1=96, y2=49, n2=109,model="Sarmanov",measure="OR")}
#' @returns a list of components and prior parameters of a single 2 by 2 table.
#' @seealso \code{SingleTable.modelFit}, \code{SingleTable.summary}, \code{SingleTable.plot}.
#' @references Chen, Y., Luo, S., (2011a). A Few Remarks on "Statistical Distribution of the Difference of
#' Two Proportions' by Nadarajah and Kotz, Statistics in Medicine 2007; 26(18):3518-3523". \cr
#' \emph{Statistics in Medicine, 30(15)}, 1913-1915. \cr 
#' <doi:10.1002/sim.4248> \cr
#' @export
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




#' @useDynLib mmeta
#' @title modelFit
#' @export
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


