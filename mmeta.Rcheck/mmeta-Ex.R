pkgname <- "mmeta"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "mmeta-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('mmeta')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("MultipleTables.create")
### * MultipleTables.create

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: MultipleTables.create
### Title: Create an object of class 'MultipleTables'.
### Aliases: MultipleTables.create

### ** Examples

## No test: 
 library(mmeta)
 library(ggplot2)
 ## Analyze the dataset colorectal to conduct exact inference of the odds ratios
 data(colorectal)
 colorectal['study_name'] <- colorectal['studynames']
 multiple_tables_obj <- MultipleTables.create(data=colorectal, measure='OR', model= 'Sarmanov')
 
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("MultipleTables.create", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("MultipleTables.modelFit")
### * MultipleTables.modelFit

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: MultipleTables.modelFit
### Title: Exact posterior inference based on multiple 2x2 tables.
### Aliases: MultipleTables.modelFit

### ** Examples

## No test: 
 library(mmeta)
 library(ggplot2)
 ## Analyze the dataset colorectal to conduct exact inference of the odds ratios
 data(colorectal)
 colorectal['study_name'] <- colorectal['studynames']
 # ########################## If exact method is used ############################
 ## Create object multiple_tables_obj_exact
 multiple_tables_obj_exact <- MultipleTables.create(data=colorectal,
 measure='OR', model= 'Sarmanov')
 ## Model fit default
 multiple_tables_obj_exact <- MultipleTables.modelFit(
 multiple_tables_obj_exact, method = 'exact')
 ## Options for Control; If set number of posterior samples is 5000
 multiple_tables_obj_exact <- MultipleTables.modelFit(multiple_tables_obj_exact, method = 'exact',
 control = list(n_samples = 3000))
 ## If set intial values correspoinding to c(a1, b1, a2, b2, rho) as c(1,1,1,1,0):
 multiple_tables_obj_exact <- MultipleTables.modelFit(multiple_tables_obj_exact, method = 'exact',
 control = list(initial_values = c(1,1,1,1,0)))
 ## If maximum number of iterations for iteration is 100
 multiple_tables_obj_exact <- MultipleTables.modelFit(multiple_tables_obj_exact, method = 'exact',
 control = list(maxit = 100))
 ## If maximum number of iterations for iteration is 100 and number of posterior samples as 3000
 multiple_tables_obj_exact <- MultipleTables.modelFit(multiple_tables_obj_exact, method = 'exact',
 control = list(maxit = 100, nsamples = 3000))
 # ########################## If sampling method is used ############################
 multiple_tables_obj_sampling <- MultipleTables.create(data=colorectal,
 measure='OR', model= 'Sarmanov')
 multiple_tables_obj_sampling <- MultipleTables.modelFit(
 multiple_tables_obj_sampling, method = 'sampling')
 ## The options of \code{control} list specifying the fitting process are similar
 ## to the codes shown above.
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("MultipleTables.modelFit", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("MultipleTables.plot")
### * MultipleTables.plot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: MultipleTables.plot
### Title: Plot Method for 'Multipletables' objects
### Aliases: MultipleTables.plot

### ** Examples

## No test: 
 library(mmeta)
 library(ggplot2)
 ## Analyze the dataset colorectal to conduct exact inference of the odds ratios
 data(colorectal)
 colorectal['study_name'] <- colorectal['studynames']
 ## If exact method is used, the codes for sampling method are similar.
 ## Create object multiple_tables_obj_exact
 multiple_tables_obj_exact <- MultipleTables.create(data=colorectal,
  measure='OR', model= 'Sarmanov')
 ## Model fit default
 multiple_tables_obj_exact <- MultipleTables.modelFit(multiple_tables_obj_exact, method = 'exact')
 ## Density plot, overlay
 ## Note: There are no enough types of line, if we have too many densities!
 MultipleTables.plot(multiple_tables_obj_exact, plot_type = 'density',
 layout_type = 'overlay')
 ## Choose Set by = ‘color’
 MultipleTables.plot(multiple_tables_obj_exact, plot_type = 'density',
 layout_type = 'overlay',by = 'color')
 ## Set by = ‘color’ and specify xlim as 0 to 5.
 MultipleTables.plot(multiple_tables_obj_exact, plot_type = 'density',
 layout_type = 'overlay', by = 'color', xlim = c(0,5))
 ## Set by = ‘color’ and specify xlim as 0 to 5 and add vertical line at OR = 1
 MultipleTables.plot(multiple_tables_obj_exact, plot_type = 'density',
 layout_type = 'overlay', by = 'color',xlim = c(0,5), add_vertical = 1)
 ## If select three studies
 MultipleTables.plot(multiple_tables_obj_exact, plot_type = 'density',
 layout_type = 'overlay',selected_study_names = c('Bell','Chen','Oda'), xlim = c(0,5))
 ## We can add external layouts for the return ggplot2 : * xlab as Odds ratio
 ggplot2_obj <- MultipleTables.plot(multiple_tables_obj_exact,
 plot_type = 'density', layout_type = 'overlay', by = 'color',xlim = c(0,5))
 ggplot2_obj + xlab('Odds Ratio')  + ggtitle('OR ration for XX cancer')
 ## density plot, plot side by side
 MultipleTables.plot(multiple_tables_obj_exact, plot_type = 'density',
 layout_type = 'side_by_side')
 ## Forest plot (default)
 MultipleTables.plot(multiple_tables_obj_exact, plot_type = 'forest')
 ## forest plot: not show the CIs
 MultipleTables.plot(multiple_tables_obj_exact, plot_type = 'forest',
 add_vertical =  1, show_CI = FALSE)
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("MultipleTables.plot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("MultipleTables.summary")
### * MultipleTables.summary

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: MultipleTables.summary
### Title: Summarize the object of class 'MultipleTables'.
### Aliases: MultipleTables.summary

### ** Examples

## No test: 
 library(mmeta)
 library(ggplot2)
 ## Analyze the dataset colorectal to conduct exact inference of the odds ratios
 data(colorectal)
 colorectal['study_name'] <- colorectal['studynames']
 ## If exact method is used, the codes for sampling method are similar.
 ## Create object multiple_tables_obj_exact
 multiple_tables_obj_exact <- MultipleTables.create(data=colorectal,
 measure='OR', model= 'Sarmanov')
 ## Model fit default
 multiple_tables_obj_exact <- MultipleTables.modelFit(multiple_tables_obj_exact, method = 'exact')
 ## Summary of the fitting process (default)
 multiple_tables_obj_exact <- MultipleTables.summary(multiple_tables_obj_exact)
 ## Structure of SingleTable object
 str(multiple_tables_obj_exact)
 ## If set alpha level to 0.1
 multiple_tables_obj_exact <- MultipleTables.summary(multiple_tables_obj_exact, alpha = 0.1)
 ## If set digit to 4
 multiple_tables_obj_exact <- MultipleTables.summary(multiple_tables_obj_exact,
 alpha = 0.05, digit = 4)
 ## If decided not to print out
 multiple_tables_obj_exact <- MultipleTables.summary(multiple_tables_obj_exact,
 alpha = 0.05, digit = 4,verbose = FALSE)
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("MultipleTables.summary", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("SingleTable.create")
### * SingleTable.create

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: SingleTable.create
### Title: Create an object of class 'singletable'.
### Aliases: SingleTable.create

### ** Examples

## Specify data (y1, n1, y2, n2), parameters (a1, b1, a2, b2, rho), model (Sarmanov/Independent),
## and Specify measure(OR/RR/RD)
## Assume we have a 2x2 table:{{40,56},{49,60}} and set prior parameters as a1=b1=a2=b2=rho=0.5.
## Create object \code{single_table_obj}
 ## No test: 
 library(mmeta)
 library(ggplot2)
 single_table_obj <- SingleTable.create(a1=0.5,b1=0.5,
 a2=0.5,b2=0.5,rho=0.5, y1=40, n1=96, y2=49, n2=109,model="Sarmanov",measure="OR")
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("SingleTable.create", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("SingleTable.modelFit")
### * SingleTable.modelFit

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: SingleTable.modelFit
### Title: Exact posterior inference based on a single 2x2 table
### Aliases: SingleTable.modelFit

### ** Examples

## Assume we have a 2x2 table:{{40,56},{49,60}} and set prior parameters as a1=b1=a2=b2=rho=0.5.
 ## No test: 
 library(mmeta)
 library(ggplot2)
 # ########################## If sampling method is used ############################
 ## Create object \code{single_table_obj_samling}
 single_table_obj_samling <- SingleTable.create(a1=0.5,b1=0.5,
 a2=0.5,b2=0.5,rho=0.5, y1=40, n1=96, y2=49, n2=109,model="Sarmanov",measure="OR")
 ## model fit
 single_table_obj_samling <- SingleTable.modelFit(single_table_obj_samling,
 method = 'sampling')
 ## Control list option examples
 ## set number of posterior samples as 3000 (default is 5000)
 single_table_obj_samling <- SingleTable.modelFit(single_table_obj_samling,
 method = 'sampling', control = list(n_sample = 3000))
 ## set initial values for MCMC is c(0.2, 0,4) (default is c(0.5,0.5))
 single_table_obj_samling <- SingleTable.modelFit(single_table_obj_samling,
 method = 'sampling', control = list(mcmc_initial = c(0.2,0.4)))
 ## set upper bound for the measure is 20( default is 100)
 single_table_obj_samling <- SingleTable.modelFit(single_table_obj_samling,
 method = 'sampling', control = list(upper_bound = 20))
 # ########################### If exact method is used ##############################
 ## Create object \code{single_table_obj_exact}
 single_table_obj_exact <- SingleTable.create(a1=0.5, b1=0.5, a2=0.5, b2=0.5,
 rho=0.5, y1=40, n1=96, y2=49, n2=109, model="Sarmanov",measure="OR")
 ## model fit
 single_table_obj_exact <- SingleTable.modelFit(single_table_obj_exact, method = 'exact')
 ## The options of \code{control} list specifying the fitting process are similar
 ## to the codes shown above.
 
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("SingleTable.modelFit", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("SingleTable.plot")
### * SingleTable.plot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: SingleTable.plot
### Title: Plot Method for 'singletable' objects.
### Aliases: SingleTable.plot

### ** Examples

## Assume we have a 2x2 table:{{40,56},{49,60}} and set prior parameters as a1=b1=a2=b2=rho=0.5.
 ## No test: 
 library(mmeta)
 library(ggplot2)
 ## If exact method is used, the codes for sampling method are similar.
 ## Create object \code{single_table_obj_exact}
 single_table_obj_exact <- SingleTable.create(a1=0.5,b1=0.5,
 a2=0.5,b2=0.5,rho=0.5, y1=40, n1=96, y2=49, n2=109,model="Sarmanov",measure="OR")
 ## model fit
 single_table_obj_exact <- SingleTable.modelFit(single_table_obj_exact, method = 'exact')
 ## Summary of the fitting process (default)
 single_table_obj_exact <- SingleTable.summary(single_table_obj_exact, alpha = 0.05)
 ## Plot the densities side-by-side
 SingleTable.plot(single_table_obj_exact, type = 'side_by_side')
 ## set xlim between 0 to 4 and add vertical line at x = 1
 SingleTable.plot(single_table_obj_exact, type = 'side_by_side',
 xlim = c(0,4), add_vertical = 1)
 ## override xlab and add title via ggplot2
 plot_obj <- SingleTable.plot(single_table_obj_exact, type = 'side_by_side',
 xlim = c(0,4), add_vertical = 1)
 plot_obj + xlab('Odds ratio') + ggtitle("Plot of density function")
 ## Overlay plot the density
 SingleTable.plot(single_table_obj_exact, type = 'overlay')
 ## Plot by color instead of line type
 SingleTable.plot(single_table_obj_exact, type = 'overlay',by = 'color')
 
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("SingleTable.plot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("SingleTable.summary")
### * SingleTable.summary

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: SingleTable.summary
### Title: Summarize the object of class 'singletable'.
### Aliases: SingleTable.summary

### ** Examples

## Assume we have a 2x2 table:{{40,56},{49,60}} and set prior parameters as a1=b1=a2=b2=rho=0.5.
 ## No test: 
 library(mmeta)
 library(ggplot2)
 ## If exact method is used, the codes for sampling method are similar.
 ## Create object \code{single_table_obj_exact}
 single_table_obj_exact <- SingleTable.create(a1=0.5,b1=0.5,
 a2=0.5,b2=0.5,rho=0.5, y1=40, n1=96, y2=49, n2=109,model="Sarmanov",measure="OR")
 ## model fit
 single_table_obj_exact <- SingleTable.modelFit(single_table_obj_exact, method = 'exact')
 ## Summary of the fitting process (default)
 single_table_obj_exact <- SingleTable.summary(single_table_obj_exact, alpha = 0.05)
 ## Structure of SingleTable object
 str(single_table_obj_exact)
 ## If set alpha level to 0.1
 single_table_obj_exact <- SingleTable.summary(single_table_obj_exact, alpha = 0.1)
 ## If set digit to 2
 single_table_obj_exact <- SingleTable.summary(single_table_obj_exact, digit  = 2)
 ## If decided not to print out
 single_table_obj_exact <- SingleTable.summary(single_table_obj_exact, verbose = FALSE)
 
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("SingleTable.summary", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("colorectal")
### * colorectal

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: colorectal
### Title: Studies on the Association of N-acetyltransferase 2 (NAT2)
###   Acetylation Status and Colorectal Cancer
### Aliases: colorectal colorectal
### Keywords: datasets

### ** Examples

## No test: 
library(mmeta)
data(colorectal)
summary(colorectal)
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("colorectal", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("diabetes")
### * diabetes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: diabetes
### Title: Studies on the Association of Gestational Diabetes Mellitus
###   (GDM) and Type 2 Diabetes Mellitus (T2DM)
### Aliases: diabetes diabetes
### Keywords: datasets

### ** Examples

## No test: 
library(mmeta)
data(diabetes)
summary(diabetes)
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("diabetes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("withdrawal")
### * withdrawal

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: withdrawal
### Title: Studies on the association of withdrawal from study due to
###   adverse events and tricyclic treatment
### Aliases: withdrawal withdrawal
### Keywords: datasets

### ** Examples

## No test: 
library(mmeta)
data(withdrawal)
summary(withdrawal)
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("withdrawal", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
