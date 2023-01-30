From: Beni Altmann <benjamin.altmann@wu.ac.at>
Date: Friday, December 2, 2022 at 11:26 AM
To: Chen, Jiajie <Jiajie.Chen@Pennmedicine.upenn.edu>, CRAN <cran-submissions@r-project.org>
Subject: [External] Re: CRAN Submission mmeta 2.4

Thanks,

Please also add references describing the methods in your package, in
the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
auto-linking. (If you want to add a title as well please put it in
quotes: "Title")

Unknown, possibly misspelled, fields in DESCRIPTION:
     'Author@R'
Should be 'Authors@R' instead.

Please make sure that you do not change the user's options, par or
working directory. If you really have to do so within functions, please
ensure with an *immediate* call of on.exit() that the settings are reset
when the function is exited.
e.g.:
...
oldpar <- par(no.readonly = TRUE) # code line i
on.exit(par(oldpar)) # code line i + 1
...
par(mfrow=c(2,2)) # somewhere after
...

e.g.: -> R/multipleplot.r;  R/singleplot.r
If you're not familiar with the function, please check ?on.exit. This
function makes it possible to restore options before exiting a function
even if the function breaks. Therefore it needs to be called immediately
after the option change within a function.

Please always make sure to reset to user's options(), working directory
or par() after you changed it in examples and vignettes and demos. -> 
man/plot.singletable.rd; man/singletable.Rd; man/summary.singletable.rd
e.g.:
oldpar <- par(mfrow = c(1,2))
...
par(oldpar)

Please fix and resubmit.

Best,
Benjamin Altmann

On 01.12.2022 20:48, CRAN Package Submission Form wrote:
> [This was generated from CRAN.R-project.org/submit.html]
>
> The following package was uploaded to CRAN:
> ===========================================
>
> Package Information:
> Package: mmeta
> Version: 2.4
> Title: Multivariate Meta-Analysis
> Author(s): Sheng Luo [aut], Yong Chen [aut], Xiao Su [aut], Haitao Chu
>    [aut], Jiajie Chen [cre], Wally Gillks [ctb], Giovanni Petris
>    [ctb], Luca Tardella [ctb]
> Maintainer: Jiajie Chen <jiajie.chen@pennmedicine.upenn.edu>
> Depends: R (>= 2.10.0)
> Suggests: testthat (>= 3.0.0)
> Description: Multiple 2 by 2 tables often arise in meta-analysis which
>    combines statistical evidence from multiple studies. Two
>    risks within the same study are possibly correlated because
>    they share some common factors such as environment and
>    population structure. This package implements a set of novel
>    Bayesian approaches for multivariate meta analysis when the
>    risks within the same study are independent or correlated.
>    The exact posterior inference of odds ratio, relative risk,
>    and risk difference given either a single 2 by 2 table or
>    multiple 2 by 2 tables is provided.
> License: GPL (>= 2)
> Imports: aod
>
>
> The maintainer confirms that he or she
> has read and agrees to the CRAN policies.
>
> Submitter's comment: ## R CMD check results
>
> 0 errors | 0 warnings | 1
>    note
>
> * This is a new release.
> * The reported note is
>    related to an old version (2.3) was archived. These
>    issues in archived version were related to
>    outdated/expired syntaxes in "arms-R.c", and are
>    resolved/fixed in version 2.4.
>
> Comments regarding
>    CRAN manual review:
>
> 1. Applied the Authors@R field
>    and declare Maintainer, Authors and Contributors with
>    person() calls
> 2. Wally Gilks and Giovanni Petris
>    have been add as 'ctb'.
> 3. "\value{}" have been added
>    to plot.singletable.rd and plot.multipletable.rd.
> 4.
>    References have been listed using "\references{}"" in
>    each function's documentation files and reference
>    links are attached.
> 5. A more detailed description
>    about the package functionality and implemented
>    methods has been added in the Description area.
> 6.
>    Example codes are uncommented. For lengthy examples
>    (> 5 sec), they are wrapped in "\donttest{}".
> 7. In
>    summary.r, cat()/print() callings are wrapped in
>    if(verbose). When "verbose=FALSE", the info messages
>    are suppressed. For other functions cat()/print()
>    callings are replaced by message()/stop().
> 8.
>    “file=” arguments are removed from both
>    multipleplot.r and singleplot.r. Now the functions do
>    not write by default or in
> examples/vignettes/tests
>    in the user's home filespace.
>
> *** All the
>    questions/concerns/problems suggested by CRAN manual
>    review are resolved/fixed. ***
>
>  
>
> =================================================
>
> Original content of DESCRIPTION file:
>
> Package: mmeta
> Type: Package
> Title: Multivariate Meta-Analysis
> Version: 2.4
> Date: 2022-11-30
> Author@R: c(person("Sheng", "Luo", role="aut"), person("Yong", "Chen",
>          role ="aut", email = "ychen123@pennmedicine.upenn.edu" ),
>          person("Xiao", "Su", role = "aut"), person("Haitao", "Chu",
>          role = "aut"), person("Jiajie", "Chen", role = "cre", email =
>          "jiajie.chen@pennmedicine.upenn.edu"), peroson("Wally",
>          "Gilks", role = "ctb"), person("Giovanni", "Petris", role =
>          "ctb"), person("Luca", "Tardella", role = "ctb"))
> Maintainer: Jiajie Chen <jiajie.chen@pennmedicine.upenn.edu>
> Description: Multiple 2 by 2 tables often arise in meta-analysis which combines statistical evidence from multiple studies. Two risks within the same study are possibly correlated because they share some common factors such as environment and population structure. This package implements a set of novel Bayesian approaches for multivariate meta analysis when the risks within the same study are independent or correlated.  The exact posterior inference of odds ratio, relative risk, and risk difference given either a single 2 by 2 table or multiple 2 by 2 tables is provided.
> Depends: R (>= 2.10.0)
> Imports: aod
> License: GPL (>= 2)
> LazyLoad: no
> Packaged: 2022-12-01 19:47:14 UTC; thech
> NeedsCompilation: yes
> Author: Sheng Luo [aut],
>    Yong Chen [aut],
>    Xiao Su [aut],
>    Haitao Chu [aut],
>    Jiajie Chen [cre],
>    Wally Gillks [ctb],
>    Giovanni Petris [ctb],
>    Luca Tardella [ctb]
> Repository: CRAN
> Date/Publication: 2017-03-28 06:00:09 UTC
> Suggests: testthat (>= 3.0.0)
> Config/testthat/edition: 3
>