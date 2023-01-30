Thanks,
 
Please rather use the Authors@R field and declare Maintainer, Authors and Contributors with their appropriate roles with person() calls.
e.g. something like:
Authors@R: c(person("Alice", "Developer", role = c("aut", "cre","cph"),
                      email = "alice.developer@some.domain.net"),
                      person("Bob", "Dev", role = "aut") )
 
The Description field is intended to be a (one paragraph) description of what the package does and why it may be useful. Please add more details about the package functionality and implemented methods in your Description text.
 
If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form authors (year) <doi:...> authors (year) <arXiv:...> authors (year, ISBN:...) or if those are not available: <https:...> with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for auto-linking.
(If you want to add a title as well please put it in quotes: "Title")
 
Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g.
\value{No return value, called for side effects} or similar) Missing Rd-tags:
      plot.multipletables.rd: \value
  	plot.singletable.rd: \value
 
Some code lines in examples are commented out.
Please never do that. Ideally find toy examples that can be regularly executed and checked. Lengthy examples (> 5 sec), can be wrapped in \donttest{}.
Examples in comments in:
   	colorectal.Rd
       multipletables.rd
       plot.multipletables.rd
       summary.multipletables.rd
       summary.singletable.rd
   	withdrawal.Rd
 
You write information messages to the console that cannot be easily suppressed. It is more R like to generate objects that can be used to extract the information a user is interested in, and then print() that object.
Instead of print()/cat() rather use message()/warning()  or
if(verbose)cat(..) (or maybe stop()) if you really have to write text to the console.
(except for print, summary, interactive functions)
 
Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd()). This is not allowed by CRAN policies.
Please omit any default path in writing functions. In your examples/vignettes/tests you can write to tempdir().
e.g.: R/multipleplot.r ; R/singleplot.r
 
Please make sure that you do not change the user's options, par or working directory. If you really have to do so within functions, please ensure with an *immediate* call of on.exit() that the settings are reset when the function is exited. e.g.:
...
oldpar <- par(no.readonly = TRUE)	# code line i
on.exit(par(oldpar))            # code line i + 1 ...
par(mfrow=c(2,2))            # somewhere after ...
 
...
oldwd <- getwd()       	# code line i
on.exit(setwd(oldwd))        # code line i+1 ...
setwd(...)            # somewhere after
...
e.g.: R/multipleplot.r ; R/singleplot.r
If you're not familiar with the function, please check ?on.exit. This function makes it possible to restore options before exiting a function even if the function breaks. Therefore it needs to be called immediately after the option change within a function.
 
Please always add all authors, contributors and copyright holders in the Authors@R field with the appropriate roles.
 From CRAN policies you agreed to:
"The ownership of copyright and intellectual property rights of all components of the package must be clear and unambiguous (including from the authors specification in the DESCRIPTION file). Where code is copied (or derived) from the work of others (including from R itself), care must be taken that any copyright/license statements are preserved and authorship is not misrepresented.
Preferably, an ‘Authors@R’ would be used with ‘ctb’ roles for the authors of such code. Alternatively, the ‘Author’ field should list these authors as contributors.
Where copyrights are held by an entity other than the package authors, this should preferably be indicated via ‘cph’ roles in the ‘Authors@R’
field, or using a ‘Copyright’ field (if necessary referring to an inst/COPYRIGHTS file)."
e.g.: Wally Gilks ; Giovanni Petris
Please explain in the submission comments what you did about this issue.
Please fix and resubmit.
 
Best,
Victoria Wimmer
 
 
Additionally:
Have the issues why your package was archived been fixed?
Please explain this in the submission comments.
