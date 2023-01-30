# Change Request: address review comment from  Victoria Wimmer from CRAN. 

* Create date: 2022-11-7
* Status: complete
* Complete data:2022-12-02
* Completed by: 
     * Jiajie Chen <jiajie.chen@pennmedicine.upenn.edu > or <chenjiajiemath@gmail.com>

## Reasons
Jiajie Chen re-submissted the V2.4 to CRAN. The reviewer Victoria Wimmer required the resolved several issues before the package uploaded to CRAN.  See the [email](https://github.com/esuxiao/mmeta/blob/change_request/msg_emails/20221107_email_CRAN.md)

## Change requests:

- [x]  use the Authors@R field and declare Maintainer, Authors and Contributors with their appropriate roles with person() calls. 

- [x]  And add add authors of HI package .

- [x]  And add add authors of HI package .

- [x] Copyrights —---- add authors of arms-R.c as ctb. 

- [x]  Add more details about the package functionality and implemented methods

- [x] add references in the description field of the DESCRIPTION file in the form:

1. authors (year) <doi:...>
2. authors (year) <arXiv:...>
3. authors (year, ISBN:...) 

- [x]  Add `\value` to .Rd files regarding exported methods and explain the functions results in the documentation. Write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g.  `\value{No return value, called for side effects}` or similar)

- [x] You write information messages to the console that cannot be easily suppressed. It is more R like to generate objects that can be used to extract the information a user is interested in, and then print() that object.Instead of print()/cat() rather use message()/warning()  or if(verbose)cat(..) (or maybe stop()) if you really have to write text to the console.(except for print, summary, interactive functions) 

- [x] Omit any default path in writing functions.  examples/vignettes/tests you can write to tempdir(). e.g.: R/multipleplot.r ; R/singleplot.r   Do not change the user's options, par or working directory. If you really have to do so within functions, please ensure with an *immediate* call of on.exit() that the settings are reset when the function is exited. e.g.:R/multipleplot.r ; R/singleplot.r


## Related tickets


## Deliverables
* Last commit:  2058d73ec059b4b90c638df69cf13c94d77dde1a (Dec, 29, 2022)
* Pass local R CRMD check for
     * windows builder
     * linux builder
     * mac builder
* Re-submit to CRAN on 
     * pass the CRAN automatic RCMD check

## Follow - up
See the follow-up change request forms:
* Received received comments from Benjamin Altmann CRAN. 
* See change request form: `20221203_ReviewComments_CRAN`