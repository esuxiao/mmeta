# Change Request: address review comment from  Victoria Wimmer from CRAN. 

* Create date: 2022-12-02
* Status: complete
* Complete data:2022-12-13
* Completed by: 
     * Jiajie Chen <jiajie.chen@pennmedicine.upenn.edu > or <chenjiajiemath@gmail.com>

## Reasons
Jiajie Chen re-submissted the V2.4 to CRAN after addressing the issues raised by Victoria Wimmer. The reviewer  Beni Altmann  required to resolved additional issues before the package uploaded to CRAN.  See the [email](https://github.com/esuxiao/mmeta/blob/change_request/msg_emails/20221202_email_CRAN.md)

## Change requests:


- [x] put references in the description

- [x] Author@R' -> 'Authors@R
- [x] Ensure that we do not change the user's options, par or working directory. Use on.exit() function before change par. Add the following lines of code at the beginning of the functions. 
```
<plot_function> <- function(){
   oldpar <- par(no.readonly = TRUE) # code line i
   on.exit(par(oldpar)) # code line i + 1
   …
   return()
}

```

- [x] Please always make sure to reset to user's options(), working directory or par() after you changed it in examples and vignettes and demos.

```
oldpar <- par(no.readonly = TRUE)
<plot function call, such as 
plot(single.OR.Jeffreys, type="overlap", xlim=c(0.5, 2),
    main="Jefferys Prior")
>
par(oldpar)
```


## Related tickets


## Deliverables
* Last commit:  a28014db3138f152ca263058f364a657ac15bb9f (Dec, 29, 2022)
* Merge pull requst to main: 24f4e5b17619c2a87b75336128dda05c4716a5e7(Dec, 29, 2022)
* Pass local R CRMD check for
     * windows builder
     * linux builder
     * mac builder
* Pass CRAN R CMD check
* Back on CRAN at 2022-12-13 as `mmeta V2.4`

