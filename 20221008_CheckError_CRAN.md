# Change Requst: Fix check error by CRAN

* create date: 2022-10-8
* complete data: 

## Background


* The mmeta V2.3 package was removed from CRAN. 
* Found the following significant warnings:
     arms-R.c:87:16: warning: a function declaration without a prototype is deprecated in all versions of C [-Wstrict-prototypes]
     arms-R.c:844:16: warning: a function declaration without a prototype is deprecated in all versions of C [-Wstrict-prototypes]

https://github.com/esuxiao/mmeta/blob/13cab625ebbe12d00307929d30a825048fc5d23b/src/arms-R.c#L87
https://github.com/esuxiao/mmeta/blob/13cab625ebbe12d00307929d30a825048fc5d23b/src/arms-R.c#L844


## Requests:
- [x] Remove `display` function
- [x] Resolve the issues caused by `u_random()`



## Related 
