# Change Request: Fix check error by CRAN

* Create date: 2022-10-8
* Status: complete
* Complete data:2022-11-7
* Completed by: 
     * Jiajie Chen <jiajie.chen@pennmedicine.upenn.edu > or <chenjiajiemath@gmail.com>

## Reasons
* The mmeta V2.3 package was removed from CRAN. See [email](https://github.com/esuxiao/mmeta/blob/change_request/msg_emails/20221008_email_CRAN.md)
* Found the following significant warnings:
     arms-R.c:87:16: warning: a function declaration without a prototype is deprecated in all versions of C [-Wstrict-prototypes]
     arms-R.c:844:16: warning: a function declaration without a prototype is deprecated in all versions of C [-Wstrict-prototypes]

https://github.com/esuxiao/mmeta/blob/13cab625ebbe12d00307929d30a825048fc5d23b/src/arms-R.c#L87
https://github.com/esuxiao/mmeta/blob/13cab625ebbe12d00307929d30a825048fc5d23b/src/arms-R.c#L844-L854


## Change requests:
- [x] Delete `display` function
- [x] Resolve the issues caused by `u_random()`

## Related tickets


## Deliverables
* Last commit: 156ce8273318172c80c9cea14d2794be67c7824a (Dec, 28, 2022)
* Pass local R CRMD check for
     * windows builder
     * linux builder
     * mac builder
* Re-submit to CRAN on 2022-11-07
     * pass the CRAN automatic RCMD check

## Follow - up
See the follow-up change request forms:
* Received received comments from Victoria Wimmer CRAN. 
* See change request form: `20201107_reviewComments_CRAN`