.th SAVE QUEL 3/10/77
.sh NAME
save \- save a relation until a date.
.sh SYNOPSIS
.bd save
relname 
.bd until
month day year
.sh DESCRIPTION
.it Save 
is used to keep relations beyond the default 7 day
life span.
.s3
.it Month 
can be an integer from 1 through
12, or the name of the month,
either abbreviated or spelled out.
.s3
Only the owner of a relation can 
.it save 
that relation.
There is an \*(II process which typically
removes a relation immediately after
its expiration date has passed.
.s3
The actual program which destroys relations is called
purge.
It is not automatically run.
It is a local decision when expired
relations are removed.
.s3
System relations have no expiration date.
.sh EXAMPLE
.nf
/* Save the emp relation until the end of February 1987 */
   save emp until feb 28 1987
.fi
.sh "SEE ALSO"
create(quel), 
retrieve(quel), 
purge(unix)
