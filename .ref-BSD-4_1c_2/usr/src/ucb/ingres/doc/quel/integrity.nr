.th INTEGRITY QUEL 2/7/79
.sh NAME
integrity \- define integrity constraints
.sh SYNOPSIS
.bd "define integrity on" 
var 
.bd is 
qual
.sh DESCRIPTION
The 
.it integrity 
statement adds an integrity constraint for the
relation  specified by 
.it var.
After the constraint is placed,
all updates to the relation must satisfy 
.it qual.
.it Qual 
must be
true when the 
.it integrity 
statement is issued or else a diagnostic is issued 
and the statement is rejected.
.s3
In the current implementation, 
.it integrity 
constraints are not
flagged  \-  bad  updates  are simply (and silently) not performed.
.s3
.it Qual 
must be a single variable  qualification  and  may  not
contain any aggregates.
.s1
.s3
.it integrity 
statement may be issued only by  the  relation
owner.
.sh EXAMPLE
/\*(** Ensure all employees have positive salaries \*(**/
  range of e is employee
  define integrity on e is e.salary > 0
.sh "SEE ALSO"
destroy(quel)
