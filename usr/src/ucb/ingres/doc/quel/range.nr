.th RANGE QUEL 2/29/79
.sh NAME
range \- declare a variable to range over a relation
.sh SYNOPSIS
.bd "range of"
variable 
.bd is
relname
.sh DESCRIPTION
.it Range 
is used to declare variables which will be used in subsequent
\s-2QUEL\s0 statements.
The 
.it variable 
is associated with the relation specified by 
.it relname.
When the 
.it variable 
is used in subsequent statements it will refer
to a tuple in the named relation.
A range declaration remains in effect for an entire 
\*(II session
(until exit from \*(II), until the variable is redeclared by a subsequent
range statement, or until the relation is removed with the destroy command.
.sh EXAMPLE
.nf
/\*(** Declare tuple variable e to range over relation emp \*(**/
   range of e is emp
.fi
.sh "SEE ALSO"
quel(quel), destroy(quel)
.sh BUGS
Only 10 variable declarations may be in effect at any time.
After the 10th range statement,
the least recently referenced
variable is re-used for the next
range statement.
