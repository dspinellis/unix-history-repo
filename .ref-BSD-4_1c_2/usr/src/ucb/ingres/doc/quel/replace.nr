.th REPLACE QUEL 2/29/79
.sh NAME
replace \- replace values of domains in a relation
.sh SYNOPSIS
.bd replace
tuple_variable (target_list)
[\c
.bd where
qual]
.sh DESCRIPTION
.it Replace 
changes the values of
the domains specified in the 
.it target_list 
for all tuples which
satisfy the qualification 
.it qual.
The 
.it tuple_variable 
must have been declared to range over
the relation which is to be modified.
Note that a tuple variable is required and
not the relation name.
Only domains which are to be modified need appear in the 
.it target_list.
These domains must be specified as result_attnames 
in the 
.it target_list 
either explicitly or by default (see quel(quel)).

Numeric domains may be replaced by values of any numeric type 
(with the exception noted below).
Replacement values will be converted to the type of the result domain.
.s1
Only the owner of a relation, or a user with 
replace pemission on the relation can do 
.it replace.
.s1
If the tuple update would violate an integrity constraint 
(see integrity(quel)), it is not done.
.sh EXAMPLE
.nf
/\*(** Give all employees who work for Smith a 10% raise \*(**/
   range of e is emp
   replace e(sal = 1.1 \*(** e.sal) where e.mgr = "Smith"
.fi
.sh "SEE ALSO"
integrity(quel), permit(quel), quel(quel), range(quel)
.sh DIAGNOSTICS
Use of a numeric type expression to replace a character type domain
or vice versa will produce diagnostics.
.sh BUGS
