.th DELETE QUEL 1/26/79
.sh NAME
delete \- delete tuples from a relation
.sh SYNOPSIS
.bd delete
tuple_variable 
[\c
.bd where
qual\c
]
.sh DESCRIPTION
.it Delete 
removes tuples which satisfy the qualification 
.it qual 
from the
relation that they belong to.
The 
.it tuple_variable 
must have been declared
to range over an existing relation in a previous 
.it range 
statement.
.it Delete 
does not have a 
.it target_list.
The
.it delete 
command requires a
tuple variable from a 
.it range 
statement, and
not the actual relation name.
If the qualification is not given, the effect is to delete all
tuples in the relation.
The result is a valid, but empty relation.
.s3
To 
.it delete 
tuples from a relation, you must be the owner of the relation,
or have 
.it delete 
permission on the relation.
.sh EXAMPLE
.nf
/\*(** Remove all employees who make over $30,000 \*(**/
   range of e is emp
   delete e where e.sal > 30000
.fi
.sh "SEE ALSO"
destroy(quel), permit(quel), quel(quel), range(quel)
.sh BUGS
