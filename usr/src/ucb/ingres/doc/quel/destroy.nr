.th DESTROY QUEL 2/21/79
.sh NAME
destroy \- destroy existing relation(s)
.sh SYNOPSIS
.bd destroy 
relname { , relname }
.br
.bd destroy 
[
.bd permit
|
.bd integrity
] relname
[
integer { , integer }
|
.bd all
]
.sh DESCRIPTION
.it Destroy 
removes relations from the data base,
and removes constraints or permissions from a relation.
Only the relation owner
may destroy a relation or its permissions and integrity
constraints.
A relation may be emptied of tuples, but not destroyed, using the delete
statement or the modify statement.
.s3
If the relation being destroyed has secondary
indices on it, the secondary indices are also destroyed.
Destruction of just a secondary index does not affect the
primary relation it indexes.
.s3
To destroy individual permissions or constraints for a relation,
the 
.it integer 
arguments should be those printed by a 
.bd "help permit"
(for 
.bd destroy 
.bd permit\c
)
or a 
.bd "help integrity"
(for 
.bd destroy
.bd integrity\c
) on the same relation.
To destroy all constraints or permissions, the 
.bd all
keyword may be used in place of individual integers.
To destroy constraints or permissions, either the
.it integer
arguments or the
.bd all 
keyword must be present.
.sh EXAMPLE
.nf
/\*(** Destroy the emp relation \*(**/
    destroy emp
    destroy emp, parts
.s1
/\*(** Destroy some permissions on parts, and all integrity
 \*(** constraints on employee
 \*(**/
   destroy permit parts 0, 4, 5
   destroy integrity employee
.fi
.sh "SEE ALSO"
create(quel), delete(quel), help(quel), index(quel), modify(quel)
