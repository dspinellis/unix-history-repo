.th RETRIEVE QUEL 2/29/79
.sh NAME
retrieve \- retrieve tuples from a relation
.sh SYNOPSIS
.bd retrieve 
[[\c
.bd into\c
] relname] (target_list) 
[\c
.bd where 
qual]
.br 
.bd "retrieve unique" 
(target_list) [\c
.bd where
qual]
.sh DESCRIPTION
.it Retrieve 
will get all tuples which satisfy the qualification and
either display them on the terminal
(standard output) or store them in a new relation.

If a relname is specified, the result of the query will be stored in a new
relation with the indicated name.
A relation with this name
owned by the user must not already exist.
The current user will be the owner of the new relation.
The relation will have domain names as specified in the 
.it target_list
result_attnames.
The new relation will be saved on the system for seven days unless
explicitly saved by the user until a later date.
.s1
If the keyword
.bd unique
is present, tuples will be sorted on the
first domain, and duplicates will be removed, before being
displayed.
.s1
The keyword 
.bd all
can be used when it is desired to
retrieve all domains.

If no result 
.it relname 
is specified then the result of the query will be
displayed on the terminal and will not be saved.
Duplicate tuples are not removed when the result is
displayed on the terminal.

The format in which domains are printed can be
defined at the time ingres is invoked (see ingres(unix)).

If a result relation is specified then
the default procedure is
to modify the result relation to an cheapsort storage structure
removing duplicate tuples in the process.

If the default cheapsort structure is not desired, the
user can override this at the time \*(II is
invoked by using the 
.bd \-r
switch (see ingres(unix)).
.s3
Only the relation's owner and users with retrieve permission may 
.it retrieve 
from it.
.sh EXAMPLE
.nf
/\*(** Find all employees who make more than their manager \*(**/
   range of e is emp
   range of m is emp
   retrieve (e.name) where e.mgr = m.name
	    and e.sal > m.sal
/\*(** Retrieve all domains for those who make more
	than the average salary \*(**/
   retrieve into temp (e.all) where e.sal > avg(e.sal)
/\*(** retrieve employees's names sorted \*(**/
   retrieve unique (e.name)
.fi
.sh "SEE ALSO"
modify(quel), permit(quel), quel(quel), range(quel), 
save(quel), ingres(unix)
.sh DIAGNOSTICS
.sh BUGS
