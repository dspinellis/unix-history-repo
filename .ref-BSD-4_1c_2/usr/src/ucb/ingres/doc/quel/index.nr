.th INDEX QUEL 2/21/79
.sh NAME
index \- create a secondary index on an existing relation.
.sh SYNOPSIS
.bd "index on"
relname 
.bd is
indexname 
(domain\c
.it 1 
{ ,domain\c
.it 2\c
})
.sh DESCRIPTION
.it Index
is used to create secondary indices on existing relations
in order to make retrieval and update with secondary keys more
efficient.
The secondary key is constructed from relname domains 1, 2,...,6
in the order given.
Only the owner of a relation is allowed to
create secondary indices on that relation.
.s3
In order to maintain the integrity of the index, users will NOT
be allowed to directly update
secondary indices. 
However, whenever a primary relation is changed,
its secondary indices will be automatically updated by the system.
Secondary indices may be modified to further increase the access
efficiency of the primary relation.
When an index is first created, it is automatically
modified to an isam storage structure
on all its domains.
If this structure is undesirable, the user may override
the default isam structure by using the
.bd \-n
switch (see ingres(unix)), or by entering
a 
.it modify
command directly.
.s3
If a 
.it modify 
or 
.it destroy 
command is used on 
.it relname\c
, all secondary indices on 
.it relname 
are destroyed.
.s3
Secondary indices on other indices, or on system
relations are forbidden.
.sh EXAMPLE
.nf
/\*(** Create a secondary index called ``x'' on relation ``emp'' \*(**/
   index on emp is x(mgr,sal)
.fi
.sh "SEE ALSO"
copy(quel), destroy(quel), modify(quel)
.sh BUGS
At most 6 domains may appear in the key.
.s3
The 
.it copy 
command cannot be used to copy into
a relation which has secondary indices.
.s3
The default structure isam is a poor choice
for an index unless the range of retrieval
is small.
