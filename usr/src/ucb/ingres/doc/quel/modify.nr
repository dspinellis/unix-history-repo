.th MODIFY QUEL 2/23/79
.sh NAME
modify \- convert the storage structure of a relation
.sh SYNOPSIS
.in +5
.ti -5
.bd modify 
relname 
.bd to 
storage-structure
[
.bd on 
key1 [ :
.it sortorder
] [ { , key2 [ :
.it sortorder
] } ] ] 
[
.bd where 
[
.bd "fillfactor =" 
.it n
] [
.bd ", minpages =" 
.it n
] [
.bd ", maxpages =" 
.it n
] ]
.i0
.sh DESCRIPTION
.it Relname 
is modified to the specified storage structure.
Only the owner of a relation can modify that relation.
This command is used to increase performance when using large
or frequently referenced relations.
The storage structures are specified as follows:
.s3
.if n .in +10
.if t .in +1i
.de xx
.if n .ti -5
.if t .ti -0.5i
..
.xx
isam \- indexed sequential storage structure
.xx
cisam \- compressed isam
.xx
hash \- random hah storage structure
.xx
chash \- compressed hash
.xx
heap \- unkeyed and unstructured
.xx
cheap \- compressed heap
.xx
heapsort \- heap with tuples sorted and duplicates removed
.xx
cheapsort \- compressed heapsort
.xx
truncated \- heap with all tuples deleted
.s3
.i0
The paper ``Creating and Maintaining a Database in \*(II''
(ERL Memo M77\-71)
discusses how to select storage structures
based on how the relation is used.
.s3
The current compression algorithm only suppresses trailing
blanks in character fields.
A more effective compression scheme may be possible,
but tradeoffs between that and a larger and slower
compression algorithm are not clear.
.s3
If the 
.it on
phrase is omitted when modifying to isam, cisam, hash or chash,
the relation will automatically be keyed on the first domain.
When modifying to heap or cheap the 
.it on
phrase must be omitted.
When modifying to heapsort or cheapsort the 
.it on
phrase is optional.
.s3
When a relation is being sorted (isam, cisam, heapsort and cheapsort),
the primary sort keys will be those specified in the 
.it on
phrase (if any).
The first key after the 
.it on
phrase will be the most significant sort key
and each successive key specified will be the next most significant
sort key.
Any domains not specified in the 
.it on
phrase will be used as least
significant sort keys in domain number sequence.
.s3
When a relation is modified to heapsort or cheapsort,
the 
.it sortorder
can be specified to be 
.bd ascending 
or
.bd descending.
The default is always 
.bd ascending.
Each key given in the
.it on
phrase can be optionally modified
to be:
.s1
.if n .ti +5
.if t .ti +0.5i
key:descending
.s1
which will cause that key to be sorted in descending order.
For completeness, 
.bd ascending
can be specified after the colon (`:'),
although this is unnecessary since
it is the default.
.bd Descending
can be abbreviated by a single `\c
.bd d\c
\&' and, correspondingly,
.bd ascending 
can be abreviated by a single `\c
.bd a\c
\&'.
.s3
.it Fillfactor 
specifies the percentage
(from 1 to 100)
of each primary data page that should be filled
with tuples, under ideal conditions.
.it Fillfactor
may be used with isam, cisam, hash and chash.
Care should be taken when using large fillfactors since a non-uniform
distribution of key values could cause overflow pages to be created,
and thus degrade access performance for the relation.
.s3
.it Minpages 
specifies the minimum number of primary pages a hash or chash
relation must have.
.it Maxpages
specifies the maximum number of primary pages
a hash or chash relation may have.
.it Minpages 
and 
.it maxpages 
must be at least one.
If both 
.bd minpages 
and 
.bd maxpages 
are specified in a modify,
.bd minpages 
cannot exceed 
.bd maxpages.
.sp
Default values for 
.bd fillfactor\c
, 
.bd minpages\c
, and 
.bd maxpages 
are as follows:
.if n .ta 5 12 25 35
.if t .ta 0.5i 1i 3i 4.5i
.nf
.ul
.s1
		FILLFACTOR	MINPAGES	MAXPAGES
.s2
	hash	50	10	no limit
	chash	75	1	no limit
	isam	80	NA	NA
	cisam	100	NA	NA
.fi
.dt
.sh EXAMPLES
.nf
/* modify the emp relation to an indexed
        sequential storage structure with
        "name" as the keyed domain */
.s1
modify emp to isam on name
.s1
/* if "name" is the first domain of the emp relation,
        the same result can be achieved by */
.s1
modify emp to isam
.s1
/* do the same modify but request a 60% occupancy
        on all primary pages */
.s1
modify emp to isam on name where fillfactor = 60

/* modify the supply relation to compressed hash
        storage structure with "num" and "quan"
        as keyed domains */
.s1
modify supply to chash on num, quan
.s1
/* now the same modify but also request 75% occupancy
        on all primary, a minimum of 7 primary pages
        pages and a maximum of 43 primary pages */
.s1
modify supply to chash on num, quan
        where fillfactor = 75, minpages = 7,
        maxpages = 43
.s1
/* again the same modify but only request a minimum
        of 16 primary pages */
.s1
modify supply to chash on num, quan
        where minpages = 16
.s1
/* modify parts to a heap storage structure */
.s1
modify parts to heap
.s1
/* modify parts to a heap again, but have tuples
        sorted on "pnum" domain and have any duplicate
        tuples removed */
.s1
modify parts to heapsort on pnum
.s1
/* modify employee in ascending order by manager,
        descending order by salary and have any
        duplicate tuples removed */
.s1
modify employee to heapsort on manager, salary:descending
.fi
.sh "SEE ALSO"
sysmod(unix)
