.th DBU ERROR 3/30/79
.sh NAME
Data Base Utility error message summary
.sh SYNOPSIS
Error numbers 5000 \- 5999
.sh DESCRIPTION
The Data Base Utility functions
perform almost all tasks
which are not directly associated
with processing queries.
The error messages which they can generate
result from some syntax checking
and a considerable amount of semantic checking.
.sh ERRORS
.lp +8 8
5001	PRINT: bad relation name %0
.s3
You are trying to print a relation which doesn't exist.
.s3
.lp +8 8
5002	PRINT: %0 is a view and can't be printed
.s3
The only way to print a view
is by retrieving it.
.s3
.lp +8 8
5003	PRINT: Relation %0 is protected.
.s3
You are not authorized to access this relation.
.s3
.lp +8 8
5102	CREATE: duplicate relation name %0
.s3
You are trying to create a relation which already exists.
.s3
.lp +8 8
5103	CREATE: %0 is a system relation
.s3
You cannot create a relation with the same name as a system relation.
The
system depends on the fact that the system relations are unique.
.s3
.lp +8 8
5104	CREATE %0: invalid attribute name %1
.s3
This will happen if you try to create a relation with an attribute longer
than 12 characters.
.s3
.lp +8 8
5105	CREATE %0: duplicate attribute name %1
.s3
Attribute names in a relation must be unique.
You are trying to create one
with a duplicated name.
.s3
.lp +8 8
5106	CREATE %0: invalid attribute format "%2" on attribute %1
.s3
The allowed formats for a domain are c1\-c255, i1, i2, i4, f4 and f8.
Any other
format will generate this error.
.s3
.lp +8 8
5107	CREATE %0: excessive domain count on attribute %1
.s3
A relation cannot have more than 49 domains.
The origin of this magic number
is obscure.
This is very difficult to change.
.s3
.lp +8 8
5108	CREATE %0: excessive relation width on attribute %1
.s3
The maximum number of bytes allowed in a tuple is 1010.
This results from the
decision that a tuple must fit on one \*(UU "page".
Assorted pointers
require the 14 bytes which separates 1010 from 1024.
This "magic number" is
very hard to change.
.s3
.lp +8 8
5201	DESTROY: %0 is a system relation
.s3
The system would immediately stop working if you were allowed to do this.
.s3
.lp +8 8
5202	DESTROY: %0 does not exist or is not owned by you
.s3
To destroy a relation,
it must exist,
and you must own it.
.s3
.lp +8 8
5203	DESTROY: %0 is an invalid integrity constraint identifier
.s3
Integers given do not identify integrity constraints on the specified relation.
For example: If you were to type "destroy permit parts 1, 2, 3",
and 1, 2, or 3 were not the numbers "help permit parts"
prints out for permissions on parts, you would get this error.
.s3
.lp +8 8
5204	DESTROY: %0 is an invalid protection constraint identifier
.s3
Integers given do not identify protection constraints on the specified relation.
Example as for error 5203.
.s3
.lp +8 8
5300	INDEX: cannot find primary relation
.s3
The relation does not exist \- check your spelling.
.s3
.lp +8 8
5301	INDEX: more than maximum number of domains
.s3
A secondary index can be created on at most six domains.
.s3
.lp +8 8
5302	INDEX: invalid domain %0
.s3
You have tried to create an index
on a domain which does not exist.
.s3
.lp +8 8
5303	INDEX: relation %0 not owned by you
.s3
You must own relations
to put indicies on them.
.s3
.lp +8 8
5304	INDEX: relation %0 is already an index
.s3
\*(II does not permit tertiary indicies.
.s3
.lp +8 8
5305	INDEX: relation %0 is a system relation
.s3
Secondary indices cannot be created on system relations.
.s3
.lp +8 8
5306	INDEX: %0 is a view and an index can't be built on it
.s3
Since views are not physically stored
in the database,
you cannot build indicies on them.
.s3
.lp +8 8
5401	HELP: relation %0 does not exist
.s3
.lp +8 8
5402	HELP: cannot find manual section "%0"
.s3
Either the desired manual section does not exist,
or your system does not have any on-line documentation.
.s3
.lp +8 8
5403	HELP: relation %0 is not a view
.s3
Did a "help view" (which prints view definition) on a nonview.
For example: "help view overpaidv" prints out overpaidv's view definition.
.s3
.lp +8 8
5404	HELP: relation %0 has no permissions on it granted
.lp +8 8
5405	HELP: relation %0 has no integrity constraints on it
.s3
You have tried to print the permissions
or integrity constraints
on a relation
which has none specified.
.s3
.lp +8 8
5410	HELP: tree buffer overflowed
.lp +8 8
5411	HELP: tree stack overflowed
.s3
Still more buffer overflows.
.s3
.lp +8 8
5500	MODIFY: relation %0 does not exist
.s3
.lp +8 8
5501	MODIFY: you do not own relation %0
.s3
You cannot modify
the storage structure
of a relation you do not own.
.s3
.lp +8 8
5502	MODIFY %0: you may not provide keys on a heap
.s3
By definition,
heaps do not have keys.
.s3
.lp +8 8
5503	MODIFY %0: too many keys provided
.s3
You can only have
49
keys on any relation.
.s3
.lp +8 8
5504	MODIFY %0: cannot modify system relation
.s3
System relations can only be modified
by using the
.it sysmod
command to the shell;
for example
.s3
	sysmod
.it dbname
.s3
.lp +8 8
5507	MODIFY %0: duplicate key "%1"
.s3
You may only specify a domain as a key once.
.s3
.lp +8 8
5508	MODIFY %0: key width (%1) too large for isam
.s3
When modifying a relation to isam, the sum of the
width of the key fields cannot exceed 245 bytes.
.s3
.lp +8 8
5510	MODIFY %0: bad storage structure "%1"
.s3
The valid storage structure names are heap, cheap, isam,
cisam, hash, and chash.
.s3
.lp +8 8
5511	MODIFY %0: bad attribute name "%1"
.s3
You have specified an attribute
that does not exist
in the relation.
.s3
.lp +8 8
5512	MODIFY %0: "%1" not allowed or specified more than once
.s3
You have specified a parameter
which conflicts with another parameter,
is inconsistant with the storage mode,
or which has already been specified.
.s3
.lp +8 8
5513	MODIFY %0: fillfactor value %1 out of bounds
.s3
.it Fillfactor
must be between 1 and 100 percent.
.s3
.lp +8 8
5514	MODIFY %0: minpages value %1 out of bounds
.s3
.it Minpages
must be greater than zero.
.s3
.lp +8 8
5515	MODIFY %0: "%1" should be "fillfactor", "maxpages", or "minpages"
.s3
You have specified an unknown parameter
to
.it modify.
.s3
.lp +8 8
5516	MODIFY %0: maxpages value %1 out of bounds
.s3
.lp +8 8
5517	MODIFY %0: minpages value exceeds maxpages value
.s3
.lp +8 8
5518	MODIFY %0: invalid sequence specifier "%1" for domain %2.
.s3
Sequence specifier may be ``ascending''
(or ``a'')
or ``descending''
(or ``d'')
in a
.it modify.
For example:
.s3
.nf
	modify parts to heapsort on
	       pnum:ascending,
	       pname:descending
.fi
.s3
.lp +8 8
5519	MODIFY: %0 is a view and can't be modified
.s3
Only physical relations can be modified.
.s3
.lp +8 8
5520	MODIFY: %0: sequence specifier "%1" on domain %2
is not allowed with the specified storage structure.
.s3
Sortorder may be supplied only when modifying to
.bd heapsort
or
.bd cheapsort.
.s3
.lp +8 8
5600	SAVE: cannot save system relation "%0"
.s3
System relations have no save date and
are guaranteed to stay for the lifetime of the data base.
.s3
.lp +8 8
5601	SAVE: bad month "%0"
.lp +8 8
5602	SAVE: bad day "%0"
.lp +8 8
5603	SAVE: bad year "%0"
.s3
This was a bad month, bad day, or maybe even a bad year
for \*(II.
.s3
.lp +8 8
5604	SAVE: relation %0 does not exist or is not owned by you
.s3
.lp +8 8
5800	COPY: relation %0 doesn't exist
.s3
.lp +8 8
5801	COPY: attribute %0 in relation %1 doesn't exist
or it has been listed twice
.s3
.lp +8 8
5803	COPY: too many attributes
.s3
Each dummy domain and real domain listed in the copy statement
count as one attribute.
The limit is 150 attributes.
.s3
.lp +8 8
5804	COPY: bad length for attribute %0.  Length="%1"
.s3
.lp +8 8
5805	COPY: can't open file %0
.s3
On a copy "from", the file is not readable by the user.
.s3
.lp +8 8
5806	COPY: can't create file %0
.s3
On a copy "into", the file is not creatable by
the user.
This is usually caused by the user not having write
permission in the specified directory.
.s3
.lp +8 8
5807	COPY: unrecognizable dummy domain "%0"
.s3
On a copy "into", a dummy domain name is used to
insert certain characters into the unix file.
The domain name given is not valid.
.s3
.lp +8 8
5808	COPY: domain %0 size too small for conversion.
.br
There were %2 tuples successfully copied from %3 into %4
.s3
When doing any copy except character to character,
copy checks that the field is large enough
to hold the value being copied.
.s3
.lp +8 8
5809	COPY: bad input string for domain %0. Input was "%1".
There were %2 tuples successfully copied from %3 into %4
.s3
This occurs when converting character strings to
integers or floating point numbers.
The character string contains something other
than numeric characters (0-9,+,\*-,blank,etc.).
.s3
.lp +8 8
5810	COPY: unexpected end of file while filling domain %0.
.br
There were %1 tuples successfully copied from %2 into %3
.s3
.lp +8 8
5811	COPY: bad type for attribute %0. Type="%1"
.s3
The only accepted types are i, f, c, and d.
.s3
.lp +8 8
5812	COPY: The relation "%0" has a secondary index.
The index(es) must be destroyed before doing a copy "from"
.s3
Copy cannot update secondary indices.
Therefore, a copy "from" cannot be done on an indexed relation.
.s3
.lp +8 8
5813	COPY: You are not allowed to update the
relation %0
.s3
You cannot copy into a system relation or secondary index.
.s3
.lp +8 8
5814	COPY: You do not own the relation %0.
.s3
You cannot use copy to update a relation
which you do not own.
A copy "into" is allowed but a copy "from" is not.
.s3
.lp +8 8
5815	COPY: An unterminated "c0" field occurred while filling domain %0.
There were %1 tuples successfully copied from %2 into %3
.s3
A string read on a copy "from" using the
"c0" option cannot be longer than 1024 characters.
.s3
.lp +8 8
5816	COPY: The full pathname must be specified for
the file %0
.s3
The file name for copy must start with a "/".
.s3
.lp +8 8
5817	COPY: The maximum width of the output file cannot
exceed 1024 bytes per tuple
.s3
The amount of data to be output
to the file for each tuple exceeds
1024.
This usually happens only if a format was
mistyped or a lot of large
dummy domains were specified.
.s3
.lp +8 8
5818	COPY: %0 is a view and can't be copied
.s3
Only physical relations
can be copied.
.s3
.lp +8 8
5819	COPY: Warning: %0 duplicate tuples were ignored.
.s3
On a copy "from", duplicate tuples were present in the relation.
.s3
.lp +8 8
5820	COPY: Warning: %0 domains had control characters
which were converted to blanks.
.s3
.lp +8 8
5821	COPY: Warning: %0 c0 character domains were truncated.
.s3
Character domains in c0 format are of the same length as the
domain length.  You had a domain value greater than this
length, and it was truncated.
.s3
.lp +8 8
5822	COPY: Relation %0 is protected.
.s3
You are not authorized to access this relation.
