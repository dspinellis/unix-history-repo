.ds HE 'Version 6.1 Changes'INGRES'Page %'
.so nmacs
Version 6.1 of INGRES is upward compatable with version 6.0
of INGRES and contains a number of new features.
The most important of these are improved aggregation
capabilities,
macros in the monitor,
full concurrency control,
and overall speed
improvement.

The only incompatability with version 6.0 is that
secondary indices are stored differently internally
and the default for "retrieve into" is now "cheapsort".
.sh 1
Incompatibilities.
.dn
Secondary Indices.
.PS
We have learned that the way in which we formed
internal tuple identifiers was not
optimal for secondary indices.
Consequently we have changed the format of
tuple identifiers (tid).

Secondary indices must either be
destroyed and recreated or else use
the program "indconv" to convert the secondary indices.
Refer to the accompaning document "indconv(unix)"
for details.
.sh
Retrieve into defaults to cheapsort.
.PS
On a "retrieve into" the result relation
used to be modified to "isam".
The default has been changed to be cheapsort.
The primary advantage of this is that you
don't have the extra processing time needed
for the modify to isam.

As in the past the default can be overridden
at the time INGRES is invoked by using
the "-r" flag.
Refer to ingres(unix) for details.
.sh 1
Monitor
.dn
Saving and restoring queries.
.PS
Three new commands have
been added to help in saving and
restoring queries.
They are:
.nf

	\\chdir directory	(change directory)
	\\include file	(switch input to file)
	\\write file	(write query buffer to file)

.fi
.sh
Alternate Monitor Commands.
.PS
The Monitor allows you to spell out commands.
Thus "\\go" is the same as "\\g".
Refer to monitor(quel) for a complete list.
.sh 1
Parser.
.dn
Special conversion functions.
.PS
There are five conversion
functions: int1, int2, int4, float4, float8.
They allow you to specify the type of
any arithmetic expression.
For example:

.nf
range of e is employee
retrieve (total = sum(int4(e.salary)))

.fi
This will take each e.salary and convert it
to an i4 before summing.
The conversion functions can also be used
to convert character values to
numeric values.
.sh
Improved range table.
.PS
The range table is now completely
organized according to least recently
used.
This means that when you declare more
than ten tuple variables, the variable
least recently referenced is
discarded.
.sh
New Aggregates.
Aggregate facilities have been expanded to include
countu, sumu, and avgu.
These aggregate only unique values.
INGRES removes all duplicates before
performing the specified aggregate.
For example if the query asks for

avgu(p.qoh * p.weight)

Then the average will be performed only
on the unique values of the product of
p.qoh * p.weight.
.sh
Bugs involving "NOT" and "OR" fixed.
.PS
There were several bugs involving
queries with NOT and OR in the
qualification.
Those bugs have all been fixed.
.sh
Default for INDEX on "-n" flag.
.PS
When invoking INGRES,
the -n flag can be used to specify the default
structure for relations created by the "index"
command.
.sh
ASCII command accepts character values.
.PS
ASCII has been changed to accept and ignore
character values.
This can be useful for equel programs since
ascii(any domain) can always be retrieved
into a variable of type character.
.sh 1
OVQP
.dn
Correct handling of I1 and F4.
.PS
In the past, a retrieve to the terminal
of an i1 or a f4 domain was handled as if
the domain were an i2 or f8 respectively.
This mattered only when using the -i1 or -f4
flags when invoking INGRES.
.sh 1
DECOMP
.dn
Aggregate functions no longer produce a cross-product.
.PS
In the past,
if more than one aggregate function was specified on
a variable,
the system incorrectly produced a cross product
on the resulting aggregates.
This bug has been fixed and matching by-lists on
aggregate functions are always linked correctly
to the rest of the query.
.sh
Multi-variable and nested aggregates.
.PS
You can now have aggregates which involve
more than one relations.
For example, a lot of people have wanted
to know how many suppliers supply the
central processor:

.nf

range of p is parts
range of s is supply
retrieve (goodnews = count(s.snum where s.pnum = p.pnum
	and p.pname = "central processor"))
.fi

.sh
Processing of aggregates in parallel.
.PS
Whenever possible,
two or more aggregates
are processed
on the same pass through the relation.
This means that in the query:

.nf
range of p is parts
retrieve (
	p.color,
	cnt = count(p.pname by p.color),
	wt  = sum(p.weight by p.color)
	)

.fi
the sum and count would be computed
with only one pass over parts.
.sh
New Definition of Aggregate Functions.
.PS
In previous versions of INGRES,
if a "by-domain" value was excluded
by the qualification,
the aggregate value of the "by-domain"
was undefined.
Starting with version 6.1 an
undefined value always defaults to zero.

Simple aggregates always defaulted to zero
if undefined.
Aggregate functions are now consistant with
that definition.

For example:

.nf
range of e is employee
retrieve (e.manager, people =
	count(e.name by e.manager where
	e.startdate < 1970))
.fi

If there is a manager with no employee
who started before 1970 then the count will
be zero.
Previously no value would have appeared
for the manager.
.sh 1
DBU
.dn
Destroy, Help, and Print can take lists of relations
.PS
Destroy, help, and print can now take a comma
separated list of agruments.
.sh
System relations cannot be "modified"
.PS
You can no longer use the modify
command on system relations.
A separate stand-alone program
called "sysmod" must be used
to modify the system relations to
their optimal structure.
.sh
Sorting a relation stored as a heap.
.PS
A heap can be sorted and duplicates
removed using the modify command:
.br

modify relname to heapsort on domain1, domain2 ...
.br

The relation is sorted on the specified domains
and left as a heap.
This is faster than modifying the relation to isam.

This is useful when it is
desired to have the final output sorted
in some particular order.
.sh
Specifing the occupancy of an isam or hash relation
.PS
The modify command now accepts user information
pertaining to how much room to leave when
modifing a relation to isam or hash.
The new specifications are:

fillfactor (how much empty space to leave on each page.
Defaults are hash:50%, chash:75%, isam:80%, cisam:100%)
.br
minpages (hash only. Minimum number of primary pages desired.
Default is ten pages.)
.br
maxpages (hash only. Maximum number of primary pages desired.
Default is 2**24.)

For example:

modify parts to hash on pnum where fillfactor = 100

modify emp to hash on eno where fillfactor = 50
and minpages = 300

.sh
COPY allows delimitor specifications.
.PS
The c0 and d0 options on copy have been extended to
allow individual delimitor specification.
The general format is dom = c0delim.

For example on a copy "from",
pname = c0nl will accept only a nl as a delimitor.

On a copy "into" the construction pname = c0nl is
functionally identical to asking for
pname = c0, nl = d1.
.sh 1
Miscellaneous.
.PS
.dn
Concurrency Control.
.PS
INGRES now has full concurrency control.
By default all new data bases will have
concurrency control turned on.
Existing data bases can be easily upgraded
to have concurrency control.
Refer to creatdb(unix) for details.
.sh
Pathname can be changed without recompilation.
.PS
The INGRES pathname "/mnt/ingres" is no
longer a defined constant.
Instead INGRES scans the /etc/passwd file
and uses the initial working
directory of the user "ingres".
.sh
Restore Expanded.
.PS
Restore has been improved and expanded to
provide recovery from modify, index,
create, and destroy commands in addition
to append, delete, and replace commands.
.sh
Sysmod.
.PS
Since the structure of the system relations
is extremely important,
we now provide a stand-alone program which
modifies the relation, attribute, and indexes
relations to their optimal structure.

You are no longer allowed to do this
using the modify command.
This both prevents the relations from being
accidentally modified to a useless structure,
and allows INGRES to make certain assumptions
which improves performance everywhere.
