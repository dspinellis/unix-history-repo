.ds HE 'Version 6.2 Changes'INGRES'Page %'
.so nmacs
Version 6.2 of INGRES is upward compatible
with version 6.1 of INGRES.
Version 6.2 supports protection statements,
integrity constraints, and views.
In addition, the system can be
configured to take advantage of separate instruction
and data spaces on 11/45's and 11/70's.

There are several new keywords.
The "-u" flag has a different format.
The protection, integrity, and view
subsystems depend on certain system relations
being present.
Existing databases can be optionally updated
by using the creatdb program with the "-e" flag.
See creatdb(unix) for details.
As part of the new protection system,
new databases are placed in .../data/base/
instead of .../datadir/.
Existing databases will still operate correctly.
The reason for this change is to get around
UNIX protection "features" related to removing files
and creating directories.
We found a number of bugs in the floating point to
character conversion routine.
It may happen that versions 6.1 and 6.2 give different
results in the last decimal place.
The 6.2 results are correctly rounded while the 6.1 are not
necessarily correct.
.sh 1
Monitor
.dn
Macros.
.PS
The monitor supports user defined macros.
It is possible to specify alternate keywords,
shorthands for commonly used phrases
and even program using the macro facilities.
In addition ingres can automatically define
special macros on a per user basis.
For example "range of p is parts" can be shortened by
the macro:

{define;rg $1 $2;range of $1 is $2}

to be

rg p parts
.sh
Tuple count.
.PS
It is now possible to write a macro
to find out the number of tuples
which satisfied the query last
successfully executed.
Thus updates do not have to be silent.
.sh
Trapping queries.
.PS
It is possible to save queries run from the monitor.
The macro "{querytrap}" can be defined to be the pathname
of a file where the queries are to be saved.
The date,
query,
and tuple count for each successfully parsed query
are appended to the end of the file whenever the
macro is defined.
.sh 1
Parser.
.dn
Retrieve unique.
.PS
Retrieves can now be specified to be "unique".
The result of the retrieve will be sorted from left
to right and duplicates removed.
For example:

.nf
	range of e is employee
	retrieve unique (e.manager)
.fi

.sh
Exponential notation.
.PS
Exponential notation is now accepted for
floating point numbers.
This feature was actually added in most later versions
of 6.1.
.sh
Expanded error checking.
.PS
The parser now detects many of the errors that
OVQP used to detect.
.sh 1
Qrymod.
.PS
Query modification is the name we give to the
collection of code which performs the
protection, integrity, and view functions.
This feature is new to version 6.2.
It requires an additional process making 6 processes
total for an 11/34 system and 5 total for an 11/70
system.
When creating a
database,
query modification can be optionally permitted.
An existing database can be changed to allow query modification.
If a database does not have query modification support,
then the qrymod process is not invoked.
.dn
Views.
.PS
The system now supports user defined views.
A view is a virtual relation.
The view relation is defined in terms
of existing relations.
Any accesses to the view are automatically
translated to accesses to the relations
on which it is defined.
For example:

.nf
	range of e is employee
	define view myemp(e.name, e.salary)
		where e.manager = 199

	range of m is myemp
	retrieve (m.name) where m.salary > 8000

.fi
is equivalent to the query:

.nf
	range of e is employee
	retrieve (e.name) where e.salary > 8000
		and e.manager = 199
.fi

.sh
Protection.
.PS
Relations created in a database supporting
query modification,
default to no access.
The DBA can use the
"permit" command to selectively
grant access to users.
For example:

.nf
	range of e is employee
	define permit retrieve on e (name)
		to sam at tty5
		from 8:00 to 17:00
		on mon to fri
		where e.salary < 15000
.fi
.sh
Integrity.
.PS
Updates to a relation can now be constrained to
satisfy certain conditions.
Updates which do not satisfy the conditions
are illegal and are ignored.
For example if employee names must have
at least a capital letter followed by a comma
followed by another capital letter then:

.nf
	define integrity on e is
		e.name = "[A-Z]*,[A-Z]*"
.fi
.sh 1
OVQP.
.dn
Expanded pattern matching
.PS
OVQP will now take advantage of storage
structures on pattern matching expressions.
Previous versions ignored the structure of a
relation on a pattern matching expression
and caused a full relation scan.
The current algorithm will analyze a
pattern matching constant and use the
storage structure of a relation if at all possible.
.sh
Concat definition changed.
.PS
Concat has been changed slightly to allow
concatenation of blanks.
It does this by guaranteeing that if the
value being concatenated is all blank,
then it is trimmed to be one blank.
For example:

.nf
	concat(x.firstname, concat(" ", x.lastname))
.fi
.sh
Special constants.
.PS
There are two constants which are recognized
by the system and processed specially.
"Dba" and "usercode" have the values
of the two character ingres id of the
database administrator for the current database,
and the current user.
Special constants will be expanded in future
versions.
For example:

.nf
	retrieve (whoami = usercode)
.fi
.sh 1
Decomp.
.dn
Ten variable queries.
.PS
Decomp can now handle queries of up to ten variables.
The previous limit was six.
You are still limited to ten variables defined at
any one time.
.sh
New processing algorithms.
.PS
Decomp has been made smarter.
It now is more intelligent about which variable
to substitute for and when to reformat a relation.
The result is that it makes better use of
existing storage structures whenever possible.

A new processing technique called "reduction" is now
considered for queries with three or more variables.
Details can be found in INGRES memo
ERL M78/17.
.sh
Aggregates correctly linked.
.PS
The final few bugs have been fixed in the
aggregation processor.
There were some cases when it did not
correctly process aggregate functions.
These have all been fixed so you can now
ask the query, "Which suppliers supply
all the parts supplied by some other supplier:"

.nf
	range of s is supply
	range of x is supply
	retrieve (x.snum) where
		count(s.pnum by s.snum,x.snum
			where s.pnum = x.pnum
			and   s.snum != x.snum)
			=
		count(s.pnum by s.snum)
.fi
.sh 1
DBU.
.dn
Improved modify to isam, hash, or heapsort.
.PS
Modify has been greatly improved to speed
the time required to modify a relation to
any storage structure.
.sh
Sort in reverse.
.PS
Modify to heapsort allows you to specify ascending
or descending sort sequences.
Previously ascending was the only ordering allowed.
The sort order of
individual domains can be specified, for example:

.nf
	modify newemp to heapsort on age:descending,
		salary, name:descending
.fi
.sh
Help remembers you forever.
.PS
The help command will now remember who you are,
even after an interrupt signal.
Previously it suffered amnesia under certain conditions.
.sh
Copy expanded a little.
.PS
The copy command now allows arbitrary delimitors
on c0 and d0 formats.
Special, single character delimitors
can be specified by putting the format in quotes.

	copy parts(name="c0&") from "/mnt/me/myfile"

specifies that the variable length name is terminated
by an "&" character.

On a copy into a relation,
the c0 format will now allow arbitrarily long fields.
The previous limit was 1024 characters.
Now it treats only the first 1024 as significant
and ignores the remainder.
.sh
Query modification information.
.PS
Help has been expanded to
give information about views, protection, and
integrity.
For example:

.nf
	help integrity parts, supply

.fi
would give information about
the integrity contraints on the parts and supply
relations.
.sh
Destroying integrity and protection constraints.
.PS
Integrity and protection constraints can be
destroyed using a variation of the destroy command.
By asking for help on a relation,
you can get identification numbers for the
contraints.
The constraints can then be deleted by

.nf
	destroy integrity 3, 6, 8

	or

	destroy integrity all
.fi
.sh
Help all.
.PS
Help now accepts the key word "all".
Its meaning is to print information
about all user relations which the user can access.

.sh 1
Equel.
.dn
Tuple count
.PS
A global variable called "IItupcount" is set
after each query which gives the
number of tuples in the result.
For a retrieve,
this is the number of tuples retrieved,
and for an update it is the number of tuples
actually changed.
.sh
Equel expanded.
.PS
Equel allows nearly all "C" language constructs.
Specifically it now supports structures,
pointers, and local variables.

Quel statements can be parameterized so that
arbitrary equel statements can be generated
at run time.

Equel now accepts "## include" files.
.sh 1
Miscellaneous.
.dn
11/70 system.
.PS
Ovqp and decomp can be combined on machines
which support separated instruction and data spaces.
In addition,
all the dbu's can be combined into one overlay
on i and d machines.
For details,
see the setup instructions.
.sh
Better page buffering.
.PS
On 11/70 systems,
INGRES uses a better buffering algorithm
which can substantially reduce disk references.
.sh
Improved "-u" flag for alter egos.
.PS
The "-u" flag now accepts the name of a user.
The database administrator can run ingres
as someone else on his/her database by
specifier the users login name,
for example "ingres -ubetty empdb"
would invoke ingres with the user "betty"s
privileges on the "empdb" database.

Alternately,
the INGRES usercode of the user can be specified by
stating "-u:xx" where "xx" is the INGRES usercode.
.sh
Datadir replaced by data/base.
.PS
New databases are now placed in the
directory .../data/base/.
In previous versions they were placed in .../datadir.
The reasons for this relate to the fact that creating a directory
requires read/write/search permission for everyone.
However,
such permission also allows any user to remove a file.
The two level directory allows the creation of directories
but protects against users accessing the database to remove files.
Existing databases in .../datadir/ will continue to work
but you should be aware that users can remove the files holding
the relations.
If desired,
existing databases can be copied from .../datadir/dbname/*
to .../data/base/dbname/*.
After being copied,
the old database must be destroyed.
You must be logged in as ingres to perform this feat.
.sh
Databases outside of .../data/base/.
.PS
Database can now exist outside of .../data/base/.
To do this,
the ingres superuser must create the directory
for the database and store the pathname of the
directory in a file under .../data/base/.
For details see the creatdb(unix) documentation.
.sh
Helpr.
.PS
There is a stand alone routine akin to
"printr" called "helpr".
Its function is the same as the help command.
.sh
Floating point conversion.
.PS
Version 6.1 did not correctly round floating point
numbers with negative exponents.
Thus it is likely that floating point values printed
using 6.2 will differ from 6.1.
.sh
Improved ISAM search.
.PS
The ISAM directory pages are now searched in a
more efficient manner.
