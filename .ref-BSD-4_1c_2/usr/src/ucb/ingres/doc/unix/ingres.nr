.th INGRES UNIX 3/14/79
.sh NAME
ingres \- \*(II relational data base management system
.sh SYNOPSIS
.bd ingres
[
.it flags
] dbname [ process_table ]
.sh DESCRIPTION
This is the \*(UU command which is used to invoke \*(II.
Dbname is the name of an existing data base.
The optional flags have the following meanings
(a ``\*(+-'' means the flag may be stated
``+\c
.it x\c
\&''
to set option
.it x
or ``\-\c
.it x\c
\&''
to clear option
.it x.
``\-'' alone means that ``\-\c
.it x\c
\&''
must be stated
to get the
.it x
function):
.s3
.de xx
.lp +10 10
.if t \fB\\$1\fP\fI\\$2\fP\t\c
.if n \\$1\\$2\t\c
..
.xx \*(+-U
Enable/disable direct update
of the system relations
and secondary indicies.
You must have the 000004 bit
in the status field
of the users file
set
for this flag to be accepted.
This option is provided
for system debugging
and is strongly discouraged for normal use.
.xx \-u name
Pretend you are the user
with login name
.it name
(found in the users file).
If
.it name
is of the form
.bd :\c
.it xx,
.it xx
is the two character user code
of a user.
This may only be used
by the DBA for the database
or by the \*(II superuser.
.xx \-c N
Set the minimum field width
for printing
character domains
to
.it N.
The default is 6.
.xx \-i lN
Set integer output field width to
.it N.
.it l
may be 1, 2, or 4 for
i1's, i2's, or i4's repectively.
.xx \-f lxM.N
Set floating point output field width
to
.it M
characters
with
.it N
decimal places.
.it l
may be 4 or 8 to apply to
f4's or f8's respectively.
.it x
may be
.bd e,
.bd E,
.bd f,
.bd F,
.bd g,
.bd G,
.bd n,
or
.bd N
to specify an output format.
.bd E
is exponential form,
.bd F
is floating point form,
and
.bd G
and
.bd N
are identical to
.bd F
unless the number
is too big to fit in that field,
when it is output in
.bd E
format.
.bd G
format guarantees decimal point alignment;
.bd N
does not.
The default format for both is
.bd n10.3.
.xx \-v X
Set the column seperator
for retrieves to the terminal
and print commands
to be
.it X.
The default is vertical bar.
.xx \-r M
Set modify mode
on the
.it retrieve into
command
to
.it M.
.it M
may be
.bd isam,
.bd cisam,
.bd hash,
.bd chash,
.bd heap,
.bd cheap,
.bd heapsort,
or
.bd cheapsort,
for ISAM,
compressed ISAM,
hash table,
compressed hash table,
heap,
compressed heap,
sorted heap,
or compressed sorted heap.
The default is ``cheapsort''.
.xx \-n M
Set modify mode on the
.it index
command to
.it M.
.it M
can take the same values as the
.bd \-r
flag above.
Default is ``isam''.
.xx \*(+-a
Set/clear
the autoclear option in the terminal monitor.
It defaults to set.
.xx \*(+-b
Set/reset batch update.
Users must the 000002 bit set
in the status field of the users file
to clear this flag.
This flag is normally set.
When clear,
queries will run slightly faster,
but no recovery can take place.
Queries which update a secondary index
automatically set this flag for that query only.
.xx \*(+-d
Print/don't print the dayfile.
Normally set.
.xx \*(+-s
Print/don't print any of the monitor messages,
including prompts.
This flags is normally set.
If cleared,
it also clears the
.bd \-d
flag.
.xx \*(+-w
Wait/don't wait for the database.
If the
.bd +w
flag is present,
\*(II will wait if
certain processes are running (purge,restore, and/or sysmod)
on the given data base.
Upon completion of those processes \*(II will proceed.
If the
.bd \-w
flag is present,
a message is returned and execution stopped
if the data base is not available.
If the
.bd \*(+-w
flag is omitted
and the data base is unavailable,
the error message is returned
if \*(II is running in foreground
(more precisly if the
standard input is from a terminal),
otherwise the wait option
is invoked.
.i0
.s3
.it Process_table
is the pathname of a \*(UU file
which may be used to specify the run-time configuration of \*(II.
This feature is intended for use in system maintenance only,
and its unenlightened use by the user community is strongly
discouraged.
.s3
Note: It is possible to run the monitor as a batch-processing
interface using the `<', `>' and `\*v' operators of the \*(UU shell,
provided the input file is in proper
monitor-format.
.sh EXAMPLE
.nf
ingres demo
ingres \-d demo
ingres \-s demo < batchfile
ingres \-f4g12.2 \-i13 +b \-rhash demo
.fi
.sh FILES
\&.../files/users \- valid \*(II users
.br
\&.../data/base/* \- data bases
.br
\&.../datadir/* \- for compatability with previous versions
.br
\&.../files/proctab7 \- runtime configuration file
.sh "SEE ALSO"
monitor(quel)
.sh DIAGNOSTICS
.lp +4 4
Too many options to \*(II \- you have stated too many flags
as \*(II options.
.lp +4 4
Bad flag format \- you have stated a flag
in a format which is not intelligible,
or a bad flag entirely.
.lp +4 4
Too many parameters \- you have given a database name,
a process table name,
and ``something else''
which \*(II doesn't know what to do with.
.lp +4 4
No database name specified
.lp +4 4
Improper database name \- the database name is not legal.
.lp +4 4
You may not access database
.it name
\- according to the users file,
you do not have permission to enter this database.
.lp +4 4
You are not authorized to use the
.it flag
flag \- the flag specified
requires some special authorization,
such as a bit in the users file,
which you do not have.
.lp +4 4
Database
.it name
does not exist
.lp +4 4
You are not a valid \*(II user \- you have not been
entered into the users file,
which means that you may not use \*(II at all.
.lp +4 4
You may not specify this process table \- special authorization
is needed to specify process tables.
.lp +4 4
Database temporarily unavailable \- someone else is currently
performing some operation on the database
which makes it impossible for you to even log in.
This condition should disappear shortly.
