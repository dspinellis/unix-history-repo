.th PURGE UNIX 3/14/79
.sh NAME
purge \- destroy all expired and temporary relations
.sh SYNOPSIS
.bd purge
[
.bd \-f
] [
.bd \-p
] [
.bd \-a
] [
.bd \-s
] [
.bd \*(+-w
] [ database ... ]
.sh DESCRIPTION
.it Purge
searches the named databases
deleting system temporary relations.
When using the
.bd \-p
flag, expired user
relations are deleted.
The
.bd \-f
flag will cause unrecognizable files to be deleted, 
normally purge will just report these files.
.s3
Only the database administrator (the DBA) for
a database may run purge,
except the \*(II superuser may purge
any database by using the
.bd \-s
flag.
.s3
If no databases are specified all databases for which you
are the DBA will be purged.
All databases will be purged if the INGRES superuser
has specified the
.bd \-s
flag.
The
.bd \-a
flag will cause purge to print a message about the pending
operation and execute it only if the response if a `y'.
Any other response is interpreted as ``no''.
.s3
.it Purge
will lock the data base while it is being processed,
since errors may occur if the database is active while purge
is working on the database.
If a data base is busy
.it purge
will report this
and go on to the next data base,
if any.
If standard input is not a
terminal
.it purge
will wait for the data base to be free.
If
.bd \-w
flag is stated
.it purge
will not wait, regardless of standard input.
The
.bd +w
flag causes
.it purge
to always wait.
.sh EXAMPLES
purge \-p +w tempdata
.br
purge \-a \-f
.sh "SEE ALSO"
save(quel),
restore(unix)
.sh DIAGNOSTICS
.in +4
.ti -4
who are you? \- you are not entered into the users file.
.ti -4
not ingres superuser \- you have tried to use the
.bd \-s
flag
but you are not the INGRES superuser.
.ti -4
you are not the dba \- you have tried to purge a database
for which you are not the DBA.
.ti -4
cannot access database \- the database does not exist.
.in -4
.sh BUGS
If no database names are given,
only the databases located in the directory
.bd data/base
are purged,
and not the old databases
in
.bd datadir.
Explicit database names still work
for databases
in either directory.
