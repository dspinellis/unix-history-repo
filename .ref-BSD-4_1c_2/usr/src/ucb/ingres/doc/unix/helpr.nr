.th HELPR UNIX 3/14/79
.sh NAME
helpr \- get information about a database.
.sh SYNOPSIS
.bd helpr
[
.bd \-u\c
.it name
] [
.bd \*(+-w
] database relation ...
.sh DESCRIPTION
.it Helpr
gives information about the named relation(s)
out of the database specified,
exactly like the
.it help
command.
.s3
Flags accepted
are
.bd \-u
and
.bd \*(+-u.
Their meanings are identical
to the meanings of the same flags
in \*(II.
.sh "SEE ALSO"
ingres(unix),
help(quel)
.sh DIAGNOSTICS
.in +4
.ti -4
bad flag \- you have specified a flag
which is not legal
or is in bad format.
.ti -4
you may not access database \- this database is prohibited
to you based on status information
in the users file.
.ti -4
cannot access database \- the database does not exist.
.in -4
