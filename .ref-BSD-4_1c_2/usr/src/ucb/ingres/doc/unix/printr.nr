.th PRINTR UNIX 3/14/79
.sh NAME
printr \- print relations
.sh SYNOPSIS
.bd printr
[
.it flags
] database relation ...
.sh DESCRIPTION
.it Printr
prints the named relation(s)
out of the database specified,
exactly like the
.it print
command.
Retrieve permission must be granted to all people
to execute this command.
.s3
Flags accepted
are
.bd \-u,
.bd \*(+-w,
.bd \-c,
.bd \-i,
.bd \-f,
and
.bd \-v.
Their meanings are identical
to the meanings of the same flags
in \*(II.
.sh "SEE ALSO"
ingres(unix),
print(quel)
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
