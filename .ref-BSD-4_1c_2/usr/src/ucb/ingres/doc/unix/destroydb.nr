.th DESTROYDB UNIX 3/14/79
.sh NAME
destroydb \- destroy an existing database
.sh SYNOPSIS
.bd destroydb
[
.bd \-s
] [
.bd \-m
] dbname
.sh DESCRIPTION
.it Destroydb
will remove all reference to an existing database.
The directory of the database and all files in that directory
will be removed.
.s3
To execute this command the current user must be the
database administrator for the database in question,
or must be the \*(II superuser
and have the
.bd \-s
flag stated.
.s3
The
.bd \-m
flag causes
.it destroydb
not to remove the
\*(UU directory.
This is useful when the directory is a separate mounted
\*(UU file system.
.sh EXAMPLE
.nf
destroydb demo
destroydb \-s erics_db
.fi
.sh FILES
\&.../data/base/*
.br
\&.../datadir/* (for compatibility with previous versions)
.sh "SEE ALSO"
.nf
creatdb(unix)
.fi
.sh DIAGNOSTICS
.in +4
.ti -4
invalid dbname \- the database name specified
is not a valid name.
.ti -4
you may not reference this database \- the database may exist,
but you do not have permission to do anything with it.
.ti -4
you may not use the
\-s
flag \- you have tried to use the
.bd \-s
flag,
but you are not the INGRES superuser.
.ti -4
you are not the dba \- someone else created this database.
.ti -4
database does not exist \- this database does not exist.
.in -4
