.th COPYDB UNIX 3/14/79
.sh NAME
copydb \- create batch files to copy out a data base and restore it.
.sh SYNOPSIS
.bd copydb
[
.bd \-u\c
.it name
] database full-path-name-of-directory
[ relation ... ]
.sh DESCRIPTION
.it Copydb
creates two
\*(II command files in the directory:
.bd Copy.out,
which contains Quel instuctions which will copy all relations owned by the user
into files in the named directory,
and
.bd copy.in,
which contains instructions to copy the files into relations,
create indexes and do modifies.
The files will have the same names as the relations with
the users \*(II id tacked on the end.
(The directory MUST NOT be the same as the data base directory
as the files have the same names as the relation files.)
The
.bd \-u
flag may be used to run
.it copydb
with a different user id.
(The fact that
.it copydb
creates the copy files does not imply that the user
can necessarily access the specified relation).
If relation names are specified only those relations will be
included in the
copy files.
.s3
.it Copydb
is written in Equel and will access the database in the usual manner.
It does not have to run as
the \*(II user.
.sh EXAMPLE
.nf
chdir /mnt/mydir
copydb db /mnt/mydir/backup
ingres db <backup/copy.out
tp r1 backup
rm \-r backup

tp x1 
ingres db <backup/copy.in
.fi
.sh DIAGNOSTICS
.it Copydb
will give self-explanatory diagnostics.
If ``too many indexes'' is reported it means that more than ten indexes have
been specified on one relation.
The constant can be increased and the program recompiled.
Other limits are set to the system limits.
.sh BUGS
.it Copydb
assumes that indexes which are ISAM do not need to be remodified.
.it Copydb
cannot tell if the relation was modified with a fillfactor or
minpages specification.
The
.bd copy.in
file may be edited to reflect this.
