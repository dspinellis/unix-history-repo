.th SYSMOD UNIX 3/14/79
.sh NAME
sysmod \- modify system relations to predetermined storage structures.
.sh SYNOPSIS
.bd sysmod
[
.bd \-s
] [
.bd \-w
] dbname
[
.bd relation
] [
.bd attribute
] [
.bd indexes
] [
.bd tree
] [
.bd protect
] [
.bd integrities
]
.sh DESCRIPTION
.it Sysmod
will modify the relation,
attribute,
indexes,
tree,
protect,
and integrities
relations
to hash unless at least one of the
.bd relation,
.bd attribute,
.bd indexes,
.bd tree,
.bd protect,
or
.bd integrities
parameters are given,
in which case only those relations given as parameters
are modified.
The system relations are modified to gain maximum access performance
when running \*(II.
The user must be the data base administrator for
the specified database,
or be the \*(II superuser and have the
.bd \-s
flag stated.
.s3
.it Sysmod
should be run on a data base when it is first created
and periodically thereafter to maintain peak performance.
If many relations and secondary indices are created and/or destroyed,
.it sysmod
should be run more often.
.sp
If the data base is being used while
.it sysmod
is running, errors will occur.
Therefore,
.it sysmod
will lock the data base while it is being processed.
If the data base is busy,
.it sysmod
will report this.
If standard input is not a terminal
.it sysmod
will wait for the data base to be free.
If
.bd \-w
flag is stated
.it sysmod
will not wait, regardless of standard input.
The
.bd +w
flag causes
.it sysmod
to always wait.
.s3
The system relations are modified to hash;
the relation relation is keyed on the first domain,
the indexes,
attribute,
protect,
and integrities
relations are keyed on the first two domains,
and the tree
relation is keyed on domains one, two, and five.
The relation and attribute relations have the minpages option
set at 10, the indexes,
protect,
and integrities
relations have the minpages value set at 5.
.sh "SEE ALSO"
modify(quel)
