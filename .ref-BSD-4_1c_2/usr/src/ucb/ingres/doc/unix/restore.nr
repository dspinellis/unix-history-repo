.th RESTORE UNIX 3/14/79
.sh NAME
restore \- recover from an \*(II or \*(UU crash.
.sh SYNOPSIS
.bd restore
[
.bd \-a
] [
.bd \-s
] [
.bd \*(+-w
] [ database ... ]
.sh DESCRIPTION
.it Restore
is used to restore a data base after
an \*(II or \*(UU crash.
It should always be run after any abnormal termination
to ensure the integrity of the data base.
.s3
In order to run restore,
you must be the DBA for the database you are restoring
or the \*(II superuser and specify the
.bd \-s
flag.
.s3
If no databases are specified then all databases for which you are the DBA
are restored.
All databases will be restored if the \*(II superuser
has specified the
.bd \-s
flag.
.s3
If the
.bd \-a
flag is specified you will be asked before restore takes any serious
actions.
It is advisable to use this flag if you suspect the database is in bad shape.
Using /dev/null as input with the
.bd \-a
flag will provide a report of problems in
the data base.
If there were no errors while restoring a database,
.it purge
will be called, with the same flags that were given to
.it restore,
to remove unwanted files
and system temporaries.
.it Restore
may be called with the
.bd \-f
and/or
.bd \-p
flags for
.it purge.
Unrecognized files and expired relations are not removed unless the proper
flags are given.
In the case of an incomplete destroy, create or index
.it restore
will not delete 
files for partially created or destroyed relations.
.it Purge
must be called with the
.bd \-f
flag to accomplish this.
.s3
.it Restore
locks the data base while it is being processed.
If a data base is busy
.it restore
will report this and go on to the next data base.
If standard input is not a terminal
.it restore
will wait for the data base to be free.
If the
.bd \-w
flag is set
.it restore
will not wait regardless of standard input.
If
.bd +w
is set it will always wait.
.s3
.it Restore
can recover a database from an update which had finished filling the
batch file.
Updates which did not make it to this stage should be rerun.
Similarly modifies which have finished recreating the relation will be completed
(the relation relation and attribute relations will be updated).
If a destroy was in progress it will be carried to completion, while a create
will almost always be backed out.
Destroying a relation with an index should destroy the index so
.it restore
may report
that a secondary relation has been found with no primary.
.s3
If interrupt (signal 2) is received the current database is closed and
the next, if any, is processed.
Quit (signal 3) will cause restore to terminate.
.sh EXAMPLE
restore \-f demo
.br
restore \-a grants < /dev/null
.sh DIAGNOSTICS
All diagnostics are followed by
a tuple from a system relations.
.s3
.in +5
.ti -5
``No relation for attribute(s)'' \- the attributes listed have no corresponding entry in the
relation relation
.ti -5
``No primary relation for index'' \- the tuple printed is the relation tuple for a secondary
index for which there is no primary relation.
The primary probably was destroyed the secondary will be.
.ti -5
``No indexes entry for primary relation'' \- the tuple is for a primary relation, the relindxd
domain will be set to zero.
This is the product of an incomplete destroy.
.ti -5
``No indexes entry for index'' \-
the tuple is for a secondary index, the index will be destroyed.
This is the product of an incomplete destroy.
.ti -5
``\c
.it relname
is index for'' \- an index has been found for a primary which is not marked as indexed.
The primary will be so marked.
This is probably the product of an incomplete index command.
The index will have been created properly but not modified.
.ti -5
``No file for'' \- There is no data for this relation tuple, the tuple will be
deleted.
If, under the
.bd \-a
option, the tuple is not deleted purge will not be called.
.ti -5
``No secondary index for indexes entry'' \- An entry has been found in the
indexes relation for which the secondary index does not exist (no relation
relation tuple).
The entry will be deleted.
.in -5
.sh "SEE ALSO"
purge(unix)
.sh BUGS
If no database names are given,
only the databases located in the directory
.bd data/base
are restored,
and not the old databases
in
.bd datadir.
Explicit database names still work
for databases
in either directory.
