.th INDCONV UNIX 12/13/77
.sh NAME
indconv - convert secondary indices so that they are compatible
with INGRES version 6.1.
.sh SYNOPSIS
INDCONV dbname
.sh DESCRIPTION
INDCONV will convert all secondary indices owned by the user
in database dbname to be compatible with version 6.1 of INGRES.
If more than user has secondary indices in the data base,
each user will have to run INDCONV.
INDCONV destroys the secondary indices, recreates them
and restores them to the proper storage structure and key ordering.
The names of the secondary indices are displayed on the user's terminal
when they have been successfully converted.
.sp
Once any secondary index is converted,
INGRES version 6.0 should NOT be run.
INGRES 6.0 cannot distinguish between 6.0 indices and 6.1 indices
and it will crash if it tries to use a 6.0 index.
.sp
If INGRES version 6.1 attempts to use a secondary index that has not
been converted, it will generate an error message
telling the user to convert secondary indices to version 6.1.
.sh DIAGNOSTICS
If there are more than 100 secondary indices,
INDCONV will display a message to that effect.
INDCONV will convert the first 100 secondary indices it finds.
When it is finished, the user should run INDCONV again
to convert the remaining secondary indices.
.sh "SEE ALSO"
index(quel), modify(quel)
.sh BUGS
There is no recovery program for when INDCONV crashes or is killed.
.sp
INDCONV cannot tell if the original secondary indices
were modified using the fillfactor, minpages or maxpages options.
