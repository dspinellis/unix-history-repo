.th DBTMPLT FILES
.sh NAME
\&.../files/dbtmplt7 \- database template
.sh DESCRIPTION
This file contains the template
for a database
used by
.it creatdb.
It has a set of entries
for each relation
to be created in the database.
The sets of entries
are separated
by a blank line.
Two blank lines
or an end of file
terminate the file.
.s3
The first line
of the file
is the database status
and the default relation status,
separated by a colon.
The rest of the file describes relations.
The first line of each group
gives the relation name
followed by an optional relation status,
separated by a colon.
The rest of the lines in each group
are the attribute name
and the type,
separated by a tab character.
.s3
All the status fields are given in octal,
and have a syntax of a single number
followed by a list of pairs of the form
.br
	\*(+-\c
.it x\c
\*(+-N
.br
which says that if the
\*(+-
.it x
flag is asserted on the
.it creatdb
command line
then set (clear) the bits
specified by
.it N.
.s3
The first set of entries must be for
the relation catalog,
and the second set
must be for
the attribute catalog.
.sh EXAMPLE
.nf
3\-c\-1+q+2:010023
relation:\-c\-20
relid	c12
relowner	c2
relspec	i1

attribute:\-c\-20
attrelid	c12
attowner	c2
attname	c12

	(other relation descriptors)
.fi
.sh "SEE ALSO"
creatdb(unix)
