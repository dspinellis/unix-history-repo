.th HELP QUEL 2/21/79
.sh NAME
help \- get information about how to use \*(II or about relations in the database.
.sh SYNOPSIS
.bd help 
[
relname
] [
"section"
] 
{, relname}{, "section"}
.br
.bd "help view"
relname {, relname}
.br
.bd "help permit" 
relname {, relname}
.br
.bd "help integrity"
relname {, relname}
.sh DESCRIPTION
.it Help 
may be used to obtain sections of
this manual, information on the content of the current data base,
information about specific relations in the data base, view definitions,
or protection and integrity constraints on a relation.
The legal forms are as follow:
.s3
.if n .in +5
.if t .in +.5i
.de xx
.if n .ti -5
.if t .ti -.5i
..
.xx
.bd "help ""\c"
.it section \c
.bd """"
\- Produces a copy of the specified section of the
\*(II Reference Manual, and prints it on the standard output
device.
.xx
.bd help
\-  Gives information about all relations that exist in the
current database.
.xx
.bd help
.it "relname {, relname}"
\- Gives information about the
specified relations.
.xx
.bd "help ""\*|"""
\- Gives the table of contents.
.xx
.bd "help view"
.it "relname {, relname}"
\- Prints view definitions of specified views.
.xx
.bd "help permit"
.it "relname {, relname}"
\- Prints permissions on specified relations.
.xx
.bd "help integrity"
.it "relname {, relname}"
\- Prints integrity constraints on specified relations.
.if n .in -5
.if t .in -.5i
.s3
The 
.bd permit
and 
.bd integrity
forms print out unique identifiers for each constraint.
These identifiers may be used to 
remove
the constraints with the
.it destroy 
statement.
.sh EXAMPLE
help
.br
help help  /\*(** prints this page of the manual \*(**/
.br
help quel
.br
help emp
.br
help emp, parts, "help", supply
.br
help view overp_view
.br
help permit parts, employee
.br
help integrity parts, employee
.br
.i0
.sh "SEE ALSO"
destroy(quel)
.sh BUGS
Alphabetics appearing within the
section name
must be in lower-case to be recognized.
