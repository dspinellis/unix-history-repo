.th GEO-QUEL UNIX 3/14/79
.if n .ds GQ GEO-QUEL
.if t .ds GQ \s-2GEO-QUEL\s0
.sh NAME
geoquel \- \*(GQ data display system
.sh SYNOPSIS
.bd geoquel
[
.bd \-s
] [
.bd \-d
] [
.bd \-a
] [
.bd \-t\c
.it T
] [ 
.bd \-tn\c
.it T
] dbname
.sh DESCRIPTION
\*(GQ is a geographical interface to \*(II.
.s3
.it Dbname
is the name of an existing data base.
.s3
The format of the graphic output depends upon
the type of terminal in use.
\*(GQ will
look up the terminal type at login time and
produce output appropriate for that terminal.
If the terminal in use is incapable of drawing
graphic output then a display list is
generated for a Tektronix 4014 but will only be
displayed if the results are saved with
.it savemap
and then re-displayed.
.s3
The flags are interpreted as follows:
.s3
.lp +8 8
\fB\-s\fP	Don't print any of the monitor messages,
including prompts.
This is inclusive of the dayfile.
.lp +8 8
\fB\-d\fP	Don't print the dayfile.
.lp +8 8
\fB\-a\fP	Disable the autoclear function in the terminal
monitor.
.lp +8 8
\fB\-t\fP\fIT\fP	Set the terminal type to
.it T.
.it T
can be
.bd gt40,
.bd gt42,
or
.bd 4014
for DEC's GT40-GT42, and Tektronix's 4014 respectively.
.lp +8 8
\fB\-tn\fP\fIT\fP	Do not display output
but prepare the display list for a terminal of type
.it T,
where
.it T
is from the above list.
.sh EXAMPLE
.nf
geoquel demo
geoquel \-d demo
geoquel \-s demo < batchfile
.fi
.sh FILES
\&.../files/grafile7
.br
\&.../files/ttytype
.sh "SEE ALSO"
\*(GQ reference manual
.sh DIAGNOSTICS
The diagnostics produced by \*(GQ are intended to be
self-explanatory.
Occasional messages may be produced by \*(II;
for an explanation of these messages see the \*(II system documentation.
