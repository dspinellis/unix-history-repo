.th TTYTYPE FILES 3/28/79
.sh NAME
\&.../files/ttytype \- \*(GQ terminal type database
.sh DESCRIPTION
The
.bd ttytype
file describes
each terminal on your system.
\*(GQ will
not attempt to display graphical output on terminals
that are not capable of displaying it.
There is a sample of the file in
.bd \&.../geoquel/ttytype.sample.
This is a copy of the file in use on the Berkeley system.
.s3
The
.bd ttytype
file consists of a series of
lines;
the first character is the terminal id,
and the rest of the line tells
the type of the terminal.
The first of these characters is a terminal class,
and the rest signify the brand, or some other more
descriptive indication.
A completely blank line terminates the useful part
of the file, after which comments may 
appear unrestricted.
In the sample file the currently recognized
(defined) terminal types are listed as comments.
.sh BUGS
The current version of \*(GQ only looks for terminals
of graphic nature and therefore only the graphic terminals
need be in this database.  If any other system or sub-system
wishes to use this file, all terminals must be kept up to date.
