.th GRAFILE FILES
.sh NAME
\&.../files/grafile7 \- \*(GQ login message
.sh DESCRIPTION
The contents of the grafile (\*(GQ dayfile)
reflect
user information of general system interest, and is
more or less analogous to
.bd /etc/motd
in \*(UU.
The file has no set
format;
it is simply copied at login time to the standard
output device by \*(GQ if the
.bd \-s
or
.bd \-d
options
have not been requested.
Moreover the grafile is not mandatory, and
its absence will not generate errors of any sort;
the same is true when the
grafile is present but not readable.
