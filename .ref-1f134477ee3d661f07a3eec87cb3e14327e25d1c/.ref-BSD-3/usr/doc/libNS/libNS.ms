.TL
A Modified Standard I/O Library Allowing Concurrent Read/Write
Access to Streams.
.AU
Keith Sklower
.AI
Dept. of Electrical Engineering and Computer Science
University of California
Berkeley, California 94720
.NH
Purpose
.PP
It is sometimes necessary for a program to maintain existing files,
in a manner that requires reading
as well as writing.  The existing standard I/O library
does not permit one to process a stream in this manner, by use
of a single device.  It is possible to access a file using the existing
library either by maintaining two descriptor objects, and flushing after
every write, or by continually closing and reopening the file with different
modes.
.PP
This seems inconvenient, to say the least.
Thus it was desired to make extensions to the existing library so that
might open a file with a new mode, say for example
.DS
stream = fopen(filename,"rw");
.DE
and to use the routines to seek, or to extract and write characters
without having to worry about flushing or reopening.
.NH
Differences from the existing library
.PP
This package is strictly upwards compatible with the existing library,
so that programs may be recompiled without change, although at some slight
loss of efficiency in writing files.  The routines fopen and freopen have
been changed so that two new modes may be recognized.  If one says, as above,
.DS
stream = fopen(filename,"rw");
.DE
a new file will be created as in fopen(filename,"w");
but one may intermix calls to fseek, getc, and putc without further ado.
.PP
If one opens a stream with mode "ra" an existing file will be opened
without truncation as in fopen(filename,"a"), and if none exists
then one will similarly be created.  The pointer is likewise
placed at the end of the file.
.NH
Employment of the package
.PP
C programs intending to use this package should contain the line
.DS
#include <nstdio.h>
.DE
rather than <stdio.h>, and should have the library /usr/lib/libNS.a
searched.  This is accomplished by
.DS
cc  ...  \-lNS
.DE
