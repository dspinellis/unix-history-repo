.NH
.IN "Linking Against Xlib"
Use of Xlib
.XS
Linking Against Xlib
.XE
.PP
To use Xlib, an include line of the form
.IN "File" "<X/Xlib.h>"
\fI#include <X/Xlib.h>\fP should be put near the top of a file
using the library.
This file defines data structures, macros,
and symbols you will likely need when programming X.
.IN "File" "<X/X.h>"
.IN "File" "<sys/types.h>"
This file also includes \fI<X/X.h>\fP, and \fI<sys/types.h>\fP.
.IN "Unix Command" "CC(1)"
To link an application to Xlib,
add a \fI-lX\fP option to your cc command.
.PP
.IN "File" "<X/Xlib.h>"
The types of most of the functions below are declared in \fIXlib.h\fP, so
that you do not have to declare their types in your program.
