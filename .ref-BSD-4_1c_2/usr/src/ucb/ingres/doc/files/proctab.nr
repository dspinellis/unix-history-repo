.th PROCTAB FILES
.sh NAME
\&.../files/proctab7 \- \*(II runtime configuration information
.sh DESCRIPTION
The process table
describes the runtime configuration
of the \*(II system.
Each line of the process table
has a special meaning depending on the first
character of the line.
Blank lines
and lines beginning with an asterisk
are comments.
All other lines have a sequence of fields
separated by commas.
Pipe descriptor fields
are lower case letters
or digits;
if they are digits
they are replaced by file descriptors
from the \*(EE flag
or the @ flag.
.s3
.bd D
defines a macro.
The first field is a single character
macro name.
The second field is the string to use as the value.
Macros are expanded using ``$\fIx\fP''
where
.it x
is the macro name.
The macro ``P'' is predefined
to be the pathname of the \*(II subtree.
.s3
.bd P
introduces a process description.
All lines up to an end of file
or another P line
describe a single process.
The first field is the process number.
The next field is the pathname of the binary
to execute for this process.
The third field is the name of the process
to use for printing messages.
The fourth field must be a single character
lower case letter
representing the input pipe that is normally read
when nothing special is happening,
or a vertical bar followed by a single digit,
meaning to read from that file descriptor.
The next field is a set of flags in octal
regarding processing of this process;
these are described below.
The final field is a single letter
telling what trace flag this process uses.
.s3
.bd L
defines what modules are defined locally
by this process.
The first field is the module number
used internally.
The second field is a set of flags
describing processing of this module:
the only bit defined is the 0001 bit
which allows this module to be executed
directly by the user.
The third field is the function number
in the process which defines this module.
The final field is the module number
to be executed after this module completes;
zero is nothing (return).
.s3
.bd R
defines modules that are known to this process
but which must be passed to another process
for execution.
The first field is the process number the modules
will be found in.
The second field is the pipe to write
to get to that process.
The third field is the pipe to read to get a response
from that process.
The fourth field is a set of flags:
0001 means to write the output pipe
if you get a broadcast message,
0002 means that the process is physically adjacent
on the read pipe,
and 0004 means that the process is adjacent
on the write pipe.
The fifth and subsequent fields are the module numbers
that are defined by this process.
.s3
The status bits for the
.bd P
line are as follows:
.nf
.s3
	000010	close diagnostic output
	000004	close standard input
	000002	run in user's directory, not database
	000001	run as the user, not as \*(II
.s3
.fi
.s3
The lowest numbered process
becomes the parent
of all the other processes.
.s1
.bd WARNING:
Giving a user permission to specify his or her own process table
will allow them to bypass all protection provided by \*(II.
This facility should be provided for system debugging only!
.sh "EXAMPLE"
The following example
will execute a three process system.
.s3
.nf
.in +4
DB:$P/bin
DS:$P/source
****  Process 0 -- terminal monitor
P0:$B/monitor:MONITOR:h:0003:M
L0:0:0:0
R1:0:a:h:0007:1
*  Process 1 -- parser
P1:$B/parser:PARSER:a:0014:P
L3:1:0:0
R0:0:h:a:0006:0
R2:0:b:g:0007:5:6:7
*  Process 2 -- data base utilities
P2:$B/alldbu:DBU:b:0014:Z
L5:0:6:0
L6:0:0:0
L7:0:1:0
R0:0:g:b:0000
R1:0:g:b:0006
.fi
.in -4
