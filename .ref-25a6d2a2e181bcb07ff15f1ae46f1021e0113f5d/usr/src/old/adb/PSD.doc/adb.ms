.\"	@(#)adb.ms	6.1 (Berkeley) %G%
.\"
.EH 'PS1:10-%''A Tutorial Introduction to ADB'
.OH 'A Tutorial Introduction to ADB''PS1:10-%'
.de P1
.sp .5
.if \\n(.$>0 .ta \\$1 \\$2 \\$3 \\$4 \\$5 \\$6
.if \\n(.$=0 .ta 1i 1.7i 2.5i
.ft 3
.nf
..
.de P2
.sp .5
.ft 1
.fi
..
.\".RP
.....TM "77-8234-11 77-1273-10" "49170-220 39199" "40952-1 39199-11"
.ND May 5, 1977
.TL
A Tutorial Introduction to ADB
.AU "MH2F-207" "3816"
J. F. Maranzano
.AU "MH2C-512" 7419
S. R. Bourne
.AI
.MH
.OK
.\"UNIX
.\"Debugging
.\"C Programming
.AB
.PP
Debugging tools generally provide a wealth of information
about the inner workings of programs.
These tools have been available on
.UX
to allow users to
examine ``core'' files 
that result from aborted programs.
A new debugging program, ADB, provides enhanced capabilities
to examine "core" and other program files in a
variety of formats, run programs with embedded breakpoints and patch files.
.PP
ADB is an indispensable but complex tool for debugging crashed systems and/or
programs.
This document provides an introduction to ADB with examples of its use.
It explains the various formatting options, 
techniques for debugging C programs, examples of printing
file system information and patching.
.AE
.CS 12 15 27 13 0 5
.NH
Introduction
.PP
ADB is a new debugging program that is
available on UNIX.
It provides capabilities to look at
``core'' files resulting from aborted programs, print output in a
variety of formats, patch files, and run programs
with embedded breakpoints.
This document provides examples of
the more useful features of ADB.
The reader is expected to be
familiar with the basic commands on
.UX
with the C
language, and with References 1, 2 and 3.
.NH
A Quick Survey
.NH 2
Invocation
.PP
ADB is invoked as:
.P1
	adb objfile corefile
.P2
where
.ul
objfile
is an executable UNIX file and 
.ul
corefile 
is a core image file.
Many times this will look like:
.P1
	adb a.out core
.P2
or more simply:
.P1
	adb
.P2
where the defaults are 
.ul
a.out
and
.ul
core
respectively.
The filename minus (\-) means ignore this argument as in:
.P1
	adb \- core
.P2
.PP
ADB has requests for examining locations in either file.
The
\fB?\fP
request examines the contents of 
.ul
objfile,
the
\fB/\fP
request examines the 
.ul
corefile.
The general form of these requests is:
.P1
	address ? format
.P2
or
.P1
	address / format
.P2
.NH 2
Current Address
.PP
ADB maintains a current address, called dot,
similar in function to the current pointer in the UNIX editor.
When an address is entered, the current address is set to that location,
so that:
.P1
	0126?i
.P2
sets dot to octal 126 and prints the instruction
at that address.
The request:
.P1
	.,10/d
.P2
prints 10 decimal numbers starting at dot.
Dot ends up referring to the address of the last item printed.
When used with the \fB?\fP or \fB/\fP requests,
the current address can be advanced by typing newline; it can be decremented
by typing \fB^\fP.
.PP
Addresses are represented by
expressions.
Expressions are made up from decimal, octal, and hexadecimal integers,
and symbols from the program under test.
These may be combined with the operators +, \-, *, % (integer division), 
& (bitwise and), | (bitwise inclusive or),  # (round up
to the next multiple), and ~ (not).
(All arithmetic within ADB is 32 bits.)
When typing a symbolic address for a C program, 
the user can type 
.ul
name
or
.ul
_name;
ADB will recognize both forms.
.NH 2
Formats
.PP
To print data, a user specifies a collection of letters and characters
that describe the format of the printout.
Formats are "remembered" in the sense that typing a request without one
will cause the new printout to appear in the previous format.
The following are the most commonly used format letters.
.P1
\fB	b	\fPone byte in octal
\fB	c	\fPone byte as a character
\fB	o	\fPone word in octal
\fB	d	\fPone word in decimal
\fB	f	\fPtwo words in floating point
\fB	i	\fPPDP 11 instruction
\fB	s	\fPa null terminated character string
\fB	a	\fPthe value of dot
\fB	u	\fPone word as unsigned integer
\fB	n	\fPprint a newline
\fB	r	\fPprint a blank space
\fB	^	\fPbackup dot
.P2
(Format letters are also available for "long" values,
for example, `\fBD\fR' for long decimal, and `\fBF\fP' for double floating point.)
For other formats see the ADB manual.
.NH 2
General Request Meanings
.PP
The general form of a request is:
.P1
	address,count command modifier
.P2
which sets `dot' to \fIaddress\fP
and executes the command
\fIcount\fR times.
.PP
The following table illustrates some general ADB command meanings:
.P1
	Command	Meaning
\fB	    ?	\fPPrint contents from \fIa.out\fP file
\fB	    /	\fPPrint contents from \fIcore\fP file
\fB	    =	\fPPrint value of "dot"
\fB	    :	\fPBreakpoint control
\fB	    $	\fPMiscellaneous requests
\fB	    ;	\fPRequest separator
\fB	    !	\fPEscape to shell
.P2
.PP
ADB catches signals, so a user cannot use a quit signal to exit from ADB.
The request $q or $Q (or cntl-D) must be used
to exit from ADB.
.NH
Debugging C Programs
.NH 2
Debugging A Core Image 
.PP
Consider the C program in Figure 1.
The program is used to illustrate a common error made by
C programmers.
The object of the program is to change the
lower case "t" to upper case in the string pointed to by
.ul
charp
and then write the character string to the file indicated by
argument 1.
The bug shown is that the character "T"
is stored in the pointer 
.ul
charp
instead of the string pointed to by
.ul
charp.
Executing the program produces a core file because of an out of bounds memory reference.
.PP
ADB is invoked by:
.P1
	adb a.out core
.P2
The first debugging request:
.P1
	$c
.P2
is used to give a C backtrace through the
subroutines called.
As shown in Figure 2
only one function (\fImain\fR) was called and the
arguments 
.ul
argc 
and 
.ul
argv 
have octal values 02 and
0177762 respectively.
Both of these values look
reasonable; 02 = two arguments, 0177762 = address on stack
of parameter vector.
.br
The next request:
.P1
	$C
.P2
is used to give a C backtrace plus an interpretation
of all the local variables in each function and their
values in octal.
The value of the variable 
.ul
cc
looks incorrect
since
.ul
cc
was declared as a character.
.PP
The next request:
.P1
	$r
.P2
prints out the registers including the program
counter and an interpretation of the instruction at that
location.
.PP
The request:
.P1
	$e
.P2
prints out the values of all external variables.
.PP
A map exists for each file
handled by
ADB.
The map for the
.ul
a.out
file is referenced by \fB?\fP whereas the map for 
.ul
core
file is referenced by \fB/\fP.
Furthermore, a good rule of thumb is to use \fB?\fP for
instructions and \fB/\fP for data when looking at programs.
To print out information about the maps type:
.P1
	$m
.P2
This produces a report of the contents of the maps.
More about these maps later.
.PP
In our example, it is useful to see the
contents of the string pointed to by
.ul
charp.
This is done by:
.P1
	*charp/s
.P2
which says use 
.ul
charp
as a pointer in the
.ul
core
file
and print the information as a character string.
This printout clearly shows that the character buffer
was incorrectly overwritten and helps identify the error.
Printing the locations around 
.ul
charp
shows that the buffer is unchanged
but that the pointer is destroyed.
Using ADB similarly, we could print information about the
arguments to a function.
The request:
.P1
	main.argc/d
.P2
prints the decimal 
.ul
core
image value of the argument 
.ul
argc
in the function 
.ul
main.
.br
The request:
.P1
	*main.argv,3/o
.P2
prints the octal values of the three consecutive
cells pointed to by 
.ul
argv
in the function 
.ul
main.
Note that these values are the addresses of the arguments
to main.
Therefore: 
.P1
	0177770/s
.P2
prints the ASCII value of the first argument.
Another way to print this value would have been
.P1
	*"/s
.P2
The " means ditto which remembers the last address
typed, in this case \fImain.argc\fP ; the \fB*\fP instructs ADB to use the address field of the
.ul
core 
file as a pointer.
.PP
The request:
.P1
	.=o
.P2
prints the current address (not its contents) in octal which has been set to the address of the first argument.
The current address, dot, is used by ADB to
"remember" its current location.
It allows the user 
to reference locations relative to the current
address, for example:
.P1
	.\-10/d
.P2
.NH 2
Multiple Functions
.PP
Consider the C program illustrated in
Figure 3.
This program calls functions 
.ul
f, g,
and
.ul
h 
until the stack is exhausted and a core image is produced.
.PP
Again you can enter the debugger via:
.P1
	adb
.P2
which assumes the names 
.ul
a.out
and 
.ul
core
for the executable
file and core image file respectively.
The request:
.P1
	$c
.P2
will fill a page of backtrace references to 
.ul
f, g,
and
.ul
h.
Figure 4 shows an abbreviated list (typing 
.ul
DEL
will terminate the output and bring you back to ADB request level).
.PP
The request:
.P1
	,5$C
.P2
prints the five most recent activations.
.PP
Notice that each function 
(\fIf,g,h\fP) has a counter
of the number of times it was called.
.PP
The request:
.P1
	fcnt/d
.P2
prints the decimal value of the counter for the function
.ul
f.
Similarly 
.ul
gcnt
and
.ul
hcnt
could be printed.
To print the value of an automatic variable,
for example the decimal value of
.ul 
x
in the last call of the function
.ul
h,
type:
.P1
	h.x/d
.P2
It is currently not possible in the exported version to print stack frames other than the most recent activation of a function.
Therefore, a user can print everything with 
\fB$C\fR or the occurrence of a variable in the most recent call of a function.
It is possible with the \fB$C\fR request, however, to print the stack frame
starting at some address as \fBaddress$C.\fR
.NH 2
Setting Breakpoints
.PP
Consider the C program in Figure 5.
This program, which changes tabs into blanks, is adapted from
.ul
Software Tools
by Kernighan and Plauger, pp. 18-27.
.PP
We will run this program under the control of ADB (see Figure 6a) by:
.P1
	adb a.out \-
.P2
Breakpoints are set in the program as:
.ul
.P1
	address:b  [request]
.P2
The requests:
.P1
	settab+4:b
	fopen+4:b
	getc+4:b
	tabpos+4:b
.P2
set breakpoints at the start of these functions.
C does not generate statement labels.
Therefore it is currently not possible to plant breakpoints at locations
other than function entry points without a knowledge of the code
generated by the C compiler.
The above addresses are entered as
.ft B
symbol+4
.ft R
so that they will appear in any
C backtrace since the first instruction of each function is a call
to the C save routine 
(\fIcsv\fR).
Note that some of the functions are from the C library.
.PP
To print the location of breakpoints one types:
.P1
	$b
.P2
The display indicates a
.ul
count
field.
A breakpoint is bypassed
.ul
count \-1
times before causing a stop.
The
.ul
command
field indicates the ADB requests to be executed each time the breakpoint is encountered.
In our example no
.ul
command
fields are present.
.PP
By displaying the original instructions at the function
.ul
settab
we see that 
the breakpoint is set after the jsr to the C save routine.
We can display the instructions using the ADB request:
.P1
	settab,5?ia
.P2
This request displays five instructions starting at
.ul
settab
with the addresses of each location displayed.
Another variation is:
.P1
	settab,5?i
.P2
which displays the instructions with only the starting address.
.PP
Notice that we accessed the addresses from the 
.ul
a.out 
file with the \fB?\fP command.
In general when asking for a printout of multiple items,
ADB will advance the current address the number of
bytes necessary to satisfy the request; in the above
example five instructions were displayed and the current address was
advanced 18 (decimal) bytes.
.PP
To run the program one simply types:
.P1
	:r
.P2
To delete a breakpoint, for instance the entry to the function
.ul
settab,
one types:
.P1
	settab+4:d
.P2
To continue execution of the program from the breakpoint type:
.P1
	:c
.PP
Once the program has stopped (in this case at the breakpoint for
.ul
fopen),
ADB requests can be used to display the contents of memory.
For example:
.P1
	$C
.P2
to display a stack trace, or:
.P1
	tabs,3/8o
.P2
to print three lines of 8 locations each from the array called
.ul
tabs.
By this time (at location
.ul
fopen)
in the C program,
.ul
settab
has been called and should have set a one in every eighth location of 
.ul
tabs.
.NH 2
Advanced Breakpoint Usage
.PP
We continue execution of the program with:
.P1
	:c
.P2
See Figure 6b.
.ul
Getc
is called three times and  the contents of the variable 
.ul
c
in the function
.ul
main
are displayed
each time.
The single character on the left hand edge is the output from the C program.
On the third occurrence of 
.ul
getc
the program stops.
We can look at the full buffer of characters by typing:
.P1
	ibuf+6/20c
.P2
When we continue the program with:
.P1
	:c
.P2
we hit our first breakpoint at
.ul
tabpos
since there is a tab following the
"This" word of the data.
.PP
Several breakpoints of
.ul
tabpos
will occur until the program has changed the tab into equivalent blanks.
Since we feel that
.ul
tabpos
is working,
we can remove the breakpoint at that location by:
.P1
	tabpos+4:d
.P2
If the program is continued with:
.P1
	:c
.P2
it resumes normal execution after ADB prints
the message
.P1
	a.out:running
.P2
.PP
The UNIX quit and interrupt signals
act on ADB itself rather than on the program being debugged.
If such a signal occurs then the program being debugged is stopped and control is returned to ADB.
The signal is saved by ADB and is passed on to the test program if:
.P1
	:c
.P2
is typed.
This can be useful when testing interrupt
handling routines.
The signal is not passed on to the test program if:
.P1
	:c  0
.P2
is typed.
.PP
Now let us reset the breakpoint at
.ul
settab
and display the instructions located there when we reach the breakpoint.
This is accomplished by:
.P1
	settab+4:b  settab,5?ia  \fR*
.P2
.FS
* Owing to a bug in early versions of ADB (including the
version distributed in Generic 3 UNIX) these statements
must be written as:
.br
.in 1i
\fBsettab+4:b	settab,5?ia;0\fR
.ft B
.br
getc+4,3:b	main.c?C;0
.br
settab+4:b	settab,5?ia; ptab/o;0
.br
.ft R
.in -1i
Note that \fB;0\fR will set dot to zero and stop at the breakpoint.
.FE
It is also possible to execute the ADB requests for each occurrence of the breakpoint but
only stop after the third occurrence by typing:
.P1
	getc+4,3:b  main.c?C  \fR*
.P2
This request will print the local variable 
.ul
c
in the function 
.ul
main
at each occurrence of the breakpoint.
The semicolon is used to separate multiple ADB requests on a single line.
.PP
Warning:
setting a breakpoint causes the value of dot to be changed;
executing the program under ADB does not change dot.
Therefore:
.P1
	settab+4:b  .,5?ia
	fopen+4:b
.P2
will print the last thing dot was set to
(in the example \fIfopen+4\fP)
.ul
not
the current location (\fIsettab+4\fP)
at which the program is executing.
.PP
A breakpoint can be overwritten without first deleting the old breakpoint.
For example:
.P1
	settab+4:b  settab,5?ia; ptab/o  \fR*
.P2
could be entered after typing the above requests.
.PP
Now the display of breakpoints:
.P1
	$b
.P2
shows the above request for the
.ul
settab
breakpoint.
When the breakpoint at
.ul
settab
is encountered the ADB requests are executed.
Note that the location at
.ul
settab+4
has been changed to plant the breakpoint;
all the other locations match their original value.
.PP
Using the functions,
.ul
f, g
and 
.ul
h
shown in Figure 3,
we can follow the execution of each function by planting non-stopping
breakpoints.
We call ADB with the executable program of Figure 3 as follows:
.P1
	adb ex3 \-
.P2
Suppose we enter the following breakpoints:
.P1
	h+4:b	hcnt/d;  h.hi/;  h.hr/
	g+4:b	gcnt/d;  g.gi/;  g.gr/
	f+4:b	fcnt/d;  f.fi/;  f.fr/
	:r
.P2
Each request line indicates that the variables are printed in decimal
(by the specification \fBd\fR).
Since the format is not changed, the \fBd\fR can be left off all but
the first request.
.PP
The output in Figure 7 illustrates two points.
First, the ADB requests in the breakpoint line are not
examined until the program under
test is run.
That means any errors in those ADB requests is not detected until run time.
At the location of the error ADB stops running the program.
.PP
The second point is the way ADB handles register variables.
ADB uses the symbol table to address variables.
Register variables, like \fIf.fr\fR above, have pointers to uninitialized
places on the stack.
Therefore the message "symbol not found".
.PP
Another way of getting at the data in this example is to print
the variables used in the call as:
.P1
	f+4:b	fcnt/d;  f.a/;  f.b/;  f.fi/
	g+4:b	gcnt/d;  g.p/;  g.q/;  g.gi/
	:c
.P2
The operator / was used instead of ?
to read values from the \fIcore\fP file.
The output for each function, as shown in Figure 7, has the same format.
For the function \fIf\fP, for example, it shows the name and value of the
.ul
external
variable
.ul
fcnt.
It also shows the address on the stack and value of the
variables
.ul
a, b
and
.ul
fi.
.PP
Notice that the addresses on the stack will continue to decrease
until no address space is left for program execution
at which time (after many pages of output)
the program under test aborts.
A display with names would be produced by requests like the following:
.P1
	f+4:b	fcnt/d;  f.a/"a="d;  f.b/"b="d;  f.fi/"fi="d
.P2
In this format the quoted string is printed literally and the \fBd\fP
produces a decimal display of the variables.
The results are shown in Figure 7.
.NH 2
Other Breakpoint Facilities
.LP
.IP \(bu 4
Arguments and change of standard input and output are passed to a program as:
.P1
	:r  arg1  arg2 ... <infile  >outfile
.P2
This request
kills any existing program under test and
starts the
.ul
a.out
afresh.
.IP \(bu
The program being debugged can be single stepped
by:
.P1
	:s
.P2
If necessary, this request will start up the program being
debugged and stop after executing
the first instruction.
.IP \(bu
ADB allows a program to be entered at a specific address
by typing:
.P1
	address:r
.P2
.IP \(bu
The count field can be used to skip the first \fIn\fR breakpoints as:
.P1
	,n:r
.P2
The request:
.P1
	,n:c
.P2
may also be used for skipping the first \fIn\fR breakpoints
when continuing a program.
.sp
.IP \(bu
A program can be continued at an address different from the breakpoint by:
.P1
	address:c
.P2
.IP \(bu
The program being debugged runs as a separate process and can be killed by:
.P1
	:k
.P2
.LP
.NH
Maps
.PP
UNIX supports several executable file formats.  These are used to tell
the loader how to load  the program file.  File type 407
is the most common and is generated by a C compiler invocation such as
\fBcc pgm.c\fP.
A 410 file is produced by a C compiler command of the form \fBcc -n pgm.c\fP,
whereas a 411 file is produced by \fBcc -i pgm.c\fP.
ADB interprets these different file formats and
provides access to the different segments through a set of maps (see Figure 8).
To print the maps type:
.P1
	$m
.P2
.PP
In 407 files, both text (instructions) and data are intermixed.
This makes it impossible for ADB to differentiate data from
instructions and some of the printed symbolic addresses look incorrect;
for example, printing data addresses as offsets from routines.
.PP
In 410 files (shared text), the instructions are separated from data and
\fB?*\fR accesses the data part of the \fIa.out\fP file.
The \fB?* \fP request tells ADB to use the second part of the
map in the
.ul
a.out
file.
Accessing data in the \fIcore\fP file shows
the data after it was modified by the execution of the program.
Notice also that the data segment may have grown during
program execution.
.PP
In 411 files (separated I & D space), the
instructions and data are also separated.
However, in this
case, since data is mapped through a separate set of segmentation
registers, the base of the data segment is also relative to address zero.
In this case since the addresses overlap it is necessary to use
the \fB?*\fR operator to access the data space of the \fIa.out\fP file.
In both 410 and 411 files the corresponding
core file does not contain the program text.
.PP
Figure 9 shows the display of three maps
for the same program linked as a 407, 410, 411 respectively.
The b, e, and f fields are used by ADB to map
addresses into file addresses.
The "f1" field is the
length of the header at the beginning of the file (020 bytes
for an \fIa.out\fP file and 02000 bytes for a \fIcore\fP file).
The "f2" field is the displacement from the beginning of the file to the data.
For a 407 file with mixed text and data this is the
same as the length of the header; for 410 and 411 files this
is the length of the header plus the size of the text portion.
.PP
The "b" and "e" fields are the starting and ending locations
for a segment.
Given an address, A, the location in
the file (either \fIa.out\fP or \fIcore\fP) is calculated as:
.P1
	b1\(<=A\(<=e1 =\h'-.5m'> file address = (A\-b1)+f1
	b2\(<=A\(<=e2 =\h'-.5m'> file address = (A\-b2)+f2
.P2
A user can access locations by using the ADB defined variables.
The \fB$v\fR request prints the variables initialized by ADB:
.P1
	b	base address of data segment
	d	length of the data segment
	s	length of the stack
	t	length of the text
	m	execution type (407,410,411)
.P2
.PP
In Figure 9 those variables not present are zero.
Use can be made of these variables by expressions such as:
.P1
	<b
.P2
in the address field.
Similarly the value of the variable can be changed by an assignment request
such as:
.P1
	02000>b
.P2
that sets \fBb\fP to octal 2000.
These variables are useful to know if the file under examination
is an executable or \fIcore\fP image file.
.PP
ADB reads the header of the \fIcore\fP image file to find the
values for these variables.
If the second file specified does not
seem to be a \fIcore\fP file, or if it is missing then the header of
the executable file is used instead.
.NH
Advanced Usage
.PP
It is possible with ADB to combine formatting requests
to provide elaborate displays.
Below are several examples.
.NH 2
Formatted dump
.PP
The line:
.P1
	<b,\-1/4o4^8Cn
.P2
prints 4 octal words followed by their ASCII interpretation
from the data space of the core image file.
Broken down, the various request pieces mean:
.sp
.in 1.7i
.ta .7i
.ti -.7i
<b	The base address of the data segment.
.sp
.ti -.7i
<b,\-1	Print from the base address to the end of file.
A negative count is used here and elsewhere to loop indefinitely
or until some error condition (like end of file) is detected.
.sp
.ti -1.7i
The format \fB4o4^8Cn\fR is broken down as follows:
.sp
.ti -.7i
4o	Print 4 octal locations.
.sp
.ti -.7i
4^	Backup the current address 4 locations (to the original start of the field).
.sp
.ti -.7i
8C	Print 8 consecutive characters using an escape convention;
each character in the range 0 to 037 is printed as @ followed by the corresponding character in the range 0140 to 0177.
An @ is printed as @@.
.sp
.ti -.7i
n	Print a newline.
.in -1.7i
.fi
.sp
.PP
The request:
.P1
	<b,<d/4o4^8Cn
.P2
could have been used instead to allow the printing to stop
at the end of the data segment (<d provides the data segment size in bytes).
.PP
The formatting requests can be combined with ADB's ability
to read in a script to produce a core image dump script.
ADB is invoked as:
.P1
	adb a.out core < dump
.P2
to read in a script file,
.ul
dump,
of requests.
An example of such a script is:
.P1
	120$w
	4095$s
	$v
	=3n
	$m
	=3n"C Stack Backtrace"
	$C
	=3n"C External Variables"
	$e
	=3n"Registers"
	$r
	0$s
	=3n"Data Segment"
	<b,\-1/8ona
.P2
.PP
The request \fB120$w\fP sets the width of the output to
120 characters
(normally, the width is 80 characters).
ADB attempts to print addresses as:
.P1
	symbol + offset
.P2
The request \fB4095$s\fP increases the maximum permissible offset
to the nearest symbolic address from 255 (default) to 4095.
The request \fB=\fP can be used to print literal strings.
Thus,
headings are provided in this
.ul
dump
program
with requests of the form:
.P1
	=3n"C Stack Backtrace"
.P2
that spaces three lines and prints the literal
string.
The request \fB$v\fP prints all non-zero ADB variables (see Figure 8).
The request
\fB0$s\fP
sets the maximum offset for symbol matches to zero thus
suppressing the printing of symbolic labels in favor
of octal values.
Note that this is only done for the printing of the data segment.
The request:
.P1
	<b,\-1/8ona
.P2
prints a dump from the base of the data segment to the end of file
with an octal address field and eight octal numbers per line.
.PP
Figure 11 shows the results of some formatting requests
on the C program of Figure 10.
.NH 2
Directory Dump
.PP
As another illustration (Figure 12) consider a set of requests to dump
the contents of a directory (which is made up
of an integer \fIinumber\fP followed by a 14 character name):
.P1
	adb dir \-
	=n8t"Inum"8t"Name"
	0,\-1? u8t14cn
.P2
In this example, the \fBu\fP prints the \fIinumber\fP as an unsigned decimal integer,
the \fB8t\fP means that ADB will space to the next
multiple of 8 on the output line, and the \fB14c\fP prints the 14 character file name.
.NH 2
Ilist Dump
.PP
Similarly the contents of the \fIilist\fP of a file system, (e.g. /dev/src,
on UNIX systems distributed by the UNIX Support Group;
see UNIX Programmer's
Manual Section V) could be dumped with the following set of 
requests:
.P1
	adb /dev/src \-
	02000>b
	?m <b
	<b,\-1?"flags"8ton"links,uid,gid"8t3bn",size"8tbrdn"addr"8t8un"times"8t2Y2na
.P2
In this example the value of the base for the map was changed 
to 02000 (by saying \fB?m<b\fR) since that is the start of an \fIilist\fP within a file system.
An artifice (\fBbrd\fP above) was used to print the 24 bit size field
as a byte, a space, and a decimal integer.
The last access time and last modify time are printed with the
\fB2Y\fR
operator.
Figure 12 shows portions of these requests as applied to a directory
and file system.
.NH 2
Converting values
.PP
ADB may be used to convert values from one representation to
another.
For example:
.P1
	072 = odx
.P2
will print
.P1
	072	58	#3a
.P2
which is the octal, decimal and hexadecimal representations
of 072 (octal).
The format is remembered so that typing
subsequent numbers will print them in the given formats.
Character values may be converted similarly, for example:
.P1
	'a' = co
.P2
prints
.P1
	a	0141
.P2
It may also be used to evaluate expressions but be
warned that all binary operators have
the same precedence which is lower than that for unary operators.
.NH
Patching
.PP
Patching files with ADB is accomplished with the 
.ul
write,
\fBw\fP or \fBW\fP, request (which is not like the \fIed\fP editor write command).
This is often used in conjunction with the 
.ul
locate,
\fBl\fP or \fBL\fP
request.
In general, the request syntax for \fBl\fP and \fBw\fP are similar as follows:
.P1
	?l value
.P2
The request \fBl\fP is used to match on two bytes, \fBL\fP is used for
four bytes.
The request \fBw\fP is used to write two bytes, whereas
\fBW\fP writes four bytes.
The \fBvalue\fP field in either 
.ul
locate
or
.ul
write
requests
is an expression.
Therefore, decimal and octal numbers, or character strings are supported.
.PP
In order to modify a file, ADB must be called as:
.P1
	adb \-w file1 file2
.P2
When called with this option, 
.ul
file1
and 
.ul
file2
are created if necessary and opened for both reading and writing.
.PP
For example, consider the C program shown in Figure 10.
We can change the word "This" to "The " in the executable file
for this program, \fIex7\fP, by using the following requests:
.P1
	adb \-w ex7 \-
	?l 'Th'
	?W 'The '
.P2
The request \fB?l\fP starts at dot and stops at the first match of "Th"
having set dot to the address of the location found.
Note the use of \fB?\fP to write to the 
.ul
a.out
file.
The form \fB?*\fP would have been used for a 411 file.
.PP
More frequently the 
request will be typed as:
.P1
	?l 'Th'; ?s
.P2
and locates the first occurrence of "Th" and print the entire string.
Execution of this ADB request will set dot to the address of the 
"Th" characters.
.PP
As another example of the utility of the patching facility,
consider a C program that has an internal logic flag.
The flag could be set by the user through ADB and the program run.
For example:
.P1
	adb a.out \-
	:s arg1 arg2
	flag/w 1
	:c
.P2
The \fB:s\fR request is normally used to single step through a process
or start a process in single step mode.
In this case it starts
.ul
a.out
as a subprocess
with arguments \fBarg1\fP and \fBarg2\fP.
If there is a subprocess running ADB writes to it rather than to the file
so the \fBw\fP request causes \fIflag\fP to be changed in the memory of the subprocess.
.NH
Anomalies
.PP
Below is a list of some strange things that users
should be aware of.
.IP 1.
Function calls and arguments are put on the stack by the C
save routine.
Putting breakpoints at the entry point to routines
means that the function appears not to have been called
when the
breakpoint occurs.
.IP 2.
When printing addresses, ADB uses
either text or data symbols from the \fIa.out\fP file.
This sometimes causes unexpected symbol names to be printed 
with data (e.g. \fIsavr5+022\fP).
This does not happen if
\fB?\fR is used for text (instructions)
and \fB/\fP for data.
.IP 3.
ADB cannot handle C register variables
in the most recently activated function.
.LP
.NH
Acknowledgements
.PP
The authors are grateful for the thoughtful comments
on how to organize this document
from R. B. Brandt, E. N. Pinson and B. A. Tague.
D. M. Ritchie made the system changes necessary to accommodate
tracing within ADB. He also participated in discussions 
during the writing of ADB.
His earlier work with DB and CDB led to many of the 
features found in ADB.
.SG MH-8234-JFM/1273-SRB-unix
.NH
References
.LP
.IP 1.
D. M. Ritchie and K. Thompson,
``The UNIX Time-Sharing System,''
CACM, July, 1974.
.IP 2.
B. W. Kernighan and D. M. Ritchie,
.ul
The C Programming Language,
Prentice-Hall, 1978.
.IP 3.
K. Thompson and D. M. Ritchie,
UNIX Programmer's Manual - 7th Edition,
1978.
.IP 4.
B. W. Kernighan and P. J. Plauger,
.ul
Software Tools,
Addison-Wesley, 1976.
.sp 100
.nr PS 9
.nr VS 11
.	\" START OF Figures
.de P1
.nf
.in +.5i
.ta .5i 1i 1.5i 2i 2.5i 3i 3.5i 4i 4.5i 5i
.sp
.ps 9
.vs 11p
..
.de P2
.sp
.fi
.ps \\n(PS
.vs \\n(VS
.in -.5i
..
.SH
Figure 1:  C program with pointer bug
.LP
.P1
struct buf {
	int fildes;
	int nleft;
	char *nextp;
	char buff[512];
	}bb;
struct buf *obuf;

char *charp "this is a sentence.";

main(argc,argv)
int argc;
char **argv;
{
	char	cc;

	if(argc < 2) {
		printf("Input file missing\\n");
		exit(8);
	}

	if((fcreat(argv[1],obuf)) < 0){
		printf("%s : not found\\n", argv[1]);
		exit(8);
	}
	charp = \'T\';
printf("debug 1 %s\\n",charp);
	while(cc=  *charp++)
		putc(cc,obuf);
	fflush(obuf);
}
.P2
.sp 100
.SH
Figure 2:  ADB output for C program of Figure 1
.LP
.P1
.ft B
adb a.out core
$c
.ft R
~main(02,0177762)
.ft B
$C
.ft R
~main(02,0177762)
	argc:	    02
	argv:	    0177762
	cc:	    02124
.ft B
$r
.ft R
ps	0170010
pc	0204	~main+0152
sp	0177740
r5	0177752
r4	01
r3	0
r2	0
r1	0
r0	0124
~main+0152:	mov	_obuf,(sp)
.ft B
$e
.ft R
savr5:	    0
_obuf:	    0
_charp:	    0124
_errno:	    0
_fout:	    0
.ft B
$m
.ft R
text map    \`ex1\'
b1 = 0		     e1	= 02360		  f1 = 020
b2 = 0		     e2	= 02360		  f2 = 020
data map    \`core1\'
b1 = 0		     e1	= 03500		  f1 = 02000
b2 = 0175400	     e2	= 0200000		  f2 = 05500
.ft B
*charp/s
.ft R
0124:		TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTLx	Nh@x&_
~
.ft B
charp/s
.ft R
_charp:		T
 
_charp+02:	this is a sentence.
 
_charp+026:	Input file missing
.ft B
main.argc/d
.ft R
0177756:	2
.ft B
*main.argv/3o
.ft R
0177762:	0177770	0177776	0177777
.ft B
0177770/s
.ft R
0177770:	a.out
.ft B
*main.argv/3o
.ft R
0177762:	0177770	0177776	0177777
.ft B
*"/s
.ft R
0177770:	a.out
.ft B
 .=o
.ft R
		0177770
.ft B
 .\(mi10/d
.ft R
0177756:	2
.ft B
$q
.P2
.sp 100
.SH
Figure 3:  Multiple function C program for stack trace illustration
.LP
.P1
int	fcnt,gcnt,hcnt;
h(x,y)
{
	int hi; register int hr;
	hi = x+1;
	hr = x\(miy+1;
	hcnt++ ;
	hj:
	f(hr,hi);
}

g(p,q)
{
	int gi; register int gr;
	gi = q\(mip;
	gr = q\(mip+1;
	gcnt++ ;
	gj:
	h(gr,gi);
}

f(a,b)
{
	int fi; register int fr;
	fi = a+2*b;
	fr = a+b;
	fcnt++ ;
	fj:
	g(fr,fi);
}

main()
{
	f(1,1);
}
.P2
.sp 100
.SH
Figure 4:  ADB output for C program of Figure 3
.LP
.P1
.ft B
adb
$c
.ft R
~h(04452,04451)
~g(04453,011124)
~f(02,04451)
~h(04450,04447)
~g(04451,011120)
~f(02,04447)
~h(04446,04445)
~g(04447,011114)
~f(02,04445)
~h(04444,04443)
.ft B
HIT DEL KEY
.ft R
adb
.ft B
,5$C
.ft R
~h(04452,04451)
	x:	    04452
	y:	    04451
	hi:	    ?
~g(04453,011124)
	p:	    04453
	q:	    011124
	gi:	    04451
	gr:	    ?
~f(02,04451)
	a:	    02
	b:	    04451
	fi:	    011124
	fr:	    04453
~h(04450,04447)
	x:	    04450
	y:	    04447
	hi:	    04451
	hr:	    02
~g(04451,011120)
	p:	    04451
	q:	    011120
	gi:	    04447
	gr:	    04450
.ft B
fcnt/d
.ft R
_fcnt:		1173
.ft B
gcnt/d
.ft R
_gcnt:		1173
.ft B
hcnt/d
.ft R
_hcnt:		1172
.ft B
h.x/d
.ft R
022004:		2346
.ft B
$q
.P2
.sp 100
.SH
Figure 5:  C program to decode tabs
.LP
.P1
#define MAXLINE	80
#define YES		1
#define NO		0
#define TABSP		8
.sp .5
char	input[] "data";
char	ibuf[518];
int	tabs[MAXLINE];
.sp .5
main()
{
	int col, *ptab;
	char c;
.sp .5
	ptab = tabs;
	settab(ptab);	/*Set initial tab stops */
	col = 1;
	if(fopen(input,ibuf) < 0) {
		printf("%s : not found\\n",input);
		exit(8);
	}
	while((c = getc(ibuf)) != \(mi1) {
		switch(c) {
			case \(fm\\t\(fm:	/* TAB */
				while(tabpos(col) != YES) {
					putchar(\(fm \(fm);	/* put BLANK */
					col++ ;
				}
				break;
			case \(fm\\n\(fm:	/*NEWLINE */
				putchar(\(fm\\n\(fm);
				col = 1;
				break;
			default:
				putchar(c);
				col++ ;
		}
	}
}
.sp .5
/* Tabpos return YES if col is a tab stop */
tabpos(col)
int col;
{
	if(col > MAXLINE)
		return(YES);
	else
		return(tabs[col]);
}
.sp .5
/* Settab - Set initial tab stops */
settab(tabp)
int *tabp;
{
	int i;
.sp .5
	for(i = 0; i<= MAXLINE; i++) 
		(i%TABSP) ? (tabs[i] = NO) : (tabs[i] = YES);
}
.P2
.sp 100
.SH
Figure 6a:  ADB output for C program of Figure 5
.LP
.P1
.ft B
adb a.out \(mi
settab+4:b
fopen+4:b
getc+4:b
tabpos+4:b
$b
.ft R
breakpoints
count	bkpt		command
1	~tabpos+04
1	_getc+04
1	_fopen+04
1	~settab+04
.ft B
settab,5?ia
.ft R
~settab:		jsr	r5,csv
~settab+04:	tst	\(mi(sp)
~settab+06:	clr	0177770(r5)
~settab+012:	cmp	$0120,0177770(r5)
~settab+020:	blt	~settab+076
~settab+022:
.ft B
settab,5?i
.ft R
~settab:		jsr	r5,csv
		tst	\(mi(sp)
		clr	0177770(r5)
		cmp	$0120,0177770(r5)
		blt	~settab+076
.ft B
:r
.ft R
a.out: running
breakpoint	~settab+04:	tst	\(mi(sp)
.ft B
settab+4:d
:c
.ft R
a.out: running
breakpoint	_fopen+04:	mov	04(r5),nulstr+012
.ft B
$C
.ft R
_fopen(02302,02472)
~main(01,0177770)
	col:	    01
	c:	    0
	ptab:	    03500
.ft B
tabs,3/8o
.ft R
03500:		01	0	0	0	0	0	0	0
		01	0	0	0	0	0	0	0
		01	0	0	0	0	0	0	0
.P2
.sp 100
.SH
Figure 6b:  ADB output for C program of Figure 5
.LP
.P1
.ft B
:c
.ft R
a.out: running
breakpoint	_getc+04:	mov	04(r5),r1
.ft B
ibuf+6/20c
.ft R
__cleanu+0202:		This	is	a test	of
.ft B
:c
.ft R
a.out: running
breakpoint	~tabpos+04:	cmp	$0120,04(r5)
.ft B
tabpos+4:d
settab+4:b  settab,5?ia
settab+4:b  settab,5?ia;  0
getc+4,3:b  main.c?C;  0
settab+4:b  settab,5?ia;  ptab/o;  0
$b
.ft R
breakpoints
count	bkpt		command
1	~tabpos+04
3	_getc+04	main.c?C;0
1	_fopen+04
1	~settab+04	settab,5?ia;ptab?o;0
~settab:		jsr	r5,csv
~settab+04:	bpt
~settab+06:	clr	0177770(r5)
~settab+012:	cmp	$0120,0177770(r5)
~settab+020:	blt	~settab+076
~settab+022:
0177766:	0177770
0177744:	@\`
T0177744:	T
h0177744:	h
i0177744:	i
s0177744:	s
.P2
.sp 100
.SH
Figure 7:  ADB output for C program with breakpoints
.LP
.in +.5i
.nf
.ps 8
.vs 9
.ft B
adb ex3 \(mi
h+4:b hcnt/d; h.hi/; h.hr/
g+4:b gcnt/d; g.gi/; g.gr/
f+4:b fcnt/d; f.fi/; f.fr/
:r
.ft R
ex3: running
_fcnt:		0
0177732:	214
symbol not found
.ft B
f+4:b fcnt/d; f.a/; f.b/; f.fi/
g+4:b gcnt/d; g.p/; g.q/; g.gi/
h+4:b hcnt/d; h.x/; h.y/; h.hi/
:c
.ft R
ex3: running
_fcnt:		0
0177746:	1
0177750:	1
0177732:	214
_gcnt:		0
0177726:	2
0177730:	3
0177712:	214
_hcnt:		0
0177706:	2
0177710:	1
0177672:	214
_fcnt:		1
0177666:	2
0177670:	3
0177652:	214
_gcnt:		1
0177646:	5
0177650:	8
0177632:	214
.ft B
HIT DEL
f+4:b fcnt/d; f.a/"a = "d; f.b/"b = "d; f.fi/"fi = "d
g+4:b gcnt/d; g.p/"p = "d; g.q/"q = "d; g.gi/"gi = "d
h+4:b hcnt/d; h.x/"x = "d; h.y/"h = "d; h.hi/"hi = "d
:r
.ft R
ex3: running
_fcnt:		0
0177746:	a = 1
0177750:	b = 1
0177732:	fi = 214
_gcnt:		0
0177726:	p = 2
0177730:	q = 3
0177712:	gi = 214
_hcnt:		0
0177706:	x = 2
0177710:	y = 1
0177672:	hi = 214
_fcnt:		1
0177666:	a = 2
0177670:	b = 3
0177652:	fi = 214
.ft B
HIT DEL
$q
.in -.5i
.sp 100
.SH
Figure 8:  ADB address maps
.LP
.de l1
.tc
.ta 1.20i +1.6i +2.5i
..
.de l3
.tc
.ta 1.6i +2.80i +.2i +1.55i
..
.de l2
.tc
.ti 1.0i
.ta +0.5i +3.0i +1.75i
.tc _
..
.de l5
.tc 
.ti 1.0i
.ta +0.75i +3.0i +1.5i
.tc _
..
.de l6
.tc
.ti 1.0i
.ta +.8i +2.85i +0.4i +1.1i
..
.de l8
.tc
.ti 1.0i
.ta +0.5i +3.0i +1.75i
.tc _
..
.de la
.tc
.ta 1.20i +1.25i +1.7i
..
.de lc
.tc
.ti 1.0i
.ta +.85i +1.6i +.35i +1.1i
..
.de lb
.tc
.ti 1.0i
.ta +0.75i +1.75i +1.5i
.tc _
..
.ul
407 files
.sp
.l1
a.out	hdr	text+data
.l2
|	|	|
.l3
	0	D
.sp
.l1
core	hdr	text+data	stack
.l5
|	|	......|	|
.l6
	0	D	S	E
.sp 2
.ul
410 files (shared text)
.sp
.l1
a.out	hdr	 text	data
.l2
|	|	|	|
.l3
	0	T	B	D
.sp
.la
core	hdr	data	stack
.lb
|	|	......|	|
.lc
	B	D	S	E
.sp 2
.ul
411 files (separated I and D space)
.sp
.l1
a.out	hdr	text	data
.l2
|	|	|	|
.l3
	0	T	0	D
.sp
.la
core	hdr	data	stack
.lb
|	|	......|	|
.lc
	0	D	S	E
.sp 2
The following 
.ul 
adb
variables are set.
.nf
.ta .75i 1.5i 3.5i 4.5i 5.5i
.sp
			407	410	411
.sp
	b	base of data	0	B	0
	d	length of data	D	D\(miB	D
	s	length of stack	S	S	S
	t	length of text	0	T	T
.sp 100
.SH
Figure 9:  ADB output for maps
.LP
.nf
.in +.5i
.ft B
adb map407 core407
$m
.ft R
text map    \`map407\'
b1 = 0		     e1	= 0256		  f1 = 020
b2 = 0		     e2	= 0256		  f2 = 020
data map    \`core407\'
b1 = 0		     e1	= 0300		  f1 = 02000
b2 = 0175400	     e2	= 0200000	  f2 = 02300
.ft B
$v
.ft R
variables
d = 0300
m = 0407
s = 02400
.ft B
$q
.sp 2
adb map410 core410
$m
.ft R
text map    \`map410\'
b1 = 0		     e1	= 0200		  f1 = 020
b2 = 020000	     e2	= 020116	  f2 = 0220
data map    \`core410\'
b1 = 020000	     e1	= 020200	  f1 = 02000
b2 = 0175400	     e2	= 0200000	  f2 = 02200
.ft B
$v
.ft R
variables
b = 020000
d = 0200
m = 0410
s = 02400
t = 0200
.ft B
$q
.sp 2
adb map411 core411
$m
.ft R
text map    \`map411\'
b1 = 0		     e1	= 0200		  f1 = 020
b2 = 0		     e2	= 0116		  f2 = 0220
data map    \`core411\'
b1 = 0		     e1	= 0200		  f1 = 02000
b2 = 0175400	     e2	= 0200000	  f2 = 02200
.ft B
$v
.ft R
variables
d = 0200
m = 0411
s = 02400
t = 0200
.ft B
$q
.in -.5i
.sp 100
.SH
Figure 10:  Simple C program for illustrating formatting and patching
.LP
.P1
char	str1[]	"This is a character string";
int	one	1;
int	number	456;
long	lnum	1234;
float	fpt	1.25;
char	str2[]	"This is the second character string";
main()
{
	one = 2;
}
.P2
.sp 100
.SH
Figure 11:  ADB output illustrating fancy formats
.LP
.nf
.ps 9
.vs 11p
.ft B
adb map410 core410
<b,\(mi1/8ona
.ft R
020000:		0	064124	071551	064440	020163	020141	064143	071141
.sp .5
_str1+016:	061541	062564	020162	072163	064562	063556	0	02
.sp .5
_number:
_number:	0710	0	02322	040240	0	064124	071551	064440
.sp .5
_str2+06:	020163	064164	020145	062563	067543	062156	061440	060550
.sp .5
_str2+026:	060562	072143	071145	071440	071164	067151	0147	0
.sp .5
savr5+02:	0	0	0	0	0	0	0	0
.sp .5
.ft B
<b,20/4o4^8Cn
.ft R
020000:		0	064124	071551	064440	@\`@\`This i
		020163	020141	064143	071141	s a char
		061541	062564	020162	072163	acter st
		064562	063556	0	02	ring@\`@\`@b@\`
.sp .5
_number:	0710	0	02322	040240	H@a@\`@\`R@d @@
		0	064124	071551	064440	@\`@\`This i
		020163	064164	020145	062563	s the se
		067543	062156	061440	060550	cond cha
		060562	072143	071145	071440	racter s
		071164	067151	0147	0	tring@\`@\`@\`
		0	0	0	0	@\`@\`@\`@\`@\`@\`@\`@\`
		0	0	0	0	@\`@\`@\`@\`@\`@\`@\`@\`
data address not found
.ft B
<b,20/4o4^8t8cna
.ft R
020000:		0	064124	071551	064440		This i
_str1+06:	020163	020141	064143	071141		s a char
_str1+016:	061541	062564	020162	072163		acter st
_str1+026:	064562	063556	0	02		ring
_number:
_number:	0710	0	02322	040240		HR
_fpt+02:	0	064124	071551	064440		This i
_str2+06:	020163	064164	020145	062563		s the se
_str2+016:	067543	062156	061440	060550		cond cha
_str2+026:	060562	072143	071145	071440		racter	s
_str2+036:	071164	067151	0147	0		tring
savr5+02:	0	0	0	0
savr5+012:	0	0	0	0
data address not found
.ft B
<b,10/2b8t^2cn
.ft R
020000:		0	0
.sp .5
_str1:		0124	0150		Th
		0151	0163		is
		040	0151		 i
		0163	040		s
		0141	040		a
		0143	0150		ch
		0141	0162		ar
		0141	0143		ac
		0164	0145		te
.ft B
$Q
.sp 100
.SH
Figure 12:  Directory and inode dumps
.LP
.nf
.ft B
adb dir \(mi
=nt"Inode"t"Name"
0,\(mi1?ut14cn
.ft R

		Inode	Name
0:		652	.
		82	..
		5971	cap.c
		5323	cap
		0	pp
.sp 4
.ft B
adb /dev/src \(mi
.ft B
02000>b
?m<b
.ft R
new map	    \`/dev/src\'
b1 = 02000	     e1	= 0100000000	  f1 = 0
b2 = 0		     e2	= 0		  f2 = 0
.ft B
$v
.ft R
variables
b = 02000
.ft B
<b,\(mi1?"flags"8ton"links,uid,gid"8t3bn"size"8tbrdn"addr"8t8un"times"8t2Y2na
.ft R
02000:		flags	073145
		links,uid,gid	0163	0164	0141
		size	0162	10356
		addr	28770	8236	25956	27766	25455	8236	25956	25206
		times	1976 Feb 5 08:34:56	1975 Dec 28 10:55:15

02040:		flags	024555
		links,uid,gid	012	0163	0164
		size	0162	25461
		addr	8308	30050	8294	25130	15216	26890	29806	10784
		times	1976 Aug 17 12:16:51	1976 Aug 17 12:16:51

02100:		flags	05173
		links,uid,gid	011	0162	0145
		size	0147	29545
		addr	25972	8306	28265	8308	25642	15216	2314	25970
		times	1977 Apr 2 08:58:01	1977 Feb 5 10:21:44
.\"
.\"	Start of Summary
.sp 100
.TL
ADB Summary
.LP
.LP
.if t .2C
.nr VS 9
.nr VS 11
.SH
Command Summary
.LP
.ta .7i
a)   formatted printing
.sp .5
.IP "\fB? \fIformat\fR" .7i
print from \fIa.out\fR file according to \fIformat\fR
.IP "\fB/ \fIformat\fR" .7i
print from \fIcore\fR file according to \fIformat\fR
.IP "\fB= \fIformat\fR" .7i
print the value of \fIdot\fR
.sp .5
.IP "\fB?w\fR expr" .7i
write expression into \fIa.out\fR file
.IP "\fB/w\fR expr" .7i
write expression into \fIcore\fR file
.sp .5
.IP "\fB?l\fR expr" .7i
locate expression in \fIa.out\fR file
.LP
.ta .7i
b)   breakpoint and program control
.LP
.ta .7i
.nf
.ta .7i
\fB:b\fR	set breakpoint at \fIdot\fR
\fB:c\fR	continue running program
\fB:d\fR	delete breakpoint
\fB:k\fR	kill the program being debugged
\fB:r\fR	run \fIa.out\fR file under ADB control
\fB:s\fR	single step
.LP
.ta .7i
c)   miscellaneous printing
.LP
.ta .7i
.nf
\fB$b\fR	print current breakpoints
\fB$c\fR	C stack trace
\fB$e\fR	external variables
\fB$f\fR	floating registers
\fB$m\fR	print ADB segment maps
\fB$q\fR	exit from ADB
\fB$r\fR	general registers
\fB$s\fR	set offset for symbol match
\fB$v\fR	print ADB variables
\fB$w\fR	set output line width
.LP
.ta .7i
d)   calling the shell
.LP
.ta .7i
.nf
\fB!\fR	call \fIshell\fP to read rest of line
.LP
.ta .7i
e)   assignment to variables
.LP
.ta .7i
.nf
\fB>\fIname\fR	assign dot to variable or register \fIname\fR
.sp 100
.SH
Format Summary
.LP
.ta .7i
.nf
\fBa	\fRthe value of dot
\fBb	\fRone byte in octal
\fBc	\fRone byte as a character
\fBd	\fRone word in decimal
\fBf	\fRtwo words in floating point
\fBi	\fRPDP 11 instruction
\fBo	\fRone word in octal
\fBn	\fRprint a newline
\fBr	\fRprint a blank space
\fBs	\fRa null terminated character string
\fIn\fBt	\fRmove to next \fIn\fR space tab
\fBu	\fRone word as unsigned integer
\fBx	\fRhexadecimal
\fBY	\fRdate
\fB^	\fRbackup dot
\fB"..."\fR	print string
.LP
.ta .7i
.SH
Expression Summary
.LP
.ta .7i
a) expression components
.LP
.ta .1.1i
.nf
\fBdecimal integer	\fRe.g. 256
\fBoctal integer	\fRe.g. 0277
\fBhexadecimal	\fRe.g. #ff
\fBsymbols	\fRe.g. flag  _main  main.argc
\fBvariables	\fRe.g. <b
\fBregisters	\fRe.g. <pc <r0
\fB(expression)	\fRexpression grouping
.LP
.ta .7i
b) dyadic operators
.LP
.ta .7i
.nf
\fB+\fP	add
\fB\(mi\fP	subtract
\fB*\fP	multiply
\fB%\fP	integer division
\fB&\fP	bitwise and
\fB|\fP	bitwise or
\fB#\fP	round up to the next multiple
.LP
.ta .7i
c) monadic operators
.LP
.ta .7i
.nf
\v'.25m'\s+2\fB~\fP\s0\v'-.25m'	not
\fB*\fR	contents of location
\fB\(mi\fR	integer negate
.fi
