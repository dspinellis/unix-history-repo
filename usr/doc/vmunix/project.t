.SH
.ce
UNIX Project Funding Imminent
.PP
It is expected that a project on the Berkeley Campus
will be funded soon by the Federal Government
to provide a Standard VAX UNIX system
for use by various government contractors.
The project will be under the direction of Computer Science Professor Bob Fabry.
.PP
A major factor in attracting these funds was the excellent reputation of
the current Berkeley VAX UNIX system, the first 
to take advantage of the paging mechanism of the VAX
to facilitate programs requiring a very large memory.
The paging facilities were developed by students Bill Joy and Ozalp Babaoglu
under the direction of Computer Science Professor Domenico Ferarri.
.PP
The system which will be developed will be an enhancement of the current
Berkeley VAX UNIX system.
File access will be improved by allowing files to be logically coupled
into the address space of a process.  Pages of such files will be
brought into memory only as they are used.  If several processes are
using the same page of a file, there will be only one copy of the page
in memory; changes made by one process will be available immediately
to the other processes.
.PP
A major effort will be mounted to improve the UNIX
interprocess communication mechanism.
The current ``pipe'' mechanism is considered inadequate.
The mechanism to be implemented will facilitate message-oriented
applications while still being compatible with the current
stream-oriented programs.  Processes will be able to communicate
without taking into account 
whether they are on the same system or are communicating over a
network.  Additional networking options will also be provided.
Local high speed networks as well as new ARPANET software being
developed at Bolt, Beranek, and Newman, Inc. will be incorporated.
.PP
Because applications expected for the system include VLSI circuit design,
image processing, large LISP programs, and so on, a number of
performance enhancements will be made to improve the response
time for large applications.
.PP
Bill Joy will be playing a central role in the project
which is budgeted for about two thirds of a million dollars over
the initial period of eighteen months.
This amount includes the cost of a medium
size VAX system which will be used for the development work.
In addition to staff members and faculty, the project
is expected to support a number of graduate students.
Students whose background and interests make them suitable for
working with the project are encouraged to contact Professor Fabry
or Bill Joy.
.PP
The resulting system is expected to be available to
all VAX UNIX Licensees for a nominal fee.
.PP
(VAX is a trademark of Digital Equipment Corporation and UNIX is a
trademark of Western Electric Company.)

      --Bob Fabry
