.nr PS 12
.NH
Software Management
.nr PS 10
.XS
\*(SN Software Management
.XE
.PP
Although the
.UX 
operating system offers a rich variety of programming tools, a frequent
complaint is that there are too few guidelines showing how to use them
in a coherent way. SPMS provides the `glue' for coordinating the use
of these tools and this section describes the techniques which have been
devised for the development and maintenance of software packages.
.NH 2
Program Development Techniques
.XS
\*(SN Program Development Techniques
.XE
.PP
Before discussing the maintenance of complete software packages, it is
worthwhile to review the basic commands for managing individual programs
and libraries. The commands are summarized in table 2 and explained below
in more detail.
.KF

.SM
.ce
\fITable 2.\fR   Program development commands
.NL

.so develop.tbl
.KE
.NH 3
\fIProgram compilation\fR
.XS
\*(SN Program compilation
.XE
.PP
The command
.ID
%  \fBmake\fR
.DE
compiles source files into object files and loads them together
to produce an executable program. Although
.I make
uses built-in information for generating the object files
from the source files, the decision on how to load the program is
left to the user. By default, the program makefiles produced by
.I mkmf
use the C compiler for this purpose. However, if the programming
language is not C, the LINKER macro definition in the makefile should
be altered accordingly. For Fortran this can be done by typing the command
.ID
%  \fBmkmf  LINKER=f77\fR
.DE
and for Pascal
.ID
%  \fBmkmf  LINKER=pc\fR
.DE
.PP
Compiler options can be specified by adding certain macros to a
makefile.  For example, the macro
.ID
CFLAGS = \-O
.DE
causes a C program to be compiled with optimization, and the macro
.ID
CFLAGS = \-I../../include  \-g
.DE
tells the C compiler to search for header files in the directory
`../../include' as well as in the current directory (see \(sc\|2.9.1),
and to compile the program with debugging information. FFLAGS and
PFLAGS can be used similarly to set options for the Fortran and Pascal
compilers respectively.
.NH 3
\fIInstallation\fR
.XS
\*(SN Installation
.XE
.PP
The command
.ID
%  \fBmake  install\fR
.DE
installs a program or library (see \(sc\|2.9.3). If any source code
files are newer than their corresponding object files they are
recompiled and the program or library reformed. Even if the object
files are up-to-date, a program may still be relinked if the libraries
on which it depends are newer than the program itself.
.PP
Normally a program is stripped of its symbol table and relocation
bits when it is installed, to save space. This can be avoided
if the
.B \-s
option is removed from the
.I install
command line in the makefile.
.NH 3
\fIUpdating\fR
.XS
\*(SN Updating
.XE
.PP
If a program or library is out-of-date \- that is, some of the source
code files are newer than the installed version \- the command
.ID
\fBmake update\fR
.DE
recompiles and reinstalls the program or library. This command is more
powerful than \fImake install\fR because it is not affected by the
absence of the object files. In the case of an out-of-date library, all
the object files are extracted from the library before any
recompilation takes place, and removed once the library has been
reinstalled.
.NH 3
\fIDependency analysis\fR
.XS
\*(SN Dependency analysis
.XE
.PP
Although the
.I make
program has a set of built-in rules for recompiling a program if any of
the files on which it depends have changed since the last time it was
constructed, these rules do not extend to included files. It is
necessary to add explicit dependency rules to a makefile so that if
a header file is changed, the affected source files will be recompiled
and a new program produced. For instance, if the rule
.ID
vs.o:  vs.h  hash.h  list.h
.DE
is added to the makefile for the `vs' program, the file `vs.c' will be
recompiled if any of the included files (see \(sc\|2.9.1) have changed
since the last time it was compiled. The command
.ID
%  \fBmake depend\fR
.DE
calls on the
.I mkmf
makefile editor to insert header file dependencies into a makefile.
.NH 3
\fIProgram checking\fR
.XS
\*(SN Program checking
.XE
.PP
Programs written in C can be checked for bugs, obscurities, wasteful
or error prone constructions, type and function usage, and portability
by the
.I lint
program\|[4]. If a makefile contains the line
.ID
lint:;  @lint $(LINTFLAGS) $(SRCS) $(LINTLIST)
.DE
where the LINTFLAGS macro definition specifies
.I lint
options, SRCS represents the source files making up the program or library,
and LINTLIST is a list of lint libraries (see below), the command
.ID
%  \fBmake lint\fR
.DE
will check that the program or library is consistent.
.PP
To set up the `vs' program might be set up for ``linting'', the macro
definitions would be
.DS
LINTFLAGS \kx= \-I../../include

SRCS\h'|\nxu'= vs.c

LINTLIST\h'|\nxu'= \ky../../lib/llib-lhash.ln \\\\
\h'|\nyu'\&../../lib/llib-llist.ln \\\\
\h'|\nyu'\-lc
.DE
.PP
Just as program libraries share functions among different programs,
lint libraries can be used to check that those functions have been used
correctly. Lint libraries are created by \fIlint \-C\fR as shown by the
following entry in the makefile belonging to the `hash' library
.DS
$(LINTLIB):  \kx$(SRCS) $(HDRS) $(EXTHDRS)
\h'|\nxu'@echo "Loading $(LINTLIB) . . ."
\h'|\nxu'@lint $(LINTFLAGS) \-C$(LIBNAME) $(SRCS)
\h'|\nxu'@echo done
.DE
where LIBNAME is defined in the makefile as `hash', and LINTLIB is defined
as `llib-l$(LIBNAME).ln' in accordance with standard lint library naming
conventions.
.NH 3
\fIVersion control and releases\fR
.XS
\*(SN Version control and releases
.XE
.PP
During the time that a program is being developed it is quite likely
that it will undergo several revisions. Features are added and
algorithms are improved. Often changes are made which are later found
not to work and need to be undone. One way to handle these changes is
to save a copy of each file before it is revised. However, this quickly
becomes expensive in terms of space. A better solution is to use a
version control system like
.I SCCS
(Source Code Control System)\|[6] or
.I RCS
(Revision Control System)\|[10] which stores only the changes made to the
source code together with details such as when each change was made,
why it was made, and who made it.
.PP
Once a program is ready for release, all of the source files should be
stamped with a common version name or number so that it can be recreated
at any time regardless of any subsequent changes.
.I RCS
has the advantage over
.I SCCS
in this respect because it enables the user to stamp each release with a
unique name. For example, if the makefile and all the source files in
release 2 of the `list' library are stamped `V2', the command sequence
.ID
%  \fBco  \-rV2  Makefile\fR
%  \fBmake  VERSION=V2  co\fR
.DE
will create that release
by extracting or ``checking out'' the Makefile and the source files from the
.I RCS
system
using the
.I co
command.
.NH 3
\fIFunction tagging\fR
.XS
\*(SN Function tagging
.XE
.PP
By creating a database of function names\**
.FS
For C, Fortran, and Pascal programs only.
.FE
with the command
.ID
%  \fBmake tags\fR
.DE
it is possible for the user to find and edit a function without having
to remember the name of the file in which the function is located. For
example, in the `src' directory of the `libhash' subproject, the
command
.ID
%  \fBvi  \-t  htinit\fR
.DE
invokes the
.I vi
editor on the file `htinit.c' and positions the cursor at the beginning of the
`htinit' hash table initialization function.
.PP
A list of functions which make up the program or library,
together with the line number and file in which each is defined, can be
obtained by the command
.ID
%  \fBmake index\fR
.DE
.NH 3
\fIPrinting\fR
.XS
\*(SN Printing
.XE
.PP
To print all of the program source and header files on the line printer
.I lpr,
type
.ID
%  \fBmake  print  |  lpr\fR
.DE
By default the files are formatted by
.I pr
so that the output is separated into pages headed by a date, the name of
the file, and a page number. Another format can be specified by changing
the PRINT macro definition in the makefile.
.NH 3
\fICleaning up\fR
.XS
\*(SN Cleaning up
.XE
.PP
To save space once a program has been completed, the command
.ID
%  \fBmake clean\fR
.DE
removes object files plus any other files which can be regenerated easily.
.NH 3
\fITesting\fR
.XS
\*(SN Testing
.XE
.PP
Using the test cases prepared previously (see \(sc\|3.4), it is possible
to test an entire program or library by typing the command
.ID
%  \fBptest\fR
.DE
.I Ptest
reports the outcome of each test \- i.e. whether it passes or fails \- and if
it fails, saves the error diagnostics in a file called E\fItest\fR where
.I test
is the name of the test.
.PP
Because
.I ptest
uses a number of temporary working files for each test and creates an error
diagnostic file for each test that fails, it is a good idea not to clutter up
the directories that contain source code or test cases, but instead perform
the testing in another directory such as `work' (see \(sc\|4.3.5).
.NH 3
\fICompound commands\fR
.XS
\*(SN Compound commands
.XE
.PP
The
.I make
program can process multiple requests. For example,
.ID
%  \fBmake install tags clean\fR
.DE
installs a program or library, creates function tags, and removes
any unneeded files. The only operation which may cause problems
if it is used in conjunction with
.I install
or
.I update
is \fImake depend\fR because it recreates the include file dependencies
.B after
the
.I make
command has already read the makefile.
.NH 3
\fIUser-defined commands\fR
.XS
\*(SN User-defined commands
.XE
.PP
Most of the tasks described above are handled by the
.I make
command. More complex programming tasks can also be defined by adding
extra instructions to each makefile. However, rather than modify every
program and library makefile in a project individually, the user can
get
.I mkmf
to use alternative `p.Makefile' and `l.Makefile' makefile templates when
creating program and library makefiles respectively, if these templates
exist in the project `lib' directory (see fig. 3). The templates for
project `vs' include directives for type checking and version control (see
appendix B). It is a worthwhile exercise to compare them against the
standard templates shown in appendix A.
.NH 2
Layered Construction of Software Packages
.XS
\*(SN Layered Construction of Software Packages
.XE
.PP
A complex software package may be built and installed in \fIlayers\fR\|[2]
as shown in table 3.
.KF

.SM
.ce
\fITable 3.\fR   Layers of software
.NL

.so layer.tbl
.KE
.PP
Each layer is assigned a specific label so that it can be built individually,
as well as a priority level so that the complete software package can be
constructed in a predetermined sequence. Layers are implemented by attaching
type labels to the directories which are part of the building process.
Table 4 suggests a set of type labels for each layer. By convention, type
label `update' is used for the construction of the entire software package.
.KF

.SM
.ce
\fITable 4.\fR   Layer type labels and priority levels
.NL

.so construct.tbl
.KE
.PP
If the project `vs' is organized into layers in the manner shown in figure 11,
.KF
.sp 34
.SM
.ce
\fIFigure 11.  \fRLayers of project `vs'

.NL
.KE
then the command
.ID
%  \fBpexec \-T\|libsrc  make  update\fR
.DE
brings the program libraries up-to-date, while the command
.ID
%  \fBpexec \-T\|cmdsrc  make  update\fR
.DE
does the same for the programs
.I vs
and
.I vstutor.
By typing
.ID
%  \fBpexec \-T\|update  make  update\fR
.DE
the entire software package can be updated.
.NH 2
Maintenance of Software Packages
.XS
\*(SN Maintenance of Software Packages
.XE
.PP
Having established the conventions for maintaining the individual components
of a software package, global tasks such as printing,
testing, cleaning, etc., can now be described in more detail. The type
labels that provide the means for coordinating these tasks are
summarized in table 5. Some of the labels have the letter
.I n
to indicate priority because the directories to which they are attached
must be processed in a particular order (see \(sc\|2.10.2). 
.KF

.SM
.ce
\fITable 5.\fR   Type label conventions
.NL

.so label.tbl
.KE
.NH 3
\fICounting of source lines\fR
.XS
\*(SN Counting of source lines
.XE
.PP
The total number of lines of source code in a software package can be counted
by concatenating the source files in each `src' directory and piping them
to the word count program as
.ID
%  \fBpexec  \-q  \-T\|src  make  PRINT=cat  print  |  wc  \-l\fR
.DE
where
.B \-q
suppresses the printing of project directory titles, and
.B \-l
tells
.I wc
to count lines only.
.NH 3
\fICataloging of functions\fR
.XS
\*(SN Cataloging of functions
.XE
.PP
A list of functions, together with the line number and file in which
each is defined, may be obtained\** for each program in a software package
.FS
For C, Fortran, and Pascal programs only.
.FE
by the command
.ID
%  \fBpexec  \-T\|cmdsrc  make index\fR
.DE
.PP
A comprehensive index of all the library functions can be generated by
outputting the function definitions in each library (e.g. `libhash' and
`liblist') to the
.I sort
program by
.ID
%  \fBpexec  \-q  \-Tlibsrc  make index  | sort\fR
.DE
.NH 3
\fIPrinting\fR
.XS
\*(SN Printing
.XE
.PP
After printing all of the source code in a project by
.ID
%  \fBpexec  \-T\|print  make print  |  lpr\fR
.DE
a table of contents can be produced by
.ID
%  \fBpexec  \-Tprint  make \\\&"PRINT=ls \-C\^\\\&" print  |  lpr\fR
.DE
Backslash `\\' characters prevent the double quotes `"' from being stripped
from the PRINT macro definition by the shell before the
.I make
command is executed in each directory.
.NH 3
\fIProgram checking\fR
.XS
\*(SN Program checking
.XE
.PP
Software packages written in C can be cross-checked for function and type usage
by applying the
.I lint
command to the source code in each of the project directories containing a
program or library
.ID
%  \fBpexec  "\-T\|libsrc\||\|cmdsrc"  make lint\fR
.DE
.NH 3
\fITesting\fR
.XS
\*(SN Testing
.XE
.PP
All of the tests previously prepared for a software package are exercised
by the command
.ID
%  \fBpexec  \-T\|test  ptest  >  testlog\fR
.DE
in the directories labeled `test'. In the case of project `vs', the
following directories are labeled `test'
.DS
.so test.tbl
.DE
The outcome of each test \- i.e. whether it passes or fails \- is recorded
in the file called `testlog' in the current working directory. If a test
fails, the cause of the failure is recorded in a file bearing the
name `E\fItest\fR' where
.I test
is the name of the test, located in the directory where the test is
executed.
.NH 3
\fIComparing versions\fR
.XS
\*(SN Comparing versions
.XE
.PP
The method for comparing the source code in two different versions of a
project depends on the way in which the versions are stored. If they
are stored in separate projects the
.I pdiff
command can be used to compare the contents of the directories belonging
to each of the two projects. For example, if `nvs' is a new version of
the project `vs', then the command
.ID
%  \fBpdiff  \-T\|src  ^vs  ^nvs\fR
.DE
will produce a summary of the differences between them. However, if the
different versions are stored as deltas in a version control system such as
.I SCCS
or
.I RCS,
then either the
.I sccsdiff
command or the
.I rcsdiff
command must be used instead. To show how this is done with
.I RCS,
.ID
%  \fBpexec  \-T\|src  make  VERSION=V2  diff\fR
.DE
compares the current working version of the source code in the project
`vs' with a previous version labeled `V2'.
.NH 3
\fIReleases\fR
.XS
\*(SN Releases
.XE
.PP
If the source code for a software package is stored in a version control
system like
.I RCS,
it is possible to re-create any particular version of the package provided
that all the source files in that version have previously been stamped
with a symbolic version name\** (for example, `V2').
.FS
\fISCCS\fR does not have this feature.
.FE
The process is carried out in two stages. After removing the current version
of the source code (hopefully, it has already been stored in the version
control system) by the following command sequence,
.ID
%  \fBpexec  \-T\|src  \'rm  \`make PRINT=echo print\`  Makefile\'\fR
.DE
the makefile and source files for the desired version (say `V2') are
checked out by
.ID
%  \fBpexec  \-T\|src  \'co  \-rV2  Makefile;  make  VERSION=V2  co\'\fR
.DE
.NH 3
\fICleaning up\fR
.XS
\*(SN Cleaning up
.XE
.PP
If a software package is in a stable state \- that is, it is not being
modified \- then, as an economy measure, the amount of space that it
takes up can be reduced by removing object files plus any other files
that can be regenerated easily. This task is implemented by
.ID
%  \fBpexec  \-T\|clean  make clean\fR
.DE
assuming that the directories containing the files to be removed are
labeled `clean'.


