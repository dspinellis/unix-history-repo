.nr PS 12
.NH
Simple Tasks
.nr PS 10
.XS
\*(SN Simple Tasks
.XE
.PP
In this document several examples related to an interactive screen-oriented
spreadsheet program are presented to demonstrate the use of SPMS for
software development and project management. It is assumed that the
reader is familiar with the
.UX
operating system and a text editor such as
.I ex.
In these examples, user input is shown in \fBbold\fR face.
.NH 2
Getting Started
.XS
\*(SN Getting Started
.XE
.PP
Before using SPMS for the first time the following steps must be performed\**
.FS
For C shell, (\fIcsh\fR), users only. Consult the UNIX Programmer's
Manual for instructions on how to set up SPMS for the Bourne shell,
(\fIsh\fR).
.FE
.IP 1.
Include the directory `/usr/new' in the
command search path. This is done by altering the PATH environment variable
in one of the startup files, `.cshrc'  or `.login', in the home directory.
.IP 2.
Add the following aliases to the `.cshrc' file located in the home directory
.br
	alias chproject  \'eval \`\^"chproject"  \\!*\`\^\'
.br
	alias pd  \'eval \`\^"pd"  \\!*\`\^\'
.IP 3.
Add the following command to the `.login' file located in the home directory
.br
	chproject  ^
.IP 4.
Convert the home directory to a project root directory by typing
.br
	\fB/usr/new/mkproject  \-d  ^\fR
.IP 5.
Execute the `.cshrc' and `.login' files by typing
.br
	\fBsource  .cshrc\fR
.br
	\fBsource  .login\fR
.NH 2
Building a Project
.XS
\*(SN Building a Project
.XE
.PP
The directory structure to support a software package is created by the
.I mkproject
and
.I pmkdir
commands. These commands create directories using the standard
.UX
.I mkdir
command, and record information about each directory in a project database
called the
.I
project link directory.
.R
This information is used by various SPMS commands to control the development
and maintenance activities for a project.
.PP
The steps for building the project structure are:
.IP 1.
Initialize the project using the
.I mkproject
command.
.I Mkproject
creates a directory known as a
.I
project root directory,
.R
to serve as the focus for a project, and initializes the project database.
After
.I mkproject
creates the project root directory, the user is prompted for a line
describing the purpose of the project.
.IP 2.
Use the \fI\%chproject\fR command to change to the root directory of
the new project and make it the \fIworking project\fR (see \(sc\|2.13).
.IP 3.
Create the project directories using the
.I pmkdir
command. After
.I pmkdir
creates each directory, the user is prompted for a line describing the
purpose of the directory.
.PP
To illustrate this process, the following commands create project `vs'
with directories `doc', `src', and `work' (see fig. 2) to support a
`Visual Spreadsheet' program\** called \fIvs\fR.
.FS
\fIVs\fR is a fictitious name bearing no resemblance to any actual program.
.FE
.DS
%  \fBmkproject vs\fR
vs: description? (1 line): \fBVisual Spreadsheet\fR
%  \fBchproject vs\fR
%  \fBpmkdir doc src work\fR
doc: description? (1 line): \fBvs user's guide\fR
src: description? (1 line): \fBvs program source code\fR
work: description? (1 line): \fBvs workbench\fR
%
.DE
.KF
.sp 11
.SM
.ce
\fIFigure 2.  \fRLayout of the project `vs'

.NL
.KE
.NH 2
Displaying a Project
.XS
\*(SN Displaying a Project
.XE
.PP
The
.I ppd
``\fBp\fRrint \fBp\fRroject \fBd\fRirectory'' command may be used to list
the directories belonging to `vs':
.DS
%  \fBppd\fR
doc		src		work
%
.DE
Alternatively, a table of contents for the project can be obtained by using
.I ppd
with the \fB\-d\fR description option to print the description of each project
directory.
.DS
%  \fBppd  \-d\fR
doc		vs user's guide
src		vs program source code
work	vs workbench
%
.DE
.NH 2
Moving Around Inside a Project
.XS
\*(SN Moving Around Inside a Project
.XE
.PP
The
.I pd
command provides a convenient way for changing to another project directory
without the user having to remember it's precise location. For example,
to move to the source code directory `src', type
.ID
%  \fBpd  src\fR
.DE
To change to the directory `work', type
.ID
%  \fBpd  work\fR
.DE
To return to the project root directory, type
.ID
%  \fBpd\fR
.DE
without any arguments.
.NH 2
Compiling a Program
.XS
\*(SN Compiling a Program
.XE
.PP
Program development and maintenance is handled by the
.I make
command\|[3].
.I Make
mechanizes many development and maintenance activities, including
compiling and linking of programs, printing of source code, and the removal
of unneeded files. The instructions which tell
.I make
how to perform these duties are kept in a special file known as a makefile,
together with the names of the source code files which make up the
program. The makefile editor program,
.I mkmf,
creates the makefile (named `Makefile' by default) by gathering up
the names of all the source code files in the current working directory and
inserting them into a standard makefile.
.PP
The following example shows how to produce the program for the visual
spreadsheet, given the file `vs.c' containing the source code in the directory
`src'.
.DS
%  \fBmkmf\fR
mkmf: creating Makefile from template /usr/new/lib/p.Makefile
%  \fBmake\fR
cc \-c vs.c
Loading a.out ... done
%
.DE
In this example the executable program is called `a.out'. However, by using the
makefile editor interactively the name `vs' could have been specified instead:
.DS
%  \fBmkmf  \-i\fR
mkmf: creating Makefile from template /usr/new/lib/p.Makefile
program name? \fBvs\fR
destination directory?
%  \fBmake\fR
cc \-c vs.c
Loading vs ... done
%
.DE
Since a carriage return was typed in response to the second question in
the example above, the destination directory for the program remains the
current directory.
.PP
Because program
.I vs
is a screen-oriented program, it would not be
surprising if it requires special functions to control cursor movement
and updating of the terminal screen. There is a standard package of C
library functions for this purpose called `curses'\|[1], and if the program
has taken advantage of these functions, this library should be included
in the makefile together with the terminal database package `termlib'.
This can be done by including the LIBS macro definition as an argument to the
.I mkmf
command\**
.FS
Arguments with embedded blanks in UNIX commands must be enclosed by double
quotes.
.FE
.ID
%  \fBmkmf  \-i  "LIBS=\-lcurses \-ltermlib"\fR
.DE
.NH 2
Moving Files Within a Project
.XS
\*(SN Moving Files Within a Project
.XE
.PP
A file can be moved to another project directory by using the
.I pmv
command. For instance, the following command moves the executable program
.I vs
from the current working directory to the `work' directory
.ID
%  \fBpmv  vs  work\fR
.DE
In a similar manner, files can be copied from one project directory
to another using the
.I pcp
command.
.I Pmv
and
.I pcp
behave very similarly to the standard
.UX
.I mv
and
.I cp
commands in that they blindly overwrite any existing files of the same
name in the destination directory unless the
.B \-i
interactive option is used.
.NH 2
More on Building a Project
.XS
\*(SN More on Building a Project
.XE
.PP
As development of a software package continues, extra project directories may
be needed to support the work. For example, project `vs' must accommodate
an additional program called
.I vstutor
which provides instruction on the use of the visual spreadsheet program;
two library packages called `hash' and `list' for hash table and linked
list operations; and three files that are ``included'' in more than one
source file \- `vs.h' which contains common program definitions,
`hash.h' which defines hash tables, and `list.h' which holds linked
list definitions. Figure 3 shows the extra directories needed for
these components and the following command sequence creates them
.DS
%  \fBpd\fR
%  \fBpmkdir bin include lib\fR
bin: description? (1 line): \fBvs and vstutor programs\fR
include: description? (1 line): \fBcommon included files\fR
lib: description? (1 line): \fBcompiled hash table and list libraries\fR
%  \fBpd src\fR
%  \fBpmkdir vs vstutor libhash liblist\fR
vs: description? (1 line): \fBvs program source code\fR
vstutor: description? (1 line): \fBvstutor program source code\fR
libhash: description? (1 line): \fBhash table library source code\fR
liblist: description? (1 line): \fBlist library source code\fR
%
.DE
The final step is to change the description of the `src' directory now
that it has been subdivided into four separate source code
directories. This can be done by using the
.I pmkdir
with the \fB+d\fR (change \fBd\fRescription) option
.DS
%  \fBpmkdir +d  src\fR
src: description? (1 line): \fBC source code\fR
%
.DE
.KF
.sp 17
.SM
.ce
\fIFigure 3.  \fRRevised layout of project `vs'

.NL
.KE
Note that in Figure 3 there are two directories called `vs'. The top one
bears the name of the project, and the bottom one is named according to the
program contained within it. Similarly, the directories `libhash' and
`liblist' are named according to libraries that they contain.
.NH 2
Creating a Program Library
.XS
\*(SN Creating a Program Library
.XE
.PP
A program library is a collection of compiled subroutines that are shared
by more than one program. In the
.UX
environment a program library is stored as an
.I archive
file. Each member of the archive is an object file containing one or
more compiled subroutines. By convention a library archive file is named
\fBlib\fIname\fR.\fBa\fR where
.I name
is the name of the program library.
.PP
The example below shows how to create a program library for the hash table
subroutines in the `libhash' directory. Note that the
.I mkmf
command must be given with the \fB\-l\fR option so that a makefile will be
created for a library rather than a program.
.DS
% \fBmkmf  \-i  \-l\fR
mkmf: creating Makefile from template /usr/new/lib/l.Makefile
library name? \fBlibhash.a\fR
destination directory? \fB../../lib\fR
% \fBmake\fR
cc \-c hthash.c
cc \-c htinit.c
cc \-c htinstall.c
cc \-c htlookup.c
cc \-c htrm.c
Loading libhash.a ... done
%
.DE
Since the `lib' directory is in the same
project as the `libhash' directory, the path to `lib' is
made
.I relative
to `libhash' so that the project will be portable.
.PP
The next step is to install the program library in the `lib' project directory
where the
.I vs
and
.I vstutor
programs can access it easily.
.DS
% \fBmake install\fR
Installing libhash.a in ../../lib
%
.DE
.NH 2
More on Developing a Program
.XS
\*(SN More on Developing a Program
.XE
.NH 3
\fIIncluded files\fR
.XS
\*(SN Included files
.XE
.PP
Definitions which are common to more than one source code file (e.g. buffer
sizes, data structure definitions) should be declared only once in a program.
This can be achieved by keeping such definitions in files separate from the
main program and ``including'' them at compilation time. In C, Fortran, and
Pascal programs, the contents of a file can be included by the statement
.ID
#include  "filename"
.DE
By convention
.I filename
ends in \fB.h\fR and is commonly referred to as a
.I header
file. Hence, in the source code for programs
.I vs
and
.I vstutor,
the statements
.DS
#include "vs.h"
#include "hash.h"
#include "list.h"
.DE
include common program definitions, hash table definitions, and linked
list definitions respectively.
.PP
Since the header files in this example are used in more than
one program, they should be placed in the `include' directory where they can
be accessed easily. Although the include statements can be rewritten as
.DS
#include "../../include/vs.h"
#include "../../include/hash.h"
#include "../../include/list.h"
.DE
it is better to tell the compiler where the header files are by using the
.B \-I
compiler option\** as follows
.FS
C and Fortran compilers only.
.FE
.ID
\-I../../include
.DE
This is done most conveniently by adding the option to the compiler
flags in the makefile (see \(sc\|4.1.1).
.NH 3
\fIProgram libraries\fR
.XS
\*(SN Program libraries
.XE
.PP
The LIBS macro definition in a makefile specifies the libraries that
are to be used by the link editor for resolving references to
subroutines that are not found in the program source code. Because
.I make
checks to see if the libraries needed by a program have changed since
the last time the program was made, their pathnames must be defined
explicitly. In the makefiles belonging to programs
.I vs
and
.I vstutor,
the LIBS macro definition looks like
.DS
LIBS = \kx../../lib/libhash.a \\\\
\h'|\nxu'\&../../lib/liblist.a \\\\
\h'|\nxu'\&/usr/lib/libcurses.a \\\\
\h'|\nxu'\&/usr/lib/libtermlib.a
.DE
Note also that when this macro definition was added to the makefile by
the command
.PP
%  \fBmkmf "LIBS=../../lib/libhash.a  ../../lib/liblist.a  \-lcurses  \-ltermlib"\fR
.LP
to include the `hash' and `list' libraries, the `curses' and `termlib'
libraries were automatically expanded to full pathnames by the makefile editor.
.NH 3
\fIInstallation\fR
.XS
\*(SN Installation
.XE
.PP
Once a program has been completed, it should be installed in a place
where it will be generally available \- that is, in a directory which
is in the command search path specified by the PATH environment
variable. In the case of the project `vs', if the `bin' directory is in
the search path, this might be a good place to install the
.I vs
and
.I vstutor
programs. If the makefiles for these programs do not already specify `bin' as
their destination directory, it can be added by the command
.ID
%  \fBmkmf  "DEST=../../bin"\fR
.DE
Then, each program can be installed by the
.I
make install
.R
command. For the program \fIvs\fR:
.DS
%  \fBpd vs\fR
%  \fBmake install\fR
Installing vs in ../../bin
%
.DE
and for the program \fIvstutor\fR:
.DS
%  \fBpd vstutor\fR
%  \fBmake install\fR
Installing vstutor in ../../bin
%
.DE
.NH 2
Global Operations
.XS
\*(SN Global Operations
.XE
.PP
One of the goals of SPMS is to reduce the effort associated with software
maintenance. This can be achieved by treating a software package as an
atomic unit \- that is, a single entity on which to perform operations.
The mechanism for executing a command over an entire software package
is provided by the
.I pexec
command. This command takes another command as an argument and executes it
in each of the directories belonging to a project, as in
.ID
%  \fBpexec  ls\fR
.DE
which lists the names of all the files in a project.
.NH 3
\fIDirectory selection\fR
.XS
\*(SN Directory selection
.XE
.PP
By labeling each project directory according to the type of activity that it
supports, global operations can be restricted to specific directories. These
labels, which are known as
.I
type labels,
.R
are attached to project directories by the
.I pmkdir
command, and removed by the
.I prmdir
command\**.
.FS
Except in the case of project root directories, where
.I mkproject
and
.I rmproject
must be used.
.FE
For instance, if the directories containing source code in project `vs'  are
labeled `src' by
.ID
%  \fBpmkdir  +T\|src  include libhash liblist vs vstutor\fR
.DE
then, the total number of lines of source code in a project can be counted by
giving the command
.ID
%  \fBpexec  \-T\|src  \'cat \(**.h \(**.c\^\'  |  wc \-l\fR
.DE
where quotes surround the
.I cat
command to prevent file name expansion in the current directory.
.PP
If a project directory supports more than one type of activity,
labels corresponding to each of the activities can be attached to
the directory.
.NH 3
\fIDirectory order\fR
.XS
\*(SN Directory order
.XE
.PP
In some instances the directories affected by a global command must be
processed in a particular order. For example, when installing a software
package which has both libraries and programs, the libraries should be
installed first. This ordering is achieved by appending priorities to type
labels. In the case of the project `vs', if the directories containing the
program and library source code are labeled `install' with the following
priorities
.so install.tbl
by the commands
.DS
%  \fBpmkdir  +T\|install.1  libhash  liblist\fR
%  \fBpmkdir  +T\|install.2  vs  vstutor\fR
.DE
then, the command
.ID
%  \fBpexec  \-T\|install  make  install\fR
.DE
installs the `vs' software package in the order shown in figure 4.
.KF
.sp 18
.SM
.ce
\fIFigure 4.  \fROrdering for `install' directories

.NL
.KE
.KS
.PP
In a similar fashion, if the directories containing source code are
labeled `print' with the following priorities,
.so print.tbl
.KE
a source code listing for the entire project may be obtained by the command
.ID
%  \fBpexec  \-T\|print  \'pr  \(**.h  \(**.c\'  |  lpr\fR
.DE
in the order shown in figure 5.
.KF
.sp 18
.SM
.ce
\fIFigure 5.  \fROrdering for `print' directories

.NL
.KE
.NH 2
Locating Files in a Project
.XS
\*(SN Locating Files in a Project
.XE
.PP
When the location of a file within a project is unknown, it can be
found by using the
.I pfind
command. For example, the command
.ID
%  \fB pfind  Makefile\fR
.DE
searches for all occurrences of `Makefile' in project `vs' and produces
the output
.ID
\&...^vs/Makefile
\&...^vstutor/Makefile
\&...^libhash/Makefile
\&...^liblist/Makefile
.DE
.PP
In a large project, the time required to search for a file can be reduced
by telling
.I pfind
to scan only those directories in which there is some likelihood of the
file being found. In the example above, since makefiles are only likely
to be found in source code directories (i.e. directories having type
label `src'), the command could have been given as
.ID
%  \fBpfind  \-T\|src  Makefile\fR
.DE
.NH 2
Searching Files for Patterns
.XS
\*(SN Searching Files for Patterns
.XE
.PP
Sometimes it is necessary to look at all the files in a software
package that contain a certain pattern\**.
.FS
The term
.I pattern
is used to denote a set of strings.
.FE
One reason might be to find all of the places from which a subroutine
is called, perhaps with the intent of altering its arguments. The
.I pgrep
command searchs through specified files in a project for lines
matching a given pattern. For example,
.ID
%  \fBpgrep  \-T\|src  listappend  \'\(**.h \(**.c\'\fR
.DE
will search all the C source code files in project `vs' for the function
`listappend'. Because of the
.B \-T
option,
.I pgrep
searchs only in those directories which have the `src' type label.
.PP
An alternative way for specifying file names is to use the
.B \-m
option. This causes
.I pgrep
to fetch the names of source code files from the HDRS and SRCS macro
definitions in a makefile. Consequently, the command in the example
above could have been expressed as
.ID
%  \fBpgrep  \-T\|src  \-m  listappend\fR
.DE
.PP
If the pattern contains characters that have a special meaning to the
shell, such as \fB\(**\fR or \fB^\fR, the pattern should be quoted.
For example,
.ID
%  \fBpgrep  \-T\|src  \-m  \'ht.\(**(\'\fR
.DE
finds all of the places where functions from the hash table library
are used.
.NH 2
Changing the Working Project
.XS
\*(SN Changing the Working Project
.XE
.PP
Along with a working directory, each user has a
.I
working project.
.R
Immediately after logging in, the working project is the root project `^'.
To change to a new working project, the
.I chproject
command must be used, as in
.ID
%  \fBchproject vs\fR
.DE
which makes `vs' the current (or working) project. To return to the
root project, execute the command
.ID
%  \fBchproject  ^\fR
.DE
To find out the name of the working project, type
.ID
%  \fBpwp\fR
.DE


