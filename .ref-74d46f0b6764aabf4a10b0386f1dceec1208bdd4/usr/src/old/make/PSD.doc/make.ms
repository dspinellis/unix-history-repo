.\" %sccs.include.proprietary.roff%
.\"
.\"	@(#)make.ms	6.2 (Berkeley) %G%
.\"
.EH 'PS1:12-%''Make \(em A Program for Maintaining Computer Programs'
.OH 'Make \(em A Program for Maintaining Computer Programs''PS1:12-%'
.....TR 57
.\".RP
.de IT
.if n .ul
\&\\$3\f2\\$1\fR\^\&\\$2
..
.TL
Make \(em A Program for Maintaining Computer Programs
.AU
S. I. Feldman
.AI
.MH
.AB
.PP
In a programming project, it is easy to lose track of which files need
to be reprocessed or recompiled after a change is made in some part of the source.
.I Make
provides a simple mechanism for maintaining up-to-date versions of programs that result
from many operations on a number of files.
It is possible to tell
.I Make
the sequence of commands that create certain files,
and the list of files that require other files to be current before the operations can be done.
Whenever a change is made in any part of the program,
the
.I Make
command will create the proper files simply, correctly,
and with a minimum amount of effort.
.PP
The basic operation of
.I Make
is to find the name of a needed target in the description, ensure that all of the files on which it depends exist and
are up to date, and then create the target if it has not been modified since its generators were.
The description file really defines the graph of dependencies;
.I Make
does a depth-first search of this graph
to determine what work is really necessary.
.PP
.I Make
also provides a simple macro substitution facility
and the ability to encapsulate commands in a single file
for convenient administration.
.sp 2
Revised April, 1986
.AE
.SH
Introduction
.PP
It is common practice to divide large programs into smaller, more manageable pieces.
The pieces may require quite different treatments:
some may need to be run through a macro processor, some may need to be processed by
a sophisticated program generator (e.g., Yacc[1] or Lex[2]).
The outputs of these generators may then have to be compiled with special options and with
certain definitions and declarations.
The code resulting from these transformations may then need to be loaded together with
certain libraries under the control of special options.
Related maintenance activities involve running complicated test scripts
and installing validated modules.
Unfortunately, it is very easy for a programmer to forget which files depend on which others,
which files have been modified recently, and the exact sequence of operations
needed to make or exercise a new version of the program.
After a long editing session, one may easily lose track of which files have been changed
and which object modules are still valid,
since a change to a declaration can obsolete a dozen other files.
Forgetting to compile a routine that has been changed or that uses changed declarations will result in
a program that will not work, and a bug that can be very hard to track down.
On the other hand, recompiling everything in sight just to be safe is very wasteful.
.PP
The program described in this report mechanizes many of the activities of program development
and maintenance.
If the information on inter-file dependences and command sequences is stored in a file, the simple command
.DS
make
.DE
is frequently sufficient to update the interesting files,
regardless of the number that have been edited since the last ``make''.
In most cases, the description file is easy to write and changes infrequently.
It is usually easier to type the
.IT make
command than to issue even one of the needed operations, so the typical cycle of program development operations becomes
.DS
think \(em edit \(em \fImake\fR \(em test  . . .
.DE
.PP
.IT Make
is most useful for medium-sized programming projects;
it does not solve the problems of maintaining multiple source versions
or of describing huge programs.
.IT Make
was designed for use on Unix, but a version runs on GCOS.
.SH
Basic Features
.PP
The basic operation of
.IT make
is to update a target file by ensuring
that all of the files on which it depends exist and are up to date,
then creating the target if it has not been modified since its dependents were.
.IT Make
does a depth-first search of the graph of dependences.
The operation of the command depends on the ability to find the date and time
that a file was last modified.
.PP
To illustrate, let us consider a simple example:
A program named
.IT prog
is made by compiling and loading three C-language files
.IT x.c ,
.IT y.c ,
and
.IT z.c
with the
.IT lS
library.
By convention, the output of the C compilations will be found in files named
.IT x.o ,
.IT y.o ,
and
.IT z.o .
Assume that the files
.IT x.c
and
.IT y.c
share some declarations in a file named
.IT defs ,
but that
.IT z.c
does not.
That is,
.IT x.c
and
.IT y.c
have the line
.DS
#include "defs"
.DE
The following text describes the relationships and operations:
.DS
prog :  x.o  y.o  z.o
	cc  x.o  y.o  z.o   \-lS  \-o  prog
.sp .5
x.o  y.o :   defs
.DE
If this information were stored in a file named
.IT makefile ,
the command
.DS
make
.DE
would perform the operations needed to recreate
.IT prog
after any changes had been made to any of the four source files
.IT x.c ,
.IT y.c ,
.IT z.c ,
or
.IT defs .
.PP
.IT Make
operates using three sources of information:
a user-supplied description file (as above),
file names and ``last-modified'' times from the file system,
and built-in rules to bridge some of the gaps.
In our example, the first line says that
.IT prog
depends on three ``\fI.o\fR'' files.
Once these object files are current, the second line describes how to load them to create
.IT prog .
The third line says that
.IT x.o
and
.IT y.o
depend on the file
.IT defs .
From the file system,
.IT make
discovers that there are three ``\fI.c\fR'' files corresponding to the needed ``\fI.o\fR'' files,
and uses built-in information on how to generate an object from a source file
(\fIi.e.,\fR issue a ``cc\ \-c'' command).
.PP
The following long-winded description file is equivalent to the one above, but
takes no advantage of
.IT make 's
innate knowledge:
.DS
prog :  x.o  y.o  z.o
	cc  x.o  y.o  z.o  \-lS  \-o  prog
.sp .3
x.o :  x.c  defs
	cc  \-c  x.c
y.o :  y.c  defs
	cc  \-c  y.c
z.o :  z.c
	cc  \-c  z.c
.DE
.PP
If none of the source or object files had changed since the last time
.IT prog
was made, all of the files would be current, and
the command
.DS
make
.DE
would just announce this fact and stop.
If, however, the
.IT defs
file had been edited,
.IT x.c
and
.IT y.c
(but not
.IT z.c )
would be recompiled, and then
.IT prog
would be created from the new ``\fI.o\fR'' files.
If only the file
.IT y.c
had changed, only it would be recompiled, but it would still be necessary to reload
.IT prog .
.PP
If no target name is given on the
.IT make
command line, the first target mentioned in the description is created;
otherwise the specified targets are made.
The command
.DS
make x.o
.DE
would recompile
.IT x.o
if
.IT x.c
or
.IT defs
had changed.
.PP
If the file exists after the commands are executed,
its time of last modification is used in further decisions;
otherwise the current time is used.
It is often quite useful to include rules with mnemonic names and commands that do not
actually produce a file with that name.
These entries can take advantage of
.IT make 's
ability to generate files and substitute macros.
Thus, an entry
``save''
might be included to copy a certain set of files, or an entry
``cleanup''
might be used to throw away unneeded intermediate files.
In other cases one may maintain a zero-length file purely to keep track
of the time at which certain actions were performed.
This technique is useful for maintaining remote archives and listings.
.PP
.IT Make
has a simple macro mechanism for substituting in dependency lines and command strings.
Macros are defined by command arguments or description file lines with embedded equal signs.
A macro is invoked by preceding the name by a dollar sign;
macro names longer than one character must be parenthesized.
The name of the macro is either the single character after the dollar sign or a name inside parentheses.
The following are valid macro invocations:
.DS
$(CFLAGS)
$2
$(xy)
$Z
$(Z)
.DE
The last two invocations are identical.
$$ is a dollar sign.
All of these macros are assigned values during input, as shown below.
Four special macros change values during the execution of the command:
$\(**, $@, $?, and $<.
They will be discussed later.
The following fragment shows the use:
.DS
OBJECTS = x.o y.o z.o
LIBES = \-lS
prog: $(OBJECTS)
	cc $(OBJECTS)  $(LIBES)  \-o prog
  . . .
.DE
The command
.DS
make
.DE
loads the three object files with the
.IT lS
library.  The command
.DS
make  "LIBES= \-ll \-lS"
.DE
loads them with both the Lex (``\-ll'') and the Standard (``\-lS'') libraries,
since macro definitions on the command line override definitions in the description.
(It is necessary to quote arguments with embedded blanks in
.UX
commands.)
.PP
The following sections detail the form of description files and the command line,
and discuss options and built-in rules in more detail.
.SH
Description Files and Substitutions
.PP
A description file contains three types of information:
macro definitions,
dependency information,
and executable commands.
There is also a comment convention:
all characters after a sharp (#) are ignored, as is the sharp itself.
Blank lines and lines beginning with a sharp are totally ignored.
If a non-comment line is too long, it can be continued using a backslash.
If the last character of a line is a backslash, the backslash, newline,
and following blanks and tabs are replaced by a single blank.
.PP
A macro definition is a line containing an equal sign not preceded by a colon or a tab.
The name (string of letters and digits) to the left of the equal sign
(trailing blanks and tabs are stripped) is assigned the string of characters following the equal sign
(leading blanks and tabs are stripped.)
The following are valid macro definitions:
.DS
2 = xyz
abc = \-ll \-ly \-lS
LIBES =
.DE
The last definition assigns LIBES the null string.
A macro that is never explicitly defined has the null string as value.
Macro definitions may also appear on the
.IT make
command line (see below).
.PP
Other lines give information about target files.
The general form of an entry is:
.DS
target1 [target2 . . .] :[:] [dependent1 . . .] [; commands] [# . . .]
[\fI(tab)\fR commands] [# . . .]
 . . .
.DE
Items inside brackets may be omitted.
Targets and dependents are strings of letters, digits, periods, and slashes.
(Shell metacharacters ``\(**'' and ``?'' are expanded.)
A command is any string of characters not including a sharp (except in quotes)
or newline.
Commands may appear either after a semicolon on a dependency line
or on lines beginning with a tab immediately following a dependency line.
.PP
A dependency line may have either a single or a double colon.
A target name may appear on more than one dependency line, but all of those lines must be of the
same (single or double colon) type.
.IP 1.
For the usual single-colon case,
at most one of these dependency lines may have a command sequence associated with it.
If the target is out of date with any of the dependents on any of the lines,
and a command sequence is specified (even a null one following a semicolon or tab),
it is executed; otherwise a default creation rule may be invoked.
.IP 2.
In the double-colon case, a command sequence may be associated with each dependency line;
if the target is out of date with any of the files on a particular line, the associated
commands are executed.
A built-in rule may also be executed.
This detailed form is of particular value in updating archive-type files.
.PP
If a target must be created, the sequence of commands is executed.
Normally, each command line is printed and then
passed to a separate invocation of the Shell after substituting for macros.
(The printing is suppressed in silent mode or if the command line begins with an @ sign).
.IT Make
normally stops if any command signals an error by returning a non-zero error code.
(Errors are ignored if the ``\-i'' flags has been specified on the
.IT make
command line,
if the fake target name ``.IGNORE'' appears in the description file,
or if the command string in the description file begins with a hyphen.
Some
.UX
commands return meaningless status).
Because each command line is passed to a separate invocation of the Shell,
care must be taken with certain commands (e.g., \fIcd\fR and Shell control commands) that have meaning only
within a single Shell process;
the results are forgotten before the next line is executed.
.PP
Before issuing any command, certain macros are set.
$@ is set to the name of the file to be ``made''.
$? is set to the string of names that were found to be younger than the target.
If the command was generated by an implicit rule (see below),
$< is the name of the related file that caused the action, and
$\(** is the prefix shared by the current and the dependent file names.
.PP
If a file must be made but there are no explicit commands or relevant
built-in rules,
the commands associated with the name ``.DEFAULT'' are used.
If there is no such name,
.IT make
prints a message and stops.
.SH
Command Usage
.PP
The
.IT make
command takes four kinds of arguments:
macro definitions, flags, description file names, and target file names.
.DS
make [ flags ]  [ macro definitions ]  [ targets ]
.DE
The following summary of the operation of the command explains how these arguments are interpreted.
.PP
First, all macro definition arguments (arguments with embedded equal signs) are analyzed
and the assignments made.
Command-line macros override corresponding definitions found in the description files.
.PP
Next, the flag arguments are examined.
The permissible flags are
.IP \-i
Ignore error codes returned by invoked commands.
This mode is entered if the fake target name ``.IGNORE'' appears in the description file.
.IP \-s
Silent mode.  Do not print command lines before executing.
This mode is also entered if the fake target name ``.SILENT'' appears in the description file.
.IP \-r
Do not use the built-in rules.
.IP \-n
No execute mode.  Print commands, but do not execute them.
Even lines beginning with an ``@'' sign are printed.
.IP \-t
Touch the target files (causing them to be up to date) rather than issue the usual commands.
.IP \-q
Question.
The
.IT make
command returns a zero or non-zero status code depending on whether the target file
is or is not up to date.
.IP \-p
Print out the complete set of macro definitions and target descriptions
.IP \-d
Debug mode.  Print out detailed information on files and times examined.
.IP \-f
Description file name.  The next argument is assumed to be the name of a description file.
A file name of ``\-'' denotes the standard input.
If there are no ``\-f\|'' arguments, the file named
.IT makefile
or
.IT Makefile
in the current directory is read.
The contents of the description files override the built-in rules if they are present).
.PP
Finally, the remaining arguments are assumed to be the names of targets to be made;
they are done in left to right order.
If there are no such arguments, the first name in the description files that does not
begin with a period is ``made''.
.SH
Implicit Rules
.PP
The
.ul
make
program uses a table of interesting suffixes and a set
of transformation rules to supply default dependency
information and implied commands.
(The Appendix describes these tables and means of overriding
them.)
The default suffix list is:
.KS
.sp
.nf
.ta 0.5i 1.5i
	\fI.o\fR	Object file
	\fI.c\fR	C source file
	\fI.e\fR	Efl source file
	\fI.r\fR	Ratfor source file
	\fI.f\fR	Fortran source file
	\fI.s\fR	Assembler source file
	\fI.y\fR	Yacc-C source grammar
	\fI.yr\fR	Yacc-Ratfor source grammar
	\fI.ye\fR	Yacc-Efl source grammar
	\fI.l\fR	Lex source grammar
.fi
.sp
.KE
The following diagram summarizes the default transformation paths.
If there are two paths connecting a pair of suffixes, the longer
one is used only if the intermediate file exists or is
named in the description.
.KS
.sp
.ft I
.ta 2i
	.o
.sp 2
.ta 0.75i 1.25i 1.6i 2.1i
	.c	.r	.e	.f  .s  .y  .yr  .ye  .l  .d
.sp 2
.ta 0.6i 1.25i 1.6i
	.y .l	.yr	.ye
.ft R
.sp
.KE
.PP
If the file
.ul
x.o
were needed and there were an
.ul
x.c
in the description or directory, it would be compiled.
If there were also an
.IT x.l ,
that grammar would be run through Lex before compiling the result.
However, if there were no
.ul
x.c
but there were an
.IT x.l ,
.IT make
would discard the intermediate C-language file and use the
direct link in the graph above.
.PP
It is possible to change the names of some of the compilers used in the
default, or the flag arguments with which they are invoked by knowing
the macro names used.
The compiler names are the macros AS, CC, RC, EC, YACC, YACCR, YACCE, and LEX.
The command
.DS
make CC=newcc
.DE
will cause the ``newcc'' command to be used instead of the
usual C compiler.
The macros CFLAGS, RFLAGS, EFLAGS, YFLAGS, and LFLAGS may be set to
cause these commands to be issued with optional flags.
Thus,
.DS
make "CFLAGS=\|\(miO"
.DE
causes the optimizing C compiler to be used.
.PP
Another special macro is `VPATH'.
The ``VPATH'' macro should be set to a list of directories separated by colons.
When
.I make
searches for a file as a result of a dependency relation, it will
first search the current directory and then each of the directories on the
``VPATH'' list.
If the file is found, the actual path to the file will be used, rather than
just the filename.
If ``VPATH'' is not defined, then only the current directory is searched.
Note that ``VPATH'' is intended to act like the System V ``VPATH'' support,
but there is no guarantee that it functions identically.
.PP
One use for ``VPATH'' is when one has several programs that compile from the
same source.
The source can be kept in one directory and each set of
object files (along with a separate
.IR makefile )
would be in a separate subdirectory.
The ``VPATH'' macro would point to the source directory in this case.
.SH
Example
.PP
As an example of the use of
.ul
make,
we will present the description file used to maintain
the
.ul
make
command itself.
The code for
.ul
make
is spread over a number of C source files and a Yacc grammar.
The description file contains:
.DS
# Description file for the Make command
.sp .3
P = und \-3 | opr \-r2       # send to GCOS to be printed
FILES = Makefile version.c defs main.c doname.c misc.c files.c dosys.c\
	gram.y lex.c gcos.c
OBJECTS = version.o main.o doname.o misc.o files.o dosys.o gram.o
LIBES= \-lS
LINT = lint \-p
CFLAGS = \-O
.sp .3
make:  $(OBJECTS)
	cc $(CFLAGS) $(OBJECTS) $(LIBES) \-o make
	size make
.sp .3
$(OBJECTS):  defs
gram.o: lex.c
.sp .3
cleanup:
	-rm *.o gram.c
	-du
.sp .3
install:
	@size make /usr/bin/make
	cp make /usr/bin/make ; rm make
.sp .3
print:  $(FILES)	# print recently changed files
	pr $? | $P
	touch print
.sp .3
test:
	make \-dp | grep \-v TIME >1zap
	/usr/bin/make \-dp | grep \-v TIME >2zap
	diff 1zap 2zap
	rm 1zap 2zap
.sp .3
lint :  dosys.c doname.c files.c main.c misc.c version.c gram.c
	$(LINT) dosys.c doname.c files.c main.c misc.c version.c gram.c
	rm gram.c
.sp .3
arch:
	ar uv /sys/source/s2/make.a $(FILES)
.DE
.IT Make
usually prints out each command before issuing it.
The following output results from typing the simple command
.DS
make
.DE
in a directory containing only the source and description file:
.DS
 cc  \-c version.c
 cc  \-c main.c
 cc  \-c doname.c
 cc  \-c misc.c
 cc  \-c files.c
 cc  \-c dosys.c
 yacc  gram.y
 mv y.tab.c gram.c
 cc  \-c gram.c
 cc  version.o main.o doname.o misc.o files.o dosys.o gram.o \-lS \-o make
 13188+3348+3044 = 19580b = 046174b
.DE
Although none of the source files or grammars were mentioned
by name in the description file,
.IT make
found them using its suffix rules and issued the needed commands.
The string of digits results from the ``size make''
command; the printing of the command line itself was
suppressed by an @ sign.
The @ sign on the
.IT size
command in the description file suppressed the printing of the command,
so only the sizes are written.
.PP
The last few entries in the description file are useful maintenance sequences.
The ``print'' entry prints only the files that have been changed since the last
``make print'' command.
A zero-length file
.IT print
is maintained to keep track of the time of the printing;
the $? macro in the command line then picks up only the names of the files
changed since
.IT print
was touched.
The printed output can be sent to a different printer or to a file by changing the definition of the
.IT P
macro:
.DS
make print "P = opr \-sp"
	\fIor\fR
make print "P=  cat >zap"
.DE
.SH
Suggestions and Warnings
.PP
The most common difficulties arise from
.IT make 's
specific meaning of dependency.
If file
.IT x.c
has a ``#include "defs"''
line, then the object file
.IT x.o
depends on
.IT defs ;
the source file
.IT x.c
does not.
(If
.IT defs
is changed, it is not necessary to do anything
to the file
.IT x.c ,
while it is necessary to recreate
.IT x.o .)
.PP
To discover what
.IT make
would do, the ``\-n'' option is very useful.
The command
.DS
make \-n
.DE
orders
.IT make
to print out the commands it would issue without actually taking the time to execute them.
If a change to a file is absolutely certain to be benign
(e.g., adding a new definition to an include file),
the ``\-t'' (touch) option
can save a lot of time:
instead of issuing a large number of superfluous recompilations,
.IT make
updates the modification times on the affected file.
Thus, the command
.DS
make \-ts
.DE
(``touch silently'') causes the relevant files to appear up to date.
Obvious care is necessary, since this mode of operation subverts
the intention of
.IT make
and destroys all memory of the previous relationships.
.PP
The debugging flag (``\-d'') causes
.IT make
to print out a very detailed description of what it is doing, including the
file times.  The output is verbose, and recommended only as a last resort.
.SH
Acknowledgments
.PP
I would like to thank S. C. Johnson for suggesting this approach
to program maintenance control.
I would like to thank S. C. Johnson and H. Gajewska for being
the prime guinea pigs during development of
.IT make .
.SH
References
.IP 1.
S. C. Johnson,
``Yacc \(em Yet Another Compiler-Compiler'',
Bell Laboratories
Computing Science Technical Report #32,
July 1978.
.IP 2.
M. E. Lesk,
``Lex \(em A Lexical Analyzer Generator'',
Computing Science Technical Report #39,
October 1975.
.bp
.SH
Appendix.  Suffixes and Transformation Rules
.PP
The
.ul
make
program itself does not know what file name suffixes are interesting
or how to transform a file with one suffix into a file with another
suffix.
This information is stored in an internal table that has the form of a description file.
If the ``\-r'' flag is used, this table is not used.
.PP
The list of suffixes is actually the dependency list for the name
``.SUFFIXES'';
.ul
make
looks for a file with any of the suffixes on the list.
If such a file exists, and if there is a transformation
rule for that combination,
.ul
make
acts as described earlier.
The transformation rule names are the concatenation of the
two suffixes.
The name of the rule to transform a ``\fI.r\fR'' file to a ``\fI.o\fR'' file
is thus ``\fI.r.o\fR''.
If the rule is present and no explicit command sequence
has been given in the user's description files, the command
sequence for the rule ``.r.o'' is used.
If a command is generated by using one of these suffixing rules,
the macro $\(** is given the value of the stem
(everything but the suffix) of the name of the file to be made,
and the macro $< is the name of the dependent that caused the action.
.PP
The order of the suffix list is significant, since it is scanned from
left to right, and the first name that is formed that has both a file
and a rule associated with it is used.
If new names are to be appended, the user can just add an entry for
``.SUFFIXES'' in his own description file; the dependents will be added to the usual list.
A ``.SUFFIXES'' line without any dependents deletes the current list.
(It is necessary to clear the current list if the order of names is to be changed).
.PP
The following is an excerpt from the default rules file:
.DS
.ta .5i 1i
 .SUFFIXES : .o .c .e .r .f .y .yr .ye .l .s
 YACC=yacc
 YACCR=yacc \-r
 YACCE=yacc \-e
 YFLAGS=
 LEX=lex
 LFLAGS=
 CC=cc
 AS=as \-
 CFLAGS=
 RC=ec
 RFLAGS=
 EC=ec
 EFLAGS=
 FFLAGS=
 .c.o :
 	$(CC) $(CFLAGS) \-c $<
 .e.o .r.o .f.o :
 	$(EC) $(RFLAGS) $(EFLAGS) $(FFLAGS) \-c $<
 .s.o :
 	$(AS) \-o $@ $<
 .y.o :
 	$(YACC) $(YFLAGS) $<
 	$(CC) $(CFLAGS) \-c y.tab.c
 	rm y.tab.c
 	mv y.tab.o $@
 .y.c :
 	$(YACC) $(YFLAGS) $<
 	mv y.tab.c $@
.DE
