.\" Copyright (c) 1980 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)intro.ms	6.3 (Berkeley) 5/14/86
.\"
.de IR
\fI\\$1\^\fR\\$2
..
.de RI
\fR\\$1\fI\\$2\^\fR\\$3
..
.TL
INTRODUCTION TO USER'S REFERENCE MANUAL
.OH 'Introduction''- % -'
.EH '- % -''Introduction'
.af % i
.pn 6
.LP
The documentation has been reorganized for 4.3BSD in a format similar
to the one used for the Usenix 4.2BSD manuals.
It is divided into three sets; each set consists of one or more volumes.
The abbreviations for the volume names are listed in square brackets;
the abbreviations for the manual sections are listed in parenthesis.
.DS
I. User's Documents
	User's Reference Manual [URM]
		Commands (1)
		Games (6)
		Macro packages and language conventions (7)
	User's Supplementary Documents [USD]
		Getting Started
		Basic Utilities
		Communicating with the World
		Text Editing
		Document Preparation
		Amusements

II. Programmer's Documents
	Programmer's Reference Manual [PRM]
		System calls (2)
		Subroutines (3)
		Special files (4)
		File formats and conventions (5)
	Programmer's Supplementary Documents, Volume 1 [PS1]
		Languages in common use
		General Reference
		Programming Tools
		Programming Libraries
	Programmer's Supplementary Documents, Volume 2 [PS2]
		Documents of Historic Interest
		Other Languages
		Database Management

III. System Manager's Manual [SMM]
	Maintenance commands (8)
	System Installation and Administration
	Supporting Documentation
.DE
.LP
References to individual documents are given as ``volume:document'',
thus USD:1 refers to the first document in the ``User's Supplementary
Documents''.
References to manual pages are given as ``\fIname\fP(section)'' thus
.IR sh (1)
refers to the shell manual entry in section 1.
.LP
The manual pages give descriptions of the publicly available features of the
.UX \s-2/32V\s0
system, as extended to provide a virtual memory environment
and other enhancements at the University of California.
They do not attempt to provide perspective or tutorial information about the
.UX
operating system, its facilities, or its implementation.
Various documents on those topics are contained in the
``UNIX User's Supplementary Documents'' (USD), the
``UNIX Programmer's Supplementary Documents'' (PS1 and PS2),
and ``UNIX System Manager's Manual'' (SMM).
In particular, for an overview see ``The UNIX Time-Sharing System'' (PS2:1)
by Ritchie and Thompson; for a tutorial see
``\s8UNIX\s10 for Beginners'' (USD:1) by Kernighan,
and for an guide to the new features of this virtual version, see
``Berkeley Software Architecture Manual (4.3 Edition)'' (PS1:6).
.LP
Within the area it surveys, this volume attempts to be timely, complete
and concise.  Where the latter two objectives conflict,
the obvious is often left unsaid in favor of brevity.
It is intended that each program be described as it is, not as it should be.
Inevitably, this means that various sections will soon be out of date.
.LP
Commands are programs intended to be invoked directly by
the user, in contrast to subroutines, that are
intended to be called by the user's programs.
User commands are described in URM section 1.
Commands generally reside in directory
.I /bin
(for
.IR bin \|ary
programs).
Some programs also reside in
.I
/\|usr/\|bin,
.R
.I
/\|usr/\|ucb,
.R
or
.I
/\|usr/\|new,
.R
to save space in
.I  /\|bin.
These directories are searched automatically by the command interpreters.
.LP
Games have been relegated to URM section 6 and
.I
/\|usr/\|games,
.R
to keep them from contaminating
the more staid information of URM section 1.
.LP
Miscellaneous collection of information necessary for
writing in various specialized languages such as character codes, 
macro packages for typesetting, etc is contained in URM section 7.
.LP
System calls are entries into the
.UX
supervisor.  The system call interface is identical to a C language
procedure call; the equivalent C procedures are described in PRM section 2.
.LP
An assortment of subroutines is available;
they are described in PRM section 3.
The primary libraries in which they are kept are described in
.IR intro (3).
The functions are described in terms of C;
those that will work with Fortran are described in
.IR intro (3f).
.LP
PRM section 4 discusses the characteristics of
each system ``file'' that refers to an I/O device.
The names in this section refer to the DEC device names for the hardware,
instead of the names of the special files themselves.
.LP
The file formats and conventions (PRM section 5)
documents the structure of particular kinds of files;
for example, the form of the output of the loader and
assembler is given.  Excluded are files used by only one command,
for example the assembler's intermediate files.
.LP
Commands and procedures intended for use primarily by the
system administrator are described in SMM section 8.
The commands and files described here are almost all kept in the directory
.I /\|etc.
.LP
Each section consists of independent entries of a page or so each.
The name of the entry is in the upper corners of its pages,
together with the section number, and sometimes a
letter characteristic of a subcategory, e.g. graphics is 1G,
and the math library is 3M.
Entries within each section are alphabetized.
except for PRM section 3f which appears after the rest of PRM section 3.
The page numbers of each entry start at 1;
it is infeasible to number consecutively the pages of 
a document like this that is republished in many variant forms.
.LP
All entries are based on a common format;
not all subsections always appear.
.RS
.LP
The
.I name
subsection lists the exact names of the commands and subroutines
covered under the entry and gives a short description of their purpose.
.LP
The
.IR synopsis ""
summarizes the use of the program being described.
A few conventions are used, particularly in the Commands subsection:
.LP
.RS
.B Boldface
words are considered literals, and are typed just as they appear.
.LP
Square brackets [ ] around an argument show that the argument is optional.
When an argument is given as ``name'', it always refers to a file name.
.LP
Ellipses ``.\|.\|.'' are used to show that the previous argument-prototype
may be repeated.
.LP
A final convention is used by the commands themselves.
An argument beginning with a minus sign ``\-'' usually means that it is an
option-specifying argument, even if it appears in a position where
a file name could appear.  Therefore, it is unwise to have files whose
names begin with ``\-''.
.LP
.RE
The
.IR description ""
subsection discusses in detail the subject at hand.
.LP
The
.IR files ""
subsection gives the names of files that are built into the program.
.LP
A
.I
see also
.R
subsection gives pointers to related information.
.LP
A
.I  diagnostics
subsection discusses the diagnostic indications that may be produced.
Messages that are intended to be self-explanatory are not listed.
.LP
The
.IR bugs ""
subsection gives known bugs and sometimes deficiencies.
Occasionally the suggested fix is also described.
.LP
.RE
At the beginning of URM is a table of contents,
organized by section and alphabetically within each section.
There is also a permuted index derived from the table of contents.
Within each index entry, the title of the writeup to which
it refers is followed by the appropriate section number in parentheses.
This fact is important because there is considerable
name duplication among the sections, arising principally from commands that
exist only to exercise a particular system call.
.SH
HOW TO GET STARTED
.LP
This section sketches the basic information you need to get started on UNIX;
how to log in and log out, how to communicate through your terminal,
and how to run a program.
See ``\c
.UX
for Beginners'' in (USD:1) for a more complete introduction to the system.
.LP
.I
Logging in.\ \ 
.R
Almost any ASCII terminal capable of
full duplex operation and generating
the entire character set can be used.
You must have a valid user name,
which may be obtained from the system administration.
If you will be accessing UNIX remotely, you will also
need to obtain the telephone number for the system that you will be using.
.LP
After a data connection is established,
the login procedure depends on what type of terminal you are using
and local system conventions.
If your terminal is directly connected to the computer,
it generally runs at 9600 or 19200 baud.
If you are using a modem running over a phone line,
the terminal must be set at the speed appropriate for the modem you are using,
typically 300, 1200, or 2400 baud.
The half/full duplex switch should always be set at full-duplex.
(This switch will often have to be changed
since many other systems require half-duplex).
.LP
When a connection is established, the system types ``login:'';
you type your user name, followed by the ``return'' key.
If you have a password, the system asks for it
and suppresses echo to the terminal so the password will not appear.
After you have logged in, the ``return'', ``new line'', or ``linefeed'' keys
will give exactly the same results.
A message-of-the-day usually greets you before your first prompt.
.LP
If the system types out a few garbage characters
after you have established a data connection
(the ``login:'' message at the wrong speed),
depress the ``break'' (or ``interrupt'') key.
This is a speed-independent signal to
.UX
that a different speed terminal is in use.
The system then will type ``login:,'' this time at another speed.
Continue depressing the break key until ``login:'' appears clearly,
then respond with your user name.
.LP
For all these terminals, it is important
that you type your name in lower-case if possible; if you type
upper-case letters,
.UX
will assume that your terminal cannot generate lower-case
letters and will translate all subsequent lower-case letters to upper case.
.LP
The evidence that you have successfully logged in is that a shell program
will type a prompt (``$'' or ``%'') to you.
(The shells are described below under ``How to run a program.'')
.LP
For more information, consult
.IR tset (1),
and
.IR stty (1),
which tell how to adjust terminal behavior;
.IR getty (8)
discusses the login sequence in more detail, and
.IR tty (4)
discusses terminal I/O.
.LP
.I
Logging out.\ \ 
.R
There are three ways to log out:
.IP
By typing ``logout'' or an end-of-file
indication (EOT character, control-D) to the shell.
The shell will terminate and the ``login:'' message will appear again.
.IP
You can log in directly as another user by giving a
.IR login (1)
command.
.IP
If worse comes to worse,
you can simply hang up the phone; but beware \- some machines may
lack the necessary hardware to detect that the phone has been hung up.
Ask your system administrator if this is a problem on your machine.
.LP
.I
How to communicate through your terminal.\ \ 
.R
When you type characters, a gnome deep in the system
gathers your characters and saves them in a secret place.
The characters will not be given to a program
until you type a return (or newline), as described above in
.I
Logging in.
.R
.LP
.UX
terminal I/O is full-duplex.
It has full read-ahead, which means that you can type at any time,
even while a program is typing at you.
Of course, if you type during output, the printed output will
have the input characters interspersed.
However, whatever you type will be saved up and interpreted in correct sequence.
There is a limit to the amount of read-ahead,
but it is generous and not likely to be exceeded unless
the system is in trouble.
When the read-ahead limit is exceeded, the system
throws away all the saved characters (or beeps, if your prompt was a ``%'').
.LP
The delete (DEL) character in typed input kills all the
preceding characters in the line,
so typing mistakes can be repaired on a single line.
Also, the backspace character (control-H) erases the last character typed.
.IR Tset (1)
or
.IR stty (1)
can be used to change these defaults.
Successive uses of backspace erases characters back to, but
not beyond, the beginning of the line.
DEL and backspace can be transmitted to a program by preceding them with ``\e''.
(So, to erase ``\e'', you need two backspaces).
.LP
An
.I
interrupt signal
.R
is sent to a program by typing control-C or the ``break'' key
which is not passed to programs.
This signal generally causes whatever program you are running to terminate.
It is typically used to stop a long printout that you do not want.
However, programs can arrange either to ignore this signal altogether,
or to be notified when it happens (instead of being terminated).
The editor, for example, catches interrupts and stops what it is doing,
instead of terminating, so that an interrupt can
be used to halt an editor printout without losing the file being edited.
The interrupt character can also be changed with
.IR tset (1)
or
.IR stty (1).
.LP
It is also possible to suspend output temporarily using ^S (control-S)
and later resume output with ^Q (control-Q).
Output can be thrown away without interrupting
the program by typing ^O (control-O); see
.IR tty (4).
.LP
The
.IR quit ""
signal is generated by typing the \s8ASCII\s10 FS character.
(FS appears many places on different terminals, most commonly
as control-\e or control-\^|\^.)
It not only causes a running program to terminate
but also generates a file with the core image of the terminated process.
Quit is useful for debugging.
.LP
Besides adapting to the speed of the terminal,
.UX
tries to be intelligent about whether
you have a terminal with the newline function
or whether it must be simulated with carriage-return and line-feed.
In the latter case, all input carriage returns
are turned to newline characters (the standard line delimiter)
and both a carriage return and a line feed are echoed to the terminal.
If you get into the wrong mode, the
.IR reset (1)
command will rescue you.
If the terminal does not appear to be echoing anything that you type,
it may be stuck in ``no-echo'' or ``raw'' mode.
Try typing ``(control-J)reset(control-J)'' to recover.
.LP
Tab characters are used freely in
.UX
source programs.
If your terminal does not have the tab function,
you can arrange to have them turned into spaces
during output, and echoed as spaces during input.
The system assumes that tabs are set every eight columns.
Again, the
.IR tset (1)
or
.IR stty (1)
command can be used to change these defaults.
.IR Tset (1)
can be used to set the tab stops automatically when necessary.
.LP
.I
How to run a program; the shells.\ \ 
.R
When you have successfully logged in, a program
called a shell is listening to your terminal.
The shell reads typed-in lines, splits them up
into a command name and arguments, and executes the command.
A command is simply an executable program.
The shell looks in several system directories to find the command.  You can also
place commands in your own directory and have the shell find them there.
There is nothing special about system-provided
commands except that they are kept in a directory where the shell can find them.
.LP
The command name is always the first word on an input line;
it and its arguments are separated from one another by spaces.
.LP
When a program terminates, the shell will ordinarily regain control and type 
a prompt at you to show that it is ready for another command.
.LP
The shells have many other capabilities, that are described in detail in
sections
.IR sh (1)
and
.IR csh (1).
If the shell prompts you with ``$'', then it is an instance of
.IR sh (1)
the standard shell provided by Bell Labs.
If it prompts with ``%'' then it is an instance of
.IR csh (1),
a shell written at Berkeley.
The shells are different for all but the most simple terminal usage.
Most users at Berkeley choose
.IR csh (1)
because of the
.I history
mechanism and the
.I alias
feature, that greatly enhance its power when used interactively.
.I Csh
also supports the job-control facilities;
see
.IR csh (1)
or the Csh introduction in USD:4 for details.
.LP
You can change from one shell to the other by using the
.I chsh (1)
command, which takes effect at your next login.
.LP
.I
The current directory.\ \ 
.R
.UX
has a file system arranged as a hierarchy of directories.
When the system administrator gave you a user name,
they also created a directory for you (ordinarily
with the same name as your user name).
When you log in, any file name you type is by default in this directory.
Since you are the owner of this directory, you have
full permission to read, write, alter, or destroy its contents.
Permissions to have your will with other directories
and files will have been granted or denied to you by their owners.
As a matter of observed fact, few
.UX
users protect their files from perusal by other users.
.LP
To change the current directory (but not the set of permissions you
were endowed with at login) use
.IR cd (1).
.LP
.I
Path names.\ \ 
.R
To refer to files not in the current directory, you must use a path name.
Full path names begin with ``/\|'', the name of the root directory of the
whole file system.
After the slash comes the name of each directory containing the next
sub-directory (followed by a ``/\|'') until finally the file name is reached.
For example,
.I
/\^usr/\^tmp/\^filex
.R
refers to the file
.I
filex
.R
in the directory
.I
tmp; tmp
.R
is itself a subdirectory of
.I
usr; usr
.R
springs directly from the root directory.
.LP
If your current directory has subdirectories,
the path names of files therein begin with
the name of the subdirectory with no prefixed ``/\|''.
.LP
A path name may be used anywhere a file name is required.
.LP
Important commands that modify the contents of files are
.IR cp (1),
.IR mv (1),
and
.IR rm (1),
which respectively copy, move (i.e. rename) and remove files.
To find out the status of files or directories, use 
.IR ls (1).
See
.IR mkdir (1)
for making directories and
.IR rmdir (1)
for destroying them.
.LP
For a fuller discussion of the file system, see
``A Fast File System for UNIX'' (SMM:14)
by McKusick, Joy, Leffler, and Fabry.
It may also be useful to glance through PRM section 2,
that discusses system calls, even if you do not intend
to deal with the system at that level.
.LP
.I
Writing a program.\ \ 
.R
To enter the text of a source program into a
.UX
file, use the editor
.IR ex (1)
or its display editing alias
.IR vi (1).
(The old standard editor
.IR ed (1)
is also available.)
The principal languages in
.UX
are provided by the C compiler
.IR cc (1),
the Fortran compiler
.IR f77 (1),
and its derivatives 
.IR efl (1)
and
.IR ratfor (1),
the Pascal compiler
.IR pc (1),
and interpreter
.IR pi (1),
and the Lisp system
.IR lisp (1).
User contributed software in the latest
release of the system supports
APL, B, the Functional Programming language, and Icon.
Refer to
.IR apl (1),
.IR b (1),
.IR fp (1),
and
.IR icon (1),
respectively for more information about each.
After the program text has been entered through the editor
and written to a file, you can give the file
to the appropriate language processor as an argument.
The output of the language processor
will be left on a file in the current directory named ``a.out''.
If the output is precious, use
.IR mv (1)
to move it to a less exposed name after successful compilation.
.LP
When you have finally gone through this entire process
without provoking any diagnostics, the resulting program
can be run by giving its name to the shell
in response to the shell (``$'' or ``%'') prompt.
.LP
Your programs can receive arguments from the command line
just as system programs do,
see ``UNIX Programming - Second Edition'' (PS2:3),
or for a more terse description
.IR execve (2).
.LP
.I
Text processing.\ \ 
.R
Almost all text is entered through the editor
.IR ex (1)
(often entered via
.IR vi (1)).
The commands most often used to write text on a terminal are:
.IR cat (1),
.IR more (1),
and
.IR nroff (1).
.LP
The
.IR cat (1)
command simply dumps \s8ASCII\s10 text
on the terminal, with no processing at all.
.IR More (1)
is useful for preventing the output of a command from
scrolling off the top of your screen.
It is also well suited to perusing files.
.IR Nroff (1)
is an elaborate text formatting program.
Used naked, it requires careful forethought, but for
ordinary documents it has been tamed; see
.IR me (7)
and
.IR ms (7).
.LP
.IR Troff (1)
prepares documents for a Graphics Systems phototypesetter
or a Versatec Plotter;
it is similar to 
.IR nroff (1),
and often works from exactly the same source text.
It was used to produce this manual.
.LP
.IR Script (1)
lets you keep a record of your session in a file,
which can then be printed, mailed, etc.
It provides the advantages of a hard-copy terminal
even when using a display terminal.
.LP
.I
Status inquiries.\ \ 
.R
Various commands exist to provide you with useful information.
.IR w (1)
prints a list of users currently logged in, and what they are doing.
.IR date (1)
prints the current time and date.
.IR ls (1)
will list the files in your directory or give
summary information about particular files.
.LP
.I
Surprises.\ \ 
.R
Certain commands provide inter-user communication.
Even if you do not plan to use them, it would be
well to learn something about them, because someone else may aim them at you.
.LP
To communicate with another user currently logged in,
.IR write (1)
or
.IR talk (1)
is used;
.IR mail (1)
will leave a message whose presence will be announced
to another user when they next log in.
The write-ups in the manual also suggest how to respond to
the these commands if you are a target.
.LP
If you use
.IR csh (1)
the key ^Z (control-Z) will cause jobs to ``stop''.
If this happens before you learn about it,
you can simply continue by saying ``fg'' (for foreground) to bring
the job back.
.SH
CONVERTING FROM 4.2BSD SYSTEMS
.LP
A detailed list of changes from the 4.2BSD to the 4.3BSD distributions
is contained in ``Bug Fixes and Changes in 4.3BSD'' (SMM:12),
and ``Changes to the Kernel in 4.3BSD'' (SMM:13).
Detailed conversion procedures are described in
``Installing and Operating 4.3BSD on the VAX'' (SMM:1);
it also discusses changes from pre-4.2BSD systems.
