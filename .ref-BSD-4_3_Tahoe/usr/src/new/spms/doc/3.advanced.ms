.nr PS 12
.NH
Advanced Use
.nr PS 10
.XS
\*(SN Advanced Use
.XE
.PP
This section summarizes the rest of the facilities offered by SPMS
for handling large software projects. Techniques for searching and
editing text files, program testing, and documentation are explained.
.NH 2
Project Hierarchies
.XS
\*(SN Project Hierarchies
.XE
.PP
To facilitate the management of large projects, such projects can be
subdivided into smaller projects. These subprojects can be nested
to any level to form a project hierarchy which is very similar to the
.UX
directory hierarchy. For example, as project `vs' grows, it might be
convenient to convert each of the `hash' and `list' program libraries
into subprojects (see fig. 6).
.KF
.sp 22
.SM
.ce
\fIFigure 6.  \fRProject `vs' with subprojects

.NL
.KE
To show how this conversion is done, the following set of
commands converts project directory `libhash' into a subproject.
.ID
\fIConvert the project directory into a subproject\fR . . .

%  \fBpd src\fR
%  \fBprmdir \-u libhash\fR
%  \fBmkproject libhash\fR
libhash: description? (1 line): \fBVS Hash Table Operations\fR

\&. . . \fIcreate the project directories\fR . . .

%  \fBchproject libhash\fR
%  \fBpmkdir src test work\fR
src: description? (1 line): \fBhash table library source code\fR
test: description? (1 line): \fBhash table library test programs\fR
work: description? (1 line): \fBhash table library workbench\fR

\&. . . \fIreattach the type labels\fR . . .

%  \fBpmkdir  +T\|libsrc,install.1,print.3   src

\&. . . \fIand rearrange the library\fR

%  \fBpmv Makefile \(**.c \(**.o src\fR
%  \fBpd src\fR
%  \fBmkmf  DEST=../../../lib\fR
.DE
.NH 3
\fIThe \fBroot\fI project\fR
.XS
\*(SN The root project
.XE
.PP
The project at the top of each user's project hierarchy is called the
.I
root project
.R
and is given the special name `^'. When the SPMS system was initially
set up (see \(sc2.1), the command
.ID
%  \fBmkproject  \-d  ^\fR
.DE
created the root project and made the user's home directory into the project
root directory for `^'.
.NH 3
Project Pathnames
.XS
\*(SN Project Pathnames
.XE
.PP
.I
Project pathnames
.R
provide a convenient way for accessing a particular directory or file
within a project hierarchy\**.
.FS
Project pathnames are recognized only by SPMS commands.
.FE
A project pathname is formed
by a succession of project names separated by `^' characters\**,
.FS
Since the Bourne shell, (\fIsh\fR), recognizes the `^' character as
an alternative pipe symbol, Bourne shell users must type `\\\\^' instead.
.FE
followed by the name of the directory or file. For instance, the pathname
.ID
^vs^libhash^src
.DE
represents the path from the root project to the `src' directory
located in subproject `libhash', and the pathname
.ID
^vs^libhash^src/hthash.c
.DE
locates the file `hthash.c' in that directory.
.PP
A project pathname can be
.I absolute
or
.I relative.
An
.I
absolute project pathname
.R
specifies the path from the root project and begins with the
character `^'. However, a project pathname not beginning with
`^' is interpreted with respect to the current working project and is
therefore called a
.I
relative project pathname.
.R
For example, the pathname
.ID
libhash^src
.DE
specifies the location of `src' relative to project `vs', assuming
that `vs' is the working project.
.PP
Since relative project pathnames are interpreted relative to the
current working
.B project
rather than the current working directory, this means that project directories
and files can be accessed from
.B any
working directory. For example, the command
.ID
%  \fBpmv  src/libhash.a  work\fR
.DE
moves the hash table library from the `src' directory in the working project
`libhash' to the `work' directory in the same project, regardless of
the location of the current working directory.
.PP
The parent of the working project is called `....' and may be used
in a project pathname to go up one level in a project hierarchy. Thus,
the command
.ID
%  \fBchproject  ....\fR
.DE
makes the parent project of the current project into the new working project.
If the current project happens to be `libhash', then the command
.ID
%  \fBchproject  ....^liblist\fR
.DE
will change to project `liblist'. For completeness, `...' is an
alternative name for the current working project. Table 1 summarizes
the conventions used in project pathnames together with the equivalent
conventions for regular pathnames.
.KF

.SM
.ce
\fITable 1.\fR   Pathname conventions
.NL

.so pathname.tbl
.KE
.PP
Project pathnames can be modified in two ways. The first way allows
a user to refer to a project belonging to someone else by prepending
~\fIuser\fR to the pathname. For example, if `root' has a copy of the project
`vs', the command
.ID
%  \fBppd  ~root^vs\fR
.DE
will print the directories in that project. The other way allows a
regular pathname to follow a project pathname, separated by a `/'
character. This enables access to directories which are not part of
a project. To illustrate, if `junk' is a regular directory in the
`work' directory of the project `vs', the command
.ID
%  \fBpd  ^vs^work/junk\fR
.DE
changes to that directory.
.NH 2
Project Environment
.XS
\*(SN Project Environment
.XE
.PP
It is possible to tailor the environment for the current project by
adding commands to the `.projectrc' startup file located in the root
directory of the project. When the project is activated by the
.I chproject
command, this file is executed. For instance, if a user wishes to be
reminded of tasks that still need attention on a project, a reminder
service can be set up by putting the reminders in a file, (e.g.
`.reminder') and adding the line
.ID
cat .reminder
.DE
to the `.projectrc' file.
.PP
It is also a good idea to include the
.B \-d
option in the alias for the
.I chproject
command (see \(sc\|2.1) so that when
.I chproject
is invoked, it will print the name of the new working project, as in
.DS
%  \fBchproject  ^vs\fR
Visual Spreadsheet
%
.DE
.NH 2
Global Operations
.XS
\*(SN Global Operations
.XE
.PP
Even if a project is divided into subprojects, commands can
still be executed globally over the entire software package by the
.I pexec
command.
.I Pexec
has two modes of execution, depending on the method chosen for selecting
directories. If type labels are not used for selecting a particular
set of directories,
.I pexec
descends recursively through the project hierarchy and executes
the command argument in the project directories at each level. The
command
.ID
%  \fBpexec ls\fR
.DE
demonstrates this mode of operation by listing the contents of the
directories in the project `vs' in the order shown in figure 7.
.KF
.sp 26
.SM
.ce
\fIFigure 7.  \fRDirectory ordering for `pexec ls'

.NL
.KE
.PP
The other mode of operation, involving the use of type labels, causes
.I pexec
to search the project hierarchy for directories with appropriate type
labels, sort the directories according to their priorities, and then
execute the command argument in each directory. As an example of this
mode of execution, figure 8 indicates the order in which the command
.ID
%  \fBpexec  \-T\|print  \'pr  \(**.h  \(**.c\'  |  lpr\fR
.DE
prints the project `vs'.
.KF
.sp 24
.SM
.ce
\fIFigure 8.  \fRDirectory ordering for `pexec \-T\|print ...'

.NL
.KE
.PP
With both modes of operation,
.I pexec
resets the current working project to the project in which the directory
resides. For each of the `src' directories in project `vs' the
corresponding working projects are
.so cwp.tbl
.NH 3
\fIBoolean type label expressions\fR
.XS
\*(SN Boolean type label expressions
.XE
.PP
Global commands can be made even more precise by using boolean expressions\**
.FS
The formal definition of a boolean type label expression is
.CD
\fIE\fR \(-> \fIE\fB or \fIE\fR  | \fIE\fB and \fIE\fR  | \fBnot\fI E\fR  | ( E ) | \fBid\fR
.DE
where
.I E
is a boolean expression; \fBand\fR, \fBor\fR, and \fBnot\fR are
boolean operators; and
.B id
is a type label. As is customary, it is assumed that
.B or
and
.B and
are left-associative, and that
.B or
has the lowest precedence, then \fBand\fR, then
.B not.
.FE
on type labels to select project directories. To show how boolean
expressions are used, let the source code directories in project `vs'
have the type labels shown below.
.KS
.so src.tbl
.KE
.LP
In terms of entering the boolean expression on the command line, `\fBor\fR'
is represented by the character `|', `\fBand\fR' by the character `&', and
`\fBnot\fR' by `!'. Since these characters, together with `(' and `)',
are meaningful
to the command shell, it is good idea to enclose the whole expression
in quotes\**.
.FS
Even if this is done, the `!' character must still be escaped by a
backslash `\\\\' if it precedes a type label to prevent it from being
interpreted by the
.I csh
history mechanism.
.FE
Then, the command
.ID
%  \fBpexec  "\-T\|print\|&\|(libsrc\||\|cmdsrc)"  \'pr  \(**.h  \(**.c\'  |  lpr\fR
.DE
prints the source code in both the program and library source code directories,
but not the directory containing header files. Alternatively,
.ID
%  \fBpexec  "\-T\|print\|&\|\\!include"  \'pr  \(**.h  \(**.c\'  |  lpr\fR
.DE
achieves the same result.
.NH 3
\fISearching and editing\fR
.XS
\*(SN Searching and editing
.XE
.PP
Whenever it becomes necessary to alter something like the number of arguments
in a call to a function, the
.I pgrep
command can be used to bring up the text editor on all the files in the software
package that contain that function call. Suppose, for example, that the
number of arguments to the
.UX
system call `open' for opening files has changed. The command
.ID
%  \fBpgrep  \-C\|vi  \-T\|src  \-m  \'open(\'\fR
.DE
will edit all the source code files containing that call, using the
.I vi
text editor.
.NH 2
Testing
.XS
\*(SN Testing
.XE
.PP
After a program is released for general use, it will require
maintenance. It may have to be modified to speed it up, fix bugs, or add
new features. Each time the program is altered, the parts that are
affected should be checked against previous test results by doing
.I regression
testing. The
.I ptest
program mechanizes this process.
.PP
.I Ptest
tests each function by running a test program and comparing the output
with previously prepared results. For example, the test for the `htinstall'
function in the hash table library produces
.DS
%  \fBptest htinstall\fR
htinstall: extracting archive ... compiling test ... executing test ... done
%
.DE
if the test succeeds. However, if the test fails,
.I ptest
reports this fact by
.ID
htinstall: extracting archive ... compiling test ... executing test ... failed
.DE
and saves the error diagnostics in a file named `Ehtinstall'.
.PP
The test program and data files for each test case are stored in an archive file
named \fItest\fR.a where
.I test
is the name of the test case, located in the `test' directory. In the
case of `htinstall', the test archive is called `htinstall.a' and contains
the test program source file, Thtinstall.c, the input data file, Ihtinstall,
and the validated output data file, Ohtinstall. The details on how to set
up a test archive are explained more fully in section ptest(1P) of the
.UX
programmer's manual.
.NH 2
Documentation
.XS
\*(SN Documentation
.XE
.NH 3
\fIThe project log\fR
.XS
\*(SN The project log
.XE
.PP
The
.I plog
project log command provides an electronic notebook system by which to record
transactions such as incoming and outgoing mail, progress reports,
minutes of project staff meetings, etc.
.I Plog
records messages in a file called `projectlog' located in the project root
directory, by invoking the
.UX
.I Mail
program\|[9]. After the
.I Mail
program starts up, the user types in the message, followed by a period `.'
or CNTL-D at the beginning of a line. Since the
.I Mail
program processes the message, the user can take advantage of all the mailing
facilities offered by the system. For instance, the following announcement
on the `vs' project can be mailed to a group of users labeled `vsusers'\**
.FS
By the
.I alias
mechanism of
.I Mail.
.FE
using the `~c' `carbon copy' facility of the
.I Mail
program:
.DS
%  \fBplog\fR
Subject: \fB`vs' release 2
~c vsusers
Release 2 of the `vs' visual spreadsheet package is now available for
distribution. It has the following features:\fR
			.
			.
			.
%
.DE
.PP
.I Plog
can be used to produce reports by printing sections of the project log
with subject headings. For example, if the above announcement is message 20
in the project log for the `vs' project, the following command will print
message 20 plus any subsequent messages.
.ID
%  \fBplog  \-p20\fR
\l'\n(.lu-\n(.iu\&-'
.ce
`vs' release 2
\l'\n(.lu-\n(.iu\&-'
From pjn Wed Aug 10 11:02:44 1983
To: /usr/pjn/vs/projectlog
Subject: `vs' release 2
Cc: vsusers

Release 2 of the `vs' visual spreadsheet package is now available for
distribution. It has the following features:
			.
			.
			.
%
.DE
.PP
.I Plog
can also be used to collect incoming mail, edit the project log, and
sort it into chronological order. These options are explained
more fully in section plog(1P) of the
.UX
programmer's manual.
.NH 3
\fIReference manual\fR
.XS
\*(SN Reference manual
.XE
.PP
The
.I pman
command supports a project reference manual in the same
way that the
.I man
command provides information from the
.UX
programmer's manual. For example, to print information about the `vs'
visual spreadsheet program, type
.ID
%  \fBpman vs\fR
.DE
and to find out about the `vstutor' program, type
.ID
%  \fBpman vstutor\fR
.DE
.PP
The directories that contain the manual entries must
be set up in the same way as the programmer's manual as shown in figure 9.
.KF
.sp 18
.SM
.ce
\fIFigure 9.  \fRLayout of the `vs' project manual

.NL
.KE
By convention, manual pages for commands have `.1' suffixes and are
kept in the `man1' directory, manual pages for libraries have `.3'
suffixes and are kept in `man3', and file formats have `.5' suffixes
and are kept in `man5'. The
.I pman
command searchs through each of the `man1', `man3', and `man5'
directories in turn until it finds the topic.
.NH 3
\fIOn-line help\fR
.XS
\*(SN On-line help
.XE
.PP
On-line help for a project is provided by the
.I phelp
command. After
.I phelp
is typed, it prints some introductory information, a list of available
topics, and then the prompt `???', indicating that it is ready for a
command. The following commands are recognized
.DS C
.so help.tbl
.DE
If a topic name is typed in reply,
.I phelp
will print a page of information and then wait until a space is typed before
it continues.
.PP
In project `vs' there are three topics available \-
`install' explains how to install `vs'; `progress' lists recent developments;
and `schedule' outlines the development plan. In the following session,
.I phelp
is used to examine some of these topics.
.ID
%  \fBphelp\fR
(\fIprints an introduction to phelp\fR)

Help topics available:  install  progress  schedule

???  \fBschedule\fR
(\fIprints `schedule' topic\fR)
???  \fBprogress\fR
(\fIprints `progress' and goes down one level to `progress' subtopics\fR)

progress subtopics: bugfixes

progress-->??? \fBbugfixes\fR 
(\fIprints `bugfixes' topic\fR)
progress-->???  \fBq\fR
(\fIexits from phelp\fR)
%
.DE
.PP
Help topics are contained in files which reside in the `help' directory
located in the project root directory. Figure 10 shows how these
topics are set up for project `vs'.
.KF
.sp 20
.SM
.ce
\fIFigure 10.  \fRHelp topics for project `vs'

.NL
.KE
The circles represent topic files, and the rectangles represent
directories. Subtopics are contained in subdirectories named according
to the topics they represent, but with a `.d' suffix. Consequently,
`bugfixes' is in the subdirectory `progress.d' since it is a subtopic of
`progress'.


