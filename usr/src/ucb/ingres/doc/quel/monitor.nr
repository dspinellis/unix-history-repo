.th MONITOR QUEL 2/23/79
.sh NAME
monitor \- interactive terminal monitor
.sh DESCRIPTION
The interactive terminal monitor
is the primary front end
to \*(II.
It provides the ability
to formulate a query
and review it
before issuing it
to \*(II.
If changes must be made,
one of the \*(UU text editors
may be called
to edit the
.it "query buffer."
.s1
.bd "Messages and Prompts."
.s2
The terminal monitor
gives a variety of messages
to keep the user informed
of the status
of the monitor
and the query buffer.
.s3
As the user logs in,
a login message is printed.
This typically tells the version
number
and the login time.
It is followed by the dayfile,
which gives information
pertinant to users.
.s3
When \*(II is ready to accept input,
the message
``go''
is printed.
This means that the query buffer
is empty.
The message
``continue''
means that there is information
in the query buffer.
After a \ego command
the query buffer
is automatically cleared
if another query is typed in,
unless a command which affects the query buffer
is typed first.
These commands are
\eappend,
\eedit,
\eprint,
\elist,
\eeval,
and
\ego.
For example, typing
.br
	help parts
.br
	\ego
.br
	print parts
.br
results in the query buffer containing
.br
print parts
.br
whereas
.br
	help parts
.br
	\ego
.br
	\eprint
.br
	print parts
.br
results in the query buffer containing
.br
	help parts
.br
	print parts
.br
An asterisk is printed at the beginning
of each line
when the monitor
is waiting for the user
to type input.
.s1
.bd Commands
.s2
There are a number of commands
which may be entered by the user
to affect the query buffer
or the user's environment.
They are all preceeded
by a backslash
(`\e'),
and all are executed immediately
(rather than at execution time
like queries).
.s3
Some commands may take a filename,
which is defined
as the first significant character
after the end of the command
until the end of the line.
These commands may have no other commands
on the line
with them.
Commands which do not take a filename
may be stacked
on the line;
for example
.br
	\edate\ego\edate
.br
will give the time
before and after
execution of the current query buffer.
.de xx
.lp +10 10
..
.s3
.xx
\er
.xx
\ereset	Erase the entire query (reset the query buffer).
The former contents of the buffer are irretrieveably lost.
.s3
.xx
\ep
.xx
\eprint	Print the current query.
The contents of the buffer are printed on the user's terminal.
.s3
.xx
\el
.xx
\elist	Print the current query as it will appear
after macro processing.
Any side effects of macro processing,
such as macro definition,
will occur.
.s3
.xx
\eeval	Macro process the query buffer
and replace the query buffer with the result.
This is just like \elist
except that the output is put into the query buffer
instead of to the terminal.
.s3
.xx
\ee
.xx
\eed
.xx
\eedit
.xx
\eeditor	Enter the \*(UU text editor
(see ED in the \*(UU Programmer's Manual); use
the ED command 'w' followed by 'q' to return to the
\*(II monitor.
If a filename is given,
the editor is called with that file instead of the query buffer.
If the macro ``{editor}'' is defined,
that macro is used as the pathname of an editor,
otherwise ``/bin/ed'' is used.
It is important that you do not use the ``e'' command
inside the editor;
if you do the
(obscure) name of the query buffer
will be forgotten.
.s3
.xx
\eg
.xx
\ego	Process the current query.
The contents of the buffer are
macro processed,
transmitted to \*(II, and run.
.s3
.xx
\ea
.xx
\eappend	Append to the query buffer.
Typing \ea after completion of a query will override the
auto-clear feature and guarantees that the query buffer
will not be reset.
.s3
.xx
\etime
.xx
\edate	Print out the current time of day.
.s3
.xx
\es
.xx
\esh
.xx
\eshell	Escape to the \*(UU shell.
Typing a control-d will cause
you to exit the shell and return to the \*(II monitor.
If there is a filename specified,
that filename is taken as a shell file which is run
with the query buffer
as the parameter ``$1''.
If no filename is given,
an interactive shell is forked.
If the macro ``{shell}'' is defined,
it is used as the pathname of a shell;
otherwise,
``/bin/sh'' is used.
.s3
.xx
\eq
.xx
\equit	Exit from \*(II.
.s3
.xx
\ecd
.xx
\echdir	Change the working directory of the monitor
to the named directory.
.s3
.xx
\ei
.xx
\einclude
.xx
\eread	Switch input to the named file.
Backslash characters in the file will be processed as read.
.s3
.xx
\ew
.xx
\ewrite	Write the contents of the query buffer
to the named file.
.s3
.xx
\ebranch	Transfer control within a \einclude
file.
See the section on branching below.
.s3
.xx
\emark	Set a label for \ebranch.
.s3
.xx
\e<any other character>
.br
Ignore any possible special meaning of character following '\e'.
This allows the '\e'
to be input as a literal character.
(See also quel(quel) - strings).
It is important to note that backslash escapes
are sometimes eaten up
by the macro processor also;
in general,
send two backslashes
if you want a backslash sent
(even this is too simplistic [sigh] \-
try to avoid using backslashes at all).
.i0
.s1
.bd Macros
.s2
For simplicity,
the macros are described in the section macros(quel).
.s1
.bd "Branching"
.s2
The \ebranch and \emark commands
permit arbitrary branching within a \einclude
file
(similar to the ``goto'' and ``:'' commands
in the shell).
\emark should be followed with a label.
\ebranch should be followed with either a label,
indicating unconditional branch,
or an expression preceeded by a question mark,
followed by a label,
indicating a conditional branch.
The branch is taken if the expression
is greater than zero.
For example,
.br
	\ebranch ?{tuplecount}<=0 notups
.br
branches to label ``notups'' if the ``{tuplecount}'' macro
is less than or equal to zero.
.s3
The expressions usable in \ebranch statements
are somewhat restricted.
The operators +, \-, \*(**,
.if t \(sl,
.if n /,
<=, >=, <, >, =, and !=
are all defined in the expected way.
The left unary operator ``!'' can be used
as to indicate logical negation.
There may be no spaces in the expression,
since a space terminates the expression.
.s1
.bd "Initialization"
.s2
At initialization (login) time
a number of initializations take place.
First,
a macro called ``{pathname}''
is defined
which expands to the pathname
of the \*(II subtree
(normally ``/mnt/ingres'');
it is used by system routines
such as demodb.
Second,
the initialization file
\&.../files/startup
is read.
This file is intended to define
system-dependent parameters,
such as the default editor and shell.
Third,
a user dependent initialization file,
specified by a field in the users file,
is read and executed.
This is normally set to the file ``.ingres'' in the
user's home directory.
The startup file might be used
to define certain macros,
execute common range statements,
and soforth.
Finally,
control is turned over
to the user's terminal.
.s3
An interrupt while executing either of the initialization files
restarts execution of that step.
.s1
.bd Flags
.s2
Certain flags
may be included
on the command line
to \*(II
which affect the operation
of the terminal monitor.
The \-a flag
disables the autoclear function.
This means that the query buffer will never
be automatically cleared;
equivalently,
it is as though a \eappend command
were inserted after every \ego.
Note that this means that the user
must explicitly clear the query buffer
using \ereset
after every query.
The \-d flag
turns off the printing of the dayfile.
The \-s flag
turns off printing of all messages
(except errors)
from the monitor,
including the login and logout messages,
the dayfile,
and prompts.
It is used for executing
``canned queries'',
that is,
queries redirected from files.
.sh "SEE ALSO"
ingres(unix),
quel(quel),
macros(quel)
.sh DIAGNOSTICS
.de xx
.lp +20 20
..
.xx
go	You may begin a fresh query.
.s3
.xx
continue	The previous query is finished and you
are back in the monitor.
.s3
.xx
Executing . . .	The query is being processed by \*(II.
.s3
.xx
>>ed	You have entered the \*(UU text editor.
.s3
.xx
>>sh	You have escaped to the \*(UU shell.
.s3
.xx
Funny character nnn converted to blank
.br
\*(II maps non-printing ASCII characters into blanks; this message
indicates that one such conversion has just been made.
.i0
.sh INCOMPATIBILITIES
Note that the construct
.br
	\erprint parts
.br
(intended to reset the query buffer
and then enter ``print parts'')
no longer works,
since ``rprint'' appears to be one word.
.sh BUGS
